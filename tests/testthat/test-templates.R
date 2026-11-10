# a template's item of one function (wherever the template puts it)
tpl_item <- function(x, fn) x$content[[match(fn, vapply(x$content, `[[`, "", "fn"))]]

# Template SPEC: one definition, its items (B) and its object (A).

tpl_ard <- function() {
  skip_if_not_installed("cards")
  saf <- adsl[adsl$SAFFL == "Y", ]
  cards::bind_ard(
    cards::ard_tabulate(saf, variables = TRT01A),
    cards::ard_summary(saf, by = TRT01A, variables = c(AGE, TRTDURD)),
    cards::ard_tabulate(saf, by = TRT01A, variables = c(AGEGR1, SEX, RACE, ETHNIC)))
}

# demog's required arguments (a template of its own: the variables and `by` are given)
dmv <- function(...) utils::modifyList(
  list(population = "SAF", by = "TRT01A", continuous = "AGE", categorical = c("AGEGR1", "SEX", "RACE", "ETHNIC")),
  list(...))

test_that("tfl_templates() lists demog and summary", {
  t <- tfl_templates()
  expect_true(all(c("demog", "summary") %in% t$name))
  expect_identical(t$layout[t$name == "demog"], "summary")
})

test_that("tfl_template_expand(): the items, marked as the template's", {
  x <- tfl_template_expand("demog", dmv())
  expect_identical(vapply(x$content, `[[`, "", "fn"),
                   c("table_plan", "plan_cells", "plan_labels", "plan_paginate_rows"))
  expect_true(all(vapply(x$content, `[[`, "", "from") == "template:demog"))
  expect_identical(x$ard$analysis_data[[1L]]$id, "adsl_saf")
  expect_identical(vapply(x$ard$analyses, `[[`, "", "id"), c("GROUPN", "CONT", "CAT"))
  # (`stack` is gone from demog: the plain unknown-parameter error)
  expect_error(tfl_template_expand("demog", dmv(stack = TRUE)), "no parameter 'stack'")
  # the code lists are the ARD's (applied when it is made): no plan_levels()
  cl <- list(SEX = list(list(value = "F", label = "Female"), list(value = "M", label = "Male")))
  y <- tfl_template_expand("demog", dmv(), codelists = cl)
  expect_null(tpl_item(y, "plan_levels"))
  # a dataset other than ADSL: its own analysis data, its records kept by `where`
  z <- tfl_template_expand("summary", list(population = "SAF", continuous = "AVAL",
                                           data = "ADVS", where = 'PARAMCD == "SYSBP"'))
  expect_identical(z$ard$analysis_data[[2L]]$id, "advs_saf")
  expect_s3_class(z$ard$analysis_data[[2L]]$where, "tfl_r")
})

test_that("ae_soc_pt: the hierarchical layout with its own defaults", {
  x <- tfl_template_expand("ae_soc_pt", list(population = "SAF"))
  expect_identical(x$ard$analyses[[1L]]$variables, c("AEBODSYS", "AEDECOD"))
  expect_s3_class(x$ard$analysis_data[[2L]]$where, "tfl_r")
  expect_identical(x$content[[1L]]$args$overall, "Any TEAE")
  expect_identical(x$content[[2L]]$args$rows, list(group1 = "AEBODSYS"))
  # the layout itself: no overall row, no where
  h <- tfl_template_expand("hierarchical", list(population = "SAF",
                                                hierarchy = c("AEBODSYS", "AEDECOD")))
  expect_null(h$content[[1L]]$args$overall)
  expect_null(h$ard$analyses[[1L]]$args)
  expect_true(all(c("hierarchical", "ae_soc_pt") %in% tfl_templates()$name))
})

test_that("tfl_template_expand(): an unknown or missing parameter is said", {
  expect_error(tfl_template_expand("demog", dmv(nope = 1)), "no parameter")
  expect_error(tfl_template_expand("demog", list()), "needs")
  expect_error(tfl_template_expand("nope"), "no template")
})

test_that("tpl_demog() / tpl_summary(): the object is the items run on the ARD", {
  ard <- tpl_ard()
  p <- suppressMessages(tpl_demog(ard, population = "SAF", by = "TRT01A", continuous = "AGE",
                                  categorical = c("AGEGR1", "SEX", "RACE", "ETHNIC")))
  expect_s3_class(p, "table_plan")
  # the same as the items, run as a report's content
  x <- tfl_template_expand("demog", dmv())
  o <- tfl_output("T", extends = "none", data = x$data,
                  content = c(list(tfl_item("normalize_ard", list())), x$content))
  q <- suppressMessages(tfl_content_run(o, ard))
  expect_identical(suppressMessages(rtfreporter::plan_apply(p, "table")),
                   suppressMessages(rtfreporter::plan_apply(q, "table")))
  s <- suppressMessages(tpl_summary(ard, population = "SAF", continuous = c("AGE", "TRTDURD")))
  tab <- suppressMessages(rtfreporter::plan_apply(s, "table"))
  expect_true(all(c("Drug X", "Control") %in% names(tab)))
})

test_that("tfl_read_template_sheets(): the Excel example, as report files", {
  skip_if_not_installed("readxl")
  f <- system.file("extdata", "template-spec", "templates.xlsx", package = "tflspec")
  r <- tfl_read_template_sheets(f)
  expect_identical(names(r), c("T-14-1-1", "T-14-3-1"))
  a <- r[["T-14-1-1"]]$template$args
  expect_identical(a$categorical, c("AGEGR1", "SEX", "RACE", "ETHNIC"))
  expect_identical(a$labels[["AGE"]], "Age (years)")
  expect_identical(r[["T-14-3-1"]]$template$args$overall, "Any TEAE")
  # the same expansion as the template's own defaults (they are the example's values)
  x <- tfl_template_expand("demog", c(list(population = "SAF"), a))
  y <- tfl_template_expand("demog", dmv())
  expect_identical(x$content, y$content)
  out <- withr::local_tempfile(fileext = ".yml")
  tfl_write_template_yaml(r[["T-14-1-1"]], out)
  back <- yaml::read_yaml(out)
  expect_identical(back$template$name, "demog")
  expect_identical(unlist(back$template$args$categorical), a$categorical)
})

# demog's ARD where Control has one subject with an age (n = 1), or none (n = 0)
dm_ard <- function(control = c("one", "none")) {
  control <- match.arg(control)
  saf <- adsl[adsl$SAFFL == "Y", ]
  ctl <- which(saf$TRT01A == "Control")
  if (control == "one") saf <- saf[-ctl[-1L], ] else saf$AGE[ctl] <- NA
  cards::bind_ard(
    cards::ard_tabulate(saf, variables = TRT01A),
    cards::ard_summary(saf, by = TRT01A, variables = AGE),
    cards::ard_tabulate(saf, by = TRT01A, variables = c(AGEGR1, SEX, RACE, ETHNIC)))
}
# demog's Age Mean (SD) row: Drug X, Control
dm_age <- function(ard, ...) {
  t <- as.data.frame(suppressMessages(rtfreporter::plan_apply(
    suppressMessages(tpl_demog(ard, population = "SAF", by = "TRT01A", continuous = "AGE",
                               categorical = c("AGEGR1", "SEX", "RACE", "ETHNIC"), ...)), "table")))
  unlist(t[t$label == "Mean (SD)", c("Drug X", "Control")], use.names = FALSE)
}

test_that("demog: Mean (SD) keeps the mean when a group has one subject; sd_missing says what prints (#673)", {
  skip_if_not_installed("cards")
  ard1 <- dm_ard("one")
  saf <- adsl[adsl$SAFFL == "Y", ]
  ctl1 <- saf$AGE[saf$TRT01A == "Control"][1L]
  drug <- saf$AGE[saf$TRT01A == "Drug X"]
  # n >= 2: as before; n = 1: the mean and `-`, to the ARD's digits (the formats are the ARD's:
  # a hand-made cards ARD prints the SD to its own default, one place)
  expect_identical(dm_age(ard1), c(sprintf("%.1f (%.1f)", mean(drug), stats::sd(drug)),
                                   sprintf("%.1f (-)", ctl1)))
  expect_identical(dm_age(ard1, sd_missing = "NA")[2L], sprintf("%.1f (NA)", ctl1))
  one <- dm_age(ard1, sd_missing = "")
  expect_identical(one, c(dm_age(ard1)[1L], sprintf("%.1f", ctl1)))
  # n = 0: blank, whatever the mark
  ard0 <- dm_ard("none")
  expect_true(is.na(dm_age(ard0)[2L]))
  expect_true(is.na(dm_age(ard0, sd_missing = "")[2L]))
  # cells the report gives are its own: sd_missing has no effect
  own <- list(n = "{N:.0f}", `Mean (SD)` = "{mean:.1f} ({sd:.2f})")
  expect_true(is.na(dm_age(ard1, cells = own, sd_missing = "NA")[2L]))
  # a mark that is not a text without braces stops
  expect_error(tfl_template_expand("demog", dmv(sd_missing = "{sd}")), "sd_missing")
  expect_error(tfl_template_expand("demog", dmv(sd_missing = NA_character_)), "sd_missing")
  # the default cell: the mean and its SD, else the mean and the mark
  # (the cells only place the statistics: the digits are the ARD's formats)
  x <- tfl_template_expand("demog", dmv())
  expect_identical(tpl_item(x, "plan_cells")$args$continuous[["Mean (SD)"]],
                   c("{mean} ({sd})", "{mean} (-)"))
  y <- tfl_template_expand("demog", dmv(sd_missing = ""))
  expect_identical(tpl_item(y, "plan_cells")$args$continuous[["Mean (SD)"]],
                   c("{mean} ({sd})", "{mean}"))
  # (the rest of the cells, the template's)
  expect_identical(tpl_item(y, "plan_cells")$args$continuous[-2L],
                   tpl_item(x, "plan_cells")$args$continuous[-2L])
  expect_true("sd_missing" %in% names(tfl_template_params("param_change")))
  expect_identical(tfl_template_params("summary")$sd_missing$study, "settings.sd_missing")
  expect_identical(tfl_template_params("demog")$sd_missing$study, "settings.sd_missing")
})

test_that("summary: categorical variables too; disposition on it", {
  x <- tfl_template_expand("summary", list(population = "SAF", categorical = c("AGEGR1", "SEX"),
                                           categorical_statistics = NULL))
  expect_identical(vapply(x$ard$analyses, `[[`, "", "id"), c("GROUPN", "CAT"))
  expect_null(x$ard$analyses[[2L]]$statistics)
  expect_false("plan_cells" %in% vapply(x$content, `[[`, "", "fn"))
  expect_identical(x$content[[length(x$content)]]$args, list(max_rows = 30L, split = "group_safe"))
  # the page is the study standard's (or |> plan_paginate_rows() after), not a parameter
  expect_error(tfl_template_expand("summary", list(population = "SAF", continuous = "AGE",
                                                   max_rows = 24)), "no parameter 'max_rows'")
  expect_error(tfl_template_expand("summary", list(population = "SAF", continuous = "AGE",
                                                   colour = "red")), "no parameter")
  expect_error(tfl_template_expand("summary", list(population = "SAF")), "categorical")
  d <- tfl_template_expand("disposition", list(population = "SAF"))
  expect_identical(d$ard$analyses[[2L]]$id, "DISP")
  expect_identical(tpl_item(d, "plan_labels")$args, list(variable = list(EOSSTT = "Status at end of study, n (%)")))
  expect_true("disposition" %in% tfl_templates()$name)
  ard <- tpl_ard()
  p <- suppressMessages(tpl_summary(ard, population = "SAF", categorical = c("AGEGR1", "SEX")))
  expect_s3_class(p, "table_plan")
})

test_that("shift and lab_shift: baseline by post category, a block a parameter", {
  x <- tfl_template_expand("lab_shift", list(population = "SAF", visit = "CYCLE 2 DAY 1",
                                             params = c("ALT", "AST")))
  # (the records kept in the code that keeps a subject once, at its worst)
  expect_match(x$ard$analysis_data[[1L]]$code,
               'AVISIT == "CYCLE 2 DAY 1" & PARAMCD %in% c("ALT", "AST")', fixed = TRUE)
  expect_identical(x$ard$analyses[[1L]]$by, c("TRT01A", "PARAMCD", "BNRIND"))
  expect_identical(x$content[[1L]]$args$cols, c("TRT01A", ".label"))
  s <- tfl_template_expand("shift", list(population = "SAF", by_levels = c("A", "B")))
  expect_identical(s$content[[3L]]$args$TRT01A, c("A", "B"))
  expect_error(tfl_template_expand("lab_shift", list(population = "SAF")), "needs")
  expect_true(all(c("shift", "lab_shift") %in% tfl_templates()$name))
})

test_that("shift: a subject once at its worst, a Missing and a Total row, a page a parameter", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("cards")
  # (a Missing row and the tested subjects' N: the options of the old defaults)
  a <- list(population = "SAF", where = 'PARAMCD %in% c("ALT", "AST")',
            missing_label = "Missing", header_n = "tested")
  x <- tfl_template_expand("shift", a)
  ad <- x$ard$analysis_data[[1L]]
  expect_null(ad$where)
  lb <- adlb
  lb$BNRIND[lb$PARAMCD == "ALT" & lb$USUBJID == lb$USUBJID[1L]] <- NA
  d <- local({
    adlb <- lb
    adsl_sub <- adsl[adsl$SAFFL == "Y", ]
    eval(parse(text = ad$code))
  })
  body <- d[d$BNRIND != "Total", ]
  expect_false(anyDuplicated(body[c("USUBJID", "PARAMCD")]) > 0L)
  expect_false(any(body$ABLFL %in% "Y"))
  expect_identical(sum(d$BNRIND == "Total"), nrow(body))
  expect_true("Missing" %in% body$BNRIND)
  # a transport file's "" for no value: a blank after baseline is no record,
  # a blank at baseline is Missing
  lb2 <- lb
  post <- which(lb2$PARAMCD == "AST" & !lb2$ABLFL %in% "Y")
  s1 <- lb2$USUBJID[post[1L]]
  s2 <- setdiff(lb2$USUBJID[post], s1)[1L]
  lb2$ANRIND[post[lb2$USUBJID[post] == s1]] <- ""
  lb2$BNRIND[lb2$PARAMCD == "AST" & lb2$USUBJID == s2] <- ""
  d2 <- local({
    adlb <- lb2
    adsl_sub <- adsl[adsl$SAFFL == "Y", ]
    eval(parse(text = ad$code))
  })
  expect_false(any(d2$ANRIND %in% ""))
  expect_false(any(d2$BNRIND %in% ""))
  expect_false(any(d2$USUBJID == s1 & d2$PARAMCD == "AST"))
  expect_true(any(d2$USUBJID == s2 & d2$PARAMCD == "AST" &
                    d2$BNRIND == "Missing"))
  # the worst: a subject with any HIGH after baseline is HIGH
  one <- lb[lb$PARAMCD == "ALT" & !lb$ABLFL %in% "Y" & lb$ANRIND %in% "HIGH", "USUBJID"][1L]
  expect_identical(body$ANRIND[body$USUBJID == one & body$PARAMCD == "ALT"], "HIGH")
  lv <- Filter(function(i) i$fn == "plan_levels", x$content)[[1L]]$args
  expect_identical(lv$BNRIND, c("LOW", "NORMAL", "HIGH", "Missing", "Total"))
  fns <- vapply(x$content, `[[`, "", "fn")
  # (the page's N on: a group a page, the parameter out of the stub, no hide)
  expect_true("plan_paginate_group" %in% fns)
  expect_false(any(c("plan_paginate_rows", "plan_hide") %in% fns))
  expect_identical(x$ard$analysis_data[[2L]]$id, "adlb_saf_den")
  # the defaults: a missing baseline is out (no Missing row), the header N is the
  # set's (a page a parameter by paginate_rows, the parameter hidden)
  dflt <- tfl_template_expand("shift", list(population = "SAF"))
  expect_identical(Filter(function(i) i$fn == "plan_levels", dflt$content)[[1L]]$args$BNRIND,
                   c("LOW", "NORMAL", "HIGH", "Total"))
  expect_match(dflt$ard$analysis_data[[1L]]$code, '!BNRIND %in% c(NA, "")', fixed = TRUE)
  expect_identical(dflt$ard$analyses[[1L]]$denominator, "adlb_saf_den")
  expect_true(all(c("plan_paginate_rows", "plan_hide") %in% vapply(dflt$content, `[[`, "", "fn")))
  expect_false("plan_paginate_group" %in% vapply(dflt$content, `[[`, "", "fn"))
  # run on the records: the subject with no baseline is in no row and not in the Total
  d3 <- local({
    adlb <- lb
    adsl_sub <- adsl[adsl$SAFFL == "Y", ]
    eval(parse(text = dflt$ard$analysis_data[[1L]]$code))
  })
  gone <- lb$USUBJID[1L]
  expect_false(any(d3$USUBJID == gone & d3$PARAMCD == "ALT"))
  expect_false(any(d3$BNRIND %in% c(NA, "", "Missing")))
  expect_true(any(d3$USUBJID == gone & d3$PARAMCD == "AST"))
  # every record, as before: no code, no extra rows, the parameter in the stub
  # (the default percent_of "total" needs a subject once a parameter: by row here)
  a$header_n <- "set"
  r <- tfl_template_expand("shift", c(a, list(worst = NULL, param_page = FALSE)))
  expect_null(r$ard$analyses[[1L]]$denominator)
  expect_null(r$ard$analysis_data[[1L]]$code)
  expect_identical(as.character(r$ard$analysis_data[[1L]]$where), a$where)
  expect_false(any(c("plan_paginate_rows", "plan_hide") %in%
                     vapply(r$content, `[[`, "", "fn")))
})

test_that("listing: one row a record, a column's width part of its definition", {
  x <- tfl_template_expand("listing", list(data = "ADAE", where = 'AESEV == "SEVERE"',
    sort = "USUBJID", columns = list(list(vars = "USUBJID", label = "Subject", width = 14,
                                          collapse_repeats = TRUE),
                                     list(vars = c("AEBODSYS", "AEDECOD"), label = "SOC/PT"))))
  expect_identical(x$data$dataset, "ADAE")
  cols <- x$content[[1L]]$args$cols
  expect_s3_class(cols[[1L]], "tfl_call")
  expect_identical(cols[[1L]]$args$width, 14L)
  expect_null(cols[[2L]]$args$width)
  expect_identical(x$content[[1L]]$args$type, "multiline")
  expect_error(tfl_template_expand("listing", list(data = "ADAE")), "needs")
  p <- suppressMessages(tpl_listing(adae[seq_len(20), ], data = "ADAE",
                                    columns = list(list(vars = "USUBJID", label = "Subject"))))
  expect_s3_class(p, "table_plan")
})

# ---- listing: `by` divides the listing (a page or a block a value) ----------------

# invented records: 4, 5 and 3 of three severities; a record is two rows (its
# blank one), so 22 rows hold 11 records
lby_data <- function() {
  data.frame(
    USUBJID = sprintf("S-%02d", c(1, 1, 2, 3, 1, 2, 2, 4, 5, 3, 4, 6)),
    AEDECOD = sprintf("Event %02d", 1:12),
    AESEV = rep(c("MILD", "MODERATE", "SEVERE"), c(4, 5, 3)),
    SITEID = rep(c("101", "102"), 6),
    stringsAsFactors = FALSE)
}
lby_cols <- list(list(vars = "USUBJID", label = "Subject"), list(vars = "AEDECOD", label = "Event"))
lby_sev <- list(vars = "AESEV", label = "Severity")
lby_x <- function(...) tfl_template_expand("listing", list(data = "ADAE", ...))
lby_arg <- function(x, fn) Filter(function(it) identical(it$fn, fn), x$content)[[1L]]$args
lby_fns <- function(x) vapply(x$content, `[[`, "", "fn")
lby_listing_vars <- function(x) vapply(lby_arg(x, "plan_listing")$cols, function(cl) paste(cl$args$vars, collapse = "+"), "")

test_that("listing by: the parameters", {
  p <- tfl_template_params("listing")
  expect_identical(p$by[c("type", "multiple")], list(type = "vars", multiple = TRUE))
  # (not the study's treatment, which `by` is in the tables' templates)
  expect_null(p$by$study)
  expect_true(p$by$no_study)
  expect_identical(tfl_template_params("shift")$by$study, "settings.treatment")
  # (done twice, as the help page does: still none)
  expect_null(.tpl_param_typed(p$by)$study)
  expect_false(.tpl_study_kind(p$by))
  expect_true(.tpl_study_kind(.tpl_param_typed(tfl_param("by", "TRT01A"))))
  expect_identical(p$by_break$choices, c("page", "block"))
  expect_identical(p$by_break$default, "page")
  expect_identical(p$by_heading$type, "text")
  expect_identical(names(p), c("data", "columns", "where", "subset", "sort", "by", "by_break", "by_heading", "by_na_label"))
})

test_that("listing by, no by: the items as they were", {
  x <- lby_x(columns = lby_cols, sort = c("USUBJID", "AEDECOD"), where = "AESEV == \"SEVERE\"")
  expect_identical(lby_fns(x), c("plan_listing", "plan_paginate_rows"))
  expect_identical(lby_arg(x, "plan_paginate_rows"), list(max_rows = 22L))
  expect_identical(names(lby_arg(x, "plan_listing")), c("cols", "type"))
  expect_identical(lby_listing_vars(x), c("USUBJID", "AEDECOD"))
  expect_identical(x$data$sort, c("USUBJID", "AEDECOD"))
  # by_break alone says nothing it could do
  expect_identical(lby_fns(lby_x(columns = lby_cols, by = NULL)), c("plan_listing", "plan_paginate_rows"))
  # the study's treatment is not a listing's `by`
  d <- withr::local_tempdir()
  dir.create(file.path(d, "spec", "outputs"), recursive = TRUE)
  writeLines(c("datasets:", "  - dataset: ADAE", "    path: data/adae.rds"), file.path(d, "spec", "data.yml"))
  writeLines(c("study_id: SS-1", "settings:", "  treatment: TRT01A"), file.path(d, "study.yml"))
  o <- tfl_output("L-1", "listing", extends = "none",
                  template = list(name = "listing", args = list(data = "ADAE", columns = lby_cols)))
  ro <- tfl_resolve_output(o, tfl_read_study_spec(d))
  expect_null(attr(ro, "template")$args$by)
  expect_identical(vapply(ro$content, `[[`, "", "fn"), c("plan_listing", "plan_paginate_rows"))
  expect_null(ro$data$sort)
})

test_that("listing by page: not printed unless a column is of it; the page named", {
  x <- lby_x(columns = lby_cols, by = "AESEV")
  expect_identical(lby_fns(x), c("plan_listing", "plan_hide", "plan_paginate_rows"))
  # the variable is a hidden column of its own, last
  expect_identical(lby_listing_vars(x), c("USUBJID", "AEDECOD", "AESEV"))
  expect_identical(lby_arg(x, "plan_listing")$cols[[3L]]$args, list(vars = "AESEV"))
  expect_identical(lby_arg(x, "plan_hide"), list(cols = list("AESEV")))
  # a page per value, 22 rows within it; not printed, so the page says which value
  expect_identical(lby_arg(x, "plan_paginate_rows"),
                   list(max_rows = 22L, page_by = "AESEV", heading = "AESEV: {value}", heading_at = "title"))
  # printed (a column of just it): not hidden, not named
  y <- lby_x(columns = c(lby_cols, list(lby_sev)), by = "AESEV")
  expect_identical(lby_fns(y), c("plan_listing", "plan_paginate_rows"))
  expect_identical(lby_listing_vars(y), c("USUBJID", "AEDECOD", "AESEV"))
  expect_identical(lby_arg(y, "plan_paginate_rows"), list(max_rows = 22L, page_by = "AESEV"))
  # by_heading names the page, printed or not
  z <- lby_x(columns = c(lby_cols, list(lby_sev)), by = "AESEV", by_heading = "Severity: {value}")
  expect_identical(lby_arg(z, "plan_paginate_rows")$heading, "Severity: {value}")
  expect_identical(lby_arg(z, "plan_paginate_rows")$heading_at, "title")
  # a variable inside a column of several is not a column of it
  w <- lby_x(columns = list(list(vars = c("USUBJID", "AESEV"))), by = "AESEV")
  expect_identical(lby_listing_vars(w), c("USUBJID+AESEV", "AESEV"))
  expect_identical(length(lby_arg(w, "plan_listing")$cols), 2L)
  expect_true("plan_hide" %in% lby_fns(w))
  # several: a token for each, by its name
  s <- lby_x(columns = lby_cols, by = c("AESEV", "SITEID"))
  pr <- lby_arg(s, "plan_paginate_rows")
  expect_identical(pr$page_by, list("AESEV", "SITEID"))
  expect_identical(pr$heading, "AESEV: {AESEV}\nSITEID: {SITEID}")
  expect_identical(lby_arg(s, "plan_hide")$cols, list("AESEV", "SITEID"))
  # one printed, one not: both named
  m <- lby_x(columns = c(lby_cols, list(lby_sev)), by = c("AESEV", "SITEID"))
  expect_identical(lby_arg(m, "plan_hide")$cols, list("SITEID"))
  expect_identical(lby_arg(m, "plan_paginate_rows")$heading, "AESEV: {AESEV}\nSITEID: {SITEID}")
})

test_that("listing by: the records are ordered by the variables first, then sort", {
  srt <- function(...) lby_x(columns = lby_cols, ...)$data$sort
  expect_identical(srt(by = "AESEV"), "AESEV")
  expect_identical(srt(by = "AESEV", sort = c("USUBJID", "AEDECOD")), c("AESEV", "USUBJID", "AEDECOD"))
  # a `sort` that has the variable later: it moves first
  expect_identical(srt(by = "AESEV", sort = c("USUBJID", "AESEV")), c("AESEV", "USUBJID"))
  # and keeps its direction
  expect_identical(srt(by = "AESEV", sort = c("-AESEV", "USUBJID")), c("-AESEV", "USUBJID"))
  expect_identical(srt(by = c("AESEV", "SITEID"), sort = c("USUBJID", "-SITEID")),
                   c("AESEV", "-SITEID", "USUBJID"))
  # no by: the sort as given
  expect_identical(srt(sort = c("-USUBJID", "AEDECOD")), c("-USUBJID", "AEDECOD"))
})

test_that("listing by block: one variable's records together; its column carries the value", {
  x <- lby_x(columns = c(lby_cols, list(lby_sev)), by = "AESEV", by_break = "block")
  expect_identical(lby_fns(x), c("plan_listing", "plan_row_group", "plan_paginate_rows"))
  expect_identical(lby_arg(x, "plan_row_group"), list(mode = "value", group_col = "AESEV"))
  expect_identical(lby_arg(x, "plan_paginate_rows"), list(max_rows = 22L))
  expect_true(lby_arg(x, "plan_listing")$cols[[3L]]$args$collapse_repeats)
  expect_null(lby_arg(x, "plan_listing")$cols[[1L]]$args$collapse_repeats)
  expect_identical(x$data$sort, "AESEV")
  # not printed: a hidden column of its own, carried the same
  y <- lby_x(columns = lby_cols, by = "AESEV", by_break = "block")
  expect_identical(lby_fns(y), c("plan_listing", "plan_hide", "plan_row_group", "plan_paginate_rows"))
  expect_identical(lby_arg(y, "plan_listing")$cols[[3L]]$args, list(vars = "AESEV", collapse_repeats = TRUE))
  expect_identical(lby_arg(y, "plan_hide"), list(cols = list("AESEV")))
})

test_that("listing by: what it will not take", {
  expect_error(lby_x(columns = lby_cols, by = c("AESEV", "SITEID"), by_break = "block"), "one `by` variable")
  expect_error(lby_x(columns = lby_cols, by = "AESEV", by_break = "block", by_heading = "{value}"),
               "for `by_break` \"page\"")
  expect_error(lby_x(columns = lby_cols, by_heading = "{value}"), "'by_heading' is about `by`")
  expect_error(lby_x(columns = lby_cols, by_break = "block"), "'by_break' is about `by`")
  expect_error(lby_x(columns = lby_cols, by = "AESEV", by_break = "pages"), "`by_break` is \"page\"")
  expect_error(lby_x(columns = lby_cols, by = "AESEV", by_heading = c("a", "b")), "one text")
})

test_that("listing by: the data must have the variables, said so", {
  dat <- lby_data()
  expect_error(tpl_listing(dat, data = "ADAE", by = "NOSUCH", columns = lby_cols),
               "tpl_listing(): `by` names 'NOSUCH', which the data does not have", fixed = TRUE)
  expect_error(tfl_tpl(dat, "listing", data = "ADAE", by = c("AESEV", "NOSUCH"), columns = lby_cols),
               "tfl_tpl(): `by` names 'NOSUCH', which the data does not have", fixed = TRUE)
})

# the pages of a plan: each page's events (the invented records' numbers)
lby_events <- function(pages) {
  lapply(pages, function(pg) as.integer(sub("Event ", "", grep("^Event", unlist(pg$data), value = TRUE))))
}

test_that("listing by page and block, as the plan pages it", {
  skip_if_not_installed("rtfreporter")
  dat <- lby_data()
  plan <- function(...) rtfreporter::plan_apply(suppressMessages(tpl_listing(dat, data = "ADAE", ...)))
  # no by: the pages cut at 22 rows, inside the severities
  pn <- plan(columns = lby_cols)
  expect_identical(unname(lby_events(pn)), list(1:11, 12L))
  # a page a value, named after it when it is not printed
  pp <- plan(columns = lby_cols, by = "AESEV")
  expect_identical(unname(lby_events(pp)), list(1:4, 5:9, 10:12))
  expect_identical(names(pp), c("AESEV: MILD", "AESEV: MODERATE", "AESEV: SEVERE"))
  expect_false("AESEV" %in% names(pp[[1L]]$data))
  # a block: SEVERE does not start on page 1 and go on, it moves whole to page 2
  pb <- plan(columns = c(lby_cols, list(lby_sev)), by = "AESEV", by_break = "block")
  expect_identical(unname(lby_events(pb)), list(1:9, 10:12))
  expect_identical(vapply(pb, function(pg) nrow(pg$data), 1L, USE.NAMES = FALSE), c(18L, 6L))
  # by is a variable of the data, not necessarily a column: the block is kept the same
  pc <- plan(columns = lby_cols, by = "AESEV", by_break = "block")
  expect_identical(unname(lby_events(pc)), list(1:9, 10:12))
  expect_false("AESEV" %in% names(pc[[1L]]$data))
})

# a study of the invented records, the report a listing template, its RTF
lby_run <- function(args, rows = seq_len(12L), extends = "none", env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  dir.create(file.path(d, "adam"))
  haven::write_xpt(lby_data()[rows, ], file.path(d, "adam", "adae.xpt"))
  haven::write_xpt(.ai_chr(tflspec::adsl), file.path(d, "adam", "adsl.xpt"))
  suppressMessages(tfl_ai_study_init(d, adam = "adam", study_id = "XYZ"))
  o <- tfl_output("L-1", "listing", label = "AEs", extends = extends,
                  template = list(name = "listing", args = c(list(data = "ADAE"), args)),
                  page = list(titles = list(`1` = list(center = "Listing 1"))))
  tfl_write_output(o, file.path(d, "spec", "outputs", "L-1.yml"))
  f <- withr::with_dir(d, suppressMessages(tfl_run_output("L-1", d, overwrite = TRUE)))
  x <- paste(readLines(f, warn = FALSE), collapse = "\n")
  # the page breaks (a section or a page command) cut the RTF into its pages:
  # each page's events and heading line
  secs <- strsplit(x, "\\\\(sect|page)(?![a-z])", perl = TRUE)[[1L]]
  secs <- secs[grepl("Event [0-9]+", secs)]
  list(events = lapply(secs, function(s)
         as.integer(sub("Event ", "", regmatches(s, gregexpr("Event [0-9]+", s))[[1L]]))),
       heading = vapply(secs, function(s) {
         h <- regmatches(s, regexpr("(AESEV|Severity): [A-Z]+", s))
         if (length(h)) h else NA_character_
       }, "", USE.NAMES = FALSE),
       n = function(txt) vapply(secs, function(s) lengths(regmatches(s, gregexpr(txt, s, fixed = TRUE))), 1L,
                                    USE.NAMES = FALSE),
       rtf = x, dir = d)
}
lby_skip <- function() {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("rtfreporter")
  skip_if_not_installed("withr")
}

test_that("listing by page: the built RTF breaks at each value, the page named", {
  lby_skip()
  # (no by: the break falls inside a value)
  r0 <- lby_run(list(columns = lby_cols))
  expect_identical(r0$events, list(1:11, 12L))
  # the records in the data in the opposite order: the pages are still in the variable's order
  r1 <- lby_run(list(columns = lby_cols, by = "AESEV", sort = "AEDECOD"), rows = 12:1)
  expect_identical(r1$events, list(1:4, 5:9, 10:12))
  expect_identical(r1$heading, c("AESEV: MILD", "AESEV: MODERATE", "AESEV: SEVERE"))
  # the variable is not a column: no column heading, no value in the rows
  expect_false(grepl("Severity", r1$rtf, fixed = TRUE))
  expect_identical(r1$n("MODERATE"), c(0L, 1L, 0L))
  # printed, named by the report: its own words
  r2 <- lby_run(list(columns = c(lby_cols, list(lby_sev)), by = "AESEV", sort = "AEDECOD",
                     by_heading = "Severity: {value}"))
  expect_identical(r2$events, list(1:4, 5:9, 10:12))
  expect_identical(r2$heading, c("Severity: MILD", "Severity: MODERATE", "Severity: SEVERE"))
  # printed, not named: the column says it
  r3 <- lby_run(list(columns = c(lby_cols, list(lby_sev)), by = "AESEV", sort = "AEDECOD"))
  expect_identical(r3$events, list(1:4, 5:9, 10:12))
  expect_true(all(is.na(r3$heading)))
  expect_true(grepl("Severity", r3$rtf, fixed = TRUE))
  # two variables: a page for each pair that is there
  r4 <- lby_run(list(columns = lby_cols, by = c("AESEV", "SITEID"), sort = "AEDECOD"))
  expect_identical(r4$events, list(c(1L, 3L), c(2L, 4L), c(5L, 7L, 9L), c(6L, 8L), 11L, c(10L, 12L)))
})

test_that("listing by block: the built RTF keeps a value together on a page", {
  lby_skip()
  # no by: SEVERE starts on page 1 and goes on (22 rows, 11 records)
  expect_identical(lby_run(list(columns = lby_cols))$events, list(1:11, 12L))
  r <- lby_run(list(columns = c(lby_cols, list(lby_sev)), by = "AESEV", by_break = "block", sort = "AEDECOD"))
  expect_identical(r$events, list(1:9, 10:12))
  # each value printed once, at the top of its block
  expect_identical(r$n("MILD"), c(1L, 0L))
  expect_identical(r$n("MODERATE"), c(1L, 0L))
  expect_identical(r$n("SEVERE"), c(0L, 1L))
  # not a column: kept together all the same, nothing of it printed
  u <- lby_run(list(columns = lby_cols, by = "AESEV", by_break = "block", sort = "AEDECOD"))
  expect_identical(u$events, list(1:9, 10:12))
  expect_false(any(grepl("SEVERE|MODERATE|MILD", u$rtf)))
})

test_that("listing by: the missing variable, said by the study run", {
  lby_skip()
  expect_error(lby_run(list(columns = lby_cols, by = "NOSUCH")),
               "`by` names 'NOSUCH', which the dataset ADAE does not have")
})

test_that("listing by: as code, as items in a file and on the sheets, nothing lost", {
  lby_skip()
  d <- withr::local_tempdir()
  dir.create(file.path(d, "adam"))
  haven::write_xpt(lby_data(), file.path(d, "adam", "adae.xpt"))
  haven::write_xpt(.ai_chr(tflspec::adsl), file.path(d, "adam", "adsl.xpt"))
  suppressMessages(tfl_ai_study_init(d, adam = "adam", study_id = "XYZ"))
  st <- tfl_read_study_spec(d)
  # (a single-item list and a text are the same in a file)
  norm <- function(a) {
    if (inherits(a, "tfl_call")) { a$args <- norm(a$args); return(a) }
    if (!is.list(a)) return(a)
    a <- lapply(a, norm)
    if (is.null(names(a)) && all(vapply(a, function(z) is.character(z) && length(z) == 1L, NA))) unlist(a) else a
  }
  key <- function(items) sort(vapply(items, function(it)
    paste(it$fn, paste(deparse(norm(it$args)[order(names(it$args))]), collapse = "")), ""))
  for (args in list(
    list(columns = lby_cols, by = "AESEV", sort = "AEDECOD"),
    list(columns = c(lby_cols, list(lby_sev)), by = "AESEV", by_heading = "Severity: {value}"),
    list(columns = lby_cols, by = c("AESEV", "SITEID")),
    list(columns = c(lby_cols, list(lby_sev)), by = "AESEV", by_break = "block"),
    list(columns = lby_cols, by = "AESEV", by_break = "block", sort = "USUBJID"))) {
    o <- tfl_output("L-1", "listing", label = "AEs", extends = "none",
                    template = list(name = "listing", args = c(list(data = "ADAE"), args)))
    ro <- tfl_resolve_output(o, st)
    # the template's call keeps the arguments, as the report's file gives them
    expect_identical(attr(ro, "template")$args$by, args$by)
    expect_identical(attr(ro, "template")$args$by_break, args$by_break)
    expect_identical(attr(ro, "template")$args$by_heading, args$by_heading)
    # the program: the sort first by the variables, the divisions in the plan
    code <- paste(tfl_content_code(ro, study = st), collapse = "\n")
    expect_match(code, paste0("dplyr::arrange(", args$by[1L]), fixed = TRUE)
    expect_match(code, if (identical(args$by_break, "block")) "plan_row_group(" else "page_by = ",
                 fixed = TRUE)
    # to items in a file
    b <- tfl_template_to_spec(o, st)
    expect_null(b$template)
    expect_identical(key(b$content), key(Filter(function(it) it$from != "defaults", ro$content)))
    expect_identical(unlist(b$data$sort), unlist(ro$data$sort))
    f <- file.path(withr::local_tempdir(), "L-1.yml")
    tfl_write_output(b, f)
    back <- tfl_read_output(f)
    expect_identical(key(back$content), key(b$content))
    expect_identical(unlist(back$data$sort), unlist(b$data$sort))
    # and on the sheets, back to a file
    sh <- suppressMessages(tfl_sheets_from_spec(st, list(`L-1` = b)))
    to <- withr::local_tempdir()
    suppressMessages(tfl_yaml_from_sheets(sh, to, overwrite = TRUE))
    sb <- tfl_read_outputs(to)[["L-1"]]
    expect_identical(key(sb$content), key(b$content))
    expect_identical(unlist(sb$data$sort), unlist(b$data$sort))
  }
})

test_that("summary has no mean (CI) analysis of its own; km_table and orr", {
  x <- tfl_template_expand("summary", list(population = "SAF", continuous = "CHG",
                                           continuous_statistics = "N"))
  expect_identical(vapply(x$ard$analyses, `[[`, "", "method"), c("categorical", "continuous"))
  k <- tfl_template_expand("km_table", list(by_levels = c("A", "B")))
  expect_null(k$ard)   # the survival analyses are the report's own
  expect_s3_class(k$data$steps[[1L]], "tfl_r")
  expect_match(k$data$steps[[1L]], "Median (95% CI)", fixed = TRUE)
  expect_false(grepl("== \"0\"", tfl_template_expand("km_table", list(drop_time_0 = FALSE))$data$steps[[1L]]))
  o <- tfl_template_expand("orr", list(responders = c("CR", "PR", "SD")))
  # (every subject of the set a row: the set's subjects, then the records joined to them)
  expect_identical(vapply(o$ard$analysis_data, `[[`, "", "id"),
                   c("adsl_itt", "adrs_bor_records", "adrs_bor"))
  expect_match(as.character(o$ard$analysis_data[[3L]]$code),
               'dplyr::mutate(RESP = AVALC %in% c("CR", "PR", "SD"))', fixed = TRUE)
  expect_identical(tpl_item(o, "plan_labels")$args$variable$RESP, "Objective response rate (CR + PR + SD)")
  expect_true(all(c("km_table", "orr") %in% tfl_templates()$name))
  # vs_change's table is param_change(baseline = TRUE), test-tpl-param-change.R
  expect_false("vs_change" %in% tfl_templates()$name)
})

test_that("hierarchical of one level; demog and ae_pt_freq on their layouts", {
  # one level: the term alone, no row keys (it stopped: the keys' names)
  h <- tfl_template_expand("hierarchical", list(population = "SAF", hierarchy = "AEDECOD"))
  tp <- h$content[[2L]]$args
  expect_null(tp$rows)
  expect_identical(tp$label, list(label = "AEDECOD"))
  # ae_pt_freq is hierarchical's ARD and items of one level, with its own
  # order, filter and page
  p <- tfl_template_expand("ae_pt_freq", list(population = "SAF", min_pct = 5))
  h1 <- tfl_template_expand("hierarchical", list(population = "SAF", hierarchy = "AEDECOD",
    where = "TRTEMFL == \"Y\"", any_row = TRUE, any_row_label = "Any TEAE"))
  # (its own ids and labels: the layout's are its parameters no more)
  k <- c("method", "data", "by", "variables", "denominator", "args")
  expect_identical(p$ard$analyses[[1L]][k], h1$ard$analyses[[1L]][k])
  expect_identical(p$ard$analyses[[1L]][c("id", "label")], list(id = "TEAE_PT", label = "TEAE by PT"))
  expect_identical(p$ard$analysis_data[[2L]]$where, h1$ard$analysis_data[[2L]]$where)
  expect_identical(vapply(p$content, `[[`, "", "fn"),
                   c("normalize_ard", "table_plan", "plan_sort", "plan_filter", "plan_col_header",
                     "plan_paginate_rows"))
  # demog is a template of its own (no longer summary's variant): its population is the report's
  d <- tfl_template_expand("demog", dmv(categorical = "SEX"))
  expect_identical(d$ard$analysis_data[[1L]]$id, "adsl_saf")
  # (and its one more set, #770's: the categorical variables with a Missing level)
  expect_identical(d$ard$analysis_data[[2L]]$id, "adsl_saf_m")
})

test_that("ae_overview: a subjects analysis and a row a flag", {
  x <- tfl_template_expand("ae_overview")
  expect_identical(vapply(x$ard$analyses, `[[`, "", "method"), c("categorical", rep("subjects", 6)))
  expect_null(x$ard$analyses[[2L]]$where)   # ANY: every event kept
  expect_identical(as.character(x$ard$analyses[[5L]]$where), 'AESER == "Y"')
  expect_identical(as.character(x$ard$analysis_data[[2L]]$where), 'TRTEMFL == "Y"')
  expect_identical(x$content[[1L]]$args$label, NA)   # no label column
  ser <- list(list(id = "SER", label = "Serious", where = 'AESER == "Y"'))
  f <- tfl_template_expand("ae_overview", list(flags = ser, population = "ITT"))
  expect_identical(tpl_item(f, "plan_labels")$args, list(variable = list(SER = "Serious")))
  expect_identical(f$ard$analysis_data[[2L]]$id, "adae_itt")
  skip_if_not_installed("cards")
  saf <- adsl[adsl$SAFFL == "Y", ]
  te <- adae[adae$TRTEMFL == "Y", ]
  saf$SER <- saf$USUBJID %in% te$USUBJID[te$AESER == "Y"]
  ard <- cards::bind_ard(
    cards::ard_tabulate(saf, variables = TRT01A),
    cards::ard_tabulate_value(saf, by = TRT01A, variables = SER, value = list(SER = TRUE)))
  p <- suppressMessages(tpl_ae_overview(ard, flags = ser))
  expect_s3_class(p, "table_plan")
  tab <- suppressMessages(rtfreporter::plan_apply(p, "table"))
  expect_identical(nrow(tab), 1L)
  expect_identical(as.character(tab[[1L]]), "Serious")
})

test_that("demog: a categorical variable the labels do not name has no label row (no `<NA> = NA`; the sheets convert)", {
  x <- tfl_template_expand("demog", dmv(categorical = c("AGEGR1", "SEX", "RACE", "ETHNIC", "ECOGBL")))
  lab <- Filter(function(it) identical(it$fn, "plan_labels"), x$content)
  expect_identical(length(lab), 1L)
  v <- lab[[1L]]$args$variable
  expect_false(anyNA(names(v)))
  expect_false("ECOGBL" %in% names(v))
  expect_true(all(c("AGE", "AGEGR1", "SEX") %in% names(v)))
})

# ---- a map written as a list of {name, text} (#637) -------------------------

nt <- function(...) {
  x <- list(...)
  unname(Map(function(n, t) list(name = n, text = t), names(x), x))
}

test_that("a map of name and text also takes a list of {name, text}: the mapping's expansion", {
  pl <- function(e) Filter(function(it) it$fn == "plan_labels", e$content)
  for (tn in c("shift", "lab_shift")) {
    extra <- if (tn == "lab_shift") list(visit = "Week 24", params = c("ALT", "HGB")) else list()
    m <- tfl_template_expand(tn, c(list(population = "SAF",
                                        param_labels = list(ALT = "Alanine Aminotransferase (U/L)", HGB = "Hemoglobin")), extra))
    l <- tfl_template_expand(tn, c(list(population = "SAF", param_labels = nt(
      ALT = "Alanine Aminotransferase (U/L)", HGB = "Hemoglobin")), extra))
    expect_identical(l, m, info = tn)
    expect_identical(pl(l)[[1L]]$args$PARAMCD$ALT, "Alanine Aminotransferase (U/L)", info = tn)
  }
  # labels, merged with the default's: only these names change
  m <- tfl_template_expand("demog", dmv(labels = list(AGE = "Age (y)")))
  l <- tfl_template_expand("demog", dmv(labels = nt(AGE = "Age (y)")))
  expect_identical(l, m)
  d <- tfl_template_expand("demog", dmv())
  a <- unlist(pl(l)[[1L]]$args)
  a0 <- unlist(pl(d)[[1L]]$args)
  expect_identical(names(a), names(a0))
  expect_identical(unname(a[names(a) != "AGE" & !endsWith(names(a), ".AGE")]),
                   unname(a0[names(a0) != "AGE" & !endsWith(names(a0), ".AGE")]))
  expect_true(any(a == "Age (y)"))
  expect_false(any(a0 == "Age (y)"))
  # subgroup_labels (forest_hr) and worst_by (shift)
  fa <- list(population = "SAF", param = "PFS")
  m <- tfl_template_expand("forest_hr", c(fa, list(subgroup_labels = list(SEX = "Gender"))))
  l <- tfl_template_expand("forest_hr", c(fa, list(subgroup_labels = nt(SEX = "Gender"))))
  expect_identical(l, m)
  expect_match(as.character(l$data$steps[[length(l$data$steps)]]$code %||% unlist(l$data)),
               'sg_label <- c(SEX = "Gender"', fixed = TRUE)
  m <- tfl_template_expand("shift", list(population = "SAF", worst_by = list(HGB = "LOW, HIGH, NORMAL")))
  l <- tfl_template_expand("shift", list(population = "SAF", worst_by = nt(HGB = "LOW, HIGH, NORMAL")))
  expect_identical(l, m)
  expect_match(as.character(l$ard$analysis_data[[1L]]$code),
               'PARAMCD == "HGB" ~ match(ANRIND, c("LOW", "HIGH", "NORMAL"))', fixed = TRUE)
})

test_that("demog: labels as a list of {name, text} are read like the mapping, the default labels kept (#640)", {
  lab <- function(e) Filter(function(it) it$fn == "plan_labels", e$content)[[1L]]$args$variable
  args <- list(population = "SAF", by = "TRT01A", continuous = "AGE", categorical = c("AGEGR1", "SEX", "RACE", "ETHNIC"))
  m <- tfl_template_expand("demog", c(args, list(labels = list(
    AGE = "Age (years)", SEX = "Sex, n (%)", RACE = "Race, n (%)"))))
  # the list as a YAML draft writes it, read by yaml
  y <- yaml::yaml.load(paste0(
    "labels:
",
    "  - {name: AGE, text: \"Age (years)\"}
",
    "  - {name: SEX, text: \"Sex, n (%)\"}
",
    "  - {name: RACE, text: \"Race, n (%)\"}
"))
  l <- tfl_template_expand("demog", c(args, y))
  expect_identical(l, m)
  # the three given, the template's others kept (never the variable names)
  d <- lab(tfl_template_expand("demog", args))
  expect_identical(lab(l)[c("AGE", "SEX", "RACE")],
                   list(AGE = "Age (years)", SEX = "Sex, n (%)", RACE = "Race, n (%)"))
  expect_identical(lab(l)[c("AGEGR1", "ETHNIC")], d[c("AGEGR1", "ETHNIC")])
  expect_identical(lab(l)[["ETHNIC"]], "Ethnicity, n (%)")
})

test_that("a map given as a list that is not {name, text} stops, naming the parameter and the mapping", {
  ex <- function(v) tfl_template_expand("shift", list(population = "SAF", param_labels = v))
  expect_error(ex(list(list(name = "ALT"))), "parameter 'param_labels'.*`name` and `text` only.*mapping.*param_labels: [{]ALT")
  expect_error(ex(list(list(name = "ALT", text = "a", unit = "U/L"))), "parameter 'param_labels'.*mapping")
  expect_error(ex(list(list(name = "ALT", text = "a"), list(name = "ALT", text = "b"))),
               "parameter 'param_labels'.*`name` 'ALT' is given twice.*mapping")
  expect_error(ex(list(list(name = "ALT", text = "a"), "HGB")), "parameter 'param_labels'.*mapping")
  # as they were: a mapping, a character vector, NULL
  expect_no_error(ex(list(ALT = "a")))
  expect_no_error(ex(c(ALT = "a")))
  expect_no_error(ex(NULL))
  # a several-records parameter is untouched: flags, a list of records
  x <- tfl_template_expand("ae_overview", list(population = "SAF",
    flags = list(list(id = "SER", label = "Serious", where = 'AESER == "Y"'))))
  expect_true(length(x$ard$analyses) > 0L)
})

test_that("the kit prints a map of name and text as a mapping, and checks the list form", {
  txt <- strsplit(tflspec:::.ai_templates_file_text(), "
", fixed = TRUE)[[1L]]   # (ai/templates.md)
  expect_true(any(grepl("`param_labels` (mapping, name: text) = ~", txt, fixed = TRUE)))
  expect_true(any(grepl("`subgroup_labels` (mapping, name: text) = ~", txt, fixed = TRUE)))
  expect_true(any(grepl("`worst_by` (mapping, name: text)", txt, fixed = TRUE)))
  expect_false(any(grepl("list {name, text}", txt, fixed = TRUE)))
  expect_true(any(grepl("`flags` (list, several {id, label, where})", txt, fixed = TRUE)))
  expect_true(any(grepl("`cells` (list {label, template})", txt, fixed = TRUE)))
  chk <- function(args) {
    a <- tfl_ai_parse(paste(c("```yaml", "tflspec_ai: {task: table, version: 1, outputs: [T-1]}",
      "outputs:", "  - id: T-1", "    kind: table", "    template:", "      name: shift",
      paste0("      args: ", args), "assumptions: []", "```"), collapse = "\n"), "table")
    tfl_ai_check(a, tfl_ai_context("table", outputs = "T-1"))
  }
  p <- chk("{population: SAF, param_labels: [{name: ALT, text: Alanine Aminotransferase (U/L)}]}")
  expect_false(any(grepl("plan_labels|param_labels", p$problem)), info = paste(p$problem, collapse = "; "))
  p <- chk("{population: SAF, param_labels: [{name: ALT}]}")
  expect_true(any(grepl("param_labels", p$problem, fixed = TRUE) & p$severity == "error"))
  expect_false(any(grepl("plan_labels", p$problem, fixed = TRUE)))
  expect_true(any(grepl("mapping", p$problem, fixed = TRUE)))
})

test_that("tfl_read_template_sheets(): the population column (or the args cell's) is the report's population:", {
  skip_if_not_installed("writexl")
  f <- withr::local_tempfile(fileext = ".xlsx")
  t <- data.frame(output_id = c("A", "B", "C", "D", "E"), kind = "table", label = "x",
                  population = c("ITT", NA, "SAF", "SAF", "ITT"),
                  template = c("demog", "demog", "demog", "demog", "summary"), by = "TRT01A", where = NA,
                  args = c(NA, "population=ITT", "population=SAF", "population=ITT", NA))
  rows <- data.frame(output_id = c("A", "B", "C", "D", "E"), param = "continuous", value = "AGE", label = NA)
  writexl::write_xlsx(list(templates = t, template_rows = rows), f)
  # (D: two different ones stop; the others read)
  expect_error(tfl_read_template_sheets(f), "give it once", fixed = TRUE)
  t <- t[t$output_id != "D", ]
  writexl::write_xlsx(list(templates = t, template_rows = rows), f)
  r <- tfl_read_template_sheets(f)
  # (demog takes the analysis set from the report: the column, else the cell's, never an argument)
  expect_identical(vapply(r[c("A", "B", "C")], function(x) x$population, ""), c(A = "ITT", B = "ITT", C = "SAF"))
  expect_null(r$A$template$args$population)
  expect_null(r$B$template$args$population)
  # (a template that has the argument keeps it)
  expect_identical(r$E$population, "ITT")
  expect_identical(r$E$template$args$population, "ITT")
})
