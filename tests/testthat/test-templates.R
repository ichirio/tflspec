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

test_that("tfl_templates() lists demog and summary", {
  t <- tfl_templates()
  expect_true(all(c("demog", "summary") %in% t$name))
  expect_identical(t$layout[t$name == "demog"], "summary")
})

test_that("tfl_template_expand(): the items, marked as the template's", {
  x <- tfl_template_expand("demog", list(population = "SAF"))
  expect_identical(vapply(x$content, `[[`, "", "fn"),
                   c("table_plan", "plan_cells", "plan_labels", "plan_paginate_rows"))
  expect_true(all(vapply(x$content, `[[`, "", "from") == "template:demog"))
  expect_identical(x$ard$analysis_data[[1L]]$id, "adsl_saf")
  expect_identical(vapply(x$ard$analyses, `[[`, "", "id"), c("GROUPN", "CONT", "CAT"))
  s <- tfl_template_expand("demog", list(population = "SAF", stack = TRUE))
  expect_identical(vapply(s$ard$analyses, `[[`, "", "id"), c("STACK", "CONT", "CAT"))
  expect_identical(s$ard$analyses[[2L]]$parent, "STACK")
  # the code lists are the ARD's (applied when it is made): no plan_levels()
  cl <- list(SEX = list(list(value = "F", label = "Female"), list(value = "M", label = "Male")))
  y <- tfl_template_expand("demog", list(population = "SAF"), codelists = cl)
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
  expect_error(tfl_template_expand("demog", list(population = "SAF", nope = 1)), "no parameter")
  expect_error(tfl_template_expand("demog", list()), "needs")
  expect_error(tfl_template_expand("nope"), "no template")
})

test_that("tpl_demog() / tpl_summary(): the object is the items run on the ARD", {
  ard <- tpl_ard()
  p <- suppressMessages(tpl_demog(ard, population = "SAF"))
  expect_s3_class(p, "table_plan")
  # the same as the items, run as a report's content
  x <- tfl_template_expand("demog", list(population = "SAF"))
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
  x <- tfl_template_expand("demog", a)
  y <- tfl_template_expand("demog", list(population = "SAF"))
  expect_identical(x$content, y$content)
  out <- withr::local_tempfile(fileext = ".yml")
  tfl_write_template_yaml(r[["T-14-1-1"]], out)
  back <- yaml::read_yaml(out)
  expect_identical(back$template$name, "demog")
  expect_identical(unlist(back$template$args$categorical), a$categorical)
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
  expect_identical(as.character(x$ard$analysis_data[[1L]]$where),
                   'AVISIT == "CYCLE 2 DAY 1" & PARAMCD %in% c("ALT", "AST")')
  expect_identical(x$ard$analyses[[1L]]$by, c("TRT01A", "PARAMCD", "BNRIND"))
  expect_identical(x$content[[1L]]$args$cols, c("TRT01A", ".label"))
  s <- tfl_template_expand("shift", list(population = "SAF", by_levels = c("A", "B")))
  expect_identical(s$content[[3L]]$args$TRT01A, c("A", "B"))
  expect_error(tfl_template_expand("lab_shift", list(population = "SAF")), "needs")
  expect_true(all(c("shift", "lab_shift") %in% tfl_templates()$name))
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
  expect_identical(as.character(o$ard$analysis_data[[1L]]$derive),
                   'RESP = AVALC %in% c("CR", "PR", "SD")')
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
  # demog's usual table is summary's
  d <- tfl_template_expand("demog", list(population = "SAF", continuous = "AGE", categorical = "SEX"))
  s <- tfl_template_expand("summary", list(population = "SAF", continuous = "AGE", categorical = "SEX",
    labels = list(AGE = "Age (years)", SEX = "Sex, n (%)")))
  no_from <- function(x) lapply(x, function(i) i[setdiff(names(i), "from")])
  expect_identical(no_from(d$content), no_from(s$content))   # (each item says its template)
  expect_identical(d$ard$analysis_data, s$ard$analysis_data)
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
