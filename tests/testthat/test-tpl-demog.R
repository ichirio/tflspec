# demog and stf_t02_demog under the new design (migration batch 1): two templates
# of their own, the variables and `by` required, the analysis set the report's,
# the formats on the ARD side, one value a parameter (the study standard reaches
# them through references only), `!unset` per parameter

b1_study <- function() tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))
b1_args <- function(...) utils::modifyList(
  list(population = "SAF", by = "TRT01A", continuous = "AGE", categorical = c("AGEGR1", "SEX")),
  list(...))
b1_fda <- function(...) utils::modifyList(
  list(population = "SAF", by = "TRT01A", continuous = "AGE",
       categorical = c("SEX", "AGEGR1", "RACE", "ETHNIC", "COUNTRY")), list(...))
b1_item <- function(x, fn) Filter(function(it) identical(it$fn, fn), x$content)
b1_out <- function(name, args, ..., id = "T-9") {
  # (no `extends`: the template's, stf_t02_demog's content step fixed to none)
  tfl_output(id, "table", population = "SAF",
             template = list(name = name, args = args), ...)
}

# A study folder on the sample ADSL (factors as text), with a few values missing:
# RACE of three subjects, SEX of two
b1_defaults <- c("content:", "  - normalize_ard: {}", "  - plan_cells: {categorical: \"{n} ({p})\"}",
                 "  - plan_stub: {name: row_label, before: true}",
                 "  - plan_blanks: {where: between_groups}")
b1_folder <- function(env = parent.frame(), na = TRUE, defaults = b1_defaults) {
  testthat::skip_if_not_installed("haven")
  d <- withr::local_tempdir(.local_envir = env)
  dir.create(file.path(d, "adam"))
  sl <- tflspec:::.ai_chr(tflspec::adsl)
  if (na) {
    sl$RACE[which(sl$SAFFL == "Y")[1:3]] <- ""
    sl$SEX[which(sl$SAFFL == "Y")[4:5]] <- NA
  }
  haven::write_xpt(sl, file.path(d, "adam", "adsl.xpt"))
  suppressMessages(tfl_ai_study_init(d, adam = "adam", study_id = "B1-01"))
  writeLines(defaults, file.path(d, "spec", "defaults.yml"))
  d
}
b1_run <- function(d, o) {
  tfl_write_output(o, file.path(d, "spec", "outputs", "T-9.yml"))
  st <- tfl_read_study_spec(d)
  ard <- suppressMessages(tfl_build_ard(st, output_id = "T-9", dir = d, save = FALSE))
  ro <- tfl_resolve_output(tfl_read_outputs(d)[["T-9"]], st)
  p <- suppressMessages(tfl_content_run(ro, ard))
  list(table = as.data.frame(suppressMessages(rtfreporter::plan_apply(p, "table"))),
       st = st, ro = ro, ard = ard)
}

test_that("demog and stf_t02_demog are templates of their own: no variant, no parent", {
  for (n in c("demog", "stf_t02_demog")) {
    t <- tflspec:::.tpl_registry[[n]]
    expect_null(t$from)
    expect_null(t$root)
    expect_identical(t$body, tflspec:::.tpl_demog_body)
    expect_identical(tflspec:::.tpl_get(n, "t")$params, t$params)
  }
  expect_false(any(c("demog", "stf_t02_demog") %in% tfl_templates()$name[!is.na(tfl_templates()$from)]))
  expect_match(tflspec:::.tpl_registry$stf_t02_demog$label, "(FDA STF Table 2)", fixed = TRUE)
  # the summary is as it was
  expect_null(tflspec:::.tpl_registry$summary$from)
})

test_that("the standalone templates equal what the variants gave: defaults and the table", {
  d <- b1_folder(na = FALSE)
  # the variants as they were (of summary, of demog)
  tfl_template_variant("b1_old_demog", from = "summary", label = "old",
    defaults = list(continuous = "AGE", categorical = c("AGEGR1", "SEX", "RACE", "ETHNIC"),
                    labels = c(AGE = "Age (years)", AGEGR1 = "Age group, n (%)",
                               SEX = "Sex, n (%)", RACE = "Race, n (%)", ETHNIC = "Ethnicity, n (%)")))
  tfl_template_variant("b1_old_stf", from = "b1_old_demog", label = "old",
    defaults = list(categorical = c("SEX", "AGEGR1", "RACE", "ETHNIC", "COUNTRY"),
                    order = c("SEX", "AGE", "AGEGR1", "RACE", "ETHNIC", "COUNTRY"),
                    labels = c(SEX = "Sex, n (%)", AGE = "Age, years",
                               AGEGR1 = "Age groups (years), n (%)", RACE = "Race, n (%)",
                               ETHNIC = "Ethnicity, n (%)", COUNTRY = "Country, n (%)"),
                    cells = list(`Mean (SD)` = "{mean:.1f} ({sd:.2f})",
                                 `Median (min, max)` = "{median:.1f} ({min:.0f}, {max:.0f})"),
                    total = TRUE))
  withr::defer(rm(list = c("b1_old_demog", "b1_old_stf"), envir = tflspec:::.tpl_registry))
  new <- tfl_template_params("demog")
  old <- tfl_template_params("b1_old_demog")
  # the defaults that stay: the labels, the dataset, no Total
  for (k in c("labels", "data", "total", "subset")) expect_identical(new[[k]]$default, old[[k]]$default, info = k)
  # the table of a report on a bare study: the same
  vars <- list(continuous = "AGE", categorical = c("AGEGR1", "SEX", "RACE", "ETHNIC"))
  a <- b1_run(d, b1_out("b1_old_demog", vars))$table
  b <- b1_run(d, b1_out("demog", c(list(by = "TRT01A"), vars)))$table
  expect_identical(a, b)
  a <- b1_run(d, b1_out("b1_old_demog", list(total = TRUE, labels = list(SEX = "Gender"), continuous = "AGE",
                                            categorical = "SEX")))$table
  b <- b1_run(d, b1_out("demog", list(by = "TRT01A", total = TRUE, labels = list(SEX = "Gender"),
                                     continuous = "AGE", categorical = "SEX")))$table
  expect_identical(a, b)
  # (values missing -- RACE blank, SEX NA: demog adds a Missing row, last, for each of
  # the two and its percentages are over the analysis set (canon 1.31); the summary
  # does the same (its missing_row, #774), so the whole table is the summary's)
  d2 <- b1_folder(na = TRUE)
  a <- b1_run(d2, b1_out("b1_old_demog", vars))$table
  b <- b1_run(d2, b1_out("demog", c(list(by = "TRT01A"), vars)))$table
  keep <- trimws(as.character(b$label)) != "Missing"
  expect_identical(sum(!keep), 2L)
  expect_identical(a, b)
  x <- tfl_template_expand("demog", b1_args())
  expect_identical(Filter(function(a) identical(a$id, "CAT"), x$ard$analyses)[[1L]]$denominator, "adsl_saf")
  # the FDA variant: the same rows (the new look adds the heading, the Missing row ...)
  new_stf <- tfl_template_params("stf_t02_demog")
  old_stf <- tfl_template_params("b1_old_stf")
  for (k in c("order", "total", "data")) expect_identical(new_stf[[k]]$default, old_stf[[k]]$default, info = k)
})

test_that("the variables and `by` are required: the plain required-argument error", {
  for (n in c("demog", "stf_t02_demog")) {
    expect_error(tfl_template_expand(n, list(population = "SAF", by = "TRT01A")),
                 "give `continuous` or `categorical`")
    expect_error(tfl_template_expand(n, list(population = "SAF", continuous = "AGE")), "needs 'by'")
    expect_no_error(tfl_template_expand(n, list(population = "SAF", by = "TRT01A", continuous = "AGE")))
    expect_no_error(tfl_template_expand(n, list(population = "SAF", by = "TRT01A", categorical = "SEX")))
    p <- tfl_template_params(n)
    expect_null(p$continuous$default)
    expect_null(p$categorical$default)
    expect_null(p$by$default)
    expect_true(p$by$required)
  }
  # the variables are as written, not the data's
  x <- tfl_template_expand("demog", b1_args())
  an <- function(id) Filter(function(a) identical(a$id, id), x$ard$analyses)[[1L]]
  expect_null(an("CONT")$optional)
  expect_identical(an("CAT")$variables, c("AGEGR1", "SEX"))
})

test_that("`by` is not filled from the study's treatment", {
  s <- b1_study()
  s$study$settings$treatment <- "TRT01A"
  for (n in c("demog", "stf_t02_demog")) {
    o <- b1_out(n, list(continuous = "AGE"))
    expect_error(tfl_resolve_output(o, s), "needs 'by'")
    expect_null(tfl_template_params(n)$by$study)
  }
  # other templates still take it (their batches are later)
  expect_identical(tflspec:::.tpl_get("summary", "t")$params$by$study, "settings.treatment")
})

test_that("continuous_statistics, categorical_statistics, stack and where are gone: the plain unknown-parameter error", {
  for (k in c("continuous_statistics", "categorical_statistics", "stack", "where")) {
    a <- b1_args()
    a[[k]] <- "x"
    for (n in c("demog", "stf_t02_demog")) {
      expect_error(tfl_template_expand(n, a), sprintf("has no parameter '%s'", k), info = paste(n, k))
    }
  }
})

test_that("population is not a template parameter: the report's `population:` drives the analysis set", {
  for (n in c("demog", "stf_t02_demog")) {
    expect_true(tflspec:::.tpl_registry[[n]]$params$population$hidden)
    expect_false("population" %in% names(tflspec:::.tpl_shown(tflspec:::.tpl_registry[[n]]$params)))
    expect_false("population" %in% tfl_templates()$params[tfl_templates()$name == n])
  }
  s <- b1_study()
  o <- tfl_output("T-9", "table", population = "SAF",
                  template = list(name = "demog", args = b1_args()[-1L]))
  r <- tfl_resolve_output(o, s)
  expect_identical(r$ard$analysis_data[[1L]]$population, "SAF")
  expect_true("population" %in% attr(r, "template")$study)
  # written in the template SPEC it stops, naming the report's field
  o2 <- tfl_output("T-9", "table", population = "SAF", template = list(name = "demog", args = b1_args()))
  expect_error(tfl_resolve_output(o2, s), "the report's `population:` field")
})

test_that("formats: the default is the active statistics catalog's (a company catalog's digits)", {
  fm <- function(x, id) Filter(function(a) identical(a$id, id), x$ard$analyses)[[1L]]$formats
  cat0 <- tfl_ard_statistics()
  cat1 <- cat0
  cat1$fmt[cat1$statistic == "sd" & cat1$kind == "continuous"] <- "xx.xxx"
  cat1$fmt[cat1$statistic == "mean" & cat1$kind == "continuous"] <- "xx.xx"
  cat1$fmt[cat1$statistic == "p" & cat1$kind == "categorical"] <- "xx.xx%"
  withr::local_options(tflspec.ard_statistics = cat1)
  x <- tfl_template_expand("demog", b1_args())
  expect_match(fm(x, "CONT"), "mean=2 | sd=3", fixed = TRUE)
  expect_identical(fm(x, "CAT"), "p=xx.xx%")
  # (given by name over the catalog's)
  y <- tfl_template_expand("demog", b1_args(formats = list(sd = 4)))
  expect_match(fm(y, "CONT"), "mean=2 | sd=4", fixed = TRUE)
  # (stf_t02_demog's formats are fixed values (canon 1.30): the catalog does not reach them)
  expect_identical(fm(tfl_template_expand("stf_t02_demog", b1_fda()), "CAT"), "p=xx.x%")
  # the built-in catalog: the digits the template had
  withr::local_options(tflspec.ard_statistics = cat0)
  z <- tfl_template_expand("demog", b1_args())
  expect_identical(fm(z, "CONT"), "N=0 | mean=1 | sd=2 | median=1 | min=0 | max=0")
  expect_identical(fm(z, "CAT"), "p=xx.x%")
})

test_that("the statistics computed are the ones formats names, plus any the cells name", {
  x <- tfl_template_expand("demog", b1_args())
  stat <- Filter(function(a) identical(a$id, "CONT"), x$ard$analyses)[[1L]]$statistics
  expect_setequal(stat, c("N", "mean", "sd", "median", "min", "max"))
  y <- tfl_template_expand("demog", b1_args(cells = list(n = "{N}", `Q1, Q3` = "{q1}, {q3}")))
  st2 <- Filter(function(a) identical(a$id, "CONT"), y$ard$analyses)[[1L]]$statistics
  expect_true(all(c("p25", "p75", "mean") %in% st2))
  cells <- b1_item(y, "plan_cells")[[1L]]$args$continuous
  expect_identical(cells[["Q1, Q3"]], "{p25}, {p75}")
  z <- tfl_template_expand("demog", list(population = "SAF", by = "TRT01A", categorical = "SEX"))
  expect_false("CONT" %in% vapply(z$ard$analyses, `[[`, "", "id"))
})

test_that("formats: the ARD's digits, by statistic and by variable; the default is today's", {
  x <- tfl_template_expand("demog", b1_args(continuous = c("AGE", "TRTDURD"),
                                           formats = list(TRTDURD = list(mean = 0, sd = 1))))
  cont <- Filter(function(a) identical(a$id, "CONT"), x$ard$analyses)[[1L]]
  expect_match(cont$formats, "mean=1 | sd=2", fixed = TRUE)
  expect_match(cont$formats, "TRTDURD:mean=0 | TRTDURD:sd=1", fixed = TRUE)
  cat <- Filter(function(a) identical(a$id, "CAT"), x$ard$analyses)[[1L]]
  expect_identical(cat$formats, "p=xx.x%")
  cells <- b1_item(x, "plan_cells")[[1L]]$args$continuous
  expect_identical(cells[["Median"]], "{median}")
  expect_identical(unlist(cells[["Mean (SD)"]]), c("{mean} ({sd})", "{mean} (-)"))
  expect_true(tfl_template_params("demog")$formats$merge)
  d <- b1_folder(na = FALSE)
  r <- b1_run(d, b1_out("demog", list(by = "TRT01A", continuous = c("AGE", "TRTDURD"), categorical = "SEX",
                                     formats = list(TRTDURD = list(mean = 0, sd = 1)))))
  txt <- unlist(lapply(r$table, as.character))
  expect_true(any(grepl("^60\\.1 \\(9\\.59\\)$", txt)))
  expect_true(any(grepl("^189 \\(135\\.0\\)$", txt)))
  expect_true(any(grepl("^41 \\(51\\.3\\)$", txt)))
})

test_that("missing_row counts the subjects with no value, last; categorical_levels shows 0 rows", {
  d <- b1_folder()
  r <- b1_run(d, b1_out("demog", list(
    by = "TRT01A", continuous = "AGE", categorical = c("SEX", "RACE"),
    missing_row = TRUE, missing_label = "Not recorded",
    categorical_levels = list(RACE = c("WHITE", "ASIAN", "BLACK OR AFRICAN AMERICAN", "OTHER", "UNKNOWN")))))
  lab <- as.character(r$table$label)
  key <- as.character(r$table$group)
  race <- lab[key == "Race, n (%)"]
  expect_setequal(race, c("WHITE", "ASIAN", "BLACK OR AFRICAN AMERICAN", "OTHER", "UNKNOWN", "Not recorded"))
  expect_true("Not recorded" %in% lab[key == "Sex, n (%)"])
  vals <- function(g) {
    k <- which(key == g & lab == "Not recorded")
    sum(as.numeric(sub(" .*$", "", unlist(r$table[k, -(1:2), drop = FALSE]))))
  }
  expect_identical(vals("Race, n (%)"), 3)
  expect_identical(vals("Sex, n (%)"), 2)
  k <- which(key == "Race, n (%)" & lab == "UNKNOWN")
  expect_true(all(grepl("^0 ", unlist(r$table[k, -(1:2), drop = FALSE]))))
  # FALSE: no such row; a value the list lacks stops
  r2 <- b1_run(d, b1_out("demog", list(by = "TRT01A", categorical = "SEX", missing_row = FALSE)))
  expect_false(any(grepl("Missing", unlist(lapply(r2$table, as.character)))))
  expect_error(b1_run(d, b1_out("demog", list(by = "TRT01A", categorical = "RACE",
                                              categorical_levels = list(RACE = c("ASIAN", "WHITE"))))),
               "code list does not")
  expect_error(tfl_template_expand("demog", b1_args(categorical_levels = list(RACE = "ASIAN"))),
               "not in `categorical`")
  expect_error(tfl_template_expand("demog", b1_args(missing_row = TRUE, missing_label = c("a", "b"))),
               "one text")
  # (stf_t02_demog's own levels for variables the report does not use are ignored)
  expect_no_error(tfl_template_expand("stf_t02_demog", b1_args()))
})

test_that("total: a Total column last, labelled by total_label", {
  x <- tfl_template_expand("demog", b1_args(total = TRUE, total_label = "All subjects"))
  expect_identical(b1_item(x, "plan_total")[[1L]]$args, list(label = "All subjects", position = "last"))
  expect_length(b1_item(tfl_template_expand("demog", b1_args()), "plan_total"), 0L)
  expect_true(Filter(function(a) identical(a$id, "CONT"), x$ard$analyses)[[1L]]$overall)
})

test_that("header_n and col_header: the arm heading text, {n} dropped with none; no heading by default", {
  x <- tfl_template_expand("demog", b1_args())
  expect_length(b1_item(x, "plan_col_header"), 0L)
  z <- tfl_template_expand("demog", b1_args(col_header = "{col}\n(N={n})"))
  expect_identical(b1_item(z, "plan_col_header")[[1L]]$args$header[[2L]]$text, "{col}\n(N={n})")
  w <- tfl_template_expand("demog", b1_args(col_header = "{col}\nN={n}", header_n = "none"))
  expect_identical(b1_item(w, "plan_col_header")[[1L]]$args$header[[2L]]$text, "{col}")
  # the percentages are over the analysis set the headings' N counts (canon 1.31): no `denominator`
  y <- tfl_template_expand("demog", b1_args())
  expect_identical(Filter(function(a) identical(a$id, "GROUPN"), y$ard$analyses)[[1L]]$data, "adsl_saf")
  expect_identical(Filter(function(a) identical(a$id, "CAT"), y$ard$analyses)[[1L]]$denominator, "adsl_saf")
  expect_error(tfl_template_expand("demog", b1_args(header_n = "tested")), "header_n")
  for (n in c("demog", "stf_t02_demog")) {
    expect_error(tfl_template_expand(n, b1_args(denominator = "adsl_itt")), "there is no `denominator` parameter")
  }
  expect_identical(tfl_template_params("demog")$header_n$choices, c("denominator", "set", "none"))
})

test_that("widths: NULL is the content's, auto the row title 35%, numbers are relative", {
  cl <- list(TRT01A = c("Drug X", "Control"))
  expect_length(b1_item(tfl_template_expand("demog", b1_args(), codelists = cl), "plan_columns"), 0L)
  x <- tfl_template_expand("demog", b1_args(widths = "auto"), codelists = cl)
  expect_equal(b1_item(x, "plan_columns")[[1L]]$args$widths, c(35, 32.5, 32.5))
  y <- tfl_template_expand("demog", b1_args(widths = "auto", total = TRUE), codelists = cl)
  expect_equal(b1_item(y, "plan_columns")[[1L]]$args$widths, c(35, rep(65 / 3, 3)))
  z <- tfl_template_expand("demog", b1_args(widths = c(40, 20, 20)), codelists = cl)
  expect_equal(b1_item(z, "plan_columns")[[1L]]$args$widths, c(40, 20, 20))
  expect_length(b1_item(tfl_template_expand("demog", b1_args(widths = "auto")), "plan_columns"), 0L)
  expect_error(tfl_template_expand("demog", b1_args(widths = c(40, -1))), "relative widths")
  expect_error(tfl_template_expand("demog", b1_args(widths = "wide")), "relative widths")
  # stf_t02_demog's default is auto
  expect_identical(tfl_template_params("stf_t02_demog")$widths$default, "auto")
  expect_null(tfl_template_params("demog")$widths$default)
})

test_that("the defaults are today's output: no header N, no blank rows, 30 rows, no stub item", {
  x <- tfl_template_expand("demog", b1_args())
  expect_identical(vapply(x$content, `[[`, "", "fn"),
                   c("table_plan", "plan_cells", "plan_labels", "plan_paginate_rows"))
  expect_identical(b1_item(x, "plan_paginate_rows")[[1L]]$args, list(max_rows = 30L, split = "group_safe"))
  y <- tfl_template_expand("demog", b1_args(max_rows = 20, blanks = FALSE, heading = "Characteristic"))
  expect_identical(b1_item(y, "plan_paginate_rows")[[1L]]$args, list(max_rows = 20L, split = "group_safe"))
  expect_identical(b1_item(y, "plan_blanks")[[1L]]$args, list(first = FALSE, last = FALSE))
  expect_identical(b1_item(y, "plan_stub")[[1L]]$args,
                   list(name = "row_label", before = TRUE, heading = "Characteristic"))
  b <- tfl_template_expand("demog", b1_args(blanks = TRUE, blanks_counted = TRUE))
  expect_identical(b1_item(b, "plan_blanks")[[1L]]$args, list(where = "between_groups", counted = TRUE))
  g <- tfl_template_expand("demog", b1_args(border_group = list(bottom = TRUE)))
  expect_identical(b1_item(g, "plan_style")[[1L]]$args$border_group,
                   tfl_call("rtf_border", list(bottom = TRUE)))
})

test_that("the study standard reaches demog through its references; other verbs lay under it", {
  s <- b1_study()
  o <- b1_out("demog", b1_args()[-1L])
  r <- tfl_resolve_output(o, s)
  tp <- attr(r, "template")
  # the fixture's defaults.yml: the heading lines, the stub, the blank rows, the rule, the cell
  expect_setequal(tp$study, c("population", "count_cell", "col_header", "stub_name", "stub_before", "blanks",
                              "blanks_first", "blanks_last", "blanks_counted", "border_header"))
  expect_identical(tp$args$blanks, "between_groups")
  expect_identical(tp$args$count_cell, "{n:.0f} ({p:.1f%})")
  expect_identical(tp$args$stub_name, "row_label")
  expect_identical(tp$args$col_header[[2L]]$row_label, "Characteristic")
  fns <- vapply(r$content, `[[`, "", "fn")
  expect_true(all(c("plan_col_header", "plan_blanks", "plan_stub", "plan_style") %in% fns))
  # ... and the template's items are the ones that carry them: nothing from the layers
  from <- vapply(r$content, function(it) it$from %||% "own", "")
  expect_true(all(from[fns %in% c("plan_col_header", "plan_blanks", "plan_stub", "plan_style", "plan_cells")] ==
                    "template:demog"))
  # a study with none of them: today's output, nothing made
  s2 <- s
  s2$defaults$content <- list()
  r2 <- tfl_resolve_output(o, s2)
  expect_setequal(vapply(r2$content, `[[`, "", "fn"),
                  c("normalize_ard", "table_plan", "plan_cells", "plan_labels", "plan_paginate_rows"))
  expect_length(attr(r2, "template")$study, 1L)
  # the report's value wins over the study's reference
  o3 <- b1_out("demog", c(b1_args()[-1L], list(blanks = FALSE)))
  b <- Filter(function(it) it$fn == "plan_blanks", tfl_resolve_output(o3, s)$content)[[1L]]$args
  expect_identical(b$where, NULL)
  expect_false(b$first)
  # max_rows is a reference (canon 1.29); a defaults.yml item of a verb the template does
  # not emit is laid under it (demog's set is [defaults], canon 1.27 and 1.32)
  s3 <- s
  s3$defaults$content[[length(s3$defaults$content) + 1L]] <- tfl_item("plan_paginate_rows", list(max_rows = 40L),
                                                                      from = "defaults")
  s3$defaults$content[[length(s3$defaults$content) + 1L]] <- tfl_item("plan_cell_style", list(bold = TRUE),
                                                                      from = "defaults")
  r3 <- tfl_resolve_output(o, s3)
  expect_identical(Filter(function(it) it$fn == "plan_paginate_rows", r3$content)[[1L]]$args,
                   list(max_rows = 40L, split = "group_safe"))
  expect_true("plan_cell_style" %in% vapply(r3$content, `[[`, "", "fn"))
  o3$extends <- list(content = "none")
  expect_false("plan_cell_style" %in% vapply(tfl_resolve_output(o3, s3)$content, `[[`, "", "fn"))
  # the page of the study stays
  expect_true(length(r3$page$header) > 0L)
  # the program writes the resolved values
  code <- paste(tfl_content_code(r), collapse = "\n")
  expect_match(code, "blanks = \"between_groups\"", fixed = TRUE)
  expect_match(code, "border_header = rtf_border(top = TRUE, bottom = TRUE)", fixed = TRUE)
  expect_match(code, "study defaults:", fixed = TRUE)
})

test_that("tfl_tpl_args(): where each demog value comes from", {
  s <- b1_study()
  a <- tfl_tpl_args("demog", study = s)
  expect_identical(a$layer[a$param == "blanks"], "study: plan_blanks.where")
  expect_identical(a$layer[a$param == "by"], "template")
  b <- tfl_tpl_args("demog")
  # (max_rows refers to the study (canon 1.29): with none, its fallback)
  expect_identical(b$layer[b$param == "max_rows"], "fallback")
  expect_false("population" %in% b$param)
})

test_that("style: demog follows settings.style, stf_t02_demog is fixed; a built-in name is always the built-in", {
  s <- b1_study()
  s$styles <- list(compact = list(data = list(), codelists = list(),
                                  content = list(tfl_item("plan_style", list(font_size_half_points = 14L),
                                                          from = "style:compact")), page = list()),
                   stf_t02 = list(data = list(), codelists = list(),
                                  content = list(tfl_item("plan_style", list(font_size_half_points = 99L),
                                                          from = "style:stf_t02")), page = list()))
  size <- function(o, study = s) {
    ro <- tfl_resolve_output(o, study)
    ps <- Filter(function(it) it$fn == "plan_style", ro$content)
    if (length(ps)) ps[[length(ps)]]$args$font_size_half_points
  }
  a <- b1_args()[-1L]
  # no settings.style: standard (the built-in, empty)
  expect_null(size(b1_out("demog", a)))
  # settings.style names the study's own set
  s$study$settings$style <- "compact"
  expect_identical(size(b1_out("demog", a)), 14L)
  # the template argument and the report's `style:` field
  expect_null(size(b1_out("demog", c(a, list(style = "standard")))))
  expect_identical(size(b1_out("demog", a, style = "compact")), 14L)
  expect_null(size(b1_out("demog", c(a, list(style = "standard"))), s))
  # the built-in "stf_t02" is never the study's set of that name
  expect_identical(size(b1_out("demog", c(a, list(style = "stf_t02")))), 16L)
  expect_identical(size(b1_out("stf_t02_demog", b1_fda()[-1L])), 16L)
  # stf_t02_demog does not follow settings.style
  expect_identical(tflspec:::.tpl_registry$stf_t02_demog$params$style$default, "stf_t02")
  expect_null(tflspec:::.tpl_registry$stf_t02_demog$params$style$study)
  # a name nobody has stops, naming the sets there are
  expect_error(size(b1_out("demog", c(a, list(style = "nope")))), "no style \"nope\".*stf_t02")
  # the set's page comes with it (the built-in look is landscape)
  r <- tfl_resolve_output(b1_out("stf_t02_demog", b1_fda()[-1L]), s)
  expect_identical(r$page$rtf_page$orientation, "landscape")
  # .tpl_style_set(): built-in names, then the study's
  expect_identical(tflspec:::.tpl_style_set("stf_t02", s)$content[[1L]]$args$font, "Arial")
  expect_identical(tflspec:::.tpl_style_set("compact", s)$content[[1L]]$args$font_size_half_points, 14L)
  expect_length(tflspec:::.tpl_style_set("standard", s)$content, 0L)
})

test_that("the two parts: ard: and content:; flat args: still reads; every parameter in one part", {
  mk <- function(t) tfl_output("T-1", "table", population = "SAF", template = t)
  a <- tflspec:::.tpl_spec_of(mk(list(name = "demog", ard = list(by = "TRT01A", continuous = "AGE"),
                                      content = list(total_label = "All", widths = "auto"))))
  expect_setequal(names(a$args), c("by", "continuous", "total_label", "widths"))
  flat <- tflspec:::.tpl_spec_of(mk(list(name = "demog", args = list(by = "TRT01A", total_label = "All"))))
  expect_identical(flat$args$total_label, "All")
  expect_error(tflspec:::.tpl_spec_of(mk(list(name = "demog", ard = list(widths = "auto")))),
               "'widths' acts in `content:`, not `ard:`")
  expect_error(tflspec:::.tpl_spec_of(mk(list(name = "stf_t02_demog", content = list(total = TRUE)))),
               "'total' acts in `ard:`, not `content:`")
  for (n in c("demog", "stf_t02_demog")) {
    p <- names(tfl_template_params(n))
    parts <- tflspec:::.tpl_registry[[n]]$parts
    expect_setequal(c(parts$ard, parts$content), setdiff(p, "population"))
    expect_length(intersect(parts$ard, parts$content), 0L)
  }
  # a two-part report runs end to end and reads back
  d <- b1_folder(na = FALSE)
  o <- tfl_output("T-9", "table", population = "SAF", extends = "defaults", template = list(
    name = "demog", ard = list(by = "TRT01A", continuous = "AGE", categorical = c("SEX", "RACE"), total = TRUE),
    content = list(total_label = "All subjects", blanks = FALSE, widths = c(40, 20, 20, 20))),
    codelists = list(TRT01A = list(list(value = "Drug X", label = "Drug X"),
                                   list(value = "Control", label = "Control"))))
  r <- b1_run(d, o)
  expect_identical(names(tfl_read_outputs(d)[["T-9"]]$template), c("name", "ard", "content"))
  expect_true("All subjects" %in% names(r$table))
  # A = B: the program and the build give the same table
  code <- tfl_content_code(r$ro, pipe = "|>")
  e <- new.env()
  e$ard <- r$ard
  suppressMessages(eval(parse(text = code), envir = e))
  expect_identical(as.data.frame(suppressMessages(rtfreporter::plan_apply(e$plan, "table"))), r$table)
})

test_that("stf_t02_demog: the FDA look as its own defaults; it runs", {
  d <- b1_folder()
  x <- tfl_template_expand("stf_t02_demog", b1_fda())
  expect_identical(b1_item(x, "plan_total")[[1L]]$args$label, "Total Population")
  hdr <- b1_item(x, "plan_col_header")[[1L]]
  expect_identical(hdr$args$header[[2L]]$text, "{col}\nN={n}")
  expect_identical(hdr$args$header[[1L]]$text, "Characteristic")
  expect_identical(b1_item(x, "plan_stub")[[1L]]$args$heading, "Characteristic")
  expect_identical(b1_item(x, "plan_blanks")[[1L]]$args, list(first = FALSE, last = FALSE))
  # (the rule under each block is the style set "stf_t02"'s, not a default of the template)
  expect_null(unlist(lapply(b1_item(x, "plan_style"), function(it) it$args$border_group)))
  ro <- tfl_resolve_output(b1_out("stf_t02_demog", b1_fda()[-1L]), b1_study())
  expect_true(any(vapply(b1_item(ro, "plan_style"), function(it) !is.null(it$args$border_group), NA)))
  expect_identical(b1_item(x, "plan_cells")[[1L]]$args$continuous[["Median (min, max)"]],
                   "{median} ({min}, {max})")
  # the study's look does not reach it: its look is fixed
  s <- b1_study()
  tp <- attr(tfl_resolve_output(b1_out("stf_t02_demog", b1_fda()[-1L]), s), "template")
  expect_setequal(tp$study, "population")
  r <- b1_run(d, b1_out("stf_t02_demog", b1_fda()[-1L],
                        codelists = list(TRT01A = list(list(value = "Drug X", label = "Drug X"),
                                                       list(value = "Control", label = "Control")))))
  expect_true("Total Population" %in% names(r$table))
  grp <- as.character(r$table$group)
  expect_true(all(c("Sex, n (%)", "Age, years", "Country of participation, n (%)") %in% grp))
  expect_true("Median (min, max)" %in% as.character(r$table$label))
})

test_that("tfl_tpl: the arms come from the ARD for the widths (stf_t02_demog's auto)", {
  skip_if_not_installed("cards")
  saf <- tflspec::adsl[tflspec::adsl$SAFFL == "Y", ]
  fm <- list(cards::everything() ~ list(N = 0, mean = 1, sd = 2, median = 1, min = 0, max = 0))
  ard <- cards::bind_ard(
    cards::ard_tabulate(saf, variables = TRT01A),
    cards::apply_fmt_fun(cards::ard_summary(saf, by = TRT01A, variables = AGE, fmt_fun = fm)),
    cards::apply_fmt_fun(cards::ard_tabulate(saf, by = TRT01A, variables = SEX,
      fmt_fun = cards::everything() ~ list(p = cards::label_round(1, scale = 100)))))
  p <- tpl_demog(ard, population = "SAF", by = "TRT01A", continuous = "AGE", categorical = "SEX")
  tab <- as.data.frame(suppressMessages(rtfreporter::plan_apply(p, "table")))
  expect_true(all(c("Drug X", "Control") %in% names(tab)))
  p2 <- tfl_tpl(ard, "stf_t02_demog", population = "SAF", by = "TRT01A", continuous = "AGE", categorical = "SEX")
  expect_s3_class(p2, "table_plan")
  # (the template's `auto` widths: the row title 35%, the three columns -- two arms and the Total -- share the rest)
  w <- tfl_template_expand("stf_t02_demog", b1_fda(), codelists = list(TRT01A = c("Drug X", "Control")))
  expect_equal(b1_item(w, "plan_columns")[[1L]]$args$widths, c(35, rep(65 / 3, 3)))
})

test_that("widths auto: a report with no code list for the arms takes them from the ARD, as tfl_tpl() does", {
  d <- b1_folder(na = FALSE)
  o <- b1_out("stf_t02_demog", b1_fda()[-1L])
  tfl_write_output(o, file.path(d, "spec", "outputs", "T-9.yml"))
  st <- tfl_read_study_spec(d)
  ard <- suppressMessages(tfl_build_ard(st, output_id = "T-9", dir = d, save = FALSE))
  ro <- tfl_resolve_output(tfl_read_outputs(d)[["T-9"]], st)
  # (resolved: no plan_columns, the arms are not known)
  expect_length(b1_item(ro, "plan_columns"), 0L)
  ro$content <- tflspec:::.tpl_auto_widths(ro, ard)
  fns <- vapply(ro$content, `[[`, "", "fn")
  expect_identical(sum(fns == "plan_columns"), 1L)
  expect_identical(which(fns == "plan_columns") + 1L, which(fns == "plan_paginate_rows"))
  expect_equal(b1_item(ro, "plan_columns")[[1L]]$args$widths, c(35, rep(65 / 3, 3)))
  # a plan_columns the report gives is left alone; widths NULL (demog) makes none
  ro2 <- tfl_resolve_output(tfl_read_outputs(d)[["T-9"]], st)
  ro2$content <- c(ro2$content, list(tfl_item("plan_columns", list(widths = c(40, 30, 30, 0)))))
  expect_identical(sum(vapply(tflspec:::.tpl_auto_widths(ro2, ard), `[[`, "", "fn") == "plan_columns"), 1L)
  o3 <- b1_out("demog", c(list(by = "TRT01A"), list(continuous = "AGE")))
  tfl_write_output(o3, file.path(d, "spec", "outputs", "T-9.yml"))
  ro3 <- tfl_resolve_output(tfl_read_outputs(d)[["T-9"]], st)
  expect_length(b1_item(list(content = tflspec:::.tpl_auto_widths(ro3, ard)), "plan_columns"), 0L)
  # the run itself
  p <- suppressMessages(tfl_content_run(ro, ard))
  expect_s3_class(p, "table_plan")
})

# ---- !unset, one test per template looping every parameter --------------------

# What each parameter stands for: `args` (over the base) that make the step happen, and
# `pat`, what the step's code has (the inner calls' code and the ARD's).  Under `!unset` the
# program has nothing of it: no call, no argument, no `if`.
b1_unset_probe <- list(
  subset = list(args = list(subset = "AGE > 0"), pat = "AGE > 0"),
  data_id = list(args = list(data = "ADSL2", data_id = "my_set"), pat = "my_set"),
  formats = list(args = list(), pat = "formats"),
  total = list(args = list(total = TRUE), pat = "plan_total|overall"),
  missing_row = list(args = list(missing_row = TRUE), pat = "Missing|is\\.factor|if \\("),
  order = list(args = list(order = c("SEX", "AGE")), pat = "plan_levels\\(variable",
               stf = list(args = list(order = c("COUNTRY", "SEX", "AGE")), pat = "COUNTRY\",\\s*\"SEX")),
  labels = list(args = list(), pat = "plan_labels"),
  categorical_levels = list(args = list(categorical_levels = list(SEX = c("M", "F"))), pat = "M, F|\"M\", \"F\""),
  cells = list(args = list(), pat = "Mean \\(SD\\)"),
  sd_missing = list(args = list(), pat = "\\(-\\)",
                    stf = list(args = list(cells = tflspec:::.tpl_demog_cells))),
  count_cell = list(args = list(count_cell = "{n} ({p})"), pat = "\\{n\\} \\(\\{p\\}\\)"),
  total_label = list(args = list(total = TRUE, total_label = "All"), pat = '"All"'),
  header_n = list(args = list(col_header = "{col}\nN={n}"), pat = "N=\\{n\\}"),
  col_header = list(args = list(col_header = "{col}\nN={n}"), pat = "plan_col_header"),
  heading = list(args = list(heading = "Characteristic"), pat = "Characteristic"),
  stub_name = list(args = list(stub_name = "rl"), pat = 'name = "rl"',
                   stf = list(args = list(heading = tfl_unset()))),
  stub_before = list(args = list(stub_before = TRUE), pat = "before = TRUE",
                     stf = list(args = list(heading = tfl_unset()))),
  widths = list(args = list(widths = "auto"), pat = "plan_columns"),
  max_rows = list(args = list(max_rows = 12), pat = "max_rows"),
  rounding = list(args = list(rounding = "sas"), pat = "plan_digits"),
  blanks = list(args = list(blanks = TRUE), pat = "where"),
  blanks_first = list(args = list(blanks = TRUE, blanks_first = TRUE), pat = "first = TRUE"),
  blanks_last = list(args = list(blanks = TRUE, blanks_last = TRUE), pat = "last = TRUE"),
  blanks_counted = list(args = list(blanks = TRUE, blanks_counted = TRUE), pat = "counted"),
  border_header = list(args = list(border_header = list(top = TRUE)), pat = "border_header"),
  border_group = list(args = list(border_group = list(bottom = TRUE)), pat = "border_group"))

# the code of a template's expansion: its items' and its ARD's
b1_code <- function(x) {
  paste(c(vapply(x$content, function(it) paste(tfl_item_code(it), collapse = "
"), ""),
          deparse(x$ard)), collapse = "
")
}

b1_unset_loop <- function(name, base) {
  t <- tflspec:::.tpl_registry[[name]]
  s <- b1_study()
  for (p in names(t$params)) {
    flag <- t$params[[p]]$unset
    info <- paste(name, p, flag)
    if (isTRUE(t$params[[p]]$fixed_value)) {
      # a value the template fixes (canon 1.30) is no parameter: giving it, `!unset` too, stops
      args <- base
      args[[p]] <- tfl_unset()
      expect_error(tfl_template_expand(name, args), sprintf("there is no `%s` parameter", p), info = info)
      next
    }
    expect_true(flag %in% c("omit", "step_off", "stop"), info = info)
    args <- base
    args[[p]] <- tfl_unset()
    if (identical(flag, "stop")) {
      # the body needs a value: stop, naming the parameter
      expect_error(tfl_template_expand(name, args), sprintf("'%s', so `!unset` cannot be used", p), info = info)
      next
    }
    if (identical(p, "style")) {
      # the style set is a layer of the report: none when unset (the FDA look's font is the set's)
      o <- tfl_output("T-9", "table", population = "SAF", template = list(name = name, args = args[-1L]))
      r <- tfl_resolve_output(o, s)
      expect_false(any(vapply(r$content, function(it) identical(it$args$font, "Arial"), NA)), info = info)
      expect_null(r$page$rtf_page$orientation, info = info)
      next
    }
    # the parameter left out: the template runs, records it, and its step/argument is gone
    expect_no_error(y <- tfl_template_expand(name, args))
    expect_true(p %in% y$unset, info = info)
    expect_false(p %in% y$given, info = info)
    pr <- b1_unset_probe[[p]]
    expect_false(is.null(pr), info = paste("no probe for", info))
    # (the FDA look has the step by default: its probe turns the other cause of it off)
    if (identical(name, "stf_t02_demog") && !is.null(pr$stf)) pr$args <- utils::modifyList(pr$args, pr$stf$args)
    if (identical(name, "stf_t02_demog") && !is.null(pr$stf$pat)) pr$pat <- pr$stf$pat
    on <- utils::modifyList(base, pr$args, keep.null = TRUE)
    cl <- list(TRT01A = c("Drug X", "Control"))
    x <- tfl_template_expand(name, on, codelists = cl)
    un <- on
    un[[p]] <- tfl_unset()
    z <- tfl_template_expand(name, un, codelists = cl)
    # with a value (or the default) the step's code is there ...
    expect_match(b1_code(x), pr$pat, info = paste(info, "on"))
    # ... and under `!unset` nothing of it is: no call, no argument, no `if`
    expect_no_match(b1_code(z), pr$pat, info = paste(info, "off"))
  }
}

test_that("demog: every parameter under `!unset` behaves as its flag says", {
  b1_unset_loop("demog", b1_args(categorical = c("SEX", "RACE")))
  # the flags are one place per parameter
  fl <- vapply(tflspec:::.tpl_registry$demog$params, `[[`, "", "unset")
  expect_setequal(names(fl)[fl == "stop"], c("population", "continuous", "categorical", "by", "data", "missing_label"))
  expect_setequal(names(fl)[fl == "step_off"],
                  c("subset", "total", "missing_row", "labels", "sd_missing", "header_n", "style"))
  expect_identical(fl, vapply(tflspec:::.tpl_registry$stf_t02_demog$params, `[[`, "", "unset"))
})

test_that("stf_t02_demog: every parameter under `!unset` behaves as its flag says", {
  b1_unset_loop("stf_t02_demog", b1_fda())
})

test_that("`!unset` in detail: cells omitted, sd_missing no guard, header_n no N, style none", {
  x <- tfl_template_expand("demog", b1_args(cells = tfl_unset(), count_cell = "{n} ({p})"))
  expect_identical(names(b1_item(x, "plan_cells")[[1L]]$args), "categorical")
  expect_length(b1_item(tfl_template_expand("demog", b1_args(cells = tfl_unset())), "plan_cells"), 0L)
  y <- tfl_template_expand("demog", b1_args(sd_missing = tfl_unset()))
  expect_identical(b1_item(y, "plan_cells")[[1L]]$args$continuous[["Mean (SD)"]], "{mean} ({sd})")
  z <- tfl_template_expand("stf_t02_demog", b1_fda(header_n = tfl_unset()))
  expect_identical(b1_item(z, "plan_col_header")[[1L]]$args$header[[2L]]$text, "{col}")
  s <- b1_study()
  # (stf_t02_demog fixes its style, canon 1.32: `!unset` is another value, a stop;
  # demog's unset style lays no set)
  expect_error(tfl_resolve_output(b1_out("stf_t02_demog", b1_fda(style = tfl_unset())[-1L]), s),
               "fixes its style")
  r <- tfl_resolve_output(b1_out("demog", b1_args(style = tfl_unset())[-1L]), s)
  expect_null(r$page$rtf_page$orientation)
  expect_false(any(vapply(r$content, function(it) identical(it$args$font, "Arial"), NA)))
  # NA and NULL stay distinct from unset
  expect_length(b1_item(tfl_template_expand("demog", c(b1_args(), list(max_rows = NULL))), "plan_paginate_rows")[[1L]]$args$max_rows, 0L)
})

# ---- appended cards -------------------------------------------------------------

test_that("an appended plan_paginate_rows(max_rows) keeps split; an appended plan_labels changes one label", {
  s <- b1_study()
  for (n in c("demog", "stf_t02_demog")) {
    a <- (if (n == "demog") b1_args() else b1_fda())[-1L]
    o <- b1_out(n, a, content = list(
      tfl_item("plan_paginate_rows", list(max_rows = 12L)),
      tfl_item("plan_labels", list(variable = list(SEX = "Gender")))))
    r <- tfl_resolve_output(o, s)
    pr <- Filter(function(it) it$fn == "plan_paginate_rows", r$content)
    expect_length(pr, 1L)
    expect_identical(pr[[1L]]$args, list(max_rows = 12L, split = "group_safe"))
    pl <- Filter(function(it) it$fn == "plan_labels", r$content)
    expect_length(pl, 1L)
    lab <- pl[[1L]]$args$variable
    expect_identical(lab$SEX, "Gender")
    # the other labels are the template's
    base <- Filter(function(it) it$fn == "plan_labels",
                   tfl_resolve_output(b1_out(n, a), s)$content)[[1L]]$args$variable
    expect_identical(lab[setdiff(names(lab), "SEX")], base[setdiff(names(base), "SEX")])
  }
})

test_that("demog: rounding, max_rows and widths reference the study standard (canon 1.29); stf_t02_demog: none", {
  mk <- function(items, settings = list()) {
    s <- b1_study()
    s$defaults$content <- c(s$defaults$content, items)
    s$study$settings <- utils::modifyList(s$study$settings %||% list(), settings)
    s
  }
  s <- mk(list(tfl_item("plan_paginate_rows", list(max_rows = 18L)),
               tfl_item("plan_columns", list(widths = list(row_label = 40, .values = 18)))),
          list(rounding = "r"))
  cols <- function(r) Filter(function(it) it$fn == "plan_columns", r$content)
  rows <- function(r) Filter(function(it) it$fn == "plan_paginate_rows", r$content)[[1L]]$args$max_rows
  digits <- function(r) Filter(function(it) it$fn == "plan_digits", r$content)
  for (n in c("demog", "stf_t02_demog")) {
    a <- (if (n == "demog") b1_args() else b1_fda())[-1L]
    base <- tfl_resolve_output(b1_out(n, a), b1_study())
    # the study says nothing: no plan_digits item, 30 rows (today's output)
    expect_length(digits(base), 0L)
    expect_identical(rows(base), 30L)
    r <- tfl_resolve_output(b1_out(n, a), s)
    if (n == "demog") {
      expect_identical(digits(r)[[1L]]$args, list(rounding = "r"))
      expect_identical(rows(r), 18L)
      # (the widths by column name, as the study's plan_columns(widths = ) gives them)
      expect_identical(cols(r)[[1L]]$args$widths, list(row_label = 40, .values = 18))
      expect_setequal(intersect(c("max_rows", "widths"), attr(r, "template")$study), c("max_rows", "widths"))
    } else {
      # canon 1.30: the template's values, not the study's
      expect_length(digits(r), 0L)
      expect_identical(rows(r), 30L)
      expect_false(any(c("max_rows", "widths", "rounding") %in% attr(r, "template")$study))
    }
    # the report's own value wins
    r2 <- tfl_resolve_output(b1_out(n, c(a, list(max_rows = 9, rounding = "sas"))), s)
    expect_identical(rows(r2), 9L)
    expect_identical(digits(r2)[[1L]]$args$rounding, "sas")
  }
  p <- tfl_template_params("demog")
  expect_identical(p$widths$study, "plan_columns.widths")
  expect_identical(p$max_rows$study, "plan_paginate_rows.max_rows")
  expect_null(p$widths$default)
  expect_identical(p$max_rows$default, 30L)
  q <- tfl_template_params("stf_t02_demog")
  expect_identical(q$widths$default, "auto")
  expect_identical(q$max_rows$default, 30L)
})

test_that("stf_t02_demog is values only (canon 1.30): no reference; formats and missing_row fixed", {
  p <- tfl_template_params("stf_t02_demog")
  refs <- names(Filter(function(x) length(x$study) && !isFALSE(x$study), p))
  # (`population` is the report's field, until its removal)
  expect_identical(setdiff(refs, "population"), character())
  a <- tfl_tpl_args("stf_t02_demog")
  expect_false(any(c("formats", "missing_row", "denominator") %in% a$param))
  expect_error(tfl_template_expand("stf_t02_demog", b1_fda(formats = list(mean = 2))),
               "there is no `formats` parameter")
  expect_error(tfl_template_expand("stf_t02_demog", b1_fda(missing_row = FALSE)),
               "there is no `missing_row` parameter")
  # the digits are the built-in catalog's, whatever a company catalog says
  fm <- function(x, id) Filter(function(a) identical(a$id, id), x$ard$analyses)[[1L]]$formats
  old <- options(tflspec.ard_statistics = local({
    st <- tflspec:::.ard_statistics_builtin()
    st$fmt[st$statistic == "mean"] <- "xx.xx"
    st
  }))
  on.exit(options(old), add = TRUE)
  expect_match(fm(tfl_template_expand("demog", b1_args()), "CONT"), "mean=2", fixed = TRUE)
  x <- tfl_template_expand("stf_t02_demog", b1_fda())
  expect_identical(fm(x, "CONT"), "N=0 | mean=1 | sd=2 | median=1 | min=0 | max=0")
  expect_identical(fm(x, "CAT"), "p=xx.x%")
  # the Missing step is there, the % over the analysis set
  expect_true("adsl_saf_m" %in% vapply(x$ard$analysis_data, `[[`, "", "id"))
  expect_identical(Filter(function(a) identical(a$id, "CAT"), x$ard$analyses)[[1L]]$denominator, "adsl_saf")
})

test_that("the `unset` flag is shown for the migrated templates, NA for the others", {
  for (n in c("demog", "stf_t02_demog")) {
    a <- tfl_tpl_args(n)
    expect_false(anyNA(a$unset), info = n)
    expect_true(all(a$unset %in% c("omit", "stop", "step_off")), info = n)
    pr <- tfl_template_params(n)
    expect_identical(unname(vapply(pr[a$param], function(p) p$unset, "")), a$unset)
    # (the flag the body checks)
    expect_identical(unname(vapply(tflspec:::.tpl_registry[[n]]$params, `[[`, "", "unset")),
                     unname(vapply(pr, `[[`, "", "unset")))
  }
  expect_true(all(is.na(tfl_tpl_args("summary")$unset)))
  expect_true(all(is.na(vapply(tfl_template_params("summary"), function(p) p$unset, ""))))
})

sr_study_b1 <- function(settings) {
  d <- withr::local_tempdir()
  file.copy(list.files(system.file("extdata", "spec-yaml", package = "tflspec"), full.names = TRUE), d,
            recursive = TRUE)
  yaml::write_yaml(list(study_id = "SR-1", settings = c(list(treatment = "TRT01P"), settings)),
                   file.path(d, "study.yml"))
  tfl_read_study_spec(d)
}

# tflplanner's sample study writes the same settings another way: the unnamed
# cell (`default`) and the header as cells (`header`); a reference may name
# several keys, read in order
b1_planner_defaults <- c(
  "content:", "  - normalize_ard: {}", "  - plan_cells:", "      default: '{n:.0f} ({p:.1f%})'",
  "  - plan_col_header:", "      header:", "        - line: '1'", "          cols: row_label",
  "        - line: '1'", "          cols: .values", "          span: each", "          text: '{col}'",
  "        - line: '2'", "          cols: row_label", "          text: Characteristic",
  "        - line: '2'", "          cols: .values", "          span: each", "          text: (N={n})",
  "  - plan_stub:", "      name: row_label", "      before: true", "      heading: ''",
  "  - plan_blanks:", "      where: between_groups")

test_that("a reference may name several keys: plan_cells.default and plan_col_header.header", {
  d <- b1_folder(defaults = b1_planner_defaults)
  o <- b1_out("demog", b1_args()[-1L])
  r <- b1_run(d, o)
  # the categorical rows are there, with the study's cell
  expect_true(any(grepl("^ *[0-9]+ +[(][0-9.]+[)]$", unlist(r$table[-1L]))))
  hdr <- b1_item(r$ro, "plan_col_header")[[1L]]
  expect_null(hdr$args$lines)
  expect_identical(vapply(hdr$args$header, function(e) e$text %||% "", ""),
                   c("", "{col}", "Characteristic", "(N={n})"))
  code <- paste(tfl_content_code(r$ro, pipe = "|>"), collapse = "
")
  expect_match(code, "col_header = list(", fixed = TRUE)
  expect_match(code, "count_cell", fixed = TRUE)
  expect_match(code, "(N={n})", fixed = TRUE)
  expect_match(code, "{n:.0f} ({p:.1f%})", fixed = TRUE)
  # (A = B: the program gives the same table)
  e <- new.env()
  e$ard <- r$ard
  suppressMessages(eval(parse(text = code), envir = e))
  expect_identical(as.data.frame(suppressMessages(rtfreporter::plan_apply(e$plan, "table"))), r$table)
  # the help shows the key that was found
  a <- tfl_tpl_args("demog", r$st, "SAF")
  expect_identical(a$layer[a$param == "count_cell"], "study: plan_cells.default")
  expect_identical(a$layer[a$param == "col_header"], "study: plan_col_header.header")
  expect_identical(a$study_key[a$param == "count_cell"], "plan_cells.default")
  expect_identical(a$study_key[a$param == "col_header"], "plan_col_header.header")
  # none found: the first key, and the key text names them all
  a0 <- tfl_tpl_args("demog", b1_study(), "SAF")
  expect_identical(a0$study_key[a0$param == "count_cell"], "plan_cells.categorical")
  expect_match(tflspec:::.tpl_ref_text(c("plan_cells.categorical", "plan_cells.default")),
               "plan_cells.categorical, plan_cells.default", fixed = TRUE)
  # a single key: as before
  expect_identical(tflspec:::.tpl_study_find("settings.a", NULL, sr_study_b1(list(a = 1)))$value, 1)
})
