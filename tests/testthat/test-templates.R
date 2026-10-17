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
                   c("table_plan", "plan_cells", "plan_labels", "plan_levels",
                     "plan_paginate_rows"))
  expect_true(all(vapply(x$content, `[[`, "", "from") == "template:demog"))
  expect_identical(x$ard$analysis_data[[1L]]$id, "adsl_saf")
  expect_identical(vapply(x$ard$analyses, `[[`, "", "id"), c("GROUPN", "CONT", "CAT"))
  s <- tfl_template_expand("demog", list(population = "SAF", stack = TRUE))
  expect_identical(vapply(s$ard$analyses, `[[`, "", "id"), c("DEMOG", "CONT", "CAT"))
  expect_identical(s$ard$analyses[[2L]]$parent, "DEMOG")
  # the code lists give the values' order, as text
  cl <- list(SEX = list(list(value = "F", label = "Female"), list(value = "M", label = "Male")))
  y <- tfl_template_expand("demog", list(population = "SAF"), codelists = cl)
  expect_identical(y$content[[4L]]$args$SEX, c("Female", "Male"))
  # a dataset other than ADSL: its own analysis data, its records kept by `where`
  z <- tfl_template_expand("summary", list(population = "SAF", variables = "AVAL",
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
  s <- suppressMessages(tpl_summary(ard, population = "SAF", variables = c("AGE", "TRTDURD")))
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
                                           cat_statistics = NULL))
  expect_identical(vapply(x$ard$analyses, `[[`, "", "id"), c("GROUPN", "CAT"))
  expect_null(x$ard$analyses[[2L]]$statistics)
  expect_false("plan_cells" %in% vapply(x$content, `[[`, "", "fn"))
  expect_identical(x$content[[length(x$content)]]$args, list(max_rows = 30L, split = "group_safe"))
  # the look is no parameter
  expect_error(tfl_template_expand("summary", list(population = "SAF", variables = "AGE",
                                                   max_rows = 24)), "no parameter")
  expect_error(tfl_template_expand("summary", list(population = "SAF")), "categorical")
  d <- tfl_template_expand("disposition", list(population = "SAF"))
  expect_identical(d$ard$analyses[[2L]]$id, "DISP")
  expect_identical(d$content[[2L]]$args, list(EOSSTT = "Status at end of study, n (%)"))
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

test_that("summary's mean (CI); km_table, vs_change and orr", {
  x <- tfl_template_expand("summary", list(population = "SAF", variables = "CHG", statistics = "N",
                                           ci_label = "Mean change (95% CI)"))
  expect_identical(vapply(x$ard$analyses, `[[`, "", "method"),
                   c("categorical", "continuous", "mean_ci"))
  k <- tfl_template_expand("km_table", list(by_levels = c("A", "B")))
  expect_null(k$ard)   # the survival analyses are the report's own
  expect_s3_class(k$data$steps[[1L]], "tfl_r")
  expect_match(k$data$steps[[1L]], "Median (95% CI)", fixed = TRUE)
  expect_false(grepl("== \"0\"", tfl_template_expand("km_table", list(drop_time_0 = FALSE))$data$steps[[1L]]))
  v <- tfl_template_expand("vs_change", list(population = "SAF", param = "SYSBP", visit = "V1",
    param_label = "Systolic BP", when = "week 1", visit_label = "Week 1", unit = "mmHg"))
  expect_identical(v$ard$analyses[[2L]]$variables, c("BASE", "AVAL", "CHG"))
  expect_identical(v$content[[3L]]$args$CHG, "Change from baseline (mmHg)")
  o <- tfl_template_expand("orr", list(responders = c("CR", "PR", "SD")))
  expect_identical(as.character(o$ard$analysis_data[[1L]]$derive),
                   'RESP = AVALC %in% c("CR", "PR", "SD")')
  expect_identical(o$content[[3L]]$args$RESP, "Objective response rate (CR + PR + SD)")
  expect_true(all(c("km_table", "vs_change", "orr") %in% tfl_templates()$name))
})
