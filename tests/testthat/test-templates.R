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
