# The study's rounding is written once, in the setup every report sources
# (options(rtfreporter.rounding = )); a report's program writes its own
# plan_digits(rounding = ) only when it differs from the study's.

sr_study <- function(rounding = "sas") {
  st <- tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))
  it <- tfl_item("plan_digits", list(rounding = rounding))
  it$from <- "defaults"
  st$defaults$content <- c(st$defaults$content, list(it))
  st
}
sr_output <- function(content = list()) {
  tfl_output("T-9", "table", population = "SAF", content = c(list(
    tfl_item("table_plan", list(cols = "TRT01A")),
    tfl_item("plan_digits", list(AGE = 0))), content))
}

test_that("the study's rounding is the setup's option, not the report's code", {
  st <- sr_study("sas")
  setup <- tfl_report_setup_code(st)
  expect_true(any(grepl("rtfreporter.rounding = \"sas\"", setup, fixed = TRUE)))
  code <- tfl_content_code(tfl_resolve_output(sr_output(), st))
  expect_false(any(grepl("rounding", code, fixed = TRUE)))
  expect_true(any(grepl("plan_digits(AGE = 0)", code, fixed = TRUE)))
  # the run in hand keeps it: the same numbers with no option set
  ro <- tfl_resolve_output(sr_output(), st)
  r <- unlist(lapply(Filter(function(it) it$fn == "plan_digits", ro$content), function(it) it$args$rounding))
  expect_identical(r, "sas")
})

test_that("changing the study's rounding changes the setup only; a report's own rounding stays", {
  a <- sr_study("sas")
  b <- sr_study("r")
  expect_false(identical(tfl_report_setup_code(a), tfl_report_setup_code(b)))
  expect_identical(tfl_content_code(tfl_resolve_output(sr_output(), a)),
                   tfl_content_code(tfl_resolve_output(sr_output(), b)))
  # a report that differs from the study says so in its own code
  own <- sr_output(list(tfl_item("plan_digits", list(rounding = "r"))))
  code <- tfl_content_code(tfl_resolve_output(own, a))
  expect_true(any(grepl("rounding = \"r\"", code, fixed = TRUE)))
  # a study without one: no option
  st0 <- tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))
  expect_false(any(grepl("rtfreporter.rounding", tfl_report_setup_code(st0), fixed = TRUE)))
})

test_that("the workbook's study rounding: in the setup, not in each table's code", {
  sp <- tfl_table_spec(
    tables = data.frame(output_id = "T1", cols = "TRT01A", stringsAsFactors = FALSE),
    study = data.frame(key = "rounding", value = "sas", stringsAsFactors = FALSE))
  expect_true(any(grepl("rtfreporter.rounding = \"sas\"", tfl_report_setup_code(sp), fixed = TRUE)))
  expect_false(any(grepl("rounding", tfl_table_code(sp, "T1", normalize = FALSE), fixed = TRUE)))
})
