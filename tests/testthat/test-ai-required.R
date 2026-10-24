# What defines a report: its analysis set and, for a dataset of parameters
# (PARAMCD), its parameter -- an AI answer without them is an error, never a
# guess (the shell always says them)

req_vars <- data.frame(dataset = c("ADSL", "ADSL", "ADLB", "ADLB", "ADLB", "ADLB"),
                       variable = c("USUBJID", "TRT01A", "USUBJID", "PARAMCD", "PARAM", "AVAL"),
                       stringsAsFactors = FALSE)
req_out <- function(analysis, population = "SAF", rows = list(group = "variable"), data = NULL) {
  tfl_output("T-1", "table", label = "Labs", population = population, extends = "defaults",
             ard = list(analysis_data = data, analyses = list(analysis)),
             content = list(tfl_item("table_plan", list(cols = "TRT01A", rows = rows))))
}
req_problems <- function(o) {
  p <- .ai_check_table_output(o, "outputs[1] T-1", list(populations = "SAF"), req_vars)
  p[p$severity == "error", , drop = FALSE]
}
lab <- function(...) {
  a <- list(id = "LB", method = "continuous", dataset = "ADLB", by = list("TRT01A"),
            variables = list("AVAL"))
  more <- list(...)
  a[names(more)] <- more
  a
}

test_that("a BDS analysis that keeps no parameter is an error saying what to write", {
  p <- req_problems(req_out(lab()))
  expect_true(any(grepl("keeps no parameter", p$problem) & grepl("ADLB", p$problem)))
  expect_match(p$problem[grepl("keeps no parameter", p$problem)], "PARAMCD == ", fixed = TRUE)
  # kept: its where, its data's where, PARAM in by / the rows
  expect_false(any(grepl("keeps no parameter", req_problems(req_out(lab(where = tfl_r('PARAMCD == "ALT"'))))$problem)))
  dd <- list(list(id = "alt", from = "ADLB", where = tfl_r('PARAMCD == "ALT"')))
  a <- lab(data = "alt")
  a$dataset <- NULL
  expect_false(any(grepl("keeps no parameter", req_problems(req_out(a, data = dd))$problem)))
  expect_false(any(grepl("keeps no parameter", req_problems(req_out(lab(by = list("TRT01A", "PARAM"))))$problem)))
  expect_false(any(grepl("keeps no parameter",
                         req_problems(req_out(lab(), rows = list(group = "PARAM")))$problem)))
  # ADSL: no parameters to keep
  expect_false(any(grepl("keeps no parameter", req_problems(req_out(
    list(id = "AGE", method = "continuous", dataset = "ADSL", by = list("TRT01A"),
         variables = list("TRT01A"))))$problem)))
})

test_that("a report with no analysis set anywhere is an error; one on an analysis is enough", {
  a <- lab(where = tfl_r('PARAMCD == "ALT"'))
  p <- req_problems(req_out(a, population = NA_character_))
  expect_true(any(grepl("names no analysis set", p$problem)))
  a$population <- "SAF"
  expect_false(any(grepl("names no analysis set", req_problems(req_out(a, population = NA_character_))$problem)))
})
