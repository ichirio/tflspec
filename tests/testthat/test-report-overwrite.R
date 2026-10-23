# generate_rtfreport(overwrite =) from the SPEC: a report's page, its
# pattern's or the study's defaults (#452)

test_that("tfl_report_overwrite(): TRUE unless the page says FALSE", {
  d <- system.file("extdata", "spec-yaml", package = "tflspec")
  st <- tfl_read_study_spec(d)
  o <- tfl_read_outputs(d)[["T-14-1-1"]]
  expect_true(tfl_report_overwrite(o, st))
  o$page$generate_rtfreport <- list(overwrite = FALSE)
  expect_false(tfl_report_overwrite(o, st))
  st$defaults$page$generate_rtfreport <- list(overwrite = FALSE)
  o2 <- tfl_read_outputs(d)[["T-14-1-1"]]
  expect_false(tfl_report_overwrite(o2, st))
  expect_identical(nrow(tfl_schema_check(tfl_item("generate_rtfreport", list(overwrite = FALSE)))), 0L)
  expect_error(tfl_report_overwrite(list()), "tfl_output")
})
