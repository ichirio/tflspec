# generate_rtfreport(overwrite =) from the SPEC: a report's page (#452)

test_that("tfl_report_overwrite(): TRUE unless the page says FALSE", {
  d <- system.file("extdata", "spec-yaml", package = "tflspec")
  st <- tfl_read_study_spec(d)
  o <- tfl_read_outputs(d)[["T-14-1-1"]]
  expect_true(tfl_report_overwrite(o, st))
  o$page$generate_rtfreport <- list(overwrite = FALSE)
  expect_false(tfl_report_overwrite(o, st))
  # (a page configuration holds no generate_rtfreport: the report's alone)
  o2 <- tfl_read_outputs(d)[["T-14-1-1"]]
  expect_true(tfl_report_overwrite(o2, st))
  expect_identical(nrow(tfl_schema_check(tfl_item("generate_rtfreport", list(overwrite = FALSE)))), 0L)
  expect_error(tfl_report_overwrite(list()), "tfl_output")
})
