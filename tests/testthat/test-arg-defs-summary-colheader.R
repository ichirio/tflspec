# tflspec_summary's col_header is a table: its columns are defined, so a
# screen draws a table field (row_label and .values, as the help says)

test_that("tflspec_summary's col_header defines its table columns", {
  d <- tfl_arg_defs(template = "tflspec_v1/tflspec_summary")
  cols <- d$columns[[match("col_header", d$name)]]
  expect_identical(names(cols), c("row_label", ".values"))
  expect_identical(cols$.values$input, "text")
})
