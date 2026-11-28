# tflspec_summary's col_header is a table: the header's cells, the tables
# sheet's col_header columns (line, cols, span, text; canon 11-28), so a
# screen draws a table field

test_that("tflspec_summary's col_header defines its table columns", {
  d <- tfl_arg_defs(template = "tflspec_v1/tflspec_summary")
  cols <- d$columns[[match("col_header", d$name)]]
  expect_identical(names(cols), c("line", "cols", "span", "text"))
  expect_identical(cols$text$input, "text")
})
