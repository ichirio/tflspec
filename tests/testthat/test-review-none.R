# A sheet's (none) is an !unset: no value, not a wrong number

none_spec <- function(v) {
  tflspec:::.table_spec_problems(list(
    page = data.frame(output_id = "T1", margin_top_in = v, stringsAsFactors = FALSE)))$problems
}

test_that("a page field written to the sheets as (none) is no problem in the review", {
  cell <- tflspec:::.sheet_cell(tfl_unset())
  expect_identical(cell, "(none)")
  expect_equal(nrow(none_spec(cell)), 0L)
  expect_equal(nrow(none_spec(" (none) ")), 0L)
})

test_that("a real non-number in a numeric page field is still a problem", {
  expect_gt(nrow(none_spec("wide")), 0L)
  expect_gt(nrow(none_spec("(nothing)")), 0L)
  expect_equal(nrow(none_spec("0.5")), 0L)
})

test_that("(none) still reads back as !unset on the way to YAML", {
  v <- tflspec:::.conv_cell(tflspec:::.sheet_cell(tfl_unset()), "margin_top_in",
                            character(), character(), function(...) NULL, "T1", "page", 1L)
  expect_s3_class(v, "tfl_unset")
})
