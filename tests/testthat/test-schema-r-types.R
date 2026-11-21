test_that("cell_format and col_header are R fields", {
  d <- tfl_arg_defs(fn = "plan_columns")
  expect_identical(d$type[d$name == "cell_format"], "r")
  expect_identical(d$input[d$name == "cell_format"], "r")
  d <- tfl_arg_defs(fn = "plan_paginate_cols")
  expect_identical(d$type[d$name == "col_header"], "r")
  expect_identical(d$input[d$name == "col_header"], "r")
})
