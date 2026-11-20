# An enum type with another type after it: the values are the enum's own

test_that("enum[TRUE|FALSE]|logical gives the values TRUE and FALSE", {
  expect_identical(tflspec:::.arg_enum_values("enum[TRUE|FALSE]|logical"), c("TRUE", "FALSE"))
  expect_identical(tflspec:::.arg_enum_values("enum[a|b]"), c("a", "b"))
  expect_null(tflspec:::.arg_enum_values("logical"))
  d <- tfl_arg_defs(fn = "plan_cell_style")
  expect_identical(d$values[[match("bold", d$name)]], c("TRUE", "FALSE"))
})
