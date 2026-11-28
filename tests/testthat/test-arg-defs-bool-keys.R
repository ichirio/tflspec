# YAML 1.1's implicit booleans (n, y, yes, no, on, off) are text in a
# template's file: a cells key `n:` is the statistic n, not FALSE

test_that("a template's cells key n stays the text n", {
  d <- tfl_arg_defs(template = "tflspec_v1/tflspec_summary")
  cells <- d$default[[match("cells", d$name)]]
  expect_identical(names(cells)[1L], "n")
})

test_that("an unquoted n, y, no or on key is read as its text", {
  f <- tempfile(fileext = ".yml")
  on.exit(unlink(f), add = TRUE)
  writeLines(c("params:", "  cells:", "    type: map<string,string>",
               "    default: {n: x, y: x, no: x, on: x, true: x}"), f)
  y <- yaml::read_yaml(f, handlers = tflspec:::.arg_yaml_handlers())
  expect_identical(names(y$params$cells$default), c("n", "y", "no", "on", "TRUE"))
})
