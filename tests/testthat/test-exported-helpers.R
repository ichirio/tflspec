test_that("tfl_split_bar splits at bars outside brackets, quotes and ||", {
  expect_identical(tfl_split_bar("AGE | SEX"), c("AGE", "SEX"))
  expect_identical(tfl_split_bar('ifelse(is.na(X) | X != "Y", 1, 0) | G'),
                   c('ifelse(is.na(X) | X != "Y", 1, 0)', "G"))
  expect_identical(tfl_split_bar("a || b"), "a || b")
  expect_identical(tfl_split_bar(NA), character())
  expect_identical(tfl_split_bar(NULL), character())
  expect_identical(tfl_split_bar("  "), character())
  expect_identical(tfl_split_bar("AGE | SEX"), tfl_split_bar("AGE | SEX"))
})

test_that("tfl_parse_formats reads a formats cell as a named vector", {
  expect_identical(tfl_parse_formats("mean=xx.x | AGE:sd=xx.xx"),
                   c(mean = "xx.x", `AGE:sd` = "xx.xx"))
  expect_identical(unname(tfl_parse_formats("mean=xx.x | sd")), c("xx.x", NA))
  expect_identical(tfl_parse_formats(NA), character())
})

test_that("tfl_fmt_ok accepts the formats the ARD code can use", {
  expect_identical(tfl_fmt_ok(c("xx.x", "xx.x%", "xx.x*1000", "2", "pvalue")),
                   rep(TRUE, 5))
  expect_identical(tfl_fmt_ok(c("xx.x*0", "xx.x*", "abc")), rep(FALSE, 3))
})

test_that("tfl_stat_kinds maps a method's kind to a statistics kind", {
  expect_identical(tfl_stat_kinds("continuous"), "continuous")
  expect_identical(tfl_stat_kinds("categorical"), "categorical")
  expect_identical(tfl_stat_kinds("missing"), "missing")
  expect_identical(tfl_stat_kinds(NULL), "result")
  expect_identical(tfl_stat_kinds("other"), "result")
})

test_that("tfl_normalize_ard_sheet puts a sheet in shape", {
  d <- data.frame(key = c(" source ", NA), value = c("adsl", NA), x = 1:2)
  out <- tfl_normalize_ard_sheet(d, "study")
  expect_identical(names(out), c("key", "value"))
  expect_identical(out$key, "source")
  expect_identical(out$value, "adsl")
  # an open sheet keeps its other columns, but not a note
  d2 <- data.frame(output_id = "T1", analysis_id = "A1", row = "1",
                   p = "x", note = "n")
  out2 <- tfl_normalize_ard_sheet(d2, "conditions")
  expect_identical(names(out2), c("output_id", "analysis_id", "row", "p"))
  expect_error(tfl_normalize_ard_sheet(d, "nope"), "`sheet` must be one of")
})
