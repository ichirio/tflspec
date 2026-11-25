test_that("tfl_object_defs() has the objects of the three steps", {
  d <- tfl_object_defs()
  expect_identical(names(d), c("step", "default_name", "fixed", "pattern", "final"))
  expect_identical(d$step, c("data", "data", "analysis", "analysis", "analysis", "content",
                             "content", "content"))
  expect_identical(d$default_name, c("{dataset}_sub", "{kind}_{n}", ".ard_{id}", "ard",
                                     "{kind}_{n}", "plan", "content", "{kind}_{n}"))
  expect_identical(d$final, c(FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE))
  # every name can be changed, the results' too (canon 356b2ae)
  expect_true(all(!d$fixed))
  expect_false("page" %in% d$step)
  expect_identical(sum(d$final[d$step == "analysis"]), 1L)
  expect_identical(d$default_name[d$step == "analysis" & d$final], "ard")
})

test_that("a name matches the pattern and is no reserved word", {
  p <- unique(tfl_object_defs()$pattern)
  expect_length(p, 1L)
  ok <- c("adsl_sub", ".ard_zz1", "ard", "plan", "content", "plot_1", "obj_2", "Zz.data")
  bad <- c("1st", "_x", "zz-sub", "zz sub", "")
  expect_true(all(grepl(p, ok)))
  expect_false(any(grepl(p, bad)))
  expect_true(all(tflspec:::.object_name_ok(ok)))
  expect_false(any(tflspec:::.object_name_ok(c(bad, "if", "TRUE", "NULL", "function", ".2x", NA))))
  # each default with its fields filled in is a good name
  filled <- c("adsl_sub", ".ard_zz", "ard", "plan", "content", "data_1", "obj_1", "plot_1")
  expect_true(all(tflspec:::.object_name_ok(filled)))
})

test_that("the step filter keeps one step; another step stops", {
  for (st in c("data", "analysis", "content")) {
    d <- tfl_object_defs(st)
    expect_true(nrow(d) > 0L, info = st)
    expect_true(all(d$step == st), info = st)
    expect_identical(rownames(d), as.character(seq_len(nrow(d))))
  }
  expect_identical(nrow(tfl_object_defs("content")), 3L)
  expect_error(tfl_object_defs("page"), "data, analysis, content")
})
