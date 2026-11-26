# tfl_check_spec(): every `!r` value of a report (its `ard:` aside, which
# the ARD check reads by its own fields) is read as R once; one that does
# not read is an error row with its place in the SPEC and the parser's
# message

test_that("an `!r` value that does not read as R: an error row, its place and the parser's message", {
  skip_if_not_installed("haven")
  d <- file.path(withr::local_tempdir(), "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  haven::write_xpt(tflspec:::.ai_chr(tflspec::adsl), file.path(d, "adam", "adsl.xpt"))
  tfl_ai_study_init(d, adam = "adam", study_id = "RP-1")
  one <- function(id, item) {
    tfl_write_output(tfl_output(id, "table", common = list(population = "SAF"),
      template = list(name = "demog", args = list(by = "TRT01A", continuous = "AGE", categorical = "SEX")),
      content = list(item)), file.path(d, "spec", "outputs", paste0(id, ".yml")))
  }
  # a slot of R (plan_filter's condition), and a guard before a cell template
  one("T-F", tfl_item("plan_filter", list(where = tfl_r("p >="))))
  one("T-G", tfl_item("plan_cells", list(categorical = list(tfl_r("n == 0 ~"), "{n} ({p:.1f%})"))))
  p <- tfl_check_spec(d)
  shown <- paste(utils::capture.output(print(p[, c("output_id", "part", "item", "level", "message")])),
                 collapse = "\n")
  r <- p[grepl("does not read as R", p$message, fixed = TRUE), , drop = FALSE]
  expect_identical(sort(r$output_id), c("T-F", "T-G"), info = shown)
  expect_true(all(r$level == "error"), info = shown)
  expect_identical(r$item[r$output_id == "T-F"], "content$plan_filter$args$where", info = shown)
  expect_identical(r$item[r$output_id == "T-G"], "content$plan_cells$args$categorical$1", info = shown)
  expect_true(all(grepl("unexpected end of input", r$message, fixed = TRUE)), info = shown)
  expect_match(r$message[r$output_id == "T-F"], "p >=", fixed = TRUE)

  # R that reads: no such row
  one("T-F", tfl_item("plan_filter", list(where = tfl_r("p >= 0.05"))))
  one("T-G", tfl_item("plan_cells", list(categorical = list(tfl_r("n == 0 ~ '0'"), "{n} ({p:.1f%})"))))
  p <- tfl_check_spec(d)
  expect_false(any(grepl("does not read as R", p$message, fixed = TRUE)))
})
