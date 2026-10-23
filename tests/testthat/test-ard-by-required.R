# A table's analyses have its columns' `by`: none is a problem saying the
# by to write; by: [] says "no columns"; the groups' N needs none

by_study <- function(d, analyses) {
  tplfig_study(d, "adsl", "BY-1")
  o <- tfl_output("T1", "table", ard = list(analyses = analyses),
                  content = list(tfl_item("table_plan", list(cols = "TRT01A"))))
  tfl_write_output(o, file.path(d, "spec", "outputs", "T1.yml"))
  tfl_read_study_spec(d)
}

by_an <- function(id, by = NULL, variables = "AGE", method = "cards::ard_summary") {
  Filter(Negate(is.null), list(id = id, method = method, dataset = "ADSL",
                               population = "SAF", by = by, variables = variables))
}

test_that("an analysis without by in a table with columns: a problem saying by: [TRT01A]", {
  skip_if_not_installed("haven")
  d <- withr_tempdir()
  st <- by_study(d, list(by_an("N", variables = "TRT01A", method = "cards::ard_tabulate"),
                         by_an("AGE")))
  e <- tryCatch(tfl_ard_code(st, output_id = "T1", save = FALSE), error = function(e) e)
  expect_s3_class(e, "tflspec_ard_spec_error")
  expect_match(conditionMessage(e), "T1 / AGE: no `by`", fixed = TRUE)
  expect_match(conditionMessage(e), "write by: [TRT01A]", fixed = TRUE)
  # (the groups' N, the columns' own count: none needed)
  expect_false(grepl("T1 / N:", conditionMessage(e), fixed = TRUE))
})

test_that("by: [] says no columns; with the by, the program is written", {
  skip_if_not_installed("haven")
  d <- withr_tempdir()
  st <- by_study(d, list(by_an("AGE", by = list())))
  sh <- tfl_sheets_from_yaml(d)
  expect_identical(sh$ard$analyses$by[sh$ard$analyses$output_id == "T1"], "(none)")
  code <- tfl_ard_code(st, output_id = "T1", save = FALSE, part = "body")
  expect_false(any(grepl("(none)", code, fixed = TRUE)))
  expect_false(any(grepl("by = ", code, fixed = TRUE)))
  # back to YAML: by: [] again
  out <- withr_tempdir()
  suppressMessages(tfl_yaml_from_sheets(sh, out))
  f <- list.files(out, "^T1[.]yml$", recursive = TRUE, full.names = TRUE)
  expect_identical(tfl_read_output(f[1L])$ard$analyses[[1L]]$by, list())
  d2 <- withr_tempdir()
  st2 <- by_study(d2, list(by_an("AGE", by = "TRT01A")))
  expect_match(paste(tfl_ard_code(st2, output_id = "T1", save = FALSE, part = "body"),
                     collapse = "\n"), "by = TRT01A", fixed = TRUE)
})

test_that("a by the data does not have stops the program, not left out", {
  skip_if_not_installed("haven")
  d <- withr_tempdir()
  st <- by_study(d, list(by_an("AGE", by = "TRTXX")))
  expect_error(suppressMessages(tfl_build_ard(st, dir = d, output_id = "T1", save = FALSE)),
               "TRTXX")
})

test_that("a listing or a table without columns needs no by", {
  a <- data.frame(output_id = "T1", analysis_id = "AGE", method = "cards::ard_summary",
                  by = NA, variables = "AGE", stringsAsFactors = FALSE)
  expect_identical(nrow(tflspec:::.by_needed_problems(a, data.frame(output_id = "T1", cols = NA))), 0L)
  expect_identical(nrow(tflspec:::.by_needed_problems(a, data.frame(output_id = "T2", cols = "ARM"))), 0L)
  expect_identical(nrow(tflspec:::.by_needed_problems(a, data.frame(output_id = "T1", cols = "ARM"))), 1L)
})
