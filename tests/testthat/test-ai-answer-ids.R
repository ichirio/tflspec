# A tester's answer read as it came (#365): a report number as the TOC writes
# it ("Table 14.1.1") made the file's id, an answer's report named another
# way taken as the one asked; a table with no column key, an overall row no
# analysis makes, and the next step when the answer has none of the reports

prep_example <- function(d) {
  dir.create(file.path(d, "adam"), recursive = TRUE, showWarnings = FALSE)
  for (x in c("adsl", "adae", "adrs", "adlb")) {
    df <- getExportedValue("tflspec", x)
    for (v in names(df)) if (is.factor(df[[v]])) df[[v]] <- as.character(df[[v]])
    haven::write_xpt(df, file.path(d, "adam", paste0(x, ".xpt")))
  }
  ex <- system.file("ai", "example", package = "tflspec")
  tfl_ai_study_init(d, adam = "adam", study_id = "XYZ-ONC-01", overwrite = TRUE)
  suppressMessages(tfl_ai_write_requests(d, tfl_ai_read_shell(file.path(ex, "onco_toc_shell.xlsx")),
                                         tfl_ai_adam_summary(file.path(d, "adam")), overwrite = TRUE))
  d
}
answer_of <- function(name) {
  readLines(system.file("ai", "example", "answers", paste0(name, ".md"), package = "tflspec"),
            encoding = "UTF-8")
}
put <- function(d, name, lines) {
  writeLines(enc2utf8(lines), file.path(d, "ai", "answers", paste0(name, ".md")), useBytes = TRUE)
}
row_of <- function(res, id) res$reports[res$reports$output_id == id, , drop = FALSE]

test_that(".ai_file_id(): letters, digits, . _ - only; an id that is one stays", {
  expect_identical(tflspec:::.ai_file_id(c("T-14-1-1", "Table 15.3.1.3.13.a", " Table  14.1.1 ")),
                   c("T-14-1-1", "Table-15.3.1.3.13.a", "Table-14.1.1"))
  expect_identical(tflspec:::.ai_file_id("Table　１４.1.1"), "Table-14.1.1")
})

test_that("an answer's report number as the TOC writes it, or another way, is the report asked for", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- prep_example(withr::local_tempdir())
  # "Table 14.1.1" (a space: as the prompt asks, the number as written)
  a <- answer_of("demographics-1")
  put(d, "demographics-1", gsub("T-14-1-1", "Table 14.1.1", a, fixed = TRUE))
  res <- suppressMessages(tfl_ai_collect_answers(d))
  expect_identical(row_of(res, "T-14-1-1")$status, "ok")
  expect_true(file.exists(file.path(d, "spec", "outputs", "T-14-1-1.yml")))
  # an answer whose report is none of those asked: an error, and the next
  # step is its repair, not "done"
  put(d, "demographics-1", gsub("T-14-1-1", "T-99-9-9", a, fixed = TRUE))
  res <- suppressMessages(tfl_ai_collect_answers(d))
  expect_identical(row_of(res, "T-14-1-1")$status, "error")
  expect_match(res$next_steps[["demographics-1"]], "repairs/demographics-1.md", fixed = TRUE)
  expect_false(grepl("done", res$next_steps[["demographics-1"]], fixed = TRUE))
})

test_that("check: a table with no column key is an error; an overall row no analysis makes, a warning", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- prep_example(withr::local_tempdir())
  ae <- answer_of("ae-1")
  # no cols in table_plan
  put(d, "ae-1", sub("table_plan: {cols: TRT01A, rows:", "table_plan: {rows:", ae, fixed = TRUE))
  res <- suppressMessages(tfl_ai_collect_answers(d))
  r <- res$reports[res$reports$request == "ae-1", , drop = FALSE]
  expect_identical(unique(r$status), "error")
  expect_match(r$first[1L], "has no `cols`", fixed = TRUE)
  rep <- paste(readLines(file.path(d, "ai", "repairs", "ae-1.md"), encoding = "UTF-8"), collapse = "\n")
  expect_match(rep, "one arm too", fixed = TRUE)
  # the overall row without over_variables
  put(d, "ae-1", sub(", args: !r over_variables = TRUE}", "}", ae, fixed = TRUE))
  res <- suppressMessages(tfl_ai_collect_answers(d))
  r <- res$reports[res$reports$request == "ae-1", , drop = FALSE]
  expect_identical(unique(r$status), "warning")
  expect_match(r$first[1L], "over_variables = TRUE", fixed = TRUE)
  # as the example has it: ok
  put(d, "ae-1", ae)
  res <- suppressMessages(tfl_ai_collect_answers(d))
  expect_identical(unique(res$reports$status[res$reports$request == "ae-1"]), "ok")
})
