# tfl_ai_prepare() and tfl_ai_write_requests(workbook = ): the workbook
# attached, the requests saying where each shell is in it (#364)

prep_study <- function(d) {
  dir.create(file.path(d, "adam"), recursive = TRUE, showWarnings = FALSE)
  for (x in c("adsl", "adae", "adrs", "adlb")) {
    df <- getExportedValue("tflspec", x)
    for (v in names(df)) if (is.factor(df[[v]])) df[[v]] <- as.character(df[[v]])
    haven::write_xpt(df, file.path(d, "adam", paste0(x, ".xpt")))
  }
  d
}
book <- function() system.file("ai", "example", "onco_toc_shell.xlsx", package = "tflspec")
req_text <- function(d, r) paste(readLines(file.path(d, r$file[1L]), encoding = "UTF-8"), collapse = "\n")

test_that("tfl_ai_prepare(): the guide's step 3 in one call; a study.yml there is kept", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- prep_study(withr::local_tempdir())
  r <- suppressMessages(tfl_ai_prepare(d, book(), study_id = "XYZ-ONC-01"))
  expect_identical(nrow(r), 4L)
  expect_true(all(file.exists(file.path(d, c("study.yml", "spec/data.yml", "ai/adam_summary.md",
                                             "ai/toc_shell.md", "ai/requests/index.md")))))
  # again: the study's settings are kept, the requests written again
  y <- file.path(d, "study.yml")
  writeLines(c(readLines(y), "# kept"), y)
  r2 <- suppressMessages(tfl_ai_prepare(d, book(), study_id = "OTHER", size = 1L))
  expect_true("# kept" %in% readLines(y))
  expect_identical(nrow(r2), 4L)
  # and the answers of the example still pass, as in the guide
  ex <- system.file("ai", "example", "answers", package = "tflspec")
  file.copy(list.files(ex, full.names = TRUE), file.path(d, "ai", "answers"), overwrite = TRUE)
  res <- suppressMessages(tfl_ai_collect_answers(d))
  expect_identical(unique(res$status), "ok")
  expect_error(tfl_ai_prepare(d, file.path(d, "nope.xlsx")), "workbook's path")
})

test_that("workbook = \"attach\" / \"both\": where each shell is in the attached workbook", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- prep_study(withr::local_tempdir())
  r_text <- suppressMessages(tfl_ai_prepare(d, book(), study_id = "XYZ-ONC-01"))
  t_text <- req_text(d, r_text)
  expect_false(grepl("is attached to this message", t_text, fixed = TRUE))
  expect_match(t_text, "# Workbook onco_toc_shell.xlsx", fixed = TRUE)
  r_att <- suppressMessages(tfl_ai_prepare(d, book(), workbook = "attach"))
  t_att <- req_text(d, r_att)
  expect_match(t_att, "The TOC / SHELL workbook `onco_toc_shell.xlsx` is attached to this message.",
               fixed = TRUE)
  expect_match(t_att, sprintf("- %s: sheet '", r_att$outputs[1L]), fixed = TRUE)
  expect_match(t_att, "its TOC row: sheet '", fixed = TRUE)
  expect_false(grepl("# Workbook onco_toc_shell.xlsx", t_att, fixed = TRUE))   # no text
  expect_lt(nchar(t_att), nchar(t_text))
  expect_match(paste(readLines(file.path(d, "ai", "requests", "index.md")), collapse = "\n"),
               "Attach `onco_toc_shell.xlsx`", fixed = TRUE)
  r_both <- suppressMessages(tfl_ai_prepare(d, book(), workbook = "both"))
  t_both <- req_text(d, r_both)
  expect_match(t_both, "is attached to this message", fixed = TRUE)
  expect_match(t_both, "# Workbook onco_toc_shell.xlsx", fixed = TRUE)
  expect_match(t_both, "the attached workbook is right", fixed = TRUE)
})

test_that("the places: a report's rows of a sheet with several, else the whole sheet", {
  skip_if_not_installed("openxlsx")
  f <- tempfile(fileext = ".xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "TOC")
  openxlsx::writeData(wb, "TOC", data.frame(No = c("Table 14.1.1", "Table 14.1.2", "Table 14.3.1"),
                                            Title = c("Demographics", "Disposition", "AEs")))
  openxlsx::addWorksheet(wb, "Shells")
  openxlsx::writeData(wb, "Shells", data.frame(x = c("Table 14.1.1", "Age", "Sex", "", "Table 14.1.2",
                                                     "Completed")), colNames = FALSE)
  openxlsx::addWorksheet(wb, "T14.3.1")
  openxlsx::writeData(wb, "T14.3.1", data.frame(x = c("Table 14.3.1", "SOC", "PT")), colNames = FALSE)
  openxlsx::saveWorkbook(wb, f)
  x <- tfl_ai_read_shell(f)
  w <- tflspec:::.ai_shell_where(x, c("Table 14.1.1", "Table 14.1.2", "Table 14.3.1"))
  expect_match(w[1L], "sheet 'Shells', cells A1:A3", fixed = TRUE)
  expect_match(w[2L], "sheet 'Shells', cells A5:A6", fixed = TRUE)
  expect_match(w[3L], "sheet 'T14.3.1', the whole sheet", fixed = TRUE)
  expect_match(w[1L], "its TOC row: sheet 'TOC', row 2", fixed = TRUE)
})
