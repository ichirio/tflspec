# The report numbers each shell sheet writes, which of them its request
# makes (the rule), and the person's correction over it: given to
# tfl_ai_prepare(numbers = ), saved as ai/sheet_numbers.csv, followed by
# the requests and the collect step

# A company-style workbook: a TOC; a sheet writing its own number and a
# reference to another report's; that report's sheet; a sheet whose number
# no TOC item has; a TOC item with no sheet
numbers_book <- function(f) {
  wb <- openxlsx::createWorkbook()
  toc <- data.frame(`Output No.` = paste("Table", c("14.1.1", "14.2.1", "14.3.1")),
                    Title = c("Summary of Demographics", "Summary of Adverse Events",
                              "Laboratory Values"),
                    Population = "Safety", check.names = FALSE, stringsAsFactors = FALSE)
  openxlsx::addWorksheet(wb, "TOC")
  openxlsx::writeData(wb, "TOC", toc)
  shell <- function(s, lines) {
    openxlsx::addWorksheet(wb, s)
    for (k in seq_along(lines)) openxlsx::writeData(wb, s, lines[[k]], startRow = k, colNames = FALSE)
  }
  body <- list(t(c("", "Placebo", "Drug X")), t(c("n", "xx", "xx")), t(c("Mean (SD)", "xx.x", "xx.x")))
  shell("t14.1.1", c(list("Table 14.1.1  Summary of Demographics", "Safety Analysis Set"), body,
                     list("Note: the adverse event counts are in Table 14.2.1")))
  shell("t14.2.1", c(list("Table 14.2.1  Summary of Adverse Events", "Safety Analysis Set"), body))
  shell("vitals", c(list("Table 14.9.9  Summary of Vital Signs", "Safety Analysis Set"), body))
  openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
  f
}

numbers_study <- function() {
  d <- tempfile("sheetnum-")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  haven::write_xpt(tflspec:::.ai_chr(tflspec::adsl), file.path(d, "adam", "adsl.xpt"))
  d
}

prep_numbers <- function(d, f, ..., adam = "adam") {
  tfl_ai_prepare(d, f, adam = adam, quiet = TRUE, mode = "agent", templates = FALSE, ...)
}

test_that("a dry run's numbers: a row a number a sheet writes, the rule's make", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("haven")
  f <- numbers_book(tempfile(fileext = ".xlsx"))
  d <- numbers_study()
  on.exit(unlink(c(f, d), recursive = TRUE), add = TRUE)
  dr <- prep_numbers(d, f, dry_run = TRUE)
  n <- dr$numbers
  expect_identical(names(n), c("sheet", "number", "title", "toc_id", "make", "source"))
  expect_identical(n$sheet, c("t14.1.1", "t14.1.1", "t14.2.1", "vitals"))
  expect_identical(n$number, paste("Table", c("14.1.1", "14.2.1", "14.2.1", "14.9.9")))
  expect_identical(n$toc_id, c("Table 14.1.1", "Table 14.2.1", "Table 14.2.1", NA))
  expect_identical(n$title, c("Summary of Demographics", "Summary of Adverse Events",
                              "Summary of Adverse Events", "Summary of Vital Signs"))
  # written in the sheet and a TOC item's: made; no TOC item: not
  expect_identical(n$make, c(TRUE, TRUE, TRUE, FALSE))
  expect_true(all(n$source == "rule"))
  expect_identical(dr$requests$outputs[dr$requests$request == "t14.1.1"],
                   "Table 14.1.1, Table 14.2.1")
  # a dry run writes nothing, not even the study folder
  d2 <- file.path(tempfile("sheetnum-none-"), "study")
  dr2 <- suppressWarnings(prep_numbers(d2, f, dry_run = TRUE, adam = file.path(d, "adam"),
                                       numbers = list(vitals = "Table 14.9.9")))
  expect_false(dir.exists(d2))
  expect_identical(dr2$numbers$source[dr2$numbers$sheet == "vitals"], "given")
})

test_that("numbers: make FALSE a reference only, make TRUE with no TOC item a report (warned)", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("haven")
  f <- numbers_book(tempfile(fileext = ".xlsx"))
  d <- numbers_study()
  on.exit(unlink(c(f, d), recursive = TRUE), add = TRUE)
  given <- data.frame(sheet = c("t14.1.1", "vitals"), number = c("Table 14.2.1", "Table 14.9.9"),
                      make = c(FALSE, TRUE), stringsAsFactors = FALSE)
  expect_warning(r <- prep_numbers(d, f, numbers = given), "no TOC item has Table 14.9.9 (sheet 'vitals')",
                 fixed = TRUE)
  rq <- r$requests
  expect_identical(rq$outputs[rq$request == "t14.1.1"], "Table 14.1.1")
  expect_identical(rq$outputs[rq$request == "vitals"], "Table 14.9.9")
  expect_identical(r$numbers$make, c(TRUE, FALSE, TRUE, TRUE))
  expect_identical(r$numbers$source, c("rule", "given", "rule", "given"))
  # the request still names the reference, as a reference
  p <- paste(readLines(file.path(d, rq$file[rq$request == "t14.1.1"]), encoding = "UTF-8"),
             collapse = "\n")
  expect_match(p, "- Table 14.2.1 (its number is written in the sheet", fixed = TRUE)
  expect_match(p, "reference only, not this sheet's reports: Table 14.2.1", fixed = TRUE)
  # saved for the next call
  csv <- utils::read.csv(file.path(d, "ai", "sheet_numbers.csv"), stringsAsFactors = FALSE)
  expect_identical(names(csv), c("sheet", "number", "make"))
  expect_identical(csv$make, c(FALSE, TRUE))
  ctx <- readRDS(file.path(d, "ai", "requests", "_contexts.rds"))
  expect_identical(ctx[["t14.1.1"]]$context$candidates, "Table 14.1.1")
  expect_identical(ctx[["vitals"]]$context$sheet_numbers$make, TRUE)
})

test_that("numbers as a list; make NA back to the rule; the saved file used, then replaced", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("haven")
  f <- numbers_book(tempfile(fileext = ".xlsx"))
  d <- numbers_study()
  on.exit(unlink(c(f, d), recursive = TRUE), add = TRUE)
  dr <- prep_numbers(d, f, dry_run = TRUE, numbers = list(t14.1.1 = "Table 14.1.1"))
  expect_identical(dr$numbers$make, c(TRUE, FALSE, TRUE, FALSE))
  expect_identical(dr$numbers$source, c("given", "given", "rule", "rule"))
  # a TOC item the rule left out (no sheet: the no-shell request's) made a
  # sheet's report
  dr <- prep_numbers(d, f, dry_run = TRUE, numbers = list(t14.2.1 = c("Table 14.2.1", "14.3.1")))
  expect_identical(dr$requests$outputs[dr$requests$request == "t14.2.1"],
                   "Table 14.2.1, Table 14.3.1")
  expect_false("no-shell" %in% dr$requests$request)
  r <- prep_numbers(d, f, numbers = data.frame(sheet = c("t14.1.1", "t14.1.1"),
                                               number = c("Table 14.2.1", "Table 14.1.1"),
                                               make = c(FALSE, NA)))
  expect_identical(utils::read.csv(file.path(d, "ai", "sheet_numbers.csv"))$number, "Table 14.2.1")
  expect_identical(r$numbers$source, c("rule", "given", "rule", "rule"))
  # the next call, a dry run or not, reads it
  dr <- prep_numbers(d, f, dry_run = TRUE)
  expect_identical(dr$numbers$source, c("rule", "saved", "rule", "rule"))
  expect_identical(dr$requests$outputs[dr$requests$request == "t14.1.1"], "Table 14.1.1")
  r <- prep_numbers(d, f)
  expect_identical(r$requests$outputs[r$requests$request == "t14.1.1"], "Table 14.1.1")
  # numbers given again: the file replaced
  r <- prep_numbers(d, f, numbers = data.frame(sheet = "t14.2.1", number = "Table 14.2.1",
                                               make = TRUE))
  expect_identical(utils::read.csv(file.path(d, "ai", "sheet_numbers.csv"))$sheet, "t14.2.1")
  expect_identical(r$requests$outputs[r$requests$request == "t14.1.1"], "Table 14.1.1, Table 14.2.1")
  # the input checked
  expect_error(prep_numbers(d, f, dry_run = TRUE,
                            numbers = data.frame(sheet = "Nope", number = "14.1.1", make = TRUE)),
               "names no shell sheet 'Nope'")
  expect_error(prep_numbers(d, f, dry_run = TRUE, numbers = data.frame(sheet = "t14.1.1")),
               "has no column number, make")
  expect_error(prep_numbers(d, f, dry_run = TRUE, numbers = list("Table 14.1.1")), "named by sheet")
})

test_that("collect follows the corrected reports: a reference not answered is not missing", {
  skip_on_cran()
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("haven")
  f <- numbers_book(tempfile(fileext = ".xlsx"))
  d <- numbers_study()
  on.exit(unlink(c(f, d), recursive = TRUE), add = TRUE)
  a <- readLines(test_path("fixtures", "answers-toc", "demographics-1.md"), encoding = "UTF-8")
  put <- function(name, id) {
    writeLines(enc2utf8(gsub("T-14-1-1", id, a, fixed = TRUE)),
               file.path(d, "ai", "answers", paste0(name, ".md")), useBytes = TRUE)
  }
  # the rule: the sheet's reference is one of its reports, missing
  prep_numbers(d, f)
  put("t14.1.1", "Table 14.1.1")
  res <- suppressMessages(tfl_ai_collect_answers(d))
  rows <- res$reports[res$reports$request == "t14.1.1", , drop = FALSE]
  expect_identical(rows$output_id, c("Table-14.1.1", "Table-14.2.1"))
  expect_match(rows$first[2L], "not in the answer", fixed = TRUE)
  # corrected: the reference no report of the sheet; the number no TOC item
  # has, a report with no warning of it
  suppressWarnings(prep_numbers(d, f, numbers = list(t14.1.1 = "Table 14.1.1",
                                                     vitals = "Table 14.9.9")))
  put("vitals", "Table 14.9.9")
  res <- suppressMessages(tfl_ai_collect_answers(d))
  rows <- res$reports[res$reports$request == "t14.1.1", , drop = FALSE]
  expect_identical(rows$output_id, "Table-14.1.1")
  v <- res$reports[res$reports$request == "vitals", , drop = FALSE]
  expect_identical(v$output_id, "Table-14.9.9")
  expect_false(grepl("no TOC item", v$problems_text %||% ""))
  expect_true(v$status %in% c("ok", "warning"))
})
