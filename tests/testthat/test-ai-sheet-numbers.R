# The report numbers each shell sheet writes, which of them its request
# makes (the rule), and the person's correction over it: given to
# tfl_ai_prepare(numbers = ), saved as ai/sheet_numbers.csv, followed by
# the requests and the collect step

# A company-style workbook: a TOC; a sheet writing its own number and a
# reference to another report's; that report's sheet; a sheet whose number
# no TOC item has; a TOC item with no sheet; TOC items with no shell whose
# number with a part letter one sheet writes (14.4.1.b), two sheets in the
# body alike (14.5.1: the first in the workbook makes it)
numbers_book <- function(f) {
  wb <- openxlsx::createWorkbook()
  toc <- data.frame(`Output No.` = paste("Table", c("14.1.1", "14.2.1", "14.3.1", "14.4.1",
                                                  "14.5.1")),
                    Title = c("Summary of Demographics", "Summary of Adverse Events",
                              "Laboratory Values", "Concomitant Medications", "Medical History"),
                    Population = "Safety", check.names = FALSE, stringsAsFactors = FALSE)
  openxlsx::addWorksheet(wb, "TOC")
  openxlsx::writeData(wb, "TOC", toc)
  shell <- function(s, lines) {
    openxlsx::addWorksheet(wb, s)
    for (k in seq_along(lines)) openxlsx::writeData(wb, s, lines[[k]], startRow = k, colNames = FALSE)
  }
  body <- list(t(c("", "Placebo", "Drug X")), t(c("n", "xx", "xx")), t(c("Mean (SD)", "xx.x", "xx.x")))
  shell("t14.1.1", c(list("Table 14.1.1  Summary of Demographics", "Safety Analysis Set"), body,
                     list("Note: the adverse event counts are in Table 14.2.1",
                          "Part A: Table 14.5.1.a")))
  shell("t14.2.1", c(list("Table 14.2.1  Summary of Adverse Events", "Safety Analysis Set"), body,
                     list("Part B: Table 14.4.1.b", "Part B: Table 14.5.1.b")))
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
  expect_identical(names(n), c("sheet", "number", "title", "toc_id", "make", "source",
                                "reason"))
  expect_identical(n$sheet, rep(c("t14.1.1", "t14.2.1", "vitals"), c(3, 3, 1)))
  expect_identical(n$number, paste("Table", c("14.1.1", "14.2.1", "14.5.1.a", "14.2.1",
                                              "14.4.1.b", "14.5.1.b", "14.9.9")))
  # a part of a TOC item (14.4.1.b): its toc_id the item's
  expect_identical(n$toc_id, c(paste("Table", c("14.1.1", "14.2.1", "14.5.1", "14.2.1", "14.4.1",
                                                "14.5.1")), NA))
  expect_identical(n$title[c(1, 5, 7)], c("Summary of Demographics", "Concomitant Medications",
                                          "Summary of Vital Signs"))
  # written in the sheet and a TOC item's: made; no TOC item: not; a TOC
  # item with no shell one sheet writes a part of: made (a guess); two
  # sheets alike: the first in the workbook, the other a candidate
  expect_identical(n$make, c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
  expect_identical(n$source, c("rule", "rule", "guess", "rule", "guess", "guess", "rule"))
  expect_identical(is.na(n$reason), n$source == "rule")
  expect_match(n$reason[6], "^2nd choice; .*chosen: sheet 't14.1.1'$")
  expect_identical(dr$requests$outputs[dr$requests$request == "t14.1.1"],
                   "Table 14.1.1, Table 14.2.1, Table 14.5.1.a")
  # the report's id the part the sheet writes; its TOC item asked by it
  expect_identical(dr$requests$outputs[dr$requests$request == "t14.2.1"],
                   "Table 14.2.1, Table 14.4.1.b")
  expect_identical(dr$toc$request[dr$toc$toc_id == "Table 14.4.1"], "t14.2.1")
  expect_identical(dr$toc$request[dr$toc$toc_id == "Table 14.5.1"], "t14.1.1")
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
  expect_identical(rq$outputs[rq$request == "t14.1.1"], "Table 14.1.1, Table 14.5.1.a")
  expect_identical(rq$outputs[rq$request == "vitals"], "Table 14.9.9")
  expect_identical(r$numbers$make, c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE))
  expect_identical(r$numbers$reason[c(2, 7)], rep("set by the person", 2))
  expect_identical(r$numbers$source, c("rule", "given", "guess", "rule", "guess", "guess", "given"))
  # the request still names the reference, as a reference
  p <- paste(readLines(file.path(d, rq$file[rq$request == "t14.1.1"]), encoding = "UTF-8"),
             collapse = "\n")
  expect_match(p, "- Table 14.2.1 (its number is written in the sheet", fixed = TRUE)
  expect_match(p, "reference only, not this sheet's reports: Table 14.2.1", fixed = TRUE)
  expect_match(p, "- Table 14.5.1.a: the TOC item Table 14.5.1 has no shell of its own; this sheet was chosen",
               fixed = TRUE)
  expect_match(p, "Other candidate sheets (references only): 't14.2.1' (2nd choice", fixed = TRUE)
  # saved for the next call
  csv <- utils::read.csv(file.path(d, "ai", "sheet_numbers.csv"), stringsAsFactors = FALSE)
  expect_identical(names(csv), c("sheet", "number", "make"))
  expect_identical(csv$make, c(FALSE, TRUE))
  ctx <- readRDS(file.path(d, "ai", "requests", "_contexts.rds"))
  expect_identical(ctx[["t14.1.1"]]$context$candidates, c("Table 14.1.1", "Table 14.5.1.a"))
  expect_identical(ctx[["vitals"]]$context$sheet_numbers$make, TRUE)
})

test_that("numbers as a list; make NA back to the rule; the saved file used, then replaced", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("haven")
  f <- numbers_book(tempfile(fileext = ".xlsx"))
  d <- numbers_study()
  on.exit(unlink(c(f, d), recursive = TRUE), add = TRUE)
  dr <- prep_numbers(d, f, dry_run = TRUE, numbers = list(t14.1.1 = "Table 14.1.1"))
  expect_identical(dr$numbers$make, c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE))
  expect_identical(dr$numbers$source, c("given", "given", "given", "rule", "guess", "guess", "rule"))
  # a TOC item the rule left out (no sheet: the no-shell request's) made a
  # sheet's report
  dr <- prep_numbers(d, f, dry_run = TRUE, numbers = list(t14.2.1 = c("Table 14.2.1", "14.3.1")))
  expect_identical(dr$requests$outputs[dr$requests$request == "t14.2.1"],
                   "Table 14.2.1, Table 14.3.1")
  # (14.4.1, the guess its other number made, now made in no sheet: no shell)
  expect_identical(dr$requests$outputs[dr$requests$request == "no-shell"], "Table 14.4.1")
  r <- prep_numbers(d, f, numbers = data.frame(sheet = c("t14.1.1", "t14.1.1"),
                                               number = c("Table 14.2.1", "Table 14.1.1"),
                                               make = c(FALSE, NA)))
  expect_identical(utils::read.csv(file.path(d, "ai", "sheet_numbers.csv"))$number, "Table 14.2.1")
  expect_identical(r$numbers$source, c("rule", "given", "guess", "rule", "guess", "guess", "rule"))
  # the next call, a dry run or not, reads it
  dr <- prep_numbers(d, f, dry_run = TRUE)
  expect_identical(dr$numbers$source, c("rule", "saved", "guess", "rule", "guess", "guess", "rule"))
  expect_identical(dr$requests$outputs[dr$requests$request == "t14.1.1"],
                   "Table 14.1.1, Table 14.5.1.a")
  r <- prep_numbers(d, f)
  expect_identical(r$requests$outputs[r$requests$request == "t14.1.1"],
                   "Table 14.1.1, Table 14.5.1.a")
  # numbers given again: over the saved rows, the file keeping both
  r <- prep_numbers(d, f, numbers = data.frame(sheet = "t14.2.1", number = "Table 14.2.1",
                                               make = TRUE))
  expect_identical(utils::read.csv(file.path(d, "ai", "sheet_numbers.csv"))$sheet,
                   c("t14.1.1", "t14.2.1"))
  expect_identical(r$requests$outputs[r$requests$request == "t14.1.1"],
                   "Table 14.1.1, Table 14.5.1.a")
  expect_identical(r$numbers$source[1:4], c("rule", "saved", "guess", "given"))
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
  expect_identical(rows$output_id, c("Table-14.1.1", "Table-14.2.1", "Table-14.5.1.a"))
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

test_that("a TOC item with no shell: one sheet writing its part makes it (out of no-shell)", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("haven")
  # (the sheet's three numbers of its own fill its candidates: 20.9 none's)
  f <- tempfile(fileext = ".xlsx")
  d <- numbers_study()
  on.exit(unlink(c(f, d), recursive = TRUE), add = TRUE)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "TOC")
  openxlsx::writeData(wb, "TOC", data.frame(
    `Output No.` = paste("Table", c("20.1", "20.2", "20.3", "20.9")),
    Title = c("Disposition", "Demographics", "Exposure", "Protocol Deviations"),
    check.names = FALSE))
  openxlsx::addWorksheet(wb, "t20")
  for (k in 1:4) {
    openxlsx::writeData(wb, "t20", paste("Table", c("20.1", "20.2", "20.3", "20.9.b"))[k],
                        startRow = k, colNames = FALSE)
  }
  openxlsx::writeData(wb, "t20", t(c("", "Placebo", "Drug X")), startRow = 5, colNames = FALSE)
  openxlsx::saveWorkbook(wb, f)
  x <- tfl_ai_read_shell(f)
  expect_true(is.na(x$toc$sheets[x$toc$output_id == "Table 20.9"]))
  dr <- prep_numbers(d, f, dry_run = TRUE)
  n <- dr$numbers[dr$numbers$number == "Table 20.9.b", , drop = FALSE]
  expect_identical(c(n$toc_id, n$source), c("Table 20.9", "guess"))
  expect_true(n$make)
  expect_false("no-shell" %in% dr$requests$request)
  expect_match(dr$requests$outputs[dr$requests$request == "t20"], "Table 20.9.b", fixed = TRUE)
  # the person over the guess
  dr <- prep_numbers(d, f, dry_run = TRUE,
                     numbers = data.frame(sheet = "t20", number = "Table 20.9.b", make = FALSE))
  expect_identical(dr$requests$outputs[dr$requests$request == "no-shell"], "Table 20.9")
})

test_that("numbers: a TOC item given to a sheet that does not write it is that sheet's", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("haven")
  f <- numbers_book(tempfile(fileext = ".xlsx"))
  d <- numbers_study()
  on.exit(unlink(c(f, d), recursive = TRUE), add = TRUE)
  # out of the no-shell request, and out of the sheet it was guessed into
  dr <- prep_numbers(d, f, dry_run = TRUE,
                     numbers = data.frame(sheet = "vitals", number = c("Table 14.3.1", "Table 14.4.1"),
                                          make = TRUE))
  n <- dr$numbers[dr$numbers$sheet == "vitals" & dr$numbers$source == "given", , drop = FALSE]
  expect_identical(n$toc_id, c("Table 14.3.1", "Table 14.4.1"))
  rq <- dr$requests
  expect_identical(rq$outputs[rq$request == "vitals"], "Table 14.3.1, Table 14.4.1")
  expect_identical(rq$outputs[rq$request == "t14.2.1"], "Table 14.2.1")
  expect_false("no-shell" %in% rq$request)
  expect_false(dr$numbers$make[dr$numbers$number == "Table 14.4.1.b"])
  # a number no TOC item has, not written in the sheet: allowed, warned
  expect_warning(dr <- prep_numbers(d, f, dry_run = TRUE,
                                    numbers = data.frame(sheet = "vitals", number = "Table 14.8.8",
                                                         make = TRUE)),
                 "no TOC item has Table 14.8.8 (sheet 'vitals')", fixed = TRUE)
  expect_match(dr$requests$outputs[dr$requests$request == "vitals"], "Table 14.8.8", fixed = TRUE)
})

test_that("the TOC view follows the numbers: made in no sheet, no shell; given, that sheet's", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("haven")
  f <- numbers_book(tempfile(fileext = ".xlsx"))
  d <- numbers_study()
  on.exit(unlink(c(f, d), recursive = TRUE), add = TRUE)
  row <- function(r, id) r$toc[r$toc$toc_id == id, , drop = FALSE]
  dr <- prep_numbers(d, f, dry_run = TRUE)
  expect_identical(row(dr, "Table 14.1.1")$shell, "t14.1.1")
  expect_identical(unname(dr$counts[["no_shell"]]), 1L)
  # its only sheet made it a reference: no shell, the no-shell request's
  off <- data.frame(sheet = "t14.1.1", number = "Table 14.1.1", make = FALSE)
  for (r in list(prep_numbers(d, f, dry_run = TRUE, numbers = off),
                 prep_numbers(d, f, numbers = off))) {
    t <- row(r, "Table 14.1.1")
    expect_true(is.na(t$shell))
    expect_false(t$shell_found)
    expect_identical(t$request, "no-shell")
    expect_identical(unname(r$counts[["no_shell"]]), 2L)
    expect_identical(r$requests$outputs[r$requests$request == "no-shell"],
                     "Table 14.1.1, Table 14.3.1")
  }
  idx <- readLines(file.path(d, "ai", "requests", "index.md"), encoding = "UTF-8")
  expect_true(any(grepl("Table 14.1.1, Table 14.3.1", idx, fixed = TRUE)))
  # a TOC item given to a sheet: its shell and request that sheet's (the
  # saved rows put away: given rows go over them)
  unlink(file.path(d, "ai", "sheet_numbers.csv"))
  on <- data.frame(sheet = "vitals", number = "Table 14.3.1", make = TRUE)
  for (r in list(prep_numbers(d, f, dry_run = TRUE, numbers = on),
                 prep_numbers(d, f, numbers = on))) {
    t <- row(r, "Table 14.3.1")
    expect_identical(c(t$shell, t$request), c("vitals", "vitals"))
    expect_true(t$shell_found)
    expect_identical(unname(r$counts[["no_shell"]]), 0L)
    expect_false("no-shell" %in% r$requests$request)
    # (a guess made: its sheet the shell)
    expect_identical(row(r, "Table 14.4.1")$shell, "t14.2.1")
  }
})

test_that("numbers NA over a saved row: back to the rule in the numbers, TOC, requests, counts", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("haven")
  f <- numbers_book(tempfile(fileext = ".xlsx"))
  d <- numbers_study()
  on.exit(unlink(c(f, d), recursive = TRUE), add = TRUE)
  prep_numbers(d, f, numbers = data.frame(sheet = c("t14.1.1", "t14.2.1"),
                                          number = c("Table 14.1.1", "Table 14.2.1"),
                                          make = FALSE))
  back <- data.frame(sheet = "t14.1.1", number = "Table 14.1.1", make = NA)
  for (dry in c(TRUE, FALSE)) {
    r <- prep_numbers(d, f, dry_run = dry, numbers = back)
    n <- r$numbers
    expect_identical(c(n$make[1], n$source[1]), c("TRUE", "rule"))
    # (the other saved row still in effect)
    expect_identical(n$source[n$sheet == "t14.2.1" & n$number == "Table 14.2.1"], "saved")
    t <- r$toc[r$toc$toc_id == "Table 14.1.1", , drop = FALSE]
    expect_identical(c(t$shell, t$request), c("t14.1.1", "t14.1.1"))
    expect_true(t$shell_found)
    expect_identical(unname(r$counts[["no_shell"]]), 1L)
    expect_false(grepl("Table 14.1.1", r$requests$outputs[r$requests$request == "no-shell"]))
  }
  # the file: the row taken out, the other kept
  csv <- utils::read.csv(file.path(d, "ai", "sheet_numbers.csv"))
  expect_identical(csv$sheet, "t14.2.1")
})
