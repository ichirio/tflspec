# A report's parts (t15.1.5.1.a, t15.1.5.1.b): the letter after a dot, a
# dash or an underscore is the number's; the parts with no sheet of their
# own read their parent's sheet (t15.1.5.1) whole, and a request says which
# part is the report's

parts_book <- function(f) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "TOC")
  openxlsx::writeData(wb, "TOC", data.frame(
    No = c("t14.1.1", "t15.1.5.1.a", "t15.1.5.1.b"),
    Title = c("Demographics", "Adverse events, Part 1", "Adverse events, Part 2")),
    startRow = 1)
  openxlsx::addWorksheet(wb, "t14.1.1")
  openxlsx::writeData(wb, "t14.1.1", "Table 14.1.1  Demographics", startRow = 1)
  # one sheet, the two parts one under the other; the second writes only
  # what differs
  s <- "t15.1.5.1"
  openxlsx::addWorksheet(wb, s)
  openxlsx::writeData(wb, s, "Table 15.1.5.1  Adverse events (Part 1)", startRow = 1)
  openxlsx::writeData(wb, s, t(c("", "Placebo", "Drug X")), startRow = 3, colNames = FALSE)
  openxlsx::writeData(wb, s, t(c("Any TEAE", "xx (xx.x)", "xx (xx.x)")), startRow = 4,
                      colNames = FALSE)
  openxlsx::writeData(wb, s, "Part 1: APHASE = \"P1\"", startRow = 6)
  openxlsx::writeData(wb, s, "Part 2: as Part 1, APHASE = \"P2\"", startRow = 9)
  openxlsx::saveWorkbook(wb, f)
  f
}

test_that("a part's letter is the number's: .a, -a, _a and a written straight on", {
  k <- .ai_id_key(c("t15.1.5.1.a", "T15.1.5.1A", "Table 15.1.5.1-b", "t15.1.5.1_a", "14.1.1",
                    "14.2.1abc"))
  expect_identical(k, c("T15.1.5.1A", "T15.1.5.1A", "T15.1.5.1B", "T15.1.5.1A", "14.1.1", "14.2.1"))
  expect_true(.ai_id_same(k[1L], k[2L]))
  expect_false(.ai_id_same(k[1L], k[3L]))
  expect_identical(.ai_id_parent(k), c("T15.1.5.1", "T15.1.5.1", "T15.1.5.1", "T15.1.5.1", NA, NA))
})

test_that("the parts read their parent's sheet whole; a request says which part", {
  f <- parts_book(tempfile(fileext = ".xlsx"))
  on.exit(unlink(f), add = TRUE)
  x <- tfl_ai_read_shell(f)
  toc <- x$toc
  expect_identical(toc$output_id, c("t14.1.1", "t15.1.5.1.a", "t15.1.5.1.b"))
  expect_identical(toc$key, c("T14.1.1", "T15.1.5.1A", "T15.1.5.1B"))
  expect_identical(toc$shell, c("t14.1.1", "t15.1.5.1", "t15.1.5.1"))
  expect_identical(toc$part_of, c(NA, "t15.1.5.1", "t15.1.5.1"))
  expect_false(any(toc$same_number))
  # part b's text: the whole sheet, Part 1's layout with it
  b <- tfl_ai_read_shell(f, outputs = "t15.1.5.1.b")
  expect_match(b$text, "Any TEAE", fixed = TRUE)
  expect_match(b$text, "Part 2: as Part 1", fixed = TRUE)
  # the request's line: which of the sheet's reports is this one
  n <- .ai_part_notes(x, "t15.1.5.1.b")
  expect_match(n[1L], "Yours is t15.1.5.1.b", fixed = TRUE)
  expect_match(n[1L], "take the layout from the first part", fixed = TRUE)
  expect_identical(.ai_part_notes(x, "t14.1.1"), character())
  # where it is: the whole sheet
  expect_match(.ai_shell_where(x, "t15.1.5.1.a"), "sheet 't15.1.5.1', the whole sheet", fixed = TRUE)
})

test_that("a number the TOC has twice is marked", {
  f <- tempfile(fileext = ".xlsx")
  on.exit(unlink(f), add = TRUE)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "TOC")
  openxlsx::writeData(wb, "TOC", data.frame(No = c("t14.1.1", "T14.1.1"), Title = c("A", "B")))
  openxlsx::addWorksheet(wb, "t14.1.1")
  openxlsx::writeData(wb, "t14.1.1", "Table 14.1.1", startRow = 1)
  openxlsx::saveWorkbook(wb, f)
  expect_warning(x <- tfl_ai_read_shell(f), "names T14.1.1 more than once")
  expect_identical(x$toc$same_number, TRUE)
})

test_that("a request's shell from what was read: the same text as reading the file again", {
  f <- parts_book(tempfile(fileext = ".xlsx"))
  on.exit(unlink(f), add = TRUE)
  x <- tfl_ai_read_shell(f)
  for (ids in list("t14.1.1", "t15.1.5.1.b", c("t14.1.1", "t15.1.5.1.a"))) {
    expect_identical(.ai_shell_for(x, ids)$text,
                     tfl_ai_read_shell(f, outputs = ids, max_chars = Inf)$text)
  }
})

# ---- a report written in parts on its sheet (one TOC row) ----------------------

sheet_parts_book <- function(f) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "TOC")
  openxlsx::writeData(wb, "TOC", data.frame(
    No = c("t.14.1.1", "t.15.1.5.1"), Title = c("Disposition", "Demographic Characteristics")))
  openxlsx::addWorksheet(wb, "t.14.1.1")
  openxlsx::writeData(wb, "t.14.1.1", "Table 14.1.1  Disposition", startRow = 1)
  s <- "t.15.1.5.1"
  openxlsx::addWorksheet(wb, s)
  # the first part whole; the second below it, what differs alone
  openxlsx::writeData(wb, s, "Table 15.1.5.1.a", startRow = 5)
  openxlsx::writeData(wb, s, "Demographic Characteristics - Phase 1 Part", startRow = 6)
  openxlsx::writeData(wb, s, "SAFFL = \"Y\" and APHASE = \"P1\"", startRow = 7)
  openxlsx::writeData(wb, s, t(c("", "Placebo", "Drug X")), startRow = 9, colNames = FALSE)
  openxlsx::writeData(wb, s, t(c("Age (years)", "xx.x", "xx.x")), startRow = 10, colNames = FALSE)
  openxlsx::writeData(wb, s, "Table 15.1.5.1.b", startRow = 85)
  openxlsx::writeData(wb, s, "Demographic Characteristics - Phase 2 Part", startRow = 86)
  openxlsx::writeData(wb, s, "APHASE = \"P2\"; columns TRT01AN = 2", startRow = 87)
  openxlsx::saveWorkbook(wb, f)
  f
}

test_that("a report in parts on its sheet: the parts and their rows; the request says them", {
  f <- sheet_parts_book(tempfile(fileext = ".xlsx"))
  on.exit(unlink(f), add = TRUE)
  x <- tfl_ai_read_shell(f)
  expect_identical(x$toc$output_id, c("t.14.1.1", "t.15.1.5.1"))
  expect_identical(x$toc$parts, c(NA, "t.15.1.5.1.a | t.15.1.5.1.b"))
  expect_identical(x$toc$parts_row, c(NA, "5 | 85"))
  l <- .ai_parts_lines(x, "t.15.1.5.1")
  expect_match(l[1L], "t.15.1.5.1 is written in 2 parts (t.15.1.5.1.a: from row 5, t.15.1.5.1.b: from row 85)",
               fixed = TRUE)
  expect_match(l[1L], "Write one report file a part", fixed = TRUE)
  # the request's shell: the whole sheet, both parts
  s <- .ai_shell_for(x, "t.15.1.5.1")$text
  expect_match(s, "Phase 1 Part", fixed = TRUE)
  expect_match(s, "Phase 2 Part", fixed = TRUE)
  # spec/toc.yml: a report a part, in the report's place
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  .ai_toc_yml(d, x)
  ids <- vapply(yaml::read_yaml(file.path(d, "spec", "toc.yml"))$outputs, `[[`, "", "id")
  expect_identical(ids, c("t.14.1.1", "t.15.1.5.1.a", "t.15.1.5.1.b"))
})

test_that("an answer in parts: what the request asked for, not a report too many", {
  pr <- .ai_check_table(list(outputs = list(`t.15.1.5.1.a` = tfl_output("t.15.1.5.1.a", "table", population = "SAF"),
                                             `t.15.1.5.1.b` = tfl_output("t.15.1.5.1.b", "table", population = "SAF"))),
                        list(outputs = "t.15.1.5.1"))
  expect_false(any(grepl("has no report|was not asked for", pr$problem)))
  # another report's parts still are not asked for
  pr2 <- .ai_check_table(list(outputs = list(`t.15.1.6.1.a` = tfl_output("t.15.1.6.1.a", "table", population = "SAF"))), list(outputs = "t.15.1.5.1"))
  expect_true(any(grepl("was not asked for", pr2$problem)))
})

test_that("one report a request: the request named after the report's number", {
  skip_if_not_installed("haven")
  f <- sheet_parts_book(tempfile(fileext = ".xlsx"))
  d <- tempfile()
  on.exit(unlink(c(f, d), recursive = TRUE), add = TRUE)
  dir.create(file.path(d, "adam"), recursive = TRUE)
  haven::write_xpt(.ai_chr(adsl), file.path(d, "adam", "adsl.xpt"))
  seen <- character()
  r <- tfl_ai_prepare(d, f, adam = "adam", quiet = TRUE, mode = "agent", size = 1,
                      templates = FALSE,
                      progress = function(step, i, n) seen <<- c(seen, paste(step, i, n)))
  expect_setequal(r$requests$request, c("t.14.1.1", "t.15.1.5.1"))
  expect_true(file.exists(file.path(d, "ai", "requests", "t.15.1.5.1.md")))
  expect_identical(seen, c("adam 0 1", "workbook 0 1", "request 0 2", "request 1 2", "request 2 2"))
})

test_that("the table instructions always say how to write a sheet's parts", {
  p <- tfl_ai_prompt("table", example = FALSE)
  expect_match(p$system, "Write one report file for each report the row makes", fixed = TRUE)
  expect_match(p$system, "the TOC's number with `.a`, `.b` ... in order", fixed = TRUE)
  fig <- tfl_ai_prompt("figure", tfl_ai_context("figure", output_id = "F-1"), example = FALSE)
  expect_no_match(fig$system, "Part 1 / Part 2", fixed = TRUE)
})

test_that("tfl_ai_prepare()'s toc, a dry run's too, says the parts as the shell's", {
  skip_if_not_installed("haven")
  f <- sheet_parts_book(tempfile(fileext = ".xlsx"))
  d <- tempfile()
  on.exit(unlink(c(f, d), recursive = TRUE), add = TRUE)
  dir.create(file.path(d, "adam"), recursive = TRUE)
  haven::write_xpt(.ai_chr(adsl), file.path(d, "adam", "adsl.xpt"))
  r <- tfl_ai_prepare(d, f, adam = "adam", quiet = TRUE, dry_run = TRUE, templates = FALSE)
  sh <- tfl_ai_read_shell(f)$toc
  expect_identical(r$toc$parts, sh$parts)
  expect_identical(r$toc$parts_row, sh$parts_row)
  expect_identical(r$toc$same_number, sh$same_number)
  expect_true(any(!is.na(r$toc$parts)))
})
