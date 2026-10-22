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
