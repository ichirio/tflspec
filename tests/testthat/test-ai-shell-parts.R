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

