# tfl_ai_read_shell() on workbooks laid out as a company would: synthetic
# fixtures only (no company's data), made with openxlsx.

# Japanese words, as escapes (the file stays ASCII)
jp <- list(
  list   = "帳票リスト",              # TOC sheet name
  no     = "帳票番号",                    # report number
  title  = "タイトル",                    # title
  pop    = "解析対象集団",        # analysis set
  saf    = "安全性解析対象集団",
  dm     = "人口統計学的特性",
  ae     = "有害事象",                    # short: shorter than `saf`
  eff    = "奇効率",
  study  = "XYZ 試験 帳票一覧"
)

# A workbook from a list of sheets, each a list of cells list(ref, text),
# with merged ranges; returns the path.
company_book <- function(sheets, merges = list()) {
  skip_if_not_installed("openxlsx")
  wb <- openxlsx::createWorkbook()
  for (nm in names(sheets)) {
    openxlsx::addWorksheet(wb, nm)
    for (cl in sheets[[nm]]) {
      rc <- openxlsx::convertFromExcelRef(cl[[1L]])
      openxlsx::writeData(wb, nm, cl[[2L]], startCol = rc[[1L]],
                          startRow = as.integer(sub("^[A-Z]+", "", cl[[1L]])), colNames = FALSE)
    }
    for (m in merges[[nm]]) openxlsx::mergeCells(wb, nm, cols = m$cols, rows = m$rows)
  }
  f <- withr::local_tempfile(fileext = ".xlsx", .local_envir = parent.frame())
  openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
  f
}

cells <- function(...) list(...)
cl <- function(ref, text) list(ref, text)

# A shell: its number and title, a header, a few rows.
shell_cells <- function(row, id, title, extra = character()) {
  c(list(cl(paste0("A", row), id), cl(paste0("A", row + 1L), title),
         cl(paste0("B", row + 3L), "Drug X (N=xx)"), cl(paste0("C", row + 3L), "Control (N=xx)"),
         cl(paste0("A", row + 4L), "Age (years)"), cl(paste0("A", row + 5L), "  n"),
         cl(paste0("B", row + 5L), "xx"), cl(paste0("C", row + 5L), "xx")),
    lapply(seq_along(extra), function(i) cl(paste0("A", row + 5L + i), extra[i])))
}

test_that("a Japanese TOC: its own sheet name, headers off row 1, merged title", {
  toc <- c(list(cl("B1", jp$study), cl("B4", jp$no), cl("C4", jp$title), cl("D4", jp$pop),
                cl("B5", "Table 14.1.1"), cl("C5", jp$dm), cl("D5", jp$saf),
                cl("B6", "Table 14.2.1"), cl("C6", jp$eff), cl("D6", "FAS"),
                cl("B8", "Table 14.3.1"), cl("C8", jp$ae), cl("D8", jp$saf)))
  f <- company_book(list(
    toc,
    shell_cells(1L, "T-14-1-1", jp$dm),
    shell_cells(2L, "Table 14.2.1", jp$eff),
    shell_cells(1L, "14.3.1", jp$ae)) |>
      stats::setNames(c(jp$list, "T-14-1-1", "T-14-2-1", "14.3.1")),
    merges = stats::setNames(list(list(list(cols = 2:4, rows = 1:2))), jp$list))
  x <- tfl_ai_read_shell(f)
  expect_identical(x$sheets$role[x$sheets$sheet == jp$list], "toc")
  expect_identical(x$toc$output_id, c("Table 14.1.1", "Table 14.2.1", "Table 14.3.1"))
  # the title is the title column's, not the longer analysis set
  expect_identical(x$toc$title, c(jp$dm, jp$eff, jp$ae))
  expect_identical(x$toc$shell, c("T-14-1-1", "T-14-2-1", "14.3.1"))
  # the Japanese text kept, in the text and its file
  expect_true(grepl(jp$saf, format(x), fixed = TRUE))
  expect_true(grepl("B1:D2=", format(x), fixed = TRUE))
  out <- withr::local_tempfile(fileext = ".md")
  tfl_ai_read_shell(f, file = out)
  b <- readBin(out, "raw", file.size(out))
  txt <- rawToChar(b)
  Encoding(txt) <- "UTF-8"
  expect_true(validUTF8(txt))
  expect_true(grepl(jp$dm, txt, fixed = TRUE))
  # one report's parts
  y <- tfl_ai_read_shell(f, outputs = "T-14-3-1")
  expect_true(grepl(jp$ae, format(y), fixed = TRUE))
  expect_false(grepl(jp$eff, sub("^.*## Sheet \"14.3.1\"", "", format(y)), fixed = TRUE))
})

test_that("several shells on one sheet, the TOC on a sheet not named like one", {
  toc <- list(cl("A1", "Output"), cl("B1", "Title"),
              cl("A2", "T14.1.1"), cl("B2", "Demographics"),
              cl("A3", "T14.2.1"), cl("B3", "Best overall response"),
              cl("A4", "T14.3.1"), cl("B4", "Adverse events by SOC and PT"))
  sh <- c(shell_cells(2L, "Table 14.1.1", "Demographics"),
          shell_cells(12L, "Table 14.2.1", "Best overall response"),
          shell_cells(25L, "Table 14.3.1", "Adverse events", extra = "Source: ADAE"))
  f <- company_book(list(Sheet1 = toc, `All shells` = sh))
  x <- tfl_ai_read_shell(f)
  expect_identical(x$toc$sheet[1L], "Sheet1")
  expect_identical(x$toc$shell, rep("All shells", 3L))
  y <- format(tfl_ai_read_shell(f, outputs = "14.2.1"))
  expect_true(grepl("Best overall response", y, fixed = TRUE))
  # the other shells of the sheet are not in its part
  expect_false(grepl("Source: ADAE", y, fixed = TRUE))
  expect_false(grepl("12: A12=Table 14.1.1", y, fixed = TRUE))
})

test_that("ids written three ways match; full-width digits too", {
  toc <- list(cl("A2", "14.1.1"), cl("B2", "Demographics"),
              cl("A3", "T-14-2-1"), cl("B3", "Response"),
              cl("A4", "４４.3.1"), cl("B4", "Adverse events"))
  f <- company_book(list(TOC = toc,
                         `Table 14.1.1` = shell_cells(1L, "Table 14.1.1", "Demographics"),
                         `T14.2.1` = shell_cells(1L, "T14.2.1", "Response"),
                         `T-44-3-1` = shell_cells(1L, "T-44-3-1", "Adverse events")))
  x <- tfl_ai_read_shell(f)
  expect_identical(x$toc$output_id, c("14.1.1", "T-14-2-1", "44.3.1"))
  expect_identical(x$toc$shell, c("Table 14.1.1", "T14.2.1", "T-44-3-1"))
})

test_that("empty rows and columns, a very wide sheet", {
  wide <- c(shell_cells(5L, "Table 14.1.1", "Demographics"),
            lapply(1:120, function(j) cl(paste0(openxlsx::int2col(j + 3L), "40"), paste("col", j))))
  toc <- list(cl("D10", "Table 14.1.1"), cl("F10", "Demographics"),
              cl("D30", "Table 14.1.2"), cl("F30", "Vital signs"),
              cl("D50", "Table 14.1.3"), cl("F50", "Labs"))
  f <- company_book(list(Contents = toc, Wide = wide))
  x <- tfl_ai_read_shell(f)
  expect_identical(nrow(x$toc), 3L)
  expect_identical(x$toc$shell, c("Wide", NA, NA))
  expect_true(grepl("DS=col 120", format(x), fixed = TRUE))
  # the reports with no shell: the print says what to pass
  expect_output(print(x), "sheets = c(", fixed = TRUE)
})

test_that("no TOC found: the print and the error say to pass toc_sheet", {
  # a TOC of two reports on a sheet not named like one: too few to guess
  toc <- list(cl("A1", "T-14-1-1"), cl("B1", "Demographics"),
              cl("A2", "T-14-2-1"), cl("B2", "Response"))
  f <- company_book(list(Plan = toc, DM = list(cl("A1", "Age (years)"))))
  x <- tfl_ai_read_shell(f)
  expect_identical(nrow(x$toc), 0L)
  expect_output(print(x), "toc_sheet = \"Plan\"", fixed = TRUE)
  expect_error(tfl_ai_write_requests(withr::local_tempdir(), x, NULL), "toc_sheet")
  # named, it is found; a request's own read passes it again
  y <- tfl_ai_read_shell(f, toc_sheet = "Plan")
  expect_identical(y$toc$output_id, c("T-14-1-1", "T-14-2-1"))
  ctx <- tfl_ai_context("table", outputs = "T-14-2-1", shell = y)
  expect_identical(ctx$titles$title, "Response")
  expect_true(grepl("T-14-2-1", ctx$shell, fixed = TRUE))
})

test_that("a sheet named by a report number and a TOC word is a shell, not the TOC", {
  toc <- list(cl("A1", "L-16-2-1"), cl("B1", "Subject listing"),
              cl("A2", "T-14-1-1"), cl("B2", "Demographics"),
              cl("A3", "T-14-2-1"), cl("B3", "Response"))
  f <- company_book(list(
    `L-16-2-1 一覧` = list(cl("A1", "Listing 16.2.1"), cl("A3", "Subject")),
    Index = toc))
  x <- tfl_ai_read_shell(f)
  expect_identical(x$toc$sheet[1L], "Index")
  expect_identical(x$toc$shell[1L], "L-16-2-1 一覧")
})
