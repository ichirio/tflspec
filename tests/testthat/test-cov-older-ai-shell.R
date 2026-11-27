# R/ai_shell.R: the branches no other test reaches -- bad arguments, a
# workbook with no TOC, several workbooks, the XML read row by row, and the
# helpers' edge inputs.  Fictional workbooks only.

# A workbook: a named list of data frames (or single strings), a sheet
# each; `hidden` the sheets written hidden.
cov_shell_book <- function(f, sheets, hidden = character()) {
  wb <- openxlsx::createWorkbook()
  for (s in names(sheets)) {
    openxlsx::addWorksheet(wb, s, visible = !s %in% hidden)
    openxlsx::writeData(wb, s, sheets[[s]], startRow = 1)
  }
  openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
  f
}

# A TOC sheet and two shell sheets named by their numbers
cov_shell_toc_book <- function(f, hidden = character()) {
  cov_shell_book(f, list(
    TOC = data.frame(`Output No` = c("T-14-1-1", "T-14-2-1"),
                     Title = c("Demographics", "Adverse events"), check.names = FALSE),
    `T-14-1-1` = data.frame(x = c("Table 14.1.1 Demographics", "Age", "Sex")),
    `T-14-2-1` = data.frame(x = c("Table 14.2.1 Adverse events", "Any TEAE", "Serious"))),
    hidden = hidden)
}

# ---- tfl_ai_read_shell() and its print ------------------------------------

test_that("a workbook path that is not one existing file stops, naming the folder", {
  d <- withr_tempdir()
  expect_error(tfl_ai_read_shell(file.path(d, "missing.xlsx")), "missing.xlsx' in ")
  expect_error(tfl_ai_read_shell(c("a.xlsx", "b.xlsx")), "give the workbook's path")
  expect_error(tfl_ai_read_shell(NA_character_), "give the workbook's path")
})

test_that("print() names the reports kept and says when the text was cut", {
  d <- withr_tempdir()
  f <- cov_shell_toc_book(file.path(d, "toc.xlsx"))
  x <- tfl_ai_read_shell(f, outputs = "T-14-1-1", max_chars = 60)
  expect_true(x$truncated)
  out <- capture.output(print(x))
  expect_true(any(grepl("kept: T-14-1-1", out, fixed = TRUE)))
  expect_true(any(grepl("characters (cut); format(x) gives it", out, fixed = TRUE)))
})

test_that("the hint of a read with no shell table names the sheets to add", {
  d <- withr_tempdir()
  f <- cov_shell_book(file.path(d, "toc.xlsx"), list(
    TOC = data.frame(`Output No` = c("T-14-1-1", "T-14-2-1"),
                     Title = c("Demographics", "Adverse events"), check.names = FALSE),
    `T-14-1-1` = data.frame(x = c("Table 14.1.1 Demographics", "Age", "Sex")),
    Extra = data.frame(x = c("Adverse events", "Any TEAE", "Serious", "Fatal", "Other"))))
  x <- tfl_ai_read_shell(f, sheet_roles = c(Extra = "other"))
  # (a read made before the shell sheets were kept: no `shells`)
  x$shells <- NULL
  h <- .ai_shell_hint(x)
  expect_match(h, "no shell found for 1 report(s) (T-14-2-1)", fixed = TRUE)
  expect_match(h, "sheets = c(\"Extra\")", fixed = TRUE)
  # (every other sheet some report's shell already: those sheets are named)
  g <- cov_shell_book(file.path(d, "toc2.xlsx"), list(
    TOC = data.frame(`Output No` = c("T-14-1-1", "T-14-2-1"),
                     Title = c("Demographics", "Adverse events"), check.names = FALSE),
    `T-14-1-1` = data.frame(x = c("Table 14.1.1 Demographics", "Age", "Sex"))))
  y <- tfl_ai_read_shell(g)
  y$shells <- NULL
  expect_match(.ai_shell_hint(y), "sheets = c(\"T-14-1-1\")", fixed = TRUE)
})

test_that("a shells workbook that does not exist stops, naming it", {
  d <- withr_tempdir()
  f <- cov_shell_toc_book(file.path(d, "toc.xlsx"))
  expect_error(tfl_ai_read_shell(f, shells = file.path(d, "missing.xlsx")),
               "`shells`: no file")
})

test_that("print() names every workbook when the shells come in a workbook of their own", {
  d <- withr_tempdir()
  f <- cov_shell_book(file.path(d, "toc.xlsx"), list(
    TOC = data.frame(`Output No` = c("T-14-1-1", "T-14-2-1"),
                     Title = c("Demographics", "Adverse events"), check.names = FALSE)))
  g <- cov_shell_book(file.path(d, "shells.xlsx"), list(
    `T-14-1-1` = data.frame(x = c("Table 14.1.1 Demographics", "Age", "Sex"))))
  x <- tfl_ai_read_shell(f, shells = g)
  out <- capture.output(print(x))
  expect_true(any(grepl("2 workbooks: toc.xlsx (the TOC), shells.xlsx", out, fixed = TRUE)))
})

test_that("print() says how to name the TOC sheet when no TOC is found", {
  d <- withr_tempdir()
  f <- cov_shell_book(file.path(d, "plain.xlsx"),
                      list(Data = data.frame(x = c("alpha", "beta", "gamma"))))
  x <- tfl_ai_read_shell(f)
  expect_identical(nrow(x$toc), 0L)
  out <- capture.output(print(x))
  expect_true(any(grepl("no TOC found: name the TOC sheet", out, fixed = TRUE)))
  expect_true(any(grepl("toc_sheet = \"Data\"", out, fixed = TRUE)))
})

test_that("a hidden shell sheet is written as hidden in the text", {
  d <- withr_tempdir()
  f <- cov_shell_toc_book(file.path(d, "toc.xlsx"), hidden = "T-14-2-1")
  x <- tfl_ai_read_shell(f, sheet_roles = c(`T-14-2-1` = "shell"))
  expect_true(x$sheets$hidden[x$sheets$sheet == "T-14-2-1"])
  expect_match(x$text, "## Sheet \"T-14-2-1\" (hidden)", fixed = TRUE)
})

# ---- what is kept: outputs and sheets -------------------------------------

test_that("sheets given by position are the sheets at those places (past the last: none)", {
  d <- withr_tempdir()
  f <- cov_shell_toc_book(file.path(d, "toc.xlsx"))
  x <- tfl_ai_read_shell(f, sheets = c(2, 9))
  expect_match(x$text, "## Sheet \"T-14-1-1\"", fixed = TRUE)
  expect_no_match(x$text, "## Sheet \"T-14-2-1\"", fixed = TRUE)
  expect_no_match(x$text, "## Sheet \"TOC\"", fixed = TRUE)
})

test_that("outputs that are no report number stop, naming them", {
  d <- withr_tempdir()
  f <- cov_shell_toc_book(file.path(d, "toc.xlsx"))
  expect_error(tfl_ai_read_shell(f, outputs = "Demographics"), "is not a report number")
})

test_that("outputs asked of a workbook with no TOC stop: the TOC has none", {
  d <- withr_tempdir()
  f <- cov_shell_book(file.path(d, "plain.xlsx"),
                      list(Data = data.frame(x = c("alpha", "beta", "gamma"))))
  expect_error(tfl_ai_read_shell(f, outputs = "T-14-1-1"), "none \\(give `toc_sheet`\\)")
})

test_that("outputs with sheets keep the TOC's header and report rows, the shell and the sheets", {
  d <- withr_tempdir()
  f <- cov_shell_book(file.path(d, "toc.xlsx"), list(
    TOC = data.frame(`Output No` = c("T-14-1-1", "T-14-2-1"),
                     Title = c("Demographics", "Adverse events"), check.names = FALSE),
    `T-14-1-1` = data.frame(x = c("Table 14.1.1 Demographics", "Age", "Sex")),
    `T-14-2-1` = data.frame(x = c("Table 14.2.1 Adverse events", "Any TEAE", "Serious")),
    Notes = data.frame(x = c("Decimal places as the data", "Footnotes as written"))))
  x <- tfl_ai_read_shell(f, outputs = "T-14-1-1", sheets = "Notes")
  expect_identical(x$outputs, "T-14-1-1")
  expect_match(x$text, "Output No", fixed = TRUE)
  expect_match(x$text, "Age", fixed = TRUE)
  expect_match(x$text, "Decimal places as the data", fixed = TRUE)
  expect_no_match(x$text, "Any TEAE", fixed = TRUE)
})

test_that("a shells workbook none of whose cells is kept writes no part of its own", {
  d <- withr_tempdir()
  f <- cov_shell_book(file.path(d, "toc.xlsx"), list(
    TOC = data.frame(`Output No` = c("T-14-1-1", "T-14-2-1"),
                     Title = c("Demographics", "Adverse events"), check.names = FALSE)))
  g <- cov_shell_book(file.path(d, "shells.xlsx"), list(
    `T-14-1-1` = data.frame(x = c("Table 14.1.1 Demographics", "Age", "Sex"))))
  x <- tfl_ai_read_shell(f, shells = g, sheets = "TOC")
  expect_match(x$text, "# Workbook toc.xlsx", fixed = TRUE)
  expect_no_match(x$text, "# Workbook shells.xlsx", fixed = TRUE)
})

# ---- the workbook's XML ---------------------------------------------------

test_that(".xl_member() of a member the zip does not hold is an empty string", {
  d <- withr_tempdir()
  f <- cov_shell_toc_book(file.path(d, "toc.xlsx"))
  members <- utils::unzip(f, list = TRUE)$Name
  expect_identical(.xl_member(f, "xl/no-such-part.xml", members), "")
})

test_that(".xl_unescape() unescapes only the values that hold an entity", {
  expect_identical(.xl_unescape(c("A &amp; B", "plain", NA)), c("A & B", "plain", NA))
})

test_that(".xl_unescape() drops a numeric entity past the integers R holds", {
  expect_identical(.xl_unescape("a&#99999999999;b"), "ab")
  expect_identical(.xl_unescape("&#65;&#x42;_x000D_"), "AB")
})

test_that(".xl_col_widths(): a hidden column 0, a column with no width the sheet's default", {
  x <- paste0('<worksheet><sheetFormatPr defaultColWidth="10"/><cols>',
              '<col min="1" max="1" width="20"/><col min="2" max="2" hidden="1" width="5"/>',
              '<col min="3" max="3"/></cols></worksheet>')
  expect_identical(.xl_col_widths(x, 4L), c(20, 0, 10, 10))
})

test_that(".xl_indents() of a styles part with no cell styles is no indent", {
  expect_identical(.xl_indents(""), integer())
  expect_identical(.xl_indents("<styleSheet></styleSheet>"), integer())
})

test_that(".xl_sheet_cells() of an empty sheet part is no cell", {
  expect_identical(nrow(.xl_sheet_cells("", "S", character(), integer())), 0L)
})

test_that(".xl_sheet_cells() of a sheet whose rows hold no cell is no cell", {
  x <- '<worksheet><sheetData><row r="1"/><row r="2"></row></sheetData></worksheet>'
  expect_identical(nrow(.xl_sheet_cells(x, "S", character(), integer())), 0L)
})

test_that("cells that each say their place: inline strings, numbers, a merge without a range", {
  x <- paste0('<worksheet><sheetData><row r="1">',
              '<c r="A1" t="inlineStr"><is><t>Hello &amp; welcome</t></is></c>',
              '<c r="B1"><v>1.50</v></c><c r="C1" t="b"><v>1</v></c>',
              '<c r="D1" t="e"><v>#N/A</v></c><c r="E1"><v>abc</v></c>',
              '</row></sheetData><mergeCells><mergeCell ref="A1"/></mergeCells></worksheet>')
  d <- .xl_sheet_cells(x, "S", character(), integer())
  expect_identical(d$text, c("Hello & welcome", "1.5", "TRUE", "#N/A", "abc"))
  expect_identical(d$ref, c("A1", "B1", "C1", "D1", "E1"))
})

test_that("cells that do not say their place follow the one before, row by row", {
  x <- paste0("<worksheet><sheetData>",
              '<row r="2"><c t="s" s="1"><v>0</v></c><c><v>3</v></c><c t="s"><v>7</v></c></row>',
              '<row><c r="C3" t="b"><v>0</v></c><c t="b"/><c t="str"><v>f(x)</v></c>',
              '<c t="inlineStr"><is><t>inline</t></is></c><c><v>n/a</v></c></row>',
              '<row r="5"><c t="b"><v>1</v></c></row>',
              '</sheetData><mergeCells><mergeCell ref="A2:B2"/><mergeCell ref="C5"/>',
              "</mergeCells></worksheet>")
  d <- .xl_sheet_cells(x, "S", shared = "Placebo", indent = c(0L, 2L))
  expect_identical(d$row, c(2L, 2L, 3L, 3L, 3L, 3L, 5L))
  expect_identical(d$ref, c("A2:B2", "B2", "C3", "E3", "F3", "G3", "A5"))
  expect_identical(d$text, c("    Placebo", "3", "FALSE", "f(x)", "inline", "n/a", "TRUE"))
})

# ---- report numbers -------------------------------------------------------

test_that(".ai_half_width() keeps an NA among full-width numbers", {
  expect_identical(.ai_half_width(c("\uff14.1.1", NA)), c("4.1.1", NA))
})

test_that(".ai_ids_in() empties its memo once it holds more than 50,000 texts", {
  memo <- .ai_ids_memo
  fill <- paste0("cov-older-filler-", seq_len(50001L))
  list2env(stats::setNames(rep(list(.ai_ids_none), length(fill)), fill), envir = memo)
  on.exit(rm(list = intersect(fill, ls(memo)), envir = memo), add = TRUE)
  h <- .ai_ids_in("Table 14.9.9 of the memo test")
  expect_identical(h[[1L]]$key, "T14.9.9")
  expect_lt(length(ls(memo)), 50000L)
})

# ---- the TOC --------------------------------------------------------------

cov_cells <- function(sheet, row, col, text, ref = NULL) {
  data.frame(sheet = sheet, row = as.integer(row), col = as.integer(col),
             ref = ref %||% paste0(.xl_col_letter(col), row), text = text,
             stringsAsFactors = FALSE)
}

test_that(".ai_shell_toc_sheet(): NA is no TOC, a sheet given is the TOC, an unknown one stops", {
  info <- data.frame(sheet = c("Contents", "Data"), stringsAsFactors = FALSE)
  cells <- cov_cells("Data", 1:2, 1L, c("a", "b"))
  expect_identical(.ai_shell_toc_sheet(info, cells, NA, "f()"), character())
  expect_identical(.ai_shell_toc_sheet(info, cells, "Data", "f()"), "Data")
  expect_error(.ai_shell_toc_sheet(info, cells, "Nope", "f()"), "no sheet 'Nope'")
})

test_that(".ai_shell_toc_sheet(): a sheet named like a TOC that lists numbers is the TOC", {
  info <- data.frame(sheet = c("Data", "Contents"), stringsAsFactors = FALSE)
  cells <- rbind(cov_cells("Data", 1:3, 1L, c("T-14-1-1", "T-14-2-1", "T-14-3-1")),
                 cov_cells("Contents", 1L, 1L, "T-14-1-1"))
  expect_identical(.ai_shell_toc_sheet(info, cells, NULL, "f()"), "Contents")
})

test_that("with no TOC sheet, each sheet named by a number is a TOC item", {
  cells <- rbind(cov_cells("T-14-1-1", 1:2, 1L, c("Table 14.1.1", "Summary of Demographics")),
                 cov_cells("Notes", 1L, 1L, "Free text"))
  toc <- .ai_shell_toc(cells, character())
  expect_identical(toc$output_id, "T-14-1-1")
  expect_identical(toc$title, "Summary of Demographics")
  expect_identical(toc$sheet, "T-14-1-1")
})

test_that("a TOC sheet that names no report number has no item", {
  cells <- cov_cells("TOC", 1:2, 1L, c("Title", "Nothing numbered here"))
  expect_null(.ai_toc_roles_of(cells))
  expect_identical(nrow(.ai_shell_toc(cells, "TOC")), 0L)
})

test_that("a TOC row whose number cell is long text, and one with only a shell's number", {
  cells <- rbind(
    cov_cells("TOC", 1L, 1:3, c("Output No", "Shell", "Title")),
    cov_cells("TOC", 2L, 1:3, c("T-14-1-1", "S-1-1", "Demographics")),
    cov_cells("TOC", 3L, 1:3, c("see the long note about T-14-1-2 written here", "S-1-2",
                                "Disposition")),
    cov_cells("TOC", 4L, 2:3, c("S-1-3", "Exposure")),
    cov_cells("TOC", 5L, 1:3, c("T-14-1-4", "S-1-4", "Vital signs")))
  toc <- .ai_shell_toc(cells, "TOC")
  # (row 3: the number cell is no number -- the shell's is the item's)
  expect_identical(toc$output_id, c("T-14-1-1", "S-1-2", "S-1-3", "T-14-1-4"))
  expect_identical(toc$shell_id[3L], "S-1-3")
})

test_that("a TOC naming a number twice keeps its first row and warns", {
  cells <- rbind(cov_cells("TOC", 1L, 1:2, c("Output No", "Title")),
                 cov_cells("TOC", 2L, 1:2, c("T-14-1-1", "Demographics")),
                 cov_cells("TOC", 3L, 1:2, c("T-14-1-1", "Demographics again")),
                 cov_cells("TOC", 4L, 1:2, c("T-14-2-1", "Adverse events")))
  expect_warning(toc <- .ai_shell_toc(cells, "TOC"), "names T-14-1-1 more than once")
  expect_identical(toc$title, c("Demographics", "Adverse events"))
  expect_identical(attr(toc, "same_number"), c(TRUE, FALSE))
})

test_that("a header merged down rows (not across columns) adds no column", {
  d <- rbind(cov_cells("TOC", 1L, 1:2, c("Output No", "Title"), ref = c("A1:A2", "B1")),
             cov_cells("TOC", 3:4, 1L, c("T-14-1-1", "T-14-2-1")),
             cov_cells("TOC", 3:4, 2L, c("Demographics", "Adverse events")))
  r <- .ai_toc_roles_of(d)
  expect_identical(r$output_id, 1L)
  expect_identical(r$title, 2L)
})

test_that("toc_cols given as a list names the columns", {
  d <- rbind(cov_cells("TOC", 1L, 1:3, c("Number", "Heading text", "Pop")),
             cov_cells("TOC", 2:3, 1L, c("T-14-1-1", "T-14-2-1")),
             cov_cells("TOC", 2:3, 2L, c("Demographics", "Adverse events")),
             cov_cells("TOC", 2:3, 3L, c("SAF", "SAF")))
  r <- .ai_toc_roles_of(d, toc_cols = list(output_id = "A", title = "Heading text",
                                           population = "C"))
  expect_identical(c(r$output_id, r$title, r$population), c(1L, 2L, 3L))
})

test_that("with no shell header, of two number columns the one naming sheets is the shell's", {
  d <- rbind(cov_cells("TOC", 1L, 1:3, c("Output No", "Ref", "Title")),
             cov_cells("TOC", 2:3, 1L, c("T-14-1-1", "T-14-1-2")),
             cov_cells("TOC", 2:3, 2L, c("S-1-1", "S-1-1")),
             cov_cells("TOC", 2:3, 3L, c("Demographics", "Demographics by age")))
  r <- .ai_toc_roles_of(d, sheet_keys = "1.1")
  expect_identical(r$output_id, 1L)
  expect_identical(r$shell, 2L)
})

test_that("the only number column given as the shell's is the report's too", {
  d <- rbind(cov_cells("TOC", 1L, 1:2, c("Shell No", "Title")),
             cov_cells("TOC", 2:3, 1L, c("T-14-1-1", "T-14-2-1")),
             cov_cells("TOC", 2:3, 2L, c("Demographics", "Adverse events")))
  r <- .ai_toc_roles_of(d, toc_cols = c(shell = "A"))
  expect_identical(r$output_id, 1L)
  expect_identical(r$shell, 1L)
})

test_that("a header saying Name (no role word) gives the title column", {
  d <- rbind(cov_cells("TOC", 1L, 1:2, c("Ref", "Name")),
             cov_cells("TOC", 2:3, 1L, c("T-14-1-1", "T-14-2-1")),
             cov_cells("TOC", 2:3, 2L, c("Demographics", "Adverse events")))
  r <- .ai_toc_roles_of(d)
  expect_identical(r$header_row, 0L)
  expect_identical(r$title, 2L)
  expect_identical(.ai_toc_title_col(d, 2L), 2L)
  expect_identical(.ai_toc_title_col(d, 1L), NA_integer_)
})

test_that("a TOC with no header at all has no title column", {
  d <- cov_cells("TOC", 1:3, 1L, c("T-14-1-1", "T-14-2-1", "T-14-3-1"))
  r <- .ai_toc_roles_of(d)
  expect_identical(r$header_row, 0L)
  expect_identical(r$title, integer())
})

test_that(".ai_type_word() reads table, figure, listing, or none", {
  expect_identical(.ai_type_word("Summary table of AEs"), "table")
  expect_identical(.ai_type_word("Forest plot"), "figure")
  expect_identical(.ai_type_word("Listing of deaths"), "listing")
  expect_identical(.ai_type_word("\u4e00\u89a7"), "listing")
  expect_identical(.ai_type_word("Summary"), NA_character_)
})

# ---- a report's rows of a sheet -------------------------------------------

test_that(".ai_shell_block(): a sheet naming other reports but not this one gives no rows", {
  d <- cov_cells("S", 1:3, 1L, c("Table 14.2.1 AEs", "Any TEAE", "Table 14.3.1 Deaths"))
  b <- .ai_shell_block(d, "T14.1.1", c("T14.1.1", "T14.2.1", "T14.3.1"))
  expect_identical(nrow(b), 0L)
})

test_that(".ai_shell_widths_line() with no widths or no cells writes no line", {
  d <- cov_cells("S", 1L, 1:2, c("a", "b"))
  expect_null(.ai_shell_widths_line(NULL, d))
  expect_null(.ai_shell_widths_line(c(10, 20), d[0L, ]))
})

# ---- batches --------------------------------------------------------------

test_that("tfl_ai_batches() stops on a size that is not one number of 1 or more", {
  toc <- data.frame(output_id = "T-14-1-1", title = "Demographics")
  expect_error(tfl_ai_batches(toc, size = 0), "`size` is one number")
  expect_error(tfl_ai_batches(toc, size = c(2, 3)), "`size` is one number")
  expect_error(tfl_ai_batches(toc, size = NA_real_), "`size` is one number")
})

test_that("tfl_ai_batches() stops on families that are not a named character vector", {
  toc <- data.frame(output_id = "T-14-1-1", title = "Demographics")
  expect_error(tfl_ai_batches(toc, families = "(?i)quality"), "`families` is a named")
  expect_error(tfl_ai_batches(toc, families = list(qol = 1)), "`families` is a named")
})

# ---- a request's lines ----------------------------------------------------

test_that(".ai_shell_where(): a report not in the TOC, and one whose shell was not found", {
  d <- withr_tempdir()
  f <- cov_shell_book(file.path(d, "toc.xlsx"), list(
    TOC = data.frame(`Output No` = c("T-14-1-1", "T-14-2-1"),
                     Title = c("Demographics", "Adverse events"), check.names = FALSE),
    `T-14-1-1` = data.frame(x = c("Table 14.1.1 Demographics", "Age", "Sex"))))
  x <- tfl_ai_read_shell(f)
  w <- .ai_shell_where(x, c("T-14-9-9", "T-14-2-1", "T-14-1-1"))
  expect_match(w[1L], "T-14-9-9: not in the TOC", fixed = TRUE)
  expect_match(w[2L], "no shell was found by its number; look for it in the workbook (its TOC row",
               fixed = TRUE)
  expect_match(w[3L], "sheet 'T-14-1-1', the whole sheet", fixed = TRUE)
})

test_that(".ai_toc_row_lines(): none when the TOC has no shell numbers or no such report", {
  expect_identical(.ai_toc_row_lines(list(toc = data.frame(output_id = "T-1-1")), "T-1-1"),
                   character())
  d <- withr_tempdir()
  f <- cov_shell_toc_book(file.path(d, "toc.xlsx"))
  x <- tfl_ai_read_shell(f)
  expect_identical(.ai_toc_row_lines(x, "T-14-9-9"), character())
})

test_that(".ai_toc_row_lines(): a report with no shell, its file, and reports sharing one", {
  toc <- data.frame(
    output_id = c("T-14-1-1", "T-14-1-2", "T-14-2-1"), key = c("T14.1.1", "T14.1.2", "T14.2.1"),
    shell_id = c("S-1", "S-1", "S-2"), shell_key = c("1", "1", "2"),
    shell = c("S-1", "S-1", NA), title = c("Demographics", "Demographics by age", NA),
    population = NA_character_, program = c("t_dm.sas", NA, NA),
    file = c("t_dm.rtf", NA, NA), info = NA_character_, stringsAsFactors = FALSE)
  l <- .ai_toc_row_lines(list(toc = toc), toc$output_id)
  expect_identical(l[1L], "The reports of this request, as their TOC rows say them:")
  expect_match(l[2L], "shell S-1 (sheet 'S-1'); title: Demographics; program: t_dm.sas; file: t_dm.rtf",
               fixed = TRUE)
  expect_match(l[4L], "no shell (the TOC names S-2; no sheet of it was found)", fixed = TRUE)
  expect_true(any(grepl("Reports sharing a shell are each a report file", l, fixed = TRUE)))
  expect_true(any(grepl("A report with no shell: write it from its TOC row", l, fixed = TRUE)))
})
