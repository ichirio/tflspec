# The TOC and the shells in workbooks apart: the TOC's workbook holds the
# TOC alone -- its other sheets (a revision history, notes that mention a
# report number) are no shells; the shells' workbook is read as before.

books_book <- function(sheets) {
  skip_if_not_installed("openxlsx")
  wb <- openxlsx::createWorkbook()
  for (nm in names(sheets)) {
    openxlsx::addWorksheet(wb, nm)
    for (cl in sheets[[nm]]) {
      rc <- openxlsx::convertFromExcelRef(cl[[1L]])
      openxlsx::writeData(wb, nm, cl[[2L]], startCol = rc[[1L]],
                          startRow = as.integer(sub("^[A-Z]+", "", cl[[1L]])), colNames = FALSE)
    }
  }
  f <- withr::local_tempfile(fileext = ".xlsx", .local_envir = parent.frame())
  openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
  f
}

books_shell <- function(id, title) {
  list(list("A1", id), list("A2", title), list("B4", "Drug X (N=xx)"), list("C4", "Control (N=xx)"),
       list("A5", "Age (years)"), list("A6", "  n"), list("B6", "xx"), list("C6", "xx"))
}

test_that("shells in a workbook of their own: the TOC workbook's other sheets are no shells", {
  toc <- list(list("A1", "Output No"), list("B1", "Title"), list("C1", "Population"),
              list("A2", "Table 14.1.1"), list("B2", "Demographic and Baseline Characteristics"), list("C2", "SAF"),
              list("A3", "Table 14.3.1"), list("B3", "Treatment-Emergent Adverse Events"), list("C3", "SAF"))
  notes <- list(list("A1", "Revision history"), list("A2", "v1: Table 14.1.1 added"),
                list("A3", "v2: Table 14.3.1 added"), list("A4", "example file T-14-1-1.yml"),
                list("A5", "notes"), list("A6", "more notes"), list("A7", "and more"))
  f_toc <- books_book(list(TOC = toc, `Revision history` = notes, Notes = notes))
  conv <- list(list("A1", "General conventions"), list("A2", "All tables: N = number of subjects"),
               list("A3", "Percentages use the column N"), list("A4", "Dates are DDMMMYYYY"),
               list("A5", "Missing is shown as blank"), list("A6", "SOC sorted by frequency"))
  f_sh <- books_book(list(`General conventions` = conv,
                          `T-DM` = books_shell("Table 14.1.1", "Demographic and Baseline Characteristics"),
                          `T-AE` = books_shell("Table 14.3.1", "Treatment-Emergent Adverse Events")))
  x <- tfl_ai_read_shell(f_toc, shells = f_sh)
  roles <- stats::setNames(x$sheets$role, x$sheets$sheet)
  expect_identical(roles[["TOC"]], "toc")
  expect_identical(unname(roles[c("Revision history", "Notes")]), c("other", "other"))
  expect_identical(unname(roles[c("T-DM", "T-AE")]), c("shell", "shell"))
  # a sheet of conventions in the shells' workbook: no shell either
  expect_identical(roles[["General conventions"]], "other")
  expect_identical(x$toc$output_id, c("Table 14.1.1", "Table 14.3.1"))
  expect_identical(x$toc$shell, c("T-DM", "T-AE"))
  # the same sheets in one workbook: the notes are shells by the rule (as before)
  f_one <- books_book(list(TOC = toc, Notes = notes,
                           `T-DM` = books_shell("Table 14.1.1", "Demographic and Baseline Characteristics")))
  y <- tfl_ai_read_shell(f_one)
  expect_identical(stats::setNames(y$sheets$role, y$sheets$sheet)[["Notes"]], "shell")
  # a person's sheet_roles still wins
  z <- tfl_ai_read_shell(f_toc, shells = f_sh, sheet_roles = c(Notes = "shell"))
  expect_identical(stats::setNames(z$sheets$role, z$sheets$sheet)[["Notes"]], "shell")
})
