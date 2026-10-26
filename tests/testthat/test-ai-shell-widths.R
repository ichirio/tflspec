# A shell sheet's column widths, as Excel keeps them, in the text the AI
# reads: what its table's plan_columns(widths) is written from

shw_book <- function(f, widths = c(36, 14, 14, 14)) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "TOC")
  openxlsx::writeData(wb, "TOC", data.frame(`Output No.` = "Table 14.1.1", Title = "Demographics",
                                            check.names = FALSE), startRow = 2)
  openxlsx::addWorksheet(wb, "t.14.1.1")
  lines <- list("Table 14.1.1  Demographics", "Safety Population",
                t(c("", "Placebo", "Drug X", "Total")), t(c("Age (years)", "", "", "")),
                t(c("  Mean (SD)", "xx.x (xx.xx)", "xx.x (xx.xx)", "xx.x (xx.xx)")))
  for (j in seq_along(lines)) openxlsx::writeData(wb, "t.14.1.1", lines[[j]], startRow = j, colNames = FALSE)
  if (!is.null(widths)) openxlsx::setColWidths(wb, "t.14.1.1", cols = 1:4, widths = widths)
  openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
  f
}

test_that("a shell sheet's column widths in its text, as parts of the narrowest; none when all alike", {
  skip_if_not_installed("openxlsx")
  f <- shw_book(tempfile(fileext = ".xlsx"))
  on.exit(unlink(f), add = TRUE)
  x <- tfl_ai_read_shell(f)
  ln <- grep("^Column widths", strsplit(x$text, "\n", fixed = TRUE)[[1L]], value = TRUE)
  expect_length(ln, 1L)
  expect_match(ln, "A=36.7 B=14.7 C=14.7 D=14.7", fixed = TRUE)
  expect_match(ln, "as parts: 2.5 : 1 : 1 : 1", fixed = TRUE)
  # a request's shell (from what was read) says the same
  expect_identical(tflspec:::.ai_shell_for(x, "Table 14.1.1")$text,
                   tfl_ai_read_shell(f, outputs = "Table 14.1.1", max_chars = Inf)$text)
  # Excel's defaults: nothing to copy
  g <- shw_book(tempfile(fileext = ".xlsx"), widths = NULL)
  on.exit(unlink(g), add = TRUE)
  lines <- strsplit(tfl_ai_read_shell(g)$text, "\n", fixed = TRUE)[[1L]]
  expect_false(any(grepl("^Column widths", lines)))
})

test_that("a shell whose columns are not plainly its table's: no widths (merged header, spacer column, spilled text)", {
  skip_if_not_installed("openxlsx")
  one <- function(edit) {
    f <- tempfile(fileext = ".xlsx")
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "TOC")
    openxlsx::writeData(wb, "TOC", data.frame(`Output No.` = "Table 14.1.1", Title = "Demographics",
                                              check.names = FALSE), startRow = 2)
    openxlsx::addWorksheet(wb, "t.14.1.1")
    openxlsx::writeData(wb, "t.14.1.1", "Table 14.1.1  Demographics", startRow = 1, colNames = FALSE)
    edit(wb)
    openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
    lines <- strsplit(tfl_ai_read_shell(f)$text, "\n", fixed = TRUE)[[1L]]
    unlink(f)
    grep("^Column widths", lines, value = TRUE)
  }
  row <- function(wb, r, x) openxlsx::writeData(wb, "t.14.1.1", t(x), startRow = r, colNames = FALSE)
  # plain: written
  expect_length(one(function(wb) {
    row(wb, 3, c("", "Placebo", "Drug X")); row(wb, 4, c("Mean (SD)", "xx.x", "xx.x"))
    openxlsx::setColWidths(wb, "t.14.1.1", 1:3, c(30, 12, 12))
  }), 1L)
  # a header merged across columns (an arm over its sub-columns)
  expect_length(one(function(wb) {
    row(wb, 3, c("", "Drug X", "")); row(wb, 4, c("", "Low", "High"))
    row(wb, 5, c("Mean (SD)", "xx.x", "xx.x"))
    openxlsx::mergeCells(wb, "t.14.1.1", cols = 2:3, rows = 3)
    openxlsx::setColWidths(wb, "t.14.1.1", 1:3, c(30, 12, 12))
  }), 0L)
  # an empty spacer column between the arms
  expect_length(one(function(wb) {
    row(wb, 3, c("", "Placebo", "", "Drug X")); row(wb, 4, c("Mean (SD)", "xx.x", "", "xx.x"))
    openxlsx::setColWidths(wb, "t.14.1.1", 1:4, c(30, 12, 2, 12))
  }), 0L)
  # a label wider than its column, spilling into the empty cell beside it
  expect_length(one(function(wb) {
    row(wb, 3, c("", "", "Placebo", "Drug X"))
    row(wb, 4, c("Subjects with at least one adverse event", "", "xx", "xx"))
    openxlsx::setColWidths(wb, "t.14.1.1", 1:4, c(10, 10, 12, 12))
  }), 0L)
})

test_that("an agent's request holds its shell's column widths", {
  skip_on_cran()
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("haven")
  root <- withr::local_tempdir()
  ad <- file.path(root, "adam")
  dir.create(ad)
  haven::write_xpt(tflspec:::.ai_chr(tflspec::adsl), file.path(ad, "adsl.xpt"))
  f <- shw_book(file.path(root, "toc.xlsx"))
  d <- file.path(root, "study")
  invisible(tfl_ai_prepare(d, f, adam = ad, quiet = TRUE, mode = "agent", templates = FALSE))
  rq <- list.files(file.path(d, "ai", "requests"), pattern = "[.]md$", full.names = TRUE)
  txt <- paste(unlist(lapply(rq, readLines, encoding = "UTF-8")), collapse = "\n")
  expect_match(txt, "Column widths (Excel, characters): A=36.7", fixed = TRUE)
})
