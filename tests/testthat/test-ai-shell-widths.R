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
