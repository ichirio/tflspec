# tfl_ai_read_shell(): a line break inside a cell (Alt+Enter in Excel) is
# read as one cell, and the cells after it keep their text (#370)

test_that("a cell with a line break does not shift the shared strings after it", {
  f <- tempfile(fileext = ".xlsx")
  on.exit(unlink(f), add = TRUE)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "TOC")
  openxlsx::writeData(wb, "TOC", data.frame(
    No = c("Table 14.1.1", "Table 14.3.1"),
    Title = c("Demographics\nSafety Population", "Adverse Events")), startRow = 1)
  openxlsx::addWorksheet(wb, "Table 14.1.1")
  s <- "Table 14.1.1"
  openxlsx::writeData(wb, s, "Table 14.1.1  Demographics", startRow = 1)
  openxlsx::writeData(wb, s, t(c("", "Placebo\n(N=xx)", "Drug X\n(N=xx)")),
                      startRow = 3, colNames = FALSE)
  openxlsx::writeData(wb, s, t(c("Age (years)", "xx.x", "xx.x")), startRow = 4, colNames = FALSE)
  openxlsx::writeData(wb, s, t(c("Sex, n (%)", "", "")), startRow = 5, colNames = FALSE)
  openxlsx::saveWorkbook(wb, f)
  x <- tfl_ai_read_shell(f)
  lines <- strsplit(x$text, "\n", fixed = TRUE)[[1L]]
  expect_true("3: B=Placebo\\n(N=xx) | C=Drug X\\n(N=xx)" %in% lines)
  expect_true("4: A=Age (years) | B=xx.x | C=xx.x" %in% lines)
  expect_true("5: A=Sex, n (%)" %in% lines)
  expect_true("2: A=Table 14.1.1 | B=Demographics\\nSafety Population" %in% lines)
  expect_true("3: A=Table 14.3.1 | B=Adverse Events" %in% lines)
  # the TOC's reports and the shell sheet are found as before
  expect_identical(x$toc$key, c("T14.1.1", "T14.3.1"))
  expect_identical(x$toc$shell[1L], "Table 14.1.1")
})

test_that("a report number in a cell with a line break: its key", {
  expect_identical(.ai_id_key("Table 14.1.1\nDemographics"), "T14.1.1")
  expect_identical(.ai_id_type("Listing 16.2.1\n(all subjects)"), "listing")
})
