# A TOC / SHELL workbook of the real shape (made up): a TOC and a Topline
# TOC; shells whose sheet names are mistyped, Part sheets the TOC names one
# shell for, a heading with a format sample (Table 15.1.2.x), a cover sheet
# with no number, a notes sheet too small to be a shell, a TOC item with
# no sheet.
real_shape_book <- function(f) {
  wb <- openxlsx::createWorkbook()
  toc <- data.frame(
    `No.` = as.character(1:6),
    `Shell No.` = c("t.14.1.1", "t.15.1.2", "t.15.1.4.2-2", "t.15.3.1", "t.16.1.1", "t.15.2.1"),
    `Output No.` = paste("Table", c("14.1.1", "15.1.2", "15.1.4.2", "15.3.1", "16.1.1", "15.2.1")),
    `Title` = c("Subject Disposition", "Summary of Demographics", "Demographics by Part",
                "Adverse Events by SOC and PT", "Listing of Deaths", "Exposure"),
    `Population` = c("All Subjects", "Safety", "Safety", "Safety", "Safety", "Safety"),
    check.names = FALSE, stringsAsFactors = FALSE)
  openxlsx::addWorksheet(wb, "TOC")
  openxlsx::writeData(wb, "TOC", toc, startRow = 2)
  openxlsx::addWorksheet(wb, "Topline_TOC")
  openxlsx::writeData(wb, "Topline_TOC", toc[c(2, 4), ], startRow = 2)
  openxlsx::writeData(wb, "Topline_TOC", data.frame(`No.` = "9", `Shell No.` = "t.15.9.1",
    `Output No.` = "Table 15.9.1", Title = "Topline Efficacy", Population = "FAS",
    check.names = FALSE), startRow = 5, colNames = FALSE)
  shell <- function(s, lines) {
    openxlsx::addWorksheet(wb, s)
    for (k in seq_along(lines)) openxlsx::writeData(wb, s, lines[[k]], startRow = k, colNames = FALSE)
  }
  body <- list(t(c("", "Placebo", "Drug X")), t(c("n", "xx", "xx")), t(c("Mean (SD)", "xx.x", "xx.x")))
  shell("t.14.1.1", c(list("Table 14.1.1  Subject Disposition", "All Subjects"), body))
  shell("t.15.1.2", c(list(t(c("Table 15.1.2.x", "Summary of Demographics")), "Safety Analysis Set"), body))
  shell("t.15.1.4.2.2-1", c(list("Table 15.1.4.2.a  Demographics - Part 1", "Safety Analysis Set"), body))
  shell("t.15.1.4.2.2-2", c(list("Table 15.1.4.2.b  Demographics - Part 2", "Safety Analysis Set"), body))
  shell("t.99.9", c(list("Table 15.3.1  Adverse Events by SOC and PT", "Safety Analysis Set"), body))
  shell("t.15.2.1", c(list("Summary of Exposure", "Safety Analysis Set"), body))
  shell("Cover", list("Study XYZ", "Shells for the CSR", "Version 1.0", "Date 2026-10-01",
                      "Prepared by: Biostatistics", "Confidential"))
  shell("Notes", list("See SAP"))
  openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
  f
}
