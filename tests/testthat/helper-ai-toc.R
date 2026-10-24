# A TOC / SHELL workbook of a company's shape (made up): a TOC row a report,
# with numbers of the company's own, the shell's number (its sheet's name),
# the report's number, two titles, the population and a part flag (p1 / p2
# / both); 60 shell sheets, several rows sharing one; a few rows whose
# shell has no sheet.  Returns the path; attr "toc" the rows written.
company_toc_book <- function(f, n_shells = 60L, n_rows = 100L) {
  shells <- sprintf("t.15.%d.%d.%d", 1L + (seq_len(n_shells) - 1L) %/% 20L,
                    1L + ((seq_len(n_shells) - 1L) %/% 5L) %% 4L, 1L + (seq_len(n_shells) - 1L) %% 5L)
  # each shell its own report first, then the rows sharing it
  use <- c(seq_len(n_shells), rep(seq(1L, n_shells, by = 3L), length.out = n_rows - n_shells - 3L))
  use <- sort(use)
  shell_of <- c(shells[use], "t.16.9.1", "t.16.9.2", "t.16.9.3")
  seq_in <- stats::ave(seq_along(shell_of), shell_of, FUN = seq_along)
  num <- sub("^t[.]", "", shell_of)
  report <- ifelse(seq_in == 1L, num, paste0(num, ".", seq_in))
  flag <- rep(c("p1", "p2", "both", ""), length.out = length(report))
  toc <- data.frame(
    `Doc No` = sprintf("D%03d", seq_along(report)),
    `Seq` = as.character(seq_along(report)),
    Shell = shell_of,
    `Output No` = paste("Table", report),
    `Title 1` = paste("Summary of Measure", seq_along(report)),
    `Title 2` = ifelse(seq_in == 1L, "Overall", paste("Subgroup", seq_in)),
    Population = rep(c("Safety Analysis Set", "Full Analysis Set"), length.out = length(report)),
    Phase = flag,
    Program = sprintf("t_%s.sas", gsub(".", "_", report, fixed = TRUE)),
    check.names = FALSE, stringsAsFactors = FALSE)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "TOC")
  openxlsx::writeData(wb, "TOC", "Tables of the study (made up)", startRow = 1)
  openxlsx::writeData(wb, "TOC", toc, startRow = 3)
  for (s in shells) {
    openxlsx::addWorksheet(wb, s)
    openxlsx::writeData(wb, s, paste("Table", sub("^t[.]", "", s)), startRow = 1)
    openxlsx::writeData(wb, s, "Summary of Measure (Population)", startRow = 2)
    openxlsx::writeData(wb, s, t(c("", "Placebo", "Drug X")), startRow = 4, colNames = FALSE)
    openxlsx::writeData(wb, s, t(c("n", "xx", "xx")), startRow = 5, colNames = FALSE)
  }
  openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
  structure(f, toc = toc)
}
