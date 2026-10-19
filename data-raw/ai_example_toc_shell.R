# The AI kit's sample TOC / SHELL workbook: inst/ai/example/onco_toc_shell.xlsx
#
# Made up in the way a company hands one over: a TOC sheet with a title row
# and a column of report numbers, then shells -- a title merged across the
# columns, a column header of two rows, the stub indented by the cell's
# alignment, the footnotes under the table.  Two shells share one sheet
# (the AE table and the laboratory shift), as some companies keep them.
# The reports are tables of tflspec's oncology sample study, and one
# figure (a Kaplan-Meier plot, its shell a mock-up described in cells, as
# companies draw a figure's shell).  Run from the
# package's root:  source("data-raw/ai_example_toc_shell.R")

library(openxlsx)

wb <- createWorkbook()
bold <- createStyle(textDecoration = "bold")
title <- createStyle(textDecoration = "bold", halign = "center")
ind1 <- createStyle(indent = 1)
ind2 <- createStyle(indent = 2)
centre <- createStyle(halign = "center")

# ---- the TOC -------------------------------------------------------------
addWorksheet(wb, "TOC")
writeData(wb, "TOC", "XYZ-ONC-01 Table of Contents (draft 0.2)", startRow = 1)
mergeCells(wb, "TOC", cols = 1:5, rows = 1)
addStyle(wb, "TOC", title, rows = 1, cols = 1)
toc <- data.frame(
  `No.` = c("", "T-14-1-1", "", "T-14-2-1", "F-14-2-2", "", "T-14-3-1", "T-14-3-5"),
  Type = c("", "Table", "", "Table", "Figure", "", "Table", "Table"),
  Title = c("14.1 Demographic Data", "Demographic and Baseline Characteristics",
            "14.2 Efficacy Data", "Best Overall Response and Objective Response Rate",
            "Kaplan-Meier Plot of Overall Survival",
            "14.3 Safety Data",
            "Treatment-Emergent Adverse Events by System Organ Class and Preferred Term",
            "Shift from Baseline in Laboratory Normal Range Indicator at Cycle 2 Day 1"),
  Population = c("", "Safety Population", "", "Intent-To-Treat Population",
                 "Intent-To-Treat Population", "", "Safety Population", "Safety Population"),
  Program = c("", "t_dm.sas", "", "t_orr.sas", "f_km_os.sas", "", "t_ae_socpt.sas",
              "t_lb_shift.sas"),
  check.names = FALSE, stringsAsFactors = FALSE)
writeData(wb, "TOC", toc, startRow = 3, headerStyle = bold)
setColWidths(wb, "TOC", 1:5, c(12, 8, 70, 26, 16))

# ---- a shell ---------------------------------------------------------------
shell <- function(sheet, start, id, ttl, pop, cols, rows, notes) {
  n <- length(cols) + 1L
  writeData(wb, sheet, id, startRow = start, startCol = 1)
  writeData(wb, sheet, ttl, startRow = start + 1L, startCol = 1)
  mergeCells(wb, sheet, cols = 1:n, rows = start + 1L)
  writeData(wb, sheet, pop, startRow = start + 2L, startCol = 1)
  mergeCells(wb, sheet, cols = 1:n, rows = start + 2L)
  addStyle(wb, sheet, title, rows = start + 1:2, cols = 1)
  r <- start + 4L
  for (h in cols) {
    writeData(wb, sheet, t(h), startRow = r, startCol = 1, colNames = FALSE)
    addStyle(wb, sheet, centre, rows = r, cols = 2:n, gridExpand = TRUE)
    r <- r + 1L
  }
  for (x in rows) {
    writeData(wb, sheet, t(x$cells), startRow = r, startCol = 1, colNames = FALSE)
    if (x$indent == 1L) addStyle(wb, sheet, ind1, rows = r, cols = 1)
    if (x$indent == 2L) addStyle(wb, sheet, ind2, rows = r, cols = 1)
    r <- r + 1L
  }
  r <- r + 1L
  for (f in notes) {
    writeData(wb, sheet, f, startRow = r, startCol = 1)
    r <- r + 1L
  }
  r + 2L
}
row <- function(label, values = character(), indent = 0L) {
  list(cells = c(label, values), indent = indent)
}
arms <- c("Drug X", "Control")
nn <- rep("(N=xx)", 2)
pct <- rep("xx (xx.x)", 2)

addWorksheet(wb, "T14.1.1")
shell("T14.1.1", 1, "Table 14.1.1", "Demographic and Baseline Characteristics",
      "(Safety Population)",
      list(c("", arms, "Total"), c("Characteristic", nn, "(N=xx)")),
      list(row("Age (years)"),
           row("n", rep("xx", 3), 1), row("Mean (SD)", rep("xx.x (xx.xx)", 3), 1),
           row("Median", rep("xx.x", 3), 1), row("Min, Max", rep("xx, xx", 3), 1),
           row("Sex, n (%)"), row("Male", rep("xx (xx.x)", 3), 1),
           row("Female", rep("xx (xx.x)", 3), 1),
           row("Age group, n (%)"), row("<65", rep("xx (xx.x)", 3), 1),
           row(">=65", rep("xx (xx.x)", 3), 1),
           row("Race, n (%)"), row("ASIAN", rep("xx (xx.x)", 3), 1),
           row("WHITE", rep("xx (xx.x)", 3), 1), row("...", character(), 1)),
      c("Percentages are based on the number of subjects in the analysis set.",
        "SD: standard deviation."))
setColWidths(wb, "T14.1.1", 1:4, c(36, 18, 18, 18))

addWorksheet(wb, "T14.2.1")
shell("T14.2.1", 1, "Table 14.2.1", "Best Overall Response and Objective Response Rate",
      "(Intent-To-Treat Population)",
      list(c("", arms), c("Response", nn)),
      list(row("Best overall response, n (%)"),
           row("Complete response (CR)", pct, 1), row("Partial response (PR)", pct, 1),
           row("Stable disease (SD)", pct, 1), row("Progressive disease (PD)", pct, 1),
           row("Not evaluable (NE)", pct, 1),
           row("Objective response rate (CR + PR)"),
           row("n (%)", pct, 1), row("95% CI", rep("[xx.x, xx.x]", 2), 1)),
      c("Best overall response per RECIST 1.1 (parameter BOR).",
        "95% CI: Clopper-Pearson exact confidence interval."))
setColWidths(wb, "T14.2.1", 1:3, c(40, 18, 18))

# a figure's shell: what the plot shows, in words
addWorksheet(wb, "F14.2.2")
fig <- c("Figure 14.2.2", "Kaplan-Meier Plot of Overall Survival",
         "(Intent-To-Treat Population)", "",
         "[Kaplan-Meier curves, one per arm: Drug X, Control]",
         "X axis: Time (Months), from 0",
         "Y axis: Survival Probability, 0 to 1 by 0.2",
         "Censored subjects marked on the curves (+)",
         "Legend inside the plot, top right", "",
         "Overall survival: ADTTE, PARAMCD = OS; censored when CNSR = 1.")
writeData(wb, "F14.2.2", fig, startRow = 1, startCol = 1)
mergeCells(wb, "F14.2.2", cols = 1:4, rows = 2)
mergeCells(wb, "F14.2.2", cols = 1:4, rows = 3)
addStyle(wb, "F14.2.2", title, rows = 2:3, cols = 1)
setColWidths(wb, "F14.2.2", 1, 70)

addWorksheet(wb, "Safety")
nxt <- shell("Safety", 1, "Table 14.3.1",
             "Treatment-Emergent Adverse Events by System Organ Class and Preferred Term",
             "(Safety Population)",
             list(c("System Organ Class", arms), c("Preferred Term", nn)),
             list(row("Subjects with at least one TEAE", pct),
                  row("SYSTEM ORGAN CLASS 1", pct), row("Preferred term 1", pct, 1),
                  row("Preferred term 2", pct, 1), row("SYSTEM ORGAN CLASS 2", pct),
                  row("Preferred term 1", pct, 1)),
             c("SOCs alphabetically; PTs by decreasing frequency within a SOC.",
               "A subject is counted once in a system organ class and once in a preferred term."))
shell("Safety", nxt, "Table 14.3.5",
      "Shift from Baseline in Laboratory Normal Range Indicator at Cycle 2 Day 1",
      "(Safety Population)",
      list(c("", "Drug X", "", "Control", ""),
           c("Parameter / Baseline", "NORMAL", "HIGH", "NORMAL", "HIGH")),
      list(row("Alanine Aminotransferase (U/L)"),
           row("NORMAL", rep("xx", 4), 1), row("HIGH", rep("xx", 4), 1),
           row("Aspartate Aminotransferase (U/L)"),
           row("NORMAL", rep("xx", 4), 1), row("HIGH", rep("xx", 4), 1)),
      c("ALT and AST at Cycle 2 Day 1; counts of subjects."))
setColWidths(wb, "Safety", 1:5, c(44, 14, 14, 14, 14))

saveWorkbook(wb, file.path("inst", "ai", "example", "onco_toc_shell.xlsx"), overwrite = TRUE)
