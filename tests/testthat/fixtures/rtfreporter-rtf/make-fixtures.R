# A small fictional rtfreporter RTF (DMY-001, Example Pharma), written by
# rtfreporter itself: the page number is words in the header row ("Page 1 of
# 1"), the titles are paragraphs before the table, the footnote is a one-cell
# row after it.  Run to rewrite it (rtfreporter installed):
#   Rscript make-fixtures.R DMY-001/t_dm.rtf
library(rtfreporter)
out <- commandArgs(TRUE)[1]
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
df <- data.frame(Characteristic = c("Age (years)", "  Mean (SD)", "Sex, n (%)", "  Female"),
                 `Drug X` = c("", "54.3 (9.1)", "", "21 (52.5)"),
                 Control = c("", "55.0 (8.7)", "", "19 (47.5)"), check.names = FALSE)
doc <- rtf_document(page = list(orientation = "landscape")) |>
  rtf_section(header = rtf_header(list(c("DMY-001 / Example Pharma", "Page 1 of 1"))),
              footer = rtf_footer(list(c("t_dm.R", "01Jan2026 09:00")))) |>
  rtf_tables(list(rtftable(df, border = "tfl")),
             titles = c("Table 14.1.1", "Demographic and Baseline Characteristics", "Safety Population"),
             footnotes = "Percentages are of the safety population.")
suppressMessages(generate_rtfreport(doc, out, overwrite = TRUE))
