# ============================================================================
#  Program    : programs/tfl/T-14-3-1.R
#  Output     : T-14-3-1 (table) -> output/tfl/T-14-3-1.rtf
#  Description: TEAEs by SOC / PT
#  Generated  : <masked>
#  The data part makes `ard`; the report part is written from spec/.
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
# the study's setup, and the header, footer and tokens of every report
source("programs/tfl/report_setup.R")

report_id <- "T-14-3-1"

# ---- data: this report's ARD -----------------------------------------------
# Setup code: runs first in every ARD and report program of this study.
# What every program needs: the packages, and the options the study shares.
library(cards)
library(dplyr)
library(rtfreporter)

# Warnings are printed as they happen, so the run's log shows where
options(warn = 1)

# this report's rows of the study ARD (made by programs/ard/T-14-3-1.R)
ard <- readRDS(file.path(path_ard, "ard.rds")) |>
  filter(output_id == report_id) |>
  select(-c(output_id, analysis_id, population_id))
if (!nrow(ard)) {
  stop("The study ARD has no rows for ", report_id, ": run programs/ard/T-14-3-1.R first.")
}

# ---- report ----------------------------------------------------------------
# the table, as spec/outputs/T-14-3-1.yml defines it
# BIGN: the column headings' N, apart from the table's rows
bign <- tflspec::tfl_bign_values(ard)
ard <- tflspec::tfl_bign_drop(ard)

plan <- ard |>
  normalize_ard(hierarchy = c("AEBODSYS", "AEDECOD"), overall = "Any TEAE") |>
  table_plan(
    cols = "TRT01A",
    rows = c(group1 = "AEBODSYS"),
    label = c(label = "AEDECOD")
  ) |>
  plan_cells(default = list(n == 0 ~ "0", "{n:.0f} ({p:.1f%})"), na = "-") |>
  plan_sort(c(".overall", "group1", ".depth", "-n", "label")) |>
  plan_col_header(
    tribble(
      ~line, ~cols,       ~span,  ~text,
      "1",   "row_label", NA,     NA,
      "1",   ".values",   "each", "{col}\n(N={n})\nn (%)",
      "2",   "row_label", NA,     "System Organ Class\n   Preferred Term",
      "2",   ".values",   NA,     NA
    ),
    values = bign
  ) |>
  plan_stub(name = "row_label", before = TRUE, heading = "") |>
  plan_blanks(
    where = "between_groups",
    first = TRUE,
    last = TRUE,
    counted = TRUE
  ) |>
  plan_paginate_rows(max_rows = 22, split = "group_force") |>
  plan_columns(widths = c(row_label = 40, .values = 18)) |>
  plan_style(align_count_pct = TRUE)

# the report, as spec/outputs/T-14-3-1.yml defines it
doc <- rtf_document(
  page = list(
    margin_top_in = 0.5,
    margin_bottom_in = 0.5,
    margin_left_in = 0.5,
    margin_right_in = 0.5
  ),
  style = rtf_style(font_size_half_points = 16),
  program_fallback = file.path(path_programs_tfl, report_id),
  tokens = list(
    OUTPUT_LABEL = "Table 14.3.1",
    OUTPUT_TITLE = "Treatment-Emergent Adverse Events by System Organ Class and Preferred Term",
    OUTPUT_POPULATION = "Safety Analysis Set"
  ),
  file = "output/tfl/T-14-3-1.rtf"
) |>
  rtf_section(
    header = header_compact,
    footer = rtf_footer(
      list(
        c(l = "Subjects are counted once per system organ class and once per preferred term."),
        c(l = "Source: tflspec's oncology sample study XYZ-ONC-01 (simulated ADaM data; not real patients)."),
        c(l = "{PROGRAM}       Generated on: {DATETIME}")
      )
    )
  ) |>
  rtf_tables(plan)
generate_rtfreport(doc, file.path(path_tfl, paste0(report_id, ".rtf")), overwrite = TRUE)
record_report(report_id)

# the end of this run
options(tflplanner.run = NULL)
