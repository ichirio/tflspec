# ============================================================================
#  Program    : programs/tfl/T-14-2-3.R
#  Output     : T-14-2-3 (table) -> output/tfl/T-14-2-3.rtf
#  Description: Systolic blood pressure: mean change at week 24 (95% CI),
#               p-value; the ARD also holds the test's method and
#               alternative as text
#  Generated  : <masked>
#  The data part makes `ard`; the report part is written from spec/.
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
# the study's setup, and the header, footer and tokens of every report
source("programs/tfl/report_setup.R")

report_id <- "T-14-2-3"

# ---- data: this report's ARD -----------------------------------------------
# Setup code: runs first in every ARD and report program of this study.
# What every program needs: the packages, and the options the study shares.
library(cards)
library(dplyr)
library(rtfreporter)

# Warnings are printed as they happen, so the run's log shows where
options(warn = 1)

# this report's rows of the study ARD (made by programs/ard/T-14-2-3.R)
ard <- readRDS(file.path(path_ard, "ard.rds")) |>
  filter(output_id == report_id) |>
  select(-c(output_id, analysis_id, population_id))
if (!nrow(ard)) {
  stop("The study ARD has no rows for ", report_id, ": run programs/ard/T-14-2-3.R first.")
}

# ---- report ----------------------------------------------------------------
# the table, as spec/outputs/T-14-2-3.yml defines it
# BIGN: the column headings' N, apart from the table's rows
bign <- tflspec::tfl_bign_values(ard)
ard <- tflspec::tfl_bign_drop(ard)

plan <- ard |>
  normalize_ard() |>
  table_plan(cols = "TRT01A", rows = c(group = "variable")) |>
  plan_cells(
    default = list(n == 0 ~ "0", "{n:.0f} ({p:.1f%})"),
    continuous = list(
      n = "{N:.0f}",
      "Mean (95% CI)" = "{estimate:.1f} ({conf.low:.1f}, {conf.high:.1f})",
      "p-value" = "{p.value:stat_fmt}"
    ),
    na = "-"
  ) |>
  plan_levels(variable = "CHG") |>
  plan_labels(variable = c(CHG = "Change from baseline at week 24 (mmHg)")) |>
  plan_col_header(
    tribble(
      ~line, ~cols,       ~span,  ~text,
      "1",   "row_label", NA,     NA,
      "1",   ".values",   "each", "{col}",
      "2",   "row_label", NA,     "Characteristic",
      "2",   ".values",   "each", "(N={n})"
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
  plan_paginate_rows(max_rows = 24) |>
  plan_columns(widths = c(row_label = 40, .values = 18)) |>
  plan_style(align_count_pct = TRUE)

# the report, as spec/outputs/T-14-2-3.yml defines it
doc <- rtf_document(
  program_fallback = file.path(path_programs_tfl, report_id),
  tokens = list(
    OUTPUT_LABEL = "Table 14.2.3",
    OUTPUT_TITLE = "Systolic Blood Pressure (mmHg): Mean Change from Baseline at Week 24 (Cycle 9 Day 1) (95% CI)",
    OUTPUT_POPULATION = "Safety Analysis Set"
  ),
  file = "output/tfl/T-14-2-3.rtf"
) |>
  rtf_section(
    header = header_standard,
    footer = rtf_footer(
      list(
        c(l = "CI = Confidence Interval.  95% CI and p-value: one-sample t-test of the change from baseline (H0: mean change = 0)."),
        c(l = "Systolic blood pressure on day 1 of each 21-day cycle; week 24 is cycle 9 day 1."),
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
