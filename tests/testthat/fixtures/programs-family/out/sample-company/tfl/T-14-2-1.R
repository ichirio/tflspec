# ============================================================================
#  Program    : programs/tfl/T-14-2-1.R
#  Output     : T-14-2-1 (table) -> output/tfl/T-14-2-1.rtf
#  Description: Systolic blood pressure: change from baseline at week 24
#               (cycle 9 day 1)
#  Generated  : <masked>
#  The data part makes `data`; the report part is written from spec/.
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
# the study's setup, and the header, footer and tokens of every report
source("programs/tfl/report_setup.R")

report_id <- "T-14-2-1"

# ---- data: this report's normalized ARD ------------------------------------
# Setup code: runs first in every ARD and report program of this study.
# What every program needs: the packages, and the options the study shares.
library(cards)
library(dplyr)
library(rtfreporter)

# Warnings are printed as they happen, so the run's log shows where
options(warn = 1)

# Example Pharma: this report's rows of output/ard/ard.rds for T-14-2-1 (programs/tfl/T-14-2-1.R)
# this report's rows of the study ARD (made by programs/ard/T-14-2-1.R)
ard <- readRDS(file.path(path_ard, "ard.rds")) |>
  filter(output_id == report_id) |>
  select(-c(output_id, analysis_id, population_id))
if (!nrow(ard)) {
  stop("The study ARD has no rows for ", report_id, ": run programs/ard/T-14-2-1.R first.")
}

data <- normalize_ard(ard)
# Example Pharma: the company's normalization
data <- data[!is.na(data$stat), , drop = FALSE]

# ---- report ----------------------------------------------------------------
# the table, as spec/outputs/T-14-2-1.yml defines it
# BIGN: the column headings' N, apart from the table's rows
bign <- tflspec::tfl_bign_values(data)
data <- tflspec::tfl_bign_drop(data)

# the code lists: each code in its order (its label, when it has one)
cl_trt01a <- c("Drug X", "Control")

plan <- table_plan(data, cols = "TRT01A", rows = c(group = "variable")) |>
  plan_cells(
    default = list(n == 0 ~ "0", "{n:.0f} ({p:.1f%})"),
    continuous = list(
      n = "{N:.0f}",
      "Mean (SD)" = "{mean:.1f} ({sd:.2f})",
      SE = "{se:.2f}",
      "95% CI of the mean" = "{mean_lcl:.1f}, {mean_ucl:.1f}",
      Median = "{median:.1f}",
      "Min, Max" = "{min:.0f}, {max:.0f}"
    ),
    na = "-"
  ) |>
  plan_levels(TRT01A = cl_trt01a) |>
  plan_labels(
    variable = c(
      BASE = "Baseline (mmHg)",
      AVAL = "Week 24, cycle 9 day 1 (mmHg)",
      CHG = "Change from baseline (mmHg)"
    )
  ) |>
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
  plan_paginate_rows(max_rows = 30, split = "group_safe") |>
  plan_columns(widths = c(row_label = 40, .values = 18)) |>
  plan_style(align_count_pct = TRUE)

# the report, as spec/outputs/T-14-2-1.yml defines it
doc <- rtf_document(
  program_fallback = file.path(path_programs_tfl, report_id),
  tokens = list(
    OUTPUT_LABEL = "Table 14.2.1",
    OUTPUT_TITLE = "Systolic Blood Pressure (mmHg): Change from Baseline at Week 24 (Cycle 9 Day 1)",
    OUTPUT_POPULATION = "Safety Analysis Set"
  ),
  file = "output/tfl/T-14-2-1.rtf"
) |>
  rtf_section(
    header = header_standard,
    footer = rtf_footer(
      list(
        c(l = "SD = Standard Deviation; SE = Standard Error; CI = Confidence Interval (t distribution)."),
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
