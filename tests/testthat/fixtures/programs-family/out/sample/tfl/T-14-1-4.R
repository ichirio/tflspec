# ============================================================================
#  Program    : programs/tfl/T-14-1-4.R
#  Output     : T-14-1-4 (table) -> output/tfl/T-14-1-4.rtf
#  Description: Demographic characteristics of the screen failures (an
#               analysis set by a condition alone)
#  Generated  : <masked>
#  The data part makes `ard`; the report part is written from spec/.
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
# the study's setup, and the header, footer and tokens of every report
source("programs/tfl/report_setup.R")

report_id <- "T-14-1-4"

# ---- data: this report's ARD -----------------------------------------------
# Setup code: runs first in every ARD and report program of this study.
# What every program needs: the packages, and the options the study shares.
library(cards)
library(dplyr)
library(rtfreporter)

# Warnings are printed as they happen, so the run's log shows where
options(warn = 1)

# this report's rows of the study ARD (made by programs/ard/T-14-1-4.R)
ard <- readRDS(file.path(path_ard, "ard.rds")) |>
  filter(output_id == report_id) |>
  select(-c(output_id, analysis_id, population_id))
if (!nrow(ard)) {
  stop("The study ARD has no rows for ", report_id, ": run programs/ard/T-14-1-4.R first.")
}

# ---- report ----------------------------------------------------------------
# the table, as spec/outputs/T-14-1-4.yml defines it
# BIGN: the column headings' N, apart from the table's rows
bign <- tflspec::tfl_bign_values(ard, labels = c("SCREEN FAILURE" = "Screen Failures"))
ard <- tflspec::tfl_bign_drop(ard)

plan <- ard |>
  normalize_ard() |>
  table_plan(cols = "ARMNRS", rows = c(group = "variable")) |>
  plan_cells(
    default = list(n == 0 ~ "0", "{n:.0f} ({p:.1f%})"),
    continuous = list(
      n = "{N:.0f}",
      "Mean (SD)" = "{mean:.1f} ({sd:.2f})",
      Median = "{median:.1f}",
      "Min, Max" = "{min:.0f}, {max:.0f}"
    ),
    na = "-"
  ) |>
  plan_labels(
    variable = c(
      AGE = "Age (years)",
      AGEGR1 = "Age group, n (%)",
      SEX = "Sex, n (%)",
      RACE = "Race, n (%)"
    ),
    ARMNRS = c("SCREEN FAILURE" = "Screen Failures"),
    SEX = c(F = "Female", M = "Male"),
    AGEGR1 = c("<65" = "<65 years", ">=65" = ">=65 years"),
    RACE = c(
      WHITE = "White",
      "BLACK OR AFRICAN AMERICAN" = "Black or African American",
      ASIAN = "Asian",
      "AMERICAN INDIAN OR ALASKA NATIVE" = "American Indian or Alaska Native",
      "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" = "Native Hawaiian or Other Pacific Islander",
      OTHER = "Other"
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

# the report, as spec/outputs/T-14-1-4.yml defines it
doc <- rtf_document(
  program_fallback = file.path(path_programs_tfl, report_id),
  tokens = list(
    OUTPUT_LABEL = "Table 14.1.4",
    OUTPUT_TITLE = "Demographic Characteristics of Screen Failures",
    OUTPUT_POPULATION = "All Screen Failures"
  ),
  file = "output/tfl/T-14-1-4.rtf"
) |>
  rtf_section(
    header = header_standard,
    footer = rtf_footer(
      list(
        c(l = "SD = Standard Deviation.  Screen failures: screened subjects not randomized (ARMNRS is SCREEN FAILURE)."),
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
