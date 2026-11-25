# ============================================================================
#  Program    : programs/tfl/T-14-0-1.R
#  Output     : T-14-0-1 (table) -> output/tfl/T-14-0-1.rtf
#  Description: Study information: dictionary versions and the dates of the
#               data (no analysis set)
#  Generated  : <masked>
#  The data part makes `data`; the report part is written from spec/.
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
# the study's setup, and the header, footer and tokens of every report
source("programs/tfl/report_setup.R")

report_id <- "T-14-0-1"

# ---- data: this report's normalized ARD ------------------------------------
# Setup code: runs first in every ARD and report program of this study.
# What every program needs: the packages, and the options the study shares.
library(cards)
library(dplyr)
library(rtfreporter)

# Warnings are printed as they happen, so the run's log shows where
options(warn = 1)

# this report's rows of the study ARD (made by programs/ard/T-14-0-1.R)
ard <- readRDS(file.path(path_ard, "ard.rds")) |>
  filter(output_id == report_id) |>
  select(-c(output_id, analysis_id, population_id))
if (!nrow(ard)) {
  stop("The study ARD has no rows for ", report_id, ": run programs/ard/T-14-0-1.R first.")
}

# normalize and rework
data <- ard |> normalize_ard()
data <- data[!is.na(data$variable), , drop = FALSE]

# ---- report ----------------------------------------------------------------
TRUE
# the table, as spec/outputs/T-14-0-1.yml defines it
# BIGN: the column headings' N, apart from the table's rows
data <- tflspec::tfl_bign_drop(data)

plan <- table_plan(data, cols = "context", rows = c(group = "variable")) |>
  plan_cells(default = "{value}", na = "-") |>
  plan_levels(variable = "INFO") |>
  plan_labels(variable = c(INFO = "Dictionaries and dates of the data")) |>
  plan_col_header(
    tribble(
      ~line, ~cols,       ~text,
      "1",   "row_label", NA,
      "1",   ".values",   NA,
      "2",   "row_label", "Item",
      "2",   ".values",   "Value"
    )
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

# the report, as spec/outputs/T-14-0-1.yml defines it
doc <- rtf_document(
  program_fallback = file.path(path_programs_tfl, report_id),
  tokens = list(
    OUTPUT_LABEL = "Table 14.0.1",
    OUTPUT_TITLE = "Study Information",
    OUTPUT_POPULATION = "All Subjects"
  ),
  file = "output/tfl/T-14-0-1.rtf"
) |>
  rtf_section(
    header = header_standard,
    footer = rtf_footer(
      list(
        c(l = "The dictionary versions are the study's (data management plan); the dates are those of ADSL (the data cut-off: DCUTDT)."),
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
