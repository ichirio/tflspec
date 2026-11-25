# ============================================================================
#  Program    : programs/tfl/T-14-3-1C.R
#  Output     : T-14-3-1C (table) -> output/tfl/T-14-3-1C.rtf
#  Description: TEAEs by SOC / PT (a code SPEC)
#  Generated  : <masked>
#  The data part makes `data`; the report part is written from spec/.
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
# the study's setup, and the header, footer and tokens of every report
source("programs/tfl/report_setup.R")

report_id <- "T-14-3-1C"

# ---- data: this report's normalized ARD ------------------------------------
# Setup code: runs first in every ARD and report program of this study.
# What every program needs: the packages, and the options the study shares.
library(cards)
library(dplyr)
library(rtfreporter)

# Warnings are printed as they happen, so the run's log shows where
options(warn = 1)

# Example Pharma: this report's rows of output/ard/ard.rds for T-14-3-1C (programs/tfl/T-14-3-1C.R)
# this report's rows of the study ARD (made by programs/ard/T-14-3-1C.R)
ard <- readRDS(file.path(path_ard, "ard.rds")) |>
  filter(output_id == report_id) |>
  select(-c(output_id, analysis_id, population_id))
if (!nrow(ard)) {
  stop("The study ARD has no rows for ", report_id, ": run programs/ard/T-14-3-1C.R first.")
}

data <- normalize_ard(ard)
# Example Pharma: the company's normalization
data <- data[!is.na(data$stat), , drop = FALSE]

# ---- report ----------------------------------------------------------------
# the table, as spec/outputs/T-14-3-1C.yml defines it
# BIGN: the column headings' N, apart from the table's rows
bign <- tflspec::tfl_bign_values(data)
data <- tflspec::tfl_bign_drop(data)

# the code lists: each code in its order (its label, when it has one)
cl_trt01a <- c("Drug X", "Control")

plan <- table_plan(
  data,
  cols = "TRT01A",
  rows = c(group1 = "AEBODSYS"),
  label = c(label = "AEDECOD")
) |>
  plan_cells(default = list(n == 0 ~ "0", "{n:.0f} ({p:.1f%})"), na = "-") |>
  plan_levels(TRT01A = cl_trt01a) |>
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

# the report, as spec/outputs/T-14-3-1C.yml defines it
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
    OUTPUT_LABEL = "Table 14.3.1C",
    OUTPUT_TITLE = "Treatment-Emergent Adverse Events by System Organ Class and Preferred Term",
    OUTPUT_POPULATION = "Safety Analysis Set"
  ),
  file = "output/tfl/T-14-3-1C.rtf"
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
