# ============================================================================
#  Program    : programs/tfl/T-14-1-1C.R
#  Output     : T-14-1-1C (table) -> output/tfl/T-14-1-1C.rtf
#  Description: Demographics (a code SPEC)
#  Generated  : <masked>
#  The data part makes `data`; the report part is written from spec/.
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
# the study's setup, and the header, footer and tokens of every report
source("programs/tfl/report_setup.R")

report_id <- "T-14-1-1C"

# ---- data: this report's normalized ARD ------------------------------------
# Setup code: runs first in every ARD and report program of this study.
# What every program needs: the packages, and the options the study shares.
library(cards)
library(dplyr)
library(rtfreporter)

# Warnings are printed as they happen, so the run's log shows where
options(warn = 1)

# Example Pharma: this report's rows of output/ard/ard.rds for T-14-1-1C (programs/tfl/T-14-1-1C.R)
# this report's rows of the study ARD (made by programs/ard/T-14-1-1C.R)
ard <- readRDS(file.path(path_ard, "ard.rds")) |>
  filter(output_id == report_id) |>
  select(-c(output_id, analysis_id, population_id))
if (!nrow(ard)) {
  stop("The study ARD has no rows for ", report_id, ": run programs/ard/T-14-1-1C.R first.")
}

data <- normalize_ard(ard)
# Example Pharma: the company's normalization
data <- data[!is.na(data$stat), , drop = FALSE]

# ---- report ----------------------------------------------------------------
# the table, as spec/outputs/T-14-1-1C.yml defines it
# BIGN: the column headings' N, apart from the table's rows
bign <- tflspec::tfl_bign_values(data, labels = c("Drug X" = "Drug X 200 mg", Control = "Control 100 mg"))
data <- tflspec::tfl_bign_drop(data)

# the code lists: each code in its order (its label, when it has one)
cl_trt01a <- c("Drug X" = "Drug X 200 mg", Control = "Control 100 mg")

plan <- table_plan(data, cols = "TRT01A", rows = c(group = "variable")) |>
  plan_cells(
    default = list(n == 0 ~ "0", "{n:.0f} ({p:.1f%})"),
    continuous = list(
      n = "{N}",
      "Mean (SD)" = "{mean} ({sd})",
      Median = "{median}",
      "Min, Max" = "{min}, {max}"
    ),
    na = "-"
  ) |>
  plan_levels(TRT01A = names(cl_trt01a)) |>
  plan_labels(
    variable = c(
      AGE = "Age (years)",
      AGEGR1 = "Age group, n (%)",
      SEX = "Sex, n (%)",
      RACE = "Race, n (%)",
      ECOGBL = "Baseline ECOG, n (%)"
    ),
    TRT01A = c("Drug X" = "Drug X 200 mg", Control = "Control 100 mg")
  ) |>
  plan_digits(
    continuous = c(N = 0, mean = 1, sd = 2, median = 1, min = 0, max = 0)
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

# the report, as spec/outputs/T-14-1-1C.yml defines it
doc <- rtf_document(
  program_fallback = file.path(path_programs_tfl, report_id),
  tokens = list(
    OUTPUT_LABEL = "Table 14.1.1C",
    OUTPUT_TITLE = "Demographic Characteristics",
    OUTPUT_POPULATION = "Safety Analysis Set"
  ),
  file = "output/tfl/T-14-1-1C.rtf"
) |>
  rtf_section(header = header_standard, footer = footer_standard) |>
  rtf_tables(plan) |>
  rtf_footnotes(list(list(c(l = "SD = Standard Deviation."))))
generate_rtfreport(doc, file.path(path_tfl, paste0(report_id, ".rtf")), overwrite = TRUE)
record_report(report_id)

# the end of this run
options(tflplanner.run = NULL)
