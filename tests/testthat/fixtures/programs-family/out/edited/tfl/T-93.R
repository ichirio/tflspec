# ============================================================================
#  Program    : programs/tfl/T-93.R
#  Output     : T-93 (table) -> output/tfl/T-93.rtf
#  Generated  : <masked>
#  The data part makes `ard`; the report part is written from spec/.
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
# the study's setup, and the header, footer and tokens of every report
source("programs/tfl/report_setup.R")

report_id <- "T-93"

# ---- data: this report's ARD -----------------------------------------------
# Setup code: runs first in every ARD and report program of this study.
# What every program needs: the packages, and the options the study shares.
library(cards)
library(dplyr)
library(rtfreporter)

# Warnings are printed as they happen, so the run's log shows where
options(warn = 1)

# ---- this output's ARD, made elsewhere and taken in (input/ard/t93.rds)
ard <- tflspec::tfl_read_ard("input/ard/t93.rds")
if ("output_id" %in% names(ard)) ard <- ard[ard$output_id == "T-93", , drop = FALSE]
ard <- ard[setdiff(names(ard), c("output_id", "analysis_id", "population_id"))]
if (!nrow(ard)) stop("The ARD taken in has no rows for T-93.")

# ---- report ----------------------------------------------------------------
# the table, as spec/outputs/T-93.yml defines it
# BIGN: the column headings' N, apart from the table's rows
bign <- tflspec::tfl_bign_values(ard)
ard <- tflspec::tfl_bign_drop(ard)

plan <- ard |>
  normalize_ard() |>
  table_plan() |>
  plan_cells(default = list(n == 0 ~ "0", "{n:.0f} ({p:.1f%})"), na = "-") |>
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
  plan_columns(widths = c(row_label = 40, .values = 18)) |>
  plan_style(align_count_pct = TRUE)

# the report, as spec/outputs/T-93.yml defines it
doc <- rtf_document(
  program_fallback = file.path(path_programs_tfl, report_id),
  tokens = list(OUTPUT_LABEL = "Table 93", OUTPUT_TITLE = "", OUTPUT_POPULATION = ""),
  file = "output/tfl/T-93.rtf"
) |>
  rtf_section(header = header_standard, footer = footer_standard) |>
  rtf_tables(plan)
generate_rtfreport(doc, file.path(path_tfl, paste0(report_id, ".rtf")), overwrite = TRUE)
record_report(report_id)

# the end of this run
options(tflplanner.run = NULL)
