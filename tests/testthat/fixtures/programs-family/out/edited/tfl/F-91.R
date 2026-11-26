# ============================================================================
#  Program    : programs/tfl/F-91.R
#  Output     : F-91 (figure) -> output/tfl/F-91.rtf
#  Generated  : <masked>
#  The data and content parts are your code where its steps are; the report part is written from spec/.
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
# the study's setup, and the header, footer and tokens of every report
source("programs/tfl/report_setup.R")

report_id <- "F-91"

# Setup code: runs first in every ARD and report program of this study.
# What every program needs: the packages, and the options the study shares.
library(cards)
library(dplyr)
library(rtfreporter)

# Warnings are printed as they happen, so the run's log shows where
options(warn = 1)

# the figure style of the company standards, the figures' packages, and the figure checks
source("programs/tfl/fig_setup.R")

# ---- data: your code -------------------------------------------------------
# the report's data: ADSL (data catalog)

# this figure's ARD (its rows of the study ARD)
ard <- readRDS(file.path(path_ard, "ard.rds")) |>
  filter(output_id == "F-91")
if (!nrow(ard)) stop("The study ARD has no rows for F-91: run programs/ard/F-91.R first.")
# (the definition it was made from: the report records it, #293)
ard_built <- ard_fingerprint("F-91")

# your code: leaves `df`
df <- adsl

df <- report_data(df, "df")

# ---- content: the figure ---------------------------------------------------
stop("tflplanner: the figure of F-91.R is still to be written.")

# ---- report ----------------------------------------------------------------
# the report, as spec/outputs/F-91.yml defines it
doc <- rtf_document(
  program_fallback = file.path(path_programs_tfl, report_id),
  tokens = list(OUTPUT_LABEL = "Figure 91", OUTPUT_TITLE = "", OUTPUT_POPULATION = ""),
  file = "output/tfl/F-91.rtf"
) |>
  rtf_section(header = header_standard, footer = footer_standard) |>
  rtf_figures(plot)
generate_rtfreport(doc, file.path(path_tfl, paste0(report_id, ".rtf")), overwrite = TRUE)
record_report(report_id, ard = ard_built)

# the end of this run
options(tflplanner.run = NULL)
