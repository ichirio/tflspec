# ============================================================================
#  Program    : programs/tfl/F-92.R
#  Output     : F-92 (figure) -> output/tfl/F-92.rtf
#  Generated  : <masked>
#  The data and content parts are your code where its steps are; the report part is written from spec/.
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
# the study's setup, and the header, footer and tokens of every report
source("programs/tfl/report_setup.R")

report_id <- "F-92"

# Setup code: runs first in every ARD and report program of this study.
# What every program needs: the packages, and the options the study shares.
library(cards)
library(dplyr)
library(rtfreporter)

# Warnings are printed as they happen, so the run's log shows where
options(warn = 1)

# the figure style of the company standards, the figures' packages, and the figure checks
source("programs/tfl/fig_setup.R")

# ---- data: the figure's data -----------------------------------------------
# the report's data: ADSL (data catalog)

# the ARD of T-14-1-1 (its rows of the study ARD)
ard <- readRDS(file.path(path_ard, "ard.rds")) |>
  filter(output_id == "T-14-1-1")
if (!nrow(ard)) stop("The study ARD has no rows for T-14-1-1: run programs/ard/T-14-1-1.R first.")
# (the definition it was made from: the report records it, #293)
ard_built <- ard_fingerprint("T-14-1-1")

# ---- content: your code ----------------------------------------------------
# your code: leaves `content`
content <- ggplot2::ggplot(adsl)

# the figure drawn at its size on the page: 6.5 x 4.5 in (its own size), 300 dpi
content <- fig_sized(content, width_in = 6.5, height_in = 4.5, dpi = 300)
content <- report_content(content)

# ---- report ----------------------------------------------------------------
# the report, as spec/outputs/F-92.yml defines it
doc <- rtf_document(
  program_fallback = file.path(path_programs_tfl, report_id),
  tokens = list(OUTPUT_LABEL = "Figure 92", OUTPUT_TITLE = "", OUTPUT_POPULATION = ""),
  file = "output/tfl/F-92.rtf"
) |>
  rtf_section(header = header_standard, footer = footer_standard) |>
  rtf_figures(content)
generate_rtfreport(doc, file.path(path_tfl, paste0(report_id, ".rtf")), overwrite = TRUE)
record_report(report_id, ard = ard_built)

# the end of this run
options(tflplanner.run = NULL)
