# ============================================================================
#  Program    : programs/tfl/F-96.R
#  Output     : F-96 (figure) -> output/tfl/F-96.rtf
#  Generated  : <masked>
#  The data part makes `content`; the report part is written from spec/.
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
# the study's setup, and the header, footer and tokens of every report
source("programs/tfl/report_setup.R")

report_id <- "F-96"

# ---- data: the figure ------------------------------------------------------
# Setup code: runs first in every ARD and report program of this study.
# What every program needs: the packages, and the options the study shares.
library(cards)
library(dplyr)
library(rtfreporter)

# Warnings are printed as they happen, so the run's log shows where
options(warn = 1)

# TODO: make the figure(s), e.g.
#   content <- list(ggplot2::ggplot(adsl, ggplot2::aes(AGE)) +
#                     ggplot2::geom_histogram())
stop("tflplanner: the data part of F-96.R is still to be written.")

# ---- report ----------------------------------------------------------------
# the report, as spec/outputs/F-96.yml defines it
doc <- rtf_document(
  program_fallback = file.path(path_programs_tfl, report_id),
  tokens = list(OUTPUT_LABEL = "Figure 96", OUTPUT_TITLE = "", OUTPUT_POPULATION = ""),
  file = "output/tfl/F-96.rtf"
) |>
  rtf_section(header = header_standard, footer = footer_standard) |>
  rtf_figures(plot)
generate_rtfreport(doc, file.path(path_tfl, paste0(report_id, ".rtf")), overwrite = TRUE)
record_report(report_id)

# the end of this run
options(tflplanner.run = NULL)
