# ============================================================================
#  Program    : programs/tfl/F-94.R
#  Output     : F-94 (figure) -> output/tfl/F-94.rtf
#  Generated  : <masked>
#  The data part makes `content`; the report part is written from spec/.
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
# the study's setup, and the header, footer and tokens of every report
source("programs/tfl/report_setup.R")

report_id <- "F-94"

# ---- data: the figure ------------------------------------------------------
# Setup code: runs first in every ARD and report program of this study.
# What every program needs: the packages, and the options the study shares.
library(cards)
library(dplyr)
library(rtfreporter)

# Warnings are printed as they happen, so the run's log shows where
options(warn = 1)

# the data: ADSL, ADVS (data catalog)

# the figure style of the company standards, and the figure checks
source("programs/tfl/fig_setup.R")

# ---- the plot (ggplot2), written by hand: leaves `plot`
#      (theme_tfl(), scale_colour_tfl(), tfl_marker() ... give the standard's look)
# TODO: the plot (ggplot2), in the figure style of the company standards, e.g.
#   plot <- ggplot2::ggplot(adsl, ggplot2::aes(AGE, fill = TRT01A)) +
#     ggplot2::geom_histogram() + scale_fill_tfl("treatment", levels(adsl$TRT01A)) +
#     theme_tfl()
stop("tflplanner: the plot of F-94.R is still to be written.")

# the figure checks: dropped rows, colours against the standard
tfl_check(plot)

# ---- report ----------------------------------------------------------------
# the report, as spec/outputs/F-94.yml defines it
doc <- rtf_document(
  program_fallback = file.path(path_programs_tfl, report_id),
  tokens = list(OUTPUT_LABEL = "Figure 94", OUTPUT_TITLE = "", OUTPUT_POPULATION = ""),
  file = "output/tfl/F-94.rtf"
) |>
  rtf_section(header = header_standard, footer = footer_standard) |>
  rtf_figures(plot)
generate_rtfreport(doc, file.path(path_tfl, paste0(report_id, ".rtf")), overwrite = TRUE)
record_report(report_id)

# the end of this run
options(tflplanner.run = NULL)
