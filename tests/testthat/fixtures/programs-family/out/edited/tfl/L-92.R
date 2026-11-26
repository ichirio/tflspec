# ============================================================================
#  Program    : programs/tfl/L-92.R
#  Output     : L-92 (listing) -> output/tfl/L-92.rtf
#  Generated  : <masked>
#  The data part makes `content`; the report part is written from spec/.
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
# the study's setup, and the header, footer and tokens of every report
source("programs/tfl/report_setup.R")

report_id <- "L-92"

# ---- data: the listing's pages ---------------------------------------------
# Setup code: runs first in every ARD and report program of this study.
# What every program needs: the packages, and the options the study shares.
library(cards)
library(dplyr)
library(rtfreporter)

# Warnings are printed as they happen, so the run's log shows where
options(warn = 1)

# TODO: make the listing pages, e.g.
#   adae    <- readRDS(file.path(path_adam, "adae.rds"))
#   content <- as_rtftables(adae[, c("USUBJID", "AEDECOD")])
stop("tflplanner: the data part of L-92.R is still to be written.")

# ---- report ----------------------------------------------------------------
# the report, as spec/outputs/L-92.yml defines it
doc <- rtf_document(
  program_fallback = file.path(path_programs_tfl, report_id),
  tokens = list(OUTPUT_LABEL = "Listing 92", OUTPUT_TITLE = "", OUTPUT_POPULATION = ""),
  file = "output/tfl/L-92.rtf"
) |>
  rtf_section(header = header_standard, footer = footer_standard) |>
  rtf_tables(content)
generate_rtfreport(doc, file.path(path_tfl, paste0(report_id, ".rtf")), overwrite = TRUE)
record_report(report_id)

# the end of this run
options(tflplanner.run = NULL)
