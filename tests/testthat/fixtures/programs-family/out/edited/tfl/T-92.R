# ============================================================================
#  Program    : programs/tfl/T-92.R
#  Output     : T-92 (table) -> output/tfl/T-92.rtf
#  Generated  : <masked>
#  The data and content parts are your code where its steps are; the report part is written from spec/.
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
# the study's setup, and the header, footer and tokens of every report
source("programs/tfl/report_setup.R")

report_id <- "T-92"

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

# your code: leaves `data` (or `ard`, normalized below)
ard <- cards::ard_continuous(adsl, variables = AGE)

data <- normalize_ard(ard)

data <- report_data(data, "data")

# ---- content: your code ----------------------------------------------------
# your code: leaves `content`
content <- head(data)

content <- report_content_check(content, "table", report_id)

# ---- report ----------------------------------------------------------------
# the report, as spec/outputs/T-92.yml defines it
doc <- rtf_document(
  program_fallback = file.path(path_programs_tfl, report_id),
  tokens = list(OUTPUT_LABEL = "Table 92", OUTPUT_TITLE = "", OUTPUT_POPULATION = ""),
  file = "output/tfl/T-92.rtf"
) |>
  rtf_section(header = header_standard, footer = footer_standard) |>
  rtf_tables(content)
generate_rtfreport(doc, file.path(path_tfl, paste0(report_id, ".rtf")), overwrite = TRUE)
record_report(report_id)

# the end of this run
options(tflplanner.run = NULL)
