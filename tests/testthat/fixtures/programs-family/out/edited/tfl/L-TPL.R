# ============================================================================
#  Program    : programs/tfl/L-TPL.R
#  Output     : L-TPL (listing) -> output/tfl/L-TPL.rtf
#  Description: AE listing
#  Generated  : <masked>
#  The data part makes `content`; the report part is written from spec/.
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
# the study's setup, and the header, footer and tokens of every report
source("programs/tfl/report_setup.R")

report_id <- "L-TPL"

# ---- data: the listing's pages ---------------------------------------------
# Setup code: runs first in every ARD and report program of this study.
# What every program needs: the packages, and the options the study shares.
library(cards)
library(dplyr)
library(rtfreporter)

# Warnings are printed as they happen, so the run's log shows where
options(warn = 1)

# ---- data ----
# its records, in its order; dates as they print
data <- adae |>
  order_levels() |>
  mutate(across(where(~ inherits(.x, c("Date", "POSIXt"))), format))

lst <- listing_spec(
  list(
    listing_col("USUBJID", label = "Subject"),
    listing_col("AEDECOD", width = 30, label = "Term")
  ),
  type = "multiline"
)
content <- as_rtftables(data, listing = lst, max_rows = 22)

# ---- report ----------------------------------------------------------------
# the report, as spec/outputs/L-TPL.yml defines it
doc <- rtf_document(
  program_fallback = file.path(path_programs_tfl, report_id),
  tokens = list(OUTPUT_LABEL = "L-TPL", OUTPUT_TITLE = "", OUTPUT_POPULATION = ""),
  file = "output/tfl/L-TPL.rtf"
) |>
  rtf_section(header = header_standard, footer = footer_standard) |>
  rtf_tables(content)
generate_rtfreport(doc, file.path(path_tfl, paste0(report_id, ".rtf")), overwrite = TRUE)
record_report(report_id, empty = nrow(data) == 0)

# the end of this run
options(tflplanner.run = NULL)
