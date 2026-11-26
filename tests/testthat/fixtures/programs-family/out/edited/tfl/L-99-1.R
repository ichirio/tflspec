# ============================================================================
#  Program    : programs/tfl/L-99-1.R
#  Output     : L-99-1 (listing) -> output/tfl/L-99-1.rtf
#  Generated  : <masked>
#  The data part makes `content`; the report part is written from spec/.
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
# the study's setup, and the header, footer and tokens of every report
source("programs/tfl/report_setup.R")

report_id <- "L-99-1"

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
  arrange(TRT01A, USUBJID, ASTDT) |>
  mutate(across(where(~ inherits(.x, c("Date", "POSIXt"))), format))

lst <- listing_spec(
  list(
    listing_col(
      "TRT01A",
      width = 9,
      label = "Actual Treatment for Period 01",
      collapse_repeats = TRUE
    ),
    listing_col(
      "USUBJID",
      width = 19,
      label = "Unique Subject Identifier",
      collapse_repeats = TRUE
    ),
    listing_col("AEDECOD", width = 18, label = "Dictionary-Derived Term"),
    listing_col("AESEV", width = 18, label = "Severity/Intensity")
  ),
  type = "multiline"
)
content <- as_rtftables(data, listing = lst)

# ---- report ----------------------------------------------------------------
# the report, as spec/outputs/L-99-1.yml defines it
doc <- rtf_document(
  program_fallback = file.path(path_programs_tfl, report_id),
  tokens = list(OUTPUT_LABEL = "Listing 99.1", OUTPUT_TITLE = "", OUTPUT_POPULATION = ""),
  file = "output/tfl/L-99-1.rtf"
) |>
  rtf_section(header = header_standard, footer = footer_standard) |>
  rtf_tables(content)
generate_rtfreport(doc, file.path(path_tfl, paste0(report_id, ".rtf")), overwrite = TRUE)
record_report(report_id, empty = nrow(data) == 0)

# the end of this run
options(tflplanner.run = NULL)
