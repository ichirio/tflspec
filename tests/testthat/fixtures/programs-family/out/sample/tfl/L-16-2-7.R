# ============================================================================
#  Program    : programs/tfl/L-16-2-7.R
#  Output     : L-16-2-7 (listing) -> output/tfl/L-16-2-7.rtf
#  Description: Listing of severe treatment-emergent adverse events
#  Generated  : <masked>
#  The data part makes `content`; the report part is written from spec/.
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
# the study's setup, and the header, footer and tokens of every report
source("programs/tfl/report_setup.R")

report_id <- "L-16-2-7"

# ---- data: the listing's pages ---------------------------------------------
# Setup code: runs first in every ARD and report program of this study.
# What every program needs: the packages, and the options the study shares.
library(cards)
library(dplyr)
library(rtfreporter)

# Warnings are printed as they happen, so the run's log shows where
options(warn = 1)

# ---- data ----
adsl <- order_levels(adsl)
adae <- order_levels(adae, from = adsl)

# adsl_sub: the subjects. population SAF (Safety Analysis Set)
adsl_sub <- filter(adsl, SAFFL == "Y")

# its records, in its order; dates as they print
data <- adae |>
  inner_join(
    select(adsl_sub, USUBJID),
    by = "USUBJID",
    relationship = "many-to-one"
  ) |>
  filter(AESEV == "SEVERE", TRTEMFL == "Y") |>
  arrange(TRT01AN, USUBJID, ASTDT) |>
  mutate(across(where(~ inherits(.x, c("Date", "POSIXt"))), format))

lst <- listing_spec(
  list(
    listing_col("TRT01A", width = 22, label = "Treatment"),
    listing_col("USUBJID", width = 14, label = "Subject", collapse_repeats = TRUE),
    listing_col(
      c("AEBODSYS", "AEDECOD"),
      width = 40,
      label = "System Organ Class/\nPreferred Term"
    ),
    listing_col(c("ASTDT", "AENDT"), width = 14, label = "Start/\nEnd"),
    listing_col("AESER", width = 8, label = "Serious"),
    listing_col("AEREL", width = 9, label = "Relation"),
    listing_col("AEOUT", width = 24, label = "Outcome")
  ),
  type = "multiline"
)
content <- as_rtftables(data, listing = lst, max_rows = 22)

# ---- report ----------------------------------------------------------------
# the report, as spec/outputs/L-16-2-7.yml defines it
doc <- rtf_document(
  page = list(orientation = "landscape"),
  program_fallback = file.path(path_programs_tfl, report_id),
  tokens = list(
    OUTPUT_LABEL = "Listing 16.2.7",
    OUTPUT_TITLE = "Severe Treatment-Emergent Adverse Events",
    OUTPUT_POPULATION = "Safety Analysis Set"
  ),
  file = "output/tfl/L-16-2-7.rtf"
) |>
  rtf_section(header = header_standard, footer = footer_standard) |>
  rtf_tables(content)
generate_rtfreport(doc, file.path(path_tfl, paste0(report_id, ".rtf")), overwrite = TRUE)
record_report(report_id, empty = nrow(data) == 0)

# the end of this run
options(tflplanner.run = NULL)
