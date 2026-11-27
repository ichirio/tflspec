# ============================================================================
#  Program    : programs/tfl/F-9.R
#  Output     : F-9 (figure) -> output/tfl/F-9.rtf
#  Description: KM by arm
#  Generated  : <masked>
#  The data and content parts make `content` (tflspec's figure function, called as the report's `figure:` says); the report part is written from spec/.
#  Edit its `figure:` key in spec/outputs/F-9.yml, not this code.
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
# the study's setup, and the header, footer and tokens of every report
source("programs/tfl/report_setup.R")
# the figure style of the company standards, the figures' packages, and the figure checks
source("programs/tfl/fig_setup.R")

report_id <- "F-9"

# Setup code: runs first in every ARD and report program of this study.
# What every program needs: the packages, and the options the study shares.
library(cards)
library(dplyr)
library(rtfreporter)

# Warnings are printed as they happen, so the run's log shows where
options(warn = 1)

# the data: ADSL, ADTTE (data catalog)

# the report's analysis data (its ARD definition)
# ---- data ----
adsl <- order_levels(adsl)
adtte <- order_levels(adtte, from = adsl)

# adsl_sub: the subjects. population SAF (Safety Analysis Set)
adsl_sub <- filter(adsl, SAFFL == "Y")
# adtte_sub: the records. ADTTE of the subjects of adsl_sub, TRT01A from ADSL
adtte_sub <- adtte |>
  select(-any_of("TRT01A")) |>
  inner_join(
    select(adsl_sub, USUBJID, TRT01A),
    by = "USUBJID",
    relationship = "many-to-one"
  ) |>
  filter(PARAMCD == "OS") |>
  mutate(AVAL = AVAL / c(days = 1, months = 30.4375)[["months"]])

# this figure's ARD (its rows of the study ARD)
ard <- readRDS(file.path(path_ard, "ard.rds")) |>
  filter(output_id == "F-9")
if (!nrow(ard)) stop("The study ARD has no rows for F-9: run programs/ard/F-9.R first.")
# (the definition it was made from: the report records it, #293)
ard_built <- ard_fingerprint("F-9")

# the figure's key does not hold:
stop("tfl_resolve_output(): `output` is a tfl_output.")

# ---- report ----------------------------------------------------------------
# the report, as spec/outputs/F-9.yml defines it
doc <- rtf_document(
  program_fallback = file.path(path_programs_tfl, report_id),
  tokens = list(OUTPUT_LABEL = "Figure 9", OUTPUT_TITLE = "", OUTPUT_POPULATION = ""),
  file = "output/tfl/F-9.rtf"
) |>
  rtf_section(header = header_standard, footer = footer_standard) |>
  rtf_figures(plot)
generate_rtfreport(doc, file.path(path_tfl, paste0(report_id, ".rtf")), overwrite = TRUE)
record_report(report_id)

# the end of this run
options(tflplanner.run = NULL)
