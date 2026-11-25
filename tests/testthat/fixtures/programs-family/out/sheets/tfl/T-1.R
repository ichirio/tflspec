# ============================================================================
#  Program    : programs/tfl/T-1.R
#  Output     : T-1 (table) -> T-1.rtf
#  Description: Its own ARD code
#  Generated  : <masked>
#  The data and content parts are your code where its steps are; the report part is written from spec/.
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
# the study's setup, and the header, footer and tokens of every report
source("programs/tfl/report_setup.R")

report_id <- "T-1"

# the figure style of the company standards, the figures' packages, and the figure checks
source("programs/tfl/fig_setup.R")

# ---- data: your code -------------------------------------------------------
# the report's data: ADSL (data catalog)
adsl <- readRDS("data/adam/adsl.rds")
adsl <- blank_as_na(adsl)

# your code: leaves `ard` (normalized in the table's plan) or `data`
ard <- cards::ard_continuous(adsl, variables = AGE)

ard <- report_data(ard, "ard")

# ---- report ----------------------------------------------------------------
# the table, as spec/outputs/T-1.yml defines it
# BIGN: the column headings' N, apart from the table's rows
ard <- tflspec::tfl_bign_drop(ard)

plan <- ard |>
  normalize_ard() |>
  table_plan()

# the report, as spec/outputs/T-1.yml defines it
doc <- rtf_document(program_fallback = "T-1", file = "T-1.rtf") |>
  rtf_tables(plan)
generate_rtfreport(doc, file.path(".", paste0(report_id, ".rtf")), overwrite = TRUE)
record_report(report_id)

# the end of this run
options(tflplanner.run = NULL)
