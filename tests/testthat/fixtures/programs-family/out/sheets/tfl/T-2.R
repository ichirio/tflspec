# ============================================================================
#  Program    : programs/tfl/T-2.R
#  Output     : T-2 (table) -> T-2.rtf
#  Generated  : <masked>
#  The data and content parts are your code where its steps are; the report part is written from spec/.
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
# the study's setup, and the header, footer and tokens of every report
source("programs/tfl/report_setup.R")

report_id <- "T-2"

# the figure style of the company standards, the figures' packages, and the figure checks
source("programs/tfl/fig_setup.R")

# ---- data: your code -------------------------------------------------------
# the report's data: ADSL (data catalog)
adsl <- readRDS("data/adam/adsl.rds")
adsl <- blank_as_na(adsl)

# your code: leaves `data` (or `ard`, normalized below)
ard <- cards::ard_continuous(adsl, variables = AGE)

data <- normalize_ard(ard)

data <- report_data(data, "data")

# ---- content: your code ----------------------------------------------------
# your code: leaves `content`
content <- head(data)

content <- report_content(content)

# ---- report ----------------------------------------------------------------
# the report, as spec/outputs/T-2.yml defines it
doc <- rtf_document(program_fallback = "T-2", file = "T-2.rtf") |>
  rtf_tables(content)
generate_rtfreport(doc, file.path(".", paste0(report_id, ".rtf")), overwrite = TRUE)
record_report(report_id)

# the end of this run
options(tflplanner.run = NULL)
