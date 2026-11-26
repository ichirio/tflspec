# ============================================================================
#  Program    : programs/tfl/L-1.R
#  Output     : L-1 (listing) -> L-1.rtf
#  Generated  : <masked>
#  The data and content parts are your code where its steps are; the report part is written from spec/.
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
# the study's setup, and the header, footer and tokens of every report
source("programs/tfl/report_setup.R")

report_id <- "L-1"

# the figure style of the company standards, the figures' packages, and the figure checks
source("programs/tfl/fig_setup.R")

# ---- data: your code -------------------------------------------------------
# the report's data: ADSL (data catalog)
adsl <- readRDS("data/adam/adsl.rds")
adsl <- blank_as_na(adsl)

# your code: leaves `data`
data <- adsl[adsl$AGE > 60, ]

data <- report_data(data, "data")

# ---- content: the listing --------------------------------------------------
# its order; dates as they print
data <- data |>
  dplyr::arrange(USUBJID) |>
  dplyr::mutate(
    dplyr::across(dplyr::where(~ inherits(.x, c("Date", "POSIXt"))), format)
  )

lst <- listing_spec(
  list(listing_col("USUBJID"), listing_col("AGE")),
  type = "multiline"
)
content <- as_rtftables(data, listing = lst)

# ---- report ----------------------------------------------------------------
# the report, as spec/outputs/L-1.yml defines it
doc <- rtf_document(program_fallback = "L-1", file = "L-1.rtf") |>
  rtf_tables(content)
generate_rtfreport(doc, file.path(".", paste0(report_id, ".rtf")), overwrite = TRUE)
record_report(report_id, empty = nrow(data) == 0)

# the end of this run
options(tflplanner.run = NULL)
