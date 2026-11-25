# ============================================================================
#  Program    : programs/tfl/L-2.R
#  Output     : L-2 (listing) -> L-2.rtf
#  Generated  : <masked>
#  The data part makes `content`; the report part is written from spec/.
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
# the study's setup, and the header, footer and tokens of every report
source("programs/tfl/report_setup.R")

report_id <- "L-2"

# ---- data: the listing's pages ---------------------------------------------
# ---- input data ----
adsl <- readRDS("data/adam/adsl.rds")
adsl <- blank_as_na(adsl)

# ---- data ----
# its records, in its order; dates as they print
data <- adsl |>
  order_levels() |>
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
# the report, as spec/outputs/L-2.yml defines it
doc <- rtf_document(program_fallback = "L-2", file = "L-2.rtf") |>
  rtf_tables(content)
generate_rtfreport(doc, file.path(".", paste0(report_id, ".rtf")), overwrite = TRUE)
record_report(report_id)

# the end of this run
options(tflplanner.run = NULL)
