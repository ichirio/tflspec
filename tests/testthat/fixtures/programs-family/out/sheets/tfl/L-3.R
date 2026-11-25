# ============================================================================
#  Program    : programs/tfl/L-3.R
#  Output     : L-3 (listing) -> L-3.rtf
#  Generated  : <masked>
#  The data part makes `content`; the report part is written from spec/.
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
# the study's setup, and the header, footer and tokens of every report
source("programs/tfl/report_setup.R")

report_id <- "L-3"

# ---- data: the listing's pages ---------------------------------------------
# TODO: make the listing pages, e.g.
#   adae    <- readRDS("data/adam/adae.rds")
#   content <- as_rtftables(adae[, c("USUBJID", "AEDECOD")])
stop("tflplanner: the data part of L-3.R is still to be written.")

# ---- report ----------------------------------------------------------------
# the report, as spec/outputs/L-3.yml defines it
doc <- rtf_document(program_fallback = "L-3", file = "L-3.rtf") |>
  rtf_tables(content)
generate_rtfreport(doc, file.path(".", paste0(report_id, ".rtf")), overwrite = TRUE)
record_report(report_id)

# the end of this run
options(tflplanner.run = NULL)
