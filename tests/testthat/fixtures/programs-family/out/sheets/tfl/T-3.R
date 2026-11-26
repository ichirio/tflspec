# ============================================================================
#  Program    : programs/tfl/T-3.R
#  Output     : T-3 (table) -> T-3.rtf
#  Generated  : <masked>
#  The data part makes `data`; the report part is written from spec/.
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
# the study's setup, and the header, footer and tokens of every report
source("programs/tfl/report_setup.R")

report_id <- "T-3"

# ---- data: this report's normalized ARD ------------------------------------
# this report's rows of the study ARD (made by programs/ard/T-3.R)
ard <- readRDS("output/ard/ard.rds") |>
  dplyr::filter(output_id == report_id) |>
  dplyr::select(-c(output_id, analysis_id, population_id))
if (!nrow(ard)) {
  stop("The study ARD has no rows for ", report_id, ": run programs/ard/T-3.R first.")
}

# normalize and rework
data <- ard |> normalize_ard()
data <- data[1:3, ]

# ---- report ----------------------------------------------------------------
stop("tflplanner: the definition of T-3 does not hold: `output_id`: this spec has no row for 'T-3' and no default rows either, so nothing would apply.\n  It defines: 'T-1', 'T-2', 'L-1', 'L-2', 'L-3', 'F-1', 'F-2', 'F-3'")

# the end of this run
options(tflplanner.run = NULL)
