# ============================================================================
#  Program    : programs/tfl/F-3.R
#  Output     : F-3 (figure) -> F-3.rtf
#  Generated  : <masked>
#  The data part makes `content`; the report part is written from spec/.
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
# the study's setup, and the header, footer and tokens of every report
source("programs/tfl/report_setup.R")

report_id <- "F-3"

# ---- data: the figure ------------------------------------------------------
# TODO: make the figure(s), e.g.
#   content <- list(ggplot2::ggplot(adsl, ggplot2::aes(AGE)) +
#                     ggplot2::geom_histogram())
stop("tflplanner: the data part of F-3.R is still to be written.")

# ---- report ----------------------------------------------------------------
# the report, as spec/outputs/F-3.yml defines it
doc <- rtf_document(program_fallback = "F-3", file = "F-3.rtf") |>
  rtf_figures(plot)
generate_rtfreport(doc, file.path(".", paste0(report_id, ".rtf")), overwrite = TRUE)
record_report(report_id)

# the end of this run
options(tflplanner.run = NULL)
