# ============================================================================
#  Program    : programs/tfl/F-2.R
#  Output     : F-2 (figure) -> F-2.rtf
#  Generated  : <masked>
#  The data part makes `content`; the report part is written from spec/.
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
# the study's setup, and the header, footer and tokens of every report
source("programs/tfl/report_setup.R")

report_id <- "F-2"

# ---- data: the figure ------------------------------------------------------
# the data: ADSL (data catalog)
adsl <- readRDS("data/adam/adsl.rds")
adsl <- blank_as_na(adsl)

# the figure style of the company standards, and the figure checks
source("programs/tfl/fig_setup.R")

# ---- the plot (ggplot2), written by hand: leaves `plot`
#      (theme_tfl(), scale_colour_tfl(), tfl_marker() ... give the standard's look)
# TODO: the plot (ggplot2), in the figure style of the company standards, e.g.
#   plot <- ggplot2::ggplot(adsl, ggplot2::aes(AGE, fill = TRT01A)) +
#     ggplot2::geom_histogram() + scale_fill_tfl("treatment", levels(adsl$TRT01A)) +
#     theme_tfl()
stop("tflplanner: the plot of F-2.R is still to be written.")

# the figure checks: dropped rows, colours against the standard
tfl_check(plot)

# ---- report ----------------------------------------------------------------
# the report, as spec/outputs/F-2.yml defines it
doc <- rtf_document(program_fallback = "F-2", file = "F-2.rtf") |>
  rtf_figures(plot)
generate_rtfreport(doc, file.path(".", paste0(report_id, ".rtf")), overwrite = TRUE)
record_report(report_id)

# the end of this run
options(tflplanner.run = NULL)
