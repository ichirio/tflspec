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
