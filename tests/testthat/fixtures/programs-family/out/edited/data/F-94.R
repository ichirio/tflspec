# Setup code: runs first in every ARD and report program of this study.
# What every program needs: the packages, and the options the study shares.
library(cards)
library(dplyr)
library(rtfreporter)

# Warnings are printed as they happen, so the run's log shows where
options(warn = 1)

# the data: ADSL, ADVS (data catalog)
adsl <- readRDS("data/adam/adsl.rds")
adsl <- blank_as_na(adsl)
advs <- readRDS("data/adam/advs.rds")
advs <- blank_as_na(advs)

# the figure style of the company standards, and the figure checks
source("programs/tfl/fig_setup.R")

# ---- the plot (ggplot2), written by hand: leaves `plot`
#      (theme_tfl(), scale_colour_tfl(), tfl_marker() ... give the standard's look)
# TODO: the plot (ggplot2), in the figure style of the company standards, e.g.
#   plot <- ggplot2::ggplot(adsl, ggplot2::aes(AGE, fill = TRT01A)) +
#     ggplot2::geom_histogram() + scale_fill_tfl("treatment", levels(adsl$TRT01A)) +
#     theme_tfl()
stop("tflplanner: the plot of F-94.R is still to be written.")

# the figure checks: dropped rows, colours against the standard
tfl_check(plot)
