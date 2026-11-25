# Setup code: runs first in every ARD and report program of this study.
# What every program needs: the packages, and the options the study shares.
library(cards)
library(dplyr)
library(rtfreporter)

# Warnings are printed as they happen, so the run's log shows where
options(warn = 1)

# the figure style of the company standards, the figures' packages, and the figure checks
source("programs/tfl/fig_setup.R")

# ---- data: the figure's data -----------------------------------------------
# the report's data: ADSL (data catalog)
adsl <- readRDS("data/adam/adsl.rds")
adsl <- blank_as_na(adsl)

# the ARD of T-14-1-1 (its rows of the study ARD)
ard <- readRDS("output/ard/ard.rds") |>
  filter(output_id == "T-14-1-1")
if (!nrow(ard)) stop("The study ARD has no rows for T-14-1-1: run programs/ard/T-14-1-1.R first.")
# (the definition it was made from: the report records it, #293)
ard_built <- ard_fingerprint("T-14-1-1")

# ---- content: your code ----------------------------------------------------
# your code: leaves `content`
content <- ggplot2::ggplot(adsl)

# the figure drawn at its size on the page: 6.5 x 4.5 in (its own size), 300 dpi
content <- fig_sized(content, width_in = 6.5, height_in = 4.5, dpi = 300)
content <- report_content(content)
