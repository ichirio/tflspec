# Setup code: runs first in every ARD and report program of this study.
# What every program needs: the packages, and the options the study shares.
library(cards)
library(dplyr)
library(rtfreporter)

# Warnings are printed as they happen, so the run's log shows where
options(warn = 1)

# the figure style of the company standards, the figures' packages, and the figure checks
source("programs/tfl/fig_setup.R")

# ---- data: your code -------------------------------------------------------
# the report's data: ADSL (data catalog)
adsl <- readRDS("data/adam/adsl.rds")
adsl <- blank_as_na(adsl)

# this figure's ARD (its rows of the study ARD)
ard <- readRDS("output/ard/ard.rds") |>
  filter(output_id == "F-91")
if (!nrow(ard)) stop("The study ARD has no rows for F-91: run programs/ard/F-91.R first.")
# (the definition it was made from: the report records it, #293)
ard_built <- ard_fingerprint("F-91")

# your code: leaves `df`
df <- adsl

df <- report_data(df, "df")

# ---- content: the figure ---------------------------------------------------
stop("tflplanner: the figure of F-91.R is still to be written.")
