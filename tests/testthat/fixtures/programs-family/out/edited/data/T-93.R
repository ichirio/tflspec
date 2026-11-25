# Setup code: runs first in every ARD and report program of this study.
# What every program needs: the packages, and the options the study shares.
library(cards)
library(dplyr)
library(rtfreporter)

# Warnings are printed as they happen, so the run's log shows where
options(warn = 1)

# ---- this output's ARD, made elsewhere and taken in (input/ard/t93.rds)
ard <- tflspec::tfl_read_ard("input/ard/t93.rds")
if ("output_id" %in% names(ard)) ard <- ard[ard$output_id == "T-93", , drop = FALSE]
ard <- ard[setdiff(names(ard), c("output_id", "analysis_id", "population_id"))]
if (!nrow(ard)) stop("The ARD taken in has no rows for T-93.")
