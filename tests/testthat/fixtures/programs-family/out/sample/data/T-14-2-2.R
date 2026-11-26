# Setup code: runs first in every ARD and report program of this study.
# What every program needs: the packages, and the options the study shares.
library(cards)
library(dplyr)
library(rtfreporter)

# Warnings are printed as they happen, so the run's log shows where
options(warn = 1)

# this report's rows of the study ARD (made by programs/ard/T-14-2-2.R)
ard <- readRDS("output/ard/ard.rds") |>
  dplyr::filter(output_id == report_id) |>
  dplyr::select(-c(output_id, analysis_id, population_id))
if (!nrow(ard)) {
  stop("The study ARD has no rows for ", report_id, ": run programs/ard/T-14-2-2.R first.")
}

# normalize and rework
data <- ard |> normalize_ard()
# row labels: the median, and the event-free probability by day (day 0 left out)
is_time <- data$variable == "time"
data$.label[data$variable == "prob"] <- "Median (95% CI)"
data$.label[is_time] <- paste("Day", as.character(data$variable_level[is_time]))
data <- data[!(is_time & as.character(data$variable_level) == "0"), ]
