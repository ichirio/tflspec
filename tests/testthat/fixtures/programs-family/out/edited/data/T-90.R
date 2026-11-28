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
# your code: leaves `data` (or `ard`, normalized below)
data <- head(adsl)

data <- report_data(data, "data")

# ---- content: your code ----------------------------------------------------
# your code: leaves `content`
plan <- table_plan(data, cols = "ARM")

content <- report_content_check(content, "table", report_id)
