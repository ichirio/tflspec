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

# your code: leaves `data` (or `ard`, normalized below)
ard <- cards::ard_continuous(adsl, variables = AGE)

data <- normalize_ard(ard)

data <- report_data(data, "data")

# ---- content: your code ----------------------------------------------------
# your code: leaves `content`
content <- head(data)

content <- report_content(content)
