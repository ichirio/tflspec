# Setup code: runs first in every ARD and report program of this study.
# What every program needs: the packages, and the options the study shares.
library(cards)
library(dplyr)
library(rtfreporter)

# Warnings are printed as they happen, so the run's log shows where
options(warn = 1)

# the figure style of the company standards, the figures' packages, and the figure checks
source("programs/tfl/fig_setup.R")

# the data: ADSL, ADTTE (data catalog)
adsl <- readRDS("data/adam/adsl.rds")
adsl <- blank_as_na(adsl)
adtte <- readRDS("data/adam/adtte.rds")
adtte <- blank_as_na(adtte)

# the report's analysis data (its ARD definition)
# ---- input data ----
adsl <- readRDS("data/adam/adsl.rds") |>
  blank_as_na() |>
  order_levels()
adtte <- readRDS("data/adam/adtte.rds") |>
  blank_as_na() |>
  order_levels(from = adsl)

# ---- data ----
# adsl_sub: the subjects. population SAF (Safety Analysis Set)
adsl_sub <- filter(adsl, SAFFL == "Y")
# adtte_sub: the records. ADTTE of the subjects of adsl_sub, TRT01A from ADSL;
# where
adtte_sub <- adtte |>
  select(-any_of("TRT01A")) |>
  inner_join(
    select(adsl_sub, USUBJID, TRT01A),
    by = "USUBJID",
    relationship = "many-to-one"
  ) |>
  filter(PARAMCD == "OS") |>
  mutate(AVAL = AVAL / c(days = 1, months = 30.4375)[["months"]])

# this figure's ARD (its rows of the study ARD)
ard <- readRDS("output/ard/ard.rds") |>
  filter(output_id == "F-9")
if (!nrow(ard)) stop("The study ARD has no rows for F-9: run programs/ard/F-9.R first.")
# (the definition it was made from: the report records it, #293)
ard_built <- ard_fingerprint("F-9")

# the figure's key does not hold:
stop("tfl_resolve_output(): `output` is a tfl_output.")
