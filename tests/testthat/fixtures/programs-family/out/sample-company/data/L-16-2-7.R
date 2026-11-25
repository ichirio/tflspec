# Setup code: runs first in every ARD and report program of this study.
# What every program needs: the packages, and the options the study shares.
library(cards)
library(dplyr)
library(rtfreporter)

# Warnings are printed as they happen, so the run's log shows where
options(warn = 1)

# ---- input data ----
adae <- readRDS("data/adam/adae.rds")
adae <- blank_as_na(adae)

# ---- data ----
# its records, in its order; dates as they print
data <- adae |>
  order_levels() |>
  dplyr::filter(AESEV == "SEVERE", TRTEMFL == "Y") |>
  dplyr::arrange(TRT01AN, USUBJID, ASTDT) |>
  dplyr::mutate(
    dplyr::across(dplyr::where(~ inherits(.x, c("Date", "POSIXt"))), format)
  )

lst <- listing_spec(
  list(
    listing_col("TRT01A", width = 22, label = "Treatment"),
    listing_col("USUBJID", width = 14, label = "Subject", collapse_repeats = TRUE),
    listing_col(
      c("AEBODSYS", "AEDECOD"),
      width = 40,
      label = "System Organ Class/\nPreferred Term"
    ),
    listing_col(c("ASTDT", "AENDT"), width = 14, label = "Start/\nEnd"),
    listing_col("AESER", width = 8, label = "Serious"),
    listing_col("AEREL", width = 9, label = "Relation"),
    listing_col("AEOUT", width = 24, label = "Outcome")
  ),
  type = "multiline"
)
content <- as_rtftables(data, listing = lst, max_rows = 22)
