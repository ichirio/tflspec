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
  dplyr::arrange(TRT01A, USUBJID, ASTDT) |>
  dplyr::mutate(
    dplyr::across(dplyr::where(~ inherits(.x, c("Date", "POSIXt"))), format)
  )

lst <- listing_spec(
  list(
    listing_col(
      "TRT01A",
      width = 9,
      label = "Actual Treatment for Period 01",
      collapse_repeats = TRUE
    ),
    listing_col(
      "USUBJID",
      width = 19,
      label = "Unique Subject Identifier",
      collapse_repeats = TRUE
    ),
    listing_col("AEDECOD", width = 18, label = "Dictionary-Derived Term"),
    listing_col("AESEV", width = 18, label = "Severity/Intensity")
  ),
  type = "multiline"
)
content <- as_rtftables(data, listing = lst)
