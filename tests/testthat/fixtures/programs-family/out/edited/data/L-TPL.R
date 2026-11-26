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
  dplyr::mutate(
    dplyr::across(dplyr::where(~ inherits(.x, c("Date", "POSIXt"))), format)
  )

lst <- listing_spec(
  list(
    listing_col("USUBJID", label = "Subject"),
    listing_col("AEDECOD", width = 30, label = "Term")
  ),
  type = "multiline"
)
content <- as_rtftables(data, listing = lst, max_rows = 22)
