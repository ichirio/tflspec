# ---- input data ----
adsl <- readRDS("data/adam/adsl.rds")
adsl <- blank_as_na(adsl)

# ---- data ----
# its records, in its order; dates as they print
data <- adsl |>
  order_levels() |>
  dplyr::arrange(USUBJID) |>
  dplyr::mutate(
    dplyr::across(dplyr::where(~ inherits(.x, c("Date", "POSIXt"))), format)
  )

lst <- listing_spec(
  list(listing_col("USUBJID"), listing_col("AGE")),
  type = "multiline"
)
content <- as_rtftables(data, listing = lst)
