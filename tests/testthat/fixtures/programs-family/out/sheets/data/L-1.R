# the figure style of the company standards, the figures' packages, and the figure checks
source("programs/tfl/fig_setup.R")

# ---- data: your code -------------------------------------------------------
# the report's data: ADSL (data catalog)
adsl <- readRDS("data/adam/adsl.rds")
adsl <- blank_as_na(adsl)

# your code: leaves `data`
data <- adsl[adsl$AGE > 60, ]

data <- report_data(data, "data")

# ---- content: the listing --------------------------------------------------
# its order; dates as they print
data <- data |>
  dplyr::arrange(USUBJID) |>
  dplyr::mutate(
    dplyr::across(dplyr::where(~ inherits(.x, c("Date", "POSIXt"))), format)
  )

lst <- listing_spec(
  list(listing_col("USUBJID"), listing_col("AGE")),
  type = "multiline"
)
content <- as_rtftables(data, listing = lst)
