# the figure style of the company standards, the figures' packages, and the figure checks
source("programs/tfl/fig_setup.R")

# ---- data: your code -------------------------------------------------------
# the report's data: ADSL (data catalog)
adsl <- readRDS("data/adam/adsl.rds")
adsl <- blank_as_na(adsl)

# your code: leaves `ard` (normalized in the table's plan) or `data`
ard <- cards::ard_continuous(adsl, variables = AGE)

ard <- report_data(ard, "ard")
