# ============================================================================
#  Program    : programs/ard/T-14-0-1.R
#  Output     : T-14-0-1 -> its rows of output/ard/ard.rds
#  Analyses   : INFO (Study information)
#  Generated  : <masked>
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
source("programs/ard/ard_setup.R")

report_id <- "T-14-0-1"

# ---- data ----
adsl <- order_levels(adsl)

# ---- analyses ----
# INFO: Study information
facts <- c(
  "MedDRA version" = "Version 26.1",
  "WHODrug version" = "Global B3 March 2023",
  "First dose" = format(min(adsl$TRTSDT, na.rm = TRUE)),
  "Last dose" = format(max(adsl$TRTEDT, na.rm = TRUE)),
  "Data cut-off" = format(max(adsl$DCUTDT, na.rm = TRUE))
)
.ard_info <- as_card(
  tibble(
    variable = "INFO",
    variable_level = as.list(names(facts)),
    context = "study_info",
    stat_name = "value",
    stat_label = "Value",
    stat = as.list(unname(facts)),
    fmt_fun = list(NULL),
    warning = list(NULL),
    error = list(NULL)
  )
) |>
  fmt_ard(fmt_default) |>
  tag_ard(report_id, "INFO")

# the report's rows of the study ARD, with the fingerprint of its
# definition (tfl_ard_spec_hash())
.ard_info |>
  save_ard(report_id, definition = "f36369138dac9176e81537a83a52ec15")

# the end of this run
options(tflplanner.run = NULL)
