# ============================================================================
#  Program    : programs/ard/T-14-1-4.R
#  Output     : T-14-1-4 -> its rows of output/ard/ard.rds
#  Analyses   : BIGN, CONT (Continuous), CAT (Categorical)
#  Generated  : <masked>
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
source("programs/ard/ard_setup.R")

report_id <- "T-14-1-4"

# ---- code lists: each value in its order, as it is in the ARD ----
# (a value a list does not have stops the program: add it to the list)
cl_armnrs <- c("SCREEN FAILURE" = "Screen Failures")
cl_sex <- c(F = "Female", M = "Male")
cl_agegr1 <- c("<65" = "<65 years", ">=65" = ">=65 years")
cl_race <- c(
  WHITE = "White",
  "BLACK OR AFRICAN AMERICAN" = "Black or African American",
  ASIAN = "Asian",
  "AMERICAN INDIAN OR ALASKA NATIVE" = "American Indian or Alaska Native",
  "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" = "Native Hawaiian or Other Pacific Islander",
  OTHER = "Other"
)

# ---- data ----
adsl <- order_levels(adsl)

# adsl_scrf: the subjects. population SCRF (Screen Failures)
adsl_scrf <- adsl |>
  filter(ARMNRS == "SCREEN FAILURE") |>
  set_levels(
    ARMNRS = cl_armnrs,
    SEX = cl_sex,
    AGEGR1 = cl_agegr1,
    RACE = cl_race
  )

# ---- analyses ----
# BIGN
.ard_bign <- adsl_scrf |>
  mutate(across(where(is.character), ~ na_if(.x, ""))) |>
  ard_tabulate(variables = ARMNRS, fmt_fun = everything() ~ fmt_default) |>
  apply_fmt_fun() |>
  tag_ard(report_id, "BIGN", population = "SCRF")

# CONT: Continuous
.ard_cont <- adsl_scrf |>
  ard_summary(
    by = ARMNRS,
    variables = AGE,
    statistic = ~ continuous_summary_fns(
      c("N", "mean", "sd", "median", "min", "max")
    ),
    fmt_fun = everything() ~ fmt_default
  ) |>
  apply_fmt_fun() |>
  tag_ard(report_id, "CONT", population = "SCRF")

# CAT: Categorical
.ard_cat <- adsl_scrf |>
  mutate(across(where(is.character), ~ na_if(.x, ""))) |>
  ard_tabulate(
    by = ARMNRS,
    variables = c(AGEGR1, SEX, RACE),
    statistic = ~ c("n", "p"),
    fmt_fun = everything() ~ fmt_default
  ) |>
  apply_fmt_fun() |>
  tag_ard(report_id, "CAT", population = "SCRF")

# the report's rows of the study ARD, with the fingerprint of its
# definition (tfl_ard_spec_hash())
bind_items(.ard_bign, .ard_cont, .ard_cat) |>
  save_ard(report_id, definition = "f581e5b2bdea4abbcb1a4758464f0cc5")

# the end of this run
options(tflplanner.run = NULL)
