# ============================================================================
#  Program    : programs/ard/T-14-1-1.R
#  Output     : T-14-1-1 -> its rows of output/ard/ard.rds
#  Analyses   : BIGN (Subjects per group), CONT (Continuous), CAT
#               (Categorical)
#  Generated  : <masked>
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
source("programs/ard/ard_setup.R")

report_id <- "T-14-1-1"

# ---- code lists: each value in its order, as it is in the ARD ----
# (a value a list does not have stops the program: add it to the list)
cl_trt01a <- c("Drug X", "Control")
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
cl_ethnic <- c(
  "HISPANIC OR LATINO" = "Hispanic or Latino",
  "NOT HISPANIC OR LATINO" = "Not Hispanic or Latino",
  "NOT REPORTED" = "Not reported",
  UNKNOWN = "Unknown"
)

# ---- data ----
adsl <- order_levels(adsl)

# adsl_saf: the subjects. population SAF (Safety Analysis Set)
adsl_saf <- adsl |>
  filter(SAFFL == "Y") |>
  set_levels(
    TRT01A = cl_trt01a,
    SEX = cl_sex,
    AGEGR1 = cl_agegr1,
    RACE = cl_race,
    ETHNIC = cl_ethnic
  )

# ---- analyses ----
# BIGN: Subjects per group
.ard_bign <- adsl_saf |>
  mutate(across(where(is.character), ~ na_if(.x, ""))) |>
  ard_tabulate(variables = TRT01A, fmt_fun = everything() ~ fmt_default) |>
  apply_fmt_fun() |>
  tag_ard(report_id, "BIGN", population = "SAF")

# CONT: Continuous
.ard_cont <- adsl_saf |>
  ard_summary(
    by = TRT01A,
    variables = AGE,
    statistic = ~ continuous_summary_fns(
      c("N", "mean", "sd", "median", "min", "max")
    ),
    fmt_fun = everything() ~ fmt_default
  ) |>
  apply_fmt_fun() |>
  tag_ard(report_id, "CONT", population = "SAF")

# CAT: Categorical
.ard_cat <- adsl_saf |>
  mutate(across(where(is.character), ~ na_if(.x, ""))) |>
  ard_tabulate(
    by = TRT01A,
    variables = c(AGEGR1, SEX, RACE, ETHNIC),
    statistic = ~ c("n", "p"),
    fmt_fun = everything() ~ fmt_default
  ) |>
  apply_fmt_fun() |>
  tag_ard(report_id, "CAT", population = "SAF")

# the report's rows of the study ARD, with the fingerprint of its
# definition (tfl_ard_spec_hash())
bind_items(.ard_bign, .ard_cont, .ard_cat) |>
  save_ard(report_id, definition = "2c0946d99b113d81dd85d0df0477b25f")

# the end of this run
options(tflplanner.run = NULL)
