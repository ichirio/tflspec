# ============================================================================
#  Program    : programs/ard/T-14-1-1S.R
#  Output     : T-14-1-1S -> its rows of output/ard/ard.rds
#  Analyses   : DEMOG (Demographics, run together), CONT (Continuous), CAT
#               (Categorical)
#  Generated  : <masked>
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
source("programs/ard/ard_setup.R")

report_id <- "T-14-1-1S"

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
# DEMOG: Demographics, run together (runs CONT, CAT)
.ard_demog <- adsl_saf |>
  ard_stack(
    .by = TRT01A,
    ard_summary(
      variables = AGE,
      statistic = ~ continuous_summary_fns(
        c("N", "mean", "sd", "median", "min", "max")
      ),
      fmt_fun = everything() ~ fmt_default
    ),
    ard_tabulate(
      variables = c(AGEGR1, SEX, RACE, ETHNIC),
      statistic = ~ c("n", "p"),
      fmt_fun = everything() ~ fmt_default
    )
  ) |>
  fmt_ard(fmt_default, skip = c("AGE", "AGEGR1", "SEX", "RACE", "ETHNIC")) |>
  tag_ard(
    report_id,
    "DEMOG",
    population = "SAF",
    analyses = list(CONT = "AGE", CAT = c("AGEGR1", "SEX", "RACE", "ETHNIC"))
  )

# the report's rows of the study ARD, with the fingerprint of its
# definition (tfl_ard_spec_hash())
.ard_demog |>
  save_ard(report_id, definition = "80a54f66cd47cc62b247ea49a881a102")

# the end of this run
options(tflplanner.run = NULL)
