# ============================================================================
#  Program    : programs/ard/T-14-1-5.R
#  Output     : T-14-1-5 -> its rows of output/ard/ard.rds
#  Analyses   : BIGN, RACE (Race), ASIANSUB (Asian sub-category), CONT
#               (Continuous), CAT (Categorical)
#  Generated  : <masked>
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
source("programs/ard/ard_setup.R")

report_id <- "T-14-1-5"

# ---- code lists: each value in its order, as it is in the ARD ----
# (a value a list does not have stops the program: add it to the list)
cl_armgrp <- c("Drug X", "Control", "Screen Failure")
cl_race <- c(
  WHITE = "White",
  "BLACK OR AFRICAN AMERICAN" = "Black or African American",
  ASIAN = "Asian",
  "AMERICAN INDIAN OR ALASKA NATIVE" = "American Indian or Alaska Native",
  "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" = "Native Hawaiian or Other Pacific Islander",
  OTHER = "Other"
)
cl_asiansub <- c(CHINESE = "Chinese", JAPANESE = "Japanese", KOREAN = "Korean")
cl_sex <- c(F = "Female", M = "Male")
cl_agegr1 <- c("<65" = "<65 years", ">=65" = ">=65 years")
cl_ethnic <- c(
  "HISPANIC OR LATINO" = "Hispanic or Latino",
  "NOT HISPANIC OR LATINO" = "Not Hispanic or Latino",
  "NOT REPORTED" = "Not reported",
  UNKNOWN = "Unknown"
)

# ---- data ----
adsl <- order_levels(adsl)

# adsl_sub: the subjects. population ENR (Screened Set)
adsl_sub <- adsl

# adsl_enr: the subjects. population ENR (Screened Set)
adsl_enr <- adsl_sub |>
  mutate(
    ARMGRP = if_else(
      ARMNRS %in% "SCREEN FAILURE",
      "Screen Failure",
      as.character(ARM)
    ),
    ASIANSUB = case_when(
      RACE == "ASIAN" & COUNTRY == "JPN" ~ "JAPANESE",
      RACE == "ASIAN" & COUNTRY == "KOR" ~ "KOREAN"
    )
  ) |>
  set_levels(
    ARMGRP = cl_armgrp,
    RACE = cl_race,
    ASIANSUB = cl_asiansub,
    SEX = cl_sex,
    AGEGR1 = cl_agegr1,
    ETHNIC = cl_ethnic
  )

# ---- analyses ----
# BIGN
.ard_bign <- adsl_enr |>
  mutate(across(where(is.character), ~ na_if(.x, ""))) |>
  ard_tabulate(variables = ARMGRP, fmt_fun = everything() ~ fmt_default) |>
  apply_fmt_fun() |>
  tag_ard(report_id, "BIGN", population = "ENR")

# RACE: Race
# (and the Total column: the same call without `by`)
.ard_race <- bind_rows(
  adsl_enr |>
    mutate(across(where(is.character), ~ na_if(.x, ""))) |>
    ard_tabulate(
      by = ARMGRP,
      variables = RACE,
      denominator = adsl_enr,
      statistic = ~ c("n", "p"),
      fmt_fun = everything() ~ fmt_default
    ),
  adsl_enr |>
    mutate(across(where(is.character), ~ na_if(.x, ""))) |>
    ard_tabulate(
      variables = RACE,
      denominator = adsl_enr,
      statistic = ~ c("n", "p"),
      fmt_fun = everything() ~ fmt_default
    )
) |>
  apply_fmt_fun() |>
  tag_ard(report_id, "RACE", population = "ENR")

# ASIANSUB: Asian sub-category
# (and the Total column: the same call without `by`)
.ard_asiansub <- bind_rows(
  adsl_enr |>
    mutate(across(where(is.character), ~ na_if(.x, ""))) |>
    ard_tabulate(
      by = ARMGRP,
      variables = ASIANSUB,
      denominator = adsl_enr,
      statistic = ~ c("n", "p"),
      fmt_fun = everything() ~ fmt_default
    ),
  adsl_enr |>
    mutate(across(where(is.character), ~ na_if(.x, ""))) |>
    ard_tabulate(
      variables = ASIANSUB,
      denominator = adsl_enr,
      statistic = ~ c("n", "p"),
      fmt_fun = everything() ~ fmt_default
    )
) |>
  apply_fmt_fun() |>
  tag_ard(report_id, "ASIANSUB", population = "ENR")

# CONT: Continuous
# (and the Total column: the same call without `by`)
.ard_cont <- bind_rows(
  adsl_enr |>
    ard_summary(
      by = ARMGRP,
      variables = AGE,
      statistic = ~ continuous_summary_fns(
        c("N", "mean", "sd", "median", "min", "max")
      ),
      fmt_fun = everything() ~ fmt_default
    ),
  adsl_enr |>
    ard_summary(
      variables = AGE,
      statistic = ~ continuous_summary_fns(
        c("N", "mean", "sd", "median", "min", "max")
      ),
      fmt_fun = everything() ~ fmt_default
    )
) |>
  apply_fmt_fun() |>
  tag_ard(report_id, "CONT", population = "ENR")

# CAT: Categorical
# (and the Total column: the same call without `by`)
.ard_cat <- bind_rows(
  adsl_enr |>
    mutate(across(where(is.character), ~ na_if(.x, ""))) |>
    ard_tabulate(
      by = ARMGRP,
      variables = c(AGEGR1, SEX, ETHNIC),
      denominator = adsl_enr,
      statistic = ~ c("n", "p"),
      fmt_fun = everything() ~ fmt_default
    ),
  adsl_enr |>
    mutate(across(where(is.character), ~ na_if(.x, ""))) |>
    ard_tabulate(
      variables = c(AGEGR1, SEX, ETHNIC),
      denominator = adsl_enr,
      statistic = ~ c("n", "p"),
      fmt_fun = everything() ~ fmt_default
    )
) |>
  apply_fmt_fun() |>
  tag_ard(report_id, "CAT", population = "ENR")

# the report's rows of the study ARD, with the fingerprint of its
# definition (tfl_ard_spec_hash())
bind_items(.ard_bign, .ard_race, .ard_asiansub, .ard_cont, .ard_cat) |>
  save_ard(report_id, definition = "2e5c765e96627beb1e02fa9943b71fd5")

# the end of this run
options(tflplanner.run = NULL)
