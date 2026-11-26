# ============================================================================
#  Program    : programs/ard/T-14-1-1C.R
#  Output     : T-14-1-1C -> its rows of output/ard/ard.rds
#  Analyses   : BIGN, AGE, AGEGR1, SEX, RACE, ECOGBL
#  Generated  : <masked>
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
source("programs/ard/ard_setup.R")

report_id <- "T-14-1-1C"

# ---- code lists: each value in its order, as it is in the ARD ----
# (a value a list does not have stops the program: add it to the list)
cl_trt01a <- c("Drug X" = "Drug X 200 mg", Control = "Control 100 mg")

# ---- data ----
adsl <- order_levels(adsl, c("AGEGR1", "SEX", "RACE", "ECOGBL"))

# adsl_sub: the subjects. population SAF (Safety Analysis Set)
adsl_sub <- adsl |>
  filter(SAFFL == "Y") |>
  set_levels(TRT01A = cl_trt01a)

# ---- analyses ----
# BIGN
.ard_bign <- adsl_sub |>
  mutate(across(where(is.character), ~ na_if(.x, ""))) |>
  ard_tabulate(variables = TRT01A) |>
  fmt_ard(
    modifyList(
      fmt_default,
      list(
        N = 0L,
        mean = 2L,
        sd = 3L,
        median = 1L,
        min = 0L,
        max = 0L,
        p = label_round(1, scale = 100),
        `AGE:mean` = 1L,
        `AGE:sd` = 2L
      )
    )
  ) |>
  tag_ard(report_id, "BIGN", population = "SAF")

# AGE
.ard_age <- adsl_sub |>
  ard_summary(
    by = TRT01A,
    variables = AGE,
    statistic = ~ continuous_summary_fns(
      c("N", "mean", "sd", "median", "min", "max")
    ),
    fmt_fun = everything() ~ modifyList(
      fmt_default,
      list(
        N = 0L,
        mean = 1L,
        sd = 2L,
        median = 1L,
        min = 0L,
        max = 0L,
        p = label_round(1, scale = 100)
      )
    )
  ) |>
  apply_fmt_fun() |>
  tag_ard(report_id, "AGE", population = "SAF")

# AGEGR1
.ard_agegr1 <- adsl_sub |>
  mutate(
    across(where(is.character), ~ na_if(.x, "")),
    across(any_of("AGEGR1") & where(is.factor), droplevels)
  ) |>
  ard_tabulate(by = TRT01A, variables = AGEGR1) |>
  fmt_ard(
    modifyList(
      fmt_default,
      list(
        N = 0L,
        mean = 2L,
        sd = 3L,
        median = 1L,
        min = 0L,
        max = 0L,
        p = label_round(1, scale = 100),
        `AGE:mean` = 1L,
        `AGE:sd` = 2L
      )
    )
  ) |>
  tag_ard(report_id, "AGEGR1", population = "SAF")

# SEX
.ard_sex <- adsl_sub |>
  mutate(
    across(where(is.character), ~ na_if(.x, "")),
    across(any_of("SEX") & where(is.factor), droplevels)
  ) |>
  ard_tabulate(by = TRT01A, variables = SEX) |>
  fmt_ard(
    modifyList(
      fmt_default,
      list(
        N = 0L,
        mean = 2L,
        sd = 3L,
        median = 1L,
        min = 0L,
        max = 0L,
        p = label_round(1, scale = 100),
        `AGE:mean` = 1L,
        `AGE:sd` = 2L
      )
    )
  ) |>
  tag_ard(report_id, "SEX", population = "SAF")

# RACE
.ard_race <- adsl_sub |>
  mutate(
    across(where(is.character), ~ na_if(.x, "")),
    across(any_of("RACE") & where(is.factor), droplevels)
  ) |>
  ard_tabulate(by = TRT01A, variables = RACE) |>
  fmt_ard(
    modifyList(
      fmt_default,
      list(
        N = 0L,
        mean = 2L,
        sd = 3L,
        median = 1L,
        min = 0L,
        max = 0L,
        p = label_round(1, scale = 100),
        `AGE:mean` = 1L,
        `AGE:sd` = 2L
      )
    )
  ) |>
  tag_ard(report_id, "RACE", population = "SAF")

# ECOGBL
.ard_ecogbl <- adsl_sub |>
  mutate(
    across(where(is.character), ~ na_if(.x, "")),
    across(any_of("ECOGBL") & where(is.factor), droplevels)
  ) |>
  ard_tabulate(by = TRT01A, variables = ECOGBL) |>
  fmt_ard(
    modifyList(
      fmt_default,
      list(
        N = 0L,
        mean = 2L,
        sd = 3L,
        median = 1L,
        min = 0L,
        max = 0L,
        p = label_round(1, scale = 100),
        `AGE:mean` = 1L,
        `AGE:sd` = 2L
      )
    )
  ) |>
  tag_ard(report_id, "ECOGBL", population = "SAF")

# the report's rows of the study ARD, with the fingerprint of its
# definition (tfl_ard_spec_hash())
bind_items(
  .ard_bign,
  .ard_age,
  .ard_agegr1,
  .ard_sex,
  .ard_race,
  .ard_ecogbl
) |>
  save_ard(report_id, definition = "5293abc268da42743e5a4f986d5e2ad0")

# the end of this run
options(tflplanner.run = NULL)
