# ============================================================================
#  Program    : programs/ard/T-14-2-1.R
#  Output     : T-14-2-1 -> its rows of output/ard/ard.rds
#  Analyses   : BIGN, SYSBP (SYSBP at week 24)
#  Generated  : <masked>
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
source("programs/ard/ard_setup.R")

report_id <- "T-14-2-1"

# ---- code lists: each value in its order, as it is in the ARD ----
# (a value a list does not have stops the program: add it to the list)
cl_trt01a <- c("Drug X", "Control")

# ---- data ----
adsl <- order_levels(adsl)
advs <- order_levels(advs, from = adsl)

# adsl_saf: the subjects. population SAF (Safety Analysis Set)
adsl_saf <- adsl |>
  filter(SAFFL == "Y") |>
  set_levels(TRT01A = cl_trt01a)
# advs_saf_sysbp: the records. ADVS of the subjects of adsl_saf; where
advs_saf_sysbp <- advs |>
  inner_join(
    select(adsl_saf, USUBJID),
    by = "USUBJID",
    relationship = "many-to-one"
  ) |>
  filter(PARAMCD == "SYSBP", AVISIT == "CYCLE 9 DAY 1") |>
  set_levels(TRT01A = cl_trt01a)

# ---- analyses ----
# BIGN
.ard_bign <- adsl_saf |>
  mutate(across(where(is.character), ~ na_if(.x, ""))) |>
  ard_tabulate(variables = TRT01A, fmt_fun = everything() ~ fmt_default) |>
  apply_fmt_fun() |>
  tag_ard(report_id, "BIGN", population = "SAF")

# SYSBP: SYSBP at week 24
.ard_sysbp <- advs_saf_sysbp |>
  ard_summary(
    by = TRT01A,
    variables = c(BASE, AVAL, CHG),
    statistic = ~ c(
      continuous_summary_fns(c("N", "mean", "sd")),
      tfl_stats[c("se", "mean_lcl", "mean_ucl")],
      continuous_summary_fns(c("median", "min", "max"))
    ),
    fmt_fun = everything() ~ fmt_default
  ) |>
  apply_fmt_fun() |>
  tag_ard(report_id, "SYSBP", population = "SAF")

# the report's rows of the study ARD, with the fingerprint of its
# definition (tfl_ard_spec_hash())
bind_items(.ard_bign, .ard_sysbp) |>
  save_ard(report_id, definition = "85170ae775618a756695be9e7ef180e3")

# the end of this run
options(tflplanner.run = NULL)
