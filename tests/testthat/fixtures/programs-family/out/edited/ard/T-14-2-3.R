# ============================================================================
#  Program    : programs/ard/T-14-2-3.R
#  Output     : T-14-2-3 -> its rows of output/ard/ard.rds
#  Analyses   : BIGN, N (Subjects), CI (Mean change (95% CI), one-sample
#               t-test)
#  Generated  : <masked>
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
source("programs/ard/ard_setup.R")

report_id <- "T-14-2-3"

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

# N: Subjects
.ard_n <- advs_saf_sysbp |>
  ard_summary(
    by = TRT01A,
    variables = CHG,
    statistic = ~ continuous_summary_fns(c("N")),
    fmt_fun = everything() ~ fmt_default
  ) |>
  apply_fmt_fun() |>
  tag_ard(report_id, "N", population = "SAF")

# CI: Mean change (95% CI), one-sample t-test
.ard_ci <- advs_saf_sysbp |>
  cardx::ard_continuous_ci(by = TRT01A, variables = CHG) |>
  fmt_ard(
    modifyList(fmt_default, list(estimate = 1L, conf.low = 1L, conf.high = 1L))
  ) |>
  relevel_ard(TRT01A = cl_trt01a) |>
  tag_ard(report_id, "CI", population = "SAF")

# the report's rows of the study ARD, with the fingerprint of its
# definition (tfl_ard_spec_hash())
bind_items(.ard_bign, .ard_n, .ard_ci) |>
  save_ard(report_id, definition = "59b0b1efd34b94fe34bc55c3494d4d90")

# the end of this run
options(tflplanner.run = NULL)
