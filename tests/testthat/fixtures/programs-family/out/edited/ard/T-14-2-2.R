# ============================================================================
#  Program    : programs/ard/T-14-2-2.R
#  Output     : T-14-2-2 -> its rows of output/ard/ard.rds
#  Analyses   : BIGN, KM (Kaplan-Meier estimates), HR (Hazard ratio vs
#               control (Cox), not printed in the table)
#  Generated  : <masked>
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
source("programs/ard/ard_setup.R")

report_id <- "T-14-2-2"

# ---- code lists: each value in its order, as it is in the ARD ----
# (a value a list does not have stops the program: add it to the list)
cl_trt01a <- c("Drug X", "Control")

# ---- data ----
adsl <- order_levels(adsl)
adtte <- order_levels(adtte, from = adsl)

# adsl_saf: the subjects. population SAF (Safety Analysis Set)
adsl_saf <- adsl |>
  filter(SAFFL == "Y") |>
  set_levels(TRT01A = cl_trt01a)
# adtte_saf_ttskin: the records. ADTTE of the subjects of adsl_saf; where
adtte_saf_ttskin <- adtte |>
  inner_join(
    select(adsl_saf, USUBJID),
    by = "USUBJID",
    relationship = "many-to-one"
  ) |>
  filter(PARAMCD == "TTSKIN") |>
  set_levels(TRT01A = cl_trt01a)

# ---- analyses ----
# BIGN
.ard_bign <- adsl_saf |>
  mutate(across(where(is.character), ~ na_if(.x, ""))) |>
  ard_tabulate(variables = TRT01A, fmt_fun = everything() ~ fmt_default) |>
  apply_fmt_fun() |>
  tag_ard(report_id, "BIGN", population = "SAF")

# KM: Kaplan-Meier estimates
.ard_km <- local({
  data <- adtte_saf_ttskin
  fit <- survival::survfit(survival::Surv(AVAL, CNSR == 0) ~ TRT01A, data = data)
  bind_ard(
    cardx::ard_survival_survfit(fit, probs = 0.5),
    cardx::ard_survival_survfit(fit, times = c(0, 30, 60, 90, 120, 150, 180)),
    .quiet = TRUE
  )
}) |>
  fmt_ard(fmt_default) |>
  relevel_ard(TRT01A = cl_trt01a) |>
  tag_ard(report_id, "KM", population = "SAF")

# HR: Hazard ratio vs control (Cox), not printed in the table
.ard_hr <- local({
  data <- adtte_saf_ttskin
  data$TRT01A <- relevel(factor(data$TRT01A), ref = "Control")
  cox <- survival::coxph(survival::Surv(AVAL, CNSR == 0) ~ TRT01A, data = data)
  cardx::ard_regression(cox, exponentiate = TRUE)
}) |>
  fmt_ard(fmt_default) |>
  relevel_ard(TRT01A = cl_trt01a) |>
  tag_ard(report_id, "HR", population = "SAF")

# the report's rows of the study ARD, with the fingerprint of its
# definition (tfl_ard_spec_hash())
bind_items(.ard_bign, .ard_km, .ard_hr) |>
  save_ard(report_id, definition = "018de7f7b3580fe77a63633f56949f54")

# the end of this run
options(tflplanner.run = NULL)
