# ============================================================================
#  Program    : programs/ard/F-TPL.R
#  Output     : F-TPL -> its rows of output/ard/ard.rds
#  Analyses   : HR (Hazard ratio, all subjects), HR_SEX (Hazard ratio by
#               SEX), HR_AGEGR1 (Hazard ratio by AGEGR1)
#  Generated  : <masked>
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
source("programs/ard/ard_setup.R")

report_id <- "F-TPL"

# ---- data ----
adsl <- order_levels(adsl)
adtte <- order_levels(adtte, from = adsl)

# adsl_sub: the subjects. population SAF (Safety Analysis Set)
adsl_sub <- filter(adsl, SAFFL == "Y")

# adtte_saf_pfs: the records. ADTTE of the subjects of adsl_sub, SEX, AGEGR1
# from ADSL; where
adtte_saf_pfs <- adtte |>
  select(-any_of(c("SEX", "AGEGR1"))) |>
  inner_join(
    select(adsl_sub, USUBJID, SEX, AGEGR1),
    by = "USUBJID",
    relationship = "many-to-one"
  ) |>
  filter(PARAMCD == "PFS")

# ---- analyses ----
# HR: Hazard ratio, all subjects
.ard_hr <- local({
  data <- adtte_saf_pfs
  n_no_arm <- sum(is.na(data$TRT01A))
  if (n_no_arm > 0) {
    warning("forest_hr: ", n_no_arm, " subjects with an empty or missing TRT01A were left out of the Cox model", call. = FALSE)
  }
  n_no_time <- sum(!is.na(data$TRT01A) & (is.na(data$AVAL) | is.na(data$CNSR)))
  if (n_no_time > 0) {
    warning("forest_hr: ", n_no_time, " subjects with a missing AVAL or CNSR were left out of the Cox model", call. = FALSE)
  }
  cox <- survival::coxph(survival::Surv(AVAL, CNSR == 0) ~ TRT01A, data = data)
  cardx::ard_regression(cox, exponentiate = TRUE)
}) |>
  fmt_ard(fmt_default) |>
  tag_ard(report_id, "HR", population = "SAF")

# HR_SEX: Hazard ratio by SEX
.ard_hr_sex <- ard_strata(
  filter(adtte_saf_pfs, !is.na(SEX)),
  .strata = SEX,
  .f = ~ cardx::ard_regression(survival::coxph(survival::Surv(AVAL, CNSR == 0) ~ TRT01A, data = .x), exponentiate = TRUE)
) |>
  fmt_ard(fmt_default) |>
  tag_ard(report_id, "HR_SEX", population = "SAF")

# HR_AGEGR1: Hazard ratio by AGEGR1
.ard_hr_agegr1 <- ard_strata(
  filter(adtte_saf_pfs, !is.na(AGEGR1)),
  .strata = AGEGR1,
  .f = ~ cardx::ard_regression(survival::coxph(survival::Surv(AVAL, CNSR == 0) ~ TRT01A, data = .x), exponentiate = TRUE)
) |>
  fmt_ard(fmt_default) |>
  tag_ard(report_id, "HR_AGEGR1", population = "SAF")

# the report's rows of the study ARD, with the fingerprint of its
# definition (tfl_ard_spec_hash())
bind_items(.ard_hr, .ard_hr_sex, .ard_hr_agegr1) |>
  save_ard(report_id, definition = "dc91a0c06a0dc1067840c183ad4af68f")

# the end of this run
options(tflplanner.run = NULL)
