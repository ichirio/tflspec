# ============================================================================
#  Program    : programs/ard/F-14-2-4.R
#  Output     : F-14-2-4 -> its rows of output/ard/ard.rds
#  Analyses   : HR (Hazard ratio, all subjects), HR_SEX (Hazard ratio by
#               SEX), HR_AGEGR1 (Hazard ratio by AGEGR1)
#  Generated  : <masked>
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
source("programs/ard/ard_setup.R")

report_id <- "F-14-2-4"

# ---- code lists: each value in its order, as it is in the ARD ----
# (a value a list does not have stops the program: add it to the list)
cl_trt01a <- c("Control", "Drug X")
cl_sex <- c(F = "Female", M = "Male")
cl_agegr1 <- c("<65" = "<65 years", ">=65" = ">=65 years")

# ---- data ----
adsl <- order_levels(adsl)
adtte <- order_levels(adtte, from = adsl)

# adsl_sub: the subjects. population SAF (Safety Analysis Set)
adsl_sub <- filter(adsl, SAFFL == "Y")

# adtte_saf_ttskin: the records. ADTTE of the subjects of adsl_sub, SEX, AGEGR1
# from ADSL
adtte_saf_ttskin <- adtte |>
  select(-any_of(c("SEX", "AGEGR1"))) |>
  inner_join(
    select(adsl_sub, USUBJID, SEX, AGEGR1),
    by = "USUBJID",
    relationship = "many-to-one"
  ) |>
  filter(PARAMCD == "TTSKIN") |>
  set_levels(TRT01A = cl_trt01a, SEX = cl_sex, AGEGR1 = cl_agegr1)

# ---- analyses ----
# HR: Hazard ratio, all subjects
.ard_hr <- local({
  data <- adtte_saf_ttskin
  cox <- survival::coxph(survival::Surv(AVAL, CNSR == 0) ~ TRT01A, data = data)
  cardx::ard_regression(cox, exponentiate = TRUE)
}) |>
  fmt_ard(fmt_default) |>
  relevel_ard(TRT01A = cl_trt01a) |>
  tag_ard(report_id, "HR", population = "SAF")

# HR_SEX: Hazard ratio by SEX
.ard_hr_sex <- ard_strata(
  adtte_saf_ttskin,
  .strata = SEX,
  .f = ~ tryCatch(
    cardx::ard_regression(
      survival::coxph(survival::Surv(AVAL, CNSR == 0) ~ TRT01A, data = .x),
      exponentiate = TRUE
    ),
    error = function(e) {
      warning("HR_SEX: the Cox model of a subgroup failed (",
              conditionMessage(e), "); the figure prints its hazard ratio NE",
              call. = FALSE)
      NULL
    }
  )
) |>
  fmt_ard(fmt_default) |>
  relevel_ard(TRT01A = cl_trt01a, SEX = cl_sex) |>
  tag_ard(report_id, "HR_SEX", population = "SAF")

# HR_AGEGR1: Hazard ratio by AGEGR1
.ard_hr_agegr1 <- ard_strata(
  adtte_saf_ttskin,
  .strata = AGEGR1,
  .f = ~ tryCatch(
    cardx::ard_regression(
      survival::coxph(survival::Surv(AVAL, CNSR == 0) ~ TRT01A, data = .x),
      exponentiate = TRUE
    ),
    error = function(e) {
      warning("HR_AGEGR1: the Cox model of a subgroup failed (",
              conditionMessage(e), "); the figure prints its hazard ratio NE",
              call. = FALSE)
      NULL
    }
  )
) |>
  fmt_ard(fmt_default) |>
  relevel_ard(TRT01A = cl_trt01a, AGEGR1 = cl_agegr1) |>
  tag_ard(report_id, "HR_AGEGR1", population = "SAF")

# the report's rows of the study ARD, with the fingerprint of its
# definition (tfl_ard_spec_hash())
bind_items(.ard_hr, .ard_hr_sex, .ard_hr_agegr1) |>
  save_ard(report_id, definition = "9c3061789671d5610a48f25e923c5a95")

# the end of this run
options(tflplanner.run = NULL)
