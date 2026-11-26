# ============================================================================
#  Program    : programs/ard/T-14-3-1C.R
#  Output     : T-14-3-1C -> its rows of output/ard/ard.rds
#  Analyses   : BIGN (Subjects per group), AE (TEAE by SOC / PT)
#  Generated  : <masked>
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
source("programs/ard/ard_setup.R")

report_id <- "T-14-3-1C"

# ---- code lists: each value in its order, as it is in the ARD ----
# (a value a list does not have stops the program: add it to the list)
cl_trt01a <- c("Drug X", "Control")

# ---- data ----
adsl <- order_levels(adsl, c("AEBODSYS", "AEDECOD"))
adae <- order_levels(adae, c("AEBODSYS", "AEDECOD"), from = adsl)

# adsl_saf: the subjects. population SAF (Safety Analysis Set)
adsl_saf <- adsl |>
  filter(SAFFL == "Y") |>
  set_levels(TRT01A = cl_trt01a)
# adae_saf: the records. ADAE of the subjects of adsl_saf, TRT01A from ADSL;
# where
adae_saf <- adae |>
  select(-any_of("TRT01A")) |>
  inner_join(
    select(adsl_saf, USUBJID, TRT01A),
    by = "USUBJID",
    relationship = "many-to-one"
  ) |>
  filter(TRTEMFL == "Y") |>
  set_levels(TRT01A = cl_trt01a)

# ---- analyses ----
# BIGN: Subjects per group
.ard_bign <- adsl_saf |>
  mutate(across(where(is.character), ~ na_if(.x, ""))) |>
  ard_tabulate(variables = TRT01A, fmt_fun = everything() ~ fmt_default) |>
  apply_fmt_fun() |>
  tag_ard(report_id, "BIGN", population = "SAF")

# AE: TEAE by SOC / PT
.ard_ae <- adae_saf |>
  ard_hierarchical_or_none(
    by = TRT01A,
    variables = c(AEBODSYS, AEDECOD),
    denominator = adsl_saf,
    id = USUBJID,
    over_variables = TRUE
  ) |>
  fmt_ard(fmt_default) |>
  tag_ard(report_id, "AE", population = "SAF")
# AE: each depth an analysis (AE_ANY, AE_L1, AE_L2); the by counts are BIGN's
.ard_ae <- .ard_ae |>
  filter(!variable %in% "TRT01A") |>
  mutate(analysis_id = case_when(
    variable == "..ard_hierarchical_overall.." ~ "AE_ANY",
    variable == "AEBODSYS" ~ "AE_L1",
    variable == "AEDECOD" ~ "AE_L2",
    .default = analysis_id))

# the report's rows of the study ARD, with the fingerprint of its
# definition (tfl_ard_spec_hash())
bind_items(.ard_bign, .ard_ae) |>
  save_ard(report_id, definition = "92cfa26ec24ac2b7135cb0189e84b3a9")

# the end of this run
options(tflplanner.run = NULL)
