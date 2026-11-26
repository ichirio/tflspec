# ============================================================================
#  Program    : programs/ard/T-14-3-1.R
#  Output     : T-14-3-1 -> its rows of output/ard/ard.rds
#  Analyses   : BIGN (Subjects per group), TEAE (TEAE by SOC / PT)
#  Generated  : <masked>
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
source("programs/ard/ard_setup.R")

report_id <- "T-14-3-1"

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
# adae_saf: the records. ADAE of the subjects of adsl_saf, TRT01A from ADSL
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

# TEAE: TEAE by SOC / PT
.ard_teae <- adae_saf |>
  ard_hierarchical_or_none(
    by = TRT01A,
    variables = c(AEBODSYS, AEDECOD),
    denominator = adsl_saf,
    id = USUBJID,
    over_variables = TRUE
  ) |>
  fmt_ard(fmt_default) |>
  tag_ard(report_id, "TEAE", population = "SAF")

# the report's rows of the study ARD, with the fingerprint of its
# definition (tfl_ard_spec_hash())
bind_items(.ard_bign, .ard_teae) |>
  save_ard(report_id, definition = "0df8ef40bd80dd17b6e4e60d31846ad4")

# the end of this run
options(tflplanner.run = NULL)
