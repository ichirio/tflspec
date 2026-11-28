# ============================================================================
#  Program    : programs/ard/T-14-1-2.R
#  Output     : T-14-1-2 -> its rows of output/ard/ard.rds
#  Analyses   : BIGN, DISP (Status at end of study)
#  Generated  : <masked>
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
source("programs/ard/ard_setup.R")

report_id <- "T-14-1-2"

# ---- code lists: each value in its order, as it is in the ARD ----
# (a value a list does not have stops the program: add it to the list)
cl_trt01a <- c("Drug X", "Control")
cl_eosstt <- c("ONGOING", "DISCONTINUED")

# ---- data ----
adsl <- order_levels(adsl)

# adsl_saf: the subjects. population SAF (Safety Analysis Set)
adsl_saf <- adsl |>
  filter(SAFFL == "Y") |>
  set_levels(TRT01A = cl_trt01a, EOSSTT = cl_eosstt)

# ---- analyses ----
# BIGN
.ard_bign <- adsl_saf |>
  mutate(across(where(is.character), ~ na_if(.x, ""))) |>
  ard_tabulate(variables = TRT01A, fmt_fun = everything() ~ fmt_default) |>
  apply_fmt_fun() |>
  tag_ard(report_id, "BIGN", population = "SAF")

# DISP: Status at end of study
.ard_disp <- adsl_saf |>
  mutate(across(where(is.character), ~ na_if(.x, ""))) |>
  ard_tabulate(
    by = TRT01A,
    variables = EOSSTT,
    statistic = ~ c("n", "p"),
    fmt_fun = everything() ~ fmt_default
  ) |>
  apply_fmt_fun() |>
  tag_ard(report_id, "DISP", population = "SAF")

# the report's rows of the study ARD, with the fingerprint of its
# definition (tfl_ard_spec_hash())
bind_items(.ard_bign, .ard_disp) |>
  save_ard(report_id, definition = "d125e751f84fbd133a40b3986840c717")

# the end of this run
options(tflplanner.run = NULL)
