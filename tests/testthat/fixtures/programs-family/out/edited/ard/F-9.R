# ============================================================================
#  Program    : programs/ard/F-9.R
#  Output     : F-9 -> its rows of output/ard/ard.rds
#  Analyses   : BIGN (Subjects per group)
#  Generated  : <masked>
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
source("programs/ard/ard_setup.R")

report_id <- "F-9"

# ---- data ----
adsl <- order_levels(adsl)

# adsl_sub: the subjects. population SAF (Safety Analysis Set)
adsl_sub <- filter(adsl, SAFFL == "Y")

# ---- analyses ----
# BIGN: Subjects per group
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

# the report's rows of the study ARD, with the fingerprint of its
# definition (tfl_ard_spec_hash())
.ard_bign |>
  save_ard(report_id, definition = "a848dda3a47300961c9361575625ecb6")

# the end of this run
options(tflplanner.run = NULL)
