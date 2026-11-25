# ============================================================================
#  Program    : programs/ard/T-TPL.R
#  Output     : T-TPL -> its rows of output/ard/ard.rds
#  Analyses   : BIGN (Subjects per group), CONT (Continuous), CAT
#               (Categorical)
#  Generated  : <masked>
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
source("programs/ard/ard_setup.R")

report_id <- "T-TPL"

# ---- data ----
adsl <- order_levels(adsl, "SEX")

# adsl_sub: the subjects. population SAF (Safety Analysis Set)
adsl_sub <- filter(adsl, SAFFL == "Y")
# adsl_miss: made by its code, from adsl_sub.
adsl_miss <- mutate(adsl_sub, across(any_of(c("SEX")), function(x) {
  chr <- trimws(as.character(x))
  miss <- is.na(chr) | !nzchar(chr)
  if (!any(miss)) return(x)
  chr[miss] <- "Missing"
  values <- if (is.factor(x)) levels(x) else sort(unique(chr[!miss]))
  factor(chr, levels = unique(c(values, "Missing")))
}))

# ---- analyses ----
# BIGN: Subjects per group
.ard_bign <- adsl_sub |>
  mutate(across(where(is.character), ~ na_if(.x, ""))) |>
  ard_tabulate(variables = TRT01A, fmt_fun = everything() ~ fmt_default) |>
  apply_fmt_fun() |>
  tag_ard(report_id, "BIGN", population = "SAF")

# CONT: Continuous
.ard_cont <- adsl_sub |>
  ard_summary(
    by = TRT01A,
    variables = AGE,
    statistic = ~ continuous_summary_fns(
      c("N", "mean", "sd", "median", "min", "max")
    ),
    fmt_fun = everything() ~ modifyList(
      fmt_default,
      list(N = 0L, mean = 1L, sd = 2L, median = 1L, min = 0L, max = 0L)
    )
  ) |>
  apply_fmt_fun() |>
  tag_ard(report_id, "CONT", population = "SAF")

# CAT: Categorical
.ard_cat <- adsl_miss |>
  mutate(
    across(where(is.character), ~ na_if(.x, "")),
    across(any_of("SEX") & where(is.factor), droplevels)
  ) |>
  ard_tabulate(
    by = TRT01A,
    variables = SEX,
    denominator = adsl_sub,
    statistic = ~ c("n", "p"),
    fmt_fun = everything() ~ modifyList(
      fmt_default,
      list(p = label_round(1, scale = 100))
    )
  ) |>
  apply_fmt_fun() |>
  tag_ard(report_id, "CAT", population = "SAF")

# the report's rows of the study ARD, with the fingerprint of its
# definition (tfl_ard_spec_hash())
bind_items(.ard_bign, .ard_cont, .ard_cat) |>
  save_ard(report_id, definition = "77ac9ae247959ef715f46cad5594d49f")

# the end of this run
options(tflplanner.run = NULL)
