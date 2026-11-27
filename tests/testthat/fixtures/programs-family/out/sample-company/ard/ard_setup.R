# ============================================================================
#  Program    : programs/ard/ard_setup.R
#  What every ARD program of the study starts with (it sources this).
#  Generated  : <masked>
# ============================================================================

# the study's setup: the company's, the study's folders and id, your own
source("programs/study_setup.R")

# the study's own analysis functions (study key `source`)
source(file.path(path_programs_ard, "functions/ard_cv_geomean.R"))

# statistics cards does not compute itself (company standards)
tfl_stats <- list(
  se = function(x) sd(x) / sqrt(length(x)),
  mean_lcl = function(x) {
    mean(x) - qt(0.975, length(x) - 1) * sd(x) / sqrt(length(x))
  },
  mean_ucl = function(x) {
    mean(x) + qt(0.975, length(x) - 1) * sd(x) / sqrt(length(x))
  }
)

# each statistic's format (company standards), as cards' fmt_fun takes
# it: an integer is that many decimals, label_round(1, scale = 100) a
# proportion as a percent
fmt_default <- list(
  N = 0L,
  mean = 1L,
  sd = 2L,
  median = 1L,
  p25 = 1L,
  p75 = 1L,
  min = 0L,
  max = 0L,
  se = 2L,
  var = 2L,
  cv = 1L,
  range = 0L,
  iqr = 1L,
  sum = 0L,
  p5 = 1L,
  p10 = 1L,
  p90 = 1L,
  p95 = 1L,
  mean_lcl = 1L,
  mean_ucl = 1L,
  geo_mean = 2L,
  geo_sd = 2L,
  geo_cv = 1L,
  geo_lcl = 2L,
  geo_ucl = 2L,
  log_mean = 3L,
  log_sd = 3L,
  n = 0L,
  p = label_round(1, scale = 100),
  n_cum = 0L,
  p_cum = label_round(1, scale = 100),
  N_obs = 0L,
  N_miss = 0L,
  N_nonmiss = 0L,
  p_miss = label_round(1, scale = 100),
  p_nonmiss = label_round(1, scale = 100),
  estimate = 2L,
  conf.low = 2L,
  conf.high = 2L,
  std.error = 3L,
  estimate1 = 2L,
  estimate2 = 2L,
  statistic = 2L,
  parameter = 1L,
  p.value = fmt_pvalue,
  conf.level = 2L,
  n.risk = 0L,
  n.event = 0L
)

# the files each ARD is built with, recorded with it (tflspec's
# save_ard(): tflplanner compares the study setup's)
options(tflspec.ard_sources = c(
  company = "programs/company_setup.R",
  setup = "programs/study_setup.R",
  user = "programs/study_user.R"))

