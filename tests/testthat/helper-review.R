# A small study the review finds nothing wrong with: two tables (a
# demographics table and an adverse events table), a listing and its data.
# Each case of fixtures/review-cases.R changes one thing of it.
.rv_study <- function() {
  adsl <- data.frame(
    USUBJID = sprintf("S%02d", 1:8),
    SAFFL = c("Y", "Y", "Y", "Y", "Y", "Y", "N", "N"),
    TRT01A = rep(c("Drug", "Placebo"), 4),
    AGE = c(61, 54, 70, 48, 66, 59, 40, 77),
    SEX = c("F", "M", "F", "F", "M", "M", "F", "M"),
    stringsAsFactors = FALSE)
  adae <- data.frame(
    USUBJID = c("S01", "S01", "S02", "S04", "S05", "S07"),
    TRT01A = c("Drug", "Drug", "Placebo", "Placebo", "Drug", "Drug"),
    AEBODSYS = c("GI", "GI", "SKIN", "GI", "SKIN", "GI"),
    AEDECOD = c("NAUSEA", "VOMITING", "RASH", "NAUSEA", "RASH", "NAUSEA"),
    AESEV = c("MILD", "SEVERE", "MILD", "MILD", "SEVERE", "MILD"),
    TRTEMFL = c("Y", "Y", "Y", "N", "Y", "Y"),
    stringsAsFactors = FALSE)
  df <- function(...) data.frame(..., stringsAsFactors = FALSE)
  ard <- list(
    study = df(key = c("id", "output"), value = c("USUBJID", "output/ard/ard.rds")),
    datasets = df(dataset = c("ADSL", "ADAE"), level = "ADaM",
                  path = c("adsl.rds", "adae.rds"), derive = NA),
    populations = df(population_id = "SAF", dataset = "ADSL",
                     where = 'SAFFL == "Y"', derive = NA),
    analysis_data = df(
      output_id = c("T-1", "T-2", "T-2"),
      data_id = c("adsl_saf", "adsl_saf", "adae_saf"),
      from = c("ADSL", "ADSL", "ADAE"),
      population_id = c("SAF", "SAF", NA),
      subjects = c(NA, NA, "adsl_saf"),
      where = c(NA, NA, 'TRTEMFL == "Y"')),
    analyses = df(
      output_id = c("T-1", "T-1", "T-1", "T-2", "T-2"),
      analysis_id = c("GROUPN", "AGE", "SEX", "GROUPN", "TEAE"),
      method = c("categorical", "continuous", "categorical", "categorical",
                 "hierarchical"),
      data = c("adsl_saf", "adsl_saf", "adsl_saf", "adsl_saf", "adae_saf"),
      by = c(NA, "TRT01A", "TRT01A", NA, "TRT01A"),
      variables = c("TRT01A", "AGE", "SEX", "TRT01A", "AEBODSYS | AEDECOD"),
      statistics = c(NA, "N | mean | sd", "n | p", NA, NA),
      denominator = c(NA, NA, NA, NA, "adsl_saf")))
  spec <- list(
    tables = df(output_id = c("T-1", "T-2"), cols = "TRT01A",
                rows = c("group = variable", "group1 = AEBODSYS | label = AEDECOD")),
    variables = df(output_id = "T-1", variable = c("AGE", "SEX"),
                   label = c("Age (years)", "Sex, n (%)")),
    codelists = df(output_id = c("T-1", "T-1", "T-1", "T-1"),
                   variable = c("TRT01A", "TRT01A", "SEX", "SEX"),
                   value = c("Drug", "Placebo", "F", "M"), order = c(1, 2, 1, 2)),
    cells = df(output_id = c("T-1", "T-1", NA),
               variable = c("continuous", "continuous", NA),
               row = c("n", "Mean (SD)", NA),
               template = c("{N}", "{mean} ({sd})", "{n} ({p:.1f%})")),
    digits = df(output_id = "T-1", variable = NA, statistic = c("mean", "sd"),
                digits = c(1, 2)))
  listings <- list(
    listings = df(output_id = "L-1", dataset = "ADAE", where = 'AESEV == "SEVERE"',
                  sort = "USUBJID"),
    listing_cols = df(output_id = c("L-1", "L-1"), vars = c("USUBJID", "AEDECOD"),
                      label = c("Subject", "Preferred term")))
  list(spec = spec, ard = ard, listings = listings, figures = NULL,
       data = list(ADSL = adsl, ADAE = adae))
}

# the review of a study (with its data's facts when `data`)
.rv_review <- function(s, data = TRUE, ard_facts = NULL, ...) {
  facts <- if (data) tfl_data_facts(s$data, populations = s$ard,
                                    listings = tfl_listing_spec(s$listings, check = FALSE))
  if (!is.null(ard_facts)) {
    if (is.null(facts)) facts <- list()
    facts$ard <- ard_facts
  }
  tfl_review_spec(s$spec, s$ard, s$listings, s$figures, facts = facts, ...)
}
