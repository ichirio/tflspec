# Extracted from test-ard-parent.R:130

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "tflspec", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
parent_spec <- function(rows) {
  S <- .ard_spec_sheets
  tfl_ard_spec(list(
    study = exact_sheet(list(list(key = "id", value = "USUBJID")), S$study),
    datasets = exact_sheet(list(list(dataset = "ADSL", path = "adam/ADSL.rds"),
                                list(dataset = "ADAE", path = "adam/ADAE.rds")),
                           S$datasets),
    populations = exact_sheet(list(list(population_id = "SAF", dataset = "ADSL",
                                        where = "SAFFL == \"Y\"")), S$populations),
    analyses = exact_sheet(lapply(rows, function(r) {
      r$output_id <- r$output_id %||% "T"
      r
    }), S$analyses)))
}

# test -------------------------------------------------------------------------
skip_on_cran()
skip_if_not_installed("cardx")
keys <- tfl_ard_methods()
expect_identical(keys$call[match(c("stack", "subgroups", "pairwise"), keys$method)],
                   c("cards::ard_stack", "cards::ard_strata", "cards::ard_pairwise"))
adam <- exact_data()
dir <- exact_dir(adam)
by_kw <- parent_spec(list(
    list(analysis_id = "DEMO", method = "stack", dataset = "ADSL",
         population_id = "SAF", by = "ARM", overall = "TRUE"),
    list(analysis_id = "CONT", parent = "DEMO", method = "continuous",
         variables = "AGE"),
    list(analysis_id = "BYSEX", method = "subgroups", dataset = "ADSL",
         population_id = "SAF", strata = "SEX"),
    list(analysis_id = "AGE", parent = "BYSEX", method = "continuous",
         by = "ARM", variables = "AGE"),
    list(analysis_id = "PAIRS", method = "pairwise", dataset = "ADSL",
         population_id = "SAF", variables = "ARM",
         args = "include = \"Placebo\""),
    list(analysis_id = "TT", parent = "PAIRS", method = "ttest", by = "ARM",
         variables = "AGE", statistics = "estimate | p.value")))
by_fn <- by_kw
by_fn$analyses$method <- .wrapper_call(by_fn$analyses$method)
expect_identical(by_fn$analyses$method[c(1L, 3L, 5L)],
                   c("cards::ard_stack", "cards::ard_strata", "cards::ard_pairwise"))
code <- tfl_ard_code(by_kw, part = "body")
expect_identical(code, tfl_ard_code(by_fn, part = "body"))
txt <- paste(code, collapse = "\n")
expect_match(txt, "ard_stack(\n    .by = ARM,", fixed = TRUE)
expect_match(txt, ".overall = TRUE", fixed = TRUE)
expect_match(txt, "ard_strata(\n    .strata = SEX,", fixed = TRUE)
expect_match(txt, "ard_pairwise(\n    variable = ARM,", fixed = TRUE)
ard <- suppressMessages(tfl_build_ard(by_kw, dir = dir, save = FALSE))
expect_setequal(unique(ard$analysis_id), c("CONT", "AGE", "TT"))
