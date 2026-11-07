# Extracted from test-spec-cards.R:27

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "tflspec", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
skip_if_not_installed("cards")
args <- tflspec:::.schema_to_r("ard_summary", list(
    variables = "AGE", by = "ARM",
    statistic = list(AGE = tfl_call("continuous_summary_fns", package = "cards", args =
                                    list(summaries = c("N", "mean")))),
    fmt_fun = list(AGE = list(mean = 1L))))
