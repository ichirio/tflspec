# Extracted from test-spec-cards.R:55

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "tflspec", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
a <- tflspec:::.schema_to_r("ard_summary", list(statistic = list(
    AGE = tfl_call("continuous_summary_fns", package = "cards",
                   args = list(summaries = c("N", "mean"))))))
