# Extracted from test-spec-cards.R:9

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "tflspec", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
ok <- tfl_item("cards::ard_summary", list(
    variables = "AGE", by = "TRT01A",
    statistic = list(AGE = tfl_call("continuous_summary_fns", package = "cards", args =
                                    list(summaries = c("N", "mean", "sd"))))))
expect_identical(nrow(tfl_schema_check(ok)), 0L)
