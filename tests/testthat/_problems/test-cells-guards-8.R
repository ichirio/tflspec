# Extracted from test-cells-guards.R:8

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "tflspec", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
it <- tfl_item("plan_cells", list(continuous = list(
    n = "{N:.0f}",
    `Mean (SD)` = list(tfl_r("PARAMCD == \"BILI\" ~ \"{mean:.2f} ({sd:.3f})\""), "{mean:.1f} ({sd:.2f})"))))
expect_identical(nrow(tfl_schema_check(it)), 0L)
