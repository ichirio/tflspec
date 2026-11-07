# Extracted from test-cells-guards.R:27

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "tflspec", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
it <- tfl_item("plan_cells", list(`Mean (SD)` = list(tfl_r("PARAMCD == \"BILI\" ~ \"{mean:.2f}\""), "{mean:.1f}")))
p <- tfl_schema_check(it)
