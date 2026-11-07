# Extracted from test-spec-cards.R:42

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "tflspec", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
skip_if_not_installed("cards")
skip_if_not_installed("cardx")
for (fn in c("ard_summary", "ard_tabulate", "ard_tabulate_value", "ard_hierarchical",
               "ard_stack_hierarchical", "ard_stack", "ard_strata", "ard_categorical_ci",
               "ard_stats_t_test", "ard_stats_chisq_test")) {
    ent <- tfl_schema(fn)
    f <- getExportedValue(ent$package, fn)
    fm <- names(formals(f))
    if (inherits(f, "function") && "UseMethod" %in% all.names(body(f))) {
      fm <- names(formals(getS3method(fn, "data.frame", envir = asNamespace(ent$package))))
    }
    expect_true(all(names(ent$args) %in% fm), label = fn)
  }
