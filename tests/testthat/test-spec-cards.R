# The ARD's analyses -- cards and cardx calls -- have their schema entries:
# an item checks, and its arguments, as R takes them, run.

test_that("a cards analysis checks against its schema", {
  ok <- tfl_item("cards::ard_summary", list(
    variables = "AGE", by = "TRT01A",
    statistic = list(AGE = tfl_call("continuous_summary_fns", package = "cards", args =
                                    list(summaries = c("N", "mean", "sd"))))))
  expect_identical(nrow(tfl_schema_check(ok)), 0L)
  typo <- tfl_schema_check(tfl_item("cards::ard_tabulate",
                                    list(variables = "SEX", denomiator = "row")))
  expect_identical(typo$level, "warning")
  expect_match(typo$message, "denominator")
  for (fn in c("ard_summary", "ard_tabulate", "ard_tabulate_value", "ard_hierarchical",
               "ard_stack_hierarchical", "ard_stack", "ard_strata", "ard_missing",
               "ard_total_n", "ard_categorical_ci", "ard_stats_t_test")) {
    expect_false(is.null(tfl_schema(fn)), label = fn)
  }
})

test_that("a cards analysis's arguments, as R takes them, make the ARD", {
  skip_if_not_installed("cards")
  args <- tflspec:::.schema_to_r("ard_summary", list(
    variables = "AGE", by = "ARM",
    statistic = list(AGE = tfl_call("continuous_summary_fns", package = "cards", args =
                                    list(summaries = c("N", "mean")))),
    fmt_fun = list(AGE = list(mean = 1L))))
  args <- lapply(args, tflspec:::.spec_eval, env = emptyenv())
  ard <- do.call(cards::ard_summary, c(list(cards::ADSL), args))
  direct <- cards::ard_summary(cards::ADSL, variables = AGE, by = ARM,
                               statistic = list(AGE = cards::continuous_summary_fns(c("N", "mean"))),
                               fmt_fun = list(AGE = list(mean = 1L)))
  expect_identical(cards::apply_fmt_fun(ard)$stat_fmt, cards::apply_fmt_fun(direct)$stat_fmt)
})

test_that("each cards schema argument is one the function has", {
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
})

test_that("another package's call is written with its package", {
  a <- tflspec:::.schema_to_r("ard_summary", list(statistic = list(
    AGE = tfl_call("continuous_summary_fns", package = "cards",
                   args = list(summaries = c("N", "mean"))))))
  expect_identical(tflspec:::.spec_call_code(a$statistic$AGE),
                   "cards::continuous_summary_fns(summaries = c(\"N\", \"mean\"))")
})
