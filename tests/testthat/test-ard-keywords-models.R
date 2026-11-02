# The keywords of the model-based and the safety analyses (cmh, prop_diff,
# lsmeans, lsmeans_diff, km, logrank, abnormal, incidence_rate, regression,
# anova, kruskal, paired_ttest, paired_wilcox, mcnemar, km_diff): each is
# its cardx function, by another name -- the same code; the ARS names the
# method by its keyword.

test_that("a keyword of a cardx analysis writes its function's call", {
  keys <- tfl_ard_methods()
  kw <- c(cmh = "cardx::ard_stats_mantelhaen_test",
          prop_diff = "cardx::ard_stats_prop_test",
          lsmeans = "cardx::ard_emmeans_emmeans",
          lsmeans_diff = "cardx::ard_emmeans_contrast",
          km = "cardx::ard_survival_survfit",
          logrank = "cardx::ard_survival_survdiff",
          abnormal = "cardx::ard_tabulate_abnormal",
          incidence_rate = "cardx::ard_incidence_rate",
          regression = "cardx::ard_regression",
          anova = "cardx::ard_car_anova",
          kruskal = "cardx::ard_stats_kruskal_test",
          paired_ttest = "cardx::ard_stats_paired_t_test",
          paired_wilcox = "cardx::ard_stats_paired_wilcox_test",
          mcnemar = "cardx::ard_stats_mcnemar_test",
          km_diff = "cardx::ard_survival_survfit_diff")
  expect_identical(keys$call[match(names(kw), keys$method)], unname(kw))
  expect_true(all(keys$kind[match(names(kw), keys$method)] == "none"))
  # no defaults: a row naming the function gets nothing the keyword adds
  expect_true(all(!nzchar(keys$defaults[match(names(kw), keys$method)])))
  # one keyword a function: a row naming the function is that keyword's
  expect_false(anyDuplicated(keys$call[keys$call %in% kw]) > 0L)
  rows <- list(
    list(method = "cmh", population_id = "SAF", by = "TRT01A",
         variables = "AGEGR1", strata = "SEX"),
    list(method = "lsmeans", population_id = "SAF",
         args = "formula = AGE ~ TRT01A + SEX, method = \"lm\""),
    list(method = "km", dataset = "ADTTE", population_id = "SAF",
         variables = "TRTA",
         args = "y = \"survival::Surv(AVAL, 1 - CNSR)\", times = 60"),
    list(method = "regression", dataset = "ADTTE", population_id = "SAF",
         args = paste("formula = survival::Surv(AVAL, 1 - CNSR) ~ TRTA,",
                      "method = \"coxph\", package = \"survival\", exponentiate = TRUE")),
    list(method = "anova", population_id = "SAF",
         args = "x = lm(AGE ~ TRT01A + SEX, data = data), type = \"III\""),
    list(method = "kruskal", population_id = "SAF", by = "TRT01A",
         variables = "AGE"),
    list(method = "km_diff", dataset = "ADTTE", population_id = "SAF",
         args = paste("x = survival::survfit(survival::Surv(AVAL, 1 - CNSR) ~ TRTA,",
                      "data = data), times = 60")))
  for (r in rows) {
    f <- r
    f$method <- keys$call[match(r$method, keys$method)]
    a <- tfl_ard_code(exact_spec(r), save = FALSE)
    expect_identical(a, tfl_ard_code(exact_spec(f), save = FALSE), label = r$method)
    expect_true(any(grepl(paste0(f$method, "("), a, fixed = TRUE)), label = r$method)
  }
})

test_that("the ARS names a model-based method by its keyword", {
  x <- exact_spec(list(method = "cardx::ard_stats_mantelhaen_test",
                       population_id = "SAF", by = "TRT01A", variables = "SEX",
                       strata = "AGEGR1"))
  m <- tfl_ars(x)$methods
  expect_identical(m[[1L]]$id, "Mth_cmh")
  expect_identical(m[[1L]]$name, "Cochran-Mantel-Haenszel test")
  expect_identical(vapply(m[[1L]]$operations, `[[`, "", "label"), "p.value")
})

test_that("the ARS lays out the new tests by their keyword", {
  x <- exact_spec(list(method = "cardx::ard_stats_kruskal_test",
                       population_id = "SAF", by = "TRT01A", variables = "AGE"))
  m <- tfl_ars(x)$methods
  expect_identical(m[[1L]]$id, "Mth_kruskal")
  expect_identical(m[[1L]]$name, "Kruskal-Wallis test")
  expect_identical(vapply(m[[1L]]$operations, `[[`, "", "label"), "p.value")
  x <- exact_spec(list(method = "mcnemar", population_id = "SAF",
                       by = "SEX", variables = "AGEGR1"))
  m <- tfl_ars(x)$methods
  expect_identical(m[[1L]]$id, "Mth_mcnemar")
  expect_identical(vapply(m[[1L]]$operations, `[[`, "", "label"), "p.value")
  # a model's estimates: its results, not a p-value
  x <- exact_spec(list(method = "regression", population_id = "SAF",
                       args = "formula = AGE ~ TRT01A + SEX, method = \"lm\""))
  m <- tfl_ars(x)$methods
  expect_identical(m[[1L]]$id, "Mth_regression")
  expect_identical(m[[1L]]$name, "Model estimates")
})

test_that("a difference in Kaplan-Meier estimates builds; its text statistic stays text", {
  skip_on_cran()
  skip_if_not_installed("cardx")
  skip_if_not_installed("survival")
  skip_if_not_installed("dplyr")
  adam <- exact_data()
  dir <- exact_dir(adam)
  on.exit(unlink(dir, recursive = TRUE))
  sp <- exact_spec(list(method = "km_diff", dataset = "ADTTE", population_id = "SAF",
                        args = paste("x = survival::survfit(survival::Surv(AVAL, 1 - CNSR) ~ TRTA,",
                                     "data = data), times = c(30, 90)")))
  ard <- suppressMessages(tfl_build_ard(sp, dir = dir, save = FALSE))
  # cardx gives `method` a number of decimals; fmt_ard() keeps it as text
  m <- ard[ard$stat_name == "method", ]
  expect_gt(nrow(m), 0L)
  expect_identical(unlist(m$stat_fmt), unlist(m$stat))
  est <- ard[ard$stat_name == "estimate", ]
  expect_true(all(grepl("^-?[0-9]+[.][0-9]{2}$", unlist(est$stat_fmt))))
})
