# The keywords of the model-based and the safety analyses (cmh, prop_diff,
# lsmeans, lsmeans_diff, km, logrank, abnormal, incidence_rate): each is its
# cardx function, by another name -- the same code; the ARS names the
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
          incidence_rate = "cardx::ard_incidence_rate")
  expect_identical(keys$call[match(names(kw), keys$method)], unname(kw))
  expect_true(all(keys$kind[match(names(kw), keys$method)] == "none"))
  # no defaults: a row naming the function gets nothing the keyword adds
  expect_true(all(!nzchar(keys$defaults[match(names(kw), keys$method)])))
  rows <- list(
    list(method = "cmh", population_id = "SAF", by = "TRT01A",
         variables = "AGEGR1", strata = "SEX"),
    list(method = "lsmeans", population_id = "SAF",
         args = "formula = AGE ~ TRT01A + SEX, method = \"lm\""),
    list(method = "km", dataset = "ADTTE", population_id = "SAF",
         variables = "TRTA",
         args = "y = \"survival::Surv(AVAL, 1 - CNSR)\", times = 60"))
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
