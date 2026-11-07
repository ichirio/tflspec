# The keywords of the model-based and the safety analyses (cmh, prop_diff,
# lsmeans, lsmeans_diff, km, logrank, abnormal, incidence_rate, regression,
# anova, kruskal, paired_ttest, paired_wilcox, mcnemar, km_diff,
# onesample_ttest, onesample_wilcox, smd, poisson, oneway, aov,
# mcnemar_long, nested_models, regression_basic, cohens_d, hedges_g,
# paired_cohens_d, paired_hedges_g, mood, vif, wald; hierarchical_count,
# records, labels, cards'): each is its function, by
# another name -- the same code; the ARS names the
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
          km_diff = "cardx::ard_survival_survfit_diff",
          onesample_ttest = "cardx::ard_stats_t_test_onesample",
          onesample_wilcox = "cardx::ard_stats_wilcox_test_onesample",
          smd = "cardx::ard_smd_smd",
          poisson = "cardx::ard_stats_poisson_test",
          oneway = "cardx::ard_stats_oneway_test",
          aov = "cardx::ard_stats_aov",
          mcnemar_long = "cardx::ard_stats_mcnemar_test_long",
          records = "cards::ard_tabulate_rows",
          labels = "cards::ard_attributes",
          nested_models = "cardx::ard_stats_anova",
          regression_basic = "cardx::ard_regression_basic",
          cohens_d = "cardx::ard_effectsize_cohens_d",
          hedges_g = "cardx::ard_effectsize_hedges_g",
          paired_cohens_d = "cardx::ard_effectsize_paired_cohens_d",
          paired_hedges_g = "cardx::ard_effectsize_paired_hedges_g",
          mood = "cardx::ard_stats_mood_test",
          vif = "cardx::ard_car_vif",
          wald = "cardx::ard_aod_wald_test")
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
                      "data = data), times = 60")),
    list(method = "onesample_ttest", population_id = "SAF", by = "TRT01A",
         variables = "AGE", args = "mu = 75"),
    list(method = "smd", population_id = "SAF", by = "SEX", variables = "AGE"),
    list(method = "poisson", dataset = "ADTTE", population_id = "SAF",
         variables = "CNSR | AVAL"),
    list(method = "aov", population_id = "SAF",
         args = "formula = AGE ~ TRT01A + SEX"),
    list(method = "mcnemar_long", dataset = "ADLB", population_id = "SAF",
         by = "AVISITN", variables = "LBNRIND", args = "id = USUBJID"),
    list(method = "records", dataset = "ADAE", population_id = "SAF",
         by = "TRTA"),
    list(method = "labels", population_id = "SAF", variables = "AGE | SEX"),
    list(method = "nested_models", population_id = "SAF",
         args = "formulas = list(AGE ~ TRT01A, AGE ~ TRT01A * SEX), method = \"lm\""),
    list(method = "regression_basic", population_id = "SAF",
         args = "formula = AGE ~ TRT01A + SEX, method = \"lm\""),
    list(method = "cohens_d", population_id = "SAF", by = "SEX",
         variables = "AGE"),
    list(method = "paired_hedges_g", dataset = "ADLB", population_id = "SAF",
         by = "AVISITN", variables = "AVAL", args = "id = USUBJID"),
    list(method = "mood", population_id = "SAF", by = "SEX", variables = "AGE"),
    list(method = "vif", population_id = "SAF",
         args = "x = lm(AGE ~ TRT01A + SEX + BMIBL, data = data)"),
    list(method = "wald", population_id = "SAF",
         args = "x = glm(SEX == \"F\" ~ TRT01A + AGE, family = binomial, data = data)"))
  for (r in rows) {
    f <- r
    f$method <- keys$call[match(r$method, keys$method)]
    a <- tfl_ard_code(exact_spec(r), save = FALSE)
    expect_identical(a, tfl_ard_code(exact_spec(f), save = FALSE), label = r$method)
    # (cards' functions are written without cards::, after library(cards))
    expect_true(any(grepl(paste0(sub("^cards::", "", f$method), "("), a, fixed = TRUE)),
                label = r$method)
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
  ard <- expect_no_warning(suppressMessages(tfl_build_ard(sp, dir = dir, save = FALSE)))
  # cardx gives `method` a number of decimals; fmt_ard() keeps it as text
  m <- ard[ard$stat_name == "method", ]
  expect_gt(nrow(m), 0L)
  expect_identical(unlist(m$stat_fmt), unlist(m$stat))
  # and the reference arm, a factor, as its text (no warning)
  rl <- ard[ard$stat_name == "reference_level", ]
  expect_gt(nrow(rl), 0L)
  expect_identical(unlist(rl$stat_fmt), vapply(rl$stat, as.character, ""))
  est <- ard[ard$stat_name == "estimate", ]
  expect_true(all(grepl("^-?[0-9]+[.][0-9]{2}$", unlist(est$stat_fmt))))
})

test_that("hierarchical_count: the nested event counts, with the population as denominator", {
  keys <- tfl_ard_methods()
  k <- match("hierarchical_count", keys$method)
  expect_identical(keys$call[k], "cards::ard_stack_hierarchical_count")
  expect_identical(keys$defaults[k], "denominator = population")
  row <- list(method = "hierarchical_count", dataset = "ADAE",
              population_id = "SAF", by = "TRTA", variables = "AESOC | AEDECOD")
  code <- paste(tfl_ard_code(exact_spec(row), save = FALSE), collapse = "\n")
  # (the program: library(cards); the population is adsl_sub)
  expect_match(code, "ard_stack_hierarchical_count(", fixed = TRUE)
  expect_match(code, "denominator = adsl_sub", fixed = TRUE)
  # a row naming the function and the denominator writes the same call
  f <- row
  f$method <- "cards::ard_stack_hierarchical_count"
  f$args <- "denominator = population"
  expect_identical(tfl_ard_code(exact_spec(row), save = FALSE),
                   tfl_ard_code(exact_spec(f), save = FALSE))
  m <- tfl_ars(exact_spec(row))$methods
  expect_identical(m[[1L]]$id, "Mth_hierarchical_count")
  expect_identical(m[[1L]]$name, "Nested event counts")
  expect_identical(vapply(m[[1L]]$operations, `[[`, "", "label"), "n")
})

test_that("the ARS lays out the one-sample, balance, rate and one-way tests by their keyword", {
  ops <- function(row) {
    m <- tfl_ars(exact_spec(row))$methods[[1L]]
    list(id = m$id, ops = vapply(m$operations, `[[`, "", "label"))
  }
  o <- ops(list(method = "cardx::ard_stats_t_test_onesample", population_id = "SAF",
                by = "TRT01A", variables = "AGE"))
  expect_identical(o$id, "Mth_onesample_ttest")
  expect_identical(o$ops, c("estimate", "conf.low", "conf.high", "p.value"))
  o <- ops(list(method = "smd", population_id = "SAF", by = "SEX", variables = "AGE"))
  expect_identical(o$id, "Mth_smd")
  expect_identical(o$ops, "estimate")
  o <- ops(list(method = "oneway", population_id = "SAF",
                args = "formula = AGE ~ TRT01A"))
  expect_identical(o$id, "Mth_oneway")
  expect_identical(o$ops, "p.value")
  o <- ops(list(method = "mcnemar_long", dataset = "ADLB", population_id = "SAF",
                by = "AVISITN", variables = "LBNRIND", args = "id = USUBJID"))
  expect_identical(o$id, "Mth_mcnemar_long")
})

test_that("the ARS lays out the record counts, effect sizes, Mood's and the model tests by their keyword", {
  ops <- function(row) {
    m <- tfl_ars(exact_spec(row))$methods[[1L]]
    list(id = m$id, name = m$name, ops = vapply(m$operations, `[[`, "", "label"))
  }
  o <- ops(list(method = "cards::ard_tabulate_rows", dataset = "ADAE",
                population_id = "SAF", by = "TRTA"))
  expect_identical(o$id, "Mth_records")
  expect_identical(o$name, "Number of records")
  expect_identical(o$ops, "n")
  o <- ops(list(method = "cardx::ard_effectsize_hedges_g", population_id = "SAF",
                by = "SEX", variables = "AGE"))
  expect_identical(o$id, "Mth_hedges_g")
  expect_identical(o$ops, c("estimate", "conf.low", "conf.high"))
  o <- ops(list(method = "mood", population_id = "SAF", by = "SEX",
                variables = "AGE"))
  expect_identical(o$id, "Mth_mood")
  expect_identical(o$ops, "p.value")
  o <- ops(list(method = "nested_models", population_id = "SAF",
                args = "formulas = list(AGE ~ TRT01A, AGE ~ TRT01A * SEX), method = \"lm\""))
  expect_identical(o$id, "Mth_nested_models")
  expect_identical(o$name, "Test of nested models")
  expect_identical(o$ops, "p.value")
  o <- ops(list(method = "vif", population_id = "SAF",
                args = "x = lm(AGE ~ TRT01A + SEX, data = data)"))
  expect_identical(o$id, "Mth_vif")
})

test_that("a treatment-by-subgroup interaction: nested_models gives the models' p-value", {
  skip_on_cran()
  skip_if_not_installed("cardx")
  skip_if_not_installed("broom")
  skip_if_not_installed("dplyr")
  adam <- exact_data()
  dir <- exact_dir(adam)
  on.exit(unlink(dir, recursive = TRUE))
  sp <- exact_spec(list(method = "nested_models", population_id = "SAF",
                        args = "formulas = list(AGE ~ TRT01A + SEX, AGE ~ TRT01A * SEX), method = \"lm\""))
  ard <- suppressMessages(tfl_build_ard(sp, dir = dir, save = FALSE))
  pop <- adam$ADSL[adam$ADSL$SAFFL == "Y", ]
  want <- stats::anova(stats::lm(AGE ~ TRT01A + SEX, data = pop),
                       stats::lm(AGE ~ TRT01A * SEX, data = pop))
  p <- ard[ard$stat_name == "p.value", ]
  expect_equal(unlist(p$stat)[!is.na(unlist(p$stat))],
               want[["Pr(>F)"]][2L], tolerance = 1e-10)
})
