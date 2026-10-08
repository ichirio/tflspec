# What an ARD spec row makes is what the same analysis written by hand
# with cards / cardx makes: every number, under the same groups, variable,
# level and statistic.  The cases (fixtures/ard-cases.R) cover the method
# keywords and cards / cardx functions written as pkg::function --
# hierarchies, survival, models, confidence intervals, tests, strata,
# denominators, subsets.

test_that("each ARD spec row makes what the hand-written call makes", {
  skip_on_cran()
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("dplyr")
  cases <- source(test_path("fixtures", "ard-cases.R"), local = TRUE)$value
  adam <- exact_data()
  dir <- exact_dir(adam)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  for (cs in cases) {
    needs <- exact_needs(cs)
    if (!all(vapply(needs, requireNamespace, NA, quietly = TRUE))) next
    res <- exact_run(cs, adam, dir)
    if (isTRUE(res$skipped)) next     # a package its function needs is not here
    expect_true(isTRUE(res$same), label = paste(cs$id, res$error))
  }
})

test_that("a method that gives several ARDs keeps which is which", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("broom")
  skip_if_not_installed("dplyr")
  sp <- exact_spec(list(method = "cards::ard_pairwise", population_id = "SAF",
                        args = paste("variable = TRT01A, .f = function(df)",
                                     "cardx::ard_stats_t_test(df, by = TRT01A, variables = AGE)")))
  code <- paste(tfl_ard_code(sp, save = FALSE), collapse = "\n")
  # formatted after the call: fmt_ard() makes the ARDs one
  expect_match(code, "fmt_ard(fmt_default)", fixed = TRUE)
  adam <- exact_data()
  dir <- exact_dir(adam)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  ard <- suppressMessages(suppressWarnings(tfl_build_ard(sp, dir = dir,
                                                         save = FALSE)))
  expect_length(unique(ard$pairwise), 3L)
})

test_that("a fitted model given as the first argument takes no data", {
  skip_if_not_installed("cardx")
  skip_if_not_installed("car")
  sp <- exact_spec(list(method = "cardx::ard_car_anova", population_id = "SAF",
                        args = "x = lm(AGE ~ TRT01A, data = data)"))
  code <- paste(tfl_ard_code(sp, save = FALSE), collapse = "\n")
  # `data` in args is the program's data, by its name
  expect_match(code, "cardx::ard_car_anova(x = lm(AGE ~ TRT01A, data = pop_saf))",
               fixed = TRUE)
  # a function with a `data` argument still takes the data first
  sp <- exact_spec(list(method = "cardx::ard_stats_aov", population_id = "SAF",
                        args = "formula = AGE ~ TRT01A"))
  code <- paste(tfl_ard_code(sp, save = FALSE), collapse = "\n")
  expect_match(code, "pop_saf |>
  cardx::ard_stats_aov(", fixed = TRUE)
})

test_that("args are read as R arguments: their order and spacing do not matter", {
  sp <- exact_spec(list(method = "hierarchical", population_id = "SAF",
                        dataset = "ADAE", by = "TRTA",
                        variables = "AESOC | AEDECOD",
                        args = "over_variables = TRUE,denominator=population"))
  code <- paste(tfl_ard_code(sp, save = FALSE), collapse = "\n")
  expect_equal(lengths(regmatches(code, gregexpr("denominator =", code))), 0L)
  expect_equal(lengths(regmatches(code, gregexpr("denominator=", code))), 1L)
  # an argument of a call inside args is not the method's own
  sp <- exact_spec(list(method = "cards::ard_strata", population_id = "SAF",
                        args = ".strata = SEX, .f = function(df) cards::ard_summary(df, by = TRT01A, variables = AGE)"))
  code <- paste(tfl_ard_code(sp, save = FALSE), collapse = "\n")
  expect_match(code, "pop_saf |>
  ard_strata(", fixed = TRUE)
  # args that are not R are refused before any code is made
  expect_error(exact_spec(list(method = "categorical", population_id = "SAF",
                               variables = "SEX", args = "denominator = population)")),
               "does not read as R arguments")
})

test_that("a study's own function is a method, loaded by the study key `source`", {
  sp <- exact_spec(list(method = "ard_riskdiff_newcombe", population_id = "SAF",
                        by = "TRT01A", variables = "SEX"),
                   source = "ard-own.R")
  code <- tfl_ard_code(sp, save = FALSE)
  expect_true("source(\"R/ard-own.R\")" %in% code)
  expect_true("source(\"R/ard-own.R\")" %in%
                tfl_ard_code(sp, part = "setup"))
  expect_match(paste(code, collapse = "\n"),
               "pop_saf |>
  ard_riskdiff_newcombe(", fixed = TRUE)
  # without `source` it still reads, with a warning
  expect_warning(exact_spec(list(method = "ard_riskdiff_newcombe",
                                 population_id = "SAF")),
                 "study key `source`")
  expect_error(suppressWarnings(exact_spec(list(method = "not a function",
                                                population_id = "SAF"))),
               "unknown method")
})

test_that("the own function of the fixtures gives Newcombe's published interval", {
  skip_if_not_installed("cards")
  skip_if_not_installed("dplyr")
  e <- new.env()
  sys.source(test_path("fixtures", "ard-own.R"), e)
  # Newcombe (1998), Stat Med 17:873, example (a): 56/70 vs 48/80,
  # method 10: 0.2000 (0.0524, 0.3339)
  d <- data.frame(g = rep(c("a", "b"), c(70, 80)),
                  y = c(rep("Y", 56), rep("N", 14), rep("Y", 48), rep("N", 32)))
  a <- e$ard_riskdiff_newcombe(d, by = g, variables = y)
  expect_equal(round(unlist(a$stat), 4), c(0.2, 0.0524, 0.3339))
})

test_that("the strata and denominator columns are checked", {
  ok <- list(method = "categorical", population_id = "SAF", variables = "SEX")
  expect_error(exact_spec(c(ok, denominator = "nobody")), "denominator\\(s\\) nobody")
  expect_error(exact_spec(c(ok, denominator = "row", args = "denominator = \"cell\"")),
               "given twice")
  # one argument, one place: statistics and by too
  expect_error(exact_spec(list(method = "continuous", population_id = "SAF",
                               variables = "AGE", statistics = "mean | sd",
                               args = "stat_label = NULL, statistic = ~ list(mean = mean)")),
               "`statistic` is given twice, by the `statistics` column")
  expect_error(exact_spec(c(ok, by = "TRT01A", args = "by = SEX")),
               "`by` is given twice")
  # a method whose statistics only choose what is kept passes no statistic:
  # args may give it
  expect_s3_class(exact_spec(list(method = "proportion_ci", population_id = "SAF",
                                  variables = "SEX", statistics = "estimate",
                                  args = "method = \"wilson\"")), "tfl_ard_spec")
  expect_error(exact_spec(list(method = "custom", population_id = "SAF",
                               code = "cards::ard_tabulate(data, variables = SEX)",
                               strata = "SEX")),
               "takes no `strata`")
  skip_if_not_installed("cards")
  expect_error(exact_spec(list(method = "hierarchical", population_id = "SAF",
                               dataset = "ADAE", variables = "AESOC",
                               strata = "SEX")),
               "takes no `strata`")
  # a definition written before the columns reads as before
  sp <- exact_spec(ok)
  sp$analyses$strata <- NULL
  sp$analyses$denominator <- NULL
  expect_s3_class(tfl_ard_spec(unclass(sp)), "tfl_ard_spec")
})

test_that("the fingerprint follows the study's own function files", {
  sp <- exact_spec(list(method = "ard_riskdiff_newcombe", population_id = "SAF",
                        by = "TRT01A", variables = "SEX"),
                   source = "ard-own.R")
  dir <- tempfile("fp")
  dir.create(file.path(dir, "R"), recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  missing <- tfl_ard_spec_hash(sp, "T", dir = dir)
  writeLines("f <- function() 1", file.path(dir, "R", "ard-own.R"))
  one <- tfl_ard_spec_hash(sp, "T", dir = dir)
  writeLines("f <- function() 2", file.path(dir, "R", "ard-own.R"))
  two <- tfl_ard_spec_hash(sp, "T", dir = dir)
  expect_false(identical(missing, one))
  expect_false(identical(one, two))
  # a column blank in every row does not count
  sp2 <- exact_spec(list(method = "categorical", population_id = "SAF",
                         variables = "SEX"))
  h <- tfl_ard_spec_hash(sp2, "T")
  sp2$analyses$strata <- NULL
  expect_identical(tfl_ard_spec_hash(sp2, "T"), h)
})
