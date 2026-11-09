# The arguments an ARD function passes on through `...` to the function
# that does the work (inst/ard/passed_args.csv): ard_regression(exponentiate
# =) to broom.helpers::tidy_plus_plus(), ard_car_anova(type =) to
# car::Anova(), the tests' own to stats' tests.

test_that("each passed-on argument is one of the function it is passed to", {
  skip_if_not_installed("cardx")
  d <- .ard_catalog_csv("passed_args.csv")
  expect_true(all(d$kind %in% c("logical", "number", "choice", "text",
                                "columns", "code")))
  for (i in seq_len(nrow(d))) {
    fm <- .ard_formals(d$call[i])
    # the function has `...` and does not take the name itself
    expect_true("..." %in% names(fm), info = d$call[i])
    expect_false(d$arg[i] %in% names(fm), info = paste(d$call[i], d$arg[i]))
    pkg <- sub("::.*$", "", d$passed_to[i])
    fn <- sub("^.*::", "", d$passed_to[i])
    if (!requireNamespace(pkg, quietly = TRUE)) next
    f <- utils::getS3method(fn, "default", optional = TRUE, envir = asNamespace(pkg))
    if (is.null(f)) f <- getExportedValue(pkg, fn)
    has <- names(formals(f))
    # (car::Anova(white.adjust =) is Anova.lm()'s, error.estimate Anova.glm()'s)
    for (cl in c("lm", "glm")) {
      m <- utils::getS3method(fn, cl, optional = TRUE, envir = asNamespace(pkg))
      if (identical(d$passed_to[i], "car::Anova") && is.function(m)) {
        has <- c(has, names(formals(m)))
      }
    }
    expect_true(d$arg[i] %in% has, info = paste(d$passed_to[i], d$arg[i]))
  }
})

test_that("tfl_ard_args() gives the passed-on arguments after the function's own", {
  skip_if_not_installed("cardx")
  a <- tfl_ard_args("cardx::ard_regression")
  own <- a$passed_to == ""
  expect_true(all(c("x", "formula", "method", "tidy_fun") %in% a$arg[own]))
  e <- a[a$arg == "exponentiate", ]
  expect_identical(e$passed_to, "broom.helpers::tidy_plus_plus")
  expect_identical(e$kind, "logical")
  expect_identical(e$default, "FALSE")
  expect_identical(e$column, "args")
  expect_false(e$required)
  # their own come first
  expect_true(max(which(own)) < min(which(!own)))
  # ard_regression_basic()'s own defaults
  b <- tfl_ard_args("cardx::ard_regression_basic")
  expect_identical(b$default[b$arg == "add_reference_rows"], "FALSE")
  t <- tfl_ard_args("cardx::ard_stats_t_test")
  expect_identical(t$choices[t$arg == "alternative"], "two.sided | less | greater")
  # a function with none: an empty passed_to column
  s <- tfl_ard_args("cards::ard_summary")
  expect_true(all(s$passed_to == ""))
  # a company's row
  old <- options(tflspec.ard_passed_args = data.frame(
    call = "cardx::ard_stats_t_test", arg = "paired", passed_to = "stats::t.test",
    default = "FALSE", kind = "logical", hint = "Ours.", choices = ""))
  on.exit(options(old))
  t2 <- tfl_ard_args("cardx::ard_stats_t_test")
  expect_identical(t2$hint[t2$arg == "paired"], "Ours.")
  expect_true("var.equal" %in% t2$arg)
})

test_that("the values of passed-on arguments are judged", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  test_row <- function(method, args, variables = "AGE")
    list(method = method, population_id = "SAF", by = "TRT01A",
         variables = variables, args = args)
  expect_warning(exact_spec(test_row("ttest", "var.equal = \"yes\"")),
                 "ard_stats_t_test\\(var.equal = \\) is TRUE or FALSE")
  expect_warning(exact_spec(test_row("ttest", "alternative = \"two-sided\"")),
                 "alternative = \\) is one of \"two.sided\", \"less\", \"greater\"")
  expect_warning(exact_spec(test_row("chisq", "B = \"2000\"", "SEX")),
                 "ard_stats_chisq_test\\(B = \\) is a number")
  expect_no_warning(exact_spec(test_row("ttest", "var.equal = TRUE, mu = 0")))
  expect_no_warning(exact_spec(test_row("chisq", "correct = FALSE", "SEX")))
  expect_no_warning(exact_spec(test_row("cmh", "exact = TRUE", "SEX")))
  # car::Anova()'s type as a word or a number
  p <- function(code) .arg_value_problem("type", str2lang(code), "choice",
                                         "II | III | 2 | 3")
  expect_null(p("\"III\""))
  expect_null(p("3"))
  expect_match(p("4"), "is one of \"II\", \"III\", \"2\", \"3\"; got 4")
})

test_that("a passed-on argument reaches the function: no continuity correction, hazard ratios", {
  skip_on_cran()
  skip_if_not_installed("cardx")
  skip_if_not_installed("dplyr")
  adam <- exact_data()
  dir <- exact_dir(adam)
  on.exit(unlink(dir, recursive = TRUE))
  run <- function(r) suppressMessages(tfl_build_ard(exact_spec(r), dir = dir, save = FALSE))
  stat <- function(ard, nm) unname(unlist(ard$stat[ard$stat_name == nm]))
  pop <- adam$ADSL[adam$ADSL$SAFFL == "Y", ]
  # a 2 x 2 table: chisq.test()'s correct = FALSE
  ard <- run(list(method = "chisq", population_id = "SAF", by = "EFFFL",
                  variables = "SEX", args = "correct = FALSE"))
  want <- suppressWarnings(stats::chisq.test(pop$SEX, pop$EFFFL, correct = FALSE))
  expect_equal(stat(ard, "p.value"), want$p.value, tolerance = 1e-10)
  expect_false(isTRUE(all.equal(stat(ard, "p.value"),
    suppressWarnings(stats::chisq.test(pop$SEX, pop$EFFFL))$p.value)))
  # a Cox model: tidy_plus_plus()'s exponentiate = TRUE, the hazard ratios
  skip_if_not_installed("survival")
  skip_if_not_installed("broom.helpers")
  ard <- run(list(method = "cardx::ard_regression", population_id = "SAF",
                  dataset = "ADTTE", args = paste(
                    "formula = survival::Surv(AVAL, 1 - CNSR) ~ TRTA,",
                    "method = \"coxph\", package = \"survival\", exponentiate = TRUE")))
  tte <- adam$ADTTE[adam$ADTTE$USUBJID %in% pop$USUBJID, ]
  fit <- survival::coxph(survival::Surv(AVAL, 1 - CNSR) ~ TRTA, data = tte)
  # (the reference arm's row: 1)
  est <- stat(ard, "estimate")
  expect_equal(sort(est[!is.na(est)]), sort(c(1, unname(exp(stats::coef(fit))))),
               tolerance = 1e-6)
})

test_that("the effect sizes', Mood's, the SMD's and the CI's passed-on arguments reach their functions", {
  skip_on_cran()
  skip_if_not_installed("cardx")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("broom", "1.0.8")
  adam <- exact_data()
  dir <- exact_dir(adam)
  on.exit(unlink(dir, recursive = TRUE))
  run <- function(method, args, by = "SEX") suppressMessages(tfl_build_ard(exact_spec(list(
    method = method, population_id = "SAF", by = by, variables = "AGE", args = args)),
    dir = dir, save = FALSE))
  stat <- function(ard, nm) unname(unlist(ard$stat[ard$stat_name == nm]))
  pop <- adam$ADSL[adam$ADSL$SAFFL == "Y", ]
  # Mood's test, one-sided
  ard <- run("cardx::ard_stats_mood_test", "alternative = \"greater\"")
  want <- stats::mood.test(AGE ~ SEX, data = pop, alternative = "greater")
  expect_equal(stat(ard, "p.value"), want$p.value, tolerance = 1e-10)
  # the SMD against the second group: its sign turns
  skip_if_not_installed("smd")
  s1 <- stat(run("cardx::ard_smd_smd", "gref = 1L"), "estimate")
  s2 <- stat(run("cardx::ard_smd_smd", "gref = 2L"), "estimate")
  expect_equal(s1, -s2, tolerance = 1e-10)
  # Cohen's d with each group's own variance (pooled_sd = FALSE)
  skip_if_not_installed("effectsize", "0.8.8")
  skip_if_not_installed("parameters")
  d1 <- stat(run("cardx::ard_effectsize_cohens_d", "pooled_sd = FALSE"), "estimate")
  want <- effectsize::cohens_d(AGE ~ SEX, data = pop, pooled_sd = FALSE)
  expect_equal(d1, want$Cohens_d, tolerance = 1e-10)
  expect_false(isTRUE(all.equal(d1, effectsize::cohens_d(AGE ~ SEX, data = pop)$Cohens_d)))
})

test_that("the median's CI takes wilcox.test()'s exact and correct", {
  skip_on_cran()
  skip_if_not_installed("cardx")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("broom", "1.0.8")
  adam <- exact_data()
  dir <- exact_dir(adam)
  on.exit(unlink(dir, recursive = TRUE))
  ard <- suppressMessages(tfl_build_ard(exact_spec(list(
    method = "cardx::ard_continuous_ci", population_id = "SAF", variables = "AGE",
    args = "method = \"wilcox.test\", exact = FALSE, correct = FALSE")),
    dir = dir, save = FALSE))
  pop <- adam$ADSL[adam$ADSL$SAFFL == "Y", ]
  want <- stats::wilcox.test(pop$AGE, conf.int = TRUE, exact = FALSE, correct = FALSE)
  expect_equal(unname(unlist(ard$stat[ard$stat_name == "conf.low"])),
               want$conf.int[1L], tolerance = 1e-10)
  expect_warning(exact_spec(list(method = "cardx::ard_continuous_ci",
                                 population_id = "SAF", variables = "AGE",
                                 args = "exact = \"no\"")),
                 "ard_continuous_ci\\(exact = \\) is TRUE or FALSE")
})

test_that("a passed-on name the function it reaches does not have is said", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  test_row <- function(method, args, variables = "AGE")
    list(method = method, population_id = "SAF", by = "TRT01A",
         variables = variables, args = args)
  # t.test.default() ignores a name it does not have: the typo is said
  expect_warning(exact_spec(test_row("ttest", "var.eqaul = TRUE")),
                 "ard_stats_t_test\\(\\) has no argument `var.eqaul`, and passes it on to stats::t.test\\(\\)")
  expect_warning(exact_spec(test_row("chisq", "corect = FALSE", "SEX")),
                 "passes it on to stats::chisq.test\\(\\), which has none either")
  # a name R matches by its beginning is the argument's (var.eq is var.equal)
  expect_no_warning(exact_spec(test_row("ttest", "var.eq = TRUE")))
  expect_no_warning(exact_spec(test_row("chisq", "resc = FALSE", "SEX")))
  expect_warning(exact_spec(test_row("ttest", "var.eqaul = TRUE")),
                 "has no argument `var.eqaul`")
  expect_identical(
    .passed_args_unknown("T / A", "cardx::ard_stats_t_test", "var.eq", "var.eq = TRUE"),
    character())
  # a name of the function's own, the catalog's or the one reached: none
  expect_no_warning(exact_spec(test_row("ttest", "var.equal = TRUE, paired = FALSE")))
  expect_no_warning(exact_spec(test_row("chisq", "rescale.p = FALSE", "SEX")))
  # a name for a study's own tidier (`tidy_fun =`): not judged
  expect_no_warning(exact_spec(list(
    method = "regression", population_id = "SAF",
    args = paste("formula = AGE ~ TRT01A, method = \"lm\",",
                 "tidy_fun = my_tidier, tidy_extra = 1"))))
  # the median's CI reaches t.test() or wilcox.test(): either one's names
  expect_no_warning(exact_spec(test_row(
    "mean_ci", "method = \"wilcox.test\", exact = FALSE, correct = FALSE")))
  expect_warning(exact_spec(test_row("mean_ci", "exatc = FALSE")),
                 "stats::t.test\\(\\) / stats::wilcox.test\\(\\)")
})

test_that("a name passed on to car::Anova() is looked for in the model's method", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("car")
  anova_row <- function(args) list(method = "anova", population_id = "SAF",
                                   args = args)
  # Anova.lm() ignores a name it does not have: type II, silently
  expect_warning(exact_spec(anova_row(
    "x = lm(AGE ~ TRT01A + SEX, data = data), tpye = \"III\"")),
    "ard_car_anova\\(\\) has no argument `tpye`, and passes it on to car's Anova.lm\\(\\)")
  # a name R matches by its beginning is the argument's (typ = is type =)
  expect_no_warning(exact_spec(anova_row(
    "x = lm(AGE ~ TRT01A + SEX, data = data), typ = \"III\"")))
  expect_warning(exact_spec(anova_row(paste(
    "x = stats::glm(I(SEX == \"F\") ~ TRT01A, family = binomial, data = data),",
    "test.statistc = \"Wald\""))),
    "Anova.glm\\(\\)")
  # the method's own names, the catalog's: none
  expect_no_warning(exact_spec(anova_row(
    "x = lm(AGE ~ TRT01A + SEX, data = data), type = \"III\", white.adjust = \"hc3\"")))
  expect_no_warning(exact_spec(anova_row(paste(
    "x = glm(I(SEX == \"F\") ~ TRT01A, family = binomial, data = data),",
    "test.statistic = \"Wald\", error.estimate = \"pearson\""))))
  # a model by a name, or one car has no method for: not judged
  expect_no_warning(exact_spec(anova_row("x = fit, tpye = 3")))
  expect_no_warning(exact_spec(anova_row("x = my_model(data), tpye = 3")))
})

test_that("a name passed on to tidy_plus_plus() is looked for in it and in broom's tidier", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("broom")
  skip_if_not_installed("broom.helpers")
  reg_row <- function(args, method = "regression") list(
    method = method, population_id = "SAF", args = args)
  glm_args <- paste("formula = I(SEX == \"F\") ~ TRT01A, method = \"glm\",",
                    "method.args = list(family = binomial)")
  # broom's tidy.glm() ignores a name it does not have: log odds ratios
  expect_warning(exact_spec(reg_row(paste(glm_args, ", exponentaite = TRUE"))),
    paste0("ard_regression\\(\\) has no argument `exponentaite`, and passes it on to ",
           "broom.helpers::tidy_plus_plus\\(\\) and broom's tidy.glm\\(\\)"))
  expect_warning(exact_spec(reg_row(paste(glm_args, ", add_pairwise_contarsts = TRUE"),
                                    "cardx::ard_regression_basic")),
    "ard_regression_basic\\(\\) has no argument `add_pairwise_contarsts`")
  expect_warning(exact_spec(reg_row(
    "formula = AGE ~ TRT01A, method = \"lm\", tidy_extra = 1")),
    "broom's tidy.lm\\(\\)")
  # tidy_plus_plus()'s own names, the catalog's, the tidier's: none; a name
  # R matches by its beginning is tidy_plus_plus()'s (exponent = )
  expect_no_warning(exact_spec(reg_row(paste(
    glm_args, ", exponent = TRUE, add_pairwise_contrasts = TRUE,",
    "contrasts_adjust = \"none\", quiet = TRUE, group_by = NULL"))))
  # a model in `x`: its tidier (survival's Cox model, tidy.coxph())
  skip_if_not_installed("survival")
  expect_warning(exact_spec(reg_row(paste(
    "x = survival::coxph(survival::Surv(AGE, SEX == \"F\") ~ TRT01A, data = data),",
    "exponentiaet = TRUE"))),
    "has no argument `exponentiaet`.*broom's tidy.coxph\\(\\)")
  expect_identical(.tidy_args_unknown(
    "T / A", "cardx::ard_regression", "exponentiaet",
    "method = \"coxph\", package = \"survival\", exponentiaet = TRUE",
    character()) |> length(), 1L)
  # a model by a name, a study's own model or tidier: not judged
  expect_no_warning(exact_spec(reg_row("x = fit, exponentaite = TRUE")))
  expect_no_warning(exact_spec(reg_row(
    "formula = AGE ~ TRT01A, method = \"my_model\", exponentaite = TRUE")))
  expect_identical(.tidy_args_unknown(
    "T / A", "cardx::ard_regression", "exponentaite",
    "method = \"lm\", tidy_fun = broom.helpers::tidy_parameters, exponentaite = TRUE",
    character()), character())
})

test_that("tidy_plus_plus()'s pairwise contrasts, reference rows and labels are judged", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  reg_row <- function(args) list(method = "regression", population_id = "SAF",
    args = paste("formula = AGE ~ TRT01A + SEX, method = \"lm\",", args))
  expect_warning(exact_spec(reg_row("add_pairwise_contrasts = \"yes\"")),
                 "ard_regression\\(add_pairwise_contrasts = \\) is TRUE or FALSE")
  expect_warning(exact_spec(reg_row("contrasts_adjust = \"tukeyhsd\"")),
                 "ard_regression\\(contrasts_adjust = \\) is one of \"none\", \"tukey\"")
  expect_warning(exact_spec(reg_row("emmeans_args = \"response\"")),
                 "ard_regression\\(emmeans_args = \\) is a list")
  expect_warning(exact_spec(reg_row("term_labels = c(\"Male\")")),
                 "ard_regression\\(term_labels = \\) is labels by name")
  expect_warning(exact_spec(reg_row("variable_labels = \"Treatment\"")),
                 "ard_regression\\(variable_labels = \\) is a list")
  expect_warning(exact_spec(reg_row("no_reference_row = 1")),
                 "ard_regression\\(no_reference_row = \\) is a column")
  expect_no_warning(exact_spec(reg_row(paste(
    "add_pairwise_contrasts = TRUE, pairwise_variables = \"TRT01A\",",
    "contrasts_adjust = \"none\", pairwise_reverse = FALSE, keep_model_terms = TRUE,",
    "emmeans_args = list(type = \"response\"), no_reference_row = \"SEX\",",
    "add_estimate_to_reference_rows = FALSE, term_labels = c(SEXM = \"Male\"),",
    "variable_labels = list(TRT01A = \"Treatment\"),",
    "categorical_terms_pattern = \"{level} vs {reference_level}\",",
    "interaction_sep = \" x \""))))
  a <- tfl_ard_args("cardx::ard_regression")
  expect_identical(a$passed_to[a$arg == "contrasts_adjust"], "broom.helpers::tidy_plus_plus")
  b <- tfl_ard_args("cardx::ard_regression_basic")
  # ard_regression_basic()'s own default
  expect_identical(b$default[b$arg == "add_estimate_to_reference_rows"], "FALSE")
})

test_that("a row's pairwise contrasts are each pair of arms' difference (emmeans)", {
  skip_on_cran()
  skip_if_not_installed("cardx")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("broom.helpers")
  skip_if_not_installed("emmeans")
  adam <- exact_data()
  dir <- exact_dir(adam)
  on.exit(unlink(dir, recursive = TRUE))
  ard <- suppressMessages(tfl_build_ard(exact_spec(list(
    method = "regression", population_id = "SAF", args = paste(
      "formula = AGE ~ TRT01A, method = \"lm\", add_pairwise_contrasts = TRUE,",
      "contrasts_adjust = \"none\""))), dir = dir, save = FALSE))
  pop <- adam$ADSL[adam$ADSL$SAFFL == "Y", ]
  m <- tapply(pop$AGE, factor(pop$TRT01A), mean)
  want <- unlist(lapply(seq_along(m)[-1L], function(j)
    vapply(seq_len(j - 1L), function(i) m[[j]] - m[[i]], 0)))
  est <- unname(unlist(ard$stat[ard$stat_name == "estimate"]))
  expect_length(est, length(want))
  # (the sign follows emmeans' order of the pair: pairwise_reverse)
  expect_equal(sort(abs(est)), sort(abs(want)), tolerance = 1e-8)
  # pairwise_reverse = FALSE: each pair the other way round
  ard2 <- suppressMessages(tfl_build_ard(exact_spec(list(
    method = "regression", population_id = "SAF", args = paste(
      "formula = AGE ~ TRT01A, method = \"lm\", add_pairwise_contrasts = TRUE,",
      "contrasts_adjust = \"none\", pairwise_reverse = FALSE"))), dir = dir, save = FALSE))
  expect_equal(unname(unlist(ard2$stat[ard2$stat_name == "estimate"])), -est,
               tolerance = 1e-8)
  # unadjusted: the p-value of one pair is the model's t test of it
  fit <- stats::lm(AGE ~ factor(TRT01A), data = pop)
  p <- unname(unlist(ard$stat[ard$stat_name == "p.value"]))
  expect_true(any(abs(p - summary(fit)$coefficients[2L, 4L]) < 1e-8))
})

test_that("Hodges-Lehmann, the CMH odds ratio's level, Fisher's options and a robust ANCOVA are judged", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  row <- function(method, args, by = "TRT01A", variables = "AGE", strata = NULL)
    list(method = method, population_id = "SAF", by = by, variables = variables,
         strata = strata, args = args)
  # the values, by kind (and a floor)
  expect_warning(exact_spec(row("cardx::ard_stats_wilcox_test", "conf.int = \"yes\"", "SEX")),
                 "ard_stats_wilcox_test\\(conf.int = \\) is TRUE or FALSE")
  expect_warning(exact_spec(row("cardx::ard_stats_wilcox_test", "digits.rank = 0", "SEX")),
                 "digits.rank = \\) is a number above 0")
  expect_warning(exact_spec(row("cardx::ard_stats_mantelhaen_test", "conf.level = 90",
                                variables = "EFFFL", by = "SEX", strata = "AGEGR1")),
                 "ard_stats_mantelhaen_test\\(conf.level = \\) is between 0 and 1")
  expect_warning(exact_spec(row("cardx::ard_stats_fisher_test", "or = 0", variables = "SEX")),
                 "ard_stats_fisher_test\\(or = \\) is a number above 0")
  expect_warning(exact_spec(row("cardx::ard_stats_fisher_test", "workspace = 0", variables = "SEX")),
                 "workspace = \\) is a number at least 1")
  expect_warning(exact_spec(row("cardx::ard_stats_fisher_test", "B = 0", variables = "SEX")),
                 "B = \\) is a number at least 1")
  expect_no_warning(exact_spec(row("cardx::ard_stats_fisher_test",
    "conf.int = FALSE, or = 2, workspace = 2e7, hybrid = TRUE", variables = "SEX")))
  expect_no_warning(exact_spec(row("cardx::ard_stats_paired_wilcox_test",
    "conf.int = TRUE, digits.rank = 7", by = NULL)))
  # car::Anova(white.adjust =): FALSE, TRUE or one of the HC types
  p <- function(code) .arg_value_problem("white.adjust", str2lang(code), "choice",
                                         "FALSE | TRUE | hc3 | hc0 | hc1 | hc2 | hc4")
  expect_null(p("TRUE"))
  expect_null(p("\"hc3\""))
  expect_match(p("\"HC3\""), "is one of \"FALSE\", \"TRUE\", \"hc3\"")
  a <- tfl_ard_args("cardx::ard_car_anova")
  expect_identical(a$kind[a$arg == "white.adjust"], "choice")
  expect_identical(a$kind[a$arg == "singular.ok"], "logical")
})

test_that("a catalogued name car's method of the model does not have is said", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("car")
  anova_row <- function(args) list(method = "anova", population_id = "SAF",
                                   args = args)
  # Anova.glm() has no white.adjust: the robust covariance would be ignored
  expect_warning(exact_spec(anova_row(paste(
    "x = glm(I(SEX == \"F\") ~ TRT01A, family = binomial, data = data),",
    "white.adjust = \"hc3\""))),
    "has no argument `white.adjust`, and passes it on to car's Anova.glm\\(\\)")
  # Anova.lm() has no test.statistic (an lm's test is F)
  expect_warning(exact_spec(anova_row(
    "x = lm(AGE ~ TRT01A + SEX, data = data), test.statistic = \"Wald\"")),
    "has no argument `test.statistic`, and passes it on to car's Anova.lm\\(\\)")
  expect_no_warning(exact_spec(anova_row(
    "x = lm(AGE ~ TRT01A + SEX, data = data), white.adjust = TRUE, singular.ok = FALSE")))
})

test_that("the passed-on Hodges-Lehmann estimate, CMH level, Fisher's odds ratio and HC3 ANCOVA reach their functions", {
  skip_on_cran()
  skip_if_not_installed("cardx")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("broom", "1.0.8")
  adam <- exact_data()
  dir <- exact_dir(adam)
  on.exit(unlink(dir, recursive = TRUE))
  run <- function(r) suppressMessages(tfl_build_ard(exact_spec(c(
    list(population_id = "SAF"), r)), dir = dir, save = FALSE))
  stat <- function(ard, nm) unname(unlist(ard$stat[ard$stat_name == nm]))
  pop <- adam$ADSL[adam$ADSL$SAFFL == "Y", ]
  # Wilcoxon: the Hodges-Lehmann shift and its interval (conf.int = TRUE)
  ard <- run(list(method = "cardx::ard_stats_wilcox_test", by = "SEX",
                  variables = "AGE", args = "conf.int = TRUE, exact = FALSE"))
  want <- stats::wilcox.test(AGE ~ SEX, data = pop, conf.int = TRUE, exact = FALSE)
  expect_equal(stat(ard, "estimate"), unname(want$estimate), tolerance = 1e-8)
  expect_equal(stat(ard, "conf.low"), want$conf.int[1L], tolerance = 1e-8)
  # CMH: the common odds ratio's 90% interval
  ard <- run(list(method = "cardx::ard_stats_mantelhaen_test", by = "SEX",
                  variables = "EFFFL", strata = "AGEGR1", args = "conf.level = 0.9"))
  want <- stats::mantelhaen.test(table(pop$SEX, pop$EFFFL, pop$AGEGR1),
                                 conf.level = 0.9)
  expect_equal(stat(ard, "conf.low"), want$conf.int[1L], tolerance = 1e-8)
  expect_equal(stat(ard, "conf.high"), want$conf.int[2L], tolerance = 1e-8)
  # Fisher: the null odds ratio moves the p-value, not the estimate
  ard <- run(list(method = "cardx::ard_stats_fisher_test", by = "SEX",
                  variables = "EFFFL", args = "or = 2"))
  want <- stats::fisher.test(pop$EFFFL, pop$SEX, or = 2)
  expect_equal(stat(ard, "p.value"), want$p.value, tolerance = 1e-10)
  # ANCOVA with the HC3 covariance (car::Anova.lm(white.adjust = "hc3"))
  skip_if_not_installed("car")
  ard <- run(list(method = "anova", args = paste(
    "x = lm(AGE ~ TRT01A + BMIBL, data = data), type = \"III\", white.adjust = \"hc3\"")))
  fit <- stats::lm(AGE ~ TRT01A + BMIBL, data = pop)
  want <- suppressMessages(car::Anova(fit, type = "III", white.adjust = "hc3"))
  got <- ard[ard$variable == "TRT01A" & ard$stat_name == "p.value", ]
  expect_equal(unname(unlist(got$stat)), want["TRT01A", "Pr(>F)"], tolerance = 1e-10)
  expect_false(isTRUE(all.equal(unname(unlist(got$stat)),
    car::Anova(fit, type = "III")["TRT01A", "Pr(>F)"])))
})
