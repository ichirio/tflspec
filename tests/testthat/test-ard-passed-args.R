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
    expect_true(d$arg[i] %in% names(formals(f)),
                info = paste(d$passed_to[i], d$arg[i]))
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
  # a name of the function's own, the catalog's or the one reached: none
  expect_no_warning(exact_spec(test_row("ttest", "var.equal = TRUE, paired = FALSE")))
  expect_no_warning(exact_spec(test_row("chisq", "rescale.p = FALSE", "SEX")))
  # a name for a function that passes `...` on again (broom.helpers'
  # tidier, car's methods): not judged
  expect_no_warning(exact_spec(list(
    method = "regression", population_id = "SAF",
    args = "formula = AGE ~ TRT01A, method = \"lm\", tidy_extra = 1")))
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
