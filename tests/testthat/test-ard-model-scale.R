# `exponentiate = TRUE` in a regression row, judged by the model the row
# fits (R/ard_exponentiate.R): exp() of a model's estimates is a ratio only
# on a log, logit or complementary log-log scale.

reg_row <- function(args, method = "regression") list(
  method = method, population_id = "SAF", args = args)

test_that("a family's link is read as written", {
  expect_identical(.family_link(quote(binomial)), "logit")
  expect_identical(.family_link(quote(binomial())), "logit")
  expect_identical(.family_link(quote(binomial(link = "probit"))), "probit")
  expect_identical(.family_link(quote(binomial("log"))), "log")
  expect_identical(.family_link(quote(stats::poisson)), "log")
  expect_identical(.family_link(quote(stats::Gamma(link = log))), "log")
  expect_identical(.family_link(quote(Gamma)), "inverse")
  expect_identical(.family_link("quasipoisson"), "log")
  expect_identical(.family_link(quote(gaussian)), "identity")
  # an object of the program, or a family not of stats: not told
  expect_identical(.family_link(quote(fam)), NA_character_)
  expect_identical(.family_link(quote(my_family(link = "log"))), NA_character_)
  expect_identical(.family_link("tweedie"), NA_character_)
})

test_that("exponentiate = TRUE on a model with no ratios is said", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("broom.helpers")
  # a glm with no family: gaussian, exp() of the mean differences
  expect_warning(exact_spec(reg_row(paste(
    "formula = AGE ~ TRT01A, method = \"glm\", exponentiate = TRUE"))),
    "ard_regression\\(exponentiate = TRUE\\): glm\\(\\) with no family \\(gaussian")
  expect_warning(exact_spec(reg_row(paste(
    "formula = I(SEX == \"F\") ~ TRT01A, method = \"glm\",",
    "method.args = list(family = binomial(link = \"identity\")), exponentiate = TRUE"))),
    "glm\\(\\) with `family = binomial\\(link = \"identity\"\\)` \\(its link: identity\\)")
  expect_warning(exact_spec(reg_row(paste(
    "formula = I(SEX == \"F\") ~ TRT01A, method = \"glm\",",
    "method.args = list(family = binomial(link = \"probit\")), exponentiate = TRUE"))),
    "its link: probit")
  # a linear model: lm() is stopped by broom.helpers (the cells empty)
  expect_warning(exact_spec(reg_row(
    "formula = AGE ~ TRT01A, method = \"lm\", exponentiate = TRUE")),
    "lm\\(\\) estimates differences .* the cells would come out empty")
  expect_warning(exact_spec(reg_row(
    "x = stats::aov(AGE ~ TRT01A, data = data), exponentiate = TRUE")),
    "aov\\(\\) estimates differences")
  expect_warning(exact_spec(reg_row(paste(
    "formula = AGE ~ TRT01A, method = \"glm\", exponentiate = TRUE"),
    "cardx::ard_regression_basic")),
    "ard_regression_basic\\(exponentiate = TRUE\\)")
  skip_if_not_installed("survival")
  expect_warning(exact_spec(reg_row(paste(
    "formula = survival::Surv(AGE, SEX == \"F\") ~ TRT01A, method = \"survreg\",",
    "package = \"survival\", exponentiate = TRUE"))),
    "survreg\\(\\) stops on it")
})

test_that("exponentiate = TRUE on a log, logit or cloglog scale is not said", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("broom.helpers")
  ok <- c(
    "formula = I(SEX == \"F\") ~ TRT01A, method = \"glm\", method.args = list(family = binomial)",
    "formula = I(SEX == \"F\") ~ TRT01A, method = \"glm\", method.args = list(family = \"binomial\")",
    "formula = I(SEX == \"F\") ~ TRT01A, method = \"glm\", method.args = list(family = binomial(link = \"log\"))",
    "formula = I(SEX == \"F\") ~ TRT01A, method = \"glm\", method.args = list(family = binomial(link = cloglog))",
    "formula = AGE ~ TRT01A, method = \"glm\", method.args = list(family = poisson)",
    "formula = AGE ~ TRT01A, method = \"glm\", method.args = list(family = Gamma(link = \"log\"))",
    "x = glm(I(SEX == \"F\") ~ TRT01A, family = quasibinomial, data = data)",
    # a family or arguments of the program, a model by a name, a study's own tidier
    "formula = AGE ~ TRT01A, method = \"glm\", method.args = list(family = fam)",
    "formula = AGE ~ TRT01A, method = \"glm\", method.args = my_args",
    "x = fit",
    "formula = AGE ~ TRT01A, method = \"lm\", tidy_fun = broom.helpers::tidy_parameters",
    "formula = AGE ~ TRT01A, method = \"my_model\"")
  for (a in ok) {
    expect_no_warning(exact_spec(reg_row(paste(a, ", exponentiate = TRUE"))))
  }
  # FALSE, or no exponentiate: nothing to say
  expect_no_warning(exact_spec(reg_row(
    "formula = AGE ~ TRT01A, method = \"lm\", exponentiate = FALSE")))
  skip_if_not_installed("survival")
  expect_no_warning(exact_spec(reg_row(paste(
    "formula = survival::Surv(AGE, SEX == \"F\") ~ TRT01A, method = \"coxph\",",
    "package = \"survival\", exponentiate = TRUE"))))
  skip_if_not_installed("MASS")
  expect_no_warning(exact_spec(reg_row(
    "formula = AGE ~ TRT01A, method = \"glm.nb\", package = \"MASS\", exponentiate = TRUE")))
  expect_warning(exact_spec(reg_row(paste(
    "formula = AGE ~ TRT01A, method = \"glm.nb\", package = \"MASS\",",
    "method.args = list(link = identity), exponentiate = TRUE"))),
    "glm.nb\\(\\) with `link = identity`")
  expect_warning(exact_spec(reg_row(paste(
    "formula = factor(AGEGR1) ~ TRT01A, method = \"polr\", package = \"MASS\",",
    "method.args = list(method = \"probit\", Hess = TRUE), exponentiate = TRUE"))),
    "polr\\(\\) with `method = \"probit\"`")
})

test_that("what the check says is what cardx gives", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("broom.helpers")
  d <- cards::ADSL
  d$TRT01A <- factor(d$TRT01A)
  est <- function(ard) unname(unlist(ard$stat[ard$stat_name == "estimate"]))
  # a gaussian glm: exp() of each difference of means, no error kept
  g <- suppressMessages(cardx::ard_regression(
    stats::glm(AGE ~ TRT01A, data = d), exponentiate = TRUE))
  diffs <- unname(stats::coef(stats::lm(AGE ~ TRT01A, data = d))[-1L])
  expect_equal(est(g)[-1L], exp(diffs))
  expect_length(unlist(g$error), 0L)
  # lm(): broom.helpers' error is kept in the ARD, no estimate
  l <- suppressMessages(cardx::ard_regression(
    stats::lm(AGE ~ TRT01A, data = d), exponentiate = TRUE))
  expect_match(unlist(l$error)[1L], "not valid")
  expect_length(Filter(Negate(is.null), l$stat[l$stat_name == "estimate"]), 0L)
  # a logistic model: odds ratios, as stats gives them
  b <- suppressMessages(cardx::ard_regression(
    stats::glm(I(SEX == "F") ~ TRT01A, data = d, family = stats::binomial),
    exponentiate = TRUE))
  or <- exp(unname(stats::coef(stats::glm(I(SEX == "F") ~ TRT01A, data = d,
                                         family = stats::binomial))[-1L]))
  expect_equal(est(b)[-1L], or)
})

test_that("an LS means row of a logistic model left on the link scale is said", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("emmeans")
  ls_row <- function(method, args) list(
    method = method, population_id = "SAF", variables = "TRT01A",
    args = paste("formula = I(SEX == \"F\") ~ TRT01A, method = \"glm\",",
                 "primary_covariate = \"TRT01A\",", args))
  for (m in c("lsmeans", "lsmeans_diff")) {
    expect_warning(exact_spec(ls_row(m, "method.args = list(family = binomial)")),
      paste0("fits glm\\(\\) with a logit link and no response_type .*",
             "log odds, not proportions .*response_type = \"dichotomous\""))
  }
  expect_warning(exact_spec(ls_row("lsmeans", paste(
    "method.args = list(family = poisson), response_type = \"continuous\""))),
    "a log link and response_type = \"continuous\".*logs of the means")
  # dichotomous, a gaussian model, an object of the program: nothing to say
  expect_no_warning(exact_spec(ls_row("lsmeans", paste(
    "method.args = list(family = binomial), response_type = \"dichotomous\""))))
  expect_no_warning(exact_spec(ls_row("lsmeans", "method.args = list(family = gaussian)")))
  expect_no_warning(exact_spec(ls_row("lsmeans", "method.args = list()")))
  expect_no_warning(exact_spec(ls_row("lsmeans", "method.args = list(family = fam)")))
  expect_no_warning(exact_spec(ls_row("lsmeans", paste(
    "method.args = list(family = binomial), response_type = rt"))))
})

test_that("the LS means of a logistic model: log odds unless dichotomous", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("emmeans")
  d <- as.data.frame(cards::ADSL)
  d$F <- as.integer(d$SEX == "F")
  d$TRT01A <- factor(d$TRT01A)
  est <- function(rt) {
    r <- suppressMessages(cardx::ard_emmeans_emmeans(
      d, F ~ TRT01A, method = "glm", method.args = list(family = binomial),
      response_type = rt, primary_covariate = "TRT01A"))
    unname(unlist(r$stat[r$stat_name == "estimate"]))
  }
  p <- as.vector(tapply(d$F, d$TRT01A, mean))
  expect_equal(est("dichotomous"), p, tolerance = 1e-6)
  expect_equal(est("continuous"), stats::qlogis(p), tolerance = 1e-6)
})
