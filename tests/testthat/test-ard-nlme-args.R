# nlme's models in a regression row (R/ard_nlme_args.R): gls() -- repeated
# measures with an unstructured covariance -- and lme(), written in
# ard_regression(x = ) because neither takes `formula =`.  Their arguments
# are catalogued in inst/ard/model_args.csv and judged by their kind.

gls_row <- function(model_args, dataset = "ADLB") list(
  method = "regression", dataset = dataset, population_id = "SAF",
  where = "PARAMCD == \"ALB\"",
  args = paste0("x = nlme::gls(AVAL ~ TRTA * AVISIT, data = data, ", model_args, ")"))

# the warnings a definition gives when read, without the one every gls() /
# lme() regression row gives (its coefficients by term)
nlme_warn <- function(r) {
  w <- character()
  withCallingHandlers(exact_spec(r), warning = function(cnd) {
    w <<- c(w, conditionMessage(cnd))
    invokeRestart("muffleWarning")
  })
  w <- unlist(strsplit(w, "\n", fixed = TRUE))
  w[nzchar(trimws(w)) & !grepl("gives the coefficients by term|not by variable and level|the table lays them out by term|same model with method|\"mmrm\", package", w)]
}

test_that("nlme::gls() in x: its correlation, variance and method are judged", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("nlme")
  # the unstructured covariance SAS PROC MIXED fits: none
  expect_length(nlme_warn(gls_row(paste(
    "correlation = nlme::corSymm(form = ~ AVISITN | USUBJID),",
    "weights = nlme::varIdent(form = ~ 1 | AVISIT), method = \"REML\""))), 0L)
  expect_length(nlme_warn(gls_row(paste(
    "correlation = nlme::corCompSymm(form = ~ 1 | USUBJID), method = \"ML\""))), 0L)
  # (a name, text or a one-sided formula in weights: the program's / varFixed)
  expect_length(nlme_warn(gls_row("weights = ~ AVISITN, verbose = FALSE")), 0L)
  # the records' order as the visit: a missed visit pairs the later ones wrongly
  expect_warning(exact_spec(gls_row("correlation = nlme::corSymm(form = ~ 1 | USUBJID)")),
                 "corSymm\\(form = ~ 1 \\| \\.\\.\\.\\) reads the visits from the records' order")
  expect_warning(exact_spec(gls_row("correlation = corAR1(form = ~ 1 | USUBJID)")),
                 "corAR1\\(form = ~ 1")
  # (corSymm(value, form): form is the second by position)
  expect_warning(exact_spec(gls_row("correlation = corSymm(NULL, ~ 1 | USUBJID)")),
                 "records' order")
  # the visit's text: nlme stops
  expect_warning(exact_spec(gls_row("correlation = corSymm(form = ~ AVISIT | USUBJID)")),
                 "needs the visit as a whole number .* write AVISITN")
  # no subject: the whole data one series
  expect_warning(exact_spec(gls_row("correlation = corCompSymm(form = ~ 1)")),
                 "has no subject after \\|")
  expect_warning(exact_spec(gls_row("correlation = corCompSymm()")),
                 "is given no `form`, so the whole data is one subject's series")
  expect_warning(exact_spec(gls_row("correlation = \"UN\"")),
                 "gls\\(correlation = \\) is an nlme corStruct")
  expect_warning(exact_spec(gls_row("weights = list(AVISIT = 1)")),
                 "gls\\(weights = \\) is an nlme varFunc")
  expect_warning(exact_spec(gls_row("method = \"reml\"")),
                 "gls\\(method = \\) is one of \"REML\", \"ML\"")
  # a name gls() does not have stops it
  expect_warning(exact_spec(gls_row("corelation = corSymm(form = ~ AVISITN | USUBJID)")),
                 "ard_regression\\(x = gls\\(\\)\\) gives `corelation`, which nlme .*'s gls\\(\\) does not have")
})

test_that("a gls() / lme() regression row says its coefficients are by term", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("nlme")
  expect_warning(exact_spec(gls_row("method = \"REML\"")),
                 "gives the coefficients by term .* mmrm")
  expect_warning(exact_spec(list(
    method = "regression", dataset = "ADLB", population_id = "SAF",
    args = paste("x = nlme::gls(AVAL ~ TRTA, data = data),",
                 "add_pairwise_contrasts = TRUE"))),
    "add_pairwise_contrasts = TRUE\\) with x = gls\\(\\): .* would stop on it")
})

test_that("nlme::lme() in x: its random effects are judged", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("nlme")
  lme_row <- function(a) list(
    method = "regression", dataset = "ADLB", population_id = "SAF",
    args = paste0("x = nlme::lme(AVAL ~ TRTA + AVISIT, data = data, ", a, ")"))
  expect_length(nlme_warn(lme_row("random = ~ 1 | USUBJID, method = \"REML\"")), 0L)
  expect_length(nlme_warn(lme_row(paste(
    "random = list(USUBJID = nlme::pdDiag(~ AVISITN)),",
    "correlation = nlme::corAR1(form = ~ AVISITN | USUBJID)"))), 0L)
  expect_warning(exact_spec(lme_row("random = ~ 1")),
                 "lme\\(random = \\) is the random effects with the subject after \\|")
  expect_warning(exact_spec(lme_row("random = ~ 1 | USUBJID, keep.data = \"yes\"")),
                 "lme\\(keep.data = \\) is TRUE or FALSE")
  expect_warning(exact_spec(list(
    method = "regression", dataset = "ADLB", population_id = "SAF",
    args = "x = nlme::lme(\"AVAL ~ TRTA\", data = data, random = ~ 1 | USUBJID)")),
    "lme\\(fixed = \\) is a two-sided formula")
})

test_that("a row that names gls() or lme() in method is told to write the model in x", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("nlme")
  expect_warning(exact_spec(list(
    method = "regression", dataset = "ADLB", population_id = "SAF",
    args = paste("formula = AVAL ~ TRTA, method = \"gls\", package = \"nlme\",",
                 "method.args = list(correlation = nlme::corCompSymm(form = ~ 1 | USUBJID))"))),
    "gls\\(\\) takes its model in `model =`, not `formula =` .* x = nlme::gls\\(model = ")
  expect_warning(exact_spec(list(
    method = "regression", dataset = "ADLB", population_id = "SAF",
    args = "formula = AVAL ~ TRTA, method = nlme::lme")),
    "lme\\(\\) takes its model in `fixed =`")
  # the others: none
  expect_no_warning(exact_spec(list(
    method = "regression", population_id = "SAF",
    args = "formula = AGE ~ TRT01A, method = \"lm\"")))
})

test_that("tfl_ard_model_args() lists gls()'s and lme()'s arguments, described", {
  skip_if_not_installed("nlme")
  g <- tfl_ard_model_args("gls", "nlme")
  expect_identical(g$fn[1L], "nlme::gls")
  expect_true(all(c("model", "correlation", "weights", "method") %in% g$arg))
  expect_true(all(g$described[match(c("model", "correlation", "weights", "method"), g$arg)]))
  expect_identical(g$kind[g$arg == "correlation"], "corstruct")
  l <- tfl_ard_model_args("nlme::lme")
  expect_true(all(c("fixed", "random", "keep.data") %in% l$arg))
  expect_identical(l$kind[l$arg == "random"], "random")
})

test_that("a regression row's gls() gives gls()'s own estimates", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("nlme")
  skip_if_not_installed("broom.helpers")
  skip_if_not_installed("parameters")
  adam <- exact_data()
  dir <- exact_dir(adam)
  on.exit(unlink(dir, recursive = TRUE))
  model <- paste("nlme::gls(AVAL ~ TRTA + AVISIT, data = data,",
                 "correlation = nlme::corSymm(form = ~ AVISITN | USUBJID),",
                 "weights = nlme::varIdent(form = ~ 1 | AVISIT))")
  case <- list(
    row = list(method = "regression", dataset = "ADLB", population_id = "SAF",
               where = "PARAMCD == \"ALB\" & AVISITN %in% c(1, 2, 3)",
               args = paste("x =", model)),
    # (corSymm()'s visit number is 1, 2, 3 ...: weeks 2, 4 and 6)
    ds_derive = "AVISITN = AVISITN / 2",
    hand = paste("cardx::ard_regression(x =", model, ")"))
  # (the definition says its coefficients are by term: tested above)
  r <- suppressWarnings(exact_run(case, adam, dir))
  expect_true(isTRUE(r$same), info = r$error)
  d <- exact_inputs(case$row, adam, ds_derive = case$ds_derive)$data
  fit <- nlme::gls(AVAL ~ TRTA + AVISIT, data = d,
                   correlation = nlme::corSymm(form = ~ AVISITN | USUBJID),
                   weights = nlme::varIdent(form = ~ 1 | AVISIT))
  a <- suppressMessages(suppressWarnings(cardx::ard_regression(fit)))
  # (by term: broom.helpers cannot read gls()'s variables)
  est <- a[startsWith(a$variable, "TRTA") & a$stat_name == "estimate", ]
  expect_equal(unname(unlist(est$stat)),
               unname(stats::coef(fit)[grep("^TRTA", names(stats::coef(fit)))]),
               tolerance = 1e-8)
})
