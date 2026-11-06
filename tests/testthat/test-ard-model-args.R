# The arguments of the model a cardx function fits (inst/ard/model_args.csv):
# written in `method.args = list(...)` (ard_survival_survfit(),
# ard_regression(), ard_emmeans_*(), ard_stats_anova()) or in a model's call
# in `x` (ard_survival_survfit_diff(x = survival::survfit(...))), their names
# looked for in the installed model function and their values judged.

test_that("each catalogued model argument is one of the installed function's", {
  d <- .ard_catalog_csv("model_args.csv")
  expect_true(all(d$kind %in% c("logical", "number", "choice", "text", "code",
                                "variable", "family", "list")))
  expect_false(anyDuplicated(paste(d$fn, d$arg)) > 0L)
  for (key in unique(d$fn)) {
    has <- .model_arg_names(key)
    if (is.null(has)) {
      # (a package not installed here)
      expect_false(requireNamespace(sub("::.*$", "", key), quietly = TRUE),
                   info = key)
      next
    }
    for (a in d$arg[d$fn == key]) {
      expect_true(a %in% unlist(has), info = paste(key, a))
    }
  }
  # a choice's default is one of its choices
  ch <- d[d$kind == "choice" & nzchar(d$default), ]
  for (i in seq_len(nrow(ch))) {
    expect_true(ch$default[i] %in% .split_bar(ch$choices[i]),
                info = paste(ch$fn[i], ch$arg[i]))
  }
})

test_that("tfl_ard_model_args() lists the model's arguments, described first by the catalog", {
  skip_if_not_installed("survival")
  s <- tfl_ard_model_args("survfit", "survival")
  expect_identical(unique(s$fn), "survival::survfit")
  # survfit.formula()'s own and survfitKM()'s, where its `...` go
  expect_true(all(c("stype", "ctype", "conf.type", "conf.int", "start.time") %in% s$arg))
  expect_false(any(c("formula", "data", "...") %in% s$arg))
  ct <- s[s$arg == "conf.type", ]
  expect_identical(ct$kind, "choice")
  expect_true(ct$described)
  expect_match(ct$choices, "log-log", fixed = TRUE)
  # a name of a package as "pkg::fun" or `package =`
  expect_identical(tfl_ard_model_args("survival::coxph")$fn[1L], "survival::coxph")
  # the search path for "base": glm is stats'; glm.control()'s too
  g <- tfl_ard_model_args("glm")
  expect_identical(g$fn[1L], "stats::glm")
  expect_true(all(c("family", "maxit", "epsilon") %in% g$arg))
  expect_false(g$described[g$arg == "etastart"])
  # an unknown model: NULL
  expect_null(tfl_ard_model_args("no_such_model_fn"))
  # a company's row
  old <- options(tflspec.ard_model_args = data.frame(
    fn = "stats::glm", arg = "family", default = "binomial", kind = "code",
    hint = "Ours.", choices = ""))
  on.exit(options(old))
  expect_identical(tfl_ard_model_args("glm")$hint[g$arg == "family"], "Ours.")
})

test_that("a model's argument written in method.args is looked for in the model function", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("survival")
  km <- function(args) list(
    method = "km", dataset = "ADTTE", population_id = "SAF", variables = "TRTA",
    args = paste("y = \"survival::Surv(AVAL, 1 - CNSR)\", times = 60,", args))
  # survfitKM() would stop on it
  expect_warning(exact_spec(km("method.args = list(conf.tpye = \"log-log\")")),
                 "ard_survival_survfit\\(method.args = \\) gives `conf.tpye`, which survival .*'s survfit\\(\\) does not have")
  # a value not among the choices
  expect_warning(exact_spec(km("method.args = list(conf.type = \"loglog\")")),
                 "survfit\\(conf.type = \\) is one of \"log\", \"log-log\"")
  expect_warning(exact_spec(km("method.args = list(conf.int = 95)")),
                 "survfit\\(conf.int = \\) is between 0 and 1")
  # cardx gives the formula and the data
  expect_warning(exact_spec(km("method.args = list(data = data)")),
                 "cardx gives the model its data itself")
  # the right names, a beginning R matches (conf.ty is conf.type): none
  expect_no_warning(exact_spec(km(
    "method.args = list(conf.type = \"log-log\", conf.int = 0.9, ctype = 2)")))
  expect_no_warning(exact_spec(km("method.args = list(timefix = FALSE, conf.ty = \"plain\")")))
  # a list made by the program: its names are the program's
  expect_no_warning(exact_spec(km("method.args = my_args")))
})

test_that("the model method and package name the function method.args are checked against", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("survival")
  cox <- function(extra) list(
    method = "regression", dataset = "ADTTE", population_id = "SAF",
    args = paste("formula = survival::Surv(AVAL, 1 - CNSR) ~ TRTA,",
                 "method = \"coxph\", package = \"survival\", exponentiate = TRUE,",
                 extra))
  expect_warning(exact_spec(cox("method.args = list(tise = \"breslow\")")),
                 "ard_regression\\(method.args = \\) gives `tise`, which survival .*'s coxph\\(\\)")
  expect_warning(exact_spec(cox("method.args = list(ties = \"breslov\")")),
                 "coxph\\(ties = \\) is one of \"efron\", \"breslow\", \"exact\"")
  # coxph.control()'s names are coxph()'s
  expect_no_warning(exact_spec(cox("method.args = list(ties = \"breslow\", iter.max = 30)")))
  # glm() with the default package (the search path), glm.control()'s too
  lsm <- function(extra) list(
    method = "lsmeans", population_id = "SAF",
    args = paste("formula = AGE ~ TRT01A + SEX, method = \"glm\",", extra))
  expect_warning(exact_spec(lsm("method.args = list(fmaily = gaussian)")),
                 "gives `fmaily`, which stats .*'s glm\\(\\) does not have")
  expect_no_warning(exact_spec(lsm("method.args = list(family = gaussian, maxit = 50)")))
  expect_no_warning(exact_spec(lsm("method.args = list(family = \"gaussian\")")))
  expect_warning(exact_spec(lsm("method.args = list(family = 1)")),
                 "glm\\(family = \\) is a family")
  # a column not in quotes: "SEX" in quotes would stop the model
  expect_warning(exact_spec(cox("method.args = list(cluster = \"SITEID\")")),
                 "coxph\\(cluster = \\) is a column of the data, not in quotes")
  expect_no_warning(exact_spec(cox("method.args = list(robust = TRUE, cluster = SITEID)")))
  # lm() ignores a name it does not have: said as ignored
  expect_warning(exact_spec(list(
    method = "nested_models", population_id = "SAF",
    args = paste("formulas = list(AGE ~ TRT01A, AGE ~ TRT01A * SEX), method = \"lm\",",
                 "method.args = list(wieghts = NULL)"))),
    "lm\\(\\) does not have .*; it would be ignored")
  # a study's own model function, or one whose `...` take anything: not judged
  own <- list(method = "lsmeans", population_id = "SAF",
              args = paste("formula = AGE ~ TRT01A + SEX, method = \"my_fit\",",
                           "method.args = list(anything = 1)"))
  expect_no_warning(exact_spec(own))
})

test_that("MMRM's degrees of freedom are written in method.args", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("mmrm")
  mmrm_row <- function(extra) list(
    method = "lsmeans_diff", dataset = "ADLB", population_id = "SAF",
    args = paste("formula = AVAL ~ TRTA + AVISIT + us(AVISIT | USUBJID),",
                 "method = \"mmrm\", package = \"mmrm\",", extra))
  # mmrm_control()'s names are mmrm()'s
  expect_no_warning(exact_spec(mmrm_row(
    "method.args = list(method = \"Kenward-Roger\", reml = TRUE)")))
  # mmrm() takes a vector, not a column name: neither form is judged as a column
  expect_no_warning(exact_spec(mmrm_row("method.args = list(weights = WTS)")))
  expect_no_warning(exact_spec(mmrm_row("method.args = list(weights = data$WTS)")))
  expect_warning(exact_spec(mmrm_row("method.args = list(method = \"KR\")")),
                 "mmrm\\(method = \\) is one of \"Satterthwaite\", \"Kenward-Roger\"")
  expect_warning(exact_spec(mmrm_row("method.args = list(metod = \"Kenward-Roger\")")),
                 "gives `metod`, which mmrm .*'s mmrm\\(\\) does not have")
  m <- tfl_ard_model_args("mmrm", "mmrm")
  expect_identical(m$kind[match(c("reml", "method", "vcov"), m$arg)],
                   c("logical", "choice", "choice"))
})

test_that("a model's call written in x has its arguments checked", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("survival")
  kmd <- function(extra) list(
    method = "km_diff", dataset = "ADTTE", population_id = "SAF",
    args = paste0("x = survival::survfit(survival::Surv(AVAL, 1 - CNSR) ~ TRTA,",
                  " data = data", extra, "), times = 60"))
  expect_warning(exact_spec(kmd(", conf.type = \"loglog\"")),
                 "ard_survival_survfit_diff\\(x = survfit\\(\\)\\): survfit\\(conf.type = \\) is one of")
  expect_warning(exact_spec(kmd(", conf.tpye = \"log-log\"")),
                 "gives `conf.tpye`")
  expect_no_warning(exact_spec(kmd(", conf.type = \"log-log\"")))
  expect_no_warning(exact_spec(kmd("")))
  # a model of a plain name (glm is stats')
  expect_warning(exact_spec(list(
    method = "wald", population_id = "SAF",
    args = "x = glm(SEX == \"F\" ~ TRT01A + AGE, fmaily = binomial, data = data)")),
    "ard_aod_wald_test\\(x = glm\\(\\)\\) gives `fmaily`")
})

test_that("a custom row's cardx call has its method.args checked too", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("survival")
  expect_identical(
    .model_args_problems("T / A", "cardx::ard_survival_survfit",
                         "y = Surv(AVAL, CNSR), method.args = list(conf.type = \"loglog\")"),
    paste("T / A: ard_survival_survfit(method.args = ): survfit(conf.type = )",
          "is one of \"log\", \"log-log\", \"plain\", \"none\", \"logit\", \"arcsin\";",
          "got \"loglog\""))
  # not a cardx function: nothing
  expect_identical(.model_args_problems("T / A", "cards::ard_summary",
                                        "method.args = list(x = 1)"), character())
})

test_that("method.args reaches the model: the log-log interval of a KM estimate", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("survival")
  adam <- exact_data()
  dir <- exact_dir(adam)
  on.exit(unlink(dir, recursive = TRUE))
  cs <- list(
    row = list(method = "km", dataset = "ADTTE", population_id = "SAF",
               variables = "TRTA",
               args = paste("y = \"survival::Surv(AVAL, 1 - CNSR)\", times = c(30, 60),",
                            "method.args = list(conf.type = \"log-log\")")),
    hand = paste("cardx::ard_survival_survfit(ADTTE, y = survival::Surv(AVAL, 1 - CNSR),",
                 "variables = \"TRTA\", times = c(30, 60),",
                 "method.args = list(conf.type = \"log-log\"))"))
  r <- exact_run(cs, adam, dir)
  expect_true(isTRUE(r$same), info = r$error)
  # and the interval is not survival's default (log)
  fit <- survival::survfit(survival::Surv(AVAL, 1 - CNSR) ~ TRTA,
                           data = adam$ADTTE, conf.type = "log-log")
  lo <- summary(fit, times = 60)$lower
  built <- suppressMessages(suppressWarnings(
    tfl_build_ard(exact_spec(cs$row), dir = dir, save = FALSE)))
  at60 <- vapply(built$variable_level, function(v) isTRUE(v == 60), NA)
  got <- unlist(built$stat[built$stat_name == "conf.low" & at60])
  expect_length(got, 3L)
  expect_equal(sort(unname(got)), sort(unname(lo)), tolerance = 1e-8)
})
