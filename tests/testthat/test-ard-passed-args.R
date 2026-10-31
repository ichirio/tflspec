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
