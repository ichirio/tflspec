# The values `args` and `post` write, judged by the argument's kind in the
# catalog (tfl_ard_args()): said when the definition is read, a warning as
# for an argument the function does not have.

test_that("a value that does not fit its argument's kind is said", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  ci <- function(args) list(method = "proportion_ci", population_id = "SAF",
                            by = "TRT01A", variables = "SEX", args = args)
  expect_warning(exact_spec(ci("conf.level = 95")),
                 "ard_categorical_ci\\(conf.level = \\) is between 0 and 1")
  expect_warning(exact_spec(ci("conf.level = \"0.9\"")),
                 "conf.level = \\) is a number; got \"0.9\"")
  expect_warning(exact_spec(ci("method = \"wilson_cc\"")),
                 "method = \\) is one of \"waldcc\", \"wald\"")
  expect_warning(exact_spec(ci("method = \"wilson\", value = \"Y\"")),
                 "value = \\) is a list \\(list\\(AGE = 1L\\)\\) or a formula")
  expect_warning(exact_spec(list(method = "hierarchical", population_id = "SAF",
                                 by = "TRT01A", variables = "SEX | AGEGR1",
                                 args = "over_variables = \"yes\"")),
                 "T / A: ard_stack_hierarchical\\(over_variables = \\) is TRUE or FALSE; got \"yes\"")
  # the values that fit, a name and a call pass
  expect_no_warning(exact_spec(ci(paste(
    "conf.level = 0.9, method = \"wilson\", value = list(SEX = \"F\")"))))
  expect_no_warning(exact_spec(ci("conf.level = level")))
  expect_no_warning(exact_spec(ci("conf.level = 1 - 0.05")))
  expect_no_warning(exact_spec(list(method = "continuous", population_id = "SAF",
                                    variables = "AGE",
                                    args = "fmt_fun = everything() ~ 1L")))
})

test_that("the value of each kind is judged", {
  p <- function(arg, code, kind, choices = "", fn = "")
    .arg_value_problem(arg, str2lang(code), kind, choices, fn)
  expect_null(p("overall", "TRUE", "logical"))
  expect_match(p("overall", "1", "logical"), "TRUE or FALSE")
  expect_null(p("times", "c(6, 12, -1)", "number"))
  expect_match(p("times", "c(6, \"12\")", "number"), "is a number")
  expect_null(p("type", "\"risk\"", "choice", "survival | risk | cumhaz"))
  expect_match(p("type", "\"hazard\"", "choice", "survival | risk | cumhaz"),
               "is one of \"survival\", \"risk\", \"cumhaz\"; got \"hazard\"")
  expect_null(p("unit_label", "\"years\"", "text"))
  expect_match(p("unit_label", "100", "text"), "text in quotes")
  expect_null(p("y", "\"survival::Surv(AVAL, 1 - CNSR)\"", "formula"))
  expect_null(p("formula", "AVAL ~ TRT01P", "formula"))
  expect_match(p("formula", "1", "formula"), "is a formula")
  expect_match(p("y", "\"Surv(AVAL,\"", "formula"), "does not read as R")
  expect_null(p("id", "USUBJID", "column"))
  expect_match(p("id", "1", "column"), "a column of the data")
  # code: by its shape
  expect_null(p("method.args", "list(var.equal = TRUE)", "code"))
  expect_match(p("method.args", "\"var.equal\"", "code"), "a list: list")
  expect_null(p("tidy_fun", "broom::tidy", "code"))
  expect_match(p("tidy_fun", "\"tidy\"", "code"), "a function")
  expect_null(p("x", "lm(AGE ~ TRT01A, data = data)", "code"))
  expect_match(p("x", "\"fit\"", "code"), "R that makes the object")
  expect_null(p("stats_to_remove", "c(\"term\", \"label\")", "code"))
  expect_null(p("stats_to_remove", "term", "code"))
  expect_null(p("filter", "n >= 2", "code"))
  expect_match(p("filter", "\"n >= 2\"", "code"), "not in quotes")
  expect_null(p("sort", "\"alphanumeric\"", "code", "descending | alphanumeric"))
  expect_null(p("sort", "list(AEDECOD = \"descending\")", "code",
                "descending | alphanumeric"))
  expect_match(p("sort", "\"frequency\"", "code", "descending | alphanumeric"),
               "\"descending\", \"alphanumeric\"")
  # an argument of any R: not judged; one function's own shape first
  expect_null(p("weights", "\"w\"", "code"))
  expect_null(p("value", "0", "code", fn = "replace_null_statistic"))
  expect_match(p("value", "0", "code", fn = "ard_tabulate_value"), "a list")
  expect_null(p("fmt_fun", "2L", "code", fn = "add_calculated_row"))
  # NA is the program's (a default written out)
  expect_null(p("conf.level", "NA", "number"))
})

test_that("the arguments of one function's own shape are judged", {
  p <- function(arg, code, fn)
    .arg_value_problem(arg, str2lang(code), "code", "", fn)
  # the stratified Wilson weights: numbers between 0 and 1 adding up to 1
  expect_null(p("weights", "c(0.5, 0.3, 0.2)", "ard_categorical_ci"))
  expect_null(p("weights", "rep(0.25, 4L)", "ard_categorical_ci"))
  expect_null(p("weights", "w", "ard_categorical_ci"))
  expect_match(p("weights", "c(0.5, 0.6)", "ard_categorical_ci"),
               "numbers between 0 and 1 that add up to 1")
  expect_match(p("weights", "\"w\"", "ard_categorical_ci"), "add up to 1")
  # the levels a pairwise analysis compares: a vector
  expect_null(p("include", "c(\"Placebo\", \"Drug A\")", "ard_pairwise"))
  expect_null(p("include", "\"Placebo\"", "ard_pairwise"))
  expect_null(p("include", "levels(data$TRT01A)[1:2]", "ard_pairwise"))
  expect_match(p("include", "list(\"Placebo\")", "ard_pairwise"),
               "the levels compared, as a vector")
  expect_match(p("include", "TRUE", "ard_pairwise"), "as a vector")
  # a calculated statistic's format: decimals, "xx.x" or a function
  expect_null(p("fmt_fun", "2L", "add_calculated_row"))
  expect_null(p("fmt_fun", "\"xx.x\"", "add_calculated_row"))
  expect_null(p("fmt_fun", "\"xx.xx%\"", "add_calculated_row"))
  expect_null(p("fmt_fun", "cards::label_round(1)", "add_calculated_row"))
  expect_null(p("fmt_fun", "function(x) format(x, nsmall = 1)", "add_calculated_row"))
  expect_match(p("fmt_fun", "1.5", "add_calculated_row"), "a whole number of decimals")
  expect_match(p("fmt_fun", "\"x.x.x\"", "add_calculated_row"), "a format")
  expect_match(p("fmt_fun", "\"xx%.x\"", "add_calculated_row"), "a format")
  expect_match(p("fmt_fun", "TRUE", "add_calculated_row"), "a format")
  expect_match(p("fmt_fun", "list(se = 2L)", "add_calculated_row"), "a format")
  # the value a NULL statistic gets: one value
  expect_null(p("value", "0", "replace_null_statistic"))
  expect_null(p("value", "\"NE\"", "replace_null_statistic"))
  expect_null(p("value", "NA", "replace_null_statistic"))
  expect_match(p("value", "c(0, 1)", "replace_null_statistic"), "one value")
  expect_match(p("value", "list(0)", "replace_null_statistic"), "one value")
})

test_that("cards takes the formats the check lets through", {
  skip_if_not_installed("cards")
  for (f in c("xx", "xx.x", "xx.xx%", "x.x")) {
    expect_true(.fmt_string_ok(f), label = f)
    expect_true(is.function(cards::alias_as_fmt_fun(f)), label = f)
  }
  for (f in c("x", "x.x.x", "xx%.x", "%xx", "9.9")) {
    expect_false(.fmt_string_ok(f), label = f)
    expect_error(cards::alias_as_fmt_fun(f), label = f)
  }
})

test_that("a post step's calculated row is judged and runs", {
  skip_if_not_installed("cards")
  hier <- function(post) list(method = "continuous", population_id = "SAF",
                              by = "TRT01A", variables = "AGE",
                              statistics = "N | mean | sd", post = post)
  expect_warning(exact_spec(hier(paste(
    "cards::add_calculated_row(expr = sd / sqrt(N), stat_name = \"se\",",
    "fmt_fun = \"x.x.x\")"))),
    "add_calculated_row\\(fmt_fun = \\) is a whole number of decimals")
  expect_warning(exact_spec(hier(
    "cards::update_ard_fmt_fun(stat_names = \"mean\", fmt_fun = \"2 decimals\")")),
    "update_ard_fmt_fun\\(fmt_fun = \\) is a whole number of decimals")
  expect_warning(exact_spec(hier(
    "cards::update_ard_fmt_fun(stat_names = \"mean\", fmt_fun = 2L, filtr = TRUE)")),
    "`post` update_ard_fmt_fun\\(\\) has no argument `filtr`")
  expect_warning(exact_spec(hier("cards::replace_null_statistic(value = c(0, 1))")),
                 "replace_null_statistic\\(value = \\) is one value")
  expect_no_warning(exact_spec(hier(paste(
    "cards::add_calculated_row(expr = sd / sqrt(N), stat_name = \"se\",",
    "fmt_fun = \"xx.xx\") | cards::replace_null_statistic(value = 0)"))))
  # what the check lets through, cards runs
  ard <- cards::ard_summary(cards::ADSL, by = ARM, variables = AGE,
                            statistic = ~ cards::continuous_summary_fns(c("N", "mean", "sd")))
  se <- cards::add_calculated_row(ard, expr = sd / sqrt(N), stat_name = "se",
                                  fmt_fun = "xx.xx")
  se <- cards::apply_fmt_fun(se)
  v <- se$stat_fmt[se$stat_name == "se"]
  expect_true(all(grepl("^ ?[0-9]+[.][0-9]{2}$", unlist(v))))
})

test_that("a function not of cards or cardx is not judged by the catalog's kinds", {
  expect_identical(.ard_arg_value_problems("T / A", "stats::t.test", "mu = \"a\""),
                   character())
})

test_that("a post step's arguments and values are said", {
  skip_if_not_installed("cards")
  hier <- function(post) list(method = "hierarchical", population_id = "SAF",
                              dataset = "ADSL", by = "TRT01A",
                              variables = "SEX | AGEGR1", post = post)
  expect_warning(exact_spec(hier("cards::filter_ard_hierarchical(filtr = n > 1)")),
                 "`post` filter_ard_hierarchical\\(\\) has no argument `filtr`")
  expect_warning(exact_spec(hier("cards::filter_ard_hierarchical(filter = \"n > 1\")")),
                 "filter_ard_hierarchical\\(filter = \\) is an expression")
  expect_warning(exact_spec(hier("cards::sort_ard_hierarchical(sort = \"frequency\")")),
                 "sort_ard_hierarchical\\(sort = \\) is \"descending\"")
  expect_no_warning(exact_spec(hier(paste(
    "cards::filter_ard_hierarchical(filter = n > 1, keep_empty = TRUE) |",
    "cards::sort_ard_hierarchical(sort = \"alphanumeric\")"))))
  # a step of another package, or the study's own, is the program's
  expect_no_warning(exact_spec(hier("dplyr::filter(stat_name != \"N\")")))
})

test_that("the catalog describes the post helpers' arguments", {
  skip_if_not_installed("cards")
  for (fn in c("add_calculated_row", "filter_ard_hierarchical",
               "sort_ard_hierarchical", "update_ard_stat_label",
               "update_ard_fmt_fun")) {
    a <- tfl_ard_args(paste0("cards::", fn))
    expect_true(all(nzchar(a$hint[-1L])), label = fn)
  }
  s <- tfl_ard_args("cards::sort_ard_hierarchical")
  expect_identical(s$choices[s$arg == "sort"], "descending | alphanumeric")
})
