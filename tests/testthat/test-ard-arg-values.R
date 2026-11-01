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
               "sort_ard_hierarchical", "update_ard_stat_label")) {
    a <- tfl_ard_args(paste0("cards::", fn))
    expect_true(all(nzchar(a$hint[-1L])), label = fn)
  }
  s <- tfl_ard_args("cards::sort_ard_hierarchical")
  expect_identical(s$choices[s$arg == "sort"], "descending | alphanumeric")
})
