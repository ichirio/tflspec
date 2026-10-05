# The catalog of ARD functions (inst/ard/functions.csv, args.csv) against
# the installed cards / cardx: a function or argument it names exists, and
# every ard_*() they export is described -- so a new version that adds or
# renames one is noticed here.

test_that("every ard_*() of the installed cards / cardx is in the catalog", {
  skip_on_cran()
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  f <- tfl_ard_functions()
  expect_identical(f$call[!f$in_catalog], character(0),
                   info = "add them to inst/ard/functions.csv")
})

test_that("what the catalog names exists", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  f <- tfl_ard_functions(installed = FALSE)
  expect_false(anyDuplicated(f$call) > 0)
  expect_true(all(nzchar(f$category) & nzchar(f$label) & nzchar(f$shape)))
  expect_true(all(f$shape %in% c("variables", "formula", "model", "columns",
                                 "wrapper", "data")))
  expect_identical(f$call[!f$installed], character(0))
  # an old name points at a function the catalog has
  old <- f$replaced_by[nzchar(f$replaced_by)]
  expect_true(all(old %in% f$call[!nzchar(f$replaced_by)]))
  # every argument a row of args.csv names is one of its function's
  a <- .ard_catalog_csv("args.csv")
  own <- a[a$call != "*", ]
  for (i in seq_len(nrow(own))) {
    expect_true(own$arg[i] %in% names(.ard_formals(own$call[i])),
                label = paste(own$call[i], own$arg[i]))
  }
  kinds <- c("data", "columns", "column", "levels", "denominator",
             "statistics", "number", "logical", "choice", "text", "formula",
             "code")
  expect_true(all(a$kind %in% kinds))
})

test_that("tfl_ard_args() reads the installed function's own arguments", {
  skip_if_not_installed("cardx")
  a <- tfl_ard_args("cardx::ard_categorical_ci")
  expect_identical(a$arg[1:2], c("data", "variables"))
  expect_identical(a$required[1:2], c(TRUE, TRUE))
  expect_identical(a$kind[1], "data")
  expect_identical(a$column[match(c("by", "variables", "strata", "denominator"),
                                  a$arg)],
                   c("by", "variables", "strata", "denominator"))
  # choices from a character vector default
  m <- a[a$arg == "method", ]
  expect_identical(m$kind, "choice")
  expect_match(m$choices, "^waldcc \\| wald \\| clopper-pearson")
  expect_identical(a$default[a$arg == "conf.level"], "0.95")
  # the data a data.frame method takes first, whatever its name
  s <- tfl_ard_args("cardx::ard_survival_survfit")
  expect_identical(s$kind[s$arg == "x"], "data")
  expect_identical(s$choices[s$arg == "type"], "survival | risk | cumhaz")
  # the data second (a formula first)
  v <- tfl_ard_args("cardx::ard_stats_aov")
  expect_identical(v$kind, c("formula", "data"))
  expect_identical(v$column, c("args", "data"))
  # an argument the catalog does not describe: its kind from its default
  k <- tfl_ard_args("cards::ard_stack")
  expect_identical(k$kind[k$arg == ".overall"], "logical")
  expect_error(tfl_ard_args("cards::ard_nothing"), "cannot be found")
  expect_error(tfl_ard_args(c("a", "b")), "one function")
})

test_that("a company's rows describe its own functions and win over the built-in", {
  skip_if_not_installed("cards")
  old <- options(
    tflspec.ard_functions = data.frame(
      call = c("cards::ard_summary", "ard_riskdiff_mn"),
      category = c("Summaries", "Company"),
      label = c("Descriptive statistics", "Risk difference (MN)"),
      description = c("Our wording.", "Miettinen-Nurminen risk difference."),
      shape = c("variables", "variables")),
    tflspec.ard_args = data.frame(call = c("*", "cards::ard_summary"),
                                  arg = c("by", "variables"),
                                  kind = c("columns", "columns"),
                                  hint = c("The arm.", "Numbers only.")))
  on.exit(options(old))
  f <- tfl_ard_functions(installed = FALSE)
  expect_identical(f$label[f$call == "cards::ard_summary"], "Descriptive statistics")
  expect_identical(sum(f$call == "cards::ard_summary"), 1L)
  expect_false(f$installed[f$call == "ard_riskdiff_mn"])
  expect_false("ard_riskdiff_mn" %in% tfl_ard_functions()$call)
  a <- tfl_ard_args("cards::ard_summary")
  expect_identical(a$hint[a$arg == "by"], "The arm.")
  expect_identical(a$hint[a$arg == "variables"], "Numbers only.")
})

test_that("the study ARD stays a cards ARD, ids in front", {
  skip_on_cran()
  skip_if_not_installed("cards")
  adam <- exact_data()
  dir <- exact_dir(adam)
  sp <- exact_spec(list(method = "continuous", dataset = "ADSL",
                        population_id = "SAF", by = "ARM",
                        variables = "AGE | BMIBL"))
  ard <- suppressMessages(tfl_build_ard(sp, dir = dir, save = FALSE))
  expect_s3_class(ard, "card")
  expect_identical(names(ard)[1:3], c("output_id", "analysis_id", "population_id"))
  # so cards' own tools take it
  nl <- cards::as_nested_list(ard)
  expect_setequal(names(nl$variable), c("AGE", "BMIBL"))
  expect_true(cards::is_ard_equal(cards::compare_ard(ard, ard)))
  # the numbers are those of the call written by hand
  hand <- cards::ard_continuous(subset(adam$ADSL, SAFFL == "Y"), by = ARM,
                                variables = c(AGE, BMIBL))
  expect_identical(exact_numbers(ard), exact_numbers(hand))
})

test_that("what a new analysis is not to choose is marked", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  f <- tfl_ard_functions()
  off <- f$call[!f$offered]
  expect_true(all(c("cardx::ard_survey_svychisq", "cardx::ard_survey_svyranktest",
                    "cardx::ard_survey_svyttest", "cards::ard_formals",
                    "cards::ard_continuous", "cardx::ard_categorical_max") %in% off))
  expect_true(f$offered[f$call == "cards::ard_summary"])
  # the arguments R has to write as they are
  expect_identical(tfl_ard_args("cardx::ard_tabulate_abnormal")$kind[
    tfl_ard_args("cardx::ard_tabulate_abnormal")$arg == "abnormal"], "code")
  e <- tfl_ard_args("cardx::ard_emmeans_emmeans")
  expect_identical(e$kind[e$arg == "primary_covariate"], "text")
})

test_that("a parent row's function says the columns the generated code reads", {
  skip_if_not_installed("cards")
  col <- function(call, arg) {
    a <- tfl_ard_args(call)
    a$column[a$arg == arg]
  }
  # ard_stack(.by =), ard_strata(.by =, .strata =) and ard_pairwise(variable =)
  # are written from the analysis row's by / strata / variables columns
  expect_identical(col("cards::ard_stack", ".by"), "by")
  expect_identical(col("cards::ard_strata", ".by"), "by")
  expect_identical(col("cards::ard_strata", ".strata"), "strata")
  expect_identical(col("cards::ard_pairwise", "variable"), "variables")
  # the rest of their arguments stay in args
  expect_identical(col("cards::ard_stack", ".total_n"), "args")
  # and the other functions are as before
  expect_identical(col("cards::ard_summary", "by"), "by")
})

test_that("every other exported function of cards / cardx has a decided use", {
  # ARD builder design ch. 7: each of cards' and cardx's functions that is
  # not an ard_*() (those are in the catalog, above) is decided once --
  # a post step, written by the generated code, a screen's tool, internal,
  # for writing one's own ARD function, a selector, a choice, not used.
  # A new cards / cardx (CI runs CRAN's every week) adds one: this test
  # names it, and inst/ard/utilities.csv gets a row.
  u <- utils::read.csv(system.file("ard", "utilities.csv", package = "tflspec"),
                       stringsAsFactors = FALSE)
  expect_true(all(u$use %in% c("post", "code", "old", "not_used", "screen",
                               "future", "internal", "own_function",
                               "selector", "choice")))
  # the versions the table was written for: an older cards (the oldest
  # supported, tested too) is not held to it
  written_for <- c(cards = "0.9.0", cardx = "0.3.4")
  for (p in c("cards", "cardx")) {
    if (!requireNamespace(p, quietly = TRUE) ||
        utils::packageVersion(p) < written_for[[p]]) next
    e <- getNamespaceExports(p)
    e <- e[!startsWith(e, "ard_") & !startsWith(e, ".")]
    new <- setdiff(e, u$`function`[u$package == p])
    expect_identical(sort(new), character(0),
                     info = paste0(p, " ", utils::packageVersion(p),
                                   ": decide the use of ", paste(new, collapse = ", "),
                                   " in inst/ard/utilities.csv"))
  }
})

test_that("two outputs with the same statistics keep their own rows", {
  # the study ARD is bound with dplyr::bind_rows(), not cards::bind_ard():
  # bind_ard() does not count output_id / analysis_id as part of a row's key,
  # so the same statistic in two outputs stops it (different values) or loses
  # one output's rows (the same values)
  skip_on_cran()
  skip_if_not_installed("cards")
  adam <- exact_data()
  dir <- exact_dir(adam)
  sp <- exact_spec(list(method = "continuous", dataset = "ADSL",
                        population_id = "SAF", by = "ARM", variables = "AGE"))
  two <- sp$analyses
  two$output_id <- "T2"
  sp$analyses <- rbind(sp$analyses, two)
  ard <- suppressMessages(tfl_build_ard(sp, dir = dir, save = FALSE))
  n <- table(ard$output_id)
  expect_identical(names(n), c("T", "T2"))
  expect_identical(n[["T"]], n[["T2"]])
  expect_identical(exact_numbers(tfl_ard_for(ard, "T")),
                   exact_numbers(tfl_ard_for(ard, "T2")))
})

test_that("the hints of by and strata say what differs, function by function (#139)", {
  h <- function(f, a) { d <- tfl_ard_args(f); d$hint[d$arg == a] }
  expect_match(h("cards::ard_summary", "by"), "do not have too", fixed = TRUE)
  expect_match(h("cards::ard_summary", "strata"), "only the combinations the data have", fixed = TRUE)
  expect_match(h("cardx::ard_stats_t_test", "by"), "the groups compared", fixed = TRUE)
  expect_match(h("cardx::ard_categorical_ci", "strata"), "stratified Wilson", fixed = TRUE)
  expect_match(h("cardx::ard_stats_mantelhaen_test", "strata"), "Cochran-Mantel-Haenszel", fixed = TRUE)
})
