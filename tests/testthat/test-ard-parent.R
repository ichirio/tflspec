# Analyses run inside another (`parent`): cards::ard_stack(), ard_strata()
# and ard_pairwise() as the call a person writes, each variable's rows
# tagged with its own analysis.

parent_spec <- function(rows) {
  S <- .ard_spec_sheets
  tfl_ard_spec(list(
    study = exact_sheet(list(list(key = "id", value = "USUBJID")), S$study),
    datasets = exact_sheet(list(list(dataset = "ADSL", path = "adam/ADSL.rds"),
                                list(dataset = "ADAE", path = "adam/ADAE.rds")),
                           S$datasets),
    populations = exact_sheet(list(list(population_id = "SAF", dataset = "ADSL",
                                        where = "SAFFL == \"Y\"")), S$populations),
    analyses = exact_sheet(lapply(rows, function(r) {
      r$output_id <- r$output_id %||% "T"
      r
    }), S$analyses)))
}

test_that("a stack is one ard_stack() call, each variable tagged with its analysis", {
  skip_on_cran()
  skip_if_not_installed("cards")
  adam <- exact_data()
  dir <- exact_dir(adam)
  sp <- parent_spec(list(
    list(analysis_id = "DEMO", method = "cards::ard_stack", dataset = "ADSL",
         population_id = "SAF", by = "ARM", args = ".total_n = TRUE"),
    list(analysis_id = "CONT", parent = "DEMO", method = "continuous",
         variables = "AGE | BMIBL", statistics = "N | mean | sd",
         formats = "mean=xx.xx"),
    list(analysis_id = "CAT", parent = "DEMO", method = "categorical",
         variables = "SEX")))
  code <- tfl_ard_code(sp, part = "body")
  txt <- paste(code, collapse = "\n")
  expect_match(txt, "ard <- cards::ard_stack(pop_saf,", fixed = TRUE)
  expect_match(txt, ".by = ARM", fixed = TRUE)
  expect_match(txt, "cards::ard_summary(variables = c(AGE, BMIBL)", fixed = TRUE)
  expect_match(txt, "cards::ard_tabulate(variables = SEX", fixed = TRUE)
  expect_match(txt, "c(`AGE` = \"CONT\", `BMIBL` = \"CONT\", `SEX` = \"CAT\", `.other` = \"DEMO\")",
               fixed = TRUE)
  # one call, not three
  expect_length(grep("^ard <- cards::", code), 1L)

  ard <- suppressMessages(tfl_build_ard(sp, dir = dir, save = FALSE))
  ids <- tapply(ard$analysis_id, ard$variable, unique)
  expect_identical(ids[["AGE"]], "CONT")
  expect_identical(ids[["SEX"]], "CAT")
  expect_identical(ids[["ARM"]], "DEMO")              # the by counts
  expect_identical(ids[["..ard_total_n.."]], "DEMO")
  # the numbers are the stack written by hand
  pop <- subset(adam$ADSL, SAFFL == "Y")
  hand <- cards::ard_stack(
    pop, .by = ARM,
    cards::ard_continuous(variables = c(AGE, BMIBL),
                          statistic = ~ cards::continuous_summary_fns(c("N", "mean", "sd"))),
    cards::ard_categorical(variables = SEX), .total_n = TRUE)
  expect_identical(exact_numbers(ard), exact_numbers(hand))
  # the row's formats are for its own variables
  fm <- ard$stat_fmt[ard$variable == "AGE" & ard$stat_name == "mean"]
  expect_true(all(grepl("^[0-9]+[.][0-9]{2}$", unlist(fm))))
})

test_that("ard_strata() and ard_pairwise() run their one analysis with .x", {
  skip_on_cran()
  skip_if_not_installed("cardx")
  adam <- exact_data()
  dir <- exact_dir(adam)
  sp <- parent_spec(list(
    list(analysis_id = "BYSEX", method = "cards::ard_strata", dataset = "ADSL",
         population_id = "SAF", strata = "SEX"),
    list(analysis_id = "AGE", parent = "BYSEX", method = "continuous",
         by = "ARM", variables = "AGE"),
    list(analysis_id = "PAIRS", method = "cards::ard_pairwise", dataset = "ADSL",
         population_id = "SAF", variables = "ARM"),
    list(analysis_id = "TT", parent = "PAIRS", method = "ttest", by = "ARM",
         variables = "AGE", statistics = "estimate | p.value")))
  txt <- paste(tfl_ard_code(sp, part = "body"), collapse = "\n")
  expect_match(txt, "cards::ard_strata(pop_saf,\n    .strata = SEX,\n    .f = ~ cards::ard_summary(.x,",
               fixed = TRUE)
  expect_match(txt, "cards::ard_pairwise(pop_saf,\n    variable = ARM,\n    .f = ~ cardx::ard_stats_t_test(.x,",
               fixed = TRUE)
  ard <- suppressMessages(tfl_build_ard(sp, dir = dir, save = FALSE))
  expect_setequal(unique(ard$analysis_id), c("AGE", "TT"))
  expect_setequal(unique(ard$stat_name[ard$analysis_id == "TT"]),
                  c("estimate", "p.value"))
  pop <- subset(adam$ADSL, SAFFL == "Y")
  hand <- cards::ard_strata(pop, .strata = SEX,
                            .f = ~ cards::ard_continuous(.x, by = ARM, variables = AGE))
  # (the pairwise ARDs add a `pairwise` column, empty on the other rows)
  mine <- ard[ard$analysis_id == "AGE", setdiff(names(ard), "pairwise")]
  expect_identical(exact_numbers(mine), exact_numbers(hand))
})

test_that("what a row inside another may not say is refused", {
  base <- list(analysis_id = "DEMO", method = "cards::ard_stack",
               dataset = "ADSL", population_id = "SAF", by = "ARM")
  bad <- function(...) expect_error(parent_spec(list(...)), regexp = NULL)
  expect_error(parent_spec(list(base, list(analysis_id = "X", parent = "NOPE",
                                           method = "continuous", variables = "AGE"))),
               "not an analysis of T")
  expect_error(parent_spec(list(
    list(analysis_id = "P", method = "continuous", variables = "AGE",
         dataset = "ADSL"),
    list(analysis_id = "X", parent = "P", method = "continuous", variables = "BMIBL"))),
    "runs no other analyses")
  expect_error(parent_spec(list(base, list(analysis_id = "X", parent = "DEMO",
                                           method = "continuous", variables = "AGE",
                                           by = "SEX", dataset = "ADSL"))),
               "`dataset`, `by` are the parent's")
  expect_error(parent_spec(list(base,
    list(analysis_id = "A", parent = "DEMO", method = "continuous", variables = "AGE"),
    list(analysis_id = "B", parent = "DEMO", method = "missing", variables = "AGE"))),
    "AGE is analysed by two")
  expect_error(parent_spec(list(base, list(analysis_id = "X", parent = "DEMO",
                                           method = "custom", code = "NULL"))),
               "cannot run inside")
  expect_error(parent_spec(list(
    list(analysis_id = "S", method = "cards::ard_strata", dataset = "ADSL", strata = "SEX"),
    list(analysis_id = "A", parent = "S", method = "continuous", variables = "AGE"),
    list(analysis_id = "B", parent = "S", method = "continuous", variables = "BMIBL"))),
    "runs one analysis")
  # without rows inside, a wrapper written in args works as before
  expect_s3_class(parent_spec(list(list(
    analysis_id = "S", method = "cards::ard_stack", dataset = "ADSL",
    args = "cards::ard_summary(variables = AGE), .by = ARM"))), "tfl_ard_spec")
})

test_that("ARS sees the analyses inside, on the parent's data and groups", {
  sp <- parent_spec(list(
    list(analysis_id = "DEMO", method = "cards::ard_stack", dataset = "ADSL",
         population_id = "SAF", by = "ARM", purpose = "PRIMARY OUTCOME MEASURE"),
    list(analysis_id = "CONT", parent = "DEMO", method = "continuous",
         variables = "AGE", purpose = "PRIMARY OUTCOME MEASURE")))
  f <- .ard_spec_flat(sp$analyses)
  expect_identical(f$analysis_id, "CONT")
  expect_identical(f$by, "ARM")
  expect_identical(f$population_id, "SAF")
  expect_identical(f$dataset, "ADSL")
})

test_that("tfl_ars() writes the analyses inside a stack", {
  sp <- parent_spec(list(
    list(analysis_id = "DEMO", method = "cards::ard_stack", dataset = "ADSL",
         population_id = "SAF", by = "ARM", purpose = "PRIMARY OUTCOME MEASURE"),
    list(analysis_id = "CONT", parent = "DEMO", method = "continuous",
         variables = "AGE | BMIBL", purpose = "PRIMARY OUTCOME MEASURE")))
  ars <- suppressWarnings(tfl_ars(sp))
  ids <- vapply(ars$analyses, `[[`, "", "id")
  expect_true(all(c("An_T_CONT_AGE", "An_T_CONT_BMIBL") %in% ids))
  expect_false(any(grepl("DEMO", ids)))
})

test_that("post: steps on the ARD after the call", {
  skip_on_cran()
  skip_if_not_installed("cards")
  adam <- exact_data()
  dir <- exact_dir(adam)
  sp <- parent_spec(list(list(
    analysis_id = "CONT", method = "continuous", dataset = "ADSL",
    population_id = "SAF", by = "ARM", variables = "AGE",
    statistics = "N | mean | sd",
    post = "cards::add_calculated_row(expr = sd / sqrt(N), stat_name = \"se\")")))
  txt <- paste(tfl_ard_code(sp, part = "body"), collapse = "\n")
  expect_match(txt, "ard <- ard |>\n  cards::add_calculated_row(expr = sd / sqrt(N), stat_name = \"se\")",
               fixed = TRUE)
  ard <- suppressMessages(tfl_build_ard(sp, dir = dir, save = FALSE))
  se <- unlist(ard$stat[ard$stat_name == "se"])
  sd <- unlist(ard$stat[ard$stat_name == "sd"])
  n <- unlist(ard$stat[ard$stat_name == "N"])
  expect_equal(se, sd / sqrt(n))
  # a | inside a call is the call's; two steps
  expect_identical(.split_post("f(a | b) | g(x = \"|\")"), c("f(a | b)", "g(x = \"|\")"))
  expect_error(parent_spec(list(list(analysis_id = "A", method = "continuous",
                                     dataset = "ADSL", variables = "AGE",
                                     post = "sd / 2"))), "is not a call")
  expect_error(parent_spec(list(
    list(analysis_id = "S", method = "cards::ard_stack", dataset = "ADSL", by = "ARM"),
    list(analysis_id = "A", parent = "S", method = "continuous", variables = "AGE",
         post = "cards::sort_ard_hierarchical()"))), "goes on the parent's row")
})
