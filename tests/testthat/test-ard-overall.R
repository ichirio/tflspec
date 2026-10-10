# `analyses$overall`: the analysis again without its by, as cards' own
# overall rows (no group) -- what a table reads as its Total column (#212).

overall_spec <- function(rows) {
  S <- .ard_spec_sheets
  tfl_ard_spec(list(
    study = exact_sheet(list(list(key = "id", value = "USUBJID")), S$study),
    datasets = exact_sheet(list(list(dataset = "ADSL", path = "adam/ADSL.rds")),
                           S$datasets),
    populations = exact_sheet(list(list(population_id = "SAF", dataset = "ADSL",
                                        where = "SAFFL == \"Y\"")), S$populations),
    analyses = exact_sheet(lapply(rows, function(r) {
      r$output_id <- r$output_id %||% "T"
      r
    }), S$analyses)))
}

test_that("overall binds the call without its by under it: cards' overall rows", {
  skip_on_cran()
  skip_if_not_installed("cards")
  adam <- exact_data()
  dir <- exact_dir(adam)
  sp <- overall_spec(list(
    list(analysis_id = "SEX", method = "categorical", dataset = "ADSL",
         population_id = "SAF", by = "ARM", variables = "SEX",
         denominator = "population", overall = "TRUE")))
  txt <- paste(tfl_ard_code(sp, part = "body"), collapse = "\n")
  expect_match(txt, "ard_sex <- bind_rows(\n  ard_tabulate(\n    pop_saf,\n    by = ARM,",
               fixed = TRUE)
  expect_match(txt, "),\n  ard_tabulate(\n    pop_saf,\n    variables = SEX,", fixed = TRUE)

  ard <- suppressMessages(tfl_build_ard(sp, dir = dir, save = FALSE))
  pop <- subset(adam$ADSL, SAFFL == "Y")
  hand <- dplyr::bind_rows(
    cards::ard_tabulate(pop, by = ARM, variables = SEX, denominator = pop),
    cards::ard_tabulate(pop, variables = SEX, denominator = pop))
  expect_identical(exact_numbers(ard), exact_numbers(hand))
  # the overall rows: no group, all the subjects
  all <- ard[is.na(ard$group1) & ard$stat_name == "n", ]
  expect_equal(sum(unlist(all$stat)), nrow(pop))
  expect_true(all(ard$analysis_id == "SEX"))
})

test_that("a blank or FALSE overall writes the program it wrote before", {
  skip_if_not_installed("cards")
  row <- list(analysis_id = "SEX", method = "categorical", dataset = "ADSL",
              population_id = "SAF", by = "ARM", variables = "SEX")
  before <- tfl_ard_code(overall_spec(list(row)), part = "body")
  # (a FALSE written is a change of the definition: its fingerprint only)
  no_fp <- function(x) sub("definition = \"[0-9a-f]+\"", "", x)
  expect_identical(no_fp(tfl_ard_code(overall_spec(list(c(row, overall = "FALSE"))),
                                      part = "body")), no_fp(before))
  expect_false(any(grepl("bind_rows(", before, fixed = TRUE)))
  # and its fingerprint: a column blank in every row does not count
  expect_identical(
    tfl_ard_spec_hash(overall_spec(list(row)), "T"),
    tfl_ard_spec_hash(overall_spec(list(c(row, overall = NA))), "T"))
})

test_that("a stack's overall is ard_stack(.overall = TRUE)", {
  skip_on_cran()
  skip_if_not_installed("cards")
  adam <- exact_data()
  dir <- exact_dir(adam)
  sp <- overall_spec(list(
    list(analysis_id = "DEMO", method = "cards::ard_stack", dataset = "ADSL",
         population_id = "SAF", by = "ARM", overall = "yes"),
    list(analysis_id = "CAT", parent = "DEMO", method = "categorical",
         variables = "SEX")))
  txt <- paste(tfl_ard_code(sp, part = "body"), collapse = "\n")
  expect_match(txt, "    .overall = TRUE\n  ) |>", fixed = TRUE)
  ard <- suppressMessages(tfl_build_ard(sp, dir = dir, save = FALSE))
  expect_true(any(is.na(ard$group1) & ard$variable == "SEX"))
})

test_that("overall is checked: a by to leave out, its own row, a yes or no", {
  skip_if_not_installed("cards")
  msg <- function(rows) {
    e <- tryCatch(overall_spec(rows), error = function(e) conditionMessage(e))
    if (is.character(e)) e else ""
  }
  expect_match(msg(list(list(analysis_id = "A", method = "categorical",
                             dataset = "ADSL", variables = "SEX", overall = "TRUE"))),
               "without its `by`, and it has none", fixed = TRUE)
  expect_match(msg(list(list(analysis_id = "A", method = "categorical",
                             dataset = "ADSL", by = "ARM", variables = "SEX",
                             overall = "maybe"))),
               "`overall` is TRUE or FALSE", fixed = TRUE)
  expect_match(msg(list(
    list(analysis_id = "DEMO", method = "cards::ard_stack", dataset = "ADSL",
         by = "ARM"),
    list(analysis_id = "CAT", parent = "DEMO", method = "categorical",
         variables = "SEX", overall = "TRUE"))),
    "the overall is the parent's", fixed = TRUE)
  expect_match(msg(list(list(analysis_id = "A", method = "custom", by = "ARM",
                             code = "cards::ard_tabulate(data, variables = SEX)",
                             overall = "TRUE"))),
               "takes no `overall`", fixed = TRUE)
  expect_match(msg(list(list(analysis_id = "A", method = "cards::ard_stack",
                             dataset = "ADSL", by = "ARM", overall = "TRUE",
                             args = ".overall = TRUE"))),
               "`.overall` is given twice", fixed = TRUE)
})
