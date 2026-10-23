# The data the ARD program makes itself: <dataset>_sub, numbered from a
# report's second of a dataset, each under a comment; tfl_ard_data_names()

sb_df <- function(...) as.data.frame(list(...), stringsAsFactors = FALSE)

sb_spec <- function(code = NA, check = TRUE) {
  s <- list(
    study = sb_df(key = "id", value = "USUBJID"),
    datasets = sb_df(dataset = c("ADSL", "ADAE"), path = c("adsl.rds", "adae.rds")),
    populations = sb_df(population_id = c("SAF", "ITT"), dataset = "ADSL",
                        where = c("SAFFL == \"Y\"", "ITTFL == \"Y\""),
                        label = c("Safety Analysis Set", "ITT Set")),
    analyses = sb_df(
      output_id = c("T1", "T1", "T1", "T2"),
      analysis_id = c("N", "AGE", "AE", "N2"),
      method = c("cards::ard_tabulate", "cards::ard_summary", "cards::ard_stack_hierarchical",
                 "custom"),
      dataset = c("ADSL", "ADSL", "ADAE", "ADSL"),
      population_id = c("SAF", "ITT", "SAF", "ITT"),
      where = c(NA, NA, "TRTEMFL == \"Y\"", NA),
      by = c(NA, "TRT01A", "TRT01A", NA),
      variables = c("TRT01A", "AGE", "AEBODSYS | AEDECOD", NA),
      denominator = c(NA, NA, "population", NA),
      code = c(NA, NA, NA, code)))
  if (check) tfl_ard_spec(s) else s
}

test_that("one report: <dataset>_sub, the second of a dataset _sub2, each under its comment", {
  code <- tfl_ard_code(sb_spec("cards::ard_tabulate(data, variables = TRT01A)"),
                       output_id = "T1", save = FALSE, part = "body")
  expect_true(all(c(
    "# adsl_sub: SAF — Safety Analysis Set (SAFFL == \"Y\")",
    "adsl_sub <- filter(adsl, SAFFL == \"Y\")",
    "# adsl_sub2: ITT — ITT Set (ITTFL == \"Y\")",
    "adsl_sub2 <- filter(adsl, ITTFL == \"Y\")",
    "# adae_sub: ADAE (TRTEMFL == \"Y\"); of the adsl_sub subjects") %in% code))
  txt <- paste(code, collapse = "\n")
  expect_match(txt, "ard_age <- adsl_sub2 |>", fixed = TRUE)
  expect_match(txt, "denominator = adsl_sub,", fixed = TRUE)
  expect_false(grepl("pop_", txt, fixed = TRUE))
})

test_that("the study's program: each report's part makes its own, a name meaning one thing there", {
  code <- tfl_ard_code(sb_spec("cards::ard_tabulate(data, variables = TRT01A)"),
                       save = FALSE, part = "body")
  t2 <- code[seq(match("# ---- T2 ----", code), length(code))]
  expect_true("adsl_sub <- filter(adsl, ITTFL == \"Y\")" %in% t2)
  expect_false("# ---- populations ----" %in% code)
  expect_silent(parse(text = code))
})

test_that("tfl_ard_data_names(): each analysis's data by the program's name", {
  x <- sb_spec("cards::ard_tabulate(data, variables = TRT01A)")
  dn <- tfl_ard_data_names(x)
  expect_identical(dn$data, c("adsl_sub", "adsl_sub2", "adae_sub", "adsl_sub"))
  expect_identical(dn$analysis_id, c("N", "AGE", "AE", "N2"))
  expect_identical(tfl_ard_data_names(x, "T2")$data, "adsl_sub")
})

test_that("an old pop_<id> in the definition's R is a problem saying the new name", {
  p <- tflspec:::.ard_spec_problems(sb_spec("cards::ard_tabulate(pop_itt, variables = TRT01A)",
                                            check = FALSE))
  expect_true(any(grepl("pop_itt, a name the ARD program no longer has", p$message, fixed = TRUE)))
  expect_true(any(grepl("data is adsl_sub now", p$message, fixed = TRUE)))
  # (a name that only has pop_ in it is not one)
  p <- tflspec:::.ard_spec_problems(sb_spec("cards::ard_tabulate(my_pop_itt, variables = TRT01A)",
                                            check = FALSE))
  expect_false(any(grepl("no longer has", p$message, fixed = TRUE)))
})

test_that("the ARD from the renamed program", {
  skip_if_not_installed("cards")
  dir <- withr_tempdir()
  adsl <- cards::ADSL
  adsl$TRT01A <- adsl$ARM
  adsl$ITTFL <- "Y"
  saveRDS(adsl, file.path(dir, "adsl.rds"))
  saveRDS(cards::ADAE, file.path(dir, "adae.rds"))
  a <- tfl_build_ard(sb_spec("cards::ard_tabulate(data, variables = TRT01A)"), dir = dir,
                     save = FALSE)
  expect_setequal(unique(a$analysis_id), c("N", "AGE", "AE", "N2"))
})
