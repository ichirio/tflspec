# A report whose data has no records says so (#678): the build that stops
# on it names the report, the dataset, the analysis set and the `where`, and
# keeps the error it stopped with.  Looked for on the failure path only.

ed_study <- function(d) {
  data <- tplfig_study(d, c("adsl", "adlb", "adae"), "EMPTY-1")
  # a transport file holds "" for no value (not NA): counted from the file
  expect_gt(sum(data$adlb$ABLFL == ""), 0L)
  expect_identical(sum(is.na(data$adlb$ABLFL)), 0L)
  data
}

ed_shift <- function(d, where) {
  f <- file.path(d, "spec", "outputs", "T-ED.yml")
  tfl_write_output(tfl_output("T-ED", "table", population = "SAF", extends = "defaults",
                              template = list(name = "shift",
                                              args = list(population = "SAF", where = where))), f)
  st <- tfl_read_study_spec(d)
  tryCatch(suppressMessages(tfl_build_ard(st, output_id = "T-ED", dir = d, save = FALSE)),
           error = function(e) e)
}

test_that("a template whose where selects no record: the report, dataset, set and where are named", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  skip_if_not_installed("dplyr")
  d <- withr_tempdir()
  data <- ed_study(d)
  w <- 'PARAMCD %in% c("ALT", "HGB") & ONTRTFL == "Y" & is.na(ABLFL)'
  # no record passes it (counted from the file, not from the code)
  lb <- data$adlb
  expect_identical(sum(lb$PARAMCD %in% c("ALT", "HGB") & lb$ONTRTFL == "Y" & is.na(lb$ABLFL)), 0L)
  e <- ed_shift(d, w)
  expect_s3_class(e, "tflspec_empty_data")
  msg <- conditionMessage(e)
  expect_match(msg, "report T-ED", fixed = TRUE)
  expect_match(msg, "dataset ADLB", fixed = TRUE)
  expect_match(msg, "analysis set SAF", fixed = TRUE)
  expect_match(msg, w, fixed = TRUE)
  # ... said for is.na(): a transport file's blank is "", not NA
  expect_match(msg, "holds \"\" for no value", fixed = TRUE)
  # the error it stopped with comes after
  expect_match(msg, "\nThe build stopped with: .+")
  # != "Y" selects the baseline-free records: it builds
  ok <- ed_shift(d, 'PARAMCD %in% c("ALT", "HGB") & ONTRTFL == "Y" & ABLFL != "Y"')
  expect_false(inherits(ok, "error"))
  expect_gt(NROW(ok), 0L)
})

test_that("a where with no is.na() gets no hint about blanks", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  skip_if_not_installed("dplyr")
  d <- withr_tempdir()
  ed_study(d)
  e <- ed_shift(d, 'PARAMCD == "NOSUCH"')
  expect_s3_class(e, "tflspec_empty_data")
  expect_match(conditionMessage(e), 'where PARAMCD == "NOSUCH"', fixed = TRUE)
  expect_false(grepl("transport file", conditionMessage(e), fixed = TRUE))
})

test_that("an analysis of an ard: analyses report on no records: named too", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  skip_if_not_installed("dplyr")
  d <- withr_tempdir()
  ed_study(d)
  o <- tfl_output("T-AN", "table", ard = list(analyses = list(
    list(id = "LB", method = "cards::ard_tabulate", dataset = "ADLB",
         population = "SAF", by = "TRT01A", variables = "ANRIND",
         where = 'PARAMCD == "NOSUCH"'))))
  tfl_write_output(o, file.path(d, "spec", "outputs", "T-AN.yml"))
  st <- tfl_read_study_spec(d)
  e <- tryCatch(suppressMessages(tfl_build_ard(st, output_id = "T-AN", dir = d, save = FALSE)),
                error = function(e) e)
  skip_if_not(inherits(e, "error"), "cards builds an ARD from no records here")
  expect_s3_class(e, "tflspec_empty_data")
  msg <- conditionMessage(e)
  expect_match(msg, "report T-AN", fixed = TRUE)
  expect_match(msg, "dataset ADLB", fixed = TRUE)
  expect_match(msg, "analysis set SAF", fixed = TRUE)
  expect_match(msg, 'where PARAMCD == "NOSUCH"', fixed = TRUE)
  expect_match(msg, "The build stopped with:", fixed = TRUE)
})

test_that("an error with data that has records is the error as it was; a failing check falls back", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- withr_tempdir()
  ed_study(d)
  st <- tfl_read_study_spec(d)
  # a program that stops on something else: nothing to say about records
  env <- new.env()
  e <- tryCatch(tflspec:::.eval_study_code("x <- data.frame(a = 1)\nstop('plain failure')", env, st, d),
                error = function(e) e)
  expect_false(inherits(e, "tflspec_empty_data"))
  expect_identical(conditionMessage(e), "plain failure")
  # data with no records, but the study cannot be read for the message
  env2 <- new.env()
  e2 <- tryCatch(tflspec:::.eval_study_code("adsl <- data.frame(a = 1)[0, , drop = FALSE]\nstop('other failure')",
                                            env2, list(not = "a study"), d),
                 error = function(e) e)
  expect_false(inherits(e2, "tflspec_empty_data"))
  expect_identical(conditionMessage(e2), "other failure")
  # a build that works never looks: no message function called
  called <- FALSE
  testthat::local_mocked_bindings(.empty_data_message = function(...) { called <<- TRUE; NULL })
  ok <- ed_shift(d, 'PARAMCD == "ALT" & ONTRTFL == "Y"')
  expect_false(inherits(ok, "error"))
  expect_false(called)
})
