# An analysis's keys reach the ARD program through the sheets (the YAML
# file -> .sheets_of_study() -> tfl_ard_code()): a key outside the analyses
# sheet's columns is the function's argument, in args (#924); a map on a
# column its pieces (#925); `subjects_in: none` keeps all (#927).  The
# smallest case of each issue.  Fictional data only (the package's sample
# ADaM data).

# The ARD program of T-14-1-1 with the analysis `a` (and the data row
# `data`) added to it, or the error's message
yk_code <- function(d, a = NULL, data = NULL) {
  f <- file.path(d, "spec", "outputs", "T-14-1-1.yml")
  o0 <- tfl_read_output(f)
  on.exit(tfl_write_output(o0, f), add = TRUE)
  o <- o0
  if (!is.null(a)) o$ard$analyses[[length(o$ard$analyses) + 1L]] <- a
  if (!is.null(data)) o$ard$analysis_data[[length(o$ard$analysis_data) + 1L]] <- data
  tfl_write_output(o, f)
  st <- tfl_read_study_spec(d)
  tryCatch(paste(suppressWarnings(suppressMessages(
    tfl_ard_code(st, output_id = "T-14-1-1", save = FALSE))), collapse = "\n"),
    error = function(e) paste("ERROR:", conditionMessage(e)))
}

yk_study <- function() {
  root <- tempfile("tflspec-yk")
  dir.create(root)
  as_study(root)
}

yk_base <- list(id = "WALK", method = "cards::ard_summary", data = "adsl_sub", by = "TRT01A",
                variables = "AGE")

test_that("an analysis's function arguments outside the sheet's columns reach the ARD program (#924)", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- yk_study()
  on.exit(unlink(dirname(d), recursive = TRUE), add = TRUE)
  without <- yk_code(d, yk_base)
  with <- yk_code(d, c(yk_base, list(stat_label = list(AGE = list(mean = "Average")))))
  expect_false(startsWith(with, "ERROR:"))
  expect_false(identical(with, without))
  expect_match(with, "stat_label = list(AGE = list(mean = \"Average\"))", fixed = TRUE)
  # an argument named as the analysis's own key (ard_categorical_ci's
  # method) goes in args
  ci <- list(id = "WALK", method = "cardx::ard_categorical_ci", data = "adsl_sub", by = "TRT01A",
             variables = "SEX")
  expect_match(yk_code(d, c(ci, list(args = tfl_r("method = \"wilson\"")))),
               "method = \"wilson\"", fixed = TRUE)
  # a key args gives too stops, naming the key and the analysis
  twice <- yk_code(d, c(yk_base, list(stat_label = list(AGE = list(mean = "A")),
                                      args = tfl_r("stat_label = list(AGE = list(mean = \"B\"))"))))
  expect_match(twice, "^ERROR: analysis WALK \\(report T-14-1-1\\): `stat_label` is written as a key and in args")
})

test_that("an analysis's formats, derive and analysis_ids maps reach the ARD program (#925)", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- yk_study()
  on.exit(unlink(dirname(d), recursive = TRUE), add = TRUE)
  without <- yk_code(d, yk_base)
  with <- yk_code(d, c(yk_base, list(formats = list(mean = 2L, sd = 3L))))
  expect_false(startsWith(with, "ERROR:"))
  expect_false(identical(with, without))
  # (the same as the sequence form the sheet's column says)
  expect_identical(with, yk_code(d, c(yk_base, list(formats = list("mean=2", "sd=3")))))
  der <- yk_code(d, c(yk_base, list(derive = list(AGEX = tfl_r("AGE * 2")))))
  expect_match(der, "AGEX = AGE * 2", fixed = TRUE)
  ids <- yk_code(d, c(yk_base, list(analysis_ids = list(AGE = "AGE_ID"))))
  expect_match(ids, "\"AGE_ID\"", fixed = TRUE)
  expect_false(grepl("\"WALK\"", ids, fixed = TRUE))
  # what cannot be carried stops, naming the key and the analysis
  expect_match(yk_code(d, c(yk_base, list(formats = list(mean = list(1L, 2L))))),
               "^ERROR: analysis WALK \\(report T-14-1-1\\): `formats` is a map of single values")
  expect_match(yk_code(d, c(yk_base, list(analysis_ids = list(SEX = "S")))),
               "^ERROR: analysis WALK \\(report T-14-1-1\\): `analysis_ids` .* SEX is not one")
})

test_that("a data row's subjects_in: none keeps all, as a blank does (#927)", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- yk_study()
  on.exit(unlink(dirname(d), recursive = TRUE), add = TRUE)
  none <- yk_code(d, data = list(id = "adae_x", from = "ADAE", subjects_in = "none"))
  expect_false(startsWith(none, "ERROR:"))
  expect_identical(none, yk_code(d, data = list(id = "adae_x", from = "ADAE")))
})
