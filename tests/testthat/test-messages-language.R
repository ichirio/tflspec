# The messages that name a missing file or a missing `by` variable, whatever
# language R gives its own error in

test_that("a missing data file the code reads is named, whatever R's error says", {
  d <- tempfile("msglang")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  st <- structure(list(data = list(datasets = list(
    list(dataset = "ADSL", path = "adam/adsl.rds"),
    list(dataset = "ADAE", path = "adam/adae.rds")))), class = "tfl_study_spec")
  # an error that does not read as a missing file (R in another language)
  code <- "adsl <- readRDS(\"adam/adsl.rds\")\nstop(\"\\u30b3\\u30cd\\u30af\\u30b7\\u30e7\\u30f3\")"
  expect_error(tflspec:::.eval_study_code(code, new.env(), st, d),
               "ADSL: no file at adam/adsl.rds (spec/data.yml)", fixed = TRUE)
  # a missing file the code does not read: R's own error, as before
  expect_error(tflspec:::.eval_study_code("stop(\"something else\")", new.env(), st, d),
               "something else", fixed = TRUE)
  # an English missing-file error with no file named: the first missing one
  expect_error(tflspec:::.eval_study_code("stop(\"cannot open the connection\")", new.env(), st, d),
               "ADSL: no file at", fixed = TRUE)
})

test_that("a dataset with no path in data.yml is not a missing file", {
  expect_null(tflspec:::.data_missing_message(list(list(dataset = "ADSL")), dir = tempdir()))
  expect_null(tflspec:::.data_missing_message(list(list(dataset = "ADSL", path = "")), dir = tempdir()))
})

test_that("a missing `by` variable is named, in any language and either quotes", {
  ro <- structure(list(data = list(dataset = "ADAE")),
                  template = list(args = list(by = c("AESEV", "NOSUCH"))))
  want <- "`by` names 'NOSUCH', which the dataset ADAE does not have"
  for (m in c("object 'NOSUCH' not found",
              "ℹ In argument: `NOSUCH == 1`.\nCaused by error:\n! オブジェクト 'NOSUCH' がありません",
              "object ‘NOSUCH’ not found")) {
    expect_error(tflspec:::.listing_by_hint(simpleError(m), ro), want, fixed = TRUE)
  }
  # another name in quotes: the error as it was
  expect_error(tflspec:::.listing_by_hint(simpleError("object 'OTHER' not found"), ro),
               "object 'OTHER' not found", fixed = TRUE)
})
