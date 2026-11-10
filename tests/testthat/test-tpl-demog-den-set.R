# The demog family (demog, stf_t02_demog): the categorical percentages are over
# the analysis set -- the report's analysis data after its population and
# filters (adsl_saf) -- not over the subjects with a value; a variable with
# missing values (NA or blank) gets a Missing row, last.  Fictional data only.

dd_pops <- list(list(id = "SAF", label = "Safety Analysis Set"))
dd_codelists <- list(TRT01A = c("Drug X", "Control"))
dd_expand <- function(name = "demog", ...) {
  tfl_template_expand(name, c(list(population = "SAF"), list(...)),
                      codelists = dd_codelists, population = dd_pops)
}
dd_an <- function(x, id) Filter(function(a) identical(a$id, id), x$ard$analyses)[[1L]]
dd_set <- function(x, id) Filter(function(d) identical(d$id, id), x$ard$analysis_data)[[1L]]

test_that("demog family: the % denominator is the analysis data GROUPN counts", {
  for (tpl in c("demog", "stf_t02_demog")) {
    x <- dd_expand(tpl)
    cat <- dd_an(x, "CAT")
    expect_identical(cat$denominator, "adsl_saf", info = tpl)
    expect_identical(dd_an(x, "GROUPN")$data, cat$denominator, info = tpl)
    expect_identical(dd_an(x, "CONT")$data, cat$denominator, info = tpl)
    # the categorical analysis reads the same set with a Missing level made on it
    expect_identical(cat$data, "adsl_saf_m", info = tpl)
    m <- dd_set(x, "adsl_saf_m")
    expect_identical(m$from, "adsl_saf", info = tpl)
    expect_match(as.character(m$code), "Missing", fixed = TRUE, info = tpl)
  }
})

test_that("demog family: the report's filter narrows the set the denominator names", {
  x <- dd_expand("demog", subset = "SEX == \"F\"")
  expect_identical(dd_an(x, "CAT")$denominator, "adsl_saf")
  expect_match(as.character(dd_set(x, "adsl_saf")$where), "SEX", fixed = TRUE)
  expect_identical(dd_an(x, "GROUPN")$data, "adsl_saf")
})

test_that("demog family: denominator is no parameter; summary and disposition keep it", {
  expect_false("denominator" %in% tfl_tpl_args("demog")$param)
  expect_false("denominator" %in% tfl_tpl_args("stf_t02_demog")$param)
  expect_true("denominator" %in% tfl_tpl_args("summary")$param)
  expect_true("denominator" %in% tfl_tpl_args("disposition")$param)
  s <- dd_expand("summary", categorical = "SEX")
  expect_false("denominator" %in% names(dd_an(s, "CAT")))
  expect_identical(dd_an(s, "CAT")$data, "adsl_saf")
})

dd_study <- function(blank_race = TRUE, args = list(), template = "demog") {
  skip_if_not_installed("haven")
  d <- tempfile("demogden")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  a <- tflspec:::.ai_chr(tflspec::adsl)
  a <- a[a$SAFFL == "Y", ]
  if (blank_race) {
    i <- which(a$TRT01A == "Control")[1:3]
    a$RACE[i] <- ""
  }
  haven::write_xpt(a, file.path(d, "adam", "adsl.xpt"))
  tfl_ai_study_init(d, adam = "adam", study_id = "DD-1")
  o <- tfl_output("T-DD", "table", population = "SAF", label = "demog",
    template = list(name = template, args = c(list(
      by = "TRT01A", continuous = "AGE", categorical = c("SEX", "RACE")), args)))
  tfl_write_output(o, file.path(d, "spec", "outputs", "T-DD.yml"))
  d
}
dd_text <- function(d) {
  rtf <- tfl_run_output("T-DD", dir = d)
  plain <- gsub("\\\\[a-z]+-?[0-9]* ?|[{}]", "", readLines(rtf, warn = FALSE))
  trimws(plain)
}

test_that("demog: % over the analysis set, a Missing row for the variable that has missing values", {
  skip_on_cran()
  d <- dd_study()
  txt <- dd_text(d)
  # Control (78): 22 / 78, not 22 / 75
  expect_true(any(grepl("ASIAN.*22 \\(28\\.2\\)", txt)))
  expect_false(any(grepl("29\\.3", txt)))
  expect_true(any(grepl("Missing.*3 \\(3\\.8\\)", txt)))
  # SEX has no missing value: one Missing row, RACE's
  expect_identical(sum(grepl("Missing", txt)), 1L)
  # (the Age n row: Drug X 80, Control 78 -- the set GROUPN counts)
  expect_true(any(grepl("n8078$", txt)))
  code <- paste(suppressMessages(tfl_ard_code(tfl_read_study_spec(d), output_id = "T-DD")),
                collapse = "\n")
  expect_match(code, "denominator = adsl_saf", fixed = TRUE)
  expect_match(code, "ard_groupn <- adsl_saf |>", fixed = TRUE)
})

test_that("demog: the report's filter makes the denominator that subset", {
  skip_on_cran()
  d <- dd_study(args = list(subset = "SEX == \"F\""))
  txt <- dd_text(d)
  a <- tflspec:::.ai_chr(tflspec::adsl)
  f <- a[a$SAFFL == "Y" & a$SEX == "F", ]
  n_arms <- paste0(sum(f$TRT01A == "Drug X"), sum(f$TRT01A == "Control"))
  expect_true(any(grepl(paste0("n", n_arms, "$"), txt)))
  # the percentages' base is that N, whatever is blank
  expect_true(any(grepl("Missing", txt)))
  expect_false(any(grepl("100\\.0", txt[grepl("Missing", txt)])))
})

test_that("demog: no missing values, no Missing row, the percentages of the set", {
  skip_on_cran()
  d <- dd_study(blank_race = FALSE)
  txt <- dd_text(d)
  expect_false(any(grepl("Missing", txt)))
  expect_true(any(grepl("ASIAN.*\\(", txt)))
})

test_that("summary with its default denominator keeps the non-missing percentages", {
  skip_on_cran()
  d <- dd_study(template = "summary")
  txt <- dd_text(d)
  # Control: 22 / 75 (the subjects with a value), no Missing row
  expect_true(any(grepl("ASIAN.*22 \\(29\\.3\\)", txt)))
  expect_false(any(grepl("Missing", txt)))
  x <- dd_expand("summary", categorical = "RACE")
  expect_false("denominator" %in% names(dd_an(x, "CAT")))
  expect_identical(dd_an(x, "CAT")$data, "adsl_saf")
})

test_that("missing_row: TRUE (default) is the Missing step; FALSE and !unset turn it off", {
  on <- dd_expand("demog")
  expect_identical(dd_expand("demog", missing_row = TRUE), on)
  expect_true("adsl_saf_m" %in% vapply(on$ard$analysis_data, `[[`, "", "id"))
  for (v in list(FALSE, tfl_unset())) {
    off <- dd_expand("demog", missing_row = v)
    expect_false("adsl_saf_m" %in% vapply(off$ard$analysis_data, `[[`, "", "id"))
    cat <- dd_an(off, "CAT")
    expect_identical(cat$data, "adsl_saf")
    # (the percentages stay over the analysis set)
    expect_identical(cat$denominator, "adsl_saf")
    expect_identical(dd_an(off, "GROUPN")$data, "adsl_saf")
  }
  a <- tfl_tpl_args("demog")
  expect_identical(a$effective[a$param == "missing_row"], "TRUE")
  expect_match(a$doc[a$param == "missing_row"], "^TRUE \\(default\\): a Missing row, last")
})

test_that("missing_row is no parameter of stf_t02_demog or summary, and passing it stops", {
  expect_false("missing_row" %in% tfl_tpl_args("stf_t02_demog")$param)
  expect_false("missing_row" %in% tfl_tpl_args("summary")$param)
  expect_error(dd_expand("stf_t02_demog", missing_row = FALSE), "there is no `missing_row` parameter")
  expect_error(dd_expand("summary", categorical = "SEX", missing_row = TRUE),
               "there is no `missing_row` parameter")
  # (stf_t02_demog keeps the Missing step)
  expect_true("adsl_saf_m" %in% vapply(dd_expand("stf_t02_demog")$ard$analysis_data, `[[`, "", "id"))
})

test_that("missing_row = FALSE: no Missing row, other percentages as with TRUE; code and export show it", {
  skip_on_cran()
  d <- dd_study(args = list(missing_row = FALSE))
  txt <- dd_text(d)
  expect_false(any(grepl("Missing", txt)))
  expect_true(any(grepl("ASIAN.*22 \\(28\\.2\\)", txt)))
  st <- tfl_read_study_spec(d)
  code <- paste(suppressMessages(tfl_ard_code(st, output_id = "T-DD")), collapse = "
")
  expect_false(grepl("adsl_saf_m", code, fixed = TRUE))
  expect_match(code, "denominator = adsl_saf", fixed = TRUE)
  cs <- tfl_template_to_spec(tfl_read_output(file.path(d, "spec", "outputs", "T-DD.yml")), st)
  expect_false("adsl_saf_m" %in% vapply(cs$ard$analysis_data, `[[`, "", "id"))
  d2 <- dd_study()
  st2 <- tfl_read_study_spec(d2)
  cs2 <- tfl_template_to_spec(tfl_read_output(file.path(d2, "spec", "outputs", "T-DD.yml")), st2)
  expect_true("adsl_saf_m" %in% vapply(cs2$ard$analysis_data, `[[`, "", "id"))
})
