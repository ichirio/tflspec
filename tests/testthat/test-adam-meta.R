# The study's code lists by name (study$codelists) and its ADaM metadata
# (study$adam_meta, a define's shape) from spec/defaults.yml.  Fictional
# values.

amt_study <- function(defaults, env = parent.frame()) {
  d <- withr_tempdir(env)
  dir.create(file.path(d, "spec", "outputs"), recursive = TRUE)
  writeLines(defaults, file.path(d, "spec", "defaults.yml"))
  d
}

amt_meta <- c(
  "adam_meta:",
  "  variables:",
  "    - {dataset: ADSL, variable: SEX, codelist: CL.SEX}",
  "    - {dataset: ADAE, variable: AESEV, codelist: CL.AESEV, label: Severity}",
  "    - {dataset: ADAE, variable: AEDECOD, codelist: MEDDRA}",
  "  codelists:",
  "    - {codelist: CL.SEX, name: Sex, value: M, decode: Male, order: 2}",
  "    - {codelist: CL.SEX, name: Sex, value: F, decode: Female, order: 1}",
  "    - {codelist: CL.AESEV, value: MILD}",
  "    - {codelist: CL.AESEV, value: SEVERE}",
  "  external:",
  "    - {codelist: MEDDRA, dictionary: MedDRA, version: '26.0'}")

test_that("adam_meta: rows of a define's tables, in its columns and classes", {
  st <- tfl_read_study_spec(amt_study(amt_meta))
  m <- st$adam_meta
  expect_s3_class(m, "tfl_define")
  def <- tfl_read_define(system.file("extdata", "define", "define.xml", package = "tflspec"))
  for (p in c("datasets", "variables", "codelists", "external", "value_level")) {
    expect_identical(names(m[[p]]), names(def[[p]]), info = p)
    expect_identical(vapply(m[[p]], function(v) class(v)[1L], ""),
                     vapply(def[[p]], function(v) class(v)[1L], ""), info = p)
  }
  expect_identical(m$variables$variable, c("SEX", "AESEV", "AEDECOD"))
  expect_identical(m$variables$label, c(NA, "Severity", NA))
  expect_identical(m$codelists$order, c(2L, 1L, NA, NA))
  expect_identical(m$codelists$decode, c("Male", "Female", NA, NA))
  expect_identical(m$external$version, "26.0")
  expect_identical(nrow(m$value_level), 0L)
  expect_identical(attr(m, "source")$kind, "defaults.yml")
  # (not a layer's: no report gets it through extends:)
  expect_null(st$defaults$adam_meta)
})

test_that("no adam_meta: the define's tables with no rows", {
  st <- tfl_read_study_spec(amt_study("content: []"))
  expect_s3_class(st$adam_meta, "tfl_define")
  expect_identical(nrow(st$adam_meta$variables), 0L)
  expect_identical(nrow(st$adam_meta$codelists), 0L)
  expect_identical(st$codelists, list())
})

test_that("adam_meta: what does not fit a define stops, saying where", {
  rd <- function(...) tfl_read_study_spec(amt_study(c("adam_meta:", ...)))
  expect_error(rd("  variable: []"), "adam_meta: no part variable; the parts are variables")
  expect_error(rd("  variables:", "    - {dataset: ADSL, variable: SEX, codelst: CL.SEX}"),
               "variables: no column codelst")
  expect_error(rd("  variables:", "    - {dataset: ADSL, codelist: CL.SEX}"),
               "variables: row 1 has no variable")
  expect_error(rd("  variables:", "    - {dataset: ADSL, variable: SEX, codelist: CL.SEX}"),
               "ADSL.SEX names the code list CL.SEX, which codelists: does not have")
  expect_error(rd("  codelists:", "    - {codelist: CL.SEX, value: F}", "    - {codelist: CL.SEX, value: F}"),
               "CL.SEX has the value \"F\" twice")
  expect_error(rd("  codelists:", "    - {codelist: CL.SEX, value: F, order: 1.5}"),
               "order is a whole number")
  expect_error(rd("  variables:", "    - {dataset: ADSL, variable: SEX}", "    - {dataset: adsl, variable: SEX}"),
               "adsl.SEX is there twice")
  expect_error(rd("  variables: {dataset: ADSL}"), "variables: is a list of rows")
  expect_error(tfl_read_study_spec(amt_study("adam_meta: [1, 2]")), "adam_meta: is a map of tables")
})

test_that("codelists: the study's lists by name, not a layer's", {
  st <- tfl_read_study_spec(amt_study(c(
    "codelists:",
    "  SEX: [{value: F, label: Female}, {value: M, label: Male}]",
    "  TOXGR: [0, 1, 2]")))
  expect_identical(st$codelists, list(SEX = c(F = "Female", M = "Male"), TOXGR = c("0", "1", "2")))
  expect_identical(st$defaults$codelists, list())
})

test_that("a define read and adam_meta: written are the same tables", {
  def <- tfl_read_define(system.file("extdata", "define", "define.xml", package = "tflspec"))
  v <- def$variables[!is.na(def$variables$codelist), c("dataset", "variable", "codelist")]
  cl <- def$codelists[def$codelists$codelist %in% v$codelist, c("codelist", "name", "value", "decode", "order")]
  cl <- cl[!duplicated(cl[c("codelist", "value")]), , drop = FALSE]
  v <- v[v$codelist %in% c(cl$codelist, def$external$codelist), , drop = FALSE]
  ext <- def$external[def$external$codelist %in% v$codelist, , drop = FALSE]
  rows <- function(d) unname(lapply(seq_len(nrow(d)), function(i) as.list(d[i, , drop = FALSE])))
  y <- list(adam_meta = Filter(length, list(variables = rows(v), codelists = rows(cl), external = rows(ext))))
  d <- amt_study("content: []")
  yaml::write_yaml(y, file.path(d, "spec", "defaults.yml"))
  m <- tfl_read_study_spec(d)$adam_meta
  rownames(v) <- rownames(cl) <- NULL
  expect_identical(m$variables[names(v)], v)
  expect_identical(m$codelists[names(cl)], cl)
})

test_that("the package's sample keeps code lists by name and an adam_meta", {
  st <- tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))
  expect_identical(st$codelists$SEX, c(F = "Female", M = "Male"))
  expect_identical(st$adam_meta$variables$codelist, c("CL.SEX", "CL.AGEGR1", "CL.RACE"))
})
