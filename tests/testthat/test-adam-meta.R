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
  cl <- st$codelists
  # (TOXGR's codes are numbers in the YAML, not text: read as text, with a
  # warning row each to quote them, R/check_spec.R)
  expect_identical(attr(cl, "problems")$code, c("0", "1", "2"))
  expect_identical(unique(attr(cl, "problems")$level), "warning")
  attr(cl, "problems") <- NULL
  expect_identical(cl, list(SEX = c(F = "Female", M = "Male"), TOXGR = c("0", "1", "2")))
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

# ---- the programs' own reads: adam_meta's levels, as study$adam has them -----

# A study on the package's ADSL (factors as text) whose adam_meta maps ADSL.SEX
# to a list with a code no subject has (U); a report counts SEX by ARM
# (levels: define_xml, the file applied; tfl_ai_study_init() writes none)
amt_run_study <- function(levels = "define_xml", env = parent.frame()) {
  skip_if_not_installed("haven")
  d <- withr_tempdir(env)
  dir.create(file.path(d, "adam"))
  haven::write_xpt(.ai_chr(tflspec::adsl), file.path(d, "adam", "adsl.xpt"))
  suppressMessages(tfl_ai_study_init(d, adam = "adam", study_id = "AM-1"))
  if (!is.null(levels)) {
    st <- yaml::read_yaml(file.path(d, "study.yml"))
    st$settings$adam_levels <- levels
    write_study_yml(d, st)
  }
  cat("adam_meta:",
      "  variables: [{dataset: ADSL, variable: SEX, codelist: CL.SEX}]",
      "  codelists:",
      "    - {codelist: CL.SEX, value: M, order: 2}",
      "    - {codelist: CL.SEX, value: F, order: 1}",
      "    - {codelist: CL.SEX, value: U, order: 3}", "",
      sep = "\n", file = file.path(d, "spec", "defaults.yml"), append = TRUE)
  writeLines(c("id: T-AM", "kind: table", "common: {population: SAF}", "extends: none",
               "analysis:", "  analyses:", "    - id: A1", "      method: categorical",
               "      population: SAF", "      by: [ARM]", "      variables: [SEX]",
               "content:", "  - table_plan: {cols: ARM}"),
             file.path(d, "spec", "outputs", "T-AM.yml"))
  d
}
amt_sex <- function(ard) {
  x <- ard[ard$variable == "SEX" & ard$stat_name == "n" & !is.na(ard$group1), ]
  unique(unlist(lapply(x$variable_level, as.character)))
}

test_that("the ARD program's own read puts adam_meta's levels on, after its study$adam_meta", {
  skip_on_cran()
  d <- amt_run_study()
  st <- tfl_read_study_spec(d)
  code <- tfl_ard_code(st, "T-AM", save = FALSE)
  i <- which(code == "study <- list()")
  expect_length(i, 1L)
  expect_true(any(grepl("~dataset, ~variable, ~codelist", code, fixed = TRUE)))
  j <- grep("set_levels(SEX = adam_codes(\"CL.SEX\"))", code, fixed = TRUE)
  expect_length(j, 1L)
  expect_lt(i, j)
  # (blanks as NA first, then the levels)
  expect_match(code[j], "blank_as_na() |>\n  set_levels(SEX", fixed = TRUE)
  # (no report list of SEX: the program keeps the study's levels, the zero row of U)
  expect_false(any(grepl("^cl_sex", code)))
  ard <- suppressMessages(tfl_build_ard(st, dir = d, output_id = "T-AM", save = FALSE))
  expect_identical(amt_sex(ard), c("F", "M", "U"))
  u <- ard[ard$variable == "SEX" & ard$stat_name == "n" &
             vapply(ard$variable_level, function(v) identical(as.character(v), "U"), NA), ]
  expect_true(nrow(u) > 0L && all(unlist(u$stat) == 0))
  # the setup and the program reading study$adam: the same ARD
  old <- options(tflspec.study_setup = TRUE)
  on.exit(options(old), add = TRUE)
  setup <- tfl_study_setup_code(st)
  prog <- tfl_ard_code(st, "T-AM", save = FALSE)
  options(old)
  expect_false(any(grepl("study <- list()", prog, fixed = TRUE)))
  env <- new.env(parent = globalenv())
  wd <- setwd(d)
  on.exit(setwd(wd), add = TRUE)
  eval(parse(text = tfl_helpers_code()), env)
  # (the setup sets the study's rounding options: put back after)
  opt_keep <- options(rtfreporter.rounding = getOption("rtfreporter.rounding"),
                  cards.round_type = getOption("cards.round_type"))
  on.exit(options(opt_keep), add = TRUE)
  suppressMessages(eval(parse(text = c(setup, prog)), env))
  setwd(wd)
  keep <- setdiff(names(ard), "fmt_fun")
  expect_identical(as.data.frame(env$ard)[keep], as.data.frame(ard)[keep])
})

test_that("adam_levels: none -- the own read has no levels; nor has a study with no adam_meta", {
  skip_on_cran()
  d <- amt_run_study("none")
  st <- tfl_read_study_spec(d)
  code <- tfl_ard_code(st, "T-AM", save = FALSE)
  expect_false(any(grepl("set_levels(SEX = adam_codes", code, fixed = TRUE)))
  expect_false("study <- list()" %in% code)
  ard <- suppressMessages(tfl_build_ard(st, dir = d, output_id = "T-AM", save = FALSE))
  expect_identical(amt_sex(ard), c("F", "M"))
  # the fingerprint: under define_xml adam_meta counts, under none it does not
  st2 <- tfl_read_study_spec(amt_run_study())
  h <- function(s) grep("definition = ", tfl_ard_code(s, "T-AM", save = TRUE, part = "body"), value = TRUE)
  expect_false(identical(h(st), h(st2)))
})

test_that("a listing's own read and tflspec's own reads put the same levels on", {
  skip_on_cran()
  d <- amt_run_study()
  st <- tfl_read_study_spec(d)
  ard <- tflspec:::.ard_spec_of_study(st, output_id = "T-AM")$spec
  expect_identical(ard$adam_meta$variables$variable, "SEX")
  sp <- tfl_listing_spec(data.frame(output_id = "L-1", dataset = "ADSL", sort = "USUBJID"),
                         data.frame(output_id = "L-1", vars = "USUBJID | SEX"))
  code <- tfl_listing_code(sp, "L-1", ard = ard, part = "data")
  expect_true("adsl <- set_levels(adsl, SEX = adam_codes(\"CL.SEX\"))" %in% code)
  expect_lt(which(code == "study <- list()"), which(code == "adsl <- blank_as_na(adsl)"))
  env <- new.env(parent = globalenv())
  eval(parse(text = tfl_helpers_code()), env)
  wd <- setwd(d)
  on.exit(setwd(wd), add = TRUE)
  suppressMessages(eval(parse(text = code), env))
  setwd(wd)
  expect_identical(levels(env$data$SEX), c("F", "M", "U"))
  # without adam_meta: as before
  expect_false(any(grepl("adam_codes", tfl_read_data_code(ard$datasets, "ADSL"))))
  # the AI draws' read (.ai_read_adam()): the same levels; none under none
  a <- .ai_read_adam(file.path(d, "adam", "adsl.xpt"), "ADSL", st)
  expect_identical(levels(a$SEX), c("F", "M", "U"))
  b <- .ai_read_adam(file.path(d, "adam", "adsl.xpt"), "ADSL", tfl_read_study_spec(amt_run_study("none")))
  expect_true(is.character(b$SEX))
  expect_true(is.character(.ai_read_adam(file.path(d, "adam", "adsl.xpt"))$SEX))
})

# ---- a filter narrows adam_meta's levels, as a report's own list (Q3) -------

test_that("a data whose filter keeps one level of an adam_meta column has that level only", {
  skip_on_cran()
  d <- amt_run_study()
  writeLines(c("id: T-AN", "kind: table", "common: {population: SAF}", "extends: none",
               "data:", "  - {id: adsl_f, from: ADSL, population: SAF, where: !r SEX == \"F\"}",
               "analysis:", "  analyses:", "    - id: A1", "      method: categorical",
               "      data: adsl_f", "      by: [ARM]", "      variables: [SEX]",
               "content:", "  - table_plan: {cols: ARM}"),
             file.path(d, "spec", "outputs", "T-AN.yml"))
  st <- tfl_read_study_spec(d)
  code <- tfl_ard_code(st, "T-AN", save = FALSE)
  expect_true(any(grepl('set_levels(SEX = intersect(adam_codes("CL.SEX"), c("F")))', code, fixed = TRUE)))
  ard <- suppressMessages(tfl_build_ard(st, dir = d, output_id = "T-AN", save = FALSE))
  # (no zero row of M or U: the filter left them out)
  expect_identical(amt_sex(ard), "F")
  # the setup and the program: the same
  old <- options(tflspec.study_setup = TRUE)
  setup <- tfl_study_setup_code(st)
  prog <- tfl_ard_code(st, "T-AN", save = FALSE)
  options(old)
  env <- new.env(parent = globalenv())
  wd <- setwd(d)
  on.exit(setwd(wd), add = TRUE)
  eval(parse(text = tfl_helpers_code()), env)
  # (the setup sets the study's rounding options: put back after)
  opt_keep <- options(rtfreporter.rounding = getOption("rtfreporter.rounding"),
                  cards.round_type = getOption("cards.round_type"))
  on.exit(options(opt_keep), add = TRUE)
  suppressMessages(eval(parse(text = c(setup, prog)), env))
  setwd(wd)
  keep <- setdiff(names(ard), "fmt_fun")
  expect_identical(as.data.frame(env$ard)[keep], as.data.frame(ard)[keep])
  # a report's own list of the column: its step narrows it, not adam_meta's
  own <- readLines(file.path(d, "spec", "outputs", "T-AN.yml"))
  writeLines(c(own, "codelists:", "  SEX: [{value: F, label: Female}, {value: M, label: Male}]"),
             file.path(d, "spec", "outputs", "T-AN.yml"))
  code <- tfl_ard_code(tfl_read_study_spec(d), "T-AN", save = FALSE)
  expect_false(any(grepl("intersect(adam_codes", code, fixed = TRUE)))
  expect_true(any(grepl('cl_sex[c("F")]', code, fixed = TRUE)))
})

test_that("adam_meta's columns count as listed for the analysis's dataset and ADSL", {
  meta <- list(variables = data.frame(dataset = c("ADSL", "ADAE"), variable = c("SEX", "AESEV"),
                                      codelist = c("CL.SEX", "CL.AESEV"), stringsAsFactors = FALSE))
  expect_named(.adam_listed(list(), meta), c("SEX", "AESEV"))
  expect_named(.adam_listed(list(), meta, "ADSL"), "SEX")
  expect_named(.adam_listed(list(), meta, "ADAE"), c("SEX", "AESEV"))
  expect_named(.adam_listed(list(ARM = "A"), meta, "ADLB"), c("ARM", "SEX"))
})

# ---- tfl_check_spec(): a study list named like a column adam_meta does not map

test_that("tfl_check_spec() warns of a study code list named like an ADaM column adam_meta does not map", {
  skip_on_cran()
  d <- amt_run_study()
  f <- file.path(d, "spec", "defaults.yml")
  cat("codelists:", "  SEX: [{value: F, label: Female}, {value: M, label: Male}]",
      "  RACE: [{value: WHITE, label: White}]", "  MYLIST: [{value: A}]", "",
      sep = "\n", file = f, append = TRUE)
  p <- suppressMessages(tfl_check_spec(d))
  w <- p[p$part %in% "study", , drop = FALSE]
  # (SEX is mapped; MYLIST names no column; RACE is ADSL's and not mapped)
  expect_identical(w$item, "codelists: RACE")
  expect_identical(w$level, "warning")
  expect_true(is.na(w$output_id))
  expect_match(w$message, "named like the column ADSL.RACE, which adam_meta does not map", fixed = TRUE)
  # under adam_levels: none, nothing
  st <- yaml::read_yaml(file.path(d, "study.yml"))
  st$settings$adam_levels <- "none"
  write_study_yml(d, st)
  p <- suppressMessages(tfl_check_spec(d))
  expect_identical(sum(p$part %in% "study"), 0L)
})

# ---- references to the study's code lists in content -----------------------

test_that("study$codelists references: left out when the study has not the list, put in for a run", {
  item <- function(args) tfl_item("plan_labels", args)
  ct <- list(item(list(SEX = tfl_r('c(SEX = "Sex", study$codelists$SEX)'),
                       RACE = tfl_r("study$codelists$RACE"), AGE = "Age")))
  p <- .study_refs_prune(ct, "SEX")
  expect_identical(names(p[[1L]]$args), c("SEX", "AGE"))
  expect_identical(p[[1L]]$args$SEX, ct[[1L]]$args$SEX)
  p0 <- .study_refs_prune(ct, character())
  expect_identical(as.character(unclass(p0[[1L]]$args$SEX)), 'c(SEX = "Sex")')
  e <- .study_refs_eval(ct, list(SEX = c(F = "Female", M = "Male")))
  expect_identical(e[[1L]]$args$SEX, c(SEX = "Sex", F = "Female", M = "Male"))
  expect_false("RACE" %in% names(e[[1L]]$args))
  expect_identical(e[[1L]]$args$AGE, "Age")
})

test_that("demog's value_labels: the study's lists by name, for the columns the report shows", {
  # (demog left tflspec_v1, canon 2026-11-24: its copy in the test series)
  local_test_series()
  r <- list(id = "T-DM", kind = "table", common = list(population = "SAF"),
            template = list(name = "tflspec_test/demog",
                            analysis = list(by = list("TRT01A"), categorical = list("SEX", "AGEGR1")),
                            content = list(labels = list(SEX = "Sex, n (%)"))))
  o <- tfl_template_resolve(r)
  lab <- Filter(function(it) identical(it$fn, "plan_labels"), o$content)
  expect_length(lab, 1L)
  a <- lab[[1L]]$args
  # (the heading under `variable`, as every path writes it: tflspec#856)
  expect_identical(names(a), c("variable", "SEX", "AGEGR1"))
  expect_identical(a$variable, list(SEX = "Sex, n (%)"))
  expect_identical(as.character(unclass(a$SEX)), "study$codelists$SEX")
  expect_identical(as.character(unclass(a$AGEGR1)), "study$codelists$AGEGR1")
  # a report's own map wins; an empty one: no value labels
  r$template$content$value_labels <- list()
  o <- tfl_template_resolve(r)
  lab <- Filter(function(it) identical(it$fn, "plan_labels"), o$content)
  expect_identical(lab[[1L]]$args, list(variable = list(SEX = "Sex, n (%)")))
})
