# The ARD's analyses as calls: the code written from them is the program's,
# and the code, run, makes what the calls make run directly.

sa_df <- function(...) {
  rows <- list(...)
  cols <- unique(unlist(lapply(rows, names)))
  as.data.frame(lapply(stats::setNames(cols, cols), function(cn)
    vapply(rows, function(r) as.character(r[[cn]] %||% NA), "")),
    stringsAsFactors = FALSE)
}

sa_spec <- function() {
  s <- list(
    study = sa_df(list(key = "id", value = "USUBJID")),
    datasets = sa_df(list(dataset = "ADSL", path = "adam/adsl.rds")),
    populations = sa_df(list(population_id = "SAF", dataset = "ADSL", where = "SAFFL == \"Y\"")),
    analyses = sa_df(
      list(output_id = "DM", analysis_id = "AGE", method = "continuous",
           population_id = "SAF", by = "ARM", variables = "AGE",
           statistics = "N | mean | sd", overall = "TRUE", label = "Age"),
      list(output_id = "DM", analysis_id = "CAT", method = "categorical",
           population_id = "SAF", by = "ARM", variables = "AGEGR1 | SEX"),
      list(output_id = "DM", analysis_id = "CHI", method = "cardx::ard_stats_chisq_test",
           population_id = "SAF", by = "ARM", variables = "SEX")))
  for (n in names(tflspec:::.ard_spec_sheets)) {
    for (c in tflspec:::.ard_spec_sheets[[n]]) if (!c %in% names(s[[n]])) s[[n]][[c]] <- NA_character_
    s[[n]] <- s[[n]][tflspec:::.ard_spec_sheets[[n]]]
  }
  tfl_ard_spec(s)
}

test_that("an analysis's calls write the code the program writes", {
  x <- sa_spec()
  keys <- tfl_ard_methods()
  for (i in seq_len(nrow(x$analyses))) {
    r <- x$analyses[i, , drop = FALSE]
    res <- tflspec:::.ard_analysis_call(x, r, keys, "USUBJID")
    expect_identical(
      tflspec:::.ard_call_lines(res, r, "data", "ard_x"),
      tflspec:::.ard_analysis_lines(x, r, keys, "USUBJID", "data", "ard_x", character()),
      info = r$analysis_id)
  }
})

test_that("an analysis's code, run, is what its calls make", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  x <- sa_spec()
  keys <- tfl_ard_methods()
  env <- new.env(parent = globalenv())
  sys.source(system.file("helpers", "study_helpers.R", package = "tflspec"), envir = env)
  # the program's own definitions: the company standards' formats
  eval(parse(text = tflspec:::.ard_helpers(c("N", "mean", "sd"))), env)
  env$data <- cards::ADSL[cards::ADSL$SAFFL == "Y", ]
  env$pop_saf <- env$data
  env$report_id <- "DM"
  for (i in seq_len(nrow(x$analyses))) {
    r <- x$analyses[i, , drop = FALSE]
    res <- tflspec:::.ard_analysis_call(x, r, keys, "USUBJID")
    run_env <- new.env(parent = env)
    eval(parse(text = tflspec:::.ard_call_lines(res, r, "data", "ard_x")), run_env)
    direct <- tflspec:::.ard_call_run(res, env)
    # the whole ARD, but the functions cards keeps (closures differ by
    # their environment, not by what they do)
    plain <- function(a) as.data.frame(a)[setdiff(names(a), c("fmt_fun", "fmt_fn"))]
    expect_identical(plain(run_env$ard_x), plain(direct), info = r$analysis_id)
  }
})

# a YAML study of two tables, the TOC listing the second first
sa_yaml_study <- function(dir) {
  dir.create(file.path(dir, "spec", "outputs"), recursive = TRUE)
  writeLines(c("study_id: SA-1", "keys:", "  id: USUBJID", "  output: output/ard/ard.rds"),
             file.path(dir, "study.yml"))
  writeLines(c("datasets:", "  - dataset: ADSL", "    level: ADaM", "    path: data/adam/adsl.rds",
               "populations:", "  - id: SAF", "    dataset: ADSL", "    where: !r SAFFL == \"Y\""),
             file.path(dir, "spec", "data.yml"))
  writeLines(c("outputs:", "  - id: T-2", "    kind: table", "  - id: T-1", "    kind: table"),
             file.path(dir, "spec", "toc.yml"))
  one <- function(id, an, method, vars) {
    writeLines(c(paste("id:", id), "kind: table", "extends: none", "ard:", "  analyses:",
                 paste("    - id:", an), paste("      method:", method),
                 "      population: SAF", "      by: [ARM]", paste0("      variables: [", vars, "]"),
                 "content:", "  - table_plan: {cols: ARM}"),
               file.path(dir, "spec", "outputs", paste0(id, ".yml")))
  }
  one("T-1", "AGE", "continuous", "AGE")
  one("T-2", "SEX", "categorical", "SEX")
  dir
}

test_that("a YAML study's ARD program: its reports' analyses, in the TOC's order", {
  dir <- sa_yaml_study(withr::local_tempdir())
  st <- tfl_read_study_spec(dir)
  code <- tfl_ard_code(st)
  at <- function(id) grep(paste0("report_id <- \"", id, "\""), code, fixed = TRUE)
  expect_length(at("T-1"), 1L)
  expect_lt(at("T-2"), at("T-1"))
  expect_true(any(grepl("ard_summary(", code, fixed = TRUE)))
  expect_identical(tfl_ard_code(st, "T-1", part = "body"),
                   tfl_ard_code(.ard_spec_of_study(st)$spec, "T-1", part = "body"))
})

# a spec/codelists.yml left in a study is not read (a message, no layer)
sa_codelists_file <- function(dir, lines) {
  writeLines(lines, file.path(dir, "spec", "codelists.yml"))
  dir
}

test_that("a study with spec/codelists.yml builds its ARD as without the file (#649, #659)", {
  skip_on_cran()
  dir <- sa_yaml_study(withr::local_tempdir())
  dir.create(file.path(dir, "data", "adam"), recursive = TRUE)
  saveRDS(getExportedValue("tflspec", "adsl"), file.path(dir, "data", "adam", "adsl.rds"))
  without <- suppressMessages(tfl_build_ard(tfl_read_study_spec(dir), dir = dir,
                                            output_id = "T-1", save = FALSE))
  code_without <- tfl_ard_code(tfl_read_study_spec(dir), "T-1", save = FALSE)
  sa_codelists_file(dir, "ANRIND: [{value: LOW, label: Low}, {value: HIGH, label: High}]")
  expect_message(st <- tfl_read_study_spec(dir), "spec/codelists.yml is not read", fixed = TRUE)
  with <- suppressMessages(tfl_build_ard(st, dir = dir, output_id = "T-1", save = FALSE))
  expect_gt(nrow(with), 0L)
  expect_identical(as.data.frame(with)[c("output_id", "variable", "stat_name")],
                   as.data.frame(without)[c("output_id", "variable", "stat_name")])
  # nor is it the sheets'
  expect_identical(nrow(tfl_sheets_from_yaml(dir)$table$codelists), 0L)
  # the program does not apply the unused list: it is the one without the file
  code <- tfl_ard_code(st, "T-1", save = FALSE)
  expect_false(any(grepl("ANRIND|HIGH", code)))
  expect_identical(code, code_without)
})

test_that("a report's own code list applies, not the file's of the same variable (#649, #659)", {
  dir <- sa_yaml_study(withr::local_tempdir())
  f <- file.path(dir, "spec", "outputs", "T-2.yml")
  writeLines(c(readLines(f), "codelists:", "  SEX:", "    - {value: M, label: Men}",
               "    - {value: F, label: Women}"), f)
  base <- tfl_ard_code(tfl_read_study_spec(dir), "T-2", save = FALSE)
  expect_true(any(grepl("Women", base)))
  sa_codelists_file(dir, "SEX: [{value: F, label: Girls}, {value: M, label: Boys}]")
  code <- tfl_ard_code(suppressMessages(tfl_read_study_spec(dir)), "T-2", save = FALSE)
  expect_identical(code, base)
  expect_false(any(grepl("Girls|Boys", code)))
})
