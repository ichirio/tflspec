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
