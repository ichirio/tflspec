# An analysis as calls (for the YAML spec's generators): the same call the
# program writes, as list(fn, args)

ac_df <- function(...) {
  rows <- list(...)
  cols <- unique(unlist(lapply(rows, names)))
  as.data.frame(lapply(stats::setNames(cols, cols), function(cn)
    vapply(rows, function(r) as.character(r[[cn]] %||% NA), "")),
    stringsAsFactors = FALSE)
}

ac_spec <- function() {
  s <- list(
    study = ac_df(list(key = "id", value = "USUBJID")),
    datasets = ac_df(list(dataset = "ADSL", path = "adam/adsl.rds"),
                     list(dataset = "ADAE", path = "adam/adae.rds")),
    populations = ac_df(list(population_id = "SAF", dataset = "ADSL", where = "SAFFL == \"Y\"")),
    analyses = ac_df(
      list(output_id = "DM", analysis_id = "GROUPN", method = "categorical",
           population_id = "SAF", variables = "TRT01A"),
      list(output_id = "DM", analysis_id = "AGE", method = "continuous",
           population_id = "SAF", by = "TRT01A", variables = "AGE",
           statistics = "N | mean | sd", overall = "TRUE"),
      list(output_id = "DM", analysis_id = "CAT", method = "categorical",
           population_id = "SAF", by = "TRT01A", variables = "AGEGR1 | SEX"),
      list(output_id = "AE", analysis_id = "TEAE", method = "hierarchical",
           dataset = "ADAE", population_id = "SAF", where = "TRTEMFL == \"Y\"",
           by = "TRTA", variables = "AEBODSYS | AEDECOD", args = "over_variables = TRUE"),
      list(output_id = "AE", analysis_id = "CHI", method = "cardx::ard_stats_chisq_test",
           population_id = "SAF", by = "TRT01A", variables = "SEX"),
      list(output_id = "AE", analysis_id = "OWN", method = "custom",
           population_id = "SAF", code = "cards::ard_summary(data, variables = AGE)")))
  for (n in names(.ard_spec_sheets)) {
    for (c in .ard_spec_sheets[[n]]) if (!c %in% names(s[[n]])) s[[n]][[c]] <- NA_character_
    s[[n]] <- s[[n]][.ard_spec_sheets[[n]]]
  }
  tfl_ard_spec(s)
}

test_that("every analysis comes back as the call the program writes", {
  x <- ac_spec()
  keys <- tfl_ard_methods()
  subj <- "USUBJID"
  n <- 0L
  for (i in seq_len(nrow(x$analyses))) {
    r <- x$analyses[i, , drop = FALSE]
    res <- tflspec:::.ard_analysis_call(x, r, keys, subj)
    expect_true(is.list(res), info = r$analysis_id)
    if (is.null(res$call)) {
      expect_s3_class(res$code, "tfl_r")
      next
    }
    expect_type(res$call$fn, "character")
    given <- c(tflspec:::.args_given(r$args), if (!is.na(r$strata)) "strata",
               if (!is.na(r$denominator)) "denominator")
    has <- function(arg) arg %in% given
    k <- tflspec:::.method_key(r$method, keys)
    pid <- r$population_id
    pop <- if (!is.na(pid)) paste0("pop_", tflspec:::.r_name(pid)) else "NULL"
    den <- tflspec:::.den_code(r$denominator, x, if (!is.na(pid)) pop, subj, population = pop)
    fmt <- c(if (!is.na(k)) tflspec:::.parse_formats(keys$formats[k]), tflspec:::.parse_formats(r$formats))
    fmt <- fmt[!duplicated(names(fmt), fromLast = TRUE)]
    fmt_fun <- tflspec:::.call_fmt_fun(r, keys, given, fmt)
    node <- tflspec:::.analysis_node(r, keys, subj, has, den, data = "data", population = pop, fmt_fun = fmt_fun)
    expect_identical(tflspec:::.lay(tflspec:::.call_as_cl(res$call)), tflspec:::.lay(node),
                     info = r$analysis_id)
    expect_identical(res$steps[[length(res$steps)]]$fn, "tag_ard")
    n <- n + 1L
  }
  expect_identical(n, 5L)
  # the Total column's analysis says so
  age <- tflspec:::.ard_analysis_call(x, x$analyses[2L, , drop = FALSE], keys, subj)
  expect_true(age$overall)
  expect_identical(age$call$fn, "cards::ard_summary")
})
