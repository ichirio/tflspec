# A2 (one population rule), S1's part: the listing's analysis set read alike
# from the sheets and the YAML (Q7), tfl_check_spec()'s look at a figure's or
# a listing's analysis set (R5: Q3, Q4), and its data check of a table whose
# header N and counts give a subject different groups (a warning, nothing
# rewritten).  The arm is a variable the SPEC names: records keep their own.

# T-1: BIGN of the report's N data (ADSL's SAF subjects), the SYSBP summary
# of the records (ADVS)
a2_spec <- function(add = NA_character_, by = "TRT01A", denominator = NA_character_,
                    bign_data = "adsl_saf") {
  tfl_ard_spec(list(
    datasets = data.frame(dataset = c("ADSL", "ADVS", "ADPC"),
                          path = c("adsl.rds", "advs.rds", "adpc.rds")),
    populations = data.frame(population_id = "SAF", dataset = "ADSL", where = "SAFFL == \"Y\""),
    analysis_data = data.frame(output_id = "T-1",
                               data_id = c("adsl_saf", "advs_saf", "advs_sysbp", "adpc_subj"),
                               from = c("ADSL", "ADVS", "advs_saf", "ADPC"),
                               population_id = c("SAF", "SAF", NA, NA),
                               where = c(NA, NA, "PARAMCD == \"SYSBP\"", NA),
                               add = c(NA, add, NA, NA), distinct = c(NA, NA, NA, "USUBJID")),
    analyses = data.frame(output_id = "T-1", analysis_id = c("BIGN", "SYSBP"),
                          method = c("categorical", "continuous"), data = c(bign_data, "advs_sysbp"),
                          by = c(NA, by), variables = c("TRT01A", "AVAL"),
                          denominator = c(NA, denominator))))
}

test_that("what the N check looks at: BIGN's variable counted from another dataset, not taken from the N's", {
  pr <- .n_count_pairs(a2_spec(), "T-1")
  expect_length(pr, 1L)
  expect_identical(pr[[1L]][c("bign", "analysis", "vars", "n_dataset", "dataset")],
                   list(bign = "BIGN", analysis = "SYSBP", vars = "TRT01A", n_dataset = "ADSL",
                        dataset = "ADVS"))
  # the N's rows: its population's; the counts': their data's conditions
  expect_identical(pr[[1L]]$n_where, "SAFFL == \"Y\"")
  expect_identical(pr[[1L]]$where, "PARAMCD == \"SYSBP\"")
  # the variable taken from the subjects (`add` of the data or one it is made
  # from, a denominator's group), another group (a record's TRTA), another
  # report, no BIGN: nothing to look at
  expect_length(.n_count_pairs(a2_spec(add = "TRT01A"), "T-1"), 0L)
  expect_length(.n_count_pairs(a2_spec(denominator = "adsl_saf"), "T-1"), 0L)
  expect_length(.n_count_pairs(a2_spec(by = "TRTA"), "T-1"), 0L)
  expect_length(.n_count_pairs(a2_spec(), "T-2"), 0L)
  x <- a2_spec()
  x$analyses$analysis_id[1L] <- "N"
  expect_length(.n_count_pairs(x, "T-1"), 0L)
  # the N is the report's own, never ADSL by default: the subjects with
  # records in ADPC (an analysis data, one row a subject); counts of ADPC too
  # have nothing to compare, counts of ADVS are compared with ADPC
  expect_identical(.n_count_pairs(a2_spec(bign_data = "adpc_subj"), "T-1")[[1L]]$n_dataset, "ADPC")
  x <- a2_spec(bign_data = "adpc_subj")
  x$analysis_data$from[2L] <- "ADPC"
  expect_length(.n_count_pairs(x, "T-1"), 0L)
  # an analysis with no analysis data: its dataset and its population's
  x <- a2_spec()
  x$analyses$data[2L] <- NA
  x$analyses$dataset[2L] <- "ADVS"
  x$analyses$population_id[2L] <- "SAF"
  expect_identical(.n_count_pairs(x, "T-1")[[1L]]$dataset, "ADVS")
})

test_that("the N check on the data: a warning only when both carry the variable and a subject's value differs", {
  pr <- .n_count_pairs(a2_spec(), "T-1")[[1L]]
  dn <- data.frame(USUBJID = c("S1", "S2", "S3"), TRT01A = c("Drug X", "Control", "Drug X"),
                   SAFFL = c("Y", "Y", "N"))
  dr <- data.frame(USUBJID = c("S1", "S1", "S2", "S3"), PARAMCD = "SYSBP",
                   TRT01A = c("Drug X", "Drug X", "Control", "Control"))
  # the same values (S3 is outside the N's SAF): nothing
  expect_identical(nrow(.n_pair_rows(pr, dn, dr, "USUBJID")), 0L)
  dr$TRT01A[2L] <- "Control"
  p <- .n_pair_rows(pr, dn, dr, "USUBJID")
  expect_identical(p$level, "warning")
  expect_identical(p$item, "analysis SYSBP / by")
  expect_match(p$message, paste0("the header N (BIGN) counts ADSL's TRT01A and analysis SYSBP counts ",
                                 "ADVS's: they differ for 1 subject of the data (S1: Drug X in ADSL, ",
                                 "Drug X, Control in ADVS)"), fixed = TRUE)
  expect_match(p$message, "`add: TRT01A`", fixed = TRUE)
  # a record of another parameter: not counted, not looked at
  dr$PARAMCD[2L] <- "DIABP"
  expect_identical(nrow(.n_pair_rows(pr, dn, dr, "USUBJID")), 0L)
  # one data without the variable: nothing
  dr$PARAMCD[2L] <- "SYSBP"
  expect_identical(nrow(.n_pair_rows(pr, dn, dr[c("USUBJID", "PARAMCD")], "USUBJID")), 0L)
})

test_that("tfl_check_spec(adam = ): the N check's warning, with the study's data or none", {
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  d <- file.path(withr::local_tempdir(), "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  sl <- tflspec:::.ai_chr(tflspec::adsl)
  ae <- tflspec:::.ai_chr(tflspec::adae)
  haven::write_xpt(sl, file.path(d, "adam", "adsl.xpt"))
  haven::write_xpt(ae, file.path(d, "adam", "adae.xpt"))
  tfl_ai_study_init(d, adam = "adam", study_id = "N-1")
  o <- tfl_output("T-N", "table", common = list(population = "SAF"), label = "AE days",
    ard = list(analysis_data = list(list(id = "adsl_sub", from = "ADSL", population = "SAF"),
                                    list(id = "adae_sub", from = "ADAE", subjects_in = "adsl_sub")),
               analyses = list(list(id = "BIGN", method = "categorical", data = "adsl_sub",
                                    variables = "TRT01A"),
                               list(id = "DAYS", method = "cards::ard_summary", data = "adae_sub",
                                    by = "TRT01A", variables = "ASTDY"))),
    content = list(tfl_item("table_plan", list(cols = "TRT01A"))))
  tfl_write_output(o, file.path(d, "spec", "outputs", "T-N.yml"))
  off <- tfl_check_spec(d, "T-N", adam = FALSE)
  # the study's data as they are (ADAE's TRT01A is ADSL's): as without data
  expect_identical(tfl_check_spec(d, "T-N", adam = list(ADSL = sl, ADAE = ae)), off)
  # one record of a SAF subject in the other arm: one warning row of T-N
  saf <- sl$USUBJID[sl$SAFFL %in% "Y" & !is.na(sl$TRT01A)]
  k <- which(ae$USUBJID %in% saf)[1L]
  ae$TRT01A[k] <- setdiff(unique(stats::na.omit(sl$TRT01A)), ae$TRT01A[k])[1L]
  p <- tfl_check_spec(d, "T-N", adam = list(ADSL = sl, ADAE = ae))
  w <- p[grepl("the header N", p$message, fixed = TRUE), ]
  expect_identical(nrow(w), 1L)
  expect_identical(c(w$output_id, w$part, w$level), c("T-N", "ard", "warning"))
  expect_match(w$message, "they differ for 1 subject of the data", fixed = TRUE)
  expect_identical(nrow(p), nrow(off) + 1L)
})

test_that("a listing's analysis set: its own, else the report's -- the sheets and the YAML alike (Q7)", {
  o <- tfl_output("L-1", "listing", common = list(population = "SAF"))
  expect_identical(.data_population(list(dataset = "ADAE"), o), "SAF")
  expect_identical(.data_population(list(dataset = "ADAE", population = "ENR"), o), "ENR")
  expect_true(is.na(.data_population(list(dataset = "ADAE"), tfl_output("L-2", "listing"))))
  d <- withr::local_tempdir()
  dir.create(file.path(d, "spec", "outputs"), recursive = TRUE)
  writeLines(c("datasets:", "  - dataset: ADSL", "    path: data/adsl.rds",
               "  - dataset: ADAE", "    path: data/adae.rds",
               "populations:", "  - id: SAF", "    dataset: ADSL", "    where: !r SAFFL == \"Y\""),
             file.path(d, "spec", "data.yml"))
  cols <- list(tfl_item("plan_listing", list(cols = list(tfl_call("listing_col", list(vars = "USUBJID"))))))
  put <- function(id, common, data) {
    l <- tfl_output(id, "listing", common = common, content = cols)
    l$data <- data
    tfl_write_output(l, file.path(d, "spec", "outputs", paste0(id, ".yml")))
  }
  put("L-1", list(population = "SAF"), list(source = "dataset", dataset = "ADAE"))
  put("L-2", list(), list(source = "dataset", dataset = "ADAE", population = "SAF"))
  put("L-3", list(), list(source = "dataset", dataset = "ADAE"))
  b <- tfl_sheets_from_yaml(d)
  ls <- b$listing$listings
  expect_identical(ls$population_id[match(c("L-1", "L-2"), ls$output_id)], c("SAF", "SAF"))
  expect_true(is.na(ls$population_id[ls$output_id == "L-3"]))
  # back to YAML: the report's set stays the report's (no copy in `data`)
  to <- file.path(d, "back")
  invisible(suppressWarnings(tfl_yaml_from_sheets(b, to)))
  back <- tfl_read_outputs(to)
  expect_null(back[["L-1"]]$data$population)
  expect_identical(back[["L-1"]]$common$population, "SAF")
  expect_identical(back[["L-2"]]$data$population, "SAF")
})

a2_study <- function() {
  list(data = list(populations = list(
    list(id = "SAF", dataset = "ADSL", where = tfl_r("SAFFL == \"Y\"")),
    list(id = "ENR", dataset = "ADSL"))))
}

test_that("a figure or listing of subjects' records with no analysis set: a warning (R5, Q3)", {
  st <- a2_study()
  l <- list(kind = "listing", data = list(source = "dataset", dataset = "ADAE"))
  p <- .check_static_rows(l, "L-1", st)
  expect_identical(p$level, "warning")
  expect_identical(p$item, "common$population")
  expect_match(p$message, "has no analysis set: write `common: population`", fixed = TRUE)
  # the report's set, the data's own, or the text of all subjects: quiet
  for (o in list(c(l, list(common = list(population = "SAF"))),
                 c(l, list(common = list(population_label = "All subjects"))),
                 list(kind = "listing", data = list(source = "dataset", dataset = "ADAE",
                                                    population = "SAF")))) {
    expect_identical(nrow(.check_static_rows(o, "L-1", st)), 0L)
  }
  # a figure of its own code, of its design's datasets: the same
  f <- list(kind = "figure", data = list(source = "code"), content = tfl_r("df <- adsl"))
  expect_identical(.check_static_rows(f, "F-1", st)$level, "warning")
  # a figure of its ARD, a table: no records read here
  expect_identical(nrow(.check_static_rows(list(kind = "figure", template = "km_simple"), "F-2", st)), 0L)
  expect_identical(nrow(.check_static_rows(list(kind = "table", data = list(source = "dataset",
                                                                            dataset = "ADAE")), "T-1", st)), 0L)
})

test_that("a step that keeps a population's rows by the records' flag: a warning, the set is the report's (R5, Q4)", {
  st <- a2_study()
  f <- list(kind = "figure", common = list(population = "SAF"), data = list(source = "code", steps = list(
    list(step = "read", dataset = "ADTTE"),
    list(step = "param", value = "TTSKIN"),
    list(step = "flag", variable = "SAFFL"),
    list(step = "flag", variable = "ANL01FL"),
    list(step = "filter", expr = "AVAL > 0 & SAFFL == \"Y\""))))
  p <- .check_static_rows(f, "F-1", st)
  expect_identical(p$level, c("warning", "warning"))
  expect_identical(p$item, c("data$steps[3]", "data$steps[5]"))
  expect_match(p$message[1L], "step 3 (`flag: SAFFL`) keeps the rows of population SAF", fixed = TRUE)
  expect_match(p$message[1L], "`common: population` (SAF)", fixed = TRUE)
  # no report set: the message says which to write (and Q3 says it has none)
  f$common <- NULL
  p2 <- .check_static_rows(f, "F-1", st)
  expect_true(any(grepl("write `common: population: SAF`", p2$message, fixed = TRUE)))
  # (a record flag, a population with no condition, no study: nothing)
  f$common <- list(population = "SAF")
  f$data$steps <- f$data$steps[c(1L, 2L, 4L)]
  expect_identical(nrow(.check_static_rows(f, "F-1", st)), 0L)
  f$data$steps[[3L]] <- list(step = "flag", variable = "SAFFL")
  expect_identical(nrow(.check_static_rows(f, "F-1")), 0L)
})
