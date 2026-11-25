# A2 (one population rule), S1's part: the listing's analysis set read alike
# from the sheets and the YAML (Q7), tfl_check_spec()'s look at a figure's or
# a listing's analysis set (R5: Q3), and its data check of a table whose
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
    content = list(tfl_item("table_plan", list(cols = "TRT01A")),
                   tfl_item("plan_col_header", list(header = "{col}\n(N={n})"))))
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
  # (Codex review 2026-11-26, decided by the coordinator 11-26) an N the
  # header does not show: not looked at
  o$content <- o$content[1L]
  tfl_write_output(o, file.path(d, "spec", "outputs", "T-N.yml"))
  p <- tfl_check_spec(d, "T-N", adam = list(ADSL = sl, ADAE = ae))
  expect_false(any(grepl("the header N", p$message, fixed = TRUE)))
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
  # the planner's population column: the data's set, else `common`'s
  pl <- b$planner
  expect_identical(pl$population[match(c("L-1", "L-2"), pl$output_id)], c("SAF", "SAF"))
  expect_false(identical(pl$population[match("L-3", pl$output_id)], "SAF"))
  # back to YAML: the set is on the data, not in `common` (canon 11-15)
  to <- file.path(d, "back")
  invisible(suppressWarnings(tfl_yaml_from_sheets(b, to)))
  back <- tfl_read_outputs(to)
  expect_identical(back[["L-1"]]$data$population, "SAF")
  expect_null(back[["L-1"]]$common$population)
  expect_identical(back[["L-2"]]$data$population, "SAF")
  expect_null(back[["L-2"]]$common$population)
  expect_null(back[["L-3"]]$data$population)
})

test_that("a figure or listing of subjects' records with no analysis set: a warning (R5, Q3)", {
  l <- list(kind = "listing", data = list(source = "dataset", dataset = "ADAE"))
  p <- .check_static_rows(l, "L-1")
  expect_identical(p$level, "warning")
  expect_identical(p$item, "data$population")
  expect_match(p$message, "has no analysis set: write `population:` on its data", fixed = TRUE)
  # the report's set, the data's own, or the text of all subjects: quiet
  for (o in list(c(l, list(common = list(population = "SAF"))),
                 c(l, list(common = list(population_label = "All subjects"))),
                 list(kind = "listing", data = list(source = "dataset", dataset = "ADAE",
                                                    population = "SAF")))) {
    expect_identical(nrow(.check_static_rows(o, "L-1")), 0L)
  }
  # a figure of its own code, of its design's datasets: the same
  f <- list(kind = "figure", data = list(source = "code"), content = tfl_r("df <- adsl"))
  expect_identical(.check_static_rows(f, "F-1")$level, "warning")
  # a figure of its ARD, a table: no records read here
  expect_identical(nrow(.check_static_rows(list(kind = "figure", template = "km_simple"), "F-2")), 0L)
  expect_identical(nrow(.check_static_rows(list(kind = "table", data = list(source = "dataset",
                                                                            dataset = "ADAE")), "T-1")), 0L)
})

test_that("Q3: a set on the analysis data rows counts (canon 11-15)", {
  f <- list(kind = "figure", data = list(source = "code"),
            ard = list(analysis_data = list(list(id = "adtte_sub", from = "ADTTE", population = "SAF"),
                                            list(id = "adsl_sub", from = "ADSL", population = "SAF"))))
  r <- .check_static_rows(f, "F-1")
  expect_false(any(r$item %in% "data$population"))
  f$ard$analysis_data <- lapply(f$ard$analysis_data, function(a) { a$population <- NULL; a })
  r <- .check_static_rows(f, "F-1")
  expect_true(any(r$item %in% "data$population"))
})

test_that("the page's analysis set line reads the data's set (canon 11-15)", {
  st <- list(data = list(populations = list(list(id = "SAF", label = "Safety Analysis Set"),
                                            list(id = "FAS", label = "Full Analysis Set"))))
  fig <- list(kind = "figure", data = list(population = "SAF", source = "code"))
  expect_identical(.page_own_tokens(fig, st)$OUTPUT_POPULATION, "Safety Analysis Set")
  # the data's set wins over `common` (read only until it goes)
  fig$common <- list(population = "FAS")
  expect_identical(.page_own_tokens(fig, st)$OUTPUT_POPULATION, "Safety Analysis Set")
  fig$data$population <- NULL
  expect_identical(.page_own_tokens(fig, st)$OUTPUT_POPULATION, "Full Analysis Set")
  # a report's data entries (a list of rows): the subjects row's set
  tab <- list(kind = "table", data = list(list(id = "adsl_sub", from = "ADSL", population = "FAS")))
  expect_identical(.page_own_tokens(tab, st)$OUTPUT_POPULATION, "Full Analysis Set")
})

test_that("a figure's set on its data: the sheets keep its subjects row's set (canon 11-15)", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "spec", "outputs"), recursive = TRUE)
  writeLines(c("datasets:", "  - dataset: ADSL", "    path: data/adsl.rds",
               "populations:", "  - id: SAF", "    dataset: ADSL", "    where: !r SAFFL == \"Y\""),
             file.path(d, "spec", "data.yml"))
  f <- tfl_output("F-1", "figure", content = tfl_r("df <- adsl_saf"))
  f$data <- list(source = "code", population = "SAF")
  f$ard <- list(analysis_data = list(list(id = "adsl_saf", from = "ADSL", population = "SAF")),
                analyses = list(list(id = "N", method = "categorical", data = "adsl_saf",
                                     variables = list("TRT01A"))))
  tfl_write_output(f, file.path(d, "spec", "outputs", "F-1.yml"))
  b <- tfl_sheets_from_yaml(d)
  to <- file.path(d, "back")
  invisible(suppressWarnings(tfl_yaml_from_sheets(b, to)))
  back <- tfl_read_outputs(to)[["F-1"]]
  # the ARD program reads the row's set: it stays on the row
  ad <- back$ard$analysis_data
  expect_identical(ad[[match("adsl_saf", vapply(ad, function(a) a$id, ""))]]$population, "SAF")
  expect_identical(back$data$population, "SAF")
  expect_null(back$common[["population"]])
})

test_that("a report with only a population text has no analysis set (no `$` partial match)", {
  o <- tfl_output("L-1", "listing", common = list(population_label = "All subjects"))
  expect_true(is.na(.out_population(o)))
  expect_true(is.na(.data_population(list(source = "dataset", dataset = "ADAE"), o)))
  d <- withr::local_tempdir()
  dir.create(file.path(d, "spec", "outputs"), recursive = TRUE)
  writeLines(c("datasets:", "  - dataset: ADAE", "    path: data/adae.rds"), file.path(d, "spec", "data.yml"))
  o$content <- list(tfl_item("plan_listing", list(cols = list(tfl_call("listing_col", list(vars = "USUBJID"))))))
  o$data <- list(source = "dataset", dataset = "ADAE")
  tfl_write_output(o, file.path(d, "spec", "outputs", "L-1.yml"))
  b <- tfl_sheets_from_yaml(d)
  expect_true(is.na(b$listing$listings$population_id[b$listing$listings$output_id == "L-1"]))
  expect_false(identical(b$planner$population[match("L-1", b$planner$output_id)], "All subjects"))
  to <- file.path(d, "back")
  invisible(suppressWarnings(tfl_yaml_from_sheets(b, to)))
  back <- tfl_read_outputs(to)[["L-1"]]
  expect_null(back$data[["population"]])
  expect_identical(back$common$population_label, "All subjects")
})

# ---- the Codex review of 2026-11-26 -------------------------------------------

test_that("a data map with no set: the page's set line is none, it does not stop (Codex 5)", {
  st <- list(data = list(populations = list(list(id = "SAF", label = "Safety Analysis Set"))))
  # a figure of its code, no set and no `common` (SAMPLE-01's F-14-2-4 with
  # tflplanner#858, before its rows are read)
  fig <- list(kind = "figure", data = list(source = "code"))
  expect_null(.page_own_tokens(fig, st)$OUTPUT_POPULATION)
  expect_length(.data_rows(fig), 0L)
  fig$data$steps <- list(list(step = "read", dataset = "ADTTE"))
  expect_null(.page_own_tokens(fig, st)$OUTPUT_POPULATION)
  lst <- list(kind = "listing", data = list(source = "dataset", dataset = "ADAE"))
  expect_null(.page_own_tokens(lst, st)$OUTPUT_POPULATION)
  # its set on its subjects row only (F-14-2-4's form): that set
  fig$ard <- list(analysis_data = list(list(id = "adtte_saf", from = "ADTTE", population = "SAF"),
                                       list(id = "adsl_saf", from = "ADSL", population = "SAF")))
  expect_length(.data_rows(fig), 2L)
  expect_identical(.page_own_tokens(fig, st)$OUTPUT_POPULATION, "Safety Analysis Set")
})

# A study folder of ADSL and ADVS, the sets SAF and SCRF, the reports `outs`
# (and a TOC's lines `toc`); removed when the calling test ends
a2_dir <- function(outs, toc = NULL) {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(d, "spec", "outputs"), recursive = TRUE)
  writeLines(c("datasets:", "  - dataset: ADSL", "    path: data/adsl.rds",
               "  - dataset: ADVS", "    path: data/advs.rds",
               "populations:", "  - id: SAF", "    dataset: ADSL", "    where: !r SAFFL == \"Y\"",
               "  - id: SCRF", "    dataset: ADSL", "    where: !r SCRNFL == \"Y\""),
             file.path(d, "spec", "data.yml"))
  if (!is.null(toc)) writeLines(toc, file.path(d, "spec", "toc.yml"))
  for (o in outs) tfl_write_output(o, file.path(d, "spec", "outputs", paste0(o$id, ".yml")))
  d
}
# The study's sheets, and its reports written back from them into `back`
a2_back <- function(d) {
  b <- suppressWarnings(tfl_sheets_from_yaml(d))
  to <- file.path(d, "back")
  invisible(suppressWarnings(tfl_yaml_from_sheets(b, to)))
  list(sheets = b, back = tfl_read_outputs(to))
}

test_that("a figure's set written in one place comes back in that place (Codex 4)", {
  # on its data map only
  f1 <- tfl_output("F-1", "figure", content = tfl_r("df <- adsl"))
  f1$data <- list(source = "code", population = "SAF")
  # on its subjects row only (F-14-2-4's form)
  f2 <- tfl_output("F-2", "figure", content = tfl_r("df <- adsl_saf"))
  f2$data <- list(source = "code")
  f2$ard <- list(analysis_data = list(list(id = "adsl_saf", from = "ADSL", population = "SAF")),
                 analyses = list(list(id = "N", method = "categorical", data = "adsl_saf",
                                      variables = list("TRT01A"))))
  # a figure of items (its sheet keeps no `source`): on its data, not in `common`
  f3 <- tfl_output("F-3", "figure",
                   data = list(source = "code", population = "SAF",
                               steps = list(list(step = "read", dataset = "ADSL"))),
                   content = list(tfl_item("ggplot", list(data = "df",
                                                          mapping = list(x = "AGE", y = "WEIGHTBL")))))
  back <- a2_back(a2_dir(list(f1, f2, f3)))$back
  expect_identical(back[["F-1"]]$data[["population"]], "SAF")
  expect_null(back[["F-1"]]$common[["population"]])
  ad <- back[["F-2"]]$ard$analysis_data
  expect_identical(ad[[match("adsl_saf", vapply(ad, function(a) a$id, ""))]][["population"]], "SAF")
  expect_null(back[["F-2"]]$data[["population"]])
  expect_null(back[["F-2"]]$common[["population"]])
  expect_identical(back[["F-3"]]$data[["population"]], "SAF")
  expect_null(back[["F-3"]]$common[["population"]])
})

test_that("a template report keeps its set through the sheets, as its template's set argument (Codex 4, A2-T)", {
  l <- tfl_output("L-9", "listing", label = "Records", common = list(population = "SAF"),
                  template = list(name = "tflspec_v1/tflspec_listing", args = list(from = "ADVS")))
  r <- a2_back(a2_dir(list(l)))
  expect_identical(r$sheets$planner$population[match("L-9", r$sheets$planner$output_id)], "SAF")
  back <- r$back[["L-9"]]
  expect_identical(back$template$name, "tflspec_v1/tflspec_listing")
  expect_identical(back$template$args[["population"]], "SAF")
  expect_null(back$common[["population"]])
})

test_that("the sheets take the data's set over the TOC's line; the check says the TOC's (Codex 3A)", {
  f <- tfl_output("F-1", "figure", content = tfl_r("df <- adsl"))
  f$data <- list(source = "code", population = "SAF")
  toc <- c("outputs:", "  - id: F-1", "    kind: figure", "    population: SCRF")
  r <- a2_back(a2_dir(list(f), toc))
  expect_identical(r$sheets$planner$population[match("F-1", r$sheets$planner$output_id)], "SAF")
  expect_identical(r$back[["F-1"]]$data[["population"]], "SAF")
  st <- list(data = list(populations = list(list(id = "SAF"), list(id = "SCRF"))),
             toc = list(outputs = list(list(id = "F-1", kind = "figure", population = "SCRF"))))
  p <- .check_set_rows(f, "F-1", st)
  expect_identical(c(p$item, p$level), c("toc$population", "warning"))
  expect_match(p$message, "the TOC says analysis set SCRF and the report's data SAF", fixed = TRUE)
  st$toc$outputs[[1L]]$population <- "SAF"
  expect_identical(nrow(.check_set_rows(f, "F-1", st)), 0L)
})

test_that("a data of one of the report's analysis data has that data's set; a set beside it is an error (Codex 2)", {
  rows <- list(list(id = "adsl_saf", from = "ADSL", population = "SAF"),
               list(id = "ae_all", from = "ADAE", subjects_in = "adsl_saf"))
  o <- tfl_output("L-1", "listing", common = list(population = "SCRF"))
  o$ard <- list(analysis_data = rows)
  # the analysis data's set (of the subjects it keeps), not `common`'s, not the map's
  expect_identical(.data_population(list(source = "dataset", data = "ae_all"), o), "SAF")
  expect_identical(.data_population(list(source = "dataset", data = "ae_all", population = "SCRF"), o), "SAF")
  # (its subjects row with no set: the report's `common` one, as the ARD
  # program reads it until `common` goes; then none)
  o$ard$analysis_data[[1L]]$population <- NULL
  expect_identical(.data_population(list(source = "dataset", data = "ae_all"), o), "SCRF")
  o$common <- NULL
  expect_true(is.na(.data_population(list(source = "dataset", data = "ae_all"), o)))
  # the one rule of its places: the analyses', the listings', the AI's, the check's
  expect_identical(.data_both("ae_all", NULL, "SAF"), "population")
  expect_identical(.data_both("ae_all", "ADAE", NA), "dataset")
  expect_identical(.data_both(NA, "ADAE", "SAF"), character())
  st <- list(data = list(populations = list(list(id = "SAF"), list(id = "SCRF"))))
  o$data <- list(source = "dataset", data = "ae_all", population = "SAF")
  p <- .check_set_rows(o, "L-1", st)
  expect_identical(c(p$item, p$level), c("data$data", "error"))
  expect_match(p$message, "the listing reads its `data` or a `population`; not both", fixed = TRUE)
  expect_error(tfl_listing_spec(data.frame(output_id = "L-1", data = "ae_all", population_id = "SAF"),
                                data.frame(output_id = "L-1", vars = "USUBJID")),
               "a listing reads its `data` or a `population_id`; not both", fixed = TRUE)
})

test_that("a report's set that is none of data.yml's: an error (Codex 8C)", {
  st <- list(data = list(populations = list(list(id = "SAF"))))
  f <- list(kind = "figure", data = list(source = "code", population = "flag:SAFFL"))
  p <- .check_set_rows(f, "F-1", st)
  expect_identical(c(p$item, p$level), c("data$population", "error"))
  expect_match(p$message, "'flag:SAFFL' is not an analysis set of spec/data.yml (SAF)", fixed = TRUE)
  f$data$population <- "SAF"
  expect_identical(nrow(.check_set_rows(f, "F-1", st)), 0L)
  expect_null(.pop_unknown_text(NULL, "SAF"))
  expect_null(.pop_unknown_text("X", character()))
})

# T-2: the N of ADSL's SAF; SYSBP of the records of the SAF subjects aged 65
# or over (`subjects_in` a subset of the N's subjects)
a2_spec65 <- function() {
  tfl_ard_spec(list(
    datasets = data.frame(dataset = c("ADSL", "ADVS"), path = c("adsl.rds", "advs.rds")),
    populations = data.frame(population_id = "SAF", dataset = "ADSL", where = "SAFFL == \"Y\""),
    analysis_data = data.frame(output_id = "T-2",
                               data_id = c("adsl_saf", "adsl_65", "advs_65"),
                               from = c("ADSL", "adsl_saf", "ADVS"),
                               population_id = c("SAF", NA, NA),
                               subjects_in = c(NA, NA, "adsl_65"),
                               where = c(NA, "AGE >= 65", "PARAMCD == \"SYSBP\"")),
    analyses = data.frame(output_id = "T-2", analysis_id = c("BIGN", "SYSBP"),
                          method = c("categorical", "continuous"), data = c("adsl_saf", "advs_65"),
                          by = c(NA, "TRT01A"), variables = c("TRT01A", "AVAL"))))
}

test_that("the N check compares the rows the program counts: the subjects it joins them to (Codex 7A)", {
  pr <- .n_count_pairs(a2_spec65(), "T-2")
  expect_length(pr, 1L)
  pr <- pr[[1L]]
  sets <- vapply(pr$subjects, function(s) paste(c(s$dataset, s$where), collapse = " & "), "")
  expect_true("ADSL & SAFFL == \"Y\" & AGE >= 65" %in% sets)
  adsl <- data.frame(USUBJID = c("S1", "S2"), SAFFL = "Y", AGE = c(40, 70),
                     TRT01A = c("Drug X", "Control"))
  advs <- data.frame(USUBJID = c("S1", "S2"), PARAMCD = "SYSBP", TRT01A = c("Control", "Control"))
  get <- function(ds) switch(toupper(ds), ADSL = adsl, ADVS = advs, NULL)
  # S1 (40) is not counted: no row (without the subjects, S1 would be one)
  expect_identical(nrow(.n_pair_rows(pr, adsl, advs, "USUBJID", get)), 0L)
  expect_identical(nrow(.n_pair_rows(pr, adsl, advs, "USUBJID")), 1L)
  # S2 (70) counted under another arm: the row
  advs$TRT01A[2L] <- "Drug X"
  p <- .n_pair_rows(pr, adsl, advs, "USUBJID", get)
  expect_match(p$message, "(S2: Control in ADSL, Drug X in ADVS)", fixed = TRUE)
  # a condition of one value keeps all rows or none; one that cannot be run: no row
  expect_identical(nrow(.n_keep(advs, "FALSE")), 0L)
  expect_identical(nrow(.n_keep(advs, "TRUE")), 2L)
  expect_null(.n_keep(advs, "NOSUCHVAR == 1"))
  pr$where <- c(pr$where, "NOSUCHVAR == 1")
  expect_identical(nrow(.n_pair_rows(pr, adsl, advs, "USUBJID", get)), 0L)
})

test_that("the subjects' value is the N's only when they are of the N's dataset (Codex 6C)", {
  # `add: TRT01A` from ADSL's subjects and the N of ADPC's subjects: compared
  expect_length(.n_count_pairs(a2_spec(add = "TRT01A", bign_data = "adpc_subj"), "T-1"), 1L)
  # ... and the N of ADSL's: taken, nothing to compare (as before)
  expect_length(.n_count_pairs(a2_spec(add = "TRT01A"), "T-1"), 0L)
})

test_that("an N the header shows: {n} in a column header that gives no values (Q6)", {
  t1 <- list(kind = "table", content = list(tfl_item("table_plan", list(cols = "TRT01A")),
                                            tfl_item("plan_col_header", list(header = "{col}\n(N={n})"))))
  expect_true(.bign_shown(t1))
  t1$content[[2L]]$args$values <- list(n = c(10, 12))
  expect_false(.bign_shown(t1))
  expect_false(.bign_shown(list(kind = "table", content = list(tfl_item("table_plan", list(cols = "TRT01A"))))))
  expect_false(.bign_shown(list(kind = "listing", content = t1$content)))
})
