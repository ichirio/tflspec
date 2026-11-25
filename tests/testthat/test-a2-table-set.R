# A2-T T1 (design note a2-2-table-population-2026-11-26, section 6): a
# table's analysis set is its data's -- the set its subjects rows carry, a
# template report's set argument -- read by one reader (.report_set(),
# tfl_report_set()) and written by one writer (tfl_set_report_set()), never
# in `common:`.  The impact gate: a study's ARD and report programs are the
# same whether its tables write the set in `common:` (the old form, read
# until T4) or on their rows.  Fictional data only.

# A study folder: ADSL, ADAE, ADPC; the sets SAF and ENR (ADSL) and PKS
# (ADPC); the reports `outs`; removed when the calling test ends
ts_dir <- function(outs) {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(d, "spec", "outputs"), recursive = TRUE)
  writeLines(c("datasets:", "  - dataset: ADSL", "    path: data/adsl.rds",
               "  - dataset: ADAE", "    path: data/adae.rds",
               "  - dataset: ADPC", "    path: data/adpc.rds",
               "populations:",
               "  - id: SAF", "    dataset: ADSL", "    where: !r SAFFL == \"Y\"",
               "    label: Safety Analysis Set",
               "  - id: ENR", "    dataset: ADSL", "    where: !r ENRLFL == \"Y\"",
               "    label: Enrolled Analysis Set",
               "  - id: PKS", "    dataset: ADPC", "    where: !r PKFL == \"Y\"",
               "    label: PK Analysis Set"),
             file.path(d, "spec", "data.yml"))
  for (o in outs) tfl_write_output(o, file.path(d, "spec", "outputs", paste0(o$id, ".yml")))
  d
}

ts_content <- list(tfl_item("table_plan", list(cols = "TRT01A")))
ts_bign <- function(data) list(id = "BIGN", method = "categorical", data = data, variables = list("TRT01A"))

# The tables in either form: `old` writes the set in `common:` (the rows
# name none), else on the rows that make the subjects
ts_tables <- function(old = FALSE) {
  row <- function(id, from, pop, ...) {
    r <- list(id = id, from = from, ...)
    if (!old) r$population <- pop
    r
  }
  com <- function(pop) if (old) list(population = pop) else list()
  list(
    # one subjects row and the records of its subjects
    tfl_output("T-A", "table", label = "AE", common = com("SAF"), extends = "none", content = ts_content, ard = list(
      analysis_data = list(row("adsl_saf", "ADSL", "SAF"),
                           list(id = "adae_saf", from = "ADAE", subjects_in = "adsl_saf")),
      analyses = list(ts_bign("adsl_saf"),
                      list(id = "AE", method = "categorical", data = "adae_saf",
                           by = list("TRT01A"), variables = list("AEDECOD"))))),
    # T-14-1-5's form: two subjects rows of the one set
    tfl_output("T-B", "table", label = "Demographics", common = com("ENR"), extends = "none", content = ts_content, ard = list(
      analysis_data = list(row("adsl_enr", "ADSL", "ENR"), row("adsl_enr_1", "ADSL", "ENR")),
      analyses = list(ts_bign("adsl_enr"),
                      list(id = "SEX", method = "categorical", data = "adsl_enr_1",
                           by = list("TRT01A"), variables = list("SEX"))))),
    # a set defined on ADPC (.pop_from()): its subjects row reads ADPC
    tfl_output("T-C", "table", label = "PK", common = com("PKS"), extends = "none", content = ts_content, ard = list(
      analysis_data = list(row("adpc_pks", "ADPC", "PKS")),
      analyses = list(ts_bign("adpc_pks")))))
}

test_that(".report_set() / tfl_report_set(): a table's set is the set its subjects rows carry", {
  tt <- stats::setNames(ts_tables(), c("A", "B", "C"))
  expect_identical(tfl_report_set(tt$A), "SAF")
  # T-14-1-5's two ENR rows: ENR
  expect_identical(tfl_report_set(tt$B), "ENR")
  # a set on a dataset that is not ADSL
  expect_identical(tfl_report_set(tt$C), "PKS")
  # the old form: `common: population`, read last until it goes
  old <- stats::setNames(ts_tables(old = TRUE), c("A", "B", "C"))
  expect_identical(vapply(old, tfl_report_set, ""), c(A = "SAF", B = "ENR", C = "PKS"))
  # no set (T-14-0-1): none
  expect_true(is.na(tfl_report_set(tfl_output("T-0", "table", content = ts_content))))
  # a figure's and a listing's map: unchanged (A2-1)
  f <- tfl_output("F-1", "figure", content = tfl_r("df <- adsl"))
  f$data <- list(source = "code", population = "SAF")
  expect_identical(tfl_report_set(f), "SAF")
  l <- tfl_output("L-1", "listing")
  l$data <- list(source = "dataset", dataset = "ADAE", population = "ENR")
  expect_identical(tfl_report_set(l), "ENR")
})

test_that("two sets on the subjects rows: BIGN's row's; no BIGN, none and a warning (Q3)", {
  ad <- list(list(id = "adsl_saf", from = "ADSL", population = "SAF"),
             list(id = "adsl_enr", from = "ADSL", population = "ENR"))
  an <- list(list(id = "N", method = "categorical", data = "adsl_enr", variables = list("TRT01A")))
  o <- tfl_output("T-2", "table", content = ts_content, ard = list(analysis_data = ad, analyses = an))
  expect_true(is.na(tfl_report_set(o)))
  p <- .check_static_rows(o, "T-2")
  w <- p[p$item == "data$population", ]
  expect_identical(w$level, "warning")
  expect_match(w$message, "carry several analysis sets (SAF, ENR) and no BIGN analysis", fixed = TRUE)
  # BIGN of the ENR row: ENR, no warning
  o$ard$analyses <- c(list(ts_bign("adsl_enr")), an)
  expect_identical(tfl_report_set(o), "ENR")
  expect_false(any(.check_static_rows(o, "T-2")$item == "data$population"))
  # BIGN of a row made from it (its set is the nearest above)
  o$ard$analysis_data <- c(ad, list(list(id = "adsl_saf65", from = "adsl_saf", where = tfl_r("AGE >= 65"))))
  o$ard$analyses <- list(ts_bign("adsl_saf65"))
  expect_identical(tfl_report_set(o), "SAF")
})

test_that("a template report's set: its set argument, found from the template, not by a name", {
  # (a template file: the parameter its subjects row's `population: "{...}"` names)
  expect_identical(.tpl_set_param("tflspec_v1/tflspec_summary"), "population")
  expect_identical(.tpl_set_param("tflspec_v1/tflspec_listing"), "population")
  # (a registered template: the shown parameter of `choices = "populations"`)
  expect_identical(.tpl_set_param("ae_soc_pt"), "population")
  expect_identical(.tpl_set_param("demog"), "population")
  # (km_survival's is a hidden data stage: no argument the report writes)
  expect_true(is.na(.tpl_set_param("km_survival")))
  expect_identical(.tpl_set_param_of(.tpl_get("km_survival", "t"), hidden = TRUE), "population")
  expect_true(is.na(.tpl_set_param("no_such_template")))
  s <- tfl_output("T-S", "table", template = list(name = "tflspec_v1/tflspec_summary",
                                                  args = list(population = "ENR", by = "TRT01A")))
  expect_identical(tfl_report_set(s), "ENR")
  d <- tfl_output("T-D", "table", template = list(name = "demog", args = list(
    population = "SAF", by = "TRT01A", continuous = "AGE", categorical = "SEX")))
  expect_identical(tfl_report_set(d), "SAF")
  # (until `common` goes: a template report that gives none has its `common: population`)
  d$template$args$population <- NULL
  d$common <- list(population = "ENR")
  expect_identical(tfl_report_set(d), "ENR")
  # (its own row adds to the template's: not its set)
  d$common <- list()
  d$ard <- list(analysis_data = list(list(id = "adsl_itt", from = "ADSL", population = "ITT")))
  expect_true(is.na(tfl_report_set(d)))
})

test_that("tfl_set_report_set(): writes where the reader reads, once, never in common:", {
  # 3. a code SPEC's table: every subjects row of no set or the report's
  o <- ts_tables()[[2L]]
  b <- tfl_set_report_set(o, "SAF")
  expect_identical(vapply(b$ard$analysis_data, function(a) a$population, ""), c("SAF", "SAF"))
  expect_identical(tfl_report_set(b), "SAF")
  expect_identical(tfl_set_report_set(b, "SAF"), b)
  # (the old form: common goes, the rows take it)
  old <- ts_tables(old = TRUE)[[2L]]
  b2 <- tfl_set_report_set(old, "ENR")
  expect_null(b2$common$population)
  expect_identical(vapply(b2$ard$analysis_data, function(a) a$population, ""), c("ENR", "ENR"))
  # (a row of another set, a denominator, keeps its own; a records row is no subjects row)
  t1 <- ts_tables()[[1L]]
  t1$ard$analysis_data <- c(t1$ard$analysis_data, list(list(id = "adsl_itt", from = "ADSL", population = "ITT")))
  b3 <- tfl_set_report_set(t1, "ENR")
  pops <- vapply(b3$ard$analysis_data, function(a) a$population %||% NA_character_, "")
  expect_identical(unname(pops), c("ENR", NA, "ITT"))
  # NA takes it away
  b4 <- tfl_set_report_set(b, NA)
  expect_true(all(vapply(b4$ard$analysis_data, function(a) is.null(a$population), NA)))
  expect_true(is.na(tfl_report_set(b4)))
  # a set on ADPC: the study's sets say the row (.pop_from())
  st <- tfl_read_study_spec(ts_dir(list()))
  pk <- tfl_set_report_set(ts_tables(old = TRUE)[[3L]], "PKS", st)
  expect_identical(pk$ard$analysis_data[[1L]]$population, "PKS")
  # no subjects row: none written, said
  expect_warning(tfl_set_report_set(tfl_output("T-0", "table"), "SAF"), "no data row that makes its subjects")
  # 1. a figure's or listing's map: its key
  l <- tfl_output("L-1", "listing", common = list(population = "SAF"))
  l$data <- list(source = "dataset", dataset = "ADAE")
  l2 <- tfl_set_report_set(l, "ENR")
  expect_identical(l2$data$population, "ENR")
  expect_null(l2$common$population)
  expect_identical(tfl_set_report_set(l2, "ENR"), l2)
  # (a figure whose subjects row carries the set and whose map names none: the row)
  f <- tfl_output("F-2", "figure", content = tfl_r("df <- adsl_saf"))
  f$data <- list(source = "code")
  f$ard <- list(analysis_data = list(list(id = "adsl_saf", from = "ADSL", population = "SAF")))
  f2 <- tfl_set_report_set(f, "ENR")
  expect_null(f2$data$population)
  expect_identical(f2$ard$analysis_data[[1L]]$population, "ENR")
  # 2. a template report: its set argument
  d <- tfl_output("T-D", "table", common = list(population = "SAF"), template = list(name = "demog", args = list(
    by = "TRT01A", continuous = "AGE", categorical = "SEX")))
  d2 <- tfl_set_report_set(d, "ENR")
  expect_identical(d2$template$args$population, "ENR")
  expect_null(d2$common$population)
  expect_identical(tfl_report_set(d2), "ENR")
  expect_identical(tfl_set_report_set(d2, "ENR"), d2)
  expect_null(tfl_set_report_set(d2, NA)$template$args$population)
  # (in the part that gives it)
  d3 <- d
  d3$template$ard <- list(population = "SAF")
  expect_identical(tfl_set_report_set(d3, "ENR")$template$ard$population, "ENR")
  # (km_survival: its set is a hidden data stage; the data map the template reads)
  k <- tfl_output("F-K", "figure", template = list(name = "km_survival", args = list(by = "TRT01A")))
  k2 <- tfl_set_report_set(k, "SAF")
  expect_identical(k2$data$population, "SAF")
  expect_null(k2$template$args$population)
})

test_that("sheets and back: a table keeps its set on its rows, a template table its argument, no common:", {
  skip_if_not_installed("openxlsx")
  tt <- ts_tables()
  d <- tfl_output("T-D", "table", label = "Demographics", template = list(name = "demog", args = list(
    population = "ENR", by = "TRT01A", continuous = "AGE", categorical = "SEX")))
  # (the old form of a template table: its `common: population` written as its argument)
  dold <- tfl_output("T-E", "table", label = "Demographics", common = list(population = "SAF"),
                     template = list(name = "demog", args = list(by = "TRT01A", continuous = "AGE", categorical = "SEX")))
  dir <- ts_dir(c(tt, list(d, dold)))
  b <- suppressWarnings(tfl_sheets_from_yaml(dir))
  pl <- b$planner
  expect_identical(pl$population[match(c("T-A", "T-B", "T-C", "T-D", "T-E"), pl$output_id)],
                   c("SAF", "ENR", "PKS", "ENR", "SAF"))
  to <- file.path(dir, "back")
  invisible(suppressWarnings(tfl_yaml_from_sheets(b, to)))
  back <- tfl_read_outputs(to)
  for (id in c("T-A", "T-B", "T-C")) {
    expect_null(back[[id]]$common$population, info = id)
    o <- Filter(function(x) identical(x$id, id), tt)[[1L]]
    sets <- function(x) vapply(x$ard$analysis_data, function(a) a$population %||% NA_character_, "")
    expect_identical(sets(back[[id]]), sets(o), info = id)
  }
  expect_identical(back[["T-D"]]$template$args$population, "ENR")
  expect_identical(back[["T-E"]]$template$args$population, "SAF")
  expect_null(back[["T-D"]]$common$population)
  expect_null(back[["T-E"]]$common$population)
})

test_that("the sheets' planner set a table's rows do not carry: on rows of no set, else said (Q7)", {
  skip_if_not_installed("openxlsx")
  # a subjects row that names no set: the planner's set is written on it
  o <- tfl_output("T-N", "table", content = ts_content, ard = list(
    analysis_data = list(list(id = "adsl_sub", from = "ADSL")),
    analyses = list(ts_bign("adsl_sub"))))
  dir <- ts_dir(list(o))
  b <- suppressWarnings(tfl_sheets_from_yaml(dir))
  b$planner$population[b$planner$output_id == "T-N"] <- "SAF"
  to <- file.path(dir, "back")
  r <- suppressMessages(suppressWarnings(tfl_yaml_from_sheets(b, to)))
  back <- tfl_read_output(file.path(to, "spec", "outputs", "T-N.yml"))
  expect_identical(back$ard$analysis_data[[1L]]$population, "SAF")
  expect_null(back$common$population)
  # another set on its rows: the rows' kept, the planner's said
  b$ard$analysis_data$population_id[b$ard$analysis_data$output_id == "T-N"] <- "ENR"
  to2 <- file.path(dir, "back2")
  r2 <- suppressMessages(suppressWarnings(tfl_yaml_from_sheets(b, to2)))
  back2 <- tfl_read_output(file.path(to2, "spec", "outputs", "T-N.yml"))
  expect_identical(back2$ard$analysis_data[[1L]]$population, "ENR")
  nc <- r2$not_converted
  expect_true(any(grepl("the planner's analysis set SAF is not the set its subjects rows carry (ENR)",
                        nc$reason, fixed = TRUE)))
})

test_that("tfl_template_to_spec(): the code SPEC's subjects row has the set, no common:", {
  dir <- ts_dir(list())
  st <- tfl_read_study_spec(dir)
  for (o in list(
    tfl_output("T-D", "table", template = list(name = "demog", args = list(
      population = "ENR", by = "TRT01A", continuous = "AGE", categorical = "SEX"))),
    tfl_output("T-D", "table", common = list(population = "ENR"), template = list(name = "demog", args = list(
      by = "TRT01A", continuous = "AGE", categorical = "SEX"))))) {
    b <- tfl_template_to_spec(o, st)
    expect_null(b$common$population)
    expect_null(b$template)
    expect_identical(tfl_report_set(b), "ENR")
  }
})

test_that("the impact gate: the ARD and report programs are the same in both forms (old common:, new rows)", {
  old <- ts_dir(ts_tables(old = TRUE))
  new <- ts_dir(ts_tables())
  so <- tfl_read_study_spec(old)
  sn <- tfl_read_study_spec(new)
  for (id in c("T-A", "T-B", "T-C")) {
    expect_identical(tfl_ard_code(sn, id, save = TRUE), tfl_ard_code(so, id, save = TRUE), info = id)
    oo <- tfl_read_output(file.path(old, "spec", "outputs", paste0(id, ".yml")))
    on <- tfl_read_output(file.path(new, "spec", "outputs", paste0(id, ".yml")))
    expect_identical(tfl_report_code(on, study = sn), tfl_report_code(oo, study = so), info = id)
    # the page heading's set: the same label
    expect_identical(.page_own_tokens(on, sn)$OUTPUT_POPULATION,
                     .page_own_tokens(oo, so)$OUTPUT_POPULATION, info = id)
  }
  expect_identical(.page_own_tokens(tfl_read_output(file.path(new, "spec", "outputs", "T-B.yml")),
                                              sn)$OUTPUT_POPULATION, "Enrolled Analysis Set")
  # a demog table: `common: population` and its argument give the same ARD program
  dold <- ts_dir(list(tfl_output("T-D", "table", common = list(population = "SAF"), template = list(
    name = "demog", args = list(by = "TRT01A", continuous = "AGE", categorical = "SEX")))))
  dnew <- ts_dir(list(tfl_output("T-D", "table", template = list(
    name = "demog", args = list(population = "SAF", by = "TRT01A", continuous = "AGE", categorical = "SEX")))))
  expect_identical(tfl_ard_code(tfl_read_study_spec(dnew), "T-D", save = TRUE),
                   tfl_ard_code(tfl_read_study_spec(dold), "T-D", save = TRUE))
})
