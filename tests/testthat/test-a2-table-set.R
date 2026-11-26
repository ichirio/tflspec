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
  # (its own row adds to the template's: not its set; none given: the template's default)
  d$common <- list()
  d$ard <- list(analysis_data = list(list(id = "adsl_itt", from = "ADSL", population = "ITT")))
  expect_identical(tfl_report_set(d), "SAF")
})

test_that("a template's own default set is the report's set: the reader, the page, the TOC check, the planner (M1)", {
  st <- tfl_read_study_spec(ts_dir(list()))
  dm_args <- list(by = "TRT01A", continuous = "AGE", categorical = "SEX")
  d <- tfl_output("T-9", "table", label = "Demographics", template = list(name = "demog", args = dm_args))
  # (canon 11-15 (1): the template holds the default, DM's SAF; orr's ITT)
  expect_identical(tfl_report_set(d), "SAF")
  expect_identical(tfl_report_set(d, st), "SAF")
  expect_identical(tfl_report_set(tfl_output("T-O", "table", template = list(name = "orr", args = list()))), "ITT")
  expect_identical(tfl_report_set(tfl_output("T-S", "table", template = list(
    name = "tflspec_v1/tflspec_summary", args = list(by = "TRT01A")))), "SAF")
  # the page's heading, as the report program reads it (resolved)
  ro <- tfl_resolve_output(d, st)
  expect_identical(.report_set(ro, st$dir), "SAF")
  expect_identical(.page_own_tokens(ro, st)$OUTPUT_POPULATION, "Safety Analysis Set")
  # a TOC line of another set: said
  st2 <- st
  st2$toc <- list(outputs = list(list(id = "T-9", kind = "table", population = "ENR")))
  p <- .check_set_rows(d, "T-9", st2)
  expect_true(any(p$item == "toc$population" & grepl("the TOC says analysis set ENR and the report's data SAF",
                                                     p$message, fixed = TRUE)))
  # the planner's column: the template's set, not the TOC's
  expect_identical(.planner_set(d, "ENR", .tpl_spec_of(d)), "SAF")
  # its own row over the template's subjects row: that row's set (as the ARD reads it)
  ids <- .tpl_set_row_ids("demog", "population", dm_args)
  expect_length(ids, 1L)
  d2 <- d
  d2$ard <- list(analysis_data = list(list(id = ids, from = "ADSL", population = "ENR")))
  expect_identical(tfl_report_set(d2), "ENR")
  # ... and the writer writes the argument and takes the row's set off: once (S2)
  d3 <- tfl_set_report_set(d2, "SAF")
  expect_identical(d3$template$args$population, "SAF")
  expect_null(d3$ard$analysis_data[[1L]]$population)
  expect_identical(tfl_report_set(d3), "SAF")
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
  # (the writer's rows are the reader's: a row of the set on ADPC, with no
  # study; a subjects row of its own code -- S1)
  pk2 <- suppressWarnings(tfl_set_report_set(ts_tables()[[3L]], "SAF"))
  expect_identical(pk2$ard$analysis_data[[1L]]$population, "SAF")
  cd <- tfl_output("T-K", "table", ard = list(analysis_data = list(
    list(id = "adsl_k", code = tfl_r("adsl |> dplyr::filter(SAFFL == \"Y\")"), population = "SAF"))))
  expect_identical(tfl_set_report_set(cd, "ENR")$ard$analysis_data[[1L]]$population, "ENR")
  # (two sets and no BIGN to say the report's: the writer cannot tell, it stops)
  two <- tfl_output("T-2", "table", ard = list(analysis_data = list(
    list(id = "adsl_saf", from = "ADSL", population = "SAF"), list(id = "adsl_enr", from = "ADSL", population = "ENR"))))
  expect_error(tfl_set_report_set(two, "FAS"), "carry several analysis sets (SAF, ENR) and no BIGN", fixed = TRUE)
  # (a set the study does not register: stops, canon 11-15 (2) -- S3)
  expect_error(tfl_set_report_set(b, "SAFFL", st), "'SAFFL' is not an analysis set of spec/data.yml", fixed = TRUE)
  # (the old form with a denominator row: the set as the ARD reads it -- common
  # on the unset subjects row, BIGN's -- changes; the denominator keeps its own)
  od <- ts_tables(old = TRUE)[[1L]]
  od$ard$analysis_data <- c(od$ard$analysis_data, list(list(id = "adsl_itt", from = "ADSL", population = "ITT")))
  expect_identical(tfl_report_set(od), "SAF")
  od2 <- tfl_set_report_set(od, "ENR")
  expect_identical(unname(vapply(od2$ard$analysis_data, function(a) a$population %||% NA_character_, "")),
                   c("ENR", NA, "ITT"))
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

test_that("a template argument is read and written where it sits: args:, a part, a step section (one rule)", {
  nm <- "tflspec_v1/tflspec_summary"
  # the two forms of a template file's report: flat `args:` and by step
  # (tflplanner's Add writes by step: `data:`, `analysis:`)
  flat <- tfl_output("T-F", "table", template = list(name = nm, args = list(population = "ENR", by = "TRT01A")))
  step <- tfl_output("T-B", "table", template = list(name = nm, data = list(population = "ENR"),
                                                     analysis = list(by = "TRT01A")))
  for (o in list(flat, step)) {
    expect_identical(.tpl_spec_of(o)$args[c("population", "by")], list(population = "ENR", by = "TRT01A"))
    expect_identical(tfl_report_set(o), "ENR")
    expect_identical(.tpl_set_given(o), "ENR")
  }
  # the writer: where it sits, never a second copy
  f2 <- tfl_set_report_set(flat, "FAS")
  expect_identical(f2$template$args$population, "FAS")
  expect_null(f2$template$data)
  s2 <- tfl_set_report_set(step, "FAS")
  expect_identical(s2$template$data$population, "FAS")
  expect_null(s2$template$args)
  expect_identical(s2$template$analysis, list(by = "TRT01A"))
  expect_identical(tfl_report_set(s2), "FAS")
  expect_identical(tfl_set_report_set(s2, "FAS"), s2)
  # (NA takes it away: an emptied section goes, the template's default is the set)
  s3 <- tfl_set_report_set(step, NA)
  expect_null(s3$template$data)
  expect_identical(tfl_report_set(s3), "SAF")
  # a report by step that does not give it: the section of its step (data);
  # one with no argument yet: by step too (the canon's form); a flat one: args:
  s4 <- tfl_set_report_set(s3, "ENR")
  expect_identical(s4$template$data, list(population = "ENR"))
  expect_null(s4$template$args)
  n <- tfl_set_report_set(tfl_output("T-N", "table", template = list(name = nm)), "ENR")
  expect_identical(n$template$data, list(population = "ENR"))
  f3 <- tfl_set_report_set(tfl_set_report_set(flat, NA), "ENR")
  expect_identical(f3$template$args$population, "ENR")
  expect_null(f3$template$data)
  # an argument written directly under `template:` (a template file's) stays there
  bare <- tfl_output("T-R", "table", template = list(name = nm, population = "ENR", analysis = list(by = "TRT01A")))
  expect_identical(tfl_report_set(bare), "ENR")
  b2 <- tfl_set_report_set(bare, "FAS")
  expect_identical(b2$template$population, "FAS")
  expect_null(b2$template$data)
  expect_null(b2$template$args)
  # given twice (two sections): stops, as resolving it does
  two <- tfl_output("T-2", "table", template = list(name = nm, args = list(population = "SAF"),
                                                    data = list(population = "ENR")))
  expect_error(.tpl_spec_of(two), "'population' is given twice")
  # a registered template: `args:` and its parts, as before
  dm <- tfl_output("T-D", "table", template = list(name = "demog", args = list(by = "TRT01A"),
                                                   ard = list(population = "ENR")))
  expect_identical(tfl_report_set(dm), "ENR")
  dm2 <- tfl_set_report_set(dm, "FAS")
  expect_identical(dm2$template$ard$population, "FAS")
  expect_null(dm2$template$args$population)
  # the reader of a YAML file reads the same (the step sections kept)
  f <- withr::local_tempfile(fileext = ".yml")
  tfl_write_output(step, f)
  expect_identical(tfl_report_set(tfl_read_output(f)), "ENR")
  # the resolver reads the same rule (.tr_given())
  given <- .tr_given(step$template, c("population", "by"), nm, "t")
  expect_identical(given[c("population", "by")], list(population = "ENR", by = "TRT01A"))
})

test_that("a template report its template cannot expand yet (a required argument empty) reads and writes its set", {
  # (tflplanner's Add and a card's edit: demog with no variables, km with its
  # `by` emptied -- the sheets read the set of every report, .tpl_set_given())
  d <- tfl_output("T-D", "table", template = list(name = "demog", args = list(by = "TRT01A")))
  ids <- .tpl_set_row_ids("demog", "population", list(by = "TRT01A"))
  expect_identical(as.vector(ids), character())
  expect_false(is.null(attr(ids, "reason")))
  expect_true(is.na(.tpl_set_given(d)))
  expect_identical(tfl_report_set(d), "SAF")
  expect_identical(tfl_set_report_set(d, "ENR")$template$args$population, "ENR")
  k <- tfl_output("F-K", "figure", template = list(name = "km", args = list(param = "OS")))
  expect_true(is.na(.tpl_set_given(k)))
  expect_true(is.na(tfl_report_set(k)))
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
    # the page heading's set, as the report program reads it: the set's label, both forms
    lab <- c("T-A" = "Safety Analysis Set", "T-B" = "Enrolled Analysis Set", "T-C" = "PK Analysis Set")[[id]]
    expect_identical(.page_own_tokens(tfl_resolve_output(on, sn), sn)$OUTPUT_POPULATION, lab, info = id)
    expect_identical(.page_own_tokens(tfl_resolve_output(oo, so), so)$OUTPUT_POPULATION, lab, info = id)
  }
  # a demog table: `common: population` and its argument give the same ARD program
  dold <- ts_dir(list(tfl_output("T-D", "table", common = list(population = "SAF"), template = list(
    name = "demog", args = list(by = "TRT01A", continuous = "AGE", categorical = "SEX")))))
  dnew <- ts_dir(list(tfl_output("T-D", "table", template = list(
    name = "demog", args = list(population = "SAF", by = "TRT01A", continuous = "AGE", categorical = "SEX")))))
  # (and the template's default, no set written: the same)
  ddef <- ts_dir(list(tfl_output("T-D", "table", template = list(
    name = "demog", args = list(by = "TRT01A", continuous = "AGE", categorical = "SEX")))))
  sts <- lapply(list(old = dold, new = dnew, default = ddef), tfl_read_study_spec)
  ard <- lapply(sts, function(s) tfl_ard_code(s, "T-D", save = TRUE))
  expect_identical(ard$new, ard$old)
  expect_identical(ard$default, ard$old)
  rep <- lapply(sts, function(s) tfl_report_code(s$outputs[["T-D"]] %||%
                                                   tfl_read_output(file.path(s$dir, "spec", "outputs", "T-D.yml")),
                                                 study = s))
  expect_identical(rep$new, rep$old)
  expect_identical(rep$default, rep$old)
  for (s in sts) {
    o <- tfl_read_output(file.path(s$dir, "spec", "outputs", "T-D.yml"))
    expect_identical(.page_own_tokens(tfl_resolve_output(o, s), s)$OUTPUT_POPULATION, "Safety Analysis Set")
  }
})

test_that("the sheets add no set to a table whose data names none: the TOC's line is said, the ARD program unchanged (M2)", {
  skip_if_not_installed("openxlsx")
  o <- tfl_output("T-N", "table", label = "All subjects", extends = "none", content = ts_content, ard = list(
    analysis_data = list(list(id = "adsl_all", from = "ADSL")),
    analyses = list(ts_bign("adsl_all"))))
  dir <- ts_dir(list(o))
  writeLines(c("outputs:", "  - id: T-N", "    kind: table", "    population: SAF"),
             file.path(dir, "spec", "toc.yml"))
  st <- tfl_read_study_spec(dir)
  # (tfl_check_spec() says it before)
  p <- .check_set_rows(o, "T-N", st)
  expect_true(any(p$item == "toc$population" &
                    grepl("the TOC says analysis set SAF and the report's data names none", p$message, fixed = TRUE)))
  w <- character()
  b <- withCallingHandlers(tfl_sheets_from_yaml(dir), warning = function(c) {
    w <<- c(w, conditionMessage(c))
    invokeRestart("muffleWarning")
  })
  expect_true(any(grepl("T-N: the TOC's analysis set SAF is not kept", w, fixed = TRUE)))
  expect_true(all(is.na(b$planner$population[b$planner$output_id == "T-N"])))
  to <- file.path(dir, "back")
  invisible(suppressWarnings(tfl_yaml_from_sheets(b, to)))
  back <- tfl_read_output(file.path(to, "spec", "outputs", "T-N.yml"))
  expect_null(back$ard$analysis_data[[1L]]$population)
  expect_null(back$common$population)
  # the ARD program: the same (all subjects, as written)
  file.copy(file.path(dir, "spec", "data.yml"), file.path(to, "spec", "data.yml"), overwrite = TRUE)
  expect_identical(suppressWarnings(tfl_ard_code(tfl_read_study_spec(to), "T-N", save = TRUE)),
                   suppressWarnings(tfl_ard_code(st, "T-N", save = TRUE)))
})
