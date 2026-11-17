# tfl_template_resolve(): the 7 SAMPLE-01 reports of the tflspec_v1 series
# (fictional study) as code SPECs.

tr_study <- function() {
  list(settings = list(rounding = "sas", output_path = "output/tfl"),
       series_settings = list(tflspec_v1 = list(style = "standard", max_rows = 25L)))
}

tr_report <- function(text) {
  f <- tempfile(fileext = ".yml")
  on.exit(unlink(f), add = TRUE)
  writeLines(text, f)
  tflspec:::.series_read_yaml(f)
}

tr_reports <- function() {
  list(
    demog = tr_report(c(
      "id: T-14-1-1", "kind: table", "label: Demographic characteristics",
      "template:", "  name: tflspec_v1/demog", "  analysis:", "    by: TRT01A",
      "    continuous: [AGE]", "    categorical: [AGEGR1, SEX, RACE, ETHNIC]")),
    ae = tr_report(c(
      "id: T-14-3-1", "kind: table", "label: TEAEs by SOC / PT",
      "template:", "  name: tflspec_v1/ae_soc_pt", "  analysis:", "    by: TRT01A")),
    lab = tr_report(c(
      "id: T-14-4-1", "kind: table", "label: Shift in chemistry toxicity grade",
      "data:", "  - id: adlb_sub",
      "    where: !r APHASE == \"P2\" & PARCAT1 == \"CHEMISTRY\"",
      "template:", "  name: tflspec_v1/lab_tox_shift", "  analysis:", "    by: TRT01A",
      "    conditions:", "      - {tox_dir: Low, param: Albumin (g/L)}",
      "      - {tox_dir: High, param: Creatinine (umol/L)}",
      "      - {tox_dir: High, param: Sodium (mmol/L), analysis_id: NA_HIGH2}")),
    orr = tr_report(c(
      "id: T-14-2-4", "kind: table", "label: Overall response rate by subgroup",
      "data:", "  - id: adsl_sub", "    from: ADSLX", "    population: none",
      "    where: !r TRT01AN == 1", "  - id: adrs_sub",
      "    where: !r PARAMCD == \"BORC\"",
      "template:", "  name: tflspec_v1/orr_subgroup", "  data:", "    subgroup_vars: [BRCA, AGEGR1]",
      "  analysis:", "    by: TR01AG1",
      "    conditions:",
      "      - {subgroup: BRCA, subgroup_lvl: POSITIVE, where: !r BRCA == \"POSITIVE\"}",
      "      - {subgroup: AGEGR1, subgroup_lvl: \"<65\", where: !r AGEGR1 == \"<65\"}",
      "    conf_level: 0.9")),
    listing = tr_report(c(
      "id: L-16-2-8", "kind: listing", "label: Listing of severe TEAEs",
      "data:", "  - id: adsl_sub", "    from: ADSLX", "    population: SAFX",
      "  - id: lst_sub", "    from: ADAE", "    where: !r AESEV == \"SEVERE\"",
      "    arrange: [TRT01AN, USUBJID, ASTDT]",
      "template:", "  name: tflspec_v1/listing", "  data:", "    from: ADAE",
      "  content:", "    columns: [USUBJID, AEDECOD, AESEV]")),
    km = tr_report(c(
      "id: F-14-2-3", "kind: figure", "label: KM curves",
      "data:", "  - id: adsl_sub", "    population: SAF", "  - id: adtte_sub",
      "    where: !r PARAMCD == \"TTSKIN\"",
      "template:", "  name: tflspec_v1/km_simple", "  analysis:", "    by: TRT01A",
      "    time_unit: days")),
    forest = tr_report(c(
      "id: F-14-2-4", "kind: figure", "label: Hazard ratio by subgroup",
      "data:", "  - id: adsl_sub", "    population: SAF", "  - id: adtte_sub",
      "    where: !r PARAMCD == \"TTSKIN\"", "    add: [TRT01A, SEX]",
      "template:", "  name: tflspec_v1/forest_hr", "  analysis:", "    by: TRT01A",
      "    conditions:", "      - {subgroup: SEX, subgroup_lvl: F, where: !r SEX == \"F\"}",
      "    ref: Control", "    comparison: Drug X"))
  )
}

tr_ids <- function(out) vapply(out$ard$analyses, function(a) a$id, "")

test_that("the 7 SAMPLE-01 reports resolve to code SPECs", {
  reps <- tr_reports()
  for (n in names(reps)) {
    out <- tfl_template_resolve(reps[[n]], study = tr_study())
    expect_s3_class(out, "tfl_output")
    expect_identical(out$id, reps[[n]]$id)
    expect_true("BIGN" %in% tr_ids(out), info = n)
  }
})

test_that("demog: an analysis a variable, its id the variable's name", {
  out <- tfl_template_resolve(tr_reports()$demog, study = tr_study())
  expect_identical(tr_ids(out), c("BIGN", "AGE", "AGEGR1", "SEX", "RACE", "ETHNIC"))
  an <- out$ard$analyses
  expect_identical(an[[2L]]$variables, "AGE")
  expect_identical(an[[2L]]$method, "continuous")
  expect_identical(an[[3L]]$by, "TRT01A")
  expect_identical(an[[3L]]$type, "ard")
  expect_identical(an[[1L]]$variables, "TRT01A")
  # the series' formats; the study's max_rows over the series's
  expect_identical(an[[2L]]$formats$mean, 2L)
  expect_identical(attr(out, "origins")[["max_rows"]], "study")
  expect_identical(attr(out, "origins")[["by"]], "report")
  expect_identical(attr(out, "args")$max_rows, 25L)
})

test_that("demog: analysis_ids name a variable's analysis", {
  rep <- tr_reports()$demog
  rep$template$analysis$analysis_ids <- list(AGE = "AGE_STATS")
  out <- tfl_template_resolve(rep, study = tr_study())
  expect_identical(tr_ids(out)[2:3], c("AGE_STATS", "AGEGR1"))
})

test_that("ORR: the conditions rows, the template's ALL row first", {
  out <- tfl_template_resolve(tr_reports()$orr, study = tr_study())
  orr <- out$ard$analyses[[2L]]
  expect_identical(orr$id, "ORR")
  expect_identical(orr$params, c("subgroup", "subgroup_lvl", "where"))
  cn <- orr$conditions
  expect_s3_class(cn, "data.frame")
  expect_identical(names(cn), c("subgroup", "subgroup_lvl", "where"))
  expect_identical(cn$subgroup, c("ALL", "BRCA", "AGEGR1"))
  expect_identical(cn$where[[1L]], "TRUE")
  expect_identical(cn$where[[2L]], "BRCA == \"POSITIVE\"")
  # the item's params stay as written; the template's are filled in
  expect_match(as.character(orr$derive), "subgroup = \"{subgroup}\"", fixed = TRUE)
  expect_match(as.character(orr$derive), "AVALCAT1 = dplyr::if_else", fixed = TRUE)
  expect_identical(as.character(orr$where), "{where}")
  expect_match(as.character(orr$args), "conf.level = 0.9", fixed = TRUE)
  expect_identical(orr$by, c("subgroup", "subgroup_lvl", "TR01AG1"))
  # the report's data rows merged into the template's by id
  d <- out$ard$analysis_data
  adrs <- d[[which(vapply(d, function(r) r$id, "") == "adrs_sub")]]
  expect_identical(adrs$from, "ADRS")
  expect_identical(adrs$add, c("TR01AG1", "BRCA", "AGEGR1"))
  expect_identical(as.character(adrs$where), "PARAMCD == \"BORC\"")
})

test_that("lab: a condition row a test and a direction, its id by PARAMCD and tox_dir", {
  out <- tfl_template_resolve(tr_reports()$lab, study = tr_study())
  sh <- out$ard$analyses[[2L]]
  expect_identical(sh$conditions$tox_dir, c("Low", "High", "High"))
  expect_identical(sh$conditions$param[[2L]], "Creatinine (umol/L)")
  # (a row's own id; the others the per rule's)
  expect_identical(sh$conditions$analysis_id, c(NA, NA, "NA_HIGH2"))
  expect_identical(sh$analysis_per, "PARAMCD|tox_dir")
  expect_identical(sh$levels$PARAM, "{param}")
  expect_length(sh$ards, 4L)})

test_that("a conditions row may name its own analysis_id", {
  rows <- list(list(tox_dir = "High", analysis_id = "ALT_HIGH"), list(tox_dir = "Low"))
  cn <- tflspec:::.tr_conditions(rows, "tox_dir", "SHIFT", "t")
  expect_identical(names(cn), c("tox_dir", "analysis_id"))
  expect_identical(cn$analysis_id, c("ALT_HIGH", NA))
  expect_named(tflspec:::.tr_conditions(rows[2L], "tox_dir", "SHIFT", "t"), "tox_dir")
  expect_error(tflspec:::.tr_conditions(list(list(zz = 1)), "tox_dir", "SHIFT", "t"), "zz")
})

test_that("analyses: per variable, per level, per columns, none", {
  env <- list(v = c("AGE", "SEX"))
  it <- function(...) c(list(id = "X", method = "categorical", data = "d", variables = "{v}"), list(...))
  ti <- tflspec:::.tr_item
  ids <- function(x) vapply(x, function(a) a$id, "")
  expect_identical(ids(ti(it(analyses = list(per = "variable", ids = list(AGE = "AGE1"))), env, "t")),
                   c("AGE1", "SEX"))
  lv <- ti(it(analyses = list(per = "level")), env, "t")
  expect_length(lv, 1L)
  expect_identical(lv[[1L]]$analysis_per, "level")
  pc <- ti(it(analyses = list(per = c("PARAMCD", "direction"))), env, "t")
  expect_identical(pc[[1L]]$analysis_per, "PARAMCD|direction")
  expect_null(pc[[1L]]$analyses)
  expect_identical(ids(ti(it(), env, "t")), "X")
})

test_that("a required argument with no value stops, naming it", {
  rep <- tr_reports()$demog
  rep$template$analysis$by <- NULL
  expect_error(tfl_template_resolve(rep, study = tr_study()), "needs by")
  rep <- tr_reports()$demog
  rep$template$analysis$colour <- "red"
  expect_error(tfl_template_resolve(rep, study = tr_study()), "no argument colour")
})

test_that("the fill: braces, splicing, NULL drops a key", {
  env <- list(by = c("A", "B"), n = 2L, none = NULL, flag = FALSE, m = list(x = "X1"))
  f <- tflspec:::.tr_fill
  expect_identical(f(c("{by}", "C"), env), c("A", "B", "C"))
  expect_identical(f("{{by}} {n}", env), "{by} 2")
  expect_identical(f(list(a = "{none}", b = 1), env), list(b = 1))
  expect_identical(f("{p} {n}", env, shadow = "n"), "{p} {n}")
})

test_that("tfl_series_list() and a study's series values", {
  sl <- tfl_series_list()
  expect_true("tflspec_v1" %in% sl$name)
  expect_identical(names(sl), c("name", "label", "released", "path", "source"))
  v <- tfl_series_settings(tr_study(), "tflspec_v1")
  expect_identical(v$max_rows, 25L)
  expect_identical(attr(v, "origins")[["max_rows"]], "study")
  expect_identical(attr(v, "origins")[["total"]], "series")

  d <- tempfile("study")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  writeLines(c("# a study", "settings:", "  rounding: sas", "series_settings:",
               "  tflspec_v1:", "    max_rows: 25", "other: 1"), file.path(d, "study.yml"))
  tfl_series_settings_set(d, "tflspec_v1", list(max_rows = 20L, total = TRUE))
  y <- yaml::read_yaml(file.path(d, "study.yml"))
  expect_identical(y$series_settings$tflspec_v1$max_rows, 20L)
  expect_true(y$series_settings$tflspec_v1$total)
  expect_identical(y$other, 1L)
  expect_identical(readLines(file.path(d, "study.yml"))[1L], "# a study")
  tfl_series_settings_set(d, "tflspec_v1", list(max_rows = NULL, total = NULL))
  expect_null(yaml::read_yaml(file.path(d, "study.yml"))$series_settings)
  expect_error(tfl_series_settings_set(d, "tflspec_v1", list(zz = 1)), "not a value")
})

test_that("lab: record_where is adlb_sub's where; a report row merges key by key, one with code replaces the template's", {
  rep <- tr_reports()$lab
  # (the template's rows alone: the sample report writes its own where)
  rep$data <- NULL
  out <- tfl_template_resolve(rep, study = tr_study())
  row_of <- function(o, id) {
    d <- o$ard$analysis_data
    d[[match(id, vapply(d, function(r) r$id, ""))]]
  }
  lb <- row_of(out, "adlb_sub")
  expect_s3_class(lb$where, "tfl_r")
  expect_identical(as.character(lb$where), "AVISIT == \"WORST POST-BASELINE\"")
  expect_identical(attr(out, "origins")[["record_where"]], "template")
  # record_where TRUE and a then on the template's row: key by key
  rep2 <- rep
  rep2$template$data <- list(record_where = tfl_r("TRUE"))
  rep2$data <- list(list(id = "adlb_sub",
                         then = tfl_r("dplyr::slice_max(ATOXGRH, n = 1, with_ties = FALSE, by = USUBJID)")))
  lb2 <- row_of(tfl_template_resolve(rep2, study = tr_study()), "adlb_sub")
  expect_identical(as.character(lb2$where), "TRUE")
  expect_identical(as.character(lb2$then), "dplyr::slice_max(ATOXGRH, n = 1, with_ties = FALSE, by = USUBJID)")
  expect_identical(lb2$from, "ADLB")
  expect_identical(lb2$subjects, "adsl_sub")
  expect_identical(lb2$add, "TRT01A")
  # a row with code: the template's row of that id replaced whole
  rep3 <- rep
  rep3$data <- list(list(id = "adlb_sub", from = "ADLB",
                         code = tfl_r("dplyr::filter(adlb, USUBJID %in% adsl_sub$USUBJID)")))
  lb3 <- row_of(tfl_template_resolve(rep3, study = tr_study()), "adlb_sub")
  expect_identical(as.character(lb3$code), "dplyr::filter(adlb, USUBJID %in% adsl_sub$USUBJID)")
  expect_identical(lb3$from, "ADLB")
  for (k in c("where", "subjects", "subjects_in", "add", "derive")) expect_null(lb3[[k]], info = k)
  # (the other rows as they were)
  expect_identical(row_of(tfl_template_resolve(rep3, study = tr_study()), "adsl_sub"), row_of(out, "adsl_sub"))
})

test_that("a flat args: under template: is read as the step sections are", {
  rep <- tr_reports()$demog
  a <- rep$template$analysis
  rep$template$analysis <- NULL
  rep$template$args <- a
  out <- tfl_template_resolve(rep, study = tr_study())
  expect_identical(tr_ids(out), c("BIGN", "AGE", "AGEGR1", "SEX", "RACE", "ETHNIC"))
  expect_identical(attr(out, "origins")[["by"]], "report")
  rep$template$args$nonesuch <- 1
  expect_error(tfl_template_resolve(rep, study = tr_study()), "has no argument nonesuch")
})

test_that("tflspec_summary: demog's arguments give demog's code SPEC; the analysis set is a param", {
  dm <- tr_reports()$demog
  sm <- dm
  sm$template$name <- "tflspec_v1/tflspec_summary"
  a <- tfl_template_resolve(dm, study = tr_study())
  b <- tfl_template_resolve(sm, study = tr_study())
  for (k in c("id", "kind", "label", "extends", "style", "data", "ard", "content", "page")) {
    expect_identical(b[[k]], a[[k]], info = k)
  }
  expect_identical(attr(b, "template"), "tflspec_v1/tflspec_summary")
  expect_identical(attr(b, "args")[names(attr(a, "args"))], attr(a, "args"))
  # the analysis set: a param, its default the series' population_safety
  expect_identical(attr(b, "args")$population, "SAF")
  expect_identical(attr(b, "origins")[["population"]], "series")
  row_pop <- function(out) {
    d <- out$ard$analysis_data
    d[[match("adsl_sub", vapply(d, function(r) r$id, ""))]]$population
  }
  expect_identical(row_pop(b), "SAF")
  sm$template$data <- list(population = "FAS")
  b2 <- tfl_template_resolve(sm, study = tr_study())
  expect_identical(row_pop(b2), "FAS")
  expect_identical(attr(b2, "origins")[["population"]], "report")
  # demog has no such argument
  dm$template$data <- list(population = "FAS")
  expect_error(tfl_template_resolve(dm, study = tr_study()), "has no argument population")
  # the template's own label when the report gives none
  sm$label <- NULL
  expect_identical(tfl_template_resolve(sm, study = tr_study())$label, "Summary")
})

test_that("tflspec_summary: nest, value_labels and col_header are optional steps", {
  sm <- tr_reports()$demog
  sm$template$name <- "tflspec_v1/tflspec_summary"
  fns <- function(o) vapply(o$content, function(it) it$fn, "")
  item <- function(o, fn) o$content[[match(fn, fns(o))]]$args
  # none given: no step (the content as it was before the params)
  b <- tfl_template_resolve(sm, study = tr_study())
  expect_identical(fns(b), c("table_plan", "plan_cells", "plan_paginate_rows"))
  for (p in c("nest", "value_labels", "col_header")) {
    expect_null(attr(b, "args")[[p]], info = p)
  }
  # nest: {nested variable: {parent variable: level}}, as the step's map
  sm$template$content <- list(nest = list(ETHNIC = list(RACE = "ASIAN")))
  n <- tfl_template_resolve(sm, study = tr_study())
  expect_identical(item(n, "plan_nest"), list(ETHNIC = list(RACE = "ASIAN")))
  expect_identical(fns(n), c("table_plan", "plan_nest", "plan_cells", "plan_paginate_rows"))
  # col_header: the lines
  ln <- list(list(row_label = "", .values = "{col}"), list(row_label = "Item", .values = "(N={n})"))
  sm$template$content <- list(col_header = ln)
  h <- tfl_template_resolve(sm, study = tr_study())
  expect_identical(item(h, "plan_col_header"), list(lines = ln))
  expect_identical(fns(h), c("table_plan", "plan_cells", "plan_col_header", "plan_paginate_rows"))
})

test_that("tflspec_summary: labels and value_labels are one plan_labels, a heading kept", {
  sm <- tr_reports()$demog
  sm$template$name <- "tflspec_v1/tflspec_summary"
  fns <- function(o) vapply(o$content, function(it) it$fn, "")
  lab <- function(o) o$content[[match("plan_labels", fns(o))]]$args
  # value_labels alone
  sm$template$content <- list(value_labels = list(SEX = list(F = "Female", M = "Male")))
  b <- tfl_template_resolve(sm, study = tr_study())
  expect_identical(sum(fns(b) == "plan_labels"), 1L)
  expect_identical(lab(b), list(SEX = list(F = "Female", M = "Male")))
  # both, a variable in each: one entry, its heading first, then its values;
  # a variable only in labels is a heading, only in value_labels its values
  sm$template$content <- list(
    labels = list(SEX = "Sex, n (%)", RACE = "Race"),
    value_labels = list(SEX = list(F = "Female", M = "Male"), AGEGR1 = list(`<65` = "Under 65")))
  b <- tfl_template_resolve(sm, study = tr_study())
  expect_identical(sum(fns(b) == "plan_labels"), 1L)
  expect_identical(lab(b), list(SEX = list(SEX = "Sex, n (%)", F = "Female", M = "Male"),
                                RACE = "Race", AGEGR1 = list(`<65` = "Under 65")))
  # the heading a value_labels entry gives under the variable's own name wins
  sm$template$content$value_labels$SEX <- list(SEX = "Sex", F = "Female")
  expect_identical(lab(tfl_template_resolve(sm, study = tr_study()))$SEX,
                   list(SEX = "Sex", F = "Female"))
  # labels alone, as before
  sm$template$content <- list(labels = list(SEX = "Sex, n (%)"))
  expect_identical(lab(tfl_template_resolve(sm, study = tr_study())), list(SEX = "Sex, n (%)"))
  # a value_labels entry is a map of codes
  sm$template$content <- list(value_labels = list(SEX = "Female"))
  expect_error(tfl_template_resolve(sm, study = tr_study()), "value_labels: SEX is a map")
})
