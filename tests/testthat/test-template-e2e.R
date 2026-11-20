# The seven tflspec_v1 templates end to end: a report SPEC of each resolved
# (tfl_template_resolve()), written into a study made from the package's
# datasets (fictional), its ARD program made and run (tfl_ard_code(),
# tfl_build_ard()).  The checks are the analyses' ids, one BIGN per report
# and BIGN's N; not the design page's full ARD counts (DM 62, AE 276, ...):
# since 2026-11-16 AE has no arm rows and Lab an id per condition row.
# The reports whose items have condition rows (ORR, forest, Lab) go through
# the study's conditions and ards sheets, and back to YAML.

e2e_rd <- function(text) {
  f <- tempfile(fileext = ".yml")
  on.exit(unlink(f))
  writeLines(text, f)
  .series_read_yaml(f)
}

e2e_settings <- list(settings = list(rounding = "sas"))

e2e_reports <- function() {
  list(
    demog = e2e_rd(c(
      "id: T-1", "kind: table", "label: Demographics",
      "template:", "  name: tflspec_v1/demog",
      "  analysis:", "    by: TRT01A", "    continuous: [AGE]", "    categorical: [AGEGR1, SEX]")),
    ae = e2e_rd(c(
      "id: T-2", "kind: table", "label: Adverse events",
      "template:", "  name: tflspec_v1/ae_soc_pt", "  analysis:", "    by: TRT01A")),
    orr = e2e_rd(c(
      "id: T-3", "kind: table", "label: Response",
      "data:", "  - id: adsl_sub", "    population: ITT",
      "  - id: adrs_sub", "    where: !r PARAMCD == \"BOR\"",
      "template:", "  name: tflspec_v1/orr_subgroup", "  data:", "    subgroup_vars: [SEX]",
      "  analysis:", "    by: TRT01A", "    category: AVALC", "    responder: CR",
      "    conditions:", "      - {subgroup: SEX, subgroup_lvl: F, where: !r SEX == \"F\"}")),
    lab = e2e_rd(c(
      "id: T-4", "kind: table", "label: Lab shift",
      "template:", "  name: tflspec_v1/lab_tox_shift", "  analysis:", "    by: TRT01A",
      "    conditions:", "      - {tox_dir: High, param: Alanine Aminotransferase (U/L)}",
      "      - {tox_dir: Low, param: Hemoglobin (g/L)}")),
    listing = e2e_rd(c(
      "id: L-1", "kind: listing", "label: Severe adverse events",
      "data:", "  - id: lst_sub", "    where: !r AESEV == \"SEVERE\"",
      "template:", "  name: tflspec_v1/listing", "  data:", "    from: ADAE")),
    km = e2e_rd(c(
      "id: F-1", "kind: figure", "label: Kaplan-Meier",
      "data:", "  - id: adsl_sub", "    population: ITT",
      "  - id: adtte_sub", "    where: !r PARAMCD == \"TTSKIN\"",
      "template:", "  name: tflspec_v1/km_simple",
      "  analysis:", "    by: TRT01A", "    time_unit: days")),
    forest = e2e_rd(c(
      "id: F-2", "kind: figure", "label: Forest",
      "data:", "  - id: adsl_sub", "    population: ITT",
      "  - id: adtte_sub", "    where: !r PARAMCD == \"TTSKIN\"",
      "template:", "  name: tflspec_v1/forest_hr", "  data:", "    subgroup_vars: [SEX]",
      "  analysis:", "    by: TRT01A",
      "    conditions:", "      - {subgroup: SEX, subgroup_lvl: F, where: !r SEX == \"F\"}"))
  )
}

# the resolved report written into the study `d`, its program and its ARD
e2e_run <- function(out, d) {
  f <- file.path(d, "spec", "outputs", paste0(out$id, ".yml"))
  tfl_write_output(out, f)
  on.exit(unlink(f))
  st <- tfl_read_study_spec(d)
  code <- tfl_ard_code(st, output_id = out$id, save = FALSE)
  ard <- suppressMessages(tfl_build_ard(st, dir = d, output_id = out$id, save = FALSE))
  list(code = code, ard = ard)
}

e2e_ids <- function(out) vapply(out$ard$analyses, function(a) a$id, "")

# BIGN once in the program; its n per arm the arms' subjects
e2e_check_bign <- function(r, pop) {
  expect_identical(sum(grepl("tag_ard(report_id, \"BIGN\"", r$code, fixed = TRUE)), 1L)
  b <- r$ard[r$ard$analysis_id == "BIGN" & r$ard$stat_name == "n", ]
  got <- stats::setNames(as.integer(unlist(b$stat)), as.character(unlist(b$variable_level)))
  # (a blank arm is missing, not an arm: ITT has two such subjects)
  want <- table(pop$TRT01A[!is.na(pop$TRT01A) & nzchar(pop$TRT01A)])
  expect_identical(got[names(want)], stats::setNames(as.integer(want), names(want)))
}

# The worst grade's Total column of a Lab analysis (by the arm, PARAM and
# tox_dir: three groups), summed: the records counted
e2e_worst_total <- function(ard, id) {
  r <- ard[ard$analysis_id == id & ard$variable == "worst" & ard$stat_name == "n", , drop = FALSE]
  g <- grep("^group[0-9]+$", names(r), value = TRUE)
  has <- matrix(vapply(g, function(cn) {
    v <- r[[cn]]
    if (is.list(v)) vapply(v, function(x) !is.null(x) && length(x) && !is.na(x[[1L]]), NA) else !is.na(v)
  }, logical(nrow(r))), nrow = nrow(r))
  sum(as.integer(unlist(r$stat[rowSums(has) == 3L])))
}

# The safety subjects (with an arm) that have a worst record of the test
e2e_worst_subjects <- function(adlb, saf, paramcd) {
  s <- saf$USUBJID[!is.na(saf$TRT01A) & nzchar(saf$TRT01A)]
  w <- adlb[adlb$AVISIT == "WORST POST-BASELINE" & adlb$PARAMCD == paramcd & adlb$USUBJID %in% s, ]
  length(unique(w$USUBJID))
}

# An ADaM with no worst record: record_where TRUE, and the template's
# adlb_sub row given a then that picks each subject's worst record
e2e_lab_then <- function() {
  e2e_rd(c(
    "id: T-5", "kind: table", "label: Lab shift, the worst record by then",
    "data:", "  - id: adlb_sub",
    paste0("    then: !r 'dplyr::filter(AVISIT != \"WORST POST-BASELINE\", !ABLFL %in% \"Y\") |> ",
           "dplyr::slice_max(ATOXGRH, n = 1, with_ties = FALSE, by = c(USUBJID, PARAMCD))'"),
    "template:", "  name: tflspec_v1/lab_tox_shift", "  data:", "    record_where: !r TRUE",
    "  analysis:", "    by: TRT01A",
    "    conditions:", "      - {tox_dir: High, param: Alanine Aminotransferase (U/L)}"))
}

skip_e2e <- function() {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("cardx")
}

test_that("the seven templates resolve, their ids as decided", {
  reps <- lapply(e2e_reports(), tfl_template_resolve, study = e2e_settings)
  expect_identical(e2e_ids(reps$demog), c("BIGN", "AGE", "AGEGR1", "SEX"))
  # AE: one item, an analysis a depth (AE_ANY, AE_L1, AE_L2 in the program)
  expect_identical(e2e_ids(reps$ae), c("BIGN", "AE"))
  expect_identical(reps$ae$ard$analyses[[2L]]$analysis_per, "level")
  # ORR: one id, the ALL row first
  expect_identical(e2e_ids(reps$orr), c("BIGN", "ORR"))
  orr <- reps$orr$ard$analyses[[2L]]
  expect_identical(orr$conditions$subgroup, c("ALL", "SEX"))
  # Lab: an id per condition row, <PARAMCD>_<direction> (analysis_per)
  lab <- reps$lab$ard$analyses
  expect_identical(e2e_ids(reps$lab)[1L], "BIGN")
  shift <- lab[[2L]]
  expect_identical(shift$conditions$tox_dir, c("High", "Low"))
  expect_true(all(c("tox_dir", "param") %in% shift$params))
  # forest: the ALL row first; KM: BIGN only (the figure makes its own fit)
  expect_identical(e2e_ids(reps$forest), c("BIGN", "HR"))
  expect_identical(reps$forest$ard$analyses[[2L]]$conditions$subgroup, c("ALL", "SEX"))
  expect_identical(e2e_ids(reps$km), "BIGN")
  expect_identical(e2e_ids(reps$listing), "BIGN")
  # one BIGN in each
  for (o in reps) expect_identical(sum(e2e_ids(o) == "BIGN"), 1L)
})

test_that("demog, AE, listing and KM: the program runs on the package's data, BIGN the arms' N", {
  skip_e2e()
  d <- file.path(tempdir(), "tpl-e2e")
  unlink(d, recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE))
  data <- tplfig_study(d, c("adsl", "adae", "adrs", "adtte"), id = "E2E-1")
  saf <- data$adsl[data$adsl$SAFFL == "Y", ]
  itt <- data$adsl[data$adsl$ITTFL == "Y", ]
  reps <- lapply(e2e_reports(), tfl_template_resolve, study = e2e_settings)

  dm <- e2e_run(reps$demog, d)
  expect_identical(unique(dm$ard$analysis_id), c("BIGN", "AGE", "AGEGR1", "SEX"))
  # DM: an analysis a variable
  for (v in c("AGE", "AGEGR1", "SEX")) {
    expect_identical(unique(dm$ard$variable[dm$ard$analysis_id == v]), v)
  }
  e2e_check_bign(dm, saf)

  ae <- e2e_run(reps$ae, d)
  expect_identical(unique(ae$ard$analysis_id), c("BIGN", "AE_ANY", "AE_L1", "AE_L2"))
  # no arm-count rows in the AE items (the % denominator is BIGN)
  expect_false(any(ae$ard$variable[ae$ard$analysis_id != "BIGN"] == "TRT01A"))
  e2e_check_bign(ae, saf)

  km <- e2e_run(reps$km, d)
  expect_identical(unique(km$ard$analysis_id), "BIGN")
  e2e_check_bign(km, itt)

  # the listing: BIGN the population's total
  ls <- e2e_run(reps$listing, d)
  expect_identical(unique(ls$ard$analysis_id), "BIGN")
  expect_identical(sum(grepl("tag_ard(report_id, \"BIGN\"", ls$code, fixed = TRUE)), 1L)
  expect_equal(as.numeric(unlist(ls$ard$stat[ls$ard$stat_name == "N"])), nrow(saf))
})

test_that("forest: the reference arm left empty is the first level", {
  out <- tfl_template_resolve(e2e_reports()$forest, study = e2e_settings)
  st <- Filter(function(x) identical(x$id, "adtte_sub"), out$data)
  der <- unlist(lapply(st, function(x) as.character(unlist(x$derive))))
  expect_length(der, 1L)
  expect_identical(trimws(sub("=.*$", "", der)), "TRT01A")
  rhs <- rlang::parse_expr(sub("^[^=]*=", "", der))
  x <- data.frame(TRT01A = c("B", "A", "B"))
  expect_identical(levels(eval(rhs, x)), c("A", "B"))
})

# an ARD's rows of one analysis: the levels of a group column, in order
e2e_levels <- function(ard, id, col) {
  v <- ard[[col]][ard$analysis_id == id]
  unique(vapply(v, function(x) if (is.null(x) || !length(x)) NA_character_ else as.character(x[[1L]]), ""))
}

test_that("ORR, forest and Lab: condition rows through the sheets, the program runs, the ids as decided", {
  skip_e2e()
  skip_if_not_installed("purrr")
  skip_if_not_installed("survival")
  skip_if_not_installed("ggsurvfit")
  d <- file.path(tempdir(), "tpl-e2e-cond")
  unlink(d, recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE))
  data <- tplfig_study(d, c("adsl", "adrs", "adtte", "adlb"), id = "E2E-2")
  saf <- data$adsl[data$adsl$SAFFL == "Y", ]
  itt <- data$adsl[data$adsl$ITTFL == "Y", ]
  reps <- lapply(e2e_reports(), tfl_template_resolve, study = e2e_settings)

  # ORR: one id, ORR, its rows the ALL row's first
  orr <- e2e_run(reps$orr, d)
  expect_identical(unique(orr$ard$analysis_id), c("BIGN", "ORR"))
  expect_identical(e2e_levels(orr$ard, "ORR", "group1_level"), c("ALL", "SEX"))
  expect_true(any(orr$ard$stat_name[orr$ard$analysis_id == "ORR"] == "estimate"))
  e2e_check_bign(orr, itt)

  # forest: HR, a hazard ratio a condition row (ALL first)
  fo <- e2e_run(reps$forest, d)
  expect_identical(unique(fo$ard$analysis_id), c("BIGN", "HR"))
  expect_identical(e2e_levels(fo$ard, "HR", "group1_level"), c("ALL", "SEX"))
  expect_identical(e2e_levels(fo$ard, "HR", "group2_level"), c("ALL", "F"))
  e2e_check_bign(fo, itt)

  # Lab: an id a condition row, <PARAMCD>_<direction>; its four calls in each
  lab <- e2e_run(reps$lab, d)
  expect_identical(unique(lab$ard$analysis_id), c("BIGN", "ALT_HIGH", "HGB_LOW"))
  for (id in c("ALT_HIGH", "HGB_LOW")) {
    expect_setequal(unique(lab$ard$variable[lab$ard$analysis_id == id]), c("worst", "base", "tox_dir"))
  }
  expect_identical(sum(grepl("<- bind_items(.ard_1, .ard_2, .ard_3, .ard_4)", lab$code, fixed = TRUE)), 1L)
  e2e_check_bign(lab, saf)
  # the worst record (record_where): one record a subject, test and direction
  expect_true(any(grepl("filter(AVISIT == \"WORST POST-BASELINE\")", lab$code, fixed = TRUE)))
  for (k in list(c("ALT_HIGH", "ALT"), c("HGB_LOW", "HGB"))) {
    expect_identical(e2e_worst_total(lab$ard, k[[1L]]), e2e_worst_subjects(data$adlb, saf, k[[2L]]),
                     info = k[[1L]])
  }
})

test_that("conditions and ards: YAML -> sheets -> YAML, one to one; the workbook keeps their columns", {
  skip_e2e()
  d <- file.path(tempdir(), "tpl-e2e-sheets")
  out <- file.path(tempdir(), "tpl-e2e-sheets-back")
  unlink(c(d, out), recursive = TRUE)
  on.exit(unlink(c(d, out), recursive = TRUE))
  tplfig_study(d, c("adsl", "adrs", "adlb"), id = "E2E-3")
  reps <- lapply(e2e_reports()[c("orr", "lab")], tfl_template_resolve, study = e2e_settings)
  for (o in reps) tfl_write_output(o, file.path(d, "spec", "outputs", paste0(o$id, ".yml")))
  sh <- tfl_sheets_from_yaml(d)
  cd <- sh$ard$conditions
  expect_identical(names(cd)[1:3], c("output_id", "analysis_id", "row"))
  orr <- cd[cd$analysis_id == "ORR", ]
  expect_identical(orr$row, c("1", "2"))
  expect_identical(orr$subgroup, c("ALL", "SEX"))
  expect_identical(orr$where, c("TRUE", "SEX == \"F\""))
  # (a column of another item's param: blank here)
  expect_true(all(is.na(orr$tox_dir)))
  lab <- cd[cd$analysis_id == "SHIFT", ]
  expect_identical(lab$param, c("Alanine Aminotransferase (U/L)", "Hemoglobin (g/L)"))
  ar <- sh$ard$ards
  expect_identical(ar$by[ar$analysis_id == "SHIFT"],
                   c("TRT01A | PARAM | tox_dir | base", "TRT01A | PARAM | tox_dir",
                     "TRT01A | PARAM | tox_dir", "TRT01A | PARAM"))
  an <- sh$ard$analyses
  expect_identical(an$levels[an$analysis_id == "SHIFT"],
                   "PARAM = \"{param}\" | tox_dir = \"{tox_dir}\"")

  suppressMessages(tfl_yaml_from_sheets(sh, out))
  back <- tfl_sheets_from_yaml(out)
  expect_identical(back$ard$conditions, sh$ard$conditions)
  expect_identical(back$ard$ards, sh$ard$ards)
  expect_identical(back$ard$analyses$levels, an$levels)
  # the YAML as it was: a list of maps, `where` R; the levels a map
  y <- tfl_read_output(file.path(out, "spec", "outputs", "T-3.yml"))
  it <- Filter(function(a) identical(a$id, "ORR"), y$ard$analyses)[[1L]]
  expect_identical(it$conditions[[2L]]$subgroup_lvl, "F")
  expect_s3_class(it$conditions[[2L]]$where, "tfl_r")
  y4 <- tfl_read_output(file.path(out, "spec", "outputs", "T-4.yml"))
  sh4 <- Filter(function(a) identical(a$id, "SHIFT"), y4$ard$analyses)[[1L]]
  expect_identical(sh4$levels, list(PARAM = "{param}", tox_dir = "{tox_dir}"))
  expect_identical(unlist(sh4$ards[[4L]]$by), c("TRT01A", "PARAM"))

  # the workbook: the sheets' own columns kept
  xl <- tempfile(fileext = ".xlsx")
  on.exit(unlink(xl), add = TRUE)
  tfl_write_ard_spec(sh$ard, xl)
  rb <- tfl_read_ard_spec(xl, check = FALSE)
  expect_true(all(c("subgroup", "subgroup_lvl", "where", "tox_dir", "param") %in% names(rb$conditions)))
  expect_identical(rb$conditions$where[rb$conditions$analysis_id == "ORR"], orr$where)
  expect_identical(rb$ards$variables, sh$ard$ards$variables)
})

test_that("a condition row's key that is not one of the item's params stops, naming it", {
  cs <- data.frame(output_id = "T", analysis_id = "A", row = c("2", "1"),
                   tox_dir = c("Low", "High"), param = c("HGB", NA), sexv = c(NA, "F"),
                   row_analysis_id = c(NA, "MINE"), stringsAsFactors = FALSE)
  expect_error(.item_cond_rows(cs, "T", "A", c("tox_dir", "param")), "sexv is not one of its params")
  got <- .item_cond_rows(cs[, -6], "T", "A", c("tox_dir", "param", "where"))
  # in row order; a param a row does not give NA; the row's own id
  expect_identical(got$tox_dir, c("High", "Low"))
  expect_identical(got$param, c(NA, "HGB"))
  expect_identical(got$where, c(NA_character_, NA_character_))
  expect_identical(got$analysis_id, c("MINE", NA))
  expect_null(.item_cond_rows(cs, "T", "B", "tox_dir"))
})

# ---- the planner's path (tflplanner#704) -------------------------------------
# The report as the app keeps it (`template:` and its arguments), read from
# its file; the code SPEC tfl_template_to_spec() makes of it; the study's
# sheets of that (tfl_sheets_from_spec()); the ARD definition read from the
# sheets, its program and its ARD.  (The arguments are written in their step
# blocks here; the flat `args:` is read by tfl_template_resolve() too.)

e2e_planner_reports <- function() {
  list(
    demog = c("id: T-11", "kind: table", "label: Demographics", "template:", "  name: tflspec_v1/demog",
              "  analysis:", "    by: TRT01A", "    continuous: [AGE]", "    categorical: [SEX, RACE]",
              "    analysis_ids: {AGE: AGE_STATS}"),
    ae = c("id: T-12", "kind: table", "label: Adverse events", "template:", "  name: tflspec_v1/ae_soc_pt",
           "  analysis:", "    by: TRT01A", "    min_pct: 5"),
    # (the category left at its default, AVALCAT1)
    orr = c("id: T-13", "kind: table", "label: Response",
            "data:", "  - id: adsl_sub", "    population: ITT",
            "template:", "  name: tflspec_v1/orr_subgroup", "  data:", "    subgroup_vars: [SEX]",
            "  analysis:", "    by: TRT01A", "    conf_level: 0.9",
            "    conditions:", "      - {subgroup: SEX, subgroup_lvl: F, where: !r SEX == \"F\"}"),
    lab = c("id: T-14", "kind: table", "label: Lab shift", "template:", "  name: tflspec_v1/lab_tox_shift",
            "  analysis:", "    by: TRT01A", "    formats: {n: 0, p: xx.xx%}",
            "    conditions:", "      - {tox_dir: High, param: Alanine Aminotransferase (U/L)}",
            "      - {tox_dir: Low, param: Hemoglobin (g/L)}"),
    km = c("id: F-11", "kind: figure", "label: Kaplan-Meier", "data:", "  - id: adsl_sub", "    population: ITT",
           "template:", "  name: tflspec_v1/km_simple", "  data:", "    time_unit: days",
           "  analysis:", "    by: TRT01A"),
    forest = c("id: F-12", "kind: figure", "label: Forest", "data:", "  - id: adsl_sub", "    population: ITT",
               "template:", "  name: tflspec_v1/forest_hr", "  data:", "    subgroup_vars: [SEX]", "    ref: Control",
               "  analysis:", "    by: TRT01A", "    comparison: Drug X",
               "    conditions:", "      - {subgroup: SEX, subgroup_lvl: F, where: !r SEX == \"F\"}"))
}

# One report through the planner's path in the study `d`: its code SPEC, the
# sheets (and their warnings), the ARD program and, with `run`, the ARD
e2e_planner <- function(lines, d, run = TRUE) {
  f <- file.path(d, "spec", "outputs", paste0(sub("^id: ", "", lines[1L]), ".yml"))
  writeLines(lines, f)
  on.exit(unlink(f))
  invisible(tfl_copy_templates(d))
  st <- tfl_read_study_spec(d)
  spec <- tfl_template_to_spec(tfl_read_output(f), st)
  warns <- character()
  sh <- withCallingHandlers(tfl_sheets_from_spec(st, stats::setNames(list(spec), spec$id)),
                            warning = function(w) {
                              warns <<- c(warns, conditionMessage(w))
                              invokeRestart("muffleWarning")
                            })
  code <- suppressWarnings(tfl_ard_code(tfl_ard_spec(sh$ard), output_id = spec$id, save = FALSE, dir = d))
  ard <- NULL
  if (run) {
    env <- new.env(parent = globalenv())
    old <- setwd(d)
    on.exit(setwd(old), add = TRUE)
    suppressMessages(eval(parse(text = code, keep.source = FALSE), envir = env))
    ard <- env$ard
  }
  list(spec = spec, sheets = sh, warnings = warns, code = code, ard = ard)
}

test_that("planner: a series report becomes its code SPEC, the sheets keep its conditions and ids, no warning", {
  skip_e2e()
  d <- file.path(tempdir(), "tpl-e2e-planner")
  unlink(d, recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE))
  tplfig_study(d, c("adsl", "adae", "adrs", "adtte", "adlb"), id = "E2E-4")
  reps <- e2e_planner_reports()

  # demog: analysis_ids name the analyses on the analyses sheet; nothing lost
  dm <- e2e_planner(reps$demog, d, run = FALSE)
  expect_null(dm$spec$template)
  expect_identical(dm$warnings, character())
  an <- dm$sheets$ard$analyses
  expect_identical(an$analysis_id[an$output_id == "T-11"], c("BIGN", "AGE_STATS", "SEX", "RACE"))

  # ORR, Lab, forest: the condition rows on the conditions sheet, no
  # "cannot hold" warning
  for (k in c("orr", "lab", "forest")) {
    r <- e2e_planner(reps[[k]], d, run = FALSE)
    expect_identical(r$warnings, character(), info = k)
    cs <- r$sheets$ard$conditions
    expect_gt(sum(cs$output_id == r$spec$id), 1L)
  }
  lab <- e2e_planner(reps$lab, d, run = FALSE)
  cs <- lab$sheets$ard$conditions
  expect_identical(cs$param[cs$output_id == "T-14"], c("Alanine Aminotransferase (U/L)", "Hemoglobin (g/L)"))
  # the derive's if / else as R in the program
  expect_true(any(grepl("worst = if (tox_dir == \"Low\") ATOXGRL else ATOXGRH", lab$code, fixed = TRUE)))
  expect_silent(parse(text = lab$code, keep.source = FALSE))
})

test_that("planner: the ARD of each series report from its sheets, the ids as decided", {
  skip_e2e()
  skip_if_not_installed("purrr")
  skip_if_not_installed("survival")
  skip_if_not_installed("ggsurvfit")
  d <- file.path(tempdir(), "tpl-e2e-planner-run")
  unlink(d, recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE))
  tplfig_study(d, c("adsl", "adae", "adrs", "adtte", "adlb"), id = "E2E-5")
  reps <- e2e_planner_reports()

  dm <- e2e_planner(reps$demog, d)
  expect_identical(unique(dm$ard$analysis_id), c("BIGN", "AGE_STATS", "SEX", "RACE"))

  # AE: the analysis_per column names the analyses a level
  ae <- e2e_planner(reps$ae, d)
  an <- ae$sheets$ard$analyses
  expect_identical(an$analysis_per[an$output_id == "T-12" & an$analysis_id == "AE"], "level")
  expect_identical(unique(ae$ard$analysis_id), c("BIGN", "AE_ANY", "AE_L1", "AE_L2"))

  # ORR with the default category: AVALCAT1 is in the data
  orr <- e2e_planner(reps$orr, d)
  expect_identical(unique(orr$ard$analysis_id), c("BIGN", "ORR"))
  expect_identical(e2e_levels(orr$ard, "ORR", "group1_level"), c("ALL", "SEX"))
  expect_true(any(orr$ard$stat_name[orr$ard$analysis_id == "ORR"] == "estimate"))
  expect_true(any(grepl("conf.level = 0.9", orr$code, fixed = TRUE)))

  lab <- e2e_planner(reps$lab, d)
  expect_identical(unique(lab$ard$analysis_id), c("BIGN", "ALT_HIGH", "HGB_LOW"))

  # forest: the reference arm's relevel in the program; HR a condition row
  fo <- e2e_planner(reps$forest, d)
  expect_true(any(grepl("intersect(\"Control\", TRT01A)", fo$code, fixed = TRUE)))
  expect_identical(unique(fo$ard$analysis_id), c("BIGN", "HR"))
  expect_identical(e2e_levels(fo$ard, "HR", "group1_level"), c("ALL", "SEX"))

  # KM: BIGN; the time unit's derive kept on the analysis_data sheet (the
  # ARD reads adsl_sub only, so its program has no adtte_sub)
  km <- e2e_planner(reps$km, d)
  expect_identical(unique(km$ard$analysis_id), "BIGN")
  ad <- km$sheets$ard$analysis_data
  der <- ad$derive[ad$output_id == "F-11" & ad$data_id == "adtte_sub"]
  expect_identical(der, "AVAL = AVAL / c(days = 1, months = 30.4375)[[\"days\"]]")
})

test_that("a step's if / else is laid out as R, not as a call", {
  s <- paste0("dplyr::mutate(worst = if (\"High\" == \"Low\") ATOXGRL else ATOXGRH, ",
              "base = if (\"High\" == \"Low\") BTOXGRL else BTOXGRH, tox_dir = \"High\")")
  code <- .lay(.step_cl(s))
  expect_false(grepl("if(", code, fixed = TRUE))
  e <- str2lang(code)
  expect_identical(e[[2L]], quote(if ("High" == "Low") ATOXGRL else ATOXGRH))
})

test_that("the sample ADRS has AVALCAT1 (orr_subgroup's default category): CR / PR a response", {
  a <- tflspec::adrs
  expect_true("AVALCAT1" %in% names(a))
  expect_identical(a$AVALCAT1 == "Response", a$AVALC %in% c("CR", "PR"))
  expect_setequal(unique(a$AVALCAT1), c("Response", "Non-response"))
})

test_that("Lab with no worst record in the ADaM: record_where TRUE and a then on adlb_sub give the same ARD", {
  skip_e2e()
  skip_if_not_installed("dplyr", "1.1.0")
  d <- file.path(tempdir(), "tpl-e2e-then")
  unlink(d, recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE))
  data <- tplfig_study(d, c("adsl", "adlb"), id = "E2E-6")
  saf <- data$adsl[data$adsl$SAFFL == "Y", ]
  rep <- tfl_template_resolve(e2e_lab_then(), study = e2e_settings)
  ad <- rep$ard$analysis_data
  lb <- ad[[match("adlb_sub", vapply(ad, function(r) r$id, ""))]]
  # the template's row with the report's then over it, key by key
  expect_identical(as.character(lb$where), "TRUE")
  expect_identical(lb$subjects, "adsl_sub")
  expect_match(as.character(lb$then), "slice_max(ATOXGRH", fixed = TRUE)
  th <- e2e_run(rep, d)
  # the then last on the row's pipe, as written, a step each
  i <- grep("^adlb_sub <- ", th$code)
  expect_length(i, 1L)
  expect_match(th$code[i], "filter(TRUE)", fixed = TRUE)
  last <- str2lang(th$code[i])[[3L]]
  expect_true(deparse(last[[1L]]) %in% c("slice_max", "dplyr::slice_max"))
  expect_true(deparse(last[[2L]][[1L]]) %in% c("filter", "dplyr::filter"))
  expect_identical(e2e_worst_total(th$ard, "ALT_HIGH"), e2e_worst_subjects(data$adlb, saf, "ALT"))
  # the same counts as the ADaM's own worst record (record_where's default)
  lab <- e2e_run(tfl_template_resolve(e2e_reports()$lab, study = e2e_settings), d)
  key <- function(a) {
    a <- a[a$analysis_id == "ALT_HIGH", , drop = FALSE]
    cols <- c(grep("^group[0-9]+(_level)?$", names(a), value = TRUE), "variable", "variable_level",
              "stat_name", "stat")
    sort(do.call(paste, lapply(a[cols], function(v) if (is.list(v)) vapply(v, function(x)
      if (is.null(x) || !length(x)) "NA" else paste(format(x), collapse = ","), "") else as.character(v))))
  }
  expect_identical(key(th$ard), key(lab$ard))

  # then through the sheets and back: the analysis_data sheet's column, !r
  f <- file.path(d, "spec", "outputs", "T-5.yml")
  tfl_write_output(rep, f)
  sh <- tfl_sheets_from_yaml(d)
  ad5 <- sh$ard$analysis_data[sh$ard$analysis_data$output_id == "T-5", ]
  expect_match(ad5$then[ad5$data_id == "adlb_sub"], "slice_max(ATOXGRH", fixed = TRUE)
  out <- file.path(tempdir(), "tpl-e2e-then-back")
  unlink(out, recursive = TRUE)
  on.exit(unlink(out, recursive = TRUE), add = TRUE)
  suppressMessages(tfl_yaml_from_sheets(sh, out))
  y <- tfl_read_output(file.path(out, "spec", "outputs", "T-5.yml"))
  lb_back <- y$ard$analysis_data[[match("adlb_sub", vapply(y$ard$analysis_data, function(r) r$id, ""))]]
  expect_s3_class(lb_back$then, "tfl_r")
  expect_identical(as.character(lb_back$then), as.character(lb$then))
})

test_that("a series report resolves as a code SPEC first: no lookup in the old registry (the report program)", {
  skip_e2e()
  d <- file.path(tempdir(), "tpl-e2e-resolve")
  unlink(d, recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE))
  tplfig_study(d, c("adsl", "adae", "adlb"), id = "E2E-7")
  reps <- e2e_planner_reports()
  for (k in c("ae", "lab")) {
    lines <- reps[[k]]
    f <- file.path(d, "spec", "outputs", paste0(sub("^id: ", "", lines[1L]), ".yml"))
    writeLines(lines, f)
    invisible(tfl_copy_templates(d))
    st <- tfl_read_study_spec(d)
    o <- tfl_read_output(f)
    ro <- tfl_resolve_output(o, st)
    expect_null(ro$template, info = k)
    expect_null(attr(ro, "template"), info = k)
    expect_true("BIGN" %in% vapply(ro$ard$analyses, function(a) a$id, ""), info = k)
    # the page and style (the report program's) are read through the same layers
    expect_error(tflspec:::.report_style(o, st), NA)
    unlink(f)
  }
  # Lab through the planner's path: both condition rows reach the sheets and
  # the program, each with its four calls (variables given)
  lab <- e2e_planner(reps$lab, d, run = FALSE)
  cs <- lab$sheets$ard$conditions
  expect_identical(cs$param[cs$output_id == "T-14"], c("Alanine Aminotransferase (U/L)", "Hemoglobin (g/L)"))
  ar <- lab$sheets$ard$ards
  expect_identical(ar$variables[ar$output_id == "T-14"], c("worst", "worst", "base", "tox_dir"))
  expect_true(any(grepl("Hemoglobin (g/L)", lab$code, fixed = TRUE)))
})
