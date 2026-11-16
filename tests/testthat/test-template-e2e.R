# The seven tflspec_v1 templates end to end: a report SPEC of each resolved
# (tfl_template_resolve()), written into a study made from the package's
# datasets (fictional), its ARD program made and run (tfl_ard_code(),
# tfl_build_ard()).  The checks are the analyses' ids, one BIGN per report
# and BIGN's N; not the design page's full ARD counts (DM 62, AE 276, ...):
# since 2026-11-16 AE has no arm rows and Lab an id per condition row.
#
# Not run end to end, said in each test's skip: the reports whose items have
# condition rows (ORR, forest, Lab), because the study's sheets have no place
# for an item's conditions yet (tfl_sheets_from_yaml() keeps a cell, not
# rows); Lab also because the generator has no `ards:` and the package's ADLB
# has no ATOXGRL / ATOXGRH grade columns.  Their resolved SPECs are checked.

e2e_rd <- function(text) {
  f <- tempfile(fileext = ".yml")
  on.exit(unlink(f))
  writeLines(text, f)
  .series_read_yaml(f)
}

e2e_settings <- list(settings = list(rounding = "sas"),
                     series_settings = list(tflspec_v1 = list(style = "standard")))

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
      "    conditions:", "      - {tox_dir: High, param: Alanine Aminotransferase}",
      "      - {tox_dir: Low, param: Hemoglobin}")),
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
