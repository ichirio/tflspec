# tfl_check_spec(): a report's own code list against the define's codes of
# the column adam_meta maps (A7 S3).  Codes only, not labels.  Fictional
# values.

dc_study <- function(codelists = NULL, meta = TRUE, study_lists = NULL, env = parent.frame()) {
  d <- withr_tempdir(env)
  dir.create(file.path(d, "spec", "outputs"), recursive = TRUE)
  defaults <- c(
    if (meta) c(
      "adam_meta:",
      "  variables:",
      "    - {dataset: ADSL, variable: SEX, codelist: CL.SEX}",
      "    - {dataset: ADAE, variable: AEDECOD, codelist: MEDDRA}",
      "  codelists:",
      "    - {codelist: CL.SEX, value: F, decode: Female, order: 1}",
      "    - {codelist: CL.SEX, value: M, decode: Male, order: 2}",
      "  external:",
      "    - {codelist: MEDDRA, dictionary: MedDRA, version: '26.0'}"),
    study_lists)
  writeLines(if (length(defaults)) defaults else "content: []", file.path(d, "spec", "defaults.yml"))
  writeLines(c("id: T-CL", "kind: table", "extends: none",
               if (length(codelists)) c("codelists:", paste0("  ", codelists)),
               "content: []"),
             file.path(d, "spec", "outputs", "T-CL.yml"))
  d
}
dc_rows <- function(d) {
  p <- suppressMessages(tfl_check_spec(d, adam = FALSE))
  p[p$part %in% "codelists", , drop = FALSE]
}

test_that("a mapped column whose own codes are all the define's: no row", {
  d <- dc_study("SEX: [{value: F, label: Female}, {value: M, label: Male}]")
  expect_identical(nrow(dc_rows(d)), 0L)
  # (a list that picks some of the codes, in its own order, too)
  d <- dc_study("SEX: [M]")
  expect_identical(nrow(dc_rows(d)), 0L)
})

test_that("a code the define's list has not: one warning naming the report, column, code and list", {
  d <- dc_study("SEX: [{value: F, label: Female}, {value: M, label: Male}, {value: U, label: Unknown}]")
  w <- dc_rows(d)
  expect_identical(nrow(w), 1L)
  expect_identical(w$output_id, "T-CL")
  expect_identical(w$item, "SEX")
  expect_identical(w$level, "warning")
  expect_match(w$message, "report T-CL: its code list of SEX has the code \"U\", which the define's code list CL.SEX",
               fixed = TRUE)
  expect_match(w$message, "adam_meta maps ADSL.SEX to it", fixed = TRUE)
  # two codes: one row naming both
  d <- dc_study("SEX: [F, X, Y]")
  w <- dc_rows(d)
  expect_identical(nrow(w), 1L)
  expect_match(w$message, "has the codes \"X\", \"Y\"", fixed = TRUE)
  # (only the reports asked for)
  p <- suppressMessages(tfl_check_spec(d, output_id = "T-CL", adam = FALSE))
  expect_identical(sum(p$part %in% "codelists"), 1L)
})

test_that("labels that differ from the define's decodes: no row", {
  d <- dc_study("SEX: [{value: F, label: Women}, {value: M, label: Men}]")
  expect_identical(nrow(dc_rows(d)), 0L)
})

test_that("a column adam_meta does not map, a dictionary's list, or no adam_meta: no row", {
  # (RACE: not mapped; AEDECOD: mapped to MedDRA, which has no values)
  d <- dc_study(c("RACE: [WHITE, MARTIAN]", "AEDECOD: [HEADACHE]"))
  expect_identical(nrow(dc_rows(d)), 0L)
  d <- dc_study("SEX: [F, M, U]", meta = FALSE)
  expect_identical(nrow(dc_rows(d)), 0L)
})

test_that("a study list is not tied to a column by its name: no row", {
  # (adam_meta maps SEX to CL.SEX, not to the study's list SEX; a list's
  # name is not a mapping, so its extra code is not compared)
  d <- dc_study(study_lists = c("codelists:", "  SEX: [{value: F, label: Female}, {value: U, label: Unknown}]"))
  expect_identical(nrow(dc_rows(d)), 0L)
})

test_that("two datasets map a column to different lists: the report's datasets' list only", {
  d <- withr_tempdir()
  dir.create(file.path(d, "spec", "outputs"), recursive = TRUE)
  writeLines(c(
    "adam_meta:",
    "  variables:",
    "    - {dataset: ADLB, variable: AVALC, codelist: CL.LB}",
    "    - {dataset: ADVS, variable: AVALC, codelist: CL.VS}",
    "  codelists:",
    "    - {codelist: CL.LB, value: HIGH}",
    "    - {codelist: CL.LB, value: LOW}",
    "    - {codelist: CL.VS, value: ABNORMAL}"), file.path(d, "spec", "defaults.yml"))
  put <- function(data) {
    writeLines(c("id: T-CL", "kind: table", "extends: none", data,
                 "codelists:", "  AVALC: [HIGH, ABNORMAL]", "content: []"),
               file.path(d, "spec", "outputs", "T-CL.yml"))
  }
  # a report on ADLB: ABNORMAL is ADVS's code, not ADLB's
  put(c("data:", "  - id: adlb_x", "    from: ADLB", "  - id: adlb_y", "    from: adlb_x"))
  w <- dc_rows(d)
  expect_identical(nrow(w), 1L)
  expect_match(w$message, "has the code \"ABNORMAL\", which the define's code list CL.LB does not", fixed = TRUE)
  expect_match(w$message, "adam_meta maps ADLB.AVALC to it", fixed = TRUE)
  # a report on ADVS: HIGH is ADLB's
  put(c("data:", "  - id: advs_x", "    from: ADVS"))
  w <- dc_rows(d)
  expect_identical(nrow(w), 1L)
  expect_match(w$message, "has the code \"HIGH\", which the define's code list CL.VS does not", fixed = TRUE)
  # no dataset known: either list's codes count
  put(character())
  expect_identical(nrow(dc_rows(d)), 0L)
})

# a study whose adam_meta maps ANRIND to CL.LB in ADLB and to CL.VS in ADVS
dc_two_lists <- function(env = parent.frame()) {
  d <- withr_tempdir(env)
  dir.create(file.path(d, "spec", "outputs"), recursive = TRUE)
  writeLines(c(
    "adam_meta:",
    "  variables:",
    "    - {dataset: ADLB, variable: ANRIND, codelist: CL.LB}",
    "    - {dataset: ADVS, variable: ANRIND, codelist: CL.VS}",
    "  codelists:",
    "    - {codelist: CL.LB, value: HIGH}",
    "    - {codelist: CL.LB, value: LOW}",
    "    - {codelist: CL.LB, value: NORMAL}",
    "    - {codelist: CL.VS, value: ABNORMAL}"), file.path(d, "spec", "defaults.yml"))
  d
}

test_that("a template report: its own dataset's list (the template's data rows), not every dataset's", {
  d <- dc_two_lists()
  write_study_yml(d, c("study_id: DC-1"))
  suppressMessages(tfl_copy_templates(d, "tflspec_v1/tflspec_shift"))
  f <- file.path(d, "spec", "outputs", "T-SH.yml")
  # a shift table on ADLB (the template's `from` default) whose list has
  # ABNORMAL, a code of ADVS's list only
  writeLines(c("id: T-SH", "kind: table", "label: Shift",
               "template:", "  name: tflspec_v1/tflspec_shift",
               "  data:", "    from: ADLB",
               "  analysis:", "    by: [TRT01A]",
               "codelists:", "  ANRIND: [HIGH, ABNORMAL]"), f)
  # (the report as written names no dataset; resolved, the template's rows
  # read ADLB)
  o <- tfl_read_output(f)
  expect_identical(.report_datasets(o), character())
  expect_true("ADLB" %in% .report_datasets(.define_codes_report(o, tfl_read_study_spec(d))))
  w <- dc_rows(d)
  expect_identical(nrow(w), 1L)
  expect_identical(w$output_id, "T-SH")
  expect_match(w$message, "has the code \"ABNORMAL\", which the define's code list CL.LB does not", fixed = TRUE)
  expect_match(w$message, "adam_meta maps ADLB.ANRIND to it", fixed = TRUE)
})

test_that("a listing's records map (data: {source: dataset}): that dataset's list", {
  d <- dc_two_lists()
  writeLines(c("id: L-CL", "kind: listing", "extends: none",
               "data: {source: dataset, dataset: ADVS}",
               "codelists:", "  ANRIND: [HIGH, ABNORMAL]",
               "content: []"), file.path(d, "spec", "outputs", "L-CL.yml"))
  w <- dc_rows(d)
  expect_identical(nrow(w), 1L)
  expect_match(w$message, "has the code \"HIGH\", which the define's code list CL.VS does not", fixed = TRUE)
})

test_that("a column mapped to a list with no rows in adam_meta: codelists: gives no row", {
  # (today: a study file cannot map a column to such a list -- the study
  # read stops -- and the check, given one, finds no codes to compare)
  meta <- list(
    variables = data.frame(dataset = "ADSL", variable = "SEX", codelist = "CL.NONE",
                           stringsAsFactors = FALSE),
    codelists = data.frame(codelist = "CL.SEX", value = "F", stringsAsFactors = FALSE))
  expect_null(.define_codes_of(meta, "SEX", "ADSL"))
  outs <- list(`T-CL` = list(id = "T-CL", codelists = list(SEX = c("F", "X")),
                             ard = list(analysis_data = list(list(id = "a", from = "ADSL")))))
  expect_identical(nrow(.check_define_code_rows(list(adam_meta = meta), outs)), 0L)
  d <- dc_study("SEX: [F, X]", meta = FALSE,
                study_lists = c("adam_meta:", "  variables:",
                                "    - {dataset: ADSL, variable: SEX, codelist: CL.NONE}"))
  expect_error(suppressMessages(tfl_check_spec(d, adam = FALSE)), "which codelists: does not have")
})

test_that("the define's codes of a column, by the datasets; a list written in place", {
  meta <- list(
    variables = data.frame(dataset = c("ADLB", "ADVS"), variable = "AVALC",
                           codelist = c("CL.LB", "CL.VS"), stringsAsFactors = FALSE),
    codelists = data.frame(codelist = c("CL.LB", "CL.VS"), value = c("HIGH", "ABNORMAL"),
                           stringsAsFactors = FALSE))
  def <- .define_codes_of(meta, "AVALC")
  expect_identical(def$codelists, c("CL.LB", "CL.VS"))
  expect_identical(def$codes, c("HIGH", "ABNORMAL"))
  def <- .define_codes_of(meta, "AVALC", "advs")
  expect_identical(def$codelists, "CL.VS")
  expect_identical(def$codes, "ABNORMAL")
  expect_null(.define_codes_of(meta, "AVALC", "ADAE"))
  expect_null(.define_codes_of(meta, "SEX"))
  o <- list(ard = list(analysis_data = list(list(id = "a", from = "ADLB"), list(id = "b", from = "a")),
                       analyses = list(list(id = "X", dataset = "ADVS"), list(id = "Y", data = "b"))))
  expect_identical(.report_datasets(o), c("ADLB", "ADVS"))
  expect_identical(.report_datasets(list()), character())
  # (a listing's records map; a table's rework map is not a dataset)
  expect_identical(.report_datasets(list(data = list(source = "dataset", dataset = "ADAE"))), "ADAE")
  expect_identical(.report_datasets(list(data = list(source = "ard", steps = list()))), character())
  expect_identical(.inplace_codes(list(list(value = "A", label = "a"), list(value = "B"))), c("A", "B"))
  expect_identical(.inplace_codes(c(F = "Female")), "F")
  # (a map is not a list written in place)
  expect_null(.inplace_codes(list(list = "SEX")))
})
