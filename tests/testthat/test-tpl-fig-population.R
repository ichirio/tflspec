# A2-2 (canon 11-15): a figure's subjects are its report's analysis set,
# written on its data as `population:`.  forest_hr and km_simple carry the
# series' efficacy set as the default, as km_cards; a report's own set (a
# data row of the subjects' id) goes over it.  Fictional data only.

fp_study <- list(settings = list(rounding = "sas"))

fp_report <- function(text) {
  f <- tempfile(fileext = ".yml")
  on.exit(unlink(f))
  writeLines(text, f)
  .series_read_yaml(f)
}

fp_forest <- function() {
  fp_report(c(
    "id: F-2", "kind: figure", "label: Forest",
    "template:", "  name: tflspec_v1/forest_hr", "  data:", "    subgroup_vars: [SEX]",
    "  analysis:", "    by: TRT01A", "    comparison: Drug X",
    "    conditions:", "      - {subgroup: SEX, subgroup_lvl: F, where: !r SEX == \"F\"}"))
}

fp_km <- function() {
  fp_report(c(
    "id: F-1", "kind: figure", "label: Kaplan-Meier",
    "template:", "  name: tflspec_v1/km_simple",
    "  analysis:", "    by: TRT01A"))
}

fp_subjects_row <- function(o) {
  rows <- o$ard$analysis_data %||% o$data
  Filter(function(r) identical(r$id, "adsl_sub"), rows)[[1L]]
}

test_that("forest_hr and km_simple: the subjects' set is the series' efficacy set by default", {
  for (mk in list(fp_forest, fp_km)) {
    o <- tfl_template_resolve(mk(), study = fp_study)
    expect_identical(fp_subjects_row(o)$population, "FAS")
  }
})

test_that("forest_hr and km_simple: the report's own set on the subjects' data row goes over it", {
  for (mk in list(fp_forest, fp_km)) {
    rep <- mk()
    rep$data <- list(list(id = "adsl_sub", population = "SAF"))
    o <- tfl_template_resolve(rep, study = fp_study)
    expect_identical(fp_subjects_row(o)$population, "SAF")
  }
})
