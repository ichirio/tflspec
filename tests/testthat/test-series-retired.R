# The purpose templates that left tflspec_v1 (canon 2026-11-24): none is
# shipped, each has its replacement (orr_subgroup none yet), and a study
# that names one without a copy is told it was retired and what to use.

sr_study <- function(root) {
  st <- file.path(root, "STUDY-RET")
  dir.create(st)
  writeLines(c("study_id: STUDY-RET", "settings:", "  rounding: sas"), file.path(st, "study.yml"))
  st
}

sr_report <- function(name) {
  list(id = "T-1", kind = "table", template = list(name = name, analysis = list(by = "TRT01A")))
}

test_that("the retired templates are not shipped; their replacements are", {
  dir <- system.file("series", "tflspec_v1", "templates", package = "tflspec")
  expect_setequal(names(.series_retired),
                  paste0("tflspec_v1/", c("demog", "ae_soc_pt", "lab_tox_shift", "listing", "orr_subgroup")))
  for (nm in names(.series_retired)) {
    expect_false(file.exists(file.path(dir, paste0(sub("^.*/", "", nm), ".yml"))), info = nm)
    by <- .series_retired[[nm]]
    if (!is.na(by)) expect_true(file.exists(file.path(dir, paste0(sub("^.*/", "", by), ".yml"))), info = nm)
  }
  expect_identical(.series_retired_note("tflspec_v1/tflspec_summary"), "")
  expect_identical(.series_retired_note("acme_v1/demog"), "")
  expect_identical(.series_retired_note(NA_character_), "")
  expect_identical(.series_retired_note("tflspec_v1/demog"),
                   "tflspec_v1/demog was retired from the series (2026-11-24); use tflspec_v1/tflspec_summary instead")
  expect_match(.series_retired_note("tflspec_v1/orr_subgroup"),
               "no replacement yet; use tflspec_v1/tflspec_summary", fixed = TRUE)
})

test_that("a study with no copy of a retired template: the stop names it retired and its replacement", {
  st <- sr_study(withr::local_tempdir())
  # the report's template, in the study folder: no copy-it hint
  err <- tryCatch(tfl_template_resolve(sr_report("tflspec_v1/demog"), study = st), error = conditionMessage)
  expect_match(err, "tflspec_v1/demog is not copied into the study", fixed = TRUE)
  expect_match(err, "tflspec_v1/demog was retired from the series", fixed = TRUE)
  expect_match(err, "use tflspec_v1/tflspec_summary instead", fixed = TRUE)
  expect_false(grepl("tfl_copy_templates(", err, fixed = TRUE))
  # no study folder: the series folders, the same sentence
  expect_error(tfl_template_resolve(sr_report("tflspec_v1/ae_soc_pt"), study = list()),
               "use tflspec_v1/tflspec_hierarchy instead", fixed = TRUE)
  expect_error(tfl_arg_defs(template = "tflspec_v1/listing"), "use tflspec_v1/tflspec_listing instead",
               fixed = TRUE)
  # a copy asked for
  expect_error(tfl_copy_templates(st, "tflspec_v1/lab_tox_shift"), "use tflspec_v1/tflspec_shift instead",
               fixed = TRUE)
  expect_error(tfl_copy_templates(st, "tflspec_v1/orr_subgroup"),
               "no replacement yet; use tflspec_v1/tflspec_summary", fixed = TRUE)
  # (any other missing template: as before, no sentence)
  err <- tryCatch(tfl_template_resolve(sr_report("tflspec_v1/no_such"), study = st), error = conditionMessage)
  expect_match(err, "tfl_copy_templates(", fixed = TRUE)
  expect_false(grepl("retired", err, fixed = TRUE))
})
