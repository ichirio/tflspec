# A template report's nested arguments on the sheets: its `conditions` (a
# list of maps) are rows of the ARD's conditions sheet with a blank
# analysis_id, read back as the same rows; a map (`formats`, `analysis_ids`)
# is not on the sheets -- it stays in the report's YAML file: back to YAML,
# taken from the report's file written over, else said.  The reports are of
# the test series tflspec_test (local_test_series(): the purpose templates
# that left tflspec_v1, canon 2026-11-24; each builder sets it for the
# calling test).

tn_study <- function() tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))

tn_code <- function() {
  tfl_output("T-8", "table", label = "Plain", common = list(population = "SAF"),
             content = list(tfl_item("plan_paginate_rows", list(max_rows = 24L))))
}

tn_demog <- function() {
  local_test_series(parent.frame())
  tfl_output("T-11", "table", label = "Demographics",
             template = list(name = "tflspec_test/demog", args = list(
               by = "TRT01A", continuous = "AGE", categorical = c("SEX", "RACE"),
               analysis_ids = list(AGE = "AGE_STATS"),
               formats = list(mean = 1L, sd = 2L))))
}

tn_lab <- function() {
  local_test_series(parent.frame())
  tfl_output("T-14", "table", label = "Lab shift",
             template = list(name = "tflspec_test/lab_tox_shift", args = list(
               by = "TRT01A", formats = list(n = 0L, p = "xx.xx%"),
               conditions = list(
                 list(tox_dir = "High", param = "Alanine Aminotransferase (U/L)"),
                 list(tox_dir = "Low", param = "Hemoglobin (g/L)", analysis_id = "HGB_LOW")))))
}

tn_orr <- function() {
  local_test_series(parent.frame())
  tfl_output("T-13", "table", label = "Response",
             template = list(name = "tflspec_test/orr_subgroup", args = list(
               by = "TRT01A", subgroup_vars = c("SEX", "AGEGR1"), conf_level = 0.9,
               conditions = list(
                 list(subgroup = "SEX", subgroup_lvl = "F", where = tfl_r("SEX == \"F\"")),
                 list(subgroup = "AGEGR1", subgroup_lvl = "<65", where = tfl_r("AGEGR1 == \"<65\""))))))
}

# the sheets back to YAML (into `to`, a new folder when NULL), and the
# reports read from it
tn_round_trip <- function(sh, to = NULL) {
  if (is.null(to)) {
    to <- tempfile("tn-yaml")
    dir.create(to)
    on.exit(unlink(to, recursive = TRUE), add = TRUE)
  }
  suppressMessages(tfl_yaml_from_sheets(sh, to, overwrite = TRUE))
  tfl_read_outputs(to)
}

# a report's template as tfl_template_resolve() writes it out
tn_resolve <- function(o, s) {
  tfl_template_resolve(list(id = o$id, kind = o$kind, label = o$label, template = o$template),
                       study = s$study)
}

tn_drop <- function(args, maps) args[setdiff(names(args), maps)]

test_that("a template's conditions are rows of the conditions sheet, analysis_id blank", {
  s <- tn_study()
  expect_no_warning(sh <- suppressMessages(tfl_sheets_from_spec(s, list(`T-14` = tn_lab(), `T-13` = tn_orr()))))
  cs <- sh$ard$conditions
  lab <- cs[cs$output_id == "T-14", , drop = FALSE]
  expect_identical(lab$row, c("1", "2"))
  expect_true(all(is.na(lab$analysis_id)))
  expect_identical(lab$tox_dir, c("High", "Low"))
  expect_identical(lab$row_analysis_id, c(NA, "HGB_LOW"))
  orr <- cs[cs$output_id == "T-13", , drop = FALSE]
  expect_identical(orr$where, c("SEX == \"F\"", "AGEGR1 == \"<65\""))
  # nothing said: the conditions are kept, the maps stay in the YAML
  expect_false(any(sh$not_converted$part == "template"))
})

test_that("lab_tox_shift: its conditions come back the same, its formats are not on the sheets", {
  s <- tn_study()
  o <- tn_lab()
  expect_no_warning(sh <- suppressMessages(tfl_sheets_from_spec(s, list(`T-14` = o, `T-8` = tn_code()))))
  expect_warning(b <- tn_round_trip(sh)[["T-14"]], "formats", fixed = TRUE)
  expect_identical(b$template$name, o$template$name)
  a0 <- tn_drop(o$template$args, "formats")
  a1 <- b$template$args
  expect_identical(a1[names(a0)], a0)
  expect_null(a1$formats)
  expect_identical(tn_resolve(b, s), tn_resolve(tfl_output("T-14", "table", label = "Lab shift",
    template = list(name = o$template$name, args = a0)), s))
})

test_that("orr_subgroup: its conditions (with an R where) come back the same, and resolve the same", {
  s <- tn_study()
  o <- tn_orr()
  expect_no_warning(sh <- suppressMessages(tfl_sheets_from_spec(s, list(`T-13` = o, `T-8` = tn_code()))))
  b <- tn_round_trip(sh)[["T-13"]]
  a0 <- o$template$args
  a1 <- b$template$args
  expect_setequal(names(a1), names(a0))
  expect_identical(a1[names(a0)], a0)
  expect_identical(tn_resolve(b, s), tn_resolve(o, s))
})

test_that("demog: its maps are not on the sheets and not said there; the other arguments come back", {
  s <- tn_study()
  o <- tn_demog()
  expect_no_warning(sh <- suppressMessages(tfl_sheets_from_spec(s, list(`T-11` = o))))
  expect_false(any(sh$not_converted$part == "template"))
  expect_false(any(grepl("AGE_STATS", unlist(sh$templates))))
  expect_false(any(sh$template_rows$param %in% c("analysis_ids", "formats")))
  expect_identical(sh$template_maps$param[sh$template_maps$output_id == "T-11"], c("analysis_ids", "formats"))
  expect_warning(b <- tn_round_trip(sh)[["T-11"]], "analysis_ids", fixed = TRUE)
  a0 <- tn_drop(o$template$args, c("analysis_ids", "formats"))
  expect_identical(b$template$args[names(a0)], a0)
})

test_that("the workbook of the three sheets is read back with the template's conditions", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("readxl")
  s <- tn_study()
  o <- tn_orr()
  sh <- suppressMessages(tfl_sheets_from_spec(s, list(`T-13` = o, `T-14` = tn_lab())))
  f <- tempfile(fileext = ".xlsx")
  on.exit(unlink(f), add = TRUE)
  openxlsx::write.xlsx(list(templates = sh$templates, template_rows = sh$template_rows,
                            conditions = sh$ard$conditions), f)
  back <- tfl_read_template_sheets(f)
  expect_identical(back[["T-13"]]$template$args$conditions, o$template$args$conditions)
  expect_identical(back[["T-14"]]$template$args$conditions, tn_lab()$template$args$conditions)
})

test_that("an empty conditions is no rows and not said; a list of maps of another name still is", {
  s <- tn_study()
  o <- tn_orr()
  o$template$args$conditions <- list()
  expect_no_warning(sh <- suppressMessages(tfl_sheets_from_spec(s, list(`T-13` = o))))
  expect_false("T-13" %in% sh$ard$conditions$output_id)
  o <- tn_orr()
  o$template$args$extra <- list(list(a = "1"))
  expect_warning(sh <- suppressMessages(tfl_sheets_from_spec(s, list(`T-13` = o))), "extra", fixed = TRUE)
  expect_false(grepl("conditions", sh$not_converted$reason[sh$not_converted$part == "template"]))
})

test_that("a condition row the sheet cannot say as it was is still said", {
  s <- tn_study()
  o <- tn_orr()
  o$template$args$conditions[[1L]]$subgroup_lvl <- c("F", "M")
  expect_warning(suppressMessages(tfl_sheets_from_spec(s, list(`T-13` = o))), "conditions", fixed = TRUE)
})

test_that("demog back to YAML over its own file: the maps come from the file, all arguments the same", {
  s <- tn_study()
  to <- tempfile("tn-yaml")
  dir.create(file.path(to, "spec", "outputs"), recursive = TRUE)
  on.exit(unlink(to, recursive = TRUE), add = TRUE)
  tfl_write_output(tn_demog(), file.path(to, "spec", "outputs", "T-11.yml"))
  o <- tfl_read_outputs(to)[["T-11"]]
  sh <- suppressMessages(tfl_sheets_from_spec(s, list(`T-11` = o)))
  expect_no_warning(b <- tn_round_trip(sh, to)[["T-11"]])
  expect_identical(b$template, o$template)
  expect_identical(tn_resolve(b, s), tn_resolve(o, s))
})

test_that("demog back to YAML into an empty folder: the maps lost are named, in a warning and a note", {
  s <- tn_study()
  sh <- suppressMessages(tfl_sheets_from_spec(s, list(`T-11` = tn_demog())))
  to <- tempfile("tn-yaml")
  dir.create(to)
  on.exit(unlink(to, recursive = TRUE), add = TRUE)
  expect_warning(r <- suppressMessages(tfl_yaml_from_sheets(sh, to, overwrite = TRUE)),
                 "analysis_ids, formats", fixed = TRUE)
  nc <- r$not_converted
  expect_true(any(grepl("analysis_ids", nc$reason) & nc$output %in% "T-11"))
})
