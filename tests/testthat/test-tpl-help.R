# A template's help: its parameters as a table (tfl_tpl_args()) and on
# the screen (tfl_tpl_help())

test_that("tfl_tpl_args(): one row a parameter, the template's order and tiers", {
  a <- tfl_tpl_args("demog")
  p <- tfl_template_params("demog")
  expect_identical(a$param, names(p))
  expect_identical(names(a), c("param", "tier", "stage", "type", "required", "default", "variant",
                               "study", "effective", "layer", "maps_to", "doc"))
  expect_identical(a$param[a$tier == "required"], "population")
  expect_identical(a$param[a$tier == "usual"],
                   c("by", "continuous", "categorical", "labels", "cells", "total"))
  expect_true(all(a$tier %in% c("required", "usual", "other")))
  # each its stage, tfl_param(stage = ): a table's all of its call
  expect_identical(a$stage, unname(vapply(p, function(x) x$stage %||% "call", "")))
  expect_true(all(a$stage == "call"))
  # the layout's default, the variant's, the one in effect
  r <- function(nm) a[a$param == nm, , drop = FALSE]
  expect_identical(r("by")$default, "TRT01A")
  expect_identical(r("by")$layer, "template")
  expect_true(is.na(r("continuous")$default))
  expect_identical(r("continuous")$variant, "AGE")
  expect_identical(r("continuous")$layer, "variant:demog")
  expect_identical(r("categorical")$effective, "AGEGR1, SEX, RACE, ETHNIC")
  expect_identical(r("max_rows")$maps_to, "plan_paginate_rows.max_rows")
  expect_identical(r("order")$maps_to, "plan_levels.variable")
  # no study: no study value
  expect_true(all(is.na(a$study)))
  expect_false(any(a$layer == "study"))
})

test_that("tfl_tpl_args(): a variant of a variant, each default its own variant's", {
  a <- tfl_tpl_args("ae_ser_soc_pt")
  r <- function(nm) a[a$param == nm, , drop = FALSE]
  expect_identical(r("where")$layer, "variant:ae_ser_soc_pt")
  expect_identical(r("overall")$effective, "Any SAE")
  # (the hierarchy is ae_soc_pt's)
  expect_identical(r("hierarchy")$layer, "variant:ae_soc_pt")
  expect_identical(r("hierarchy")$tier, "required")
  expect_false(r("hierarchy")$required)
  expect_identical(r("data")$layer, "template")
})

test_that("tfl_tpl_args(): the study standard's values, layer study", {
  s <- tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))
  a <- tfl_tpl_args("demog", study = s, population = "SAF")
  r <- function(nm) a[a$param == nm, , drop = FALSE]
  expect_identical(r("population")$study, "SAF")
  expect_identical(r("population")$layer, "study")
  # the sample study's plan_stub: {name: row_label, before: true}
  expect_identical(r("stub_name")$effective, "row_label")
  expect_identical(r("stub_name")$layer, "study")
  expect_identical(r("stub_before")$effective, "TRUE")
  expect_identical(r("blanks")$effective, "between_groups")
  # one the study does not say: the variant's, the template's
  expect_identical(r("continuous")$layer, "variant:demog")
  expect_identical(r("max_rows")$layer, "template")
  expect_identical(r("max_rows")$effective, "30")
  # a study's folder is read the same
  d <- system.file("extdata", "spec-yaml", package = "tflspec")
  expect_identical(tfl_tpl_args("demog", study = d, population = "SAF"), a)
  expect_error(tfl_tpl_args("demog", study = 1), "tfl_study_spec")
  expect_error(tfl_tpl_args("no_such"), "no template")
})

test_that("tfl_tpl_help(): the head, the tiers, its table invisibly", {
  expect_snapshot(tfl_tpl_help("demog", tier = c("required", "usual")))
  out <- capture.output(a <- tfl_tpl_help("ae_ser_soc_pt", tier = "required"))
  expect_identical(out[1L],
                   "ae_ser_soc_pt (table, layout hierarchical; variant of ae_soc_pt; tflspec)")
  expect_false(any(grepl("^usual", out)))
  expect_identical(a, tfl_tpl_args("ae_ser_soc_pt"))
  out <- capture.output(tfl_tpl_help("hierarchical"))
  expect_identical(out[1L], "hierarchical (table, layout hierarchical; tflspec)")
  expect_true(any(grepl("^other [(]", out)))
  expect_error(tfl_tpl_help("demog", tier = "most"))
})
