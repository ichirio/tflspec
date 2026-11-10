# A template's help: its parameters as a table (tfl_tpl_args()) and on
# the screen (tfl_tpl_help())

test_that("tfl_tpl_args(): one row a parameter, the template's order and tiers", {
  a <- tfl_tpl_args("demog")
  p <-tfl_template_params("demog")
  # (the registry keeps `population` for a direct tpl_demog() call; the table leaves it out)
  expect_identical(a$param, setdiff(names(Filter(function(x) !isTRUE(x$hidden), p)), "population"))
  # (demog has no `denominator` a report gives: its percentages are over the
  # analysis set, a hidden fixed value)
  expect_true(isTRUE(p$denominator$hidden))
  expect_identical(names(a), c("param", "tier", "stage", "type", "required", "default", "variant",
                               "study", "effective", "layer", "maps_to", "doc", "study_key", "fallback", "unset"))
  # (the analysis set is the report's: no `population` parameter; the variables and `by` are required)
  expect_false("population" %in% a$param)
  expect_identical(a$param[a$tier == "required"], c("continuous", "categorical", "by"))
  expect_identical(a$param[a$tier == "usual"], c("formats", "total", "labels", "cells"))
  expect_true(all(a$tier %in% c("required", "usual", "other")))
  # each its stage, tfl_param(stage = ): a table's all of its call
  expect_identical(a$stage, unname(vapply(p[a$param], function(x) x$stage %||% "call", "")))
  expect_true(all(a$stage == "call"))
  # the layout's default, the variant's, the one in effect
  r <- function(nm) a[a$param == nm, , drop = FALSE]
  # (`by` and the variables: no default, no variant, not from the study)
  expect_true(r("by")$required)
  for (nm in c("by", "continuous", "categorical")) {
    expect_true(is.na(r(nm)$default), info = nm)
    expect_true(is.na(r(nm)$variant), info = nm)
    expect_true(is.na(r(nm)$effective), info = nm)
    expect_true(is.na(r(nm)$study_key), info = nm)
    expect_identical(r(nm)$layer, "template", info = nm)
  }
  expect_identical(r("count_cell")$maps_to, "plan_cells.categorical, plan_cells.default")
  expect_identical(r("order")$maps_to, "plan_levels.variable")
  # no study: no study value
  expect_true(all(is.na(a$study)))
  expect_false(any(startsWith(a$layer, "study")))
})

test_that("tfl_tpl_args(): a variant of a variant, each default its own variant's", {
  a <- tfl_tpl_args("ae_ser_soc_pt")
  r <- function(nm) a[a$param == nm, , drop = FALSE]
  expect_identical(r("where")$layer, "variant:ae_ser_soc_pt")
  expect_identical(r("any_row_label")$effective, "Any SAE")
  # (the hierarchy is ae_soc_pt's)
  expect_identical(r("hierarchy")$layer, "variant:ae_soc_pt")
  expect_identical(r("hierarchy")$tier, "required")
  expect_false(r("hierarchy")$required)
  expect_identical(r("data")$layer, "template")
})

test_that("tfl_tpl_args(): the study standard's values, layer study: <key>", {
  s <- tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))
  a <- tfl_tpl_args("demog", study = s)
  r <- function(nm) a[a$param == nm, , drop = FALSE]
  # the sample study's plan_cells(categorical = )
  expect_identical(r("count_cell")$effective, "{n:.0f} ({p:.1f%})")
  expect_identical(r("count_cell")$layer, "study: plan_cells.categorical")
  # (the look reaches the template through a reference only: plan_stub.name)
  expect_identical(r("stub_name")$effective, "row_label")
  expect_identical(r("stub_name")$layer, "study: plan_stub.name")
  # one the study does not say: the template's
  expect_identical(r("continuous")$layer, "template")
  expect_identical(r("total")$layer, "template")
  expect_identical(r("total")$effective, "FALSE")
  # a study's folder is read the same
  d <- system.file("extdata", "spec-yaml", package = "tflspec")
  expect_identical(tfl_tpl_args("demog", study = d), a)
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

test_that("tfl_tpl_help(): the call's first argument follows the kind", {
  call_of <- function(nm) {
    out <- capture.output(tfl_tpl_help(nm, tier = "required"))
    out[startsWith(out, "Call: ")]
  }
  # a table: its ARD
  expect_identical(call_of("demog"), "Call: tfl_tpl(ard, \"demog\", ...)")
  # a listing: its data, named after the dataset
  expect_identical(call_of("listing"), "Call: tfl_tpl(adae, \"listing\", ...)")
  # a figure: its data source's name, else `data`
  expect_identical(call_of("mean_se"), "Call: tfl_tpl(adlb, \"mean_se\", ...)")
  expect_identical(call_of("waterfall"), "Call: tfl_tpl(adtr, \"waterfall\", ...)")
  expect_identical(.tpl_first_arg(list(kind = "figure", name = "x", params = list())), "data")
})
