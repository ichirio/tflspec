# A report's map argument over the series' map, name by name (canon
# e023312): tfl_template_resolve() on fictional reports of the tflspec_v1
# series.

mm_report <- function(text) {
  f <- tempfile(fileext = ".yml")
  on.exit(unlink(f), add = TRUE)
  writeLines(text, f)
  tflspec:::.series_read_yaml(f)
}

mm_demog <- function(formats = NULL) {
  mm_report(c("id: T-MM-1", "kind: table", "label: Fictional demographics",
              "template:", "  name: tflspec_v1/demog", "  analysis:", "    by: TRT01A",
              "    continuous: [AGE]", "    categorical: [SEX]", formats))
}

mm_series_formats <- function() tfl_series_read("tflspec_v1")$values$formats

test_that("a report's formats keep the series' names it does not write", {
  out <- tfl_template_resolve(mm_demog(c("    formats: {mean: 3, q1: 1}")))
  f <- attr(out, "args")$formats
  s <- mm_series_formats()
  expect_identical(f$mean, 3L)
  expect_identical(f$q1, 1L)
  for (k in setdiff(names(s), "mean")) expect_identical(f[[k]], s[[k]])
  expect_identical(names(f), c(names(s), "q1"))
  expect_identical(attr(out, "origins")[["formats"]], "report")
  # not written: the series' map as it is
  d <- tfl_template_resolve(mm_demog())
  expect_identical(attr(d, "args")$formats, s)
  expect_identical(attr(d, "origins")[["formats"]], "series")
})

test_that("a name whose value is !unset is removed from the series' map", {
  out <- tfl_template_resolve(mm_demog(c("    formats: {sd: !unset, \"AGE:sd\": !unset}")))
  f <- attr(out, "args")$formats
  expect_false(any(c("sd", "AGE:sd") %in% names(f)))
  expect_identical(f$mean, mm_series_formats()$mean)
  # the whole argument !unset: no formats
  u <- tfl_template_resolve(mm_demog(c("    formats: !unset")))
  expect_null(attr(u, "args")$formats)
})

test_that("a map inside the map merges by name too", {
  study <- list(series_settings = list(tflspec_v1 = list(
    formats = list(mean = 2L, AUCLST = list(mean = 1L, sd = 2L), CMAX = list(mean = 0L)))))
  out <- tfl_template_resolve(
    mm_demog(c("    formats:", "      AUCLST: {mean: 3, median: !unset}",
               "      CMAX: {sd: !unset}", "      TMAX: {median: 1}")),
    study = study)
  f <- attr(out, "args")$formats
  expect_identical(f$mean, 2L)
  expect_identical(f$AUCLST, list(mean = 3L, sd = 2L))
  expect_identical(f$CMAX, list(mean = 0L))
  expect_identical(f$TMAX, list(median = 1L))
  # a map left empty drops its name
  e <- tfl_template_resolve(mm_demog(c("    formats:", "      CMAX: {mean: !unset}")),
                            study = study)
  expect_false("CMAX" %in% names(attr(e, "args")$formats))
  expect_identical(attr(e, "args")$formats$AUCLST, list(mean = 1L, sd = 2L))
})

test_that("a list argument is still replaced whole", {
  r <- mm_report(c("id: T-MM-2", "kind: table", "label: Fictional AEs",
                   "template:", "  name: tflspec_v1/ae_soc_pt", "  analysis:", "    by: TRT01A",
                   "    sort: [label]"))
  out <- tfl_template_resolve(r)
  expect_identical(unlist(attr(out, "args")$sort), "label")
})

test_that(".tr_merge_map(): names over, maps merged, !unset removed", {
  m <- tflspec:::.tr_merge_map
  expect_identical(m(list(a = 1, b = 2), list(b = 3, c = 4)), list(a = 1, b = 3, c = 4))
  expect_identical(m(list(a = 1, b = 2), list(a = tfl_unset())), list(b = 2))
  expect_null(m(list(a = 1), list(a = tfl_unset())))
  expect_identical(m(NULL, list(a = 1, b = tfl_unset())), list(a = 1))
  expect_identical(m(tfl_unset(), list(a = 1)), list(a = 1))
  expect_identical(m(list(a = list(x = 1)), list(a = 2)), list(a = 2))
  expect_identical(m(list(a = 1), list(a = list(x = 1))), list(a = list(x = 1)))
  expect_identical(m(list(a = 1), list()), list(a = 1))
})
