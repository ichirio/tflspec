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

# a fictional study folder with its own copy of tflspec_v1 (and, given
# `v2`, of a second series tflspec_v2 made from it with other formats)
mm_study <- function(root, v1_formats = NULL, v2_formats = NULL) {
  st <- file.path(root, "STUDY-MM")
  dir.create(st)
  writeLines(c("study_id: STUDY-MM", "settings:", "  rounding: sas"), file.path(st, "study.yml"))
  tfl_copy_templates(st, "tflspec_v1/demog")
  if (!is.null(v1_formats)) tfl_series_values_set(st, "tflspec_v1", list(formats = v1_formats))
  if (!is.null(v2_formats)) {
    src <- file.path(root, "src")
    dir.create(src)
    file.copy(system.file("series", "tflspec_v1", package = "tflspec"), src, recursive = TRUE)
    file.rename(file.path(src, "tflspec_v1"), file.path(src, "tflspec_v2"))
    f <- file.path(src, "tflspec_v2", "series.yml")
    writeLines(sub("^name: tflspec_v1$", "name: tflspec_v2", readLines(f)), f)
    tfl_copy_templates(st, "tflspec_v2/demog", dir = src)
    tfl_series_values_set(st, "tflspec_v2", list(formats = v2_formats))
  }
  st
}

test_that("a map inside the map merges by name too", {
  root <- tempfile("mm")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  st <- mm_study(root, v1_formats = list(mean = 2L, AUCLST = list(mean = 1L, sd = 2L),
                                         CMAX = list(mean = 0L)))
  out <- tfl_template_resolve(
    mm_demog(c("    formats:", "      AUCLST: {mean: 3, median: !unset}",
               "      CMAX: {sd: !unset}", "      TMAX: {median: 1}")),
    study = st)
  f <- attr(out, "args")$formats
  expect_identical(f$mean, 2L)
  expect_identical(f$AUCLST, list(mean = 3L, sd = 2L))
  expect_identical(f$CMAX, list(mean = 0L))
  expect_identical(f$TMAX, list(median = 1L))
  # a map left empty drops its name
  e <- tfl_template_resolve(mm_demog(c("    formats:", "      CMAX: {mean: !unset}")),
                            study = st)
  expect_false("CMAX" %in% names(attr(e, "args")$formats))
  expect_identical(attr(e, "args")$formats$AUCLST, list(mean = 1L, sd = 2L))
})

test_that("two series in one study: each report gets its own series' digits", {
  root <- tempfile("mm")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  st <- mm_study(root, v1_formats = list(N = 0L, mean = 2L, sd = 3L, min = 0L),
                 v2_formats = list(N = 0L, mean = 1L, sd = 2L, min = 1L))
  r2 <- mm_demog(c("    formats: {sd: 4}"))
  r2$template$name <- "tflspec_v2/demog"
  a <- attr(tfl_template_resolve(mm_demog(c("    formats: {mean: 3}")), study = st), "args")$formats
  b <- attr(tfl_template_resolve(r2, study = st), "args")$formats
  # the name each report writes, and its own series for the rest
  expect_identical(a, list(N = 0L, mean = 3L, sd = 3L, min = 0L))
  expect_identical(b, list(N = 0L, mean = 1L, sd = 4L, min = 1L))
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
