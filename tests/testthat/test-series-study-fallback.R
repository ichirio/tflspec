# `!study` with a fallback: !study {key: K, fallback: V} (R/series.R).
# The key is a dotted path from the top of study.yml.  Fictional data only.

sf_root <- function() {
  root <- tempfile("series-test")
  dir.create(root)
  root
}

# a series folder under `root`: series.yml (and an empty templates/)
sf_make <- function(root, name = "demo_v1", values = NULL) {
  d <- file.path(root, name)
  dir.create(file.path(d, "templates"), recursive = TRUE, showWarnings = FALSE)
  writeLines(c(
    paste0("name: ", name),
    "label: Demo templates",
    "released: 2026-11-16",
    "needs: tflspec (>= 0.0)",
    "fixes: []",
    "values:",
    values %||% c(
      "  style: tfl_standard",
      "  formats: {N: 0, mean: 2, sd: 3, median: 1, min: 0, max: 0}",
      "  total: false",
      "  max_rows: 30")
  ), file.path(d, "series.yml"))
  d
}

sf_series <- function() {
  root <- sf_root()
  sf_make(root)
  list(root = root, series = tfl_series_read("demo_v1", dir = root))
}

sf_load <- function(text) yaml::yaml.load(text, handlers = tflspec:::.series_handlers())

test_that("!study parses as a scalar and as a {key, fallback} map", {
  x <- sf_load(paste0("a: !study settings.rounding\n",
                      "b: !study {key: settings.formats, fallback: !series formats}\n",
                      "c: !study {key: settings.max_rows, fallback: 30}\n",
                      "d: !study {key: settings.x}"))
  expect_s3_class(x$a, "tfl_study_ref")
  expect_identical(x$a$key, "settings.rounding")
  expect_false("fallback" %in% names(x$a))
  expect_s3_class(x$b, "tfl_study_ref")
  expect_identical(x$b$key, "settings.formats")
  expect_s3_class(x$b$fallback, "tfl_series_ref")
  expect_identical(x$b$fallback$key, "formats")
  expect_identical(x$c$fallback, 30L)
  expect_false("fallback" %in% names(x$d))
  # a bad map is a marker with the problem (yaml::yaml.load() would only warn
  # about an error in a handler); it stops when it is resolved
  sf <- sf_series()
  on.exit(unlink(sf$root, recursive = TRUE), add = TRUE)
  res <- function(text) tflspec:::.series_resolve(sf_load(text), sf$series, template = "demo_v1/km")
  expect_error(res("a: !study {key: k, other: 1, more: 2}"), "template demo_v1/km: .*not other, more")
  expect_error(res("a: !study {fallback: 1}"), "key is missing")
  expect_error(res("a: !study {fallback: 1, x: 2}"), "not x")
  expect_true(!is.null(sf_load("a: !study {fallback: 1}")$a$problem))
})

test_that("an absent key gives the fallback; a !series fallback gives the series value", {
  sf <- sf_series()
  on.exit(unlink(sf$root, recursive = TRUE), add = TRUE)
  x <- sf_load(paste0("plain: !study {key: settings.nope, fallback: 7}\n",
                      "ser: !study {key: settings.nope.deeper, fallback: !series max_rows}\n",
                      "map: !study {key: settings.nope, fallback: {mean: 2, sd: 3}}"))
  st <- list(settings = list(rounding = "sas"))
  r <- tflspec:::.series_resolve(x, sf$series, study = st)
  expect_identical(r$plain, 7L)
  expect_identical(r$ser, 30L)
  expect_identical(r$map, list(mean = 2L, sd = 3L))
  o <- attr(r, "origins")
  expect_identical(o[["plain"]], "template")
  expect_identical(o[["ser"]], "series")
  # a missing !series stops as !series does
  y <- sf_load("a: !study {key: settings.nope, fallback: !series conf_level}")
  expect_error(tflspec:::.series_resolve(y, sf$series, study = st, template = "demo_v1/km"),
               "template demo_v1/km: !series conf_level")
  # a key that is there is used, the fallback is not
  z <- sf_load("a: !study {key: settings.rounding, fallback: !series conf_level}")
  rz <- tflspec:::.series_resolve(z, sf$series, study = st)
  expect_identical(rz$a, "sas")
  expect_identical(attr(rz, "origins")[["a"]], "study")
})

test_that("no study gives the fallback; no fallback and absent is unset", {
  sf <- sf_series()
  on.exit(unlink(sf$root, recursive = TRUE), add = TRUE)
  x <- sf_load(paste0("a: !study {key: settings.formats, fallback: !series formats}\n",
                      "b: !study {key: settings.formats}\n",
                      "c: !study settings.formats"))
  r <- tflspec:::.series_resolve(x, sf$series)
  expect_identical(r$a$mean, 2L)
  expect_identical(attr(r, "origins")[["a"]], "series")
  expect_s3_class(r$b, "tfl_unset")
  expect_s3_class(r$c, "tfl_unset")
  expect_identical(attr(r, "origins")[["b"]], "study")
  r2 <- tflspec:::.series_resolve(x, sf$series, study = list(settings = list()))
  expect_identical(r2$a$mean, 2L)
  expect_s3_class(r2$b, "tfl_unset")
})

test_that("null falls back; !unset is unset; FALSE, 0, empty text and an empty list are kept", {
  sf <- sf_series()
  on.exit(unlink(sf$root, recursive = TRUE), add = TRUE)
  x <- sf_load("a: !study {key: settings.v, fallback: !series max_rows}")
  res <- function(settings) tflspec:::.series_resolve(x, sf$series, study = list(settings = settings))
  # YAML null
  st <- yaml::yaml.load("settings:\n  v: ~", handlers = tflspec:::.yaml_handlers())
  expect_true("v" %in% names(st$settings))
  rn <- tflspec:::.series_resolve(x, sf$series, study = st)
  expect_identical(rn$a, 30L)
  expect_identical(attr(rn, "origins")[["a"]], "series")
  # !unset in the study: unset, not the fallback
  su <- yaml::yaml.load("settings:\n  v: !unset", handlers = tflspec:::.yaml_handlers())
  ru <- tflspec:::.series_resolve(x, sf$series, study = su)
  expect_s3_class(ru$a, "tfl_unset")
  expect_identical(attr(ru, "origins")[["a"]], "study")
  # kept as they are
  for (v in list(FALSE, 0L, "", list())) {
    r <- res(list(v = v))
    expect_identical(r$a, v)
    expect_identical(attr(r, "origins")[["a"]], "study")
  }
})

test_that("a series.yml with !study under values: stops", {
  root <- sf_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  sf_make(root, values = c("  total: false", "  note: !study settings.note"))
  expect_error(tfl_series_read("demo_v1", dir = root),
               "values: note: !study is not allowed in a series's values \\(the series is the study's fallback\\)")
  unlink(file.path(root, "demo_v1"), recursive = TRUE)
  sf_make(root, values = c("  total: false",
                                    "  deep:", "    a: [1, {b: !study {key: k, fallback: 1}}]"))
  expect_error(tfl_series_read("demo_v1", dir = root), "values: deep: !study is not allowed")
  unlink(file.path(root, "demo_v1"), recursive = TRUE)
  sf_make(root)
  expect_s3_class(tfl_series_read("demo_v1", dir = root), "tfl_series")
})

# ---- end to end: a template default with a fallback --------------------------

# a study with its copy of tflspec_v1/tflspec_summary whose `formats` default is the
# fallback form; `formats` is the study.yml settings: formats (a YAML flow map), or NULL
sf_study <- function(root, formats = NULL) {
  st <- file.path(root, "STUDY-SF")
  dir.create(st)
  write_study_yml(st, c("study_id: STUDY-SF", "settings:", "  rounding: sas",
               if (!is.null(formats)) paste0("  formats: ", formats)))
  tfl_copy_templates(st, "tflspec_v1/tflspec_summary")
  f <- file.path(st, "spec", "series", "tflspec_v1", "templates", "tflspec_summary.yml")
  ln <- readLines(f, warn = FALSE)
  at <- which(trimws(ln) == "default: !series formats")
  stopifnot(length(at) == 1L)
  ln[at] <- sub("default: !series formats",
                "default: !study {key: settings.formats, fallback: !series formats}", ln[at], fixed = TRUE)
  writeLines(ln, f)
  st
}

sf_report <- function(formats = NULL) {
  f <- tempfile(fileext = ".yml")
  writeLines(c("id: T-SF-1", "kind: table", "label: Fictional demographics",
               "template:", "  name: tflspec_v1/tflspec_summary", "  analysis:", "    by: TRT01A",
               "    continuous: [AGE]", "    categorical: [SEX]", formats), f)
  f
}

test_that("a template default !study {key, fallback}: the study's map, else the series'", {
  root <- tempfile("sf")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  series_formats <- tfl_series_read("tflspec_v1")$values$formats

  # (a) the study has settings.formats
  dir.create(file.path(root, "a"))
  sa <- sf_study(file.path(root, "a"), "{mean: 5, sd: 1}")
  out_a <- tfl_template_resolve(sf_report(), study = sa)
  expect_identical(attr(out_a, "args")$formats, list(mean = 5L, sd = 1L))
  expect_identical(attr(out_a, "origins")[["formats"]], "study")

  # (b) it has none: the series' formats
  dir.create(file.path(root, "b"))
  sb <- sf_study(file.path(root, "b"))
  out_b <- tfl_template_resolve(sf_report(), study = sb)
  expect_identical(attr(out_b, "args")$formats, series_formats)
  expect_identical(attr(out_b, "origins")[["formats"]], "series")

  # (c) a report's map goes over the resolved map, by key
  out_c <- tfl_template_resolve(sf_report("    formats: {mean: 3, q1: 1}"), study = sa)
  f <- attr(out_c, "args")$formats
  expect_identical(f$mean, 3L)
  expect_identical(f$sd, 1L)
  expect_identical(f$q1, 1L)
  expect_identical(names(f), c("mean", "sd", "q1"))
  out_d <- tfl_template_resolve(sf_report("    formats: {mean: 3, q1: 1}"), study = sb)
  expect_identical(attr(out_d, "args")$formats$mean, 3L)
  expect_identical(attr(out_d, "args")$formats$sd, series_formats$sd)
})
