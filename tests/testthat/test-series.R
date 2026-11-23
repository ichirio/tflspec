# The series layer (R/series.R): series folders, the !series / !study /
# !fixed / !unset tags.  A study's copies: test-study-template-copy.R.
# Fictional data only.

series_test_root <- function() {
  root <- tempfile("series-test")
  dir.create(root)
  root
}

# a series folder under `root`: series.yml and templates/<name>.yml
series_test_make <- function(root, name = "demo_v1", values = NULL, templates = list()) {
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
      "  extends: [defaults]",
      "  style: tfl_standard",
      "  population_safety: SAF",
      "  formats: {N: 0, mean: 2, sd: 3, median: 1, min: 0, max: 0, p: \"xx.x%\", \"AGE:mean\": 1}",
      "  total: false",
      "  total_label: Total",
      "  max_rows: 30")
  ), file.path(d, "series.yml"))
  for (t in names(templates)) writeLines(templates[[t]], file.path(d, "templates", paste0(t, ".yml")))
  d
}

series_test_demog <- c(
  "template: demog",
  "label: Demographics",
  "params:",
  "  total: !series total",
  "  missing_row: true",
  "  formats: !series formats",
  "  denominator: !fixed adsl_sub",
  "  page_note: !study output.note",
  "  footnote: !unset",
  "extends: !series extends",
  "style: !series style",
  "data:",
  "  - id: adsl_sub",
  "    from: ADSL",
  "    population: !series population_safety")

test_that("tfl_series_dir() finds a series in dir, then the option", {
  root <- series_test_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  a <- file.path(root, "a")
  b <- file.path(root, "b")
  dir.create(a)
  dir.create(b)
  series_test_make(a, "demo_v1")
  series_test_make(b, "demo_v1")
  series_test_make(b, "other_v2")
  old <- options(tflspec.series_dir = b)
  on.exit(options(old), add = TRUE)
  expect_identical(tfl_series_dir("demo_v1", dir = a),
                   normalizePath(file.path(a, "demo_v1"), winslash = "/"))
  expect_identical(tfl_series_dir("demo_v1"),
                   normalizePath(file.path(b, "demo_v1"), winslash = "/"))
  # the first folder that has <series>/series.yml wins: a has no other_v2
  expect_identical(tfl_series_dir("other_v2", dir = a),
                   normalizePath(file.path(b, "other_v2"), winslash = "/"))
  expect_error(tfl_series_dir("none_v1", dir = a), "no series \"none_v1\"")
})

test_that("a series name is checked", {
  expect_error(tfl_series_dir("Bad-Name"), "series \"Bad-Name\" is not a series name")
  expect_error(tfl_series_read("1abc"), "series \"1abc\"")
  expect_error(tfl_series_read("a/b"), "series \"a/b\"")
})

test_that("tfl_series_read() reads series.yml", {
  root <- series_test_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  series_test_make(root)
  s <- tfl_series_read("demo_v1", dir = root)
  expect_s3_class(s, "tfl_series")
  expect_identical(s$name, "demo_v1")
  expect_identical(s$label, "Demo templates")
  expect_identical(s$values$max_rows, 30L)
  expect_identical(s$values$style, "tfl_standard")
  expect_identical(s$path, normalizePath(file.path(root, "demo_v1"), winslash = "/"))
  expect_true(file.exists(s$file))
  expect_output(print(s), "demo_v1")
  # the name is the folder's
  dir.create(file.path(root, "copy_v1"))
  file.copy(file.path(root, "demo_v1", "series.yml"), file.path(root, "copy_v1"))
  expect_error(tfl_series_read("copy_v1", dir = root), "series \"copy_v1\"")
})

test_that("tfl_series_read() reads the values' descriptions", {
  root <- series_test_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  series_test_make(root)
  # none: an empty named character vector
  s <- tfl_series_read("demo_v1", dir = root)
  expect_identical(s$descriptions, stats::setNames(character(), character()))
  f <- file.path(root, "demo_v1", "series.yml")
  writeLines(c(readLines(f), "descriptions:", "  max_rows: The most rows a page holds."), f)
  s <- tfl_series_read("demo_v1", dir = root)
  expect_identical(s$descriptions, c(max_rows = "The most rows a page holds."))
  writeLines(c(readLines(f), "  style: [a, b]"), f)
  expect_error(tfl_series_read("demo_v1", dir = root), "descriptions: is a map")
})

test_that(".series_resolve() replaces the tags and records the origins", {
  root <- series_test_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  series_test_make(root, templates = list(demog = series_test_demog))
  s <- tfl_series_read("demo_v1", dir = root)
  x <- tflspec:::.series_read_yaml(file.path(s$path, "templates", "demog.yml"))
  study <- list(settings = list(rounding = "sas", output = list(note = "Fictional study")))
  r <- tflspec:::.series_resolve(x, s, study = study, template = "demo_v1/demog")
  expect_false(r$params$total)
  expect_identical(r$params$formats$mean, 2L)
  expect_identical(r$style, "tfl_standard")
  expect_identical(r$extends, "defaults")
  expect_identical(r$data[[1]]$population, "SAF")
  expect_identical(r$params$page_note, "Fictional study")
  expect_s3_class(r$params$footnote, "tfl_unset")
  expect_identical(as.vector(r$params$denominator), "adsl_sub")
  expect_true(attr(r$params$denominator, "fixed"))
  o <- attr(r, "origins")
  expect_identical(o[["params.total"]], "series")
  expect_identical(o[["params.formats"]], "series")
  expect_identical(o[["params.denominator"]], "fixed")
  expect_identical(o[["params.page_note"]], "study")
  expect_identical(o[["params.missing_row"]], "template")
  expect_identical(o[["style"]], "series")
  expect_identical(o[["data.1.population"]], "series")
  expect_identical(o[["data.1.from"]], "template")
  # a report's own values
  rr <- tflspec:::.series_resolve(list(a = 1L), s, origin = "report")
  expect_identical(attr(rr, "origins")[["a"]], "report")
})

test_that("!series without a value stops, naming the template and the key", {
  root <- series_test_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  series_test_make(root)
  s <- tfl_series_read("demo_v1", dir = root)
  x <- yaml::yaml.load("params: {conf: !series conf_level}", handlers = tflspec:::.series_handlers())
  expect_error(tflspec:::.series_resolve(x, s, template = "demo_v1/km"),
               "template demo_v1/km: !series conf_level")
})

test_that("!study: a missing key, or no study, is unset", {
  root <- series_test_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  series_test_make(root)
  s <- tfl_series_read("demo_v1", dir = root)
  x <- yaml::yaml.load("a: !study rounding\nb: !study x.y.z", handlers = tflspec:::.series_handlers())
  r <- tflspec:::.series_resolve(x, s, study = list(settings = list(rounding = "sas")))
  expect_identical(r$a, "sas")
  expect_s3_class(r$b, "tfl_unset")
  r0 <- tflspec:::.series_resolve(x, s)
  expect_s3_class(r0$a, "tfl_unset")
})

test_that("!fixed keeps the value's type; tags in flow style and sequences", {
  root <- series_test_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  series_test_make(root)
  s <- tfl_series_read("demo_v1", dir = root)
  x <- yaml::yaml.load(paste0("a: !fixed 3\nb: !fixed [1, 2]\nc: !fixed {k: true}\n",
                              "d: [!series style, !series total_label]\n",
                              "e: {t: !series total, n: 1}"),
                       handlers = tflspec:::.series_handlers())
  r <- tflspec:::.series_resolve(x, s)
  expect_identical(as.vector(r$a), 3L)
  expect_identical(as.vector(r$b), c(1L, 2L))
  expect_identical(r$c$k, TRUE)
  expect_true(attr(r$c, "fixed"))
  expect_identical(r$d, list("tfl_standard", "Total"))
  expect_false(r$e$t)
  o <- attr(r, "origins")
  expect_identical(o[["c"]], "fixed")
  expect_identical(o[["d.1"]], "series")
})

test_that("study.yml series_settings: stops, naming each series and the next step", {
  study <- list(series_settings = list(demo_v1 = list(max_rows = 25L), other_v1 = list(a = 1L)))
  expect_error(tflspec:::.series_settings_gone(study, "C:/st"),
               "demo_v1: move these values into C:/st/spec/series/demo_v1/series.yml values:")
  expect_error(tflspec:::.series_settings_gone(study), "other_v1: move these values into <study>")
  expect_null(tflspec:::.series_settings_gone(list(settings = list(a = 1))))
})

test_that(".series_fmt_list() splits base and VAR:stat formats", {
  f <- list(N = 0L, mean = 2L, sd = 3, median = 1L, min = 0L, max = 0L, p = "xx.x%", "AGE:mean" = 1L)
  expect_identical(tflspec:::.series_fmt_list(f),
                   list(base = list(N = 0L, mean = 2L, sd = 3L, median = 1L, min = 0L, max = 0L,
                                    p = "xx.x%"),
                        vars = list(AGE = list(mean = 1L))))
  y <- yaml::yaml.load('{N: 0, mean: 2.0, sd: 2.5, "AGE:mean": 1, "AGE:sd": 2, "BMI:mean": 1}',
                       handlers = tflspec:::.series_handlers())
  r <- tflspec:::.series_fmt_list(y)
  expect_identical(r$base, list(N = 0L, mean = 2L, sd = 2.5))
  expect_identical(r$vars, list(AGE = list(mean = 1L, sd = 2L), BMI = list(mean = 1L)))
  expect_identical(tflspec:::.series_fmt_list(NULL), list(base = list(), vars = list()))
})
