# Branches of R/series.R that no other test reaches: the series name check,
# the search, series.yml's shapes, the tags, the walk and its origins, and
# the formats.  Fictional data only.

cs_root <- function(env = parent.frame()) {
  root <- tempfile("cov-series")
  dir.create(root)
  do.call(on.exit, list(bquote(unlink(.(root), recursive = TRUE)), add = TRUE), envir = env)
  root
}

# a series folder `name` under `root`, its series.yml the given lines
cs_make <- function(root, name = "demo_v1", lines = NULL) {
  d <- file.path(root, name)
  dir.create(file.path(d, "templates"), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines %||% c(paste0("name: ", name), "label: Demo", "values:",
                          "  max_rows: 30", "  style: tfl_standard"),
             file.path(d, "series.yml"))
  d
}

cs_load <- function(text) yaml::yaml.load(text, handlers = tflspec:::.series_handlers())

cs_series <- function(root) {
  cs_make(root)
  tfl_series_read("demo_v1", dir = root)
}

# ---- the series name, and the search ----------------------------------------

test_that("a series name that is not one string of the right shape stops", {
  expect_error(tfl_series_dir(1), "series \"1\" is not a series name")
  expect_error(tfl_series_dir(c("a_v1", "b_v1")), "is not a series name")
  expect_error(tfl_series_dir(NA_character_), "is not a series name")
  expect_error(tfl_series_dir("Demo_v1"), "series \"Demo_v1\" is not a series name")
  expect_error(tfl_series_dir("1demo"), "is not a series name")
  expect_error(tfl_series_read("demo-v1"), "tfl_series_read\\(\\): series \"demo-v1\"")
  expect_error(tfl_series_dir(NULL), "is not a series name")
})

test_that("a series in no folder stops naming the folders looked in", {
  root <- cs_root()
  expect_error(tfl_series_dir("nope_v1", dir = root),
               paste0("no series \"nope_v1\" \\(no nope_v1/series.yml in .*",
                      basename(root)))
})

test_that("an empty dir is skipped, and a dir with a trailing slash is found and normalised", {
  root <- cs_root()
  cs_make(root)
  withr::local_options(tflspec.series_dir = NULL)
  expect_identical(tfl_series_dir("demo_v1", dir = c("", paste0(root, "/"))),
                   normalizePath(file.path(root, "demo_v1"), winslash = "/"))
  hit <- tflspec:::.series_find("demo_v1", dir = c("", root))
  expect_identical(hit$source, "dir")
})

test_that("the option's folder is found after dir, and its source is option", {
  root <- cs_root()
  cs_make(root)
  withr::local_options(tflspec.series_dir = root)
  hit <- tflspec:::.series_find("demo_v1")
  expect_identical(hit$source, "option")
  expect_null(tflspec:::.series_find("nope_v1", dir = root))
})

# ---- series.yml's shapes ----------------------------------------------------

test_that("an empty series.yml is a series with no values and no descriptions", {
  root <- cs_root()
  cs_make(root, lines = character())
  s <- tfl_series_read("demo_v1", dir = root)
  expect_s3_class(s, "tfl_series")
  expect_identical(s$values, list())
  expect_identical(s$descriptions, stats::setNames(character(), character()))
  expect_identical(s$fixes, list())
  expect_null(s$label)
})

test_that("a series.yml that is a sequence, not a map, stops", {
  root <- cs_root()
  cs_make(root, lines = c("- a", "- b"))
  expect_error(tfl_series_read("demo_v1", dir = root), "series.yml is not a map")
})

test_that("a series.yml whose name is another series's stops", {
  root <- cs_root()
  cs_make(root, lines = c("name: other_v1", "values: {a: 1}"))
  expect_error(tfl_series_read("demo_v1", dir = root),
               "series \"demo_v1\": its series.yml says name: other_v1")
})

test_that("values: that are a sequence or one scalar stop", {
  root <- cs_root()
  cs_make(root, lines = c("name: demo_v1", "values: [1, 2]"))
  expect_error(tfl_series_read("demo_v1", dir = root), "values: is a map")
  cs_make(root, lines = c("name: demo_v1", "values: 3"))
  expect_error(tfl_series_read("demo_v1", dir = root), "values: is a map")
})

test_that("values: of null or an empty map are no values", {
  root <- cs_root()
  cs_make(root, lines = c("name: demo_v1", "values: ~"))
  expect_identical(tfl_series_read("demo_v1", dir = root)$values, list())
  cs_make(root, lines = c("name: demo_v1", "values: {}"))
  expect_identical(tfl_series_read("demo_v1", dir = root)$values, list())
})

test_that("descriptions: not a map, or a text that is not one string, stop", {
  root <- cs_root()
  for (d in c("descriptions: [a, b]", "descriptions: {a: [x, y]}", "descriptions: {a: 1}",
              "descriptions: {a: ~}")) {
    cs_make(root, lines = c("name: demo_v1", "values: {a: 1}", d))
    expect_error(tfl_series_read("demo_v1", dir = root), "descriptions: is a map", info = d)
  }
})

test_that("a !study nested in a list of values: stops; one outside values: does not", {
  root <- cs_root()
  cs_make(root, lines = c("name: demo_v1", "values:", "  fmt:", "    deep: [1, !study a.b]"))
  expect_error(tfl_series_read("demo_v1", dir = root),
               "values: fmt: !study is not allowed in a series's values")
  cs_make(root, lines = c("name: demo_v1", "label: !study a.b", "values: {a: 1}"))
  expect_s3_class(tfl_series_read("demo_v1", dir = root)$label, "tfl_study_ref")
})

test_that("print() of a series shows the label only when there is one", {
  root <- cs_root()
  cs_make(root, lines = c("name: demo_v1", "values: {a: 1, b: 2}"))
  s <- tfl_series_read("demo_v1", dir = root)
  out <- capture.output(res <- print(s))
  expect_identical(out[[1L]], "<tfl_series> demo_v1")
  expect_match(out[[3L]], "values: a, b", fixed = TRUE)
  expect_identical(res, s)
  s$label <- "Demo"
  expect_identical(capture.output(print(s))[[1L]], "<tfl_series> demo_v1 (Demo)")
})

# ---- the tags -------------------------------------------------------------

test_that("!study {} with no names, or a key that is not one text, is a marker with a problem", {
  x <- cs_load(paste0("a: !study [x, y]\n",
                      "b: !study {key: [p, q]}\n",
                      "c: !study {key: ''}\n",
                      "d: !study {key: 3}"))
  expect_match(x$a$problem, "<no names>", fixed = TRUE)
  expect_match(x$b$problem, "key is a dotted path")
  expect_match(x$c$problem, "key is a dotted path")
  expect_match(x$d$problem, "key is a dotted path")
  expect_true(all(is.na(c(x$a$key, x$b$key, x$c$key, x$d$key))))
})

test_that("!study {key: K, fallback: ~} keeps a null fallback", {
  x <- cs_load("a: !study {key: k, fallback: ~}")
  expect_true("fallback" %in% names(x$a))
  expect_null(x$a$fallback)
})

test_that("!fixed reads its scalar again as YAML, folds a sequence, keeps a map", {
  x <- cs_load(paste0("n: !fixed 3\n",
                      "t: !fixed abc\n",
                      "b: !fixed true\n",
                      "s: !fixed [1, 2, 3]\n",
                      "m: !fixed {a: 1}"))
  expect_identical(x$n$value, 3L)
  expect_identical(x$t$value, "abc")
  expect_identical(x$b$value, TRUE)
  expect_identical(x$s$value, 1:3)
  expect_identical(x$m$value, list(a = 1L))
})

# ---- the walk ---------------------------------------------------------------

test_that(".series_resolve() takes a series's name, and stops on anything else", {
  root <- cs_root()
  cs_make(root)
  withr::local_options(tflspec.series_dir = root)
  r <- tflspec:::.series_resolve(cs_load("a: !series max_rows"), "demo_v1")
  expect_identical(r$a, 30L)
  expect_error(tflspec:::.series_resolve(list(a = 1), list(name = "demo_v1")),
               "`series` is a tfl_series or a series's name")
})

test_that("with no template the messages name the series", {
  root <- cs_root()
  s <- cs_series(root)
  expect_error(tflspec:::.series_resolve(cs_load("a: !series nope"), s),
               "^series demo_v1: !series nope: no value nope in series demo_v1")
  expect_error(tflspec:::.series_resolve(cs_load("a: !study {fallback: 1}"), s),
               "^series demo_v1: !study")
})

test_that("a NULL value resolves to NULL with no origins; a null element is kept", {
  root <- cs_root()
  s <- cs_series(root)
  expect_null(tflspec:::.series_resolve(NULL, s))
  r <- tflspec:::.series_resolve(cs_load("a: ~\nb: 1"), s)
  expect_true("a" %in% names(r))
  expect_null(r$a)
  expect_identical(attr(r, "origins")[["a"]], "template")
})

test_that("a scalar at the top has the origin `.`; an empty list keeps its origin", {
  root <- cs_root()
  s <- cs_series(root)
  r <- tflspec:::.series_resolve(5L, s, origin = "report")
  expect_identical(attr(r, "origins"), c(. = "report"))
  r <- tflspec:::.series_resolve(list(a = list()), s)
  expect_identical(r$a, list())
  expect_identical(attr(r, "origins")[["a"]], "template")
})

test_that("elements without names are by position, a mixed list by name where there is one", {
  root <- cs_root()
  s <- cs_series(root)
  x <- list(list(1, cs_load("a: !series max_rows")$a), b = 2)
  names(x) <- c("", "b")
  r <- tflspec:::.series_resolve(x, s)
  o <- attr(r, "origins")
  expect_identical(o[["1.1"]], "template")
  expect_identical(o[["1.2"]], "series")
  expect_identical(o[["b"]], "template")
  expect_identical(r[[1L]][[2L]], 30L)
})

test_that("!fixed of a !series is fixed, with its value from the series", {
  root <- cs_root()
  s <- cs_series(root)
  x <- list(a = structure(list(value = structure(list(key = "max_rows"), class = "tfl_series_ref")),
                          class = "tfl_fixed_ref"))
  r <- tflspec:::.series_resolve(x, s)
  expect_identical(as.vector(r$a), 30L)
  expect_true(attr(r$a, "fixed"))
  expect_identical(attr(r$a, "origin"), "fixed")
  expect_identical(attr(r, "origins")[["a"]], "fixed")
})

test_that("a !fixed map's elements have the origin fixed", {
  root <- cs_root()
  s <- cs_series(root)
  r <- tflspec:::.series_resolve(cs_load("a: !fixed {x: 1, y: 2}"), s)
  o <- attr(r, "origins")
  expect_identical(unname(o[c("a.x", "a.y", "a")]), rep("fixed", 3L))
  expect_true(attr(r$a, "fixed"))
})

test_that("tfl_unset and tfl_r values are not walked into", {
  root <- cs_root()
  s <- cs_series(root)
  x <- list(u = tfl_unset(), r = tfl_r("1 + 1"))
  r <- tflspec:::.series_resolve(x, s)
  expect_s3_class(r$u, "tfl_unset")
  expect_s3_class(r$r, "tfl_r")
  expect_identical(unname(attr(r, "origins")[c("u", "r")]), c("template", "template"))
})

# ---- `!study` by what the study has: the value and its origin ---------------

cs_study_cases <- function() {
  list(
    absent = list(settings = list(other = 1L)),
    null = list(settings = list(k = NULL)),
    unset = list(settings = list(k = tfl_unset())),
    false = list(settings = list(k = FALSE)),
    zero = list(settings = list(k = 0L)),
    empty_text = list(settings = list(k = "")),
    empty_list = list(settings = list(k = list())))
}

test_that("!study K without a fallback: each value the study can hold, and its origin", {
  root <- cs_root()
  s <- cs_series(root)
  x <- cs_load("a: !study settings.k")
  r <- lapply(cs_study_cases(), function(st) tflspec:::.series_resolve(x, s, study = st))
  for (nm in names(r)) {
    expect_identical(attr(r[[nm]], "origins")[["a"]], "study", info = nm)
  }
  expect_s3_class(r$absent$a, "tfl_unset")
  expect_s3_class(r$null$a, "tfl_unset")
  expect_s3_class(r$unset$a, "tfl_unset")
  expect_false(r$false$a)
  expect_identical(r$zero$a, 0L)
  expect_identical(r$empty_text$a, "")
  expect_false(inherits(r$empty_text$a, "tfl_unset"))
  expect_identical(r$empty_list$a, list())
})

test_that("!study K with a fallback: absent and null give it; the others are the study's", {
  root <- cs_root()
  s <- cs_series(root)
  x <- cs_load(paste0("a: !study {key: settings.k, fallback: 7}\n",
                      "b: !study {key: settings.k, fallback: !series max_rows}"))
  r <- lapply(cs_study_cases(), function(st) tflspec:::.series_resolve(x, s, study = st))
  for (nm in c("absent", "null")) {
    expect_identical(r[[nm]]$a, 7L, info = nm)
    expect_identical(r[[nm]]$b, 30L, info = nm)
    expect_identical(attr(r[[nm]], "origins")[["a"]], "template", info = nm)
    expect_identical(attr(r[[nm]], "origins")[["b"]], "series", info = nm)
  }
  # the study switched it off: the fallback is not used
  expect_s3_class(r$unset$a, "tfl_unset")
  expect_s3_class(r$unset$b, "tfl_unset")
  expect_false(r$false$a)
  expect_identical(r$zero$b, 0L)
  expect_identical(r$empty_text$a, "")
  expect_identical(r$empty_list$b, list())
  for (nm in c("unset", "false", "zero", "empty_text", "empty_list")) {
    expect_identical(unname(attr(r[[nm]], "origins")[c("a", "b")]), c("study", "study"), info = nm)
  }
})

test_that("a report's !study fallback has the origin report", {
  root <- cs_root()
  s <- cs_series(root)
  x <- cs_load("a: !study {key: settings.k, fallback: 7}")
  r <- tflspec:::.series_resolve(x, s, study = list(), origin = "report")
  expect_identical(attr(r, "origins")[["a"]], "report")
})

test_that("no study, or a step of the key that is not a map, is absent", {
  root <- cs_root()
  s <- cs_series(root)
  x <- cs_load("a: !study {key: settings.k.deep, fallback: 1}\nb: !study settings.k.deep")
  for (st in list(NULL, list(settings = 3L), list(settings = list(k = "text")),
                  list(settings = list(k = c(deep = 2L))))) {
    r <- tflspec:::.series_resolve(x, s, study = st)
    expect_identical(r$a, 1L)
    expect_s3_class(r$b, "tfl_unset")
  }
  expect_s3_class(tflspec:::.series_study_value(NULL, "a"), "tfl_absent")
})

test_that("a null fallback gives NULL, kept in its list", {
  root <- cs_root()
  s <- cs_series(root)
  r <- tflspec:::.series_resolve(cs_load("a: !study {key: k, fallback: ~}\nb: 1"), s, study = list())
  expect_true("a" %in% names(r))
  expect_null(r$a)
})

test_that("a !study value that is a map is the study's as it is (not walked into)", {
  root <- cs_root()
  s <- cs_series(root)
  st <- list(settings = list(formats = list(mean = 1L, sd = 2L)))
  r <- tflspec:::.series_resolve(cs_load("f: !study settings.formats"), s, study = st)
  expect_identical(r$f, list(mean = 1L, sd = 2L))
  expect_identical(attr(r, "origins"), c(f = "study"))
})

# ---- series_settings: and the formats --------------------------------------

test_that("study.yml series_settings: stops naming each series; nothing else does", {
  expect_invisible(tflspec:::.series_settings_gone(NULL))
  expect_invisible(tflspec:::.series_settings_gone("text"))
  expect_invisible(tflspec:::.series_settings_gone(list(study_id = "S-1")))
  expect_error(tflspec:::.series_settings_gone(list(series_settings = list(a_v1 = list(x = 1),
                                                                           b_v1 = list()))),
               "a_v1: move these values into <study>/spec/series/a_v1/series.yml.*b_v1: move")
  expect_error(tflspec:::.series_settings_gone(list(series_settings = list(1, 2)), "C:/studies/S-1"),
               "<series>: move these values into C:/studies/S-1/spec/series/<series>/")
})

test_that(".series_fmt_list(): empty, unnamed, VAR:stat, and numbers that stay numbers", {
  empty <- list(base = list(), vars = list())
  expect_identical(tflspec:::.series_fmt_list(NULL), empty)
  expect_identical(tflspec:::.series_fmt_list(list()), empty)
  expect_error(tflspec:::.series_fmt_list(list(1, 2)), "formats: is a map")
  expect_error(tflspec:::.series_fmt_list(list(a = 1, 2)), "formats: is a map")
  expect_error(tflspec:::.series_fmt_list(list(`:mean` = 1)), "formats: :mean is VAR:stat")
  expect_error(tflspec:::.series_fmt_list(list(`AGE:` = 1)), "formats: AGE: is VAR:stat")
  f <- tflspec:::.series_fmt_list(c(N = 0, mean = 2.5, big = 1e12, `AGE:mean` = 1, `AGE:sd` = 2))
  expect_identical(f$base$N, 0L)
  expect_identical(f$base$mean, 2.5)
  expect_identical(f$base$big, 1e12)
  expect_identical(f$vars, list(AGE = list(mean = 1L, sd = 2L)))
  f <- tflspec:::.series_fmt_list(list(mean = "xx.x", na = NA_real_, two = c(1, 2)))
  expect_identical(f$base, list(mean = "xx.x", na = NA_real_, two = c(1, 2)))
})

test_that(".series_templates_used(): none is an empty frame; a pair is found once", {
  root <- cs_root()
  f1 <- file.path(root, "a.yml")
  f2 <- file.path(root, "b.yml")
  writeLines(c("id: T-1", "kind: table", "content: []"), f1)
  expect_identical(tflspec:::.series_templates_used(f1),
                   data.frame(series = character(), template = character()))
  writeLines(c("id: T-2", "kind: table", "template: {name: demo_v1/demog}",
               "page: {template: {name: plain}}", "other: {template: {name: [a/b, c/d]}}"), f2)
  u <- tflspec:::.series_templates_used(c(f1, f2, f2))
  expect_identical(u, data.frame(series = "demo_v1", template = "demog"))
})

test_that(".series_md5() is the file's md5, unnamed", {
  root <- cs_root()
  f <- file.path(root, "x.txt")
  writeLines("abc", f)
  expect_identical(tflspec:::.series_md5(f), unname(as.character(tools::md5sum(f))))
  expect_null(names(tflspec:::.series_md5(f)))
})
