# tfl_template_values(): a series template's arguments as resolved, the
# record a template card shows.  Fictional series and study only.

# a source series folder `<root>/src/card_v1`
tv_series <- function(root) {
  src <- file.path(root, "src")
  d <- file.path(src, "card_v1")
  dir.create(file.path(d, "templates"), recursive = TRUE)
  writeLines(c(
    "name: card_v1", "label: Card test templates", "released: 2026-11-19",
    "needs: tflspec (>= 0.0)", "fixes: []", "values:",
    "  style: tfl_standard", "  population_safety: SAF", "  total: false",
    "  formats: {N: 0, mean: 2}"), file.path(d, "series.yml"))
  writeLines(c(
    "template: card", "label: Card test",
    "params:",
    "  by: {type: list<column>, required: true, tier: required}",
    "  total: {type: logical, default: !series total}",
    "  formats: {type: \"map<string,number|string>\", default: !series formats}",
    "  title_note: {type: string, default: !study output.note}",
    "  sponsor: {type: string, default: !study sponsor.name}",
    "  denominator: {type: string, default: !fixed adsl_sub}",
    "  footnote:", "    type: string", "    default: !unset",
    "  missing_row: {type: logical, default: true}",
    "style: !series style",
    "data:",
    "  - id: adsl_sub",
    "    from: ADSL",
    "    population: !series population_safety",
    "analysis:",
    "  - id: CAT",
    "    method: categorical",
    "    data: adsl_sub",
    "    by: [\"{by}\"]",
    "    overall: \"{total}\""), file.path(d, "templates", "card.yml"))
  src
}

# a study folder with its copy of card_v1/card (its total: true)
tv_study <- function(root, src) {
  st <- file.path(root, "STUDY-V")
  dir.create(st)
  writeLines(c("study_id: STUDY-V", "settings:", "  output:", "    note: Fictional study EX-001"),
             file.path(st, "study.yml"))
  tfl_copy_templates(st, "card_v1/card", dir = src)
  tfl_series_values_set(st, "card_v1", list(total = TRUE))
  st
}

tv_row <- function(v, p) as.list(v[match(p, v$name), c("source", "key", "fallback")])

test_that("each param's value in effect and its source, with a study and a report value", {
  root <- tempfile("tv")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  st <- tv_study(root, tv_series(root))
  v <- tfl_template_values("card_v1/card", study = st,
                           args = list(by = "TRT01A", footnote = "Fictional footnote"))
  expect_identical(names(v), c("name", "value", "source", "key", "fallback", "default"))
  expect_identical(v$name, c("by", "total", "formats", "title_note", "sponsor", "denominator",
                             "footnote", "missing_row"))
  val <- stats::setNames(v$value, v$name)
  # the report's value
  expect_identical(val$by, "TRT01A")
  expect_identical(tv_row(v, "by"), list(source = "report", key = NA_character_, fallback = FALSE))
  # !series: the study's copy of series.yml (its own total)
  expect_true(val$total)
  expect_identical(tv_row(v, "total"), list(source = "series", key = "total", fallback = FALSE))
  expect_identical(val$formats$mean, 2L)
  expect_identical(tv_row(v, "formats"), list(source = "series", key = "formats", fallback = FALSE))
  # !study: the study's setting
  expect_identical(val$title_note, "Fictional study EX-001")
  expect_identical(tv_row(v, "title_note"),
                   list(source = "study_settings", key = "output.note", fallback = FALSE))
  # !study the study does not have: the fallback (unset)
  expect_s3_class(val$sponsor, "tfl_unset")
  expect_identical(tv_row(v, "sponsor"),
                   list(source = "fallback", key = "sponsor.name", fallback = TRUE))
  # !fixed
  expect_identical(as.vector(val$denominator), "adsl_sub")
  expect_identical(v$source[v$name == "denominator"], "fixed")
  # the report writes over !unset; a plain default is the template's
  expect_identical(val$footnote, "Fictional footnote")
  expect_identical(v$source[v$name == "footnote"], "report")
  expect_true(val$missing_row)
  expect_identical(v$source[v$name == "missing_row"], "template")
  # the default as written, tagged
  d <- stats::setNames(v$default, v$name)
  expect_identical(attr(d$total, "tag"), "series")
  expect_identical(attr(d$footnote, "tag"), "unset")
  # the data rows, resolved
  rows <- attr(v, "data")
  expect_identical(rows[[1]]$id, "adsl_sub")
  expect_identical(rows[[1]]$population, "SAF")
  expect_true(all(v$source %in% tflspec:::.series_origins))
  # the study as its study.yml file, or as a study spec object: the same
  expect_identical(tfl_template_values("card_v1/card", study = file.path(st, "study.yml"))$source,
                   tfl_template_values("card_v1/card", study = st)$source)
  expect_identical(tfl_template_values("card_v1/card", study = tfl_read_study_spec(st))$source,
                   tfl_template_values("card_v1/card", study = st)$source)
})

test_that("no report values and no study: the source series' defaults", {
  root <- tempfile("tv")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  old <- options(tflspec.series_dir = tv_series(root))
  on.exit(options(old), add = TRUE)
  v <- tfl_template_values("card_v1/card")
  val <- stats::setNames(v$value, v$name)
  # a required param with no value is NULL, not an error
  expect_null(val$by)
  expect_identical(v$source[v$name == "by"], "template")
  # no study: !series the source's value, !study its fallback
  expect_false(val$total)
  expect_identical(v$source[v$name == "total"], "series")
  expect_identical(tv_row(v, "title_note"),
                   list(source = "fallback", key = "output.note", fallback = TRUE))
  # !unset
  expect_s3_class(val$footnote, "tfl_unset")
  expect_identical(v$source[v$name == "footnote"], "template")
  expect_identical(tfl_template_values("card_v1/card", args = list())$source, v$source)
  expect_identical(tfl_template_values("card_v1/card", args = NULL)$source, v$source)
})

test_that("a report: its template, its arguments and data rows; args over them", {
  root <- tempfile("tv")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  st <- tv_study(root, tv_series(root))
  f <- file.path(root, "T-1.yml")
  writeLines(c("id: T-1", "kind: table",
               "data:", "  - id: adsl_sub", "    population: FAS",
               "template:", "  name: card_v1/card", "  analysis:", "    by: TRT01A",
               "    total: false", "    footnote: !unset"), f)
  v <- tfl_template_values(f, study = st)
  val <- stats::setNames(v$value, v$name)
  expect_identical(val$by, "TRT01A")
  expect_false(val$total)
  expect_identical(tv_row(v, "total"), list(source = "report", key = NA_character_, fallback = FALSE))
  expect_s3_class(val$footnote, "tfl_unset")
  expect_identical(v$source[v$name == "footnote"], "report")
  expect_identical(attr(v, "data")[[1]]$population, "FAS")
  v2 <- tfl_template_values(tflspec:::.series_read_yaml(f), study = st, args = list(by = "ARM"))
  expect_identical(v2$value[[1]], "ARM")
})

test_that("the shipped series, with a study given as a list", {
  v <- tfl_template_values("tflspec_v1/demog", study = list(settings = list(rounding = "sas")))
  expect_identical(v$name, tfl_arg_defs(template = "tflspec_v1/demog")$name)
  expect_identical(v$source[v$name == "max_rows"], "series")
})

test_that("a template that is not a series template, or not copied, stops", {
  expect_error(tfl_template_values("demog"), "\"demog\" is not a series template")
  expect_error(tfl_template_values(list(id = "T-1")), "names no template")
  expect_error(tfl_template_values("tflspec_v1/nope"), "no file")
  expect_error(tfl_template_values("tflspec_v1/demog", args = list("TRT01A")), "named list")
  expect_error(tfl_template_values("tflspec_v1/demog", args = list(nope = 1)),
               "has no argument nope")
  root <- tempfile("tv")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  st <- tv_study(root, tv_series(root))
  expect_error(tfl_template_values("tflspec_v1/demog", study = st), "not copied into the study")
})

test_that("a !fixed param the report gives stops, as the resolver does", {
  root <- tempfile("tv")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  st <- tv_study(root, tv_series(root))
  expect_error(tfl_template_values("card_v1/card", study = st, args = list(denominator = "x")),
               "is fixed")
})
