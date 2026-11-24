# A whole series copied into a study (tfl_copy_templates(series =), canon
# 2026-11-24) and the study's copied templates listed
# (tfl_series_templates()).  Only tflspec_v1's basic templates are named;
# the series's full list is read from its folder, never counted here.
# Fictional names and data only.

ssc_root <- function() {
  root <- tempfile("ssc")
  dir.create(root)
  root
}

ssc_study <- function(root, id = "STUDY-W") {
  st <- file.path(root, id)
  dir.create(file.path(st, "spec", "outputs"), recursive = TRUE)
  writeLines(c(paste0("study_id: ", id), "settings:", "  rounding: sas"), file.path(st, "study.yml"))
  st
}

ssc_copy <- function(st, ...) file.path(st, "spec", "series", "tflspec_v1", ...)

ssc_basic <- c("tflspec_summary", "tflspec_hierarchy", "tflspec_shift", "tflspec_listing", "km_cards")

# the package's tflspec_v1 templates, as its folder has them
ssc_source_templates <- function() {
  d <- system.file("series", "tflspec_v1", "templates", package = "tflspec")
  sort(sub("[.]yml$", "", list.files(d, "[.]yml$")))
}

test_that("series = copies series.yml and every template of the series; each resolves", {
  root <- ssc_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  st <- ssc_study(root)
  r <- tfl_copy_templates(st, series = "tflspec_v1")
  expect_identical(names(r), c("series", "template", "file", "from", "md5", "status"))
  expect_identical(r$series, rep("tflspec_v1", nrow(r)))
  expect_true(is.na(r$template[[1L]]))
  expect_identical(r$template[-1L], ssc_source_templates())
  expect_true(all(ssc_basic %in% r$template))
  expect_identical(unique(r$status), "copied")
  expect_true(all(file.exists(r$file)))
  expect_true(file.exists(ssc_copy(st, "series.yml")))
  src <- system.file("series", "tflspec_v1", package = "tflspec")
  for (t in ssc_basic) {
    expect_identical(readLines(ssc_copy(st, "templates", paste0(t, ".yml"))),
                     readLines(file.path(src, "templates", paste0(t, ".yml"))))
  }
  # copied.yml: the same record as one template's copy, one entry a file
  cp <- yaml::read_yaml(ssc_copy(st, "copied.yml"))
  expect_setequal(names(cp), c("series.yml", paste0("templates/", ssc_source_templates(), ".yml")))
  expect_identical(cp[["templates/km_cards.yml"]]$from$source, "package")
  expect_identical(cp[["templates/km_cards.yml"]]$from$template, "km_cards")
  expect_identical(cp[["series.yml"]]$from$version, as.character(utils::packageVersion("tflspec")))
  expect_identical(unique(tfl_template_status(st)$status), "same")

  # each is read from the study's copy
  for (t in ssc_basic) {
    expect_true(nrow(tfl_arg_defs(template = paste0("tflspec_v1/", t), study_dir = st)) > 0L)
  }
  rep <- yaml::yaml.load(paste(c(
    "id: T-1", "kind: table",
    "template:", "  name: tflspec_v1/tflspec_summary", "  analysis:", "    by: [TRT01A]",
    "    continuous: [AGE]", "    categorical: [SEX]"), collapse = "\n"),
    handlers = tflspec:::.series_handlers())
  out <- tfl_template_resolve(rep, study = st)
  expect_identical(attr(out, "template"), "tflspec_v1/tflspec_summary")
})

test_that("series = again keeps every file the study has; nothing is overwritten", {
  root <- ssc_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  st <- ssc_study(root)
  first <- tfl_copy_templates(st, series = "tflspec_v1")
  f <- ssc_copy(st, "templates", "tflspec_summary.yml")
  writeLines(c(readLines(f), "# the study's note"), f)
  tfl_series_values_set(st, "tflspec_v1", list(max_rows = 25L))
  before <- yaml::read_yaml(ssc_copy(st, "copied.yml"))
  # a template the study removed is copied again; the rest are present
  unlink(ssc_copy(st, "templates", "km_cards.yml"))
  again <- tfl_copy_templates(st, series = c("tflspec_v1", "tflspec_v1"))
  expect_identical(again$template, first$template)
  expect_identical(again$status[again$template %in% "km_cards"], "copied")
  expect_identical(unique(again$status[!again$template %in% "km_cards"]), "present")
  expect_identical(utils::tail(readLines(f), 1L), "# the study's note")
  expect_identical(yaml::read_yaml(ssc_copy(st, "series.yml"))$values$max_rows, 25L)
  after <- yaml::read_yaml(ssc_copy(st, "copied.yml"))
  expect_identical(after[["templates/tflspec_summary.yml"]], before[["templates/tflspec_summary.yml"]])
  expect_identical(after[["series.yml"]], before[["series.yml"]])
  s <- tfl_template_status(st)
  expect_identical(s$status[s$template %in% "tflspec_summary"], "study_changed")
})

test_that("series =: an unknown series, a bad name, `as` and a bad type stop clearly", {
  root <- ssc_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  st <- ssc_study(root)
  expect_error(tfl_copy_templates(st, series = "nope_v1"), "no series \"nope_v1\" in the series folders")
  expect_error(tfl_copy_templates(st, series = "Bad-Name"), "is not a series name")
  expect_error(tfl_copy_templates(st, series = NA_character_), "`series` is series names")
  expect_error(tfl_copy_templates(st, series = 1), "`series` is series names")
  expect_error(tfl_copy_templates(st, "tflspec_v1/tflspec_summary", as = "summary_wide",
                                  series = "tflspec_v1"), "not with `series`")
  # nothing was copied by a call that stopped
  expect_false(dir.exists(file.path(st, "spec", "series", "nope_v1")))
  expect_false(file.exists(ssc_copy(st, "series.yml")))
  # a series with no templates
  src <- file.path(root, "src")
  dir.create(file.path(src, "empty_v1"), recursive = TRUE)
  writeLines(c("name: empty_v1", "label: Empty"), file.path(src, "empty_v1", "series.yml"))
  expect_error(tfl_copy_templates(st, series = "empty_v1", dir = src), "series empty_v1 has no templates")
})

test_that("tfl_series_templates() lists the study's copied templates with label and kind", {
  root <- ssc_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  st <- ssc_study(root)
  none <- tfl_series_templates(st)
  expect_identical(names(none), c("series", "template", "label", "kind", "file"))
  expect_identical(nrow(none), 0L)
  invisible(tfl_copy_templates(st, series = "tflspec_v1"))
  x <- tfl_series_templates(st)
  expect_identical(names(x), c("series", "template", "label", "kind", "file"))
  expect_identical(x$template, ssc_source_templates())
  expect_identical(unique(x$series), "tflspec_v1")
  expect_true(all(x$kind %in% c("table", "listing", "figure")))
  b <- x[match(ssc_basic, x$template), , drop = FALSE]
  expect_identical(b$kind, c("table", "table", "table", "listing", "figure"))
  expect_false(anyNA(b$label))
  expect_identical(b$file, normalizePath(ssc_copy(st, "templates", paste0(ssc_basic, ".yml")),
                                         winslash = "/"))
  expect_error(tfl_series_templates(file.path(root, "none")), "no study folder")
})

test_that("tfl_series_templates(): the kind is kind:, else figure:, else a listing style, else a table", {
  root <- ssc_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  src <- file.path(root, "src")
  d <- file.path(src, "acme_v1", "templates")
  dir.create(d, recursive = TRUE)
  writeLines(c("name: acme_v1", "label: Acme", "values:", "  style: tfl_standard",
               "  style_listing: tfl_small"), file.path(src, "acme_v1", "series.yml"))
  writeLines(c("template: a_kind", "label: Kind given", "kind: figure", "params: {}"),
             file.path(d, "a_kind.yml"))
  writeLines(c("template: b_fig", "label: \"A figure: by key\"", "figure: km_curves", "params: {}"),
             file.path(d, "b_fig.yml"))
  writeLines(c("# style: listing in a comment is not a style", "template: c_lst", "params: {}",
               "style: !series style_listing"), file.path(d, "c_lst.yml"))
  writeLines(c("template: d_tab", "label: A table", "params: {}", "style: !series style"),
             file.path(d, "d_tab.yml"))
  writeLines(c("template: e_kind", "kind: listing", "figure: km_curves", "params: {}"),
             file.path(d, "e_kind.yml"))
  st <- ssc_study(root, "STUDY-K")
  r <- tfl_copy_templates(st, series = "acme_v1", dir = src)
  expect_identical(r$template, c(NA, "a_kind", "b_fig", "c_lst", "d_tab", "e_kind"))
  cp <- yaml::read_yaml(file.path(st, "spec", "series", "acme_v1", "copied.yml"))
  expect_identical(cp[["templates/b_fig.yml"]]$from$source, "dir")
  x <- tfl_series_templates(st)
  expect_identical(x$template, c("a_kind", "b_fig", "c_lst", "d_tab", "e_kind"))
  expect_identical(x$kind, c("figure", "figure", "listing", "table", "listing"))
  expect_identical(x$label, c("Kind given", "A figure: by key", NA, "A table", NA))
  # a series folder without series.yml is not a copied series
  dir.create(file.path(st, "spec", "series", "stray_v1", "templates"), recursive = TRUE)
  writeLines("template: z", file.path(st, "spec", "series", "stray_v1", "templates", "z.yml"))
  expect_identical(unique(tfl_series_templates(st)$series), "acme_v1")
})

test_that("a per-template copy and a series copy work together", {
  root <- ssc_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  src <- file.path(root, "src")
  dir.create(file.path(src, "acme_v1", "templates"), recursive = TRUE)
  writeLines(c("name: acme_v1", "label: Acme"), file.path(src, "acme_v1", "series.yml"))
  for (t in c("demog", "ae")) {
    writeLines(c(paste0("template: ", t), paste0("label: Acme ", t), "params: {}"),
               file.path(src, "acme_v1", "templates", paste0(t, ".yml")))
  }
  st <- ssc_study(root, "STUDY-B")
  # a report naming an acme_v1 template: with series = and no templates, it is not copied
  writeLines(c("id: T-1", "kind: table", "template:", "  name: acme_v1/demog"),
             file.path(st, "spec", "outputs", "T-1.yml"))
  one <- tfl_copy_templates(st, "tflspec_v1/tflspec_summary")
  expect_identical(one$status, c("copied", "copied"))
  wide <- tfl_copy_templates(st, "tflspec_v1/tflspec_summary", as = "summary_wide")
  expect_identical(wide$status, c("present", "copied"))
  whole <- tfl_copy_templates(st, series = "tflspec_v1", dir = src)
  expect_identical(whole$status[is.na(whole$template)], "present")
  expect_identical(whole$status[whole$template %in% "tflspec_summary"], "present")
  expect_identical(whole$status[whole$template %in% "km_cards"], "copied")
  expect_false("summary_wide" %in% whole$template)
  expect_false(dir.exists(file.path(st, "spec", "series", "acme_v1")))
  # the template given and a series in one call: both, each file once
  both <- tfl_copy_templates(st, c("acme_v1/demog", "tflspec_v1/km_cards"), series = "tflspec_v1",
                             dir = src)
  expect_identical(sum(both$template %in% "km_cards"), 1L)
  expect_identical(both$status[both$series == "acme_v1"], c("copied", "copied"))
  expect_identical(both$template[both$series == "acme_v1"], c(NA, "demog"))
  expect_false(file.exists(file.path(st, "spec", "series", "acme_v1", "templates", "ae.yml")))
  x <- tfl_series_templates(st)
  expect_identical(unique(x$series), c("acme_v1", "tflspec_v1"))
  expect_true(all(c(ssc_basic, "summary_wide") %in% x$template[x$series == "tflspec_v1"]))
  expect_identical(x$kind[x$template == "summary_wide"], "table")
  expect_identical(x$template[x$series == "acme_v1"], "demog")
})
