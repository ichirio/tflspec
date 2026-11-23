# A study's own copy of the templates it uses (R/study_templates.R):
# tfl_copy_templates(), tfl_template_status(), tfl_template_diff(),
# tfl_recopy_templates(), tfl_series_values_set(), and the study read from
# its copy only.  Fictional names and data only.

stc_root <- function() {
  root <- tempfile("stc")
  dir.create(root)
  root
}

# a source series folder `<root>/src/acme_v1`: series.yml and small templates
stc_source <- function(root) {
  src <- file.path(root, "src")
  d <- file.path(src, "acme_v1", "templates")
  dir.create(d, recursive = TRUE)
  writeLines(c("# acme_v1: a fictional company series", "name: acme_v1", "label: Acme templates",
               "values:", "  # the page", "  max_rows: 30", "  total: false",
               "  total_label: Total", "descriptions:", "  max_rows: The most rows a page holds."),
             file.path(src, "acme_v1", "series.yml"))
  for (t in c("demog", "listing", "ae", "km")) {
    writeLines(c(paste0("template: ", t), paste0("label: Acme ", t), "params: {}"),
               file.path(d, paste0(t, ".yml")))
  }
  src
}

# a study folder with report SPECs naming acme_v1 templates
stc_study <- function(root) {
  st <- file.path(root, "STUDY-X")
  dir.create(file.path(st, "spec", "outputs", "sub"), recursive = TRUE)
  writeLines(c("study_id: STUDY-X", "settings:", "  rounding: sas"), file.path(st, "study.yml"))
  writeLines(c("id: T-1", "kind: table", "template:", "  name: acme_v1/demog"),
             file.path(st, "spec", "outputs", "T-1.yml"))
  writeLines(c("outputs:", "  - id: L-1", "    kind: listing", "    template: {name: acme_v1/listing}",
               "  - id: F-1", "    kind: figure", "    template: {name: km_survival}"),
             file.path(st, "spec", "outputs", "sub", "L-1.yml"))
  st
}

stc_copy <- function(st, ...) file.path(st, "spec", "series", "acme_v1", ...)

test_that("tfl_copy_templates() copies series.yml once and the templates the reports use", {
  root <- stc_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  src <- stc_source(root)
  st <- stc_study(root)
  r <- tfl_copy_templates(st, dir = src)
  expect_identical(names(r), c("series", "template", "file", "from", "md5", "status"))
  expect_identical(r$template, c(NA, "demog", "listing"))
  expect_identical(r$status, rep("copied", 3L))
  expect_identical(r$from[[2L]], file.path(normalizePath(file.path(src, "acme_v1"), winslash = "/"),
                                           "templates", "demog.yml"))
  expect_true(all(file.exists(r$file)))
  expect_identical(readLines(stc_copy(st, "templates", "demog.yml")),
                   readLines(file.path(src, "acme_v1", "templates", "demog.yml")))
  expect_false(file.exists(stc_copy(st, "templates", "ae.yml")))
  # copied.yml: written by tflspec, one entry a file
  cp <- yaml::read_yaml(stc_copy(st, "copied.yml"))
  expect_true(startsWith(readLines(stc_copy(st, "copied.yml"))[1L], "# copied.yml"))
  expect_identical(sort(names(cp)), c("series.yml", "templates/demog.yml", "templates/listing.yml"))
  e <- cp[["templates/demog.yml"]]
  expect_identical(e$from$source, "dir")
  expect_identical(e$from$folder, normalizePath(file.path(src, "acme_v1"), winslash = "/"))
  expect_identical(e$from$template, "demog")
  expect_null(e$from$version)
  expect_identical(e$md5, unname(as.character(tools::md5sum(stc_copy(st, "templates", "demog.yml")))))
  expect_true(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2} ", e$at))

  # never overwrites: a second call finds them present
  writeLines("label: the study's own", stc_copy(st, "templates", "demog.yml"))
  r2 <- tfl_copy_templates(st, dir = src)
  expect_identical(r2$status, rep("present", 3L))
  expect_identical(readLines(stc_copy(st, "templates", "demog.yml")), "label: the study's own")
  # one more, named (the source found again from the folder copied from)
  r3 <- tfl_copy_templates(st, "acme_v1/ae")
  expect_identical(r3$status, c("present", "copied"))
  expect_error(tfl_copy_templates(st, "acme_v1/nope"), "no template acme_v1/nope")
  expect_error(tfl_copy_templates(st, "other_v1/demog"), "no series \"other_v1\"")
  expect_error(tfl_copy_templates(st, "demog"), "write <series>/<name>")
  expect_error(tfl_copy_templates(file.path(root, "none")), "no study folder")
})

test_that("tfl_copy_templates() of the package's series records its version", {
  root <- stc_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  st <- file.path(root, "STUDY-P")
  dir.create(st)
  r <- tfl_copy_templates(st, "tflspec_v1/demog")
  expect_identical(r$status, c("copied", "copied"))
  cp <- yaml::read_yaml(file.path(st, "spec", "series", "tflspec_v1", "copied.yml"))
  expect_identical(cp[["series.yml"]]$from$source, "package")
  expect_identical(cp[["series.yml"]]$from$version, as.character(utils::packageVersion("tflspec")))
  # no report names a template: nothing to copy
  expect_identical(nrow(tfl_copy_templates(st)), 0L)
})

test_that("as: a copy under a new name, a template of the study only", {
  root <- stc_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  src <- stc_source(root)
  st <- stc_study(root)
  r <- tfl_copy_templates(st, "acme_v1/demog", as = "demog_wide", dir = src)
  expect_identical(r$template, c(NA, "demog_wide"))
  expect_identical(r$status, c("copied", "copied"))
  expect_true(file.exists(stc_copy(st, "templates", "demog_wide.yml")))
  expect_false(file.exists(stc_copy(st, "templates", "demog.yml")))
  cp <- yaml::read_yaml(stc_copy(st, "copied.yml"))
  expect_identical(cp[["templates/demog_wide.yml"]]$from$template, "demog")
  s <- tfl_template_status(st)
  expect_identical(s$status[s$template %in% "demog_wide"], "same")
  expect_identical(tfl_copy_templates(st, "acme_v1/demog", as = "demog_wide")$status,
                   c("present", "present"))
  expect_error(tfl_copy_templates(st, c("acme_v1/demog", "acme_v1/ae"), as = "x"), "one template")
  expect_error(tfl_copy_templates(st, "acme_v1/demog", as = "Bad-Name"), "`as` is a template's name")
})

test_that("tfl_template_status() gives each copied file's state, all five", {
  root <- stc_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  src <- stc_source(root)
  st <- stc_study(root)
  tfl_copy_templates(st, paste0("acme_v1/", c("demog", "listing", "ae", "km")), dir = src)
  s0 <- tfl_template_status(st)
  expect_identical(names(s0), c("series", "template", "file", "from", "copied_at", "status"))
  expect_identical(s0$template, c(NA, "ae", "demog", "km", "listing"))
  expect_true(all(s0$status == "same"))
  stmp <- file.path(src, "acme_v1", "templates")
  cat("# the study's note\n", file = stc_copy(st, "templates", "demog.yml"), append = TRUE)
  cat("# a source fix\n", file = file.path(stmp, "listing.yml"), append = TRUE)
  cat("# the study's note\n", file = stc_copy(st, "templates", "ae.yml"), append = TRUE)
  cat("# a source fix\n", file = file.path(stmp, "ae.yml"), append = TRUE)
  unlink(file.path(stmp, "km.yml"))
  s <- tfl_template_status(st)
  got <- stats::setNames(s$status, ifelse(is.na(s$template), "series.yml", s$template))
  expect_identical(got, c(series.yml = "same", ae = "both_changed", demog = "study_changed",
                          km = "source_missing", listing = "source_changed"))
  # no copy at all
  empty <- file.path(root, "STUDY-E")
  dir.create(empty)
  expect_identical(nrow(tfl_template_status(empty)), 0L)
})

test_that("tfl_template_diff() gives the copy's lines against the source", {
  root <- stc_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  src <- stc_source(root)
  st <- stc_study(root)
  tfl_copy_templates(st, "acme_v1/demog", dir = src)
  expect_identical(tfl_template_diff(st, "acme_v1/demog"),
                   c("--- copy", "+++ source", " template: demog", " label: Acme demog", " params: {}"))
  f <- stc_copy(st, "templates", "demog.yml")
  writeLines(c("template: demog", "label: Study demog", "params: {}", "# the study's note"), f)
  expect_identical(tfl_template_diff(st, "acme_v1/demog"),
                   c("--- copy", "+++ source", " template: demog", "-label: Study demog",
                     "+label: Acme demog", " params: {}", "-# the study's note"))
  expect_identical(tflspec:::.line_diff(character(), c("a", "b")), c("+a", "+b"))
  expect_identical(tflspec:::.line_diff(c("a", "b"), character()), c("-a", "-b"))
  expect_error(tfl_template_diff(st, "acme_v1/ae"),
               "tfl_copy_templates(\"", fixed = TRUE)
  unlink(file.path(src, "acme_v1", "templates", "demog.yml"))
  expect_error(tfl_template_diff(st, "acme_v1/demog"), "the source of template acme_v1/demog is missing")
})

test_that("tfl_recopy_templates() copies again, warning when the study's edits are lost", {
  root <- stc_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  src <- stc_source(root)
  st <- stc_study(root)
  tfl_copy_templates(st, paste0("acme_v1/", c("demog", "listing")), dir = src)
  cat("# the study's note\n", file = stc_copy(st, "templates", "demog.yml"), append = TRUE)
  cat("# a source fix\n", file = file.path(src, "acme_v1", "templates", "listing.yml"), append = TRUE)
  cat("# a source fix\n", file = file.path(src, "acme_v1", "templates", "demog.yml"), append = TRUE)
  expect_warning(r <- tfl_recopy_templates(st, "acme_v1/demog"), "the study's edits of .* are lost")
  expect_identical(names(r), c("series", "template", "file", "from", "md5", "status"))
  expect_identical(r$status, "recopied")
  expect_identical(readLines(stc_copy(st, "templates", "demog.yml"))[4L], "# a source fix")
  expect_warning(r2 <- tfl_recopy_templates(st, "acme_v1/listing"), NA)
  expect_identical(r2$md5, unname(as.character(tools::md5sum(stc_copy(st, "templates", "listing.yml")))))
  s <- tfl_template_status(st)
  expect_true(all(s$status == "same"))
  expect_error(tfl_recopy_templates(st, "acme_v1/ae"), "is not copied into the study")
})

test_that("tfl_series_values_set() writes values: in the study's copy, keeping the rest", {
  root <- stc_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  src <- stc_source(root)
  st <- stc_study(root)
  expect_error(tfl_series_values_set(st, "acme_v1", list(max_rows = 25L)),
               "tfl_copy_templates(\"", fixed = TRUE)
  tfl_copy_templates(st, "acme_v1/demog", dir = src)
  f <- stc_copy(st, "series.yml")
  crlf <- any(readBin(f, "raw", file.size(f)) == as.raw(13L))
  out <- tfl_series_values_set(st, "acme_v1", list(max_rows = 25L, total_label = NULL,
                                                   any_row_label = "Subjects with at least one event",
                                                   formats = list(n = 0L, p = "xx.x%")))
  expect_identical(out, normalizePath(f, winslash = "/"))
  y <- yaml::read_yaml(f)
  expect_identical(y$values$max_rows, 25L)
  expect_null(y$values$total_label)
  expect_false(y$values$total)
  expect_identical(y$values$any_row_label, "Subjects with at least one event")
  expect_identical(y$values$formats, list(n = 0L, p = "xx.x%"))
  expect_identical(y$descriptions$max_rows, "The most rows a page holds.")
  x <- readLines(f)
  expect_identical(x[1L], "# acme_v1: a fictional company series")
  expect_true("  # the page" %in% x)
  expect_identical(any(readBin(f, "raw", file.size(f)) == as.raw(13L)), crlf)
  # the copy is what a template's !series reads
  expect_identical(tflspec:::.series_read_at(dirname(f), "acme_v1", "x")$values$max_rows, 25L)
  # values: in flow style is written again as a block
  writeLines(c("name: acme_v1", "values: {max_rows: 30, total: false}"), f)
  tfl_series_values_set(st, "acme_v1", list(total = TRUE))
  expect_identical(yaml::read_yaml(f)$values, list(max_rows = 30L, total = TRUE))
  # no values: yet
  writeLines("name: acme_v1", f)
  tfl_series_values_set(st, "acme_v1", list(total = TRUE))
  expect_identical(yaml::read_yaml(f)$values, list(total = TRUE))
  expect_error(tfl_series_values_set(st, "acme_v1", list(1)), "named list")
})

test_that("a study reads its own copy only; a template not copied stops with the next step", {
  root <- stc_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  st <- file.path(root, "STUDY-R")
  dir.create(st)
  writeLines(c("study_id: STUDY-R", "settings:", "  rounding: sas"), file.path(st, "study.yml"))
  rep <- yaml::yaml.load(paste(c(
    "id: T-1", "kind: table",
    "template:", "  name: tflspec_v1/demog", "  analysis:", "    by: TRT01A",
    "    continuous: [AGE]", "    categorical: [SEX]"), collapse = "\n"),
    handlers = tflspec:::.series_handlers())
  expect_error(tfl_template_resolve(rep, study = st), "tflspec_v1/demog is not copied into the study")
  expect_error(tfl_template_resolve(rep, study = st),
               paste0("tfl_copy_templates(\"", normalizePath(st, winslash = "/"), "\", \"tflspec_v1/demog\")"),
               fixed = TRUE)
  tfl_copy_templates(st, "tflspec_v1/demog")
  tfl_series_values_set(st, "tflspec_v1", list(max_rows = 25L))
  f <- file.path(st, "spec", "series", "tflspec_v1", "templates", "demog.yml")
  x <- readLines(f)
  x[x == "label: Demographics"] <- "label: Demographics of STUDY-R"
  writeLines(x, f)
  for (s in list(st, file.path(st, "study.yml"))) {
    out <- tfl_template_resolve(rep, study = s)
    expect_identical(attr(out, "args")$max_rows, 25L)
    expect_identical(attr(out, "origins")[["max_rows"]], "series")
    expect_identical(out$label, "Demographics of STUDY-R")
  }
  # a study given as a list, without its folder: the package's series
  expect_identical(attr(tfl_template_resolve(rep, study = list()), "args")$max_rows, 30L)
  # a template of the study only (as)
  tfl_copy_templates(st, "tflspec_v1/demog", as = "demog_wide")
  rep$template$name <- "tflspec_v1/demog_wide"
  expect_identical(attr(tfl_template_resolve(rep, study = st), "template"), "tflspec_v1/demog_wide")
  expect_true("by" %in% tfl_arg_defs(template = "tflspec_v1/demog_wide", study_dir = st)$name)
  expect_error(tfl_arg_defs(template = "tflspec_v1/demog_wide"), "no file")
  # the series.yml copy gone: no fall-back to the source
  unlink(file.path(st, "spec", "series", "tflspec_v1", "series.yml"))
  expect_error(tfl_template_resolve(rep, study = st), "series tflspec_v1 is not copied into the study")
})

test_that("study.yml series_settings: stops, naming the series and where the values go", {
  root <- stc_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  st <- file.path(root, "STUDY-S")
  dir.create(st)
  writeLines(c("study_id: STUDY-S", "series_settings:", "  tflspec_v1:", "    max_rows: 25",
               "  acme_v1:", "    total: true"), file.path(st, "study.yml"))
  msg <- "tflspec_v1: move these values into .*spec/series/tflspec_v1/series.yml values: \\(tfl_series_values_set\\(\\)\\), after tfl_copy_templates\\(\\)"
  expect_error(tfl_read_study_spec(st), msg)
  expect_error(tfl_read_study_spec(st), "acme_v1: move these values")
  rep <- list(id = "T-1", kind = "table", template = list(name = "tflspec_v1/demog"))
  expect_error(tfl_template_resolve(rep, study = st), msg)
  expect_error(tfl_template_resolve(rep, study = list(series_settings = list(tflspec_v1 = list(a = 1)))),
               "<study>/spec/series/tflspec_v1/series.yml")
})
