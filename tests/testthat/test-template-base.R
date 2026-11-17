# The prototype of a report template on a base template (R/tpl_base.R,
# canon 2026-11-17 d399dac / 8b37892): tflspec_dm and tflspec_disposition
# name tflspec_summary and write only what they change.

tb_study <- function() {
  list(settings = list(rounding = "sas"),
       series_settings = list(tflspec_v1 = list(max_rows = 25L)))
}

tb_report <- function(template) {
  list(id = "T-1", kind = "table",
       template = list(name = template,
                       analysis = list(by = "TRT01A", continuous = list("AGE"),
                                       categorical = list("SEX", "RACE"))))
}

# a copy of the package's series in a folder of its own, read first
tb_series_copy <- function() {
  d <- tempfile("series")
  dir.create(d)
  file.copy(system.file("series", "tflspec_v1", package = "tflspec"), d, recursive = TRUE)
  d
}

test_that("tflspec_dm: the base's params, data, analysis and content; its own label and suggestions", {
  dm <- tfl_template_resolve(tb_report("tflspec_v1/tflspec_dm"), study = tb_study())
  sm <- tfl_template_resolve(tb_report("tflspec_v1/tflspec_summary"), study = tb_study())
  for (k in c("data", "ard", "content", "page", "extends", "style")) {
    expect_identical(dm[[k]], sm[[k]], info = k)
  }
  expect_identical(dm$label, "Demographics")
  expect_identical(sm$label, "Summary")
  expect_identical(attr(dm, "args"), attr(sm, "args"))
  sb <- attr(dm, "set_by")
  expect_identical(sb[["by"]], "report")
  expect_identical(sb[["missing_row"]], "tflspec_v1/tflspec_summary")
  expect_identical(attr(sm, "set_by")[["missing_row"]], "tflspec_v1/tflspec_summary")

  a <- tfl_arg_defs(template = "tflspec_v1/tflspec_dm")
  b <- tfl_arg_defs(template = "tflspec_v1/tflspec_summary")
  expect_identical(a$name, b$name)
  expect_identical(a$suggest[[match("categorical", a$name)]], c("AGEGR1", "SEX", "RACE", "ETHNIC"))
  expect_null(a$default[[match("categorical", a$name)]])
  expect_null(b$suggest[[match("categorical", b$name)]])
})

test_that("tflspec_disposition: its own defaults over the base's", {
  rep <- tb_report("tflspec_v1/tflspec_disposition")
  rep$template$analysis$continuous <- NULL
  out <- tfl_template_resolve(rep, study = tb_study())
  expect_identical(attr(out, "args")$population, "ENRL")
  expect_false(attr(out, "args")$missing_row)
  sb <- attr(out, "set_by")
  expect_identical(sb[["population"]], "tflspec_v1/tflspec_disposition")
  expect_identical(sb[["missing_label"]], "tflspec_v1/tflspec_summary")
  expect_identical(out$label, "Subject Disposition")
})

test_that("a change to the base reaches the report template, not an independent template", {
  d <- tb_series_copy()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  old <- options(tflspec.series_dir = d)
  on.exit(options(old), add = TRUE)
  f <- file.path(d, "tflspec_v1", "templates", "tflspec_summary.yml")
  x <- readLines(f)
  at <- grep("^  missing_row:", x)
  k <- at + match("    default: true", x[at:length(x)]) - 1L
  x[k] <- "    default: false"
  writeLines(x, f)
  args <- function(t) attr(tfl_template_resolve(tb_report(t), study = tb_study()), "args")
  expect_false(args("tflspec_v1/tflspec_summary")$missing_row)
  expect_false(args("tflspec_v1/tflspec_dm")$missing_row)
  # demog: the same definition written out in full, not tied to the base
  expect_true(args("tflspec_v1/demog")$missing_row)
})

test_that("base: one level, an existing template of the series, not itself", {
  d <- tb_series_copy()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  old <- options(tflspec.series_dir = d)
  on.exit(options(old), add = TRUE)
  tdir <- file.path(d, "tflspec_v1", "templates")
  writeLines(c("template: t_self", "base: t_self", "label: X"), file.path(tdir, "t_self.yml"))
  writeLines(c("template: t_none", "base: nonesuch", "label: X"), file.path(tdir, "t_none.yml"))
  writeLines(c("template: t_two", "base: tflspec_dm", "label: X"), file.path(tdir, "t_two.yml"))
  res <- function(t) tfl_template_resolve(tb_report(paste0("tflspec_v1/", t)), study = tb_study())
  expect_error(res("t_self"), "names the template itself")
  expect_error(res("t_none"), "no file")
  expect_error(res("t_two"), "one level only")
  expect_error(tfl_arg_defs(template = "tflspec_v1/t_two"), "one level only")
})
