# A template SPEC in two parts (`template: {name, ard:, content:}`) beside the
# flat `args:`, and the analysis set from the report's `population:` field
# (migration batch 0: the machinery, no output change)

tp_study <- function() tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))

tp_output <- function(template, population = "SAF", id = "T-9") {
  tfl_output(id, "table", population = population,
             codelists = list(TRT01A = list(list(value = "Drug X"), list(value = "Control"))),
             template = template)
}

# The example of a template with its names shared between `ard:` and `content:`
# (every other name, so both parts are used)
# (a template that declares its parts: each name in its own part)
tp_split <- function(ex, t) {
  part <- vapply(names(ex), function(n) tflspec:::.tpl_part_of(t, n), "")
  i <- if (anyNA(part)) seq_along(ex) %% 2L == 1L else part == "ard"
  list(ard = ex[i], content = ex[!i])
}

test_that("every table template: the example as flat args: and as ard:/content: resolves the same", {
  s <- tp_study()
  nm <- tfl_templates()$name[tfl_templates()$kind == "table"]
  expect_gt(length(nm), 15L)
  done <- 0L
  for (t in nm) {
    ex <- tflspec:::.tpl_examples[[t]]
    if (is.null(ex) || !length(ex)) ex <- list()
    # (demog and stf_t02_demog take the analysis set from the report: no `population` argument)
    if (t %in% c("demog", "stf_t02_demog")) ex$population <- NULL
    flat <- tp_output(list(name = t, args = ex))
    two <- tp_output(c(list(name = t), tp_split(ex, t)))
    a <- tflspec:::.tpl_spec_of(flat)
    b <- tflspec:::.tpl_spec_of(two)
    # (a figure design's preset name is no template SPEC)
    if (is.null(a) || is.null(b)) next
    expect_identical(b$args[names(a$args)], a$args, label = t)
    ra <- tryCatch(tfl_resolve_output(flat, s), error = function(e) NULL)
    if (is.null(ra)) next
    rb <- tfl_resolve_output(two, s)
    expect_identical(rb$content, ra$content, label = t)
    expect_identical(rb$ard, ra$ard, label = t)
    expect_identical(rb$data, ra$data, label = t)
    done <- done + 1L
  }
  expect_gt(done, 15L)
})

test_that("both forms together are read; a name in both stops, naming it", {
  o <- tp_output(list(name = "demog", args = list(continuous = "AGE"), content = list(total_label = "All"),
                      ard = list(categorical = "SEX")))
  a <- tflspec:::.tpl_spec_of(o)$args
  expect_identical(a[c("continuous", "total_label", "categorical")],
                   list(continuous = "AGE", total_label = "All", categorical = "SEX"))
  o <- tp_output(list(name = "demog", args = list(continuous = "AGE"), ard = list(continuous = "AGE")))
  expect_error(tflspec:::.tpl_spec_of(o), "'continuous' is given twice")
  o <- tp_output(list(name = "demog", args = list(by = "ARM"), ard = list(by = "ARM")))
  expect_error(tflspec:::.tpl_spec_of(o), "'by' is given twice")
  o <- tp_output(list(name = "demog", ard = "SEX"))
  expect_error(tflspec:::.tpl_spec_of(o), "`ard:` is a map")
})

test_that("a template that declares its parts: a name in the other part stops, naming the right one", {
  withr::defer(rm(list = "tp_parts", envir = tflspec:::.tpl_registry))
  tflspec:::.tfl_template("tp_parts", kind = "table", layout = "summary",
    params = list(tfl_param("population", "SAF"), tfl_param("continuous", "AGE"),
                  tfl_param("widths", NULL, type = "number")),
    body = function(a, ctx) list(), parts = list(ard = c("population", "continuous"), content = "widths"))
  expect_identical(tflspec:::.tpl_part_of("tp_parts", "continuous"), "ard")
  expect_identical(tflspec:::.tpl_part_of("tp_parts", "widths"), "content")
  expect_true(is.na(tflspec:::.tpl_part_of("tp_parts", "nope")))
  # (demog declares its parts too)
  expect_identical(tflspec:::.tpl_part_of("demog", "continuous"), "ard")
  expect_identical(tflspec:::.tpl_part_of("demog", "widths"), "content")
  ok <- tp_output(list(name = "tp_parts", ard = list(continuous = "AGE"), content = list(widths = 5)))
  expect_identical(tflspec:::.tpl_spec_of(ok)$args, list(continuous = "AGE", widths = 5))
  bad <- tp_output(list(name = "tp_parts", content = list(continuous = "AGE")))
  expect_error(tflspec:::.tpl_spec_of(bad), "'continuous' acts in `ard:`, not `content:`")
  bad <- tp_output(list(name = "tp_parts", ard = list(widths = 5)))
  expect_error(tflspec:::.tpl_spec_of(bad), "'widths' acts in `content:`, not `ard:`")
  # the flat form is read as written
  flat <- tp_output(list(name = "tp_parts", args = list(continuous = "AGE", widths = 5)))
  expect_identical(tflspec:::.tpl_spec_of(flat)$args, list(continuous = "AGE", widths = 5))
  # a variant of it keeps the parts
  tfl_template_variant("tp_parts_v", from = "tp_parts", defaults = list(continuous = "SEX"))
  withr::defer(rm(list = "tp_parts_v", envir = tflspec:::.tpl_registry))
  expect_identical(tflspec:::.tpl_part_of("tp_parts_v", "widths"), "content")
})

test_that("a two-part SPEC survives a YAML round trip", {
  o <- tp_output(list(name = "demog", ard = list(by = "TRT01A", continuous = "AGE", total = TRUE),
                      content = list(labels = c(AGE = "Age (years)"), total_label = "All")))
  f <- withr::local_tempfile(fileext = ".yml")
  tfl_write_output(o, f)
  back <- tfl_read_output(f)
  expect_equal(lapply(tflspec:::.tpl_spec_of(back)$args, unlist), lapply(tflspec:::.tpl_spec_of(o)$args, unlist))
  expect_identical(tfl_resolve_output(back, tp_study())$content,
                   tfl_resolve_output(o, tp_study())$content)
})

test_that("population: not given, it is the report's `population:`", {
  s <- tp_study()
  o <- tp_output(list(name = "summary", args = list(continuous = "AGE")), population = "SAF")
  r <- tfl_resolve_output(o, s)
  expect_identical(r$ard$analysis_data[[1L]]$population, "SAF")
  expect_identical(attr(r, "template")$args$population, "SAF")
  expect_true("population" %in% attr(r, "template")$study)
})

test_that("population: args equal to the report's, or the report has none: used, no message", {
  s <- tp_study()
  base <- tfl_resolve_output(tp_output(list(name = "summary", args = list(continuous = "AGE"))), s)
  same <- tp_output(list(name = "summary", args = list(population = "SAF", continuous = "AGE")))
  expect_no_message(r <- tfl_resolve_output(same, s))
  expect_identical(r$content, base$content)
  expect_identical(r$ard, base$ard)
  # ard: says it too
  two <- tp_output(list(name = "summary", ard = list(population = "SAF", continuous = "AGE")))
  expect_identical(tfl_resolve_output(two, s)$content, base$content)
  # the report has no `population:`: the template's is used
  none <- tp_output(list(name = "summary", args = list(population = "SAF", continuous = "AGE")),
                    population = NULL)
  expect_true(is.na(none$population))
  r <- tfl_resolve_output(none, s)
  expect_identical(r$ard$analysis_data[[1L]]$population, "SAF")
  expect_identical(r$content, base$content)
})

test_that("population: args differing from the report's: built with the template's, with the message (as before)", {
  s <- tp_study()
  o <- tp_output(list(name = "summary", args = list(population = "ITT", continuous = "AGE")))
  expect_message(r <- tfl_resolve_output(o, s), "overrides the study standard \\(output.population\\)")
  expect_identical(r$ard$analysis_data[[1L]]$population, "ITT")
  o <- tp_output(list(name = "summary", ard = list(population = "ITT", continuous = "AGE")))
  expect_message(r <- tfl_resolve_output(o, s), "overrides the study standard")
  expect_identical(r$ard$analysis_data[[1L]]$population, "ITT")
})

test_that("tfl_check_spec(): a template's population that differs from the report's gives a warning row naming both", {
  mk <- function(args, population = "SAF") tp_output(list(name = "summary", args = args), population)
  p <- tflspec:::.check_static_rows(mk(list(continuous = "AGE")), "T1")
  expect_identical(nrow(p), 0L)
  # equal to the report's, or the report has none: used, no row
  expect_identical(nrow(tflspec:::.check_static_rows(mk(list(population = "SAF", continuous = "AGE")), "T1")), 0L)
  expect_identical(nrow(tflspec:::.check_static_rows(mk(list(population = "SAF"), population = NULL), "T1")), 0L)
  expect_identical(nrow(tflspec:::.check_static_rows(
    tp_output(list(name = "summary", ard = list(population = "SAF"))), "T1")), 0L)
  p <- tflspec:::.check_static_rows(mk(list(population = "ITT")), "T1")
  expect_identical(p$level, "warning")
  expect_identical(p$item, "population")
  expect_match(p$message, "(ITT) differs from the report's `population:` (SAF)", fixed = TRUE)
  expect_match(p$message, "make the report's `population:` the one to use", fixed = TRUE)
})

test_that("demog has no `population` parameter: the report's `population:` drives it, args naming it stop", {
  s <- tp_study()
  ok <- tp_output(list(name = "demog", args = list(by = "TRT01A", continuous = "AGE")))
  r <- tfl_resolve_output(ok, s)
  expect_identical(r$ard$analysis_data[[1L]]$population, "SAF")
  for (key in c("args", "ard")) {
    bad <- tp_output(list(name = "demog", args = list(by = "TRT01A", continuous = "AGE")))
    bad$template[[key]] <- c(bad$template[[key]], list(population = "SAF"))
    expect_error(tfl_resolve_output(bad, s), "takes the analysis set from the report's `population:` field")
  }
})

test_that("the look as a template parameter is no warning, and no finding", {
  withr::defer(rm(list = "tp_wide", envir = tflspec:::.tpl_registry))
  expect_no_warning(tfl_template("tp_wide", kind = "table", layout = "summary",
    params = list(tfl_param("population", "SAF"), tfl_param("max_rows", 30L, type = "number")),
    body = function(a, ctx) {
      x <- tfl_template_expand("summary", list(population = a$population, continuous = "AGE"))
      x$content <- c(x$content, list(tfl_item("plan_paginate_rows", list(max_rows = a$max_rows))))
      x
    }))
  expect_identical(setdiff(tfl_test_templates("tp_wide")$status, "no subset"), character(0))
  # an unknown parameter is still found
  expect_error(tfl_template_variant("tp_bad", from = "tp_wide", defaults = list(colour = 1)),
               "no parameter 'colour'")
})
