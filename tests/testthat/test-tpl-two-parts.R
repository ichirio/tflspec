# A template SPEC in two parts (`template: {name, analysis:, content:}`) beside the
# flat `args:`, and the analysis set from the report's `population:` field
# (migration batch 0: the machinery, no output change)

tp_study <- function() tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))

tp_output <- function(template, population = "SAF", id = "T-9") {
  tfl_output(id, "table", common = list(population = population),
             codelists = list(TRT01A = list(list(value = "Drug X"), list(value = "Control"))),
             template = template)
}

# The example of a template with its names shared between `analysis:` and `content:`
# (every other name, so both parts are used)
# (a template that declares its parts: each name in its own part)
tp_split <- function(ex, t) {
  part <- vapply(names(ex), function(n) tflspec:::.tpl_part_of(t, n), "")
  i <- if (anyNA(part)) seq_along(ex) %% 2L == 1L else part == "analysis"
  list(analysis = ex[i], content = ex[!i])
}

test_that("every table template: the example as flat args: and as analysis:/content: resolves the same", {
  s <- tp_study()
  nm <- tfl_templates()$name[tfl_templates()$kind == "table"]
  expect_gt(length(nm), 15L)
  done <- 0L
  for (t in nm) {
    ex <- tflspec:::.tpl_examples[[t]]
    if (is.null(ex) || !length(ex)) ex <- list()
    # (demog and stf_t02_demog take the analysis set from the report: no `population` argument)
    if (t %in% c("demog", "stf_t02_demog", "summary", "disposition")) ex$population <- NULL
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
                      analysis = list(categorical = "SEX")))
  a <- tflspec:::.tpl_spec_of(o)$args
  expect_identical(a[c("continuous", "total_label", "categorical")],
                   list(continuous = "AGE", total_label = "All", categorical = "SEX"))
  o <- tp_output(list(name = "demog", args = list(continuous = "AGE"), analysis = list(continuous = "AGE")))
  expect_error(tflspec:::.tpl_spec_of(o), "'continuous' is given twice")
  o <- tp_output(list(name = "demog", args = list(by = "ARM"), analysis = list(by = "ARM")))
  expect_error(tflspec:::.tpl_spec_of(o), "'by' is given twice")
  o <- tp_output(list(name = "demog", analysis = "SEX"))
  expect_error(tflspec:::.tpl_spec_of(o), "`analysis:` is a map")
})

test_that("a template that declares its parts: a name in the other part stops, naming the right one", {
  withr::defer(rm(list = "tp_parts", envir = tflspec:::.tpl_registry))
  tflspec:::.tfl_template("tp_parts", kind = "table", layout = "summary",
    params = list(tfl_param("population", "SAF"), tfl_param("continuous", "AGE"),
                  tfl_param("widths", NULL, type = "number")),
    body = function(a, ctx) list(), parts = list(ard = c("population", "continuous"), content = "widths"))
  expect_identical(tflspec:::.tpl_part_of("tp_parts", "continuous"), "analysis")
  expect_identical(tflspec:::.tpl_part_of("tp_parts", "widths"), "content")
  expect_true(is.na(tflspec:::.tpl_part_of("tp_parts", "nope")))
  # (demog declares its parts too)
  expect_identical(tflspec:::.tpl_part_of("demog", "continuous"), "analysis")
  expect_identical(tflspec:::.tpl_part_of("demog", "widths"), "content")
  ok <- tp_output(list(name = "tp_parts", analysis = list(continuous = "AGE"), content = list(widths = 5)))
  expect_identical(tflspec:::.tpl_spec_of(ok)$args, list(continuous = "AGE", widths = 5))
  bad <- tp_output(list(name = "tp_parts", content = list(continuous = "AGE")))
  expect_error(tflspec:::.tpl_spec_of(bad), "'continuous' acts in `analysis:`, not `content:`")
  bad <- tp_output(list(name = "tp_parts", analysis = list(widths = 5)))
  expect_error(tflspec:::.tpl_spec_of(bad), "'widths' acts in `content:`, not `analysis:`")
  # the flat form is read as written
  flat <- tp_output(list(name = "tp_parts", args = list(continuous = "AGE", widths = 5)))
  expect_identical(tflspec:::.tpl_spec_of(flat)$args, list(continuous = "AGE", widths = 5))
  # a variant of it keeps the parts
  tfl_template_variant("tp_parts_v", from = "tp_parts", defaults = list(continuous = "SEX"))
  withr::defer(rm(list = "tp_parts_v", envir = tflspec:::.tpl_registry))
  expect_identical(tflspec:::.tpl_part_of("tp_parts_v", "widths"), "content")
})

test_that("a two-part SPEC survives a YAML round trip", {
  o <- tp_output(list(name = "demog", analysis = list(by = "TRT01A", continuous = "AGE", total = TRUE),
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
  o <- tp_output(list(name = "ae_soc_pt", args = list()), population = "SAF")
  r <- tfl_resolve_output(o, s)
  expect_identical(r$ard$analysis_data[[1L]]$population, "SAF")
  expect_identical(attr(r, "template")$args$population, "SAF")
  expect_false("population" %in% attr(r, "template")$study)
})

test_that("population: args equal to the report's, or the report has none: used, no message", {
  s <- tp_study()
  base <- tfl_resolve_output(tp_output(list(name = "ae_soc_pt", args = list())), s)
  same <- tp_output(list(name = "ae_soc_pt", args = list(population = "SAF")))
  expect_no_message(r <- tfl_resolve_output(same, s))
  expect_identical(r$content, base$content)
  expect_identical(r$ard, base$ard)
  # analysis: says it too
  two <- tp_output(list(name = "ae_soc_pt", analysis = list(population = "SAF")))
  expect_identical(tfl_resolve_output(two, s)$content, base$content)
  # the report has no `population:`: the template's is used
  none <- tp_output(list(name = "ae_soc_pt", args = list(population = "SAF")),
                    population = NULL)
  expect_true(is.na(tflspec:::.out_population(none)))
  r <- tfl_resolve_output(none, s)
  expect_identical(r$ard$analysis_data[[1L]]$population, "SAF")
  expect_identical(r$content, base$content)
})

test_that("population: args differing from the report's `common: population`: written twice, an error (A2-T Q6)", {
  s <- tp_study()
  o <- tp_output(list(name = "ae_soc_pt", args = list(population = "ITT")))
  expect_error(tfl_resolve_output(o, s), "the set is written twice")
  o <- tp_output(list(name = "ae_soc_pt", analysis = list(population = "ITT")))
  expect_error(tfl_resolve_output(o, s), "the set is written twice")
  # the argument alone: built with it
  r <- tfl_resolve_output(tp_output(list(name = "ae_soc_pt", args = list(population = "ITT")), population = NULL), s)
  expect_identical(r$ard$analysis_data[[1L]]$population, "ITT")
})

test_that("tfl_check_spec(): a template's set argument and the report's own row over its subjects row: an error (A2-T Q6)", {
  mk <- function(args, population = NULL) tp_output(list(name = "ae_soc_pt", args = args), population)
  expect_identical(nrow(tflspec:::.check_static_rows(mk(list(population = "SAF")), "T1")), 0L)
  # the template's subjects row (its id) written over with another set
  x <- tfl_template_expand("ae_soc_pt", list(population = "SAF"))
  subj <- Filter(function(a) identical(a$population, "SAF") && is.null(a$subjects_in),
                 c(if (is.null(names(x$data))) x$data, x$ard$analysis_data))[[1L]]$id
  o <- mk(list(population = "SAF"))
  o$ard <- list(analysis_data = list(list(id = subj, from = "ADSL", population = "ITT")))
  p <- tflspec:::.check_static_rows(o, "T1")
  expect_identical(p$level, "error")
  expect_identical(p$item, paste0("data$", subj))
  expect_match(p$message, "the set is written twice: the template argument `population` (SAF) and ",
               fixed = TRUE)
  # the same set there, or a row of its own (another set's denominator): no row
  o$ard$analysis_data[[1L]]$population <- "SAF"
  expect_identical(nrow(tflspec:::.check_static_rows(o, "T1")), 0L)
  o$ard$analysis_data <- list(list(id = "adsl_itt", from = "ADSL", population = "ITT"))
  expect_identical(nrow(tflspec:::.check_static_rows(o, "T1")), 0L)
  # the template's argument and another set beside it (`common:` until it
  # goes, the data map, the canon's place): one report, one message -- the
  # resolve's stop, which tfl_check_spec() says as an error row; no row of
  # the look before the build
  expect_identical(nrow(tflspec:::.check_static_rows(mk(list(population = "ITT"), population = "SAF"), "T1")), 0L)
  f <- tp_output(list(name = "ae_soc_pt", args = list(population = "ITT")), NULL)
  f$data <- list(source = "code", population = "SAF")
  expect_identical(nrow(tflspec:::.check_static_rows(f, "T1")), 0L)
  expect_error(tfl_resolve_output(f, tp_study()), "the set is written twice", fixed = TRUE)
  d <- withr::local_tempdir()
  file.copy(list.files(system.file("extdata", "spec-yaml", package = "tflspec"), full.names = TRUE), d,
            recursive = TRUE)
  tfl_write_output(mk(list(population = "ITT"), population = "SAF"), file.path(d, "outputs", "T-9.yml"))
  ck <- suppressWarnings(tfl_check_spec(d, output_id = "T-9", adam = FALSE))
  ck <- ck[ck$output_id == "T-9" & grepl("written twice", ck$message, fixed = TRUE), , drop = FALSE]
  expect_identical(ck$level, "error")
  expect_match(ck$message, "the template argument `population` (ITT) and the report's data (SAF)", fixed = TRUE)
  expect_match(ck$message, "write it once, on the report's data", fixed = TRUE)
})

test_that("demog's `population` parameter (A2-T Q2): given in either part, the same report", {
  s <- tp_study()
  ok <- tp_output(list(name = "demog", args = list(by = "TRT01A", continuous = "AGE")))
  r <- tfl_resolve_output(ok, s)
  expect_identical(r$ard$analysis_data[[1L]]$population, "SAF")
  for (key in c("args", "analysis")) {
    own <- tp_output(list(name = "demog", args = list(by = "TRT01A", continuous = "AGE")), population = NULL)
    own$template[[key]] <- c(own$template[[key]], list(population = "SAF"))
    r2 <- tfl_resolve_output(own, s)
    expect_identical(r2$ard, r$ard, info = key)
  }
})

test_that("the look as a template parameter is no warning, and no finding", {
  withr::defer(rm(list = "tp_wide", envir = tflspec:::.tpl_registry))
  expect_no_warning(tfl_template("tp_wide", kind = "table", layout = "summary",
    params = list(tfl_param("population", "SAF"), tfl_param("max_rows", 30L, type = "number")),
    body = function(a, ctx) {
      x <- tfl_template_expand("summary", list(by = "TRT01A", population = a$population, continuous = "AGE"))
      x$content <- c(x$content, list(tfl_item("plan_paginate_rows", list(max_rows = a$max_rows))))
      x
    }))
  expect_identical(setdiff(tfl_test_templates("tp_wide")$status, "no subset"), character(0))
  # an unknown parameter is still found
  expect_error(tfl_template_variant("tp_bad", from = "tp_wide", defaults = list(colour = 1)),
               "no parameter 'colour'")
})
