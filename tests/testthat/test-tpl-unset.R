# `!unset` for template parameters (migration batch 0c): a template's default
# or a report's value that leaves the argument out of the inner call

us_study <- function() tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))

# a test-only template: `ind` goes straight into an inner call, `lab` has an
# `!unset` default, `mr` a value
us_register <- function(name) {
  tflspec:::.tfl_template(name, "table", "summary",
    params = list(tfl_param("population", "SAF", type = "text"),
                  tfl_param("ind", 2, type = "number", doc = "plan_stub(indent = )"),
                  tfl_param("lab", tfl_unset(), type = "text", doc = "plan_stub(label = )"),
                  tfl_param("mr", 30, type = "number", doc = "plan_paginate_rows(max_rows = )")),
    body = function(a, ctx) {
      x <- tfl_template_expand("summary", list(population = a$population, continuous = "AGE"),
                               codelists = ctx$codelists, population = ctx$population)
      x$content <- c(x$content, list(
        tfl_item("plan_stub", list(indent = a$ind, label = a$lab)),
        tfl_item("plan_paginate_rows", list(max_rows = a$mr))))
      x$given <- ctx$given
      x
    },
    example = list(population = "SAF"))
}
us_item <- function(x, fn) {
  it <- Filter(function(i) identical(i$fn, fn), x$content)
  it[[length(it)]]
}
us_code <- function(x, fn) paste(tfl_item_code(us_item(x, fn)), collapse = "
")

test_that("a template's `!unset` default: the body reads it as absent, the call omits it", {
  us_register("us_t")
  withr::defer(rm("us_t", envir = tflspec:::.tpl_registry))
  x <- tfl_template_expand("us_t", list(population = "SAF"))
  expect_identical(x$unset, "lab")
  expect_null(us_item(x, "plan_stub")$args$label)
  code <- us_code(x, "plan_stub")
  expect_match(code, "indent = 2", fixed = TRUE)
  expect_no_match(code, "label")
  expect_false("lab" %in% x$given)
  # a value over it is used
  y <- tfl_template_expand("us_t", list(population = "SAF", lab = "Age"))
  expect_identical(y$unset, character())
  expect_identical(us_item(y, "plan_stub")$args$label, "Age")
  expect_match(us_code(y, "plan_stub"), "label", fixed = TRUE)
})

test_that("a report's `!unset` over a default omits the argument; NA and NULL stay distinct", {
  us_register("us_t")
  withr::defer(rm("us_t", envir = tflspec:::.tpl_registry))
  ex <- function(args) tfl_template_expand("us_t", c(list(population = "SAF"), args))
  d <- ex(list())
  expect_match(us_code(d, "plan_paginate_rows"), "max_rows = 30", fixed = TRUE)
  u <- ex(list(mr = tfl_unset()))
  expect_no_match(us_code(u, "plan_paginate_rows"), "max_rows")
  expect_setequal(u$unset, c("mr", "lab"))
  expect_false("mr" %in% u$given)
  # NA is a value: written
  n <- ex(list(mr = NA))
  expect_match(us_code(n, "plan_paginate_rows"), "max_rows = NA", fixed = TRUE)
  expect_identical(n$unset, "lab")
  # NULL is "none": given, not recorded as unset
  nl <- ex(list(mr = NULL))
  expect_identical(nl$unset, "lab")
  expect_true("mr" %in% nl$given)
  expect_false(identical(nl$given, u$given))
  # the summary's `cells` unset: no continuous cells in the call
  s <- tfl_template_expand("summary", list(population = "SAF", continuous = "AGE", cells = tfl_unset()))
  expect_identical(s$unset, "cells")
  expect_false("plan_cells" %in% vapply(s$content, `[[`, "", "fn"))
})

test_that("a required parameter left `!unset` is still needed", {
  expect_error(tfl_template_expand("summary", list(population = tfl_unset(), continuous = "AGE")),
               "needs 'population'")
})

test_that("a SPEC's `!unset` (flat args:, ard:, content:) reaches the body as absent", {
  us_register("us_t")
  withr::defer(rm("us_t", envir = tflspec:::.tpl_registry))
  s <- us_study()
  for (template in list(list(name = "us_t", args = list(mr = tfl_unset())),
                        list(name = "us_t", ard = list(mr = tfl_unset())),
                        list(name = "us_t", content = list(mr = tfl_unset())))) {
    o <- tfl_output("T-9", "table", population = "SAF", template = template)
    r <- tfl_resolve_output(o, s)
    expect_s3_class(attr(r, "template")$args$mr, "tfl_unset")
    expect_identical(Filter(function(i) identical(i$fn, "plan_paginate_rows"), r$content)[[1L]]$args$max_rows, 30L)
  }
})

test_that("a study value or fallback that is `!unset` means omit", {
  tflspec:::.tfl_template("us_s", "table", "summary",
    params = list(tfl_param("population", "SAF", type = "text"),
                  tfl_param("mr", 30, type = "number", study = "settings.mr"),
                  tfl_param("ind", tfl_unset(), type = "number", study = "settings.ind")),
    body = function(a, ctx) {
      x <- tfl_template_expand("summary", list(population = a$population, continuous = "AGE"),
                               codelists = ctx$codelists, population = ctx$population)
      x$content <- c(x$content, list(tfl_item("plan_stub", list(indent = a$ind))))
      x$seen <- a
      x
    }, example = list(population = "SAF"))
  withr::defer(rm("us_s", envir = tflspec:::.tpl_registry))
  s <- us_study()
  s$study$settings$mr <- tfl_unset()
  o <- tfl_output("T-9", "table", population = "SAF", template = list(name = "us_s"))
  # (the fallback `!unset` is omitted; the study's `!unset` is omitted)
  x <- tflspec:::.tpl_expand_for(o, s, list(), NULL, warn = FALSE)
  expect_null(x$x$seen$ind)
  expect_null(x$x$seen$mr)
  expect_setequal(x$x$unset, c("ind", "mr"))
  # the study's value over the `!unset` fallback is written
  s$study$settings$ind <- 4
  x2 <- tflspec:::.tpl_expand_for(o, s, list(), NULL, warn = FALSE)
  expect_identical(x2$x$seen$ind, 4)
  expect_identical(us_item(x2$x, "plan_stub")$args$indent, 4)
})

test_that("the help shows an `!unset` default as (none); the kit says what it does", {
  us_register("us_t")
  withr::defer(rm("us_t", envir = tflspec:::.tpl_registry))
  a <- tfl_tpl_args("us_t")
  expect_identical(a$default[a$param == "lab"], "(none)")
  expect_identical(a$effective[a$param == "lab"], "(none)")
  expect_identical(a$default[a$param == "mr"], "30")
  txt <- paste(tflspec:::.ai_templates_list("table"), collapse = "\n")
  expect_match(txt, "`lab` (text) = !unset", fixed = TRUE)
  expect_match(tflspec:::.ai_templates_text("table"),
               "`!unset` (a default, or a value you write): leave the argument out of the call.",
               fixed = TRUE)
  # no built-in template's default is `!unset`
  for (n in setdiff(tfl_templates()$name, "us_t")) {
    t <- tflspec:::.tpl_get(n, "t")
    expect_false(any(vapply(t$params, function(p) inherits(p$default, "tfl_unset"), NA)), info = n)
  }
})

test_that("tfl_check_spec() accepts `!unset` on a template argument", {
  d <- withr::local_tempdir()
  file.copy(list.files(system.file("extdata", "spec-yaml", package = "tflspec"), full.names = TRUE),
            d, recursive = TRUE)
  yml <- file.path(d, "spec", "outputs")
  dir.create(yml, recursive = TRUE, showWarnings = FALSE)
  tfl_write_output(tfl_output("T-US", "table", population = "SAF", extends = "defaults",
    template = list(name = "summary", args = list(continuous = "AGE", cells = tfl_unset(),
                                                  sd_missing = tfl_unset()))),
    file.path(yml, "T-US.yml"))
  expect_match(paste(readLines(file.path(yml, "T-US.yml")), collapse = "\n"), "cells: !unset",
               fixed = TRUE)
  p <- suppressMessages(tfl_check_spec(d))
  expect_identical(nrow(p[p$level == "error", ]), 0L)
})
