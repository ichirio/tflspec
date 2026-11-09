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

# ---- `!unset` and the checks of a parameter's value ----------------------------

us_lcols <- list(list(vars = "USUBJID", label = "Subject"), list(vars = "AEDECOD", label = "Event"))
us_sbc <- list(list(label = "n", template = "{n}"))

# every choice parameter a body checks: the case that reaches the check
us_choice <- list(
  list("listing", list(data = "ADAE", columns = us_lcols, by = "AESEV"), "by_break"),
  list("ancova", list(population = "SAF", data = "ADVS", param = "SYSBP", visit = "CYCLE 2 DAY 1"), "adjust"),
  list("ae_soc_pt", list(population = "SAF", min_pct = 5), "min_pct_in"),
  list("ae_soc_pt", list(population = "SAF"), "header_n"),
  list("ae_pt_freq", list(population = "SAF"), "min_pct_in"),
  list("ae_pt_freq", list(population = "SAF"), "header_n"),
  list("ae_max_grade", list(population = "SAF"), "grade_in"),
  list("ae_max_grade", list(population = "SAF"), "sort"),
  list("ae_max_grade", list(population = "SAF"), "header_n"),
  list("ae_overview", list(population = "SAF"), "header_n"),
  list("param_change", list(population = "SAF", data = "ADVS"), "layout"),
  list("orr", list(responders = c("CR", "PR", "SD")), "stat_layout"),
  list("orr", list(responders = c("CR", "PR", "SD")), "header_n"),
  list("shift", list(population = "SAF"), "header_n"),
  list("lab_shift", list(population = "SAF", visit = "CYCLE 2 DAY 1", params = "ALT"), "header_n"),
  list("lab_abnormal", list(population = "SAF"), "header_n"))

test_that("a choice check lets `!unset` through to a message that names the parameter", {
  for (cs in us_choice) {
    args <- c(cs[[2L]], stats::setNames(list(tfl_unset()), cs[[3L]]))
    msg <- tryCatch({
      tfl_template_expand(cs[[1L]], args)
      "no error"
    }, error = function(e) conditionMessage(e))
    info <- paste(cs[[1L]], cs[[3L]])
    expect_match(msg, sprintf("%s: `%s` cannot be left unset (!unset).", cs[[1L]], cs[[3L]]),
                 fixed = TRUE, info = info)
    # (not a choice error, and not a raw R error)
    expect_no_match(msg, "is 'page'|takes|must be one of|'arg'|should be one of|is \"page\"", info = info)
  }
})

test_that("the same parameters with a value, or left out, work as before", {
  for (cs in us_choice) {
    expect_no_error(tfl_template_expand(cs[[1L]], cs[[2L]]))
  }
  x <- tfl_template_expand("listing", list(data = "ADAE", columns = us_lcols, by = "AESEV",
                                            by_break = "block"))
  expect_identical(x$unset, character())
  # a wrong value is still a wrong value
  expect_error(tfl_template_expand("listing", list(data = "ADAE", columns = us_lcols, by = "AESEV",
                                                    by_break = "x")), "`by_break` is")
  expect_error(tfl_template_expand("ancova", list(population = "SAF", data = "ADVS", param = "SYSBP",
                                                   visit = "V", adjust = "x")), "must be one of")
  # an unset parameter that is not read stays harmless: min_pct_in with no min_pct
  expect_no_error(tfl_template_expand("ae_soc_pt", list(population = "SAF",
                                                         min_pct_in = tfl_unset())))
})

# ---- tfl_item_own() with `!unset` arguments --------------------------------------

test_that("tfl_item_own() keeps an `!unset` argument of a fixed-argument verb as the report's own", {
  expect_false(tflspec:::.dots_named("plan_blanks"))
  below <- tfl_item("plan_blanks", list(where = "between_groups", first = TRUE))
  un <- function(...) tfl_item("plan_blanks", list(...))
  # an unset argument over a value: own, unset
  o <- tfl_item_own(un(where = "between_groups", first = tfl_unset()), below)
  expect_identical(names(o$args), "first")
  expect_s3_class(o$args$first, "tfl_unset")
  expect_identical(o$from, "own")
  # the rest is compared as before: an argument that differs is own too
  o2 <- tfl_item_own(un(where = "after_group", first = tfl_unset()), below)
  expect_identical(o2$args$where, "after_group")
  expect_s3_class(o2$args$first, "tfl_unset")
  # an argument the same as below is not own
  expect_false("where" %in% names(o$args))
  # nothing below: the item as it is
  o3 <- tfl_item_own(un(first = tfl_unset()), NULL)
  expect_s3_class(o3$args$first, "tfl_unset")
  # an unset argument below does not matter to the comparison
  o4 <- tfl_item_own(un(where = "between_groups", first = tfl_unset()),
                     tfl_item("plan_blanks", list(where = "between_groups")))
  expect_identical(names(o4$args), "first")
  # no unset: as before (NULL when it is the same)
  expect_null(tfl_item_own(un(where = "between_groups", first = TRUE), below))
})

test_that("tfl_item_own() keeps an `!unset` argument of a named-arguments verb", {
  expect_true(tflspec:::.dots_named("plan_cells"))
  below <- tfl_item("plan_cells", list(default = "{n}", continuous = "{mean}"))
  o <- tfl_item_own(tfl_item("plan_cells", list(default = "{n}", continuous = tfl_unset())), below)
  expect_identical(names(o$args), "continuous")
  expect_s3_class(o$args$continuous, "tfl_unset")
  # unset where nothing is below it: still the report's own
  o2 <- tfl_item_own(tfl_item("plan_cells", list(default = "{n}", categorical = tfl_unset())), below)
  expect_identical(names(o2$args), c("categorical", "continuous"))
  expect_s3_class(o2$args$categorical, "tfl_unset")
  # an argument below the item does not have is unset, as before
  o3 <- tfl_item_own(tfl_item("plan_cells", list(default = "{n}")), below)
  expect_identical(names(o3$args), "continuous")
  expect_s3_class(o3$args$continuous, "tfl_unset")
})

# ---- the program writes `!unset` readably; inner calls leave it out ----------

us_unset_output <- function() {
  tfl_output("T-9", "table", population = "SAF", template = list(name = "summary",
    args = list(continuous = "AGE", cells = tfl_unset(), sd_missing = tfl_unset(),
                total = tfl_unset())))
}

test_that("a template's call writes `!unset` as tflspec::tfl_unset(), tfl_unset() when attached", {
  s <- us_study()
  r <- tfl_resolve_output(us_unset_output(), s)
  code <- paste(tfl_content_code(r), collapse = "\n")
  expect_match(code, "cells = tflspec::tfl_unset()", fixed = TRUE)
  expect_match(code, "sd_missing = tflspec::tfl_unset()", fixed = TRUE)
  expect_no_match(code, "structure(", fixed = TRUE)
  expect_no_match(code, "class = ", fixed = TRUE)
  withr::local_options(tflspec.attached = "tflspec")
  att <- paste(tfl_content_code(r), collapse = "\n")
  expect_match(att, "cells = tfl_unset()", fixed = TRUE)
  expect_no_match(att, "tflspec::", fixed = TRUE)
  # the written program is R that makes the same value
  e <- str2lang("tfl_unset()")
  expect_s3_class(eval(e), "tfl_unset")
})

test_that("a figure template's call writes `!unset` the same way", {
  o <- tfl_output("F-9", "figure", data = list(steps = list(
    list(step = "read", dataset = "ADTTE"), list(step = "param", value = "OS"))),
    template = list(name = "km_survival", args = list(by = "TRT01A", title = tfl_unset())))
  r <- tfl_resolve_output(o, us_study())
  code <- paste(tfl_content_code(r), collapse = "\n")
  expect_match(code, "title = tflspec::tfl_unset()", fixed = TRUE)
  expect_no_match(code, "structure(", fixed = TRUE)
})

us_has_unset <- function(x) {
  inherits(x, "tfl_unset") || (is.list(x) && any(vapply(x, us_has_unset, NA)))
}

test_that("expanded inner calls leave an `!unset` argument out", {
  r <- tfl_resolve_output(us_unset_output(), us_study())
  # the items: no unset, no argument
  expect_false(any(vapply(r$content, us_has_unset, NA)))
  fns <- vapply(r$content, `[[`, "", "fn")
  expect_false("plan_total" %in% fns)
  cells <- Filter(function(i) identical(i$fn, "plan_cells"), r$content)[[1L]]
  expect_false("continuous" %in% names(cells$args))
  items <- paste(tfl_content_code(r, template = "items"), collapse = "\n")
  expect_no_match(items, "unset", fixed = TRUE)
  expect_no_match(items, "structure(", fixed = TRUE)
  expect_no_match(items, "tfl_tpl", fixed = TRUE)
  # the ARD's analyses
  x <- tfl_template_expand("ae_soc_pt", list(population = "SAF", min_pct = 5,
                                             where = tfl_unset(), total = tfl_unset()))
  expect_false(us_has_unset(x$ard))
  expect_false(us_has_unset(x$content))
  expect_null(x$ard$analyses[[1L]]$where)
  expect_false("plan_total" %in% vapply(x$content, `[[`, "", "fn"))
  expect_setequal(x$unset, c("where", "total"))
})
