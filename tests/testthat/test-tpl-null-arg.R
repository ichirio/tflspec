# A report's argument written `~` (YAML null) is not given: the template's
# default applies, as when it is left out.  None (not passed) is `!unset`;
# NULL itself is `!r NULL` (canon 2026-11-24).  The report read from YAML
# text, as tfl_template_resolve() reads a report file.  Fictional values only.

# a tflspec_hierarchy report file; `extra` more lines under `template:`
na_file <- function(extra = character(), env = parent.frame()) {
  f <- tempfile(fileext = ".yml")
  writeLines(c("id: X", "kind: table", "label: Records by hierarchy",
               "template:", "  name: tflspec_v1/tflspec_hierarchy", "  from: ADAE",
               "  by: [TRT01A]", "  hierarchy: [AEBODSYS, AEDECOD]", extra), f)
  withr::defer(unlink(f), envir = env)
  f
}
na_resolve <- function(extra = character()) tfl_template_resolve(na_file(extra))

na_fns <- function(o) vapply(o$content, function(it) .schema_fn_name(it$fn), "")
na_item <- function(o, fn) {
  k <- match(fn, na_fns(o))
  if (is.na(k)) NULL else o$content[[k]]$args
}

# the same resolved report: its data, analyses, content, args and origins
na_same <- function(a, b, info = NULL) {
  expect_identical(a$data, b$data, info = info)
  expect_identical(a$ard, b$ard, info = info)
  expect_identical(a$content, b$content, info = info)
  expect_identical(attr(a, "args"), attr(b, "args"), info = info)
  expect_identical(attr(a, "origins"), attr(b, "origins"), info = info)
}

test_that("`~` is read as YAML null: the name is there, its value NULL", {
  r <- .series_read_yaml(na_file("  heading: ~"))
  expect_true("heading" %in% names(r$template))
  expect_null(r$template$heading)
})

test_that("an argument written `~` is the same as one left out: heading, sort_column", {
  absent <- na_resolve()
  na_same(absent, na_resolve("  heading: ~"), "heading: ~")
  na_same(absent, na_resolve("  sort_column: ~"), "sort_column: ~")
  # (a step section reads the same)
  na_same(absent, na_resolve(c("  content:", "    heading: ~", "    sort_column: ~")), "content:")
  # given as R, NULL: the same
  r <- .series_read_yaml(na_file())
  r$template["heading"] <- list(NULL)
  na_same(absent, tfl_template_resolve(r), "R NULL")
  expect_identical(as.character(unlist(na_item(absent, "plan_sort")$keys)),
                   c(".overall", "-n@group1", "group1", ".depth", "-n", "label"))
})

test_that("`~` takes the default, not none: empty_text and any_row", {
  absent <- na_resolve()
  # (the default sentence: a plan_empty card -- before, `~` dropped it)
  expect_true("plan_empty" %in% na_fns(absent))
  e <- na_resolve("  empty_text: ~")
  na_same(absent, e, "empty_text: ~")
  expect_true("plan_empty" %in% na_fns(e))
  expect_identical(attr(e, "origins")[["empty_text"]], attr(absent, "origins")[["empty_text"]])
  na_same(absent, na_resolve("  any_row: ~"), "any_row: ~")
  # none is !unset: no plan_empty card, as before
  u <- na_resolve("  empty_text: !unset")
  expect_false("plan_empty" %in% na_fns(u))
  expect_identical(attr(u, "origins")[["empty_text"]], "report")
})

test_that("max_rows: `~` the default, !unset none, `!r NULL` passes NULL", {
  absent <- na_resolve()
  dflt <- na_item(absent, "plan_paginate_rows")$max_rows
  expect_false(is.null(dflt))
  expect_identical(na_item(na_resolve("  max_rows: ~"), "plan_paginate_rows")$max_rows, dflt)
  # !unset: the argument not passed
  u <- na_item(na_resolve("  max_rows: !unset"), "plan_paginate_rows")
  expect_false("max_rows" %in% names(u))
  # !r NULL: NULL passed, as R code
  n <- na_resolve("  max_rows: !r NULL")
  v <- na_item(n, "plan_paginate_rows")$max_rows
  expect_s3_class(v, "tfl_r")
  expect_identical(as.character(unclass(v)), "NULL")
  expect_s3_class(attr(n, "args")$max_rows, "tfl_r")
  expect_identical(attr(n, "origins")[["max_rows"]], "report")
})

test_that("a name written `~` that is no argument still stops", {
  expect_error(na_resolve("  no_such_arg: ~"), "has no argument no_such_arg", fixed = TRUE)
})
