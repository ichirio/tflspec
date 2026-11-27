# A template report's verb called twice (its template's items and the cards
# appended after them) is one call: a later call's arguments win, key by
# key; plan_hide()'s columns add up; the verbs whose calls are layers of
# their own are left; a figure is not touched.  A code SPEC file's own two
# cards of a verb that does not stack stop (PRINCIPLES 11-10, 1.13 / 1.32).

pr_items <- function(...) list(...)

test_that("last wins argument by argument, key by key; a message says so", {
  items <- pr_items(
    tfl_item("table_plan", list(cols = "TRT01A")),
    tfl_item("plan_labels", list(AGE = "Age", SEX = "Sex")),
    tfl_item("plan_style", list(font_size_half_points = 16L, row_height_twips = 240L)),
    tfl_item("plan_labels", list(SEX = "Sex, n (%)", RACE = "Race")),
    tfl_item("plan_style", list(font_size_half_points = 18L)))
  expect_message(out <- tflspec:::.plan_merge_repeats(items, "T-1"),
                 "plan_labels\\(\\), plan_style\\(\\) called more than once")
  expect_identical(vapply(out, `[[`, "", "fn"), c("table_plan", "plan_labels", "plan_style"))
  expect_identical(out[[2L]]$args, list(AGE = "Age", SEX = "Sex, n (%)", RACE = "Race"))
  expect_identical(out[[3L]]$args, list(font_size_half_points = 18L, row_height_twips = 240L))
})

test_that("plan_hide()'s columns add up; filters, cell styles, steps after are left", {
  items <- pr_items(
    tfl_item("plan_hide", list(cols = list("A"))), tfl_item("plan_hide", list(cols = list("B"))),
    tfl_item("plan_cell_style", list(cols = "A", bold = TRUE)),
    tfl_item("plan_cell_style", list(cols = "B", italic = TRUE)),
    tfl_item("plan_filter", list(any = TRUE)), tfl_item("plan_filter", list(any = FALSE)))
  out <- suppressMessages(tflspec:::.plan_merge_repeats(items))
  expect_identical(vapply(out, `[[`, "", "fn"),
                   c("plan_hide", "plan_cell_style", "plan_cell_style", "plan_filter", "plan_filter"))
  expect_identical(out[[1L]]$args, list(cols = list("A", "B")))
  # nothing to put together: no message
  expect_silent(tflspec:::.plan_merge_repeats(items[3:6]))
})

test_that("a code SPEC file with two cards of a verb that does not stack stops, naming it and the file", {
  d <- withr::local_tempdir()
  f <- file.path(d, "T-1.yml")
  writeLines(c("id: T-1", "kind: table", "content:", "  - table_plan: {cols: TRT01A}",
               "  - plan_labels: {AGE: Age}", "  - plan_labels: {SEX: Sex}"), f)
  e <- expect_error(tfl_read_output(f), "plan_labels() in more than one card", fixed = TRUE)
  expect_match(conditionMessage(e), "T-1 (", fixed = TRUE)
  expect_match(conditionMessage(e), f, fixed = TRUE)
  expect_match(conditionMessage(e), "put its arguments in one card", fixed = TRUE)
  # (an argument verb too: plan_style)
  writeLines(c("id: T-1", "kind: table", "content:", "  - table_plan: {cols: TRT01A}",
               "  - plan_style: {font_size_half_points: 16}",
               "  - plan_style: {font_size_half_points: 18}"), f)
  expect_error(tfl_read_output(f), "plan_style() in more than one card", fixed = TRUE)
  # the same in a listing
  writeLines(c("id: L-1", "kind: listing", "content:",
               "  - plan_paginate_rows: {max_rows: 20}", "  - plan_paginate_rows: {max_rows: 30}"),
             file.path(d, "L-1.yml"))
  expect_error(tfl_read_output(file.path(d, "L-1.yml")), "plan_paginate_rows() in more than one card",
               fixed = TRUE)
})

test_that("a code SPEC file: the verbs that stack may be written more than once", {
  d <- withr::local_tempdir()
  f <- file.path(d, "T-1.yml")
  writeLines(c("id: T-1", "kind: table", "content:", "  - table_plan: {cols: TRT01A}",
               "  - plan_cell_style: {cols: A, bold: true}",
               "  - plan_cell_style: {cols: B, italic: true}"), f)
  o <- tfl_read_output(f)
  expect_identical(vapply(o$content, `[[`, "", "fn"),
                   c("table_plan", "plan_cell_style", "plan_cell_style"))
  # plan_hide stacks: its columns still put together in one call
  writeLines(c("id: T-1", "kind: table", "content:", "  - table_plan: {cols: TRT01A}",
               "  - plan_hide: {cols: [A]}", "  - plan_hide: {cols: [B]}"), f)
  expect_message(o <- tfl_read_output(f), "plan_hide()'s columns added up", fixed = TRUE)
  expect_identical(vapply(o$content, `[[`, "", "fn"), c("table_plan", "plan_hide"))
})

test_that("a template report's appended cards of one verb: put together, a later one winning (no stop)", {
  d <- withr::local_tempdir()
  f <- file.path(d, "T-2.yml")
  writeLines(c("id: T-2", "kind: table", "template:", "  name: tflspec_v1/tflspec_summary",
               "content:", "  - plan_labels: {AGE: Age}", "  - plan_labels: {AGE: Age (years), SEX: Sex}"), f)
  expect_message(o <- tfl_read_output(f), "called more than once")
  expect_identical(vapply(o$content, `[[`, "", "fn"), "plan_labels")
  expect_identical(o$content[[1L]]$args, list(AGE = "Age (years)", SEX = "Sex"))
  # (the items with no file -- tfl_template_resolve()'s template items and
  # appended cards -- are put together too)
  items <- pr_items(tfl_item("plan_style", list(font_size_half_points = 16L)),
                    tfl_item("plan_style", list(font_size_half_points = 18L)))
  out <- suppressMessages(tflspec:::.plan_merge_repeats(items, "T-2"))
  expect_identical(out[[1L]]$args, list(font_size_half_points = 18L))
})

test_that("read from a file: a figure's content as it is", {
  d <- withr::local_tempdir()
  g <- file.path(d, "F-1.yml")
  writeLines(c("id: F-1", "kind: figure", "content:", "  - geom_line: {}", "  - geom_line: {}"), g)
  expect_silent(fo <- tfl_read_output(g))
  expect_length(fo$content, 2L)
})
