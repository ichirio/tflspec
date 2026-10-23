# A table's verb called twice is one call when read: a later call's
# arguments win, key by key; plan_hide()'s columns add up; the verbs whose
# calls are layers of their own are left; a figure is not touched

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

test_that("read from a file: a table's twice is once; a figure's content as it is", {
  d <- withr::local_tempdir()
  f <- file.path(d, "T-1.yml")
  writeLines(c("id: T-1", "kind: table", "content:", "  - table_plan: {cols: TRT01A}",
               "  - plan_labels: {AGE: Age}", "  - plan_labels: {SEX: Sex}"), f)
  expect_message(o <- tfl_read_output(f), "called more than once")
  expect_identical(vapply(o$content, `[[`, "", "fn"), c("table_plan", "plan_labels"))
  expect_identical(o$content[[2L]]$args, list(AGE = "Age", SEX = "Sex"))
  g <- file.path(d, "F-1.yml")
  writeLines(c("id: F-1", "kind: figure", "content:", "  - geom_line: {}", "  - geom_line: {}"), g)
  expect_silent(fo <- tfl_read_output(g))
  expect_length(fo$content, 2L)
})
