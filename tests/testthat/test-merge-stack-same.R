test_that("a report that repeats a stacking item of its layers updates it, not a second one", {
  ap <- function(name, ...) tfl_item("plan_append", list(name = name, ...))
  base <- list(tfl_item("table_plan", list()), ap("summary", absent = "omit"))
  # the same part again (a saved report that repeats its template's): one part
  out <- tflspec:::.merge_content(base, list(ap("summary", absent = "omit")))
  expect_identical(sum(vapply(out, function(i) identical(i$fn, "plan_append"), NA)), 1L)
  # the same name, another argument: one part, updated by argument
  out <- tflspec:::.merge_content(base, list(ap("summary", absent = "keep")))
  ap_out <- Filter(function(i) identical(i$fn, "plan_append"), out)
  expect_length(ap_out, 1L)
  expect_identical(ap_out[[1L]]$args$absent, "keep")
  # another name: added after the layers' part
  out <- tflspec:::.merge_content(base, list(ap("other")))
  ap_out <- Filter(function(i) identical(i$fn, "plan_append"), out)
  expect_identical(vapply(ap_out, function(i) i$args$name, ""), c("summary", "other"))
  # a cell style repeated with the same arguments: one; a new one: added
  cs <- function(b) tfl_item("plan_cell_style", list(where = "a", bold = b))
  out <- tflspec:::.merge_content(list(cs(TRUE)), list(cs(TRUE)))
  expect_length(out, 1L)
  out <- tflspec:::.merge_content(list(cs(TRUE)), list(cs(FALSE)))
  expect_length(out, 2L)
})

test_that("a template with a part (pk_conc) and a report that writes the same part: one part", {
  st <- tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))
  o <- tfl_output("X-PK", "table", label = "pk", population = "SAF",
                  template = list(name = "pk_conc", args = list(population = "SAF")))
  parts <- function(r) Filter(function(i) identical(i$fn, "plan_append"), r$content)
  expect_length(parts(tfl_resolve_output(o, st)), 1L)
  # the report's own card for the part, as tflplanner writes it (other arguments)
  o$content <- list(tfl_item("plan_append", list(name = "summary", data = list(source = "ard"))))
  p <- parts(tfl_resolve_output(o, st))
  expect_length(p, 1L)
  expect_identical(p[[1L]]$args$name, "summary")
  expect_false(is.null(p[[1L]]$args$content))
})

test_that("an !unset argument is dropped from every resolved item, with or without items below", {
  has_unset <- function(content) {
    any(vapply(content, function(it) any(vapply(it$args, inherits, NA, "tfl_unset")), NA))
  }
  tp <- function() tfl_item("table_plan", list(cols = "TRT", show_keys = tfl_unset()))
  # (a) the layers have items, none of table_plan
  below <- list(tfl_item("plan_style", list(bold = TRUE), from = "defaults"))
  out <- tflspec:::.merge_content(below, list(tp()))
  tpo <- Filter(function(it) identical(it$fn, "table_plan"), out)[[1L]]
  expect_false(has_unset(out))
  expect_identical(names(tpo$args), "cols")
  expect_no_error(code <- tfl_item_code(tpo))
  expect_false(any(grepl("show_keys", code)))
  # (b) nothing below
  out <- tflspec:::.merge_content(list(), list(tp()))
  expect_false(has_unset(out))
  expect_identical(names(out[[1L]]$args), "cols")
  out <- tflspec:::.merge_content(NULL, list(tp()))
  expect_false(has_unset(out))
  # an !r content below is left alone
  r <- tfl_r("table_plan()")
  expect_identical(tflspec:::.merge_content(r, list()), r)
  # (c) a verb only the report has (plan_hide stacks; plan_style is "arg")
  hide <- tfl_item("plan_hide", list(where = "a", bold = tfl_unset()))
  out <- tflspec:::.merge_content(below, list(hide))
  expect_false(has_unset(out))
  expect_identical(Filter(function(it) identical(it$fn, "plan_hide"), out)[[1L]]$args$where, "a")
  # the unset still removes an argument of an item below
  out <- tflspec:::.merge_content(
    list(tfl_item("plan_style", list(bold = TRUE, font = "Arial"))),
    list(tfl_item("plan_style", list(bold = tfl_unset()))))
  expect_identical(names(out[[1L]]$args), "font")
})
