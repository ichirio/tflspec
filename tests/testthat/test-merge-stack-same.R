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
