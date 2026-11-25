# A figure's content object in the map form `{name, from, calls}` (OBJECTS.md
# B-4): reading, the round trip, the read stops, the checks of `from:`, and
# the code (no writer for `from:` before B-5).

co_calls <- function() list(tfl_item("ggplot", list()), tfl_item("geom_point", list()))

co_output <- function(...) tfl_output("F-1", "figure", content = list(...))

co_roundtrip <- function(o) {
  f <- tempfile(fileext = ".yml")
  on.exit(unlink(f), add = TRUE)
  tfl_write_output(o, f)
  list(lines = readLines(f), back = tfl_read_output(f))
}

# the report's file, as text, read
co_read_text <- function(txt) {
  f <- tempfile(fileext = ".yml")
  on.exit(unlink(f), add = TRUE)
  writeLines(txt, f)
  tfl_read_output(f)
}

test_that("tfl_plot() has input and form, and checks them", {
  p <- tfl_plot("km", co_calls())
  expect_identical(p$form, "short")
  expect_null(p[["input"]])
  q <- tfl_plot("km", co_calls(), input = "adsl", form = "map")
  expect_identical(q$input, "adsl")
  expect_identical(q$form, "map")
  expect_identical(q$from, "own")
  expect_error(tfl_plot("km", co_calls(), input = c("a", "b")), "input")
  expect_error(tfl_plot("km", co_calls(), input = 1), "input")
  expect_error(tfl_plot("km", co_calls(), input = "a b"), "input")
  expect_error(tfl_plot("km", co_calls(), form = "long"), "should be one of")
})

test_that("both forms read and write back as they are, in one report", {
  o <- co_output(
    tfl_plot("km", co_calls(), input = "adsl", form = "map"),
    tfl_plot("risk", co_calls(), form = "map"),
    tfl_plot("old", co_calls()))
  r <- co_roundtrip(o)
  y <- r$lines
  expect_true(any(y == "  - name: km"))
  expect_true(any(y == "    from: adsl"))
  expect_true(any(y == "    calls:"))
  expect_true(any(y == "  - old:"))
  b <- r$back$content
  expect_identical(vapply(b, function(it) it$form, ""), c("map", "map", "short"))
  expect_identical(b[[1L]]$input, "adsl")
  expect_null(b[[2L]][["input"]])
  expect_identical(vapply(b, function(it) it$fn, ""), c("km", "risk", "old"))
  expect_identical(b[[1L]]$items[[2L]]$fn, "geom_point")
  # read -> write -> read keeps each form, and the text is the same
  r2 <- co_roundtrip(r$back)
  expect_identical(r2$lines, y)
  expect_identical(r2$back$content, b)
  # the order of the keys written: name, from, calls
  expect_identical(match(c("  - name: km", "    from: adsl", "    calls:"), y),
                   sort(match(c("  - name: km", "    from: adsl", "    calls:"), y)))
})

test_that("the short form is read as before, and `- name: {args}` is no object map", {
  p <- .item_from_list(list(km = list(list(ggplot = list()), list(geom_point = list()))))
  expect_s3_class(p, "tfl_plot")
  expect_identical(p$form, "short")
  it <- .item_from_list(list(plan_x = list(a = 1)))
  expect_false(inherits(it, "tfl_plot"))
  expect_identical(it$fn, "plan_x")
  expect_false(.is_obj_map(list(plan_x = list(a = 1))))
  expect_false(.is_obj_map(list(fn = "plan_x", args = list())))
  expect_true(.is_obj_map(list(name = "km", calls = list())))
  expect_true(.is_obj_map(list(name = "km", from = "adsl")))
  expect_true(.is_obj_map(list(calls = list(), other = 1)))
})

test_that("the four read stops", {
  one <- list(list(ggplot = list()))
  # no calls, or empty calls
  expect_error(.item_from_list(list(name = "km", from = "adsl")), "`km`.*needs `calls:`")
  expect_error(.item_from_list(list(name = "km", calls = list())), "`km`.*needs `calls:`")
  expect_error(.item_from_list(list(name = "km", calls = NULL)), "needs `calls:`")
  # a key other than name / from / calls
  expect_error(.item_from_list(list(name = "km", calls = one, data = "x")),
               "name, from, calls.*`data`")
  # no name
  expect_error(.item_from_list(list(from = "adsl", calls = one)),
               "without `name:` is not supported yet: it comes with B-5")
  # the same, through a file
  expect_error(co_read_text(c("id: F-1", "kind: figure", "content:", "  - name: km", "    from: adsl")),
               "needs `calls:`")
  expect_error(co_read_text(c("id: F-1", "kind: figure", "content:", "  - name: km", "    calls:",
                              "      - ggplot: {}", "    data: x")),
               "name, from, calls")
  expect_error(co_read_text(c("id: F-1", "kind: figure", "content:", "  - from: adsl", "    calls:",
                              "      - ggplot: {}")),
               "B-5")
})

test_that("`from:` names an object made above, or a study dataset", {
  ok <- co_output(tfl_plot("a", co_calls(), form = "map"),
                  tfl_plot("b", list(tfl_item("geom_point", list())), input = "a", form = "map"))
  expect_equal(nrow(.check_object_rows(ok, "F-1")), 0L)
  # the objects list has the use
  u <- .obj_table(ok)$uses
  expect_true(any(u$name == "a" & u$arg == "from"))
  # made below
  below <- co_output(tfl_plot("b", list(tfl_item("geom_point", list())), input = "a", form = "map"),
                     tfl_plot("a", co_calls(), form = "map"))
  r <- .check_object_rows(below, "F-1")
  expect_true(any(r$level == "error" & grepl("made below it", r$message)))
  # undefined
  und <- co_output(tfl_plot("b", list(tfl_item("geom_point", list())), input = "nothing", form = "map"))
  r <- .check_object_rows(und, "F-1")
  expect_true(any(r$level == "error" & grepl("`nothing`, which nothing above", r$message)))
  # a study dataset is fine
  ds <- co_output(tfl_plot("b", list(tfl_item("geom_point", list())), input = "ADSL", form = "map"))
  expect_equal(nrow(.check_object_rows(ds, "F-1", datasets = "ADSL")), 0L)
  expect_true(any(.check_object_rows(ds, "F-1")$level == "error"))
})

test_that("a rename follows `from:`", {
  o <- co_output(tfl_plot("a", co_calls(), form = "map"),
                 tfl_plot("b", list(tfl_item("geom_point", list())), input = "a", form = "map"))
  n <- tfl_output_rename(o, "a", "first")
  expect_identical(n$content[[1L]]$fn, "first")
  expect_identical(n$content[[2L]]$input, "first")
})

test_that("B-7: a call that writes the argument the pipe fills is an error", {
  d <- tfl_fn_defs()
  first <- d$first_arg_name[d$fn == "ggplot"]
  expect_false(is.na(first))
  args <- stats::setNames(list("df"), first)
  bad <- co_output(tfl_plot("a", list(tfl_item("ggplot", args)), input = "ADSL", form = "map"))
  r <- .check_object_rows(bad, "F-1", datasets = "ADSL")
  expect_true(any(r$level == "error" & grepl("the value and the pipe disagree", r$message)))
  # the same call without `from:`, and `from:` without the argument: no row
  none <- co_output(tfl_plot("a", list(tfl_item("ggplot", args)), form = "map"))
  expect_equal(nrow(.check_object_rows(none, "F-1")), 0L)
  fine <- co_output(tfl_plot("a", list(tfl_item("ggplot", list())), input = "ADSL", form = "map"))
  expect_equal(nrow(.check_object_rows(fine, "F-1", datasets = "ADSL")), 0L)
})

test_that("code: `from:` stops, a map object without it is written as its short twin", {
  withfrom <- co_output(tfl_plot("km", co_calls(), input = "adsl", form = "map"))
  expect_error(.fig_content_code(withfrom),
               "`from:` on a content object \\(km\\) is not written into the program yet: it comes with B-5")
  expect_error(tfl_content_code(withfrom), "B-5")
  m <- co_output(tfl_plot("km", co_calls(), form = "map"),
                 tfl_plot("risk", co_calls(), form = "map"),
                 tfl_item("wrap_plots", list(plots = c("km", "risk"))))
  s <- co_output(tfl_plot("km", co_calls()), tfl_plot("risk", co_calls()),
                 tfl_item("wrap_plots", list(plots = c("km", "risk"))))
  expect_identical(.fig_content_code(m), .fig_content_code(s))
  # through a file too
  expect_identical(.fig_content_code(co_roundtrip(m)$back), .fig_content_code(s))
})
