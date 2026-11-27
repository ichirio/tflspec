# The old forms of the SPEC (R/spec_old_forms.R): one table, one classed
# stop (canon 11-10 :377-:379, 11-12 :380, :433).

test_that("an old form stops with a classed condition that names the form and the place", {
  e <- tryCatch(tflspec:::.old_form_stop("item_fn_map", "T-1.yml"), error = function(e) e)
  expect_s3_class(e, "tflspec_old_form")
  expect_identical(e$form, "item_fn_map")
  expect_identical(e$where, "T-1.yml")
  expect_match(conditionMessage(e), "^T-1.yml: a content item written as a map .* is no longer read: write it")
  # (a place that knows better says its own text)
  e <- tryCatch(tflspec:::.old_form_stop("titles_at", "S", was = "X", instead = "Y."), error = function(e) e)
  expect_identical(conditionMessage(e), "S: X is no longer read: Y.")
  expect_error(tflspec:::.old_form_stop("nope", "S"), "no old form nope")
})

test_that("the old forms' table: one row a form, each with what to write instead and the canon's line", {
  f <- tflspec:::.old_forms()
  expect_false(anyDuplicated(f$id) > 0L)
  expect_true(all(nzchar(f$was) & nzchar(f$instead) & nzchar(f$canon)))
  expect_true(all(c("spec_version", "patterns_dir", "extends_pattern", "item_fn_map", "titles_at",
                    "footnotes_at", "population", "defaults_page", "series_settings") %in% f$id))
  l <- tflspec:::.old_form_lines()
  expect_length(l, nrow(f) - 1L)
  expect_true(all(startsWith(l, "- ")))
  expect_false(any(grepl("no `spec_version`", l, fixed = TRUE)))
})

test_that("a key not in a closed list stops naming the closest one, else the list", {
  expect_identical(tflspec:::.near_name("contnet", c("content", "codelists")), "content")
  expect_identical(tflspec:::.near_name("zzz", c("content", "codelists")), NA_character_)
  expect_identical(tflspec:::.near_name("x", character()), NA_character_)
  expect_error(tflspec:::.spec_key_stop("contnet", c("content", "page"), "T-1"),
               "T-1: `contnet:` is not a key; did you mean `content:`\\?")
  expect_error(tflspec:::.spec_key_stop("zzz", c("content", "page"), "T-1", "a report key"),
               "`zzz:` is not a report key; the keys are `content`, `page`.")
})

test_that("study.yml series_settings: stops in both readers, as an old form", {
  d <- withr::local_tempdir()
  write_study_yml(d, c("study_id: S-1", "series_settings:", "  demo_v1: {max_rows: 25}"))
  expect_error(tfl_read_study_spec(d), "demo_v1: move these values into", class = "tflspec_old_form")
  expect_error(tflspec:::.tr_study(d, "tfl_tpl()"), "demo_v1: move these values into",
               class = "tflspec_old_form")
  expect_error(tflspec:::.tr_study(list(series_settings = list(a_v1 = list())), "tfl_tpl()"),
               "a_v1: move", class = "tflspec_old_form")
})

test_that("defaults.yml page: stops as an old form", {
  d <- withr::local_tempdir()
  write_study_yml(d, "study_id: S-1")
  dir.create(file.path(d, "spec"))
  writeLines(c("page:", "  header:", "    1: {left: X}"), file.path(d, "spec", "defaults.yml"))
  e <- tryCatch(tfl_read_study_spec(d), error = function(e) e)
  expect_s3_class(e, "tflspec_old_form")
  expect_identical(e$form, "defaults_page")
})

test_that("#11 a content item written as a {fn:, args:, from:} map stops (canon :378)", {
  d <- withr::local_tempdir()
  f <- file.path(d, "T-1.yml")
  writeLines(c("id: T-1", "kind: table", "content:", "  - fn: plan_cells",
               "    args: {categorical: '{n}'}"), f)
  e <- tryCatch(tfl_read_output(f), error = function(e) e)
  expect_s3_class(e, "tflspec_old_form")
  expect_identical(e$form, "item_fn_map")
  expect_match(conditionMessage(e), "T-1.yml: a content item written as a map `{fn: plan_cells",
               fixed = TRUE)
  expect_match(conditionMessage(e), "write it `- plan_cells: {<args>}`", fixed = TRUE)
  # (with `from:` too, and in a layer: the same)
  writeLines(c("id: T-1", "kind: table", "content:", "  - {fn: plan_cells, from: defaults}"), f)
  expect_error(tfl_read_output(f), class = "tflspec_old_form")
  expect_error(tflspec:::.layer_from_list(list(content = list(list(fn = "plan_cells"))), "defaults"),
               class = "tflspec_old_form")
  # the current form reads, and an item made in code is taken as it is
  writeLines(c("id: T-1", "kind: table", "content:", "  - plan_cells: {categorical: '{n}'}"), f)
  expect_identical(tfl_read_output(f)$content[[1]]$fn, "plan_cells")
  o <- tflspec:::.output_from_list(list(id = "T-2", kind = "table",
                                        content = list(tfl_item("plan_cells", list(categorical = "{n}")))))
  expect_identical(o$content[[1]]$args$categorical, "{n}")
  # (a call's value {fn, args} is a nested call, not an item: untouched)
  writeLines(c("id: T-1", "kind: table", "content:",
               "  - plan_style: {border_header: {fn: rtf_border, args: {top: true}}}"), f)
  expect_s3_class(tfl_read_output(f)$content[[1]]$args$border_header, "tfl_call")
})
