# `~` (YAML null) in a card is "not written" (PRINCIPLES 11-24, L463): the
# layer below or the function's default applies, as when the argument is
# left out -- in every path, the merges included.  `!unset` leaves the
# argument out whatever is below; `!r NULL` passes NULL.  One mechanism:
# tfl_item() drops a `~` argument (.drop_null()).  Fictional values only.

# a study whose defaults.yml has `defaults` cards, a report T-1 with `report` cards
tn_resolve <- function(defaults = character(), report = character(), env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  dir.create(file.path(d, "outputs"))
  writeLines(c("content:", "  - plan_stub: {name: row_label}", defaults), file.path(d, "defaults.yml"))
  writeLines(c("id: T-1", "kind: table", "content:", "  - table_plan: {cols: ARM}", report),
             file.path(d, "outputs", "T-1.yml"))
  s <- tfl_read_study_spec(d)
  tfl_resolve_output(tfl_read_outputs(d)[["T-1"]], s)
}
tn_args <- function(r, fn) {
  k <- match(fn, vapply(r$content, `[[`, "", "fn"))
  if (is.na(k)) NULL else r$content[[k]]$args
}
tn_read <- function(cards, env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  f <- file.path(d, "T-1.yml")
  writeLines(c("id: T-1", "kind: table", "content:", "  - table_plan: {cols: ARM}", cards), f)
  tfl_read_output(f)
}

test_that("a card's `~` is read as the argument left out; !unset is kept as !unset", {
  a <- tn_read("  - plan_blanks: {last: true}")
  n <- tn_read("  - plan_blanks: {first: ~, last: true}")
  expect_identical(n$content, a$content)
  expect_identical(n$content[[2L]]$args, list(last = TRUE))
  u <- tn_read("  - plan_blanks: {first: !unset, last: true}")
  expect_s3_class(u$content[[2L]]$args$first, "tfl_unset")
  # (in a map inside an argument and a call's arguments too; a sequence keeps its elements)
  it <- tfl_item("plan_style", list(a = list(x = NULL, y = 1), b = list(1, NULL),
                                    c = tfl_call("rtf_border", list(top = NULL, bottom = TRUE))))
  expect_identical(it$args$a, list(y = 1))
  expect_length(it$args$b, 2L)
  expect_identical(it$args$c$args, list(bottom = TRUE))
})

test_that("a layer below: `~` keeps its value (as absent), !unset takes it out", {
  lay <- "  - plan_paginate_rows: {max_rows: 40}"
  absent <- tn_resolve(lay)
  expect_identical(tn_args(absent, "plan_paginate_rows")$max_rows, 40L)
  n <- tn_resolve(lay, "  - plan_paginate_rows: {max_rows: ~}")
  expect_identical(tn_args(n, "plan_paginate_rows"), tn_args(absent, "plan_paginate_rows"))
  u <- tn_resolve(lay, "  - plan_paginate_rows: {max_rows: !unset}")
  expect_false("max_rows" %in% names(tn_args(u, "plan_paginate_rows")))
})

test_that("the merges: a NULL over keeps the base's value; !unset removes it", {
  base <- list(n = 1L, w = list(a = 1, b = 2))
  expect_identical(.merge_by_arg(base, list(n = NULL, w = list(a = NULL, b = 3))),
                   list(n = 1L, w = list(a = 1, b = 3)))
  expect_identical(.merge_by_arg(base, list(n = tfl_unset(), w = list(a = tfl_unset()))),
                   list(w = list(b = 2)))
  expect_identical(.merge_map(base, list(n = NULL, w = list(a = NULL))), base)
  expect_identical(.merge_map(base, list(n = tfl_unset())), list(w = list(a = 1, b = 2)))
})

test_that("a verb only the report has: `~` is not in the call, as absent", {
  r <- tn_resolve(report = "  - plan_blanks: {first: ~, last: true}")
  expect_identical(tn_args(r, "plan_blanks"), list(last = TRUE))
  k <- match("plan_blanks", vapply(r$content, `[[`, "", "fn"))
  code <- paste(tfl_item_code(r$content[[k]]), collapse = " ")
  expect_false(grepl("first", code, fixed = TRUE))
  # !r NULL passes NULL
  expect_match(paste(tfl_item_code(tfl_item("plan_blanks", list(first = tfl_r("NULL")))), collapse = " "),
               "first = NULL", fixed = TRUE)
})

test_that("a stacking verb: `~` is not in the item", {
  r <- tn_resolve("  - plan_cell_style: {cols: A, italic: true}",
                  "  - plan_cell_style: {cols: B, bold: ~, italic: true}")
  st <- Filter(function(it) identical(it$fn, "plan_cell_style"), r$content)
  expect_length(st, 2L)
  expect_false("bold" %in% names(st[[2L]]$args))
})

test_that("a dots verb: `~` keeps the layer's key; with an empty base it is not written", {
  e <- tn_resolve(report = "  - plan_cells: {categorical: ~, default: '{n}'}")
  expect_identical(names(tn_args(e, "plan_cells")), "default")
  lay <- "  - plan_cells: {categorical: '{n} ({p}%)'}"
  n <- tn_resolve(lay, "  - plan_cells: {categorical: ~, default: '{n}'}")
  expect_identical(tn_args(n, "plan_cells")$categorical, "{n} ({p}%)")
  u <- tn_resolve(lay, "  - plan_cells: {categorical: !unset, default: '{n}'}")
  expect_false("categorical" %in% names(tn_args(u, "plan_cells")))
})

test_that("a later card's `~` does not wipe the earlier card's value (a template report)", {
  d <- withr::local_tempdir()
  f <- file.path(d, "T-2.yml")
  writeLines(c("id: T-2", "kind: table", "template:", "  name: tflspec_v1/tflspec_summary",
               "content:", "  - plan_labels: {AGE: Age}", "  - plan_labels: {AGE: ~, SEX: Sex}"), f)
  o <- suppressMessages(tfl_read_output(f))
  expect_identical(o$content[[1L]]$args, list(AGE = "Age", SEX = "Sex"))
  # (an item whose NULL was set after it was made: the same)
  b <- tfl_item("plan_style", list(font_size_half_points = 18L))
  b$args["font_size_half_points"] <- list(NULL)
  out <- suppressMessages(.plan_merge_repeats(list(tfl_item("plan_style", list(font_size_half_points = 16L)), b)))
  expect_identical(out[[1L]]$args, list(font_size_half_points = 16L))
})

test_that("a figure's `~` is no argument: not `x = c()`", {
  expect_null(.fig_value_lang(NULL))
  it <- tfl_item("geom_line", list(linewidth = 0.5, colour = NULL))
  expect_identical(names(it$args), "linewidth")
  it$args["colour"] <- list(NULL)
  expect_identical(.fig_deparse(.fig_item_lang(it)), "geom_line(linewidth = 0.5)")
})

test_that("page arguments agree: `~` keeps the configuration's value, !unset takes it out", {
  cfg <- list(rtf_document = list(orientation = "landscape"))
  absent <- .page_resolve(cfg, NULL, list())
  n <- .page_resolve(cfg, NULL, list(rtf_document = list(orientation = NULL)))
  expect_identical(n$rtf_document$orientation, "landscape")
  expect_identical(n$rtf_document, absent$rtf_document)
  u <- .page_resolve(cfg, NULL, list(rtf_document = list(orientation = tfl_unset())))
  expect_null(u$rtf_document$orientation)
  expect_identical(.page_args(list(a = NULL, b = 1)), list(b = 1))
})
