# The appearance verbs merge by argument: a report's plan_paginate_rows(),
# plan_columns(), plan_blanks() or plan_style() updates the one item below
# it (a template's, the study's) argument by argument; its shape stays

mba_study <- function() {
  tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))
}
mba_resolve <- function(name, args, content) {
  o <- tfl_output("T-9", "table", common = list(population = "SAF"),
                  template = list(name = name, args = args), content = content)
  tfl_resolve_output(o, mba_study())
}
mba_items <- function(ro, fn) Filter(function(it) identical(it$fn, fn), ro$content)

test_that("a template's page_by, heading and heading_at stay under a report's max_rows", {
  ro <- mba_resolve("shift", list(population = "SAF"),
                    list(tfl_item("plan_paginate_rows", list(max_rows = 20))))
  pr <- mba_items(ro, "plan_paginate_rows")
  expect_length(pr, 1L)
  expect_identical(pr[[1L]]$args$max_rows, 20)
  expect_identical(pr[[1L]]$args$page_by, "group")
  expect_true(all(c("heading", "heading_at") %in% names(pr[[1L]]$args)))
  # (the same in param_change)
  ro2 <- mba_resolve("param_change", list(population = "SAF", data = "ADLB", by = "TRT01A", params = c("ALT", "AST"),
                                      visits = c("BASELINE", "CYCLE 2 DAY 1")),
                     list(tfl_item("plan_paginate_rows", list(max_rows = 20))))
  pr2 <- mba_items(ro2, "plan_paginate_rows")
  expect_length(pr2, 1L)
  expect_identical(pr2[[1L]]$args$max_rows, 20)
  expect_true(all(c("page_by", "heading") %in% names(pr2[[1L]]$args)))
})

test_that("the split of a template stays under a report's max_rows", {
  ro <- mba_resolve("demog", list(by = "TRT01A", continuous = "AGE", categorical = c("AGEGR1", "SEX")),
                    list(tfl_item("plan_paginate_rows", list(max_rows = 25))))
  pr <- mba_items(ro, "plan_paginate_rows")
  expect_length(pr, 1L)
  expect_identical(pr[[1L]]$args$max_rows, 25)
  expect_identical(pr[[1L]]$args$split, "group_safe")
})

test_that("pk_conc's decimal stays under a report's widths; its blanks' where under plan_blanks", {
  ro <- mba_resolve("pk_conc", list(population = "SAF"),
                    list(tfl_item("plan_columns", list(widths = c(2, 1))),
                         tfl_item("plan_blanks", list(last = FALSE))))
  cl <- mba_items(ro, "plan_columns")
  expect_length(cl, 1L)
  expect_identical(cl[[1L]]$args$decimal, ".values")
  expect_identical(cl[[1L]]$args$widths, c(2, 1))
  bl <- mba_items(ro, "plan_blanks")
  expect_length(bl, 1L)
  expect_identical(bl[[1L]]$args$where, "between_parts")
  expect_false(bl[[1L]]$args$last)
  expect_true(bl[[1L]]$args$first)
})

test_that("plan_style: the study's border_header and the report's font both stay", {
  base <- list(tfl_item("plan_style", list(border_header = "tfl", bold = TRUE), from = "defaults"))
  over <- list(tfl_item("plan_style", list(font = "Arial", bold = FALSE)))
  m <- tflspec:::.merge_content(base, over)
  expect_length(m, 1L)
  expect_identical(m[[1L]]$args, list(border_header = "tfl", bold = FALSE, font = "Arial"))
  # !unset takes a key of the layers away; several report items fold in order
  m2 <- tflspec:::.merge_content(base, list(tfl_item("plan_style", list(bold = tfl_unset()))))
  expect_identical(names(m2[[1L]]$args), "border_header")
  two <- list(tfl_item("plan_style", list(font = "A")), tfl_item("plan_style", list(font = "B")))
  m3 <- tflspec:::.merge_content(base, two)
  expect_length(m3, 1L)
  expect_identical(m3[[1L]]$args, list(border_header = "tfl", bold = TRUE, font = "B"))
  # several items below: the report's replace them
  expect_identical(tflspec:::.merge_content(c(base, base), over), over)
})

test_that("plan_col_header: one argument changed, the others kept", {
  base <- list(tfl_item("plan_col_header", list(label = "A", note = "x"), from = "defaults"))
  m <- tflspec:::.merge_content(base, list(tfl_item("plan_col_header", list(label = "B"))))
  expect_length(m, 1L)
  expect_identical(m[[1L]]$args, list(label = "B", note = "x"))
})

test_that("plan_col_header: the report's header replaces the layers' lines, and the other way", {
  base <- list(tfl_item("plan_col_header", list(lines = list(c(a = "x")), values = list(n = TRUE)), from = "defaults"))
  m <- tflspec:::.merge_content(base, list(tfl_item("plan_col_header", list(header = list(list(line = 1, cols = "a"))))))
  expect_identical(names(m[[1L]]$args), c("values", "header"))
  base2 <- list(tfl_item("plan_col_header", list(header = list(list(line = 1, cols = "a"))), from = "defaults"))
  m2 <- tflspec:::.merge_content(base2, list(tfl_item("plan_col_header", list(lines = list(c(a = "x"))))))
  expect_identical(names(m2[[1L]]$args), "lines")
})

test_that("dots verbs (plan_labels) still merge by key", {
  base <- list(tfl_item("plan_labels", list(a = "A", b = "B"), from = "defaults"))
  m <- tflspec:::.merge_content(base, list(tfl_item("plan_labels", list(b = "B2", c = "C"))))
  expect_length(m, 1L)
  expect_identical(m[[1L]]$args, list(a = "A", b = "B2", c = "C"))
})

test_that("stacking verbs add the report's items after the layers' items", {
  for (fn in c("plan_cell_style", "plan_filter", "plan_hide", "plan_after", "plan_append")) {
    base <- list(tfl_item(fn, list(x = 1), from = "defaults"), tfl_item("plan_style", list(bold = TRUE)),
                 tfl_item(fn, list(x = 2), from = "defaults"))
    over <- list(tfl_item(fn, list(x = 3)), tfl_item(fn, list(x = 4)))
    m <- tflspec:::.merge_content(base, over)
    xs <- vapply(Filter(function(it) identical(it$fn, fn), m), function(it) it$args$x, 1)
    expect_identical(xs, c(1, 2, 3, 4), info = fn)
    # the layers' items stay in place; the report's go right after the last of them
    expect_identical(vapply(m, `[[`, "", "fn"), c(fn, "plan_style", fn, fn, fn), info = fn)
  }
  # one item below too
  m <- tflspec:::.merge_content(list(tfl_item("plan_cell_style", list(bold = TRUE), from = "defaults")),
                                list(tfl_item("plan_cell_style", list(italic = TRUE))))
  expect_length(m, 2L)
  expect_identical(m[[1L]]$args, list(bold = TRUE))
  expect_identical(m[[2L]]$args, list(italic = TRUE))
})

test_that("a single-item verb merges by argument: plan_titles, plan_stub, plan_total", {
  base <- list(tfl_item("plan_titles", list(titles = c("T1", "T2"), pages = TRUE), from = "defaults"),
               tfl_item("plan_stub", list(heading = "H", indent = 2), from = "defaults"),
               tfl_item("plan_total", list(label = "Total", position = "last"), from = "defaults"))
  over <- list(tfl_item("plan_titles", list(titles = "New")),
               tfl_item("plan_stub", list(heading = "H2")),
               tfl_item("plan_total", list(label = "All")))
  m <- tflspec:::.merge_content(base, over)
  expect_length(m, 3L)
  expect_identical(m[[1L]]$args, list(titles = "New", pages = TRUE))
  expect_identical(m[[2L]]$args, list(heading = "H2", indent = 2))
  expect_identical(m[[3L]]$args, list(label = "All", position = "last"))
})

test_that("a map argument merges by key, one level; !unset removes a key; a non-map replaces", {
  base <- list(tfl_item("plan_columns", list(widths = list(a = 1, b = 2), decimal = ".values"), from = "defaults"))
  m <- tflspec:::.merge_content(base, list(tfl_item("plan_columns", list(widths = list(b = 3, c = 4)))))
  expect_identical(m[[1L]]$args$widths, list(a = 1, b = 3, c = 4))
  expect_identical(m[[1L]]$args$decimal, ".values")
  m2 <- tflspec:::.merge_content(base, list(tfl_item("plan_columns", list(widths = list(a = tfl_unset())))))
  expect_identical(m2[[1L]]$args$widths, list(b = 2))
  # a plain vector replaces whole; the whole argument unset removes it
  m3 <- tflspec:::.merge_content(base, list(tfl_item("plan_columns", list(widths = c(2, 1)))))
  expect_identical(m3[[1L]]$args$widths, c(2, 1))
  m4 <- tflspec:::.merge_content(base, list(tfl_item("plan_columns", list(widths = tfl_unset()))))
  expect_identical(names(m4[[1L]]$args), "decimal")
  # a style's map argument
  sb <- list(tfl_item("plan_style", list(border = list(top = "single", bottom = "single")), from = "defaults"))
  ms <- tflspec:::.merge_content(sb, list(tfl_item("plan_style", list(border = list(bottom = "double")))))
  expect_identical(ms[[1L]]$args$border, list(top = "single", bottom = "double"))
})

test_that("several items of a single-item verb below are replaced by the report's", {
  base <- list(tfl_item("plan_col_header", list(label = "A", note = "x"), from = "defaults"),
               tfl_item("plan_col_header", list(label = "C"), from = "defaults"))
  over <- list(tfl_item("plan_col_header", list(label = "B")))
  expect_identical(tflspec:::.merge_content(base, over), over)
})

test_that("a figure's item is still replaced whole", {
  base <- list(tfl_item("labs", list(title = "A", x = "x"), from = "defaults"))
  over <- list(tfl_item("labs", list(title = "B")))
  m <- tflspec:::.merge_content(base, over)
  expect_length(m, 1L)
  expect_identical(m[[1L]]$args, list(title = "B"))
})

test_that("end to end: a template's plan_cell_style stays and the report's is added after", {
  o <- tfl_output("T-9", "table", common = list(population = "SAF"),
                  template = list(name = "demog", args = list(by = "TRT01A", continuous = "AGE", categorical = c("AGEGR1", "SEX"))),
                  content = list(tfl_item("plan_cell_style", list(bold = TRUE, cols = "total")),
                                 tfl_item("plan_titles", list(titles = "My title"))))
  ro <- tfl_resolve_output(o, mba_study(), provenance = TRUE)
  cs <- mba_items(ro, "plan_cell_style")
  expect_gte(length(cs), 1L)
  expect_identical(cs[[length(cs)]]$args$cols, "total")
  tt <- mba_items(ro, "plan_titles")
  expect_length(tt, 1L)
  expect_identical(tt[[1L]]$args$titles, "My title")
  # provenance: the report's argument is "own"; one it did not write comes from below
  expect_identical(tt[[1L]]$arg_from[["titles"]], "own")
  if (length(setdiff(names(tt[[1L]]$args), "titles"))) {
    expect_false(all(tt[[1L]]$arg_from == "own"))
  }
})

test_that("provenance of a single-item verb is per argument from the nearest layer", {
  base <- list(tfl_item("plan_stub", list(heading = "H", indent = 2), from = "defaults"))
  items <- tflspec:::.merge_content(base, list(tfl_item("plan_stub", list(heading = "H2"))))
  prov <- tflspec:::.content_provenance(items, list(tfl_item("plan_stub", list(heading = "H2"))),
                                        list(list(content = base)), base)
  expect_identical(prov[[1L]]$arg_from, c(heading = "own", indent = "defaults"))
})

test_that("the program after a template's call carries the report's arguments, and they add up to the SPEC's item", {
  ro <- mba_resolve("shift", list(population = "SAF"),
                    list(tfl_item("plan_paginate_rows", list(max_rows = 20))))
  tp <- attr(ro, "template")
  expect_true(tp$ok)
  line <- Filter(function(it) identical(it$fn, "plan_paginate_rows"), tp$rest)
  expect_length(line, 1L)
  # rtfreporter merges two calls of plan_paginate_rows() (last wins per argument):
  # the template's call and this line make the item the SPEC route resolves
  tpl_it <- Filter(function(it) identical(it$fn, "plan_paginate_rows"), tp$items)[[1L]]
  expect_identical(line[[1L]]$args, list(max_rows = 20))
  spec_it <- mba_items(ro, "plan_paginate_rows")[[1L]]
  expect_setequal(names(spec_it$args), union(names(tpl_it$args), names(line[[1L]]$args)))
  expect_identical(spec_it$args$max_rows, 20)
  code <- paste(tfl_content_code(ro, pipe = "|>"), collapse = "\n")
  expect_match(code, "plan_paginate_rows(max_rows = 20)", fixed = TRUE)
  expect_false(grepl("page_by", code, fixed = TRUE))
})
