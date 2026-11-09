# The appearance verbs merge by argument: a report's plan_paginate_rows(),
# plan_columns(), plan_blanks() or plan_style() updates the one item below
# it (a template's, the study's) argument by argument; its shape stays

mba_study <- function() {
  tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))
}
mba_resolve <- function(name, args, content) {
  o <- tfl_output("T-9", "table", population = "SAF",
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
  ro2 <- mba_resolve("param_change", list(population = "SAF", data = "ADLB", params = c("ALT", "AST"),
                                      visits = c("BASELINE", "CYCLE 2 DAY 1")),
                     list(tfl_item("plan_paginate_rows", list(max_rows = 20))))
  pr2 <- mba_items(ro2, "plan_paginate_rows")
  expect_length(pr2, 1L)
  expect_identical(pr2[[1L]]$args$max_rows, 20)
  expect_true(all(c("page_by", "heading") %in% names(pr2[[1L]]$args)))
})

test_that("the split of a template stays under a report's max_rows", {
  ro <- mba_resolve("demog", list(population = "SAF"),
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
  # !unset takes a key of the layers away; several items replace as before
  m2 <- tflspec:::.merge_content(base, list(tfl_item("plan_style", list(bold = tfl_unset()))))
  expect_identical(names(m2[[1L]]$args), "border_header")
  two <- list(tfl_item("plan_style", list(font = "A")), tfl_item("plan_style", list(font = "B")))
  expect_identical(tflspec:::.merge_content(base, two), two)
  expect_identical(tflspec:::.merge_content(c(base, base), over), over)
})

test_that("a verb that is not an appearance verb is still replaced whole", {
  base <- list(tfl_item("plan_col_header", list(label = "A", note = "x"), from = "defaults"))
  over <- list(tfl_item("plan_col_header", list(label = "B")))
  m <- tflspec:::.merge_content(base, over)
  expect_length(m, 1L)
  expect_identical(m[[1L]]$args, list(label = "B"))
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
