# What a form reads from the schema: an argument's label, help and
# whether it is shown from the start (main); the arguments are the
# functions' own, their types ones the schema knows.

test_that("the table and listing functions' arguments say their label; main ones say so", {
  s <- tfl_schema()
  fns <- c("normalize_ard", "table_plan", "plan_cells", "plan_columns", "plan_style",
           "plan_cell_style", "plan_paginate_rows", "plan_blanks")
  for (fn in fns) {
    a <- s[[fn]]$args
    expect_true(length(a) > 0L, label = fn)
    lab <- vapply(a, function(x) is.character(x$label) && length(x$label) == 1L && nzchar(x$label), NA)
    expect_true(all(lab), label = paste(fn, paste(names(a)[!lab], collapse = ", ")))
  }
  expect_true(isTRUE(s$plan_columns$args$widths$main))
  expect_false(isTRUE(s$plan_style$args$table_width_twips$main))
  # main is a logical when it is there
  for (fn in names(s)) for (a in names(s[[fn]]$args)) {
    m <- s[[fn]]$args[[a]]$main
    if (!is.null(m)) expect_true(isTRUE(m) || isFALSE(m), label = paste0(fn, "$", a))
  }
})

test_that("a type the schema gives checks a value; a colour also takes an !r", {
  expect_identical(nrow(tfl_schema_check(tfl_item("plan_cell_style",
    list(where = tfl_r("~ x == 1"), color = "#CC0000")))), 0L)
  expect_identical(nrow(tfl_schema_check(tfl_item("plan_cell_style",
    list(color = tfl_r("~ ifelse(n > 5, \"#CC0000\", NA)"))))), 0L)
  p <- tfl_schema_check(tfl_item("plan_style", list(table_width_pct = "wide")))
  expect_identical(p$level, "error")
})

test_that("plan_blanks(where) and plan_paginate_cols(order) are their choices", {
  ok <- function(fn, a) nrow(tfl_schema_check(tfl_item(fn, a))) == 0L
  expect_true(ok("plan_blanks", list(where = "between_parts")))
  expect_true(ok("plan_blanks", list(where = list(0L, 3L))))
  expect_false(ok("plan_blanks", list(where = "between_rows")))
  expect_true(ok("plan_paginate_cols", list(order = "down")))
  expect_true(ok("plan_paginate_cols", list(order = list("group", "rows", "cols"))))
  expect_false(ok("plan_paginate_cols", list(order = "sideways")))
})
