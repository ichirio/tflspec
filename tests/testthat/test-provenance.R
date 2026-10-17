# Where each argument comes from (tfl_resolve_output(provenance = TRUE)),
# one item's code / written arguments / own part, the schema's formals,
# and a study part written back.

prov_study <- function() {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(d, "spec", "patterns"), recursive = TRUE)
  file.copy(system.file("extdata", "spec-yaml", "defaults.yml", package = "tflspec"),
            file.path(d, "spec"))
  writeLines(c("content:", "  - plan_cells: {continuous: {n: \"{N}\"}}",
               "  - plan_digits: {rounding: r}"),
             file.path(d, "spec", "patterns", "Tight.yml"))
  d
}

test_that("provenance: argument by argument, and the item below", {
  d <- prov_study()
  st <- tfl_read_study_spec(d)
  o <- tfl_output("T", extends = c("patterns/Tight", "defaults"),
                  content = list(tfl_item("table_plan", list(cols = "TRT01A")),
                                 tfl_item("plan_cells", list(default = "{n}"))))
  r <- tfl_resolve_output(o, st, provenance = TRUE)
  pc <- Filter(function(it) it$fn == "plan_cells", r$content)[[1L]]
  expect_identical(pc$arg_from[["default"]], "own")
  expect_identical(pc$arg_from[["continuous"]], "pattern:Tight")
  expect_identical(pc$arg_from[["categorical"]], "defaults")
  expect_identical(pc$below$args$continuous, list(n = "{N}"))
  expect_null(pc$below$args$default)
  dg <- Filter(function(it) it$fn == "plan_digits", r$content)[[1L]]
  expect_identical(unname(dg$arg_from), "pattern:Tight")
  tp <- Filter(function(it) it$fn == "table_plan", r$content)[[1L]]
  expect_null(tp$below)
  # without it, nothing is added
  r0 <- tfl_resolve_output(o, st)
  expect_null(r0$content[[1L]]$arg_from)
  expect_identical(lapply(r0$content, `[[`, "args"), lapply(r$content, `[[`, "args"))
})

test_that("tfl_item_code() / tfl_item_written() / tfl_item_own()", {
  it <- tfl_item("plan_cells", list(continuous = list(n = "{N}"), default = "{n}"))
  expect_identical(tfl_item_code(it), 'plan_cells(continuous = list(n = "{N}"), default = "{n}")')
  expect_identical(tfl_item_written(it), c("continuous", "default"))
  expect_identical(tfl_item_written(tfl_item("plan_titles", list(titles = list("A")))), "...")
  below <- tfl_item("plan_cells", list(default = "{n}", categorical = "{n} ({p})"), from = "defaults")
  own <- tfl_item_own(it, below)
  expect_identical(names(own$args), c("continuous", "categorical"))
  expect_s3_class(own$args$categorical, "tfl_unset")
  # another verb: the same as below is no own item; different, the item whole
  expect_null(tfl_item_own(tfl_item("plan_paginate_rows", list(max_rows = 30L)),
                           tfl_item("plan_paginate_rows", list(max_rows = 30), from = "defaults")))
  w <- tfl_item_own(tfl_item("plan_paginate_rows", list(max_rows = 22L, split = "group_force")),
                    tfl_item("plan_paginate_rows", list(max_rows = 30L), from = "defaults"))
  expect_identical(w$args$max_rows, 22L)
  expect_identical(tfl_item_own(it)$from, "own")
  expect_error(tfl_item_own(it, tfl_item("plan_digits", list())), "is a plan_digits item")
})

test_that("tfl_schema(): the function's formals and the required ones", {
  e <- tfl_schema("plan_paginate_rows")
  expect_true("max_rows" %in% e$formals)
  expect_false(any(c(".plan", "plan", "x") %in% e$formals))
  expect_identical(e$required, character())
  skip_if_not_installed("ggplot2")
  expect_true("yintercept" %in% tfl_schema("geom_hline")$required)
})

test_that("tfl_write_study_part(): each part written back reads the same", {
  d <- prov_study()
  st <- tfl_read_study_spec(d)
  t <- withr::local_tempdir()
  st2 <- st
  st2$dir <- t
  tfl_write_study_part(st2, "defaults")
  tfl_write_study_part(st2, "patterns", name = "Tight")
  back <- tfl_read_study_spec(t)
  expect_identical(back$defaults, st$defaults)
  expect_identical(back$patterns, st$patterns)
  expect_error(tfl_write_study_part(st2, "patterns", name = "Nope"), "one of the study's patterns")
})
