# A template parameter's type, for an input form (tfl_param(type = ))

test_that("every tflspec template parameter has its type declared", {
  types <- c("vars", "levels", "flag", "number", "text", "choice", "list")
  # the names that mean two things are typed in each template
  own <- c("param", "sort")
  for (n in tfl_templates()$name) {
    for (p in tfl_template_params(n)) {
      at <- paste0(n, "$", p$name)
      expect_true(p$name %in% c(names(.tpl_param_kinds), own), info = at)
      expect_true(p$type %in% types, info = at)
      expect_true(is.logical(p$multiple) && length(p$multiple) == 1L, info = at)
      if (identical(p$type, "choice")) {
        expect_true(length(p$choices) >= 2L, info = at)
        expect_true(is.null(p$default) || p$default %in% p$choices, info = at)
      }
      if (identical(p$type, "list")) expect_true(length(p$fields) >= 2L, info = at)
      if (identical(p$type, "levels")) expect_true(length(p$of) == 1L, info = at)
      expect_false(isTRUE(p$guessed), info = at)
    }
  }
})

test_that("the names that mean two things, as each template says", {
  ty <- function(n, p) tfl_template_params(n)[[p]][c("type", "multiple")]
  expect_identical(ty("shift", "param"), list(type = "vars", multiple = FALSE))
  expect_identical(ty("orr", "param"), list(type = "levels", multiple = FALSE))
  expect_identical(ty("listing", "sort"), list(type = "vars", multiple = TRUE))
  expect_identical(tfl_template_params("ae_max_grade")$sort$choices, c("n", "name"))
  expect_identical(tfl_template_params("listing")$columns$fields,
                   c("vars", "label", "width", "collapse_repeats"))
  expect_identical(tfl_template_params("demog")$labels$fields, c("name", "text"))
})

test_that("a company's parameter without a type: by its name, else from its default", {
  p <- .tpl_param_typed(tfl_param("by", "ARM"))
  expect_identical(p$type, "vars")
  expect_false(p$multiple)
  expect_identical(.tpl_param_typed(tfl_param("show_total", TRUE))$type, "flag")
  expect_identical(.tpl_param_typed(tfl_param("digits", 1))$type, "number")
  q <- .tpl_param_typed(tfl_param("rows", c("A", "B")))
  expect_identical(q$type, "text")
  expect_true(q$multiple)
  # declared wins over the name
  expect_identical(.tpl_param_typed(tfl_param("by", "x", type = "levels"))$type, "levels")
  expect_error(tfl_param("x", type = "date"))
})

test_that("levels say whose values they are", {
  expect_identical(tfl_template_params("shift")$by_levels$of, "by")
  expect_identical(tfl_template_params("ae_max_grade")$grade_levels$of, "grade")
  expect_identical(tfl_template_params("lab_shift")$params$of, "PARAMCD")
  expect_identical(tfl_template_params("orr")$param$of, "PARAMCD")
})

test_that("tfl_test_templates(): a parameter with no type is listed", {
  expect_false(any(grepl("^no type", tfl_test_templates()$status)))
  d <- .tpl_get("demog", "test")
  suppressMessages(tfl_template("tt_guess", "table", "summary",
                                params = c(d$params, list(tfl_param("digits", 1))),
                                body = d$body, example = list(population = "SAF")))
  on.exit(rm("tt_guess", envir = .tpl_registry))
  r <- tfl_test_templates("tt_guess")
  expect_identical(r$status[r$param %in% "digits"], "no type (guessed: number)")
})

test_that("the schema's tier of each plan function, for the GUI's builder", {
  tier <- function(fn) tfl_schema(fn)$tier
  expect_identical(vapply(c("normalize_ard", "table_plan", "plan_cells"), tier, ""),
                   c(normalize_ard = "required", table_plan = "required", plan_cells = "required"))
  expect_identical(tier("plan_paginate_rows"), "usual")
  expect_identical(tier("plan_total"), "other")
  expect_identical(tier("plan_filter"), "other")
})
