cols <- c("name", "label_key", "step", "input", "type", "required", "default",
          "default_kind", "choices", "values", "free", "ordered", "columns", "fixed_rows", "tier",
          "description")

test_that("tfl_arg_defs() takes fn or template, one of them", {
  expect_error(tfl_arg_defs(), "one of them")
  expect_error(tfl_arg_defs(fn = "data", template = "tflspec_v1/demog"), "one of them")
})

test_that("the data row's and the analysis item's keys are defined", {
  d <- tfl_arg_defs(fn = "data")
  expect_identical(names(d), cols)
  expect_true(all(c("id", "from", "population", "subjects_in", "add", "where", "derive",
                    "keep", "distinct", "arrange", "then", "code") %in% d$name))

  expect_false("when" %in% d$name)
  expect_true(all(d$step == "data"))
  expect_identical(d$input[d$name == "where"], "condition")
  a <- tfl_arg_defs(fn = "analysis")
  expect_true(all(c("id", "type", "method", "data", "params", "conditions", "derive",
                    "levels", "ards", "analysis_ids") %in% a$name))

  expect_false("when" %in% a$name)
  cc <- a$columns[[which(a$name == "conditions")]]
  expect_identical(cc[[1L]], "params")
  expect_identical(cc$where$input, "condition")
  expect_false(cc$analysis_id$required)
  expect_true(a$free[a$name == "formats"])
  expect_identical(a$values[[which(a$name == "type")]], c("ard", "model", "value"))
  expect_identical(a$default_kind[a$name == "type"], "value")
  expect_true(a$required[a$name == "method"])
  expect_true(all(a$input %in% c("pick", "text", "condition", "table")))
})

test_that("a schema function's arguments come back in the same table", {
  d <- tfl_arg_defs(fn = "plan_cells")
  expect_identical(names(d), cols)
  expect_true("stats" %in% d$name)
  expect_identical(d$input[d$name == "stats"], "pick")
  expect_identical(d$values[[which(d$name == "stats")]], c("cells", "rows"))
  expect_true(all(d$step == "content"))
  expect_error(tfl_arg_defs(fn = "no_such_function"), "not a function of the schema")
})

test_that("generic defs name another argument with from_args(), a plain vars() is a data row", {
  d <- tfl_arg_defs(fn = "data")
  ch <- function(x, n) x$choices[[which(x$name == n)]]
  expect_identical(ch(d, "where"), "vars(from_args(from))")
  expect_identical(ch(d, "add"), "vars(from_args(subjects_in))")
  expect_identical(ch(d, "subjects_in"), c("report_data", "fixed"))
  expect_identical(d$values[[which(d$name == "subjects_in")]], "none")
  a <- tfl_arg_defs(fn = "analysis")
  expect_identical(ch(a, "variables"), "vars(from_args(data))")
  expect_silent(.arg_check_choices("vars(from_args(from))", "x"))
  expect_error(.arg_check_choices("vars(report_data(from))", "x"), "vars\\(\\) takes")
})

test_that("a schema column argument without choices gets ard (normalize_ard) or columns (plan_*)", {
  n <- tfl_arg_defs(fn = "normalize_ard")
  expect_identical(n$choices[[which(n$name == "keys")]], "ard")
  expect_identical(n$choices[[which(n$name == "hierarchy")]], "ard")
  expect_true(n$ordered[n$name == "hierarchy"])
  p <- tfl_arg_defs(fn = "plan_paginate_rows")
  expect_identical(p$choices[[which(p$name == "page_by")]], "columns")
  g <- tfl_arg_defs(fn = "plan_paginate_group")
  expect_identical(g$choices[[which(g$name == "col")]], "columns")
})

test_that("the paginate functions are the page step, other stages stay content", {
  for (f in c("plan_paginate_rows", "plan_paginate_cols", "plan_paginate_group")) {
    expect_true(all(tfl_arg_defs(fn = f)$step == "page"), info = f)
  }
  expect_true(all(tfl_arg_defs(fn = "normalize_ard")$step == "content"))
  expect_true(all(tfl_arg_defs(fn = "plan_blanks")$step == "content"))
})

test_that("an r argument is a text box, a condition only for where", {
  expect_identical(.arg_input_for_type("r", "code"), "text")
  expect_identical(.arg_input_for_type("r", "where"), "condition")
  expect_identical(.arg_input_for_type("r", "when"), "text")
  expect_identical(.arg_input_for_type("r"), "text")
})

test_that("no row is made for a ... argument", {
  for (f in c("plan_cells", "rtf_col_header", "cell_rows")) {
    d <- tfl_arg_defs(fn = f)
    expect_false("..." %in% d$name, info = f)
    expect_false(any(grepl("...", d$label_key, fixed = TRUE)), info = f)
  }
})

test_that("a template's params: are read the same way, tags kept", {
  dir <- tempfile("series")
  dir.create(file.path(dir, "test_v1", "templates"), recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  old <- options(tflspec.series_dir = dir)
  on.exit(options(old), add = TRUE)
  writeLines(c(
    "template: demog",
    "label: Demographics",
    "params:",
    "  by:",
    "    type: list<column>",
    "    input: pick",
    "    choices: vars(adsl_sub)",
    "    required: true",
    "    step: analysis",
    "    description: The groups across.",
    "  total:",
    "    type: logical",
    "    default: !series total",
    "    description: A Total column.",
    "  missing_row:",
    "    type: logical",
    "    default: true",
    "  conditions:",
    "    type: rows<map<string,string|r>>",
    "    input: table",
    "    columns: params",
    "    default: []",
    "data:",
    "  - id: adsl_sub",
    "    from: ADSL",
    "analysis:",
    "  - id: BIGN",
    "    method: categorical",
    "    data: adsl_sub",
    "    variables: [\"{by}\"]"),
    file.path(dir, "test_v1", "templates", "demog.yml"))
  d <- tfl_arg_defs(template = "test_v1/demog")
  expect_identical(names(d), cols)
  expect_identical(d$name, c("by", "total", "missing_row", "conditions"))
  expect_true(d$required[1L])
  expect_identical(d$choices[[1L]], "vars(adsl_sub)")
  expect_identical(d$input[d$name == "total"], "pick")
  expect_identical(d$default_kind, c(NA, "series", "value", "value"))
  expect_s3_class(d$default[[2L]], "tfl_tagged")
  expect_identical(d$default[[2L]]$value, "total")
  expect_identical(d$columns[[4L]], "params")
  expect_identical(d$label_key[1L], "template.test_v1.demog.by")
  expect_error(tfl_arg_defs(template = "test_v1/none"), "no file")
  expect_error(tfl_arg_defs(template = "demog"), "<series>/<template>")
  expect_error(tfl_arg_defs(template = "Test/demog"), "lower case")
})

test_that("a choice source outside the 14 words stops", {
  expect_error(.arg_check_choices("vars(adsl_sub), datasets", "x"), "unknown choice source")
  expect_silent(.arg_check_choices("from_args(continuous, categorical), report_data", "x"))
  expect_identical(.arg_choice_words("from_args(continuous, categorical), report_data"),
                   c("from_args(continuous, categorical)", "report_data"))
})

test_that("fixed_rows: the rows a template writes next to a table param's \"{name}\"", {
  body <- list(analysis = list(list(id = "HR", conditions = list(
    list(subgroup = "ALL", subgroup_lvl = "ALL", where = tfl_r("TRUE")), "{conditions}"))))
  fr <- tflspec:::.arg_fixed_rows(body, "conditions")
  expect_identical(fr$subgroup, "ALL")
  expect_identical(fr$where, "TRUE")
  expect_identical(attr(fr, "position"), "before")
  expect_null(tflspec:::.arg_fixed_rows(body, "by"))
  d <- tfl_arg_defs(template = "tflspec_v1/forest_hr")
  fx <- d$fixed_rows[[which(d$name == "conditions")]]
  expect_identical(fx$subgroup, "ALL")
  expect_null(d$fixed_rows[[which(d$name == "by")]])
  expect_null(tfl_arg_defs(fn = "analysis")$fixed_rows[[1L]])
})
