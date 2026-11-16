cols <- c("name", "label_key", "step", "input", "type", "required", "default",
          "default_kind", "choices", "values", "free", "ordered", "columns", "tier",
          "description")

test_that("tfl_arg_defs() takes fn or template, one of them", {
  expect_error(tfl_arg_defs(), "one of them")
  expect_error(tfl_arg_defs(fn = "data", template = "tflspec_v1/demog"), "one of them")
})

test_that("the data row's and the analysis item's keys are defined", {
  d <- tfl_arg_defs(fn = "data")
  expect_identical(names(d), cols)
  expect_true(all(c("id", "from", "population", "subjects_in", "add", "where", "derive",
                    "keep", "distinct", "arrange", "then", "code", "when") %in% d$name))
  expect_true(all(d$step == "data"))
  expect_identical(d$input[d$name == "where"], "condition")
  a <- tfl_arg_defs(fn = "analysis")
  expect_true(all(c("id", "type", "method", "data", "params", "conditions", "derive",
                    "levels", "ards", "analysis_ids", "when") %in% a$name))
  expect_identical(a$columns[[which(a$name == "conditions")]], "params")
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
