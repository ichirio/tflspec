# A spec's content -- items, one a function call -- to code (tfl_content_code()),
# the schema it is checked against (tfl_schema(), tfl_schema_check()), and the
# bridge between it and the sheets of a table workbook, both ways.

ext_spec <- function(name) system.file("extdata", "ard-spec", name, package = "tflspec")

test_that("a table workbook, through its content and back, writes the same code", {
  for (f in c("AE.xlsx", "DM.xlsx", "LB.xlsx", "ORR.xlsx", "PK.xlsx")) {
    sp <- tfl_read_table_spec(ext_spec(f))
    for (id in unique(stats::na.omit(sp$tables$output_id))) {
      items <- tflspec:::.plan_content_from_sheets(sp, id)
      expect_true(all(vapply(items, inherits, NA, "tfl_item")))
      expect_identical(items[[1L]]$fn, "table_plan")
      back <- tflspec:::.plan_sheets_from_content(items, id)
      expect_length(attr(back, "not_converted"), 0L)
      expect_identical(tfl_table_code(back, id), tfl_table_code(sp, id))
    }
  }
})

test_that("the bridge's items hold the spec's forms, not R's", {
  sp <- tfl_read_table_spec(ext_spec("AE.xlsx"))
  items <- tflspec:::.plan_content_from_sheets(sp, "AE")
  by <- stats::setNames(items, vapply(items, `[[`, "", "fn"))
  # plan_sort's keys under `keys`, no unnamed argument
  expect_identical(by$plan_sort$args$keys[1:2], c(".overall", "group1"))
  # a named vector as a map
  expect_true(is.list(by$plan_columns$args$widths))
  # the header's data frame as rows of fields
  expect_true(is.list(by$plan_col_header$args$header))
  expect_identical(by$plan_col_header$args$header[[1L]]$line, "1")
})

test_that("tfl_content_code(): a resolved spec file's table, its inherited items marked", {
  d <- system.file("extdata", "spec-yaml", package = "tflspec")
  st <- tfl_read_study_spec(d)
  out <- tfl_resolve_output(tfl_read_outputs(d)[[1L]], st)
  code <- tfl_content_code(out, pipe = "|>")
  expect_match(code[1L], "^plan <- table_plan\\(data, cols = \"TRT01A\"", perl = TRUE)
  expect_true(any(grepl("# (study default)", code, fixed = TRUE)))
  expect_true(any(grepl("plan_cell_style(where = ~ p > 0.5, bold = TRUE)", code, fixed = TRUE)))
  expect_true(any(grepl("border_header = rtf_border(top = TRUE, bottom = TRUE)", code, fixed = TRUE)))
  # a statistic named N (or n) is not YAML's false
  expect_false(any(grepl("\"FALSE\"", code, fixed = TRUE)))
  expect_true(any(grepl("N = 0", code, fixed = TRUE)))
  # an !unset left over is an error, not a silent NULL
  bad <- out
  bad$content[[2L]]$args$x <- tfl_unset()
  expect_error(tfl_content_code(bad), "!unset")
  expect_error(tfl_content_code(list()), "tfl_output")
})

test_that("tfl_content_code(): the content runs as the plan the code writes", {
  skip_if_not_installed("cards")
  ard <- cards::ard_stack(cards::ADSL, .by = ARM, cards::ard_tabulate(variables = SEX))
  data <- rtfreporter::normalize_ard(ard)
  out <- tfl_output("T-1", "table", content = list(
    tfl_item("table_plan", list(cols = "ARM", rows = list(group = "variable"))),
    tfl_item("plan_cells", list(categorical = "{n} ({p:.1f%})")),
    tfl_item("plan_sort", list(keys = "-n"))))
  code <- tfl_content_code(out, pipe = "|>")
  env <- new.env()
  env$data <- data
  eval(parse(text = c("library(rtfreporter)", code)), env)
  expect_s3_class(env$plan, "table_plan")
})

test_that("tfl_schema(): the 13 functions of `...` have their grammar", {
  dots13 <- c("plan_cells", "plan_digits", "plan_levels", "plan_labels", "plan_nest",
              "plan_stat_groups", "plan_hide", "plan_sort", "plan_titles",
              "plan_footnotes", "plan_after", "plan_filter", "plan_compare")
  s <- tfl_schema()
  expect_true(all(dots13 %in% names(s)))
  for (f in dots13) expect_false(is.null(s[[f]]$dots), info = f)
  expect_identical(tfl_schema("rtfreporter::plan_sort")$dots$key, "keys")
  expect_null(tfl_schema("no_such_fn"))
  # every type parses
  for (f in names(s)) {
    for (a in c(list(s[[f]]$dots), s[[f]]$args)) {
      if (!is.null(a$type)) expect_type(tflspec:::.schema_type(a$type), "list")
    }
  }
})

test_that("tfl_schema_check(): a wrong argument or value is said, a right one passes", {
  ok <- tfl_schema_check(tfl_item("plan_sort", list(keys = c("-n", "SOC"), stat = "n")))
  expect_identical(nrow(ok), 0L)
  p <- tfl_schema_check(tfl_item("plan_sort", list(keys = "-n", sortt = 1)))
  expect_identical(p$arg, "sortt")
  expect_match(p$message, "no argument")
  p <- tfl_schema_check(tfl_item("plan_paginate_rows", list(max_rows = "many")))
  expect_identical(p$arg, "max_rows")
  expect_match(p$message, "integer")
  p <- tfl_schema_check(tfl_item("plan_cell_style", list(where = "p > 0.5")))
  expect_match(p$message, "formula")
  expect_identical(nrow(tfl_schema_check(tfl_item("plan_cell_style",
    list(where = tfl_r("p > 0.5"), bold = TRUE)))), 0L)
  # named `...`: any key, the value's type checked
  expect_identical(nrow(tfl_schema_check(tfl_item("plan_labels", list(AGE = "Age")))), 0L)
  p <- tfl_schema_check(tfl_item("plan_labels", list(AGE = 1:3)))
  expect_identical(p$arg, "AGE")
  # a nested call of the right function
  expect_identical(nrow(tfl_schema_check(tfl_item("plan_style",
    list(border_header = tfl_call("rtf_border", list(top = TRUE)))))), 0L)
  p <- tfl_schema_check(tfl_item("no_such_fn", list()))
  expect_match(p$message, "neither the schema nor its package")
})

test_that("the sheets say what they cannot hold, and leave it out", {
  items <- list(
    tfl_item("table_plan", list(cols = "TRT", rows = list(group = "variable"))),
    tfl_item("plan_after", list(steps = tfl_r("function(x) x"))),
    tfl_item("plan_columns", list(widths = list(3, 2))))
  sp <- tflspec:::.plan_sheets_from_content(items, "T")
  nc <- attr(sp, "not_converted")
  expect_true(any(grepl("^plan_after:", nc)))
  expect_true(any(grepl("widths by position", nc, fixed = TRUE)))
  expect_identical(sp$tables$cols, "TRT")
})

test_that("versions: an unknown argument warns, a renamed one says its new name, a newer one warns", {
  old <- tflspec:::.schema_cache$all
  on.exit(assign("all", old, envir = tflspec:::.schema_cache), add = TRUE)
  s <- tfl_schema()
  s$plan_sort$args$sortkeys <- list(renamed_to = "keys", until = "rtfreporter 0.8.2.9040")
  s$plan_total$args$label$since <- "rtfreporter 99.0.0"
  assign("all", s, envir = tflspec:::.schema_cache)
  p <- tfl_schema_check(tfl_item("plan_sort", list(sortkeys = "-n")))
  expect_identical(p$level, "warning")
  expect_identical(p$message, "plan_sort(sortkeys = ) is now `keys` (since rtfreporter 0.8.2.9040).")
  expect_identical(tfl_schema_check(tfl_item("plan_sort", list(keys = "-n", sortt = 1)))$level, "warning")
  out <- tfl_output("T-1", "table", content = list(
    tfl_item("table_plan", list(cols = "ARM")),
    tfl_item("plan_total", list(label = "Total"))))
  expect_warning(code <- tfl_content_code(out), "rtfreporter >= 99.0.0")
  expect_true(length(code) > 0L)                 # warned, not stopped
})

test_that("the bridge reads a workbook's defaults alone (NA), or a table's own rows (own = TRUE)", {
  sp <- tfl_read_table_spec(ext_spec("study.xlsx"))
  ids <- unique(stats::na.omit(sp$tables$output_id))
  d <- tflspec:::.plan_content_from_sheets(sp, NA)
  expect_false("table_plan" %in% vapply(d, `[[`, "", "fn"))
  o <- tflspec:::.plan_content_from_sheets(sp, ids[1L], own = TRUE)
  expect_identical(o[[1L]]$fn, "table_plan")
  a <- tflspec:::.plan_content_from_sheets(sp, ids[1L])
  expect_true(length(a) >= length(o))
})

test_that("data.steps come before the plan, as written, and run the same", {
  skip_if_not_installed("cards")
  out <- tfl_output("T-1", "table",
    data = list(source = "ard", object = "ard", steps = list(tfl_r(
      "data <- normalize_ard(ard)\ndata <- data[data$variable != \"RACE\", ]"))),
    content = list(
      tfl_item("table_plan", list(cols = "ARM", rows = list(group = "variable"))),
      tfl_item("plan_cells", list(categorical = "{n} ({p:.1f%})"))))
  code <- tfl_content_code(out, pipe = "|>")
  expect_identical(code[1:4], c("data <- normalize_ard(ard)",
                                "data <- data[data$variable != \"RACE\", ]", "",
                                "plan <- table_plan(data, cols = \"ARM\", rows = c(group = \"variable\")) |>"))
  ard <- cards::ard_stack(cards::ADSL, .by = ARM,
                          cards::ard_tabulate(variables = c(SEX, RACE)))
  env <- new.env()
  env$ard <- ard
  eval(parse(text = c("library(rtfreporter)", code)), env)
  pages <- function(p) suppressMessages(rtfreporter::plan_apply(p, "table"))
  expect_identical(pages(env$plan), pages(tfl_content_run(out, ard)))
  bad <- out
  bad$data$steps <- list(list(fn = "geom_point"))
  expect_error(tfl_content_code(bad), "are R")
})

test_that("the content as the report's own code is written as it is and runs to `content`", {
  out <- tfl_output("F-1", "figure",
                    data = list(source = "ard", object = "ard",
                                steps = list(tfl_r("data <- ard[ard$x > 1, , drop = FALSE]"))))
  out$content <- tfl_r("content <- nrow(data)\ncontent <- content * 10")
  expect_identical(tfl_content_code(out), c("data <- ard[ard$x > 1, , drop = FALSE]", "",
                                            "content <- nrow(data)", "content <- content * 10"))
  expect_identical(tfl_content_run(out, data.frame(x = 1:3)), 20)
  out$content <- tfl_r("plan <- 1")
  expect_error(tfl_content_run(out, data.frame(x = 1)), "leaves no `content`")
})

test_that("a data step of the report's own code is that code, whatever else data says", {
  out <- tfl_output("F-1", "figure", data = list(
    source = "code", code = tfl_r("df <- ard\ndata <- df[df$x > 1, , drop = FALSE]"),
    steps = list(list(fn = "geom_point"))))
  out$content <- tfl_r("content <- nrow(data)")
  expect_identical(tfl_content_code(out), c("df <- ard", "data <- df[df$x > 1, , drop = FALSE]",
                                            "", "content <- nrow(data)"))
  expect_identical(tfl_content_run(out, data.frame(x = 1:3)), 2L)
})

test_that("a nested variable with no heading keeps its place after its parent, back from the content (#273)", {
  sp <- tfl_table_spec(
    tables = data.frame(output_id = "T-1", cols = "TRT01A", rows = "group = variable"),
    variables = data.frame(output_id = "T-1",
                           variable = c("AGE", "RACE", "ASIANSUB", "ETHNIC"),
                           label = c("Age", "Race", NA, "Ethnicity"),
                           order = c("1", "2", "3", "4"),
                           under = c(NA, NA, "RACE: Asian", NA)),
    cells = data.frame(output_id = "T-1", template = "{n}"))
  items <- .plan_content_from_sheets(sp, "T-1", own = TRUE)
  back <- .plan_sheets_from_content(items, "T-1")$variables
  ord <- stats::setNames(as.character(back$order), back$variable)
  expect_identical(unname(ord[c("AGE", "RACE", "ASIANSUB", "ETHNIC")]), c("1", "2", "3", "4"))
  expect_identical(back$under[back$variable == "ASIANSUB"], "RACE: Asian")
  # two children of one parent: in plan_nest's order, both after it
  v <- list(A = list(variable = "A", order = 1L), B = list(variable = "B", order = 2L),
            C1 = list(variable = "C1", under = "A: x"), C2 = list(variable = "C2", under = "A: y"))
  o <- .nest_order(v)
  expect_identical(vapply(o, `[[`, 0L, "order"), c(A = 1L, B = 4L, C1 = 2L, C2 = 3L))
})

test_that("the recommended order: the mapping first, then the finishing (rank and stage)", {
  s <- .schema_all(figure = FALSE)
  verbs <- c("normalize_ard", "table_plan", "plan_cells", "plan_stat_groups", "plan_total",
             "plan_compare", "plan_filter", "plan_labels", "plan_levels", "plan_nest",
             "plan_digits", "plan_sort", "plan_col_header", "plan_stub", "plan_blanks",
             "plan_paginate_rows", "plan_paginate_group", "plan_paginate_cols", "plan_row_group",
             "plan_hide", "plan_columns", "plan_style", "plan_cell_style")
  ranks <- vapply(verbs, function(v) as.numeric(s[[v]]$rank), 0)
  expect_false(is.unsorted(ranks, strictly = TRUE))
  st <- vapply(verbs, function(v) tfl_schema(v)$stage, "")
  expect_true(all(st[1:7] == "mapping"))
  expect_true(all(st[-(1:7)] == "finishing"))
  # every figure function says its stage: the plot and layers, the settings, the output
  fig <- Filter(function(e) !is.null(e$figure), s)
  expect_true(all(vapply(fig, function(e) e$stage %in% c("mapping", "finishing", "output"), NA)))
  expect_identical(tfl_schema("tfl_image")$stage, "output")
  expect_identical(tfl_schema("scale_x_continuous")$stage, "finishing")
  expect_identical(tfl_schema("geom_line")$stage, "mapping")
  # the bridge writes the verbs in that order
  sp <- tfl_table_spec(
    tables = data.frame(output_id = "T", cols = "TRT", rows = "group = variable", sort = "-n",
                        total = "Total"),
    variables = data.frame(output_id = "T", variable = "AGE", label = "Age"),
    cells = data.frame(output_id = "T", template = "{n}"))
  fns <- vapply(.plan_content_from_sheets(sp, "T"), `[[`, "", "fn")
  r <- vapply(fns, function(f) as.numeric(s[[f]]$rank), 0)
  expect_false(is.unsorted(r))
  expect_true(match("plan_cells", fns) < match("plan_labels", fns))
  expect_true(match("plan_total", fns) < match("plan_sort", fns))
})
