# plan_columns(), plan_col_header() and plan_labels() with an argument no
# sheet has a column for: what the columns can say stays on the sheets' rows,
# and the tables row's content_more holds only the rest (#689).  Each case
# goes YAML -> sheets -> YAML, in memory and through a workbook, and the
# table it builds is the original's.

trc_head <- tfl_item("table_plan", list(cols = "ARM", rows = list(group = "variable")))
trc_cf <- tfl_r("function(x) paste0(x, '!')")

trc_ard <- function() {
  adsl <- cards::ADSL[cards::ADSL$ARM %in% c("Placebo", "Xanomeline High Dose"), ]
  cards::ard_stack(
    adsl, .by = "ARM",
    cards::ard_continuous(variables = c("AGE", "BMIBL"),
                          statistic = ~ cards::continuous_summary_fns(c("N", "mean", "sd"))),
    cards::ard_categorical(variables = "SEX"),
    .overall = FALSE, .missing = FALSE, .attributes = TRUE, .total_n = FALSE)
}

# the same with the sexes as a row key of their own: SEX is a data column, which has value labels
trc_ard_keyed <- function() {
  adsl <- cards::ADSL[cards::ADSL$ARM %in% c("Placebo", "Xanomeline High Dose"), ]
  cards::ard_stack(
    adsl, .by = c("ARM", "SEX"),
    cards::ard_continuous(variables = c("AGE", "BMIBL"),
                          statistic = ~ cards::continuous_summary_fns(c("N", "mean", "sd"))),
    .overall = FALSE, .missing = FALSE, .attributes = TRUE, .total_n = FALSE)
}
trc_head_keyed <- tfl_item("table_plan", list(cols = "ARM", rows = list(group = "variable", sex = "SEX")))

trc_study <- function(content, env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  dir.create(file.path(d, "spec", "outputs"), recursive = TRUE)
  writeLines(c("study_id: T-689", "title: t689", "settings:", "  rounding: sas",
               "  output_path: output/tfl", "  program_dir: programs/tfl",
               "  file: '{output_id}.rtf'", "keys:", "  id: USUBJID"),
             file.path(d, "study.yml"))
  writeLines(c("datasets:", "  - dataset: ADSL", "    level: ADaM", "    path: adam/adsl.xpt",
               "populations:", "  - id: SAF", "    dataset: ADSL",
               "    where: !r SAFFL == \"Y\"", "    label: Safety Population"),
             file.path(d, "spec", "data.yml"))
  y <- list(content = tflspec:::.items_to_list(list(tfl_item("normalize_ard", list()))))
  writeLines(yaml::as.yaml(y, indent.mapping.sequence = TRUE, handlers = tflspec:::.yaml_out_handlers()),
             file.path(d, "spec", "defaults.yml"))
  o <- tfl_output("T-1", "table", label = "One", population = "SAF", extends = "defaults",
                  data = list(source = "ard", object = "ard"), content = content)
  tfl_write_output(o, file.path(d, "spec", "outputs", "T-1.yml"))
  d
}

trc_trip <- function(d, sh = tfl_sheets_from_yaml(d), env = parent.frame()) {
  to <- withr::local_tempdir(.local_envir = env)
  suppressMessages(tfl_yaml_from_sheets(sh, to, overwrite = TRUE))
  list(sh = sh, to = to)
}

# the table sheets through a workbook and back to YAML
trc_book <- function(sh, env = parent.frame()) {
  f <- withr::local_tempfile(fileext = ".xlsx", .local_envir = env)
  keep <- intersect(names(sh$table), c("study", names(tflspec:::.ard_spec_schema())))
  tfl_write_table_spec(tfl_table_spec(sh$table[keep]), f)
  wb <- unclass(tfl_read_table_spec(f))
  sh2 <- sh
  sh2$table[intersect(names(wb), keep)] <- wb[intersect(names(wb), keep)]
  trc_trip(NULL, sh2, env)
}

trc_pages <- function(d, ard) {
  o <- tfl_resolve_output(suppressMessages(tfl_read_outputs(d))[["T-1"]], tfl_read_study_spec(d))
  suppressMessages(rtfreporter::plan_apply(tfl_content_run(o, ard), "pages"))
}

# a built table to compare: its data, header, widths and so on (not the RTF's bytes)
trc_same <- function(a, b, info = NULL) {
  expect_equal(unclass(a[[1L]]), unclass(b[[1L]]), info = info)
}

trc_ard_of <- function(cs) if (isTRUE(cs$keyed)) trc_ard_keyed() else trc_ard()

trc_args <- function(d, fn) {
  its <- Filter(function(it) identical(it$fn, fn), suppressMessages(tfl_read_outputs(d))[["T-1"]]$content)
  lapply(its, function(it) tflspec:::.nz_canon(it$args))
}

trc_cm <- function(sh) {
  tb <- sh$table$tables
  v <- tb$content_more[tb$output_id %in% "T-1"]
  if (length(v) && !is.na(v)) v else NA_character_
}

# a sheet's column for the report, named by its key column (or as it is)
trc_col <- function(sh, sheet, col, by = NULL) {
  d <- sh$table[[sheet]]
  d <- d[d$output_id %in% "T-1", , drop = FALSE]
  if (is.null(by)) d[[col]] else stats::setNames(d[[col]], d[[by]])
}

# the items of the content_more cell as a list
trc_rest <- function(sh) {
  cm <- trc_cm(sh)
  if (is.na(cm)) return(list())
  tflspec:::.sheet_more_in(cm, items = TRUE)
}

trc_route <- function(d, route, env = parent.frame()) {
  sh <- tfl_sheets_from_yaml(d)
  if (identical(route, "memory")) trc_trip(d, sh, env) else trc_book(sh, env)
}

# every case: the items, the cells expected, the rest expected
trc_cases <- list(
  columns_all = list(
    content = list(trc_head, tfl_item("plan_columns", list(
      row_title = "group", decimal = ".values", widths = list(group = 2, label = 3, .values = 2),
      auto_width = FALSE, sep = "____", cell_format = trc_cf))),
    cells = function(sh) {
      expect_identical(trc_col(sh, "columns", "rel_width", "column"),
                       c(group = "2", label = "3", .values = "2"))
      expect_identical(trc_col(sh, "columns", "row_title", "column")[["group"]], "TRUE")
      expect_identical(trc_col(sh, "columns", "decimal_split", "column")[[".values"]], "TRUE")
      expect_identical(trc_col(sh, "tables", "sep"), "____")
      expect_identical(trc_col(sh, "style", "auto_width"), "FALSE")
    },
    rest = list(plan_columns = "cell_format")),
  columns_twips = list(
    content = list(trc_head, tfl_item("plan_columns", list(
      row_title = "group", decimal = ".values", column_widths_twips = list(1000L, 2000L, 1500L, 1500L),
      auto_width = FALSE, sep = "____"))),
    cells = function(sh) {
      expect_identical(trc_col(sh, "columns", "row_title", "column")[["group"]], "TRUE")
      expect_identical(trc_col(sh, "columns", "decimal_split", "column")[[".values"]], "TRUE")
      expect_true(all(is.na(trc_col(sh, "columns", "rel_width"))))
      expect_identical(trc_col(sh, "tables", "sep"), "____")
      expect_identical(trc_col(sh, "style", "auto_width"), "FALSE")
    },
    rest = list(plan_columns = "column_widths_twips")),
  widths_by_position = list(
    content = list(trc_head, tfl_item("plan_columns", list(
      widths = list(2, 3, 2, 2), row_title = "group", decimal = ".values"))),
    cells = function(sh) {
      expect_identical(trc_col(sh, "columns", "row_title", "column")[["group"]], "TRUE")
      expect_identical(trc_col(sh, "columns", "decimal_split", "column")[[".values"]], "TRUE")
      expect_true(all(is.na(trc_col(sh, "columns", "rel_width"))))
    },
    rest = list(plan_columns = "widths")),
  row_title_by_position = list(
    content = list(trc_head, tfl_item("plan_columns", list(
      row_title = list(1L), decimal = ".values", sep = "____"))),
    cells = function(sh) {
      expect_identical(trc_col(sh, "columns", "decimal_split", "column")[[".values"]], "TRUE")
      expect_false(any(trc_col(sh, "columns", "row_title") %in% "TRUE"))
      expect_identical(trc_col(sh, "tables", "sep"), "____")
    },
    rest = list(plan_columns = "row_title")),
  header_sep = list(
    content = list(trc_head, tfl_item("plan_col_header", list(
      header = list(list(line = 1L, cols = "label", text = "Parameter"),
                    list(line = 1L, cols = ".values", text = "{col}")),
      header_sep = " / "))),
    cells = function(sh) {
      expect_identical(trc_col(sh, "col_header", "cols"), c("label", ".values"))
      expect_identical(trc_col(sh, "col_header", "text"), c("Parameter", "{col}"))
    },
    rest = list(plan_col_header = "header_sep")),
  lines_header_sep = list(
    # (the cells carry no span, and a blank one is "one cell over the columns" while
    # `lines` with no `span` is "each": the table differs, as before this change;
    # the next test says so)
    built = FALSE,
    content = list(trc_head, tfl_item("plan_col_header", list(
      lines = list(list(group = "", label = "Parameter", .values = "{col}")),
      header_sep = " / "))),
    cells = function(sh) {
      expect_identical(trc_col(sh, "col_header", "cols"), c("group", "label", ".values"))
      expect_identical(trc_col(sh, "col_header", "text"), c(NA, "Parameter", "{col}"))
    },
    rest = list(plan_col_header = "header_sep")),
  lines_span = list(
    content = list(trc_head, tfl_item("plan_col_header", list(
      lines = list(list(group = "", label = "", .values = "Treatment"),
                   list(group = "", label = "Parameter", .values = "{col}")),
      span = list(list(.values = "one"), list(.values = "each"))))),
    cells = function(sh) {
      expect_identical(trc_col(sh, "col_header", "line"), c("1", "1", "1", "2", "2", "2"))
      expect_identical(trc_col(sh, "col_header", "span"), c("each", "each", NA, "each", "each", "each"))
      expect_identical(trc_col(sh, "col_header", "text"), c(NA, NA, "Treatment", NA, "Parameter", "{col}"))
    },
    rest = list()),
  lines_span_key = list(
    content = list(trc_head, tfl_item("plan_col_header", list(
      lines = list(list(group = "", label = "", .values = "Treatment"),
                   list(group = "", label = "Parameter", .values = "{col}")),
      span = "ARM", header_sep = " / "))),
    cells = function(sh) {
      expect_identical(trc_col(sh, "col_header", "span"), rep("ARM", 6))
    },
    rest = list(plan_col_header = "header_sep")),
  # (a span that is R, not a value the cells can hold: it stays with the lines)
  lines_span_code = list(
    content = list(trc_head, tfl_item("plan_col_header", list(
      col_header_align = "left",
      lines = list(list(group = "", label = "", .values = "Treatment"),
                   list(group = "", label = "Parameter", .values = "{col}")),
      span = tfl_r("list(c(.values = 'one'), 'each')")))),
    cells = function(sh) {
      expect_false(any(sh$table$col_header$output_id %in% "T-1"))
      expect_identical(trc_col(sh, "style", "col_header_align"), "left")
    },
    rest = list(plan_col_header = c("lines", "span"))),
  span_only = list(
    content = list(trc_head, tfl_item("plan_col_header", list(span = "ARM"))),
    cells = function(sh) expect_false(any(sh$table$col_header$output_id %in% "T-1")),
    rest = list(plan_col_header = "span")),
  span_with_align = list(
    content = list(trc_head, tfl_item("plan_col_header", list(span = "ARM", col_header_align = "left"))),
    cells = function(sh) expect_identical(trc_col(sh, "style", "col_header_align"), "left"),
    rest = list(plan_col_header = "span")),
  labels_twice = list(
    content = list(trc_head, tfl_item("plan_labels", list(
      variable = list(AGE = "Age (years)", BMIBL = "BMI"), AGE = "Age at baseline"))),
    cells = function(sh) {
      expect_identical(trc_col(sh, "variables", "label", "variable"), c(AGE = "Age (years)", BMIBL = "BMI"))
    },
    rest = list(plan_labels = "AGE")),
  labels_value_labels = list(
    keyed = TRUE,
    content = list(trc_head_keyed, tfl_item("plan_labels", list(
      variable = list(AGE = "Age (years)", BMIBL = "BMI"), SEX = list(F = "Female", M = "Male")))),
    cells = function(sh) {
      expect_identical(trc_col(sh, "variables", "label", "variable"), c(AGE = "Age (years)", BMIBL = "BMI"))
    },
    rest = list(plan_labels = "SEX")))

test_that("the cells hold what the columns say, content_more only the rest", {
  skip_if_not_installed("cards")
  for (nm in names(trc_cases)) {
    cs <- trc_cases[[nm]]
    sh <- tfl_sheets_from_yaml(trc_study(cs$content))
    cs$cells(sh)
    rest <- trc_rest(sh)
    expect_identical(vapply(rest, function(it) it$fn, ""), as.character(names(cs$rest)), info = nm)
    for (it in rest) {
      expect_identical(sort(names(it$args)), sort(cs$rest[[it$fn]]), info = nm)
    }
    # nothing is lost: no note
    expect_false(any(sh$not_converted$output %in% "T-1"), info = nm)
  }
})

# what the item's arguments are once read back: a header written as `lines` is
# read as its cells (`header`, and the `span` they carry), so those are
# compared by the table they build
trc_norm <- function(args, lines) {
  args[setdiff(names(args), c("header", if (lines) c("lines", "span")))]
}

test_that("round trip: the item's arguments and the table built are the original's, in memory and through a workbook", {
  skip_if_not_installed("cards")
  skip_if_not_installed("openxlsx")
  for (nm in names(trc_cases)) {
    d <- trc_study(trc_cases[[nm]]$content)
    ard <- trc_ard_of(trc_cases[[nm]])
    want <- trc_pages(d, ard)
    # (the lines are the header cells when the sheets have them)
    lines <- any(vapply(trc_cases[[nm]]$content, function(it) "lines" %in% names(it$args), NA)) &&
      any(tfl_sheets_from_yaml(d)$table$col_header$output_id %in% "T-1")
    for (route in c("memory", "workbook")) {
      info <- paste(nm, route)
      r <- trc_route(d, route)
      for (fn in c("plan_columns", "plan_col_header", "plan_labels")) {
        w <- trc_args(d, fn)
        g <- trc_args(r$to, fn)
        # one item where there was one, its arguments the same (the order may differ)
        expect_identical(length(g), length(w), info = paste(info, fn))
        if (length(w)) expect_identical(trc_norm(g[[1L]], lines), trc_norm(w[[1L]], lines), info = paste(info, fn))
      }
      if (!isFALSE(trc_cases[[nm]]$built)) trc_same(want, trc_pages(r$to, ard), info = info)
    }
  }
})

test_that("header lines with no span: the cells say none, as before", {
  skip_if_not_installed("cards")
  ard <- trc_ard()
  d <- trc_study(trc_cases$lines_header_sep$content)
  sh <- tfl_sheets_from_yaml(d)
  expect_true(all(is.na(trc_col(sh, "col_header", "span"))))
  # rtfreporter reads `lines` with no `span` as "each" and a header cell with a blank
  # span as one cell over its columns: the built header differs, with or without the rest
  d2 <- trc_study(list(trc_head, tfl_item("plan_col_header", list(
    lines = list(list(group = "", label = "Parameter", .values = "{col}"))))))
  expect_false(isTRUE(all.equal(unclass(trc_pages(d2, ard)[[1L]])$col_header,
                                unclass(trc_pages(trc_trip(d2)$to, ard)[[1L]])$col_header)))
})

test_that("a cell edited after writing takes effect", {
  skip_if_not_installed("cards")
  ard <- trc_ard()
  # a width
  d <- trc_study(trc_cases$columns_all$content)
  sh <- tfl_sheets_from_yaml(d)
  cl <- sh$table$columns
  cl$rel_width[cl$output_id %in% "T-1" & cl$column %in% "label"] <- "5"
  sh$table$columns <- cl
  r <- trc_trip(d, sh)
  p <- trc_pages(r$to, ard)
  expect_identical(unclass(p[[1L]])$col_rel_width[2L], 5)
  expect_identical(trc_args(r$to, "plan_columns")[[1L]]$cell_format, "function(x) paste0(x, '!')")
  expect_match(paste(unlist(unclass(p[[1L]])$data[[3L]]), collapse = " "), "!", fixed = TRUE)
  # a heading
  d <- trc_study(trc_cases$labels_twice$content)
  sh <- tfl_sheets_from_yaml(d)
  v <- sh$table$variables
  v$label[v$output_id %in% "T-1" & v$variable %in% "BMIBL"] <- "Body mass index"
  sh$table$variables <- v
  r <- trc_trip(d, sh)
  expect_identical(trc_args(r$to, "plan_labels")[[1L]]$variable$BMIBL, "Body mass index")
  expect_identical(trc_args(r$to, "plan_labels")[[1L]]$AGE, "Age at baseline")
  expect_true(any(grepl("Body mass index", unlist(lapply(as.data.frame(trc_pages(r$to, ard)[[1L]]$data), as.character)), fixed = TRUE)))
  # a header cell and its span
  d <- trc_study(trc_cases$lines_span$content)
  sh <- tfl_sheets_from_yaml(d)
  h <- sh$table$col_header
  h$text[h$output_id %in% "T-1" & h$line %in% "1" & h$cols %in% ".values"] <- "Arm"
  sh$table$col_header <- h
  r <- trc_trip(d, sh)
  p <- trc_pages(r$to, ard)
  expect_identical(unclass(p[[1L]])$col_header[[1L]][[3L]]$label, "Arm")
})

test_that("an item with nothing the sheets can say goes whole, as before", {
  skip_if_not_installed("cards")
  for (it in list(tfl_item("plan_columns", list(cell_format = trc_cf)),
                  tfl_item("plan_columns", list(column_widths_twips = list(1000L, 2000L, 1500L, 1500L))),
                  tfl_item("plan_col_header", list(header_sep = " / ")),
                  tfl_item("plan_labels", list(SEX = list(F = "Female", M = "Male"))))) {
    keyed <- identical(it$fn, "plan_labels")
    ard <- if (keyed) trc_ard_keyed() else trc_ard()
    d <- trc_study(list(if (keyed) trc_head_keyed else trc_head, it))
    sh <- tfl_sheets_from_yaml(d)
    expect_identical(vapply(trc_rest(sh), function(x) x$fn, ""), it$fn, info = it$fn)
    expect_identical(names(trc_rest(sh)[[1L]]$args), names(it$args), info = it$fn)
    expect_false(any(sh$not_converted$output %in% "T-1"))
    trc_same(trc_pages(d, ard), trc_pages(trc_trip(d, sh)$to, ard), info = it$fn)
  }
})

test_that("an item with a text a cell would not keep goes whole, so that nothing it says changes", {
  skip_if_not_installed("cards")
  for (it in list(tfl_item("plan_columns", list(sep = " / ", row_title = "group", cell_format = trc_cf)),
                  tfl_item("plan_col_header", list(
                    header = list(list(line = 1L, cols = "label", text = "  Category")), header_sep = " / ")),
                  tfl_item("plan_labels", list(variable = list(AGE = "Age "), SEX = list(F = "Female", M = "Male"))))) {
    keyed <- identical(it$fn, "plan_labels")
    d <- trc_study(list(if (keyed) trc_head_keyed else trc_head, it))
    sh <- tfl_sheets_from_yaml(d)
    rest <- trc_rest(sh)
    expect_identical(vapply(rest, function(x) x$fn, ""), it$fn, info = it$fn)
    expect_identical(names(rest[[1L]]$args), names(it$args), info = it$fn)
    expect_identical(trc_args(trc_trip(d, sh)$to, it$fn)[[1L]], trc_args(d, it$fn)[[1L]], info = it$fn)
  }
})

test_that("a verb in two layers builds the same table as one call", {
  skip_if_not_installed("cards")
  run <- function(ard, content) {
    o <- tfl_output("T-1", "table", data = list(source = "ard"),
                    content = c(list(tfl_item("normalize_ard", list())), content))
    suppressMessages(rtfreporter::plan_apply(tfl_content_run(o, ard), "pages"))
  }
  # plan_columns: the widths and titles, then the cell format
  ard <- trc_ard()
  one <- run(ard, list(trc_head, tfl_item("plan_columns", list(
    row_title = "group", widths = list(group = 2, label = 3, .values = 2), cell_format = trc_cf))))
  two <- run(ard, list(trc_head,
                       tfl_item("plan_columns", list(row_title = "group", widths = list(group = 2, label = 3, .values = 2))),
                       tfl_item("plan_columns", list(cell_format = trc_cf))))
  none <- run(ard, list(trc_head, tfl_item("plan_columns", list(
    row_title = "group", widths = list(group = 2, label = 3, .values = 2)))))
  trc_same(one, two)
  expect_false(isTRUE(all.equal(unclass(one[[1L]]), unclass(none[[1L]]))))
  # plan_col_header: header_sep after a header's alignment; the names are cut at "____", so " / " leaves them whole
  ard2 <- trc_ard_keyed()
  head2 <- tfl_item("table_plan", list(cols = list("ARM", "SEX"), rows = list(group = "variable")))
  one <- run(ard2, list(head2, tfl_item("plan_col_header", list(col_header_align = "left", header_sep = " / "))))
  two <- run(ard2, list(head2, tfl_item("plan_col_header", list(col_header_align = "left")),
                        tfl_item("plan_col_header", list(header_sep = " / "))))
  trc_same(one, two)
  # (and it matters: with no header_sep the names are cut at "____" into a header of two rows)
  only <- run(ard2, list(head2, tfl_item("plan_col_header", list(header_sep = " / "))))
  expect_false(isTRUE(all.equal(unclass(only[[1L]])$col_header, unclass(run(ard2, list(head2))[[1L]])$col_header)))
  # plan_labels: the headings, then a heading named again
  one <- run(ard, list(trc_head, tfl_item("plan_labels", list(
    variable = list(AGE = "Age (years)", BMIBL = "BMI"), AGE = "Age at baseline"))))
  two <- run(ard, list(trc_head,
                       tfl_item("plan_labels", list(variable = list(AGE = "Age (years)", BMIBL = "BMI"))),
                       tfl_item("plan_labels", list(AGE = "Age at baseline"))))
  trc_same(one, two)
})

test_that("two calls: the bridge gives the second's rest, and the reader puts them together", {
  # plan_labels called twice, the second naming a heading again (the YAML reader
  # puts the two into one call; the bridge is given the two as they are)
  items <- list(tfl_item("plan_labels", list(variable = list(AGE = "Age (years)", BMIBL = "BMI"))),
                tfl_item("plan_labels", list(AGE = "Age at baseline", SEX = "Sex")))
  sp <- tflspec:::.plan_sheets_from_content(items, "T-1")
  expect_identical(attr(sp, "not_converted_at"), integer())
  rest <- attr(sp, "not_converted_rest")
  expect_length(rest, 1L)
  expect_identical(rest[[1L]]$args, list(AGE = "Age at baseline"))
  expect_identical(sp$variables$label, c("Age (years)", "BMI", "Sex"))
  # two layers are two items; no argument name shared, so they are read as one
  tb <- data.frame(output_id = "T-1", content_more = tflspec:::.sheet_more_out(rest, items = TRUE))
  sp2 <- tflspec:::.plan_content_more(tflspec:::.plan_content_items(sp), tb)
  lb <- Filter(function(it) identical(it$fn, "plan_labels"), sp2)
  expect_length(lb, 1L)
  expect_identical(names(lb[[1L]]$args), c("variable", "AGE"))
  # a name both have: separate, in order, the later one winning as written
  tb2 <- data.frame(output_id = "T-1", content_more = tflspec:::.sheet_more_out(
    list(tfl_item("plan_labels", list(variable = list(AGE = "x")))), items = TRUE))
  sp3 <- tflspec:::.plan_content_more(tflspec:::.plan_content_items(sp), tb2)
  expect_identical(sum(vapply(sp3, function(it) identical(it$fn, "plan_labels"), NA)), 2L)
  # `variable` is never split: a heading twice inside it sends the item whole
  sp4 <- tflspec:::.plan_sheets_from_content(list(
    tfl_item("plan_labels", list(AGE = "Age at baseline", variable = list(AGE = "Age (years)", BMIBL = "BMI")))), "T-1")
  expect_identical(attr(sp4, "not_converted_at"), 1L)
  expect_length(attr(sp4, "not_converted_rest"), 0L)
})

test_that("widths in a cell and column_widths_twips in the rest: rtfreporter's one message", {
  skip_if_not_installed("cards")
  ard <- trc_ard()
  d <- trc_study(list(trc_head, tfl_item("plan_columns", list(
    row_title = "group", column_widths_twips = list(1000L, 2000L, 1500L, 1500L)))))
  sh <- tfl_sheets_from_yaml(d)
  cl <- sh$table$columns
  cl$rel_width[cl$output_id %in% "T-1" & cl$column %in% "group"] <- "3"
  sh$table$columns <- cl
  r <- trc_trip(d, sh)
  # one item, both widths: rtfreporter says give one
  expect_named(trc_args(r$to, "plan_columns")[[1L]], c("row_title", "widths", "column_widths_twips"), ignore.order = TRUE)
  expect_error(trc_pages(r$to, ard), "`widths` and `column_widths_twips` both set the columns' widths", fixed = TRUE)
})
