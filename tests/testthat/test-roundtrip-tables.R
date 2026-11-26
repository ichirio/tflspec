# YAML -> sheets -> YAML keeps a table's and a listing's keys (#689, the
# issue's first table, "Tables and listings").  Each case: the report read
# back has the same content (its items and their arguments) and data, and
# the program it makes is the same text -- in memory and through the
# workbooks.  The keys are kept because the sheets are read back and
# compared (.sheets_said()), not because of a list of their names.  Then
# every table and listing template, expanded with its example arguments.

rt_study <- function(outputs, env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  dir.create(file.path(d, "spec", "outputs"), recursive = TRUE)
  writeLines(c("study_id: RT-689", "title: rt689", "settings:", "  rounding: sas",
               "  output_path: output/tfl", "  program_dir: programs/tfl",
               "  file: '{output_id}.rtf'", "keys:", "  id: USUBJID"),
             file.path(d, "study.yml"))
  writeLines(c("datasets:", "  - dataset: ADSL", "    level: ADaM", "    path: adam/adsl.xpt",
               "populations:", "  - id: SAF", "    dataset: ADSL",
               "    where: !r SAFFL == \"Y\"", "    label: Safety Population"),
             file.path(d, "spec", "data.yml"))
  writeLines("content: []", file.path(d, "spec", "defaults.yml"))
  for (o in outputs) tfl_write_output(o, file.path(d, "spec", "outputs", paste0(o$id, ".yml")))
  d
}

rt_table <- function(content, steps = NULL) {
  data <- list(source = "ard", object = "ard")
  if (!is.null(steps)) data$steps <- steps
  tfl_output("T-1", "table", label = "One", common = list(population = "SAF"), extends = "defaults",
             data = data, content = content)
}

rt_listing <- function(steps) {
  tfl_output("L-1", "listing", label = "Subjects", common = list(population = "SAF"),
             extends = "defaults",
             data = list(source = "dataset", dataset = "ADSL", steps = steps),
             content = list(tfl_item("plan_listing", list(cols = list(
               tfl_call("listing_col", list(vars = "USUBJID")),
               tfl_call("listing_col", list(vars = "AGE")))))))
}

rt_head <- tfl_item("table_plan", list(cols = "TRT01A", rows = list(group = "variable")))
rt_nz <- tfl_item("normalize_ard", list())

# the sheets through the workbooks: the table spec's and the listing spec's
# (the planner sheet, tflplanner's, as it is)
rt_book <- function(sh, env = parent.frame()) {
  f <- withr::local_tempfile(fileext = ".xlsx", .local_envir = env)
  keep <- intersect(names(sh$table), c("study", names(tflspec:::.ard_spec_schema())))
  tfl_write_table_spec(tfl_table_spec(sh$table[keep]), f)
  wb <- unclass(tfl_read_table_spec(f))
  sh$table[intersect(names(wb), keep)] <- wb[intersect(names(wb), keep)]
  if (nrow(sh$listing$listings)) {
    g <- withr::local_tempfile(fileext = ".xlsx", .local_envir = env)
    tfl_write_listing_spec(tfl_listing_spec(sh$listing$listings, sh$listing$listing_cols), g)
    lf <- tfl_read_listing_spec(g)
    sh$listing$listings <- lf$listings
    sh$listing$listing_cols <- lf$listing_cols
  }
  sh
}

rt_trip <- function(d, route = "memory", sh = NULL, env = parent.frame()) {
  # (nothing said aloud: a warning is a key that does not come back)
  sh <- sh %||% expect_no_warning(suppressMessages(tfl_sheets_from_yaml(d)))
  if (identical(route, "workbook")) sh <- rt_book(sh, env)
  to <- withr::local_tempdir(.local_envir = env)
  suppressMessages(tfl_yaml_from_sheets(sh, to, overwrite = TRUE))
  list(sh = sh, to = to)
}

# an item as a key: its verb and its arguments as written to YAML, in their
# order; R text as R reads it (a guard's '0' and "0" are one guard)
rt_canon <- function(x) {
  if (inherits(x, "tfl_r")) {
    return(paste(unlist(lapply(parse(text = as.character(x), keep.source = FALSE), deparse)),
                 collapse = " "))
  }
  if (is.list(x)) {
    a <- attributes(x)
    x <- lapply(unclass(x), rt_canon)
    if (!is.null(a$names)) names(x) <- a$names
  }
  x
}
rt_key <- function(items, sorted = FALSE) {
  vapply(items, function(it) {
    a <- rt_canon(it$args)
    # (sorted: the named arguments by name, as the sheets may give them back)
    if (sorted && length(a)) {
      nm <- names(a) %||% rep("", length(a))
      a <- a[order(nzchar(nm), nm)]
    }
    paste(it$fn, paste(deparse(a), collapse = " "))
  }, "")
}

rt_report <- function(d, id) suppressMessages(tfl_read_outputs(d))[[id]]

# a report's data as the sheets give it back: a listing's analysis set is its
# data's own, else `common: population` (the planner row's population
# column, read back into data)
rt_data <- function(o) {
  d <- o$data
  if (identical(o$kind, "listing") && is.list(d) && is.null(d$population) && !is.null(o$common$population)) {
    at <- match("dataset", names(d), nomatch = length(d))
    d <- append(d, list(population = o$common$population), after = at)
  }
  d
}

# a program with each call's named arguments in one order, and its
# statements too: the sheets give a call's named arguments back in their own
# order, which is the same call (and plan_levels()' code lists, at the head,
# follow its arguments)
rt_sorted <- function(code) {
  srt <- function(e) {
    if (!is.call(e)) return(e)
    a <- as.list(e)
    args <- lapply(a[-1L], function(x) if (missing(x)) x else srt(x))
    nm <- names(args) %||% rep("", length(args))
    o <- order(nzchar(nm), nm)
    as.call(c(list(srt(a[[1L]])), args[o]))
  }
  ex <- parse(text = code, keep.source = FALSE)
  sort(vapply(ex, function(e) paste(deparse(srt(e)), collapse = "\n"), ""))
}

rt_code <- function(d, id) {
  out <- tfl_resolve_output(rt_report(d, id), tfl_read_study_spec(d))
  suppressMessages(tfl_content_code(out, pipe = "|>"))
}

# the report read back is the report: its items (as a set: the reader puts
# normalize_ard() first), its data, and its program as text
rt_same <- function(d, to, id, info) {
  a <- rt_report(d, id)
  b <- rt_report(to, id)
  expect_setequal(rt_key(b$content), rt_key(a$content))
  expect_identical(length(b$content), length(a$content), info = info)
  expect_identical(rt_canon(b$data), rt_canon(rt_data(a)), info = info)
  expect_identical(rt_code(to, id), rt_code(d, id), info = info)
}

rt_cases <- list(
  columns = rt_table(list(rt_nz, rt_head, tfl_item("plan_columns", list(
    column_widths_twips = list(3000L, 1500L, 1500L), cell_format = tfl_r("function(x) toupper(x)"))))),
  columns_with_widths = rt_table(list(rt_nz, rt_head, tfl_item("plan_columns", list(
    widths = list(row_label = 3, .values = 2), column_widths_twips = list(3000L, 1500L, 1500L),
    cell_format = tfl_r("function(x) toupper(x)"))))),
  col_header = rt_table(list(rt_nz, rt_head, tfl_item("plan_col_header", list(
    span = "TRT01A", header_sep = " / ")))),
  normalize_ard = rt_table(list(tfl_item("normalize_ard", list(
    keys = list("TRT01A"), hierarchy = list("AEBODSYS", "AEDECOD", "AESEV"),
    drop_contexts = list("attributes"), drop_key_variables = TRUE)), rt_head)),
  normalize_ard_empty = rt_table(list(rt_nz, rt_head)),
  table_steps = rt_table(list(rt_nz, rt_head),
                         steps = list(tfl_r("data <- data[data$variable != \"AGE\", ]"),
                                      tfl_r("data <- data[order(data$variable), ]"))),
  paginate_cols = rt_table(list(rt_nz, rt_head, tfl_item("plan_paginate_cols", list(
    every = 3L, col_header = tfl_r("rtfreporter::rtf_col_header(c(\"A\", \"B\", \"C\"))"))))),
  labels_two = rt_table(list(rt_nz, rt_head, tfl_item("plan_labels", list(AGE = "Age (y)")),
                             tfl_item("plan_labels", list(AGE = "Age, years", SEX = "Sex")))),
  labels_two_variable = rt_table(list(rt_nz, rt_head,
                                      tfl_item("plan_labels", list(variable = list(AGE = "Age (y)"))),
                                      tfl_item("plan_labels", list(variable = list(SEX = "Sex"))))),
  listing_step = rt_listing(list(tfl_r("data <- data[data$AGE >= 18, ]"))),
  listing_steps = rt_listing(list(tfl_r("data <- data[data$AGE >= 18, ]"),
                                  tfl_r("data <- data[order(data$AGE), ]"))))

test_that("each key of the issue's first table comes back, and the program is the same text", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("readxl")
  for (nm in names(rt_cases)) {
    o <- rt_cases[[nm]]
    d <- rt_study(list(o))
    for (route in c("memory", "workbook")) {
      r <- rt_trip(d, route)
      rt_same(d, r$to, o$id, info = paste(nm, route))
      # (kept, so nothing is said lost)
      expect_false(any(r$sh$not_converted$output %in% o$id), info = paste(nm, route))
    }
  }
})

test_that("what the sheets cannot say is in content_more, what they can is on them", {
  d <- rt_study(list(rt_cases$columns_with_widths))
  sh <- suppressMessages(tfl_sheets_from_yaml(d))
  tb <- sh$table$tables
  cm <- tb$content_more[tb$output_id %in% "T-1"]
  expect_match(cm, "column_widths_twips", fixed = TRUE)
  expect_match(cm, "cell_format", fixed = TRUE)
  expect_false(grepl("widths:\n", cm, fixed = TRUE))
  cl <- sh$table$columns
  expect_identical(cl$rel_width[cl$output_id %in% "T-1"], c("3", "2"))
  # (a normalize_ard() with arguments no column says: in content_more)
  sh <- suppressMessages(tfl_sheets_from_yaml(rt_study(list(rt_cases$normalize_ard))))
  tb <- sh$table$tables
  expect_match(tb$content_more[tb$output_id %in% "T-1"], "drop_key_variables: true", fixed = TRUE)
  # (two R steps: R on the planner row, a separator line between them)
  sh <- suppressMessages(tfl_sheets_from_yaml(rt_study(list(rt_cases$listing_steps))))
  pc <- sh$planner$process_code[sh$planner$output_id %in% "L-1"]
  expect_identical(pc, paste("data <- data[data$AGE >= 18, ]", "# ---- step ----",
                             "data <- data[order(data$AGE), ]", sep = "\n"))
  expect_length(parse(text = pc), 2L)
  sh <- suppressMessages(tfl_sheets_from_yaml(rt_study(list(rt_cases$listing_step))))
  expect_identical(sh$planner$process_code[sh$planner$output_id %in% "L-1"],
                   "data <- data[data$AGE >= 18, ]")
})

test_that("a cell edited after writing still takes effect next to content_more", {
  d <- rt_study(list(rt_cases$columns_with_widths))
  sh <- suppressMessages(tfl_sheets_from_yaml(d))
  cl <- sh$table$columns
  cl$rel_width[cl$output_id %in% "T-1" & cl$column %in% "row_label"] <- "4"
  sh$table$columns <- cl
  r <- rt_trip(d, sh = sh)
  pc <- Filter(function(it) identical(it$fn, "plan_columns"), rt_report(r$to, "T-1")$content)
  expect_length(pc, 1L)
  expect_equal(pc[[1L]]$args$widths$row_label, 4)
  expect_identical(names(pc[[1L]]$args), c("widths", "column_widths_twips", "cell_format"))
})

# ---- every table and listing template ----------------------------------------
# Expanded with its example arguments, written to YAML and read back (the
# form a study keeps it in), then through the sheets.  The program is the
# same but for the order of a call's named arguments (the shift tables'
# plan_levels() come back in the sheets' order).  Two differences
# belong to the issue's other tables and are left out of the program's
# comparison: a records table's data (pk_conc, "Data of a table") and a
# report's own `style:` (stf_t02_demog, "Shared files").
rt_open <- c(pk_conc = "data of a records table (#689, Data of a table)",
             stf_t02_demog = "a report's style: (#689, Shared files)")

test_that("every table and listing template comes back as it was expanded", {
  skip_if_not_installed("openxlsx")
  s <- tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))
  tt <- tfl_templates()
  tt <- tt[tt$kind %in% c("table", "listing"), , drop = FALSE]
  expect_gt(nrow(tt), 20L)
  for (i in seq_len(nrow(tt))) {
    nm <- tt$name[i]
    ex <- tflspec:::.tpl_get(nm, "test")$example %||% tflspec:::.tpl_examples[[nm]] %||% list()
    pop <- ex$population %||% "SAF"
    ex$population <- NULL
    o <- tfl_output("X-1", tt$kind[i], label = nm, common = list(population = pop),
                    template = list(name = nm, args = ex))
    e <- suppressMessages(suppressWarnings(tfl_template_to_spec(o, s)))
    f <- withr::local_tempfile(fileext = ".yml")
    tfl_write_output(e, f)
    e <- suppressMessages(tfl_read_output(f))
    # (each template makes content; the sheets say nothing aloud)
    expect_gt(length(e$content), 0L)
    sh <- expect_no_warning(suppressMessages(tfl_sheets_from_spec(s, list(`X-1` = e))), message = nm)
    to <- withr::local_tempdir()
    suppressMessages(suppressWarnings(tfl_yaml_from_sheets(sh, to, overwrite = TRUE)))
    b <- suppressMessages(suppressWarnings(tfl_read_outputs(to)))[["X-1"]]
    expect_setequal(rt_key(b$content, sorted = TRUE), rt_key(e$content, sorted = TRUE))
    expect_identical(length(b$content), length(e$content), info = nm)
    expect_false(any(grepl("does not come back|the sheets give it", sh$not_converted$reason)), info = nm)
    if (nm %in% names(rt_open)) next
    expect_identical(rt_canon(b$data), rt_canon(rt_data(e)), info = nm)
    code <- function(x, st) suppressMessages(tfl_content_code(tfl_resolve_output(x, st), pipe = "|>"))
    expect_identical(rt_sorted(code(b, tfl_read_study_spec(to))), rt_sorted(code(e, s)), info = nm)
  }
})

test_that("what does not come back even as YAML is said, by report and verb", {
  # (a bridge that writes an item the report does not have: the check reads
  # it back and says so, it is not taken silently)
  extra <- function(items, id) {
    tflspec:::.plan_sheets_from_content(c(items, list(tfl_item("plan_total", list(label = "All")))), id)
  }
  r <- tflspec:::.sheets_said(list(rt_head), list(), "T-1", extra)
  expect_match(r$unkept, "plan_total(): the sheets give it", fixed = TRUE)
  expect_true(all(r$unkept %in% r$notes))
  # (the package's bridge: nothing to say for a plain table)
  r <- tflspec:::.sheets_said(list(rt_head), list(), "T-1", tflspec:::.plan_sheets_from_content)
  expect_length(r$unkept, 0L)
})

# ---- the forms the bridge writes, read back as written (review M1) -----------
# Header `lines` are written as header rows and plan_labels(AGE = ) as the
# variables sheet's labels: they stay in the grids, content_more marks the
# form (`lines: !sheet`), and an edit in the grid is the value.

rt_sample <- function(env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  file.copy(list.files(system.file("extdata", "spec-yaml", package = "tflspec"), full.names = TRUE),
            d, recursive = TRUE)
  d
}
rt_items <- function(d, id, fn) {
  x <- if (is.na(id)) suppressMessages(tfl_read_study_spec(d))$defaults$content else rt_report(d, id)$content
  Filter(function(it) identical(it$fn, fn), x)
}

test_that("the sample's header and labels stay in the grids, and the program is the same text", {
  d <- rt_sample()
  sh <- expect_no_warning(suppressMessages(tfl_sheets_from_yaml(d)))
  h <- sh$table$col_header
  expect_identical(h$text[is.na(h$output_id)], c(NA, "{col}", "Characteristic", "(N={n})"))
  expect_identical(h$span[is.na(h$output_id)], rep("each", 4L))
  v <- sh$table$variables
  expect_identical(v$label[v$output_id %in% "T-14-1-1"],
                   c("Age (years)", "Age group, n (%)", "Sex, n (%)", "Race, n (%)", "Baseline ECOG, n (%)"))
  tb <- sh$table$tables
  cm <- tb$content_more[tb$output_id %in% "T-14-1-1"]
  expect_match(cm, "AGE: !sheet", fixed = TRUE)
  expect_false(grepl("Age (years)", cm, fixed = TRUE))
  expect_identical(nrow(sh$not_converted[!sh$not_converted$part %in% c("codelists", "adam_meta"), ]), 0L)
  for (route in c("memory", "workbook")) {
    r <- rt_trip(d, route)
    # (as a set: the sheets give the items in the plan's order)
    expect_setequal(rt_key(rt_report(r$to, "T-14-1-1")$content), rt_key(rt_report(d, "T-14-1-1")$content))
    # (the defaults as written; the converter puts its normalize_ard() first)
    def <- function(x) Filter(function(it) !identical(it$fn, "normalize_ard"),
                              suppressMessages(tfl_read_study_spec(x))$defaults$content)
    expect_identical(rt_key(def(r$to)), rt_key(def(d)))
  }
  # the program: the sample's two forms in a study whose defaults say
  # their normalize_ard(), the same text after the round trip
  d2 <- rt_study(list(rt_table(list(rt_nz, rt_head, tfl_item("plan_labels", list(
    AGE = "Age (years)", SEX = "Sex, n (%)"))))))
  writeLines(c("content:", "  - normalize_ard: {}",
               grep("^content:", readLines(file.path(d, "defaults.yml")), value = TRUE, invert = TRUE)),
             file.path(d2, "spec", "defaults.yml"))
  for (route in c("memory", "workbook")) rt_same(d2, rt_trip(d2, route)$to, "T-1", info = route)
})

test_that("a grid edit of a header row or a label takes effect: one form, never header + lines", {
  d <- rt_sample()
  sh <- suppressMessages(tfl_sheets_from_yaml(d))
  h <- sh$table$col_header
  h$text[is.na(h$output_id) & h$line %in% "2" & h$cols %in% "row_label"] <- "Parameter"
  sh$table$col_header <- h
  v <- sh$table$variables
  v$label[v$output_id %in% "T-14-1-1" & v$variable %in% "AGE"] <- "Age, years"
  # (a row added in the grid: a heading of `variable`)
  v[nrow(v) + 1L, ] <- NA
  v$output_id[nrow(v)] <- "T-14-1-1"
  v$variable[nrow(v)] <- "BMIBL"
  v$label[nrow(v)] <- "BMI"
  sh$table$variables <- v
  for (route in c("memory", "workbook")) {
    r <- rt_trip(d, route, sh = sh)
    hd <- rt_items(r$to, NA, "plan_col_header")
    expect_length(hd, 1L)
    expect_identical(names(hd[[1L]]$args), "lines")
    expect_identical(hd[[1L]]$args$lines[[2L]]$row_label, "Parameter")
    expect_identical(hd[[1L]]$args$lines[[1L]]$.values, "{col}")
    lb <- rt_items(r$to, "T-14-1-1", "plan_labels")
    expect_length(lb, 1L)
    expect_identical(lb[[1L]]$args$AGE, "Age, years")
    expect_identical(lb[[1L]]$args$SEX, "Sex, n (%)")
    expect_identical(lb[[1L]]$args$variable$BMIBL, "BMI")
    expect_false("AGE" %in% names(lb[[1L]]$args$variable))
    # (the report as the plan reads it: the defaults' header once, in one form)
    out <- tfl_resolve_output(rt_report(r$to, "T-14-1-1"), tfl_read_study_spec(r$to))
    ch <- Filter(function(it) identical(it$fn, "plan_col_header"), out$content)
    expect_false(any(vapply(ch, function(it) all(c("header", "lines") %in% names(it$args)), NA)))
  }
  # a header row deleted in the grid: the header is the rows left
  h <- h[!(is.na(h$output_id) & h$line %in% "1"), ]
  sh$table$col_header <- h
  hd <- rt_items(rt_trip(d, sh = sh)$to, NA, "plan_col_header")
  expect_length(hd[[1L]]$args$lines, 1L)
})

test_that("an integer header line, and labels in both forms, come back as written", {
  hd <- tfl_item("plan_col_header", list(header = list(list(line = 1L, cols = "row_label", text = "Parameter"),
                                                       list(line = 1L, cols = ".values", text = "{col}"))))
  lb <- tfl_item("plan_labels", list(variable = list(SEX = "Sex"), AGE = "Age"))
  d <- rt_study(list(rt_table(list(rt_nz, rt_head, lb, hd))))
  r <- rt_trip(d)
  expect_identical(rt_items(r$to, "T-1", "plan_col_header")[[1L]]$args$header[[1L]]$line, 1L)
  rt_same(d, r$to, "T-1", info = "header line, labels")
  # (both headings on the variables sheet: only AGE's form is marked)
  v <- r$sh$table$variables
  expect_identical(v$label[v$output_id %in% "T-1"], c("Sex", "Age"))
  cm <- r$sh$table$tables$content_more[r$sh$table$tables$output_id %in% "T-1"]
  expect_identical(cm, paste0("- normalize_ard: {}\n- plan_labels:\n    AGE: !sheet ''\n",
                              "- plan_col_header:\n    header: !sheet ''"))
})

# ---- data steps: process_code stays R (review M2) ------------------------------

test_that("several R steps are R in process_code, a separator line between them", {
  st <- list(tfl_r("data <- data[data$AGE >= 18, ]"), tfl_r("data <- data[order(data$AGE), ]"))
  expect_identical(tflspec:::.steps_cell(st), paste(
    "data <- data[data$AGE >= 18, ]", "# ---- step ----", "data <- data[order(data$AGE), ]", sep = "\n"))
  expect_identical(lapply(tflspec:::.steps_from_cell(tflspec:::.steps_cell(st)), as.character),
                   lapply(st, as.character))
  # (one step: as it was; a bare tfl_r is one step too)
  expect_identical(tflspec:::.steps_cell(st[1L]), "data <- data[data$AGE >= 18, ]")
  expect_identical(tflspec:::.steps_cell(st[[1L]]), "data <- data[data$AGE >= 18, ]")
  expect_identical(tflspec:::.steps_from_cell("x <- 1\r\ny <- 2"), list(tfl_r("x <- 1\ny <- 2")))
  # (an edit adding a step, as R: three steps)
  cell <- paste(tflspec:::.steps_cell(st), "# ---- step ----", "data$X <- 1", sep = "\n")
  expect_length(tflspec:::.steps_from_cell(cell), 3L)
  # the table's and the listing's: R in the cell, the same steps after the trip
  for (nm in c("table_steps", "listing_steps")) {
    o <- rt_cases[[nm]]
    d <- rt_study(list(o))
    r <- rt_trip(d)
    pc <- r$sh$planner$process_code[r$sh$planner$output_id %in% o$id]
    expect_length(parse(text = pc), 2L)
    rt_same(d, r$to, o$id, info = nm)
  }
})

test_that("a step that is not R: process_code blank, the steps whole in content_more, said", {
  st <- list(tfl_r("data <- data[data$AGE >= 18, ]"), list(op = "filter", where = "AGE > 18"))
  for (o in list(rt_listing(st), rt_table(list(rt_nz, rt_head), steps = st))) {
    d <- rt_study(list(o))
    r <- rt_trip(d)
    expect_true(is.na(r$sh$planner$process_code[r$sh$planner$output_id %in% o$id]), info = o$kind)
    cm <- if (o$kind == "listing") r$sh$listing$listings$content_more else
      r$sh$table$tables$content_more[r$sh$table$tables$output_id %in% o$id]
    expect_match(cm, "- rework:", fixed = TRUE)
    expect_match(r$sh$not_converted$reason[r$sh$not_converted$output %in% o$id], "a step that is not R",
                 fixed = TRUE)
    expect_identical(rt_canon(rt_report(r$to, o$id)$data), rt_canon(rt_data(rt_report(d, o$id))), info = o$kind)
    expect_identical(rt_key(rt_report(r$to, o$id)$content), rt_key(rt_report(d, o$id)$content), info = o$kind)
  }
})

# ---- the check reads the sheets as they are returned (review M3) ----------------

test_that("the defaults' plan_digits(rounding = ) over study.yml's: the study keeps it, said", {
  d <- rt_study(list(rt_table(list(rt_nz, rt_head))))
  writeLines(c("content:", "  - plan_digits: {rounding: r}"), file.path(d, "spec", "defaults.yml"))
  r <- rt_trip(d)
  st <- r$sh$table$study
  expect_identical(st$value[st$key == "rounding"], "r")
  expect_match(r$sh$not_converted$reason, "settings rounding: sas; the defaults' content says r", fixed = TRUE)
  s2 <- suppressMessages(tfl_read_study_spec(r$to))
  expect_identical(rt_key(rt_items(r$to, NA, "plan_digits")), rt_key(rt_items(d, NA, "plan_digits")))
  expect_identical(tflspec:::.study_rounding(s2), "r")
  expect_identical(tflspec:::.study_rounding(suppressMessages(tfl_read_study_spec(d))), "r")
  # (study.yml's rounding alone, defaults with other items: nothing said)
  writeLines(c("content:", "  - plan_stub: {name: row_label}"), file.path(d, "spec", "defaults.yml"))
  r <- rt_trip(d)
  expect_identical(nrow(r$sh$not_converted), 0L)
})

# ---- an argument on both sides, a reader error, holding (review S1-S3) --------

test_that("an argument on both sides is one: the grid's edit wins, and that is said", {
  d <- rt_study(list(rt_table(list(rt_nz, rt_head, tfl_item("plan_columns", list(
    widths = list(row_label = 3), sep = " / "))))))
  sh <- suppressMessages(tfl_sheets_from_yaml(d))
  tb <- sh$table$tables
  i <- which(tb$output_id %in% "T-1")
  expect_match(tb$content_more[i], "sep: ' / '", fixed = TRUE)
  expect_true(is.na(tb$sep[i]))
  rt_same(d, rt_trip(d, sh = sh)$to, "T-1", info = "sep kept")
  tb$sep[i] <- "|"
  sh$table$tables <- tb
  r <- rt_trip(d, sh = sh)
  pc <- rt_items(r$to, "T-1", "plan_columns")
  expect_length(pc, 1L)
  expect_identical(pc[[1L]]$args$sep, "|")
  nc <- readLines(file.path(r$to, "spec", "_not_converted.yml"))
  expect_true(any(grepl("plan_columns(sep): on the sheets and in content_more", nc, fixed = TRUE)))
})

test_that("a reader error is said, and only the items it comes from are kept whole", {
  bad <- function(items, id) {
    sp <- tflspec:::.plan_sheets_from_content(items, id)
    if (any(vapply(items, function(it) identical(it$fn, "plan_total"), NA))) sp$tables$total_position <- "nowhere"
    sp
  }
  items <- list(rt_head, tfl_item("plan_total", list(label = "All")),
                tfl_item("plan_columns", list(widths = list(row_label = 3))))
  r <- tflspec:::.sheets_said(items, list(), "T-1", bad)
  expect_identical(vapply(r$more, function(it) it$fn, ""), "plan_total")
  expect_match(r$notes, "could not be read back from the sheets (`tables$total_position`", fixed = TRUE)
  expect_length(r$unkept, 0L)
  expect_identical(r$sp$columns$rel_width, "3")
})

test_that("only the arguments that differ are held; two unwritable values are not one", {
  said <- function(items) tflspec:::.sheets_said(items, list(), "T-1", tflspec:::.plan_sheets_from_content)
  r <- said(list(rt_head, tfl_item("plan_columns", list(widths = list(row_label = 3), sep = " / ",
                                                        decimal = ".values"))))
  expect_identical(lapply(r$more, function(it) names(it$args)), list("sep"))
  expect_identical(r$sp$columns$rel_width[r$sp$columns$column %in% "row_label"], "3")
  # (the argument missing from what is read back first: `AGE`, a text a cell
  # does not keep, is held; `variable` then comes back and stays on the sheet)
  r <- said(list(rt_head, tfl_item("plan_labels", list(variable = list(SEX = "Sex"), AGE = "Age "))))
  expect_identical(lapply(r$more, function(it) it$args), list(list(AGE = "Age ")))
  expect_identical(r$sp$variables$label, "Sex")
  # (two values the program cannot write, with the same error: not the same)
  code <- tflspec:::.sheets_code
  expect_match(code("plan_labels", list(AGE = quote(nofn(1)))), "^<unwritable>")
  expect_false(identical(code("plan_labels", list(AGE = quote(nofn(1)))),
                         code("plan_labels", list(AGE = quote(nofn(2))))))
  expect_identical(code("plan_labels", list(AGE = quote(nofn(1)))), code("plan_labels", list(AGE = quote(nofn(1)))))
})

test_that("normalize_ard(stack_sep = ) with no stack: no stack cell (no partial match)", {
  o <- rt_table(list(tfl_item("normalize_ard", list(stack_sep = " - ")), rt_head))
  d <- rt_study(list(o))
  r <- rt_trip(d)
  tb <- r$sh$table$tables
  expect_true(is.null(tb$stack) || is.na(tb$stack[tb$output_id %in% "T-1"]))
  expect_match(tb$content_more[tb$output_id %in% "T-1"], "stack_sep: ' - '", fixed = TRUE)
  rt_same(d, r$to, "T-1", info = "stack_sep")
})
