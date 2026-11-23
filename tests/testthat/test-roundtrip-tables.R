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
  sh <- sh %||% suppressMessages(tfl_sheets_from_yaml(d))
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
rt_key <- function(items) {
  vapply(items, function(it) paste(it$fn, paste(deparse(rt_canon(it$args)), collapse = " ")), "")
}

rt_report <- function(d, id) suppressMessages(tfl_read_outputs(d))[[id]]

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
  expect_identical(rt_canon(b$data), rt_canon(a$data), info = info)
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
  # (data steps other than one R step: their YAML on the planner row)
  sh <- suppressMessages(tfl_sheets_from_yaml(rt_study(list(rt_cases$listing_steps))))
  pc <- sh$planner$process_code[sh$planner$output_id %in% "L-1"]
  expect_match(pc, "^- !r ")
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
# form a study keeps it in), then through the sheets.  Two differences
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
    sh <- suppressMessages(suppressWarnings(tfl_sheets_from_spec(s, list(`X-1` = e))))
    to <- withr::local_tempdir()
    suppressMessages(suppressWarnings(tfl_yaml_from_sheets(sh, to, overwrite = TRUE)))
    b <- suppressMessages(suppressWarnings(tfl_read_outputs(to)))[["X-1"]]
    expect_setequal(rt_key(b$content), rt_key(e$content))
    expect_identical(length(b$content), length(e$content), info = nm)
    expect_false(any(grepl("does not come back", sh$not_converted$reason, fixed = TRUE)), info = nm)
    if (nm %in% names(rt_open)) next
    expect_identical(rt_canon(b$data), rt_canon(e$data), info = nm)
    code <- function(x, st) suppressMessages(tfl_content_code(tfl_resolve_output(x, st), pipe = "|>"))
    expect_identical(code(b, tfl_read_study_spec(to)), code(e, s), info = nm)
  }
})
