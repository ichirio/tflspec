# A table YAML -> sheets -> YAML: an item any of whose arguments the sheets
# cannot say goes whole, as YAML, into the tables row's content_more (not
# into the other sheets as well), and comes back -- nothing lost (#448)

tsm_study <- function(extra = list(), defaults_extra = list(), env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  src <- system.file("extdata", "spec-yaml", package = "tflspec")
  file.copy(list.files(src, full.names = TRUE), d, recursive = TRUE)
  o <- tfl_read_outputs(d)[["T-14-1-1"]]
  # (the items the sheets say whole, and stay as they are)
  o$content <- c(list(
    tfl_item("table_plan", list(cols = "TRT01A", rows = list(group = "variable"))),
    tfl_item("plan_cells", list(continuous = list(n = "{N}", `Mean (SD)` = "{mean} ({sd})"))),
    tfl_item("plan_paginate_rows", list(max_rows = 30L, split = "group_safe"))), extra)
  tfl_write_output(o, file.path(d, "outputs", "T-14-1-1.yml"))
  if (length(defaults_extra)) {
    f <- file.path(d, "defaults.yml")
    y <- yaml::read_yaml(f, handlers = tflspec:::.yaml_handlers())
    y$content <- c(y$content, tflspec:::.items_to_list(defaults_extra))
    writeLines(yaml::as.yaml(y, indent.mapping.sequence = TRUE,
                             handlers = tflspec:::.yaml_out_handlers()), f)
  }
  d
}

tsm_key <- function(items) {
  vapply(items, function(it) paste(it$fn, paste(deparse(it$args[order(names(it$args))]), collapse = "")), "")
}

tsm_back <- function(sh) {
  to <- withr::local_tempdir(.local_envir = parent.frame())
  suppressMessages(tfl_yaml_from_sheets(sh, to, overwrite = TRUE))
  to
}

tsm_unsaid <- list(
  tfl_item("plan_titles", list(titles = list("Safety population"))),
  tfl_item("plan_after", list(steps = tfl_r("function(x) x"))),
  tfl_item("plan_digits", list(continuous = list(N = 0L, mean = 1L))),
  tfl_item("plan_style", list(border_body = tfl_call("rtf_border", list(top = "double")))),
  tfl_item("plan_columns", list(widths = list(3, 1, 1))),
  tfl_item("plan_hide", list(cols = list("A"))),
  tfl_item("plan_hide", list(cols = list("B"))))

test_that("the items the sheets cannot say: whole in content_more, and back", {
  d <- tsm_study(tsm_unsaid)
  sh <- tfl_sheets_from_yaml(d)
  tb <- sh$table$tables
  cm <- tb$content_more[tb$output_id %in% "T-14-1-1"]
  for (fn in c("plan_titles", "plan_after", "plan_digits", "plan_style", "plan_columns")) {
    expect_match(cm, paste0("- ", fn, ":"), fixed = TRUE)
  }
  # plan_hide twice: one call when read (R/plan_repeats.R), its columns
  # added up, said by the columns sheet
  expect_false(grepl("plan_hide", cm, fixed = TRUE))
  cl <- sh$table$columns
  expect_identical(cl$hide[cl$output_id %in% "T-14-1-1" & cl$column %in% c("A", "B")], c("TRUE", "TRUE"))
  # never both: plan_style's border and the widths are not on the other sheets
  st <- sh$table$style
  expect_true(!any(st$output_id %in% "T-14-1-1") ||
                all(is.na(st$border_body[st$output_id %in% "T-14-1-1"])))
  expect_false(any(nzchar(cl$rel_width[cl$output_id %in% "T-14-1-1"] %||% character()) &
                     !is.na(cl$rel_width[cl$output_id %in% "T-14-1-1"])))
  # nothing lost: no note for them
  expect_false(any(sh$not_converted$output %in% "T-14-1-1"))
  back <- tfl_read_outputs(tsm_back(sh))[["T-14-1-1"]]
  expect_setequal(tsm_key(back$content), tsm_key(tfl_read_outputs(d)[["T-14-1-1"]]$content))
  # the content_more items by their verbs' rank: table_plan first
  expect_identical(back$content[[1L]]$fn, "table_plan")
})

test_that("a table_plan() the sheets cannot say goes whole, and is the only one back", {
  d <- tsm_study()
  o <- tfl_read_outputs(d)[["T-14-1-1"]]
  o$content[[1L]] <- tfl_item("table_plan", list(cols = "TRT01A", rows = list(group = "variable"),
                                                 stat = "stat"))
  tfl_write_output(o, file.path(d, "outputs", "T-14-1-1.yml"))
  sh <- tfl_sheets_from_yaml(d)
  tb <- sh$table$tables
  i <- tb$output_id %in% "T-14-1-1"
  expect_match(tb$content_more[i], "- table_plan:", fixed = TRUE)
  expect_true(is.na(tb$cols[i]))
  back <- tfl_read_outputs(tsm_back(sh))[["T-14-1-1"]]
  expect_identical(sum(vapply(back$content, function(it) identical(it$fn, "table_plan"), NA)), 1L)
  expect_setequal(tsm_key(back$content), tsm_key(tfl_read_outputs(d)[["T-14-1-1"]]$content))
})

test_that("the defaults' items the sheets cannot say: on the blank row, and back", {
  d <- tsm_study(defaults_extra = list(tfl_item("plan_footnotes", list(footnotes = list("Source: ADSL")))))
  sh <- tfl_sheets_from_yaml(d)
  tb <- sh$table$tables
  expect_match(tb$content_more[is.na(tb$output_id)], "- plan_footnotes:", fixed = TRUE)
  to <- tsm_back(sh)
  back <- tfl_read_study_spec(to)$defaults$content
  fn <- tfl_read_study_spec(d)$defaults$content
  fn <- Filter(function(it) identical(it$fn, "plan_footnotes"), fn)
  expect_true(all(tsm_key(fn) %in% tsm_key(back)))
  expect_length(fn, 1L)
})

test_that("a table the sheets say whole: no content_more, as before", {
  d <- tsm_study()
  sh <- tfl_sheets_from_yaml(d)
  tb <- sh$table$tables
  expect_true(is.null(tb$content_more) || all(is.na(tb$content_more)))
  back <- tfl_read_outputs(tsm_back(sh))[["T-14-1-1"]]
  expect_identical(tsm_key(back$content), tsm_key(tfl_read_outputs(d)[["T-14-1-1"]]$content))
})

# ---- what the sheets dropped without a word (#689) ---------------------------
# Each case goes YAML -> sheets -> YAML and compares the report's parsed
# content and the code it writes.

# a one-table study: the defaults normalize the ARD (as the converter writes
# them), the report says its data is the ARD
t689_study <- function(content, ard = NULL, defaults = list(tfl_item("normalize_ard", list())),
                       env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  dir.create(file.path(d, "spec", "outputs"), recursive = TRUE)
  writeLines(c("study_id: T-689", "title: t689", "settings:", "  rounding: sas",
               "  output_path: output/tfl", "  program_dir: programs/tfl",
               "  file: '{output_id}.rtf'", "keys:", "  id: USUBJID"),
             file.path(d, "study.yml"))
  writeLines(c("datasets:", "  - dataset: ADSL", "    level: ADaM", "    path: adam/adsl.xpt",
               "  - dataset: ADAE", "    level: ADaM", "    path: adam/adae.xpt",
               "populations:", "  - id: SAF", "    dataset: ADSL",
               "    where: !r SAFFL == \"Y\"", "    label: Safety Population"),
             file.path(d, "spec", "data.yml"))
  y <- list(content = tflspec:::.items_to_list(defaults))
  writeLines(yaml::as.yaml(y, indent.mapping.sequence = TRUE,
                           handlers = tflspec:::.yaml_out_handlers()),
             file.path(d, "spec", "defaults.yml"))
  o <- tfl_output("T-1", "table", label = "One", population = "SAF", extends = "defaults",
                  data = list(source = "ard", object = "ard"), content = content, ard = ard)
  tfl_write_output(o, file.path(d, "spec", "outputs", "T-1.yml"))
  d
}

t689_trip <- function(d, env = parent.frame()) {
  sh <- tfl_sheets_from_yaml(d)
  to <- withr::local_tempdir(.local_envir = env)
  suppressMessages(tfl_yaml_from_sheets(sh, to, overwrite = TRUE))
  list(sh = sh, to = to)
}

t689_code <- function(d, id = "T-1") {
  out <- tfl_resolve_output(suppressMessages(tfl_read_outputs(d))[[id]], tfl_read_study_spec(d))
  suppressMessages(tfl_content_code(out, pipe = "|>"))
}

# the report's items as a comparison key, the table_plan first as read
t689_items <- function(d, id = "T-1") tsm_key(suppressMessages(tfl_read_outputs(d))[[id]]$content)

t689_head <- list(tfl_item("table_plan", list(cols = "TRT01A", rows = list(group = "variable"))))

test_that("plan_columns / plan_col_header with an argument no sheet says: the rest in content_more", {
  # (the arguments the columns say first, as the item is read back)
  content <- c(t689_head, list(
    tfl_item("plan_columns", list(widths = list(row_label = 3, .values = 2),
                                  column_widths_twips = list(1000L, 2000L, 2000L),
                                  cell_format = "right_align")),
    tfl_item("plan_col_header", list(col_header_align = "center", span = "TRT01A",
                                     header_sep = " / ")),
    tfl_item("plan_paginate_rows", list(max_rows = 30L))))
  d <- t689_study(content)
  r <- t689_trip(d)
  tb <- r$sh$table$tables
  cm <- tb$content_more[tb$output_id %in% "T-1"]
  expect_match(cm, "- plan_columns:", fixed = TRUE)
  expect_match(cm, "column_widths_twips", fixed = TRUE)
  expect_match(cm, "- plan_col_header:", fixed = TRUE)
  expect_match(cm, "header_sep", fixed = TRUE)
  # what the columns say is on the sheets, and only the rest in content_more
  cl <- r$sh$table$columns
  expect_identical(cl$rel_width[cl$output_id %in% "T-1"], c("3", "2"))
  st <- r$sh$table$style
  expect_identical(st$col_header_align[st$output_id %in% "T-1"], "center")
  expect_false(grepl("widths:|col_header_align", cm))
  # what the sheets say whole stays on them
  expect_identical(r$sh$table$layout$pages_max_rows[r$sh$table$layout$output_id %in% "T-1"], "30")
  expect_false(any(r$sh$not_converted$output %in% "T-1"))
  expect_setequal(t689_items(r$to), t689_items(d))
  expect_identical(t689_code(r$to), t689_code(d))
})

test_that("the bridge's guard: an argument no branch reads is named, never dropped", {
  un <- tflspec:::.bridge_unread
  expect_identical(un("plan_columns", c("widths", "cell_format", "column_widths_twips"), 0L),
                   c("cell_format", "column_widths_twips"))
  expect_identical(un("plan_col_header", c("lines", "span", "header_sep"), 0L),
                   c("span", "header_sep"))
  expect_identical(un("plan_paginate_cols", c("at", "col_header"), 0L), "col_header")
  expect_identical(un("plan_paginate_rows", c("max_rows", "page_by"), 0L), character())
  expect_identical(un("plan_labels", "AGE", 1L), "an unnamed argument")
  expect_identical(un("plan_hide", character(), 2L), character())
  # every argument the schema lists for these verbs is read or named
  for (fn in c("plan_columns", "plan_col_header", "plan_stub", "plan_blanks", "plan_total",
               "plan_row_group", "plan_paginate_group", "plan_style")) {
    args <- names(tfl_schema(fn)$args)
    expect_type(un(fn, args, 0L), "character")
  }
  # the bridge itself: the item's other rows are not written
  # an item with an argument no sheet says and one a sheet does: the sheet's is
  # written, the other is the item's rest (test-table-sheet-rest.R); with
  # nothing a sheet says, the item goes whole
  items <- list(tfl_item("plan_columns", list(widths = list(row_label = 3), cell_format = "x")))
  sp <- tflspec:::.plan_sheets_from_content(items, "T-1")
  expect_identical(attr(sp, "not_converted_at"), integer())
  expect_identical(sp$columns$rel_width, "3")
  expect_identical(attr(sp, "not_converted_rest")[[1L]]$args, list(cell_format = "x"))
  expect_length(attr(sp, "not_converted"), 0L)
  items <- list(tfl_item("plan_columns", list(cell_format = "x")))
  sp <- tflspec:::.plan_sheets_from_content(items, "T-1")
  expect_identical(attr(sp, "not_converted_at"), 1L)
  expect_match(attr(sp, "not_converted"), "cell_format", fixed = TRUE)
  expect_true(is.null(sp$columns) || !nrow(sp$columns))
})

test_that("normalize_ard(): keys, hierarchy, drop_contexts, drop_key_variables go whole", {
  content <- list(
    tfl_item("normalize_ard", list(keys = list("AGE", "SEX"), hierarchy = list("AEBODSYS", "AEDECOD"),
                                   drop_contexts = list("alpha", "beta"), drop_key_variables = TRUE)),
    tfl_item("table_plan", list(cols = "TRT01A", rows = list(group = "variable"))))
  d <- t689_study(content)
  r <- t689_trip(d)
  tb <- r$sh$table$tables
  expect_match(tb$content_more[tb$output_id %in% "T-1"], "- normalize_ard:", fixed = TRUE)
  expect_match(tb$content_more[tb$output_id %in% "T-1"], "drop_key_variables", fixed = TRUE)
  back <- tfl_read_outputs(r$to)[["T-1"]]
  # one normalize_ard(), not the defaults' as well
  expect_identical(sum(vapply(back$content, function(it) identical(it$fn, "normalize_ard"), NA)), 1L)
  expect_setequal(t689_items(r$to), t689_items(d))
  expect_identical(t689_code(r$to), t689_code(d))
  expect_match(paste(t689_code(r$to), collapse = "\n"), "drop_key_variables = TRUE", fixed = TRUE)
})

test_that("normalize_ard(): a hierarchy the report's first hierarchical analysis does not give stays whole", {
  # the ae_max_grade shape: three hierarchical analyses, the first one
  # with two levels; the table's hierarchy has three
  an <- function(id, vars) list(id = id, method = "hierarchical", dataset = "ADAE", population = "SAF",
                                by = "TRT01A", variables = as.list(vars))
  ard <- list(analyses = list(an("A1", c("AEBODSYS", "AESEV")), an("A2", c("AEBODSYS", "AESEV")),
                              an("A3", c("AEBODSYS", "AEDECOD", "AESEV"))))
  content <- list(
    tfl_item("normalize_ard", list(hierarchy = list("AEBODSYS", "AEDECOD", "AESEV"))),
    tfl_item("table_plan", list(cols = "TRT01A", rows = list(group1 = "AEBODSYS", group2 = "AEDECOD"),
                                label = list(label = "AESEV"))))
  d <- t689_study(content, ard = ard)
  r <- t689_trip(d)
  tb <- r$sh$table$tables
  expect_match(tb$content_more[tb$output_id %in% "T-1"], "AESEV", fixed = TRUE)
  back <- tfl_read_outputs(r$to)[["T-1"]]
  nz <- Filter(function(it) identical(it$fn, "normalize_ard"), back$content)
  expect_length(nz, 1L)
  expect_identical(as.character(unlist(nz[[1L]]$args$hierarchy)), c("AEBODSYS", "AEDECOD", "AESEV"))
  expect_setequal(t689_items(r$to), t689_items(d))
  expect_identical(t689_code(r$to), t689_code(d))
  # the same hierarchy as the first analysis has: on the sheets, as before
  ard2 <- list(analyses = list(an("A1", c("AEBODSYS", "AEDECOD", "AESEV"))))
  d2 <- t689_study(content, ard = ard2)
  r2 <- t689_trip(d2)
  tb2 <- r2$sh$table$tables
  expect_true(is.null(tb2$content_more) || all(is.na(tb2$content_more)))
  expect_setequal(t689_items(r2$to), t689_items(d2))
  expect_identical(t689_code(r2$to), t689_code(d2))
})

test_that("normalize_ard(): the defaults' arguments go whole too", {
  d <- t689_study(t689_head, defaults = list(
    tfl_item("normalize_ard", list(drop_contexts = list("alpha"))),
    tfl_item("plan_paginate_rows", list(max_rows = 25L))))
  r <- t689_trip(d)
  tb <- r$sh$table$tables
  expect_match(tb$content_more[is.na(tb$output_id)], "drop_contexts", fixed = TRUE)
  back <- tfl_read_study_spec(r$to)$defaults$content
  nz <- Filter(function(it) identical(it$fn, "normalize_ard"), back)
  expect_length(nz, 1L)
  expect_identical(as.character(unlist(nz[[1L]]$args$drop_contexts)), "alpha")
})

test_that("a template's hierarchy (ae_max_grade) comes back whole", {
  skip_if_not_installed("rtfreporter")
  d <- t689_study(t689_head)
  s <- tfl_read_study_spec(d)
  o <- tfl_output("T-mg", "table", population = "SAF", extends = "defaults",
                  template = list(name = "ae_max_grade",
                                  args = list(population = "SAF", hierarchy = c("AEBODSYS", "AEDECOD"))))
  b <- suppressMessages(tfl_template_to_spec(o, s))
  hier <- function(x) {
    nz <- Filter(function(it) identical(it$fn, "normalize_ard"), x$content)
    as.character(unlist(nz[[1L]]$args$hierarchy))
  }
  expect_identical(hier(b), c("AEBODSYS", "AEDECOD", "AESEV"))
  suppressMessages(tfl_write_output(b, file.path(d, "spec", "outputs", "T-mg.yml")))
  r <- t689_trip(d)
  expect_identical(hier(tfl_read_outputs(r$to)[["T-mg"]]), c("AEBODSYS", "AEDECOD", "AESEV"))
})

test_that("plan_paginate_cols(col_header = ): that item goes whole, the rest of the report stays", {
  content <- c(t689_head, list(tfl_item("plan_paginate_cols", list(at = 2L, col_header = "Continued"))))
  d <- t689_study(content)
  r <- t689_trip(d)
  tb <- r$sh$table$tables
  i <- tb$output_id %in% "T-1"
  expect_match(tb$content_more[i], "- plan_paginate_cols:", fixed = TRUE)
  expect_identical(tb$cols[i], "TRT01A")
  expect_identical(tb$rows[i], "group = variable")
  expect_false(any(r$sh$not_converted$output %in% "T-1"))
  lay <- r$sh$table$layout
  expect_false(any(!is.na(lay$colpages_at[lay$output_id %in% "T-1"])))
  expect_setequal(t689_items(r$to), t689_items(d))
  expect_identical(t689_code(r$to), t689_code(d))
})

test_that("an item the sheets refuse goes whole, the report's others stay", {
  # a cell style that styles nothing has no row the sheets accept
  content <- c(t689_head, list(tfl_item("plan_cell_style", list(cols = list("AGE")))))
  d <- t689_study(content)
  r <- t689_trip(d)
  tb <- r$sh$table$tables
  i <- tb$output_id %in% "T-1"
  expect_match(tb$content_more[i], "- plan_cell_style:", fixed = TRUE)
  expect_identical(tb$cols[i], "TRT01A")
  expect_setequal(t689_items(r$to), t689_items(d))
})

test_that("two plan_labels items naming a heading twice: the second stays whole", {
  content <- c(t689_head, list(
    tfl_item("plan_labels", list(variable = list(AVALC = "Best response", RESP = "Response rate"))),
    tfl_item("plan_labels", list(RESP = ""))))
  d <- t689_study(content)
  r <- t689_trip(d)
  v <- r$sh$table$variables
  # (the reader puts the two calls together: one item, RESP named twice with
  # two texts; no cell holds both, so the first text is on the variables sheet
  # and the second is the item's rest in content_more -- not the second's text
  # over the first's)
  expect_identical(v$label[v$output_id %in% "T-1" & v$variable %in% "RESP"], "Response rate")
  tb <- r$sh$table$tables
  expect_match(tb$content_more[tb$output_id %in% "T-1"], "RESP: ''", fixed = TRUE)
  back <- suppressMessages(tfl_read_outputs(r$to))[["T-1"]]
  labs <- Filter(function(it) identical(it$fn, "plan_labels"), back$content)
  expect_length(labs, 1L)
  expect_identical(labs[[1L]]$args$RESP, "")
  expect_identical(labs[[1L]]$args$variable$RESP, "Response rate")
  expect_setequal(t689_items(r$to), t689_items(d))
  expect_identical(t689_code(r$to), t689_code(d))
  # two items naming different variables are still put on the sheets together
  content2 <- c(t689_head, list(tfl_item("plan_labels", list(AGE = "Age (y)")),
                                tfl_item("plan_labels", list(SEX = "Sex"))))
  r2 <- t689_trip(t689_study(content2))
  tb2 <- r2$sh$table$tables
  expect_true(is.null(tb2$content_more) || all(is.na(tb2$content_more)))
})

# ---- normalize_ard(): what the columns can say is on the tables row, always ----
# The tables row says overall_label, stack, stack_sep and stack_nest of a
# table's normalize_ard(); the rest (hierarchy, keys, drop_contexts, ...) is an
# item in content_more -- with no arguments when the row says it all and only
# the hierarchy is the report's own.  Read back, the item's arguments stand,
# and the row's cells go on top.

tnz_ard <- function(vars) {
  list(analyses = list(list(id = "A1", method = "hierarchical", dataset = "ADAE", population = "SAF",
                            by = "TRT01A", variables = as.list(vars))))
}
tnz_plan <- tfl_item("table_plan", list(cols = "TRT01A", rows = list(group1 = "AEBODSYS", group2 = "AEDECOD"),
                                        label = list(label = "AESEV")))
.nz_canon_t <- function(x) tflspec:::.nz_canon(x)
tnz_nz <- function(dir, id = "T-1") {
  nz <- Filter(function(it) identical(it$fn, "normalize_ard"), tfl_read_outputs(dir)[[id]]$content)
  if (length(nz)) nz[[1L]]$args
}
tnz_cm <- function(sh, id = "T-1") {
  tb <- sh$table$tables
  v <- tb$content_more[tb$output_id %in% id]
  if (length(v) && !is.na(v)) v else NA_character_
}
# the table sheets through a workbook (tfl_write_table_spec() / tfl_read_table_spec()) and back to YAML
tnz_book <- function(sh, env = parent.frame()) {
  f <- withr::local_tempfile(fileext = ".xlsx", .local_envir = env)
  keep <- intersect(names(sh$table), c("study", names(tflspec:::.ard_spec_schema())))
  tfl_write_table_spec(tfl_table_spec(sh$table[keep]), f)
  wb <- unclass(tfl_read_table_spec(f))
  sh2 <- sh
  sh2$table[intersect(names(wb), keep)] <- wb[intersect(names(wb), keep)]
  to <- withr::local_tempdir(.local_envir = env)
  suppressMessages(tfl_yaml_from_sheets(sh2, to, overwrite = TRUE))
  list(sh = sh2, to = to)
}

tnz_cases <- list(
  # the hierarchy has three levels, the first hierarchical analysis two: the sheets cannot read it back
  hier_overall = list(
    nz = list(hierarchy = list("AEBODSYS", "AEDECOD", "AESEV"), overall = "Any adverse event"),
    ard = tnz_ard(c("AEBODSYS", "AESEV")), more = "- normalize_ard:", row = c(overall_label = "Any adverse event")),
  # (an item with an overall and the analysis's hierarchy: the hierarchy is the item's)
  keys_overall_hier = list(
    nz = list(keys = list("AGE", "SEX"), overall = "Any adverse event"),
    ard = tnz_ard(c("AEBODSYS", "AEDECOD")), more = "hierarchy:", row = c(overall_label = "Any adverse event"),
    back = list(keys = list("AGE", "SEX"), hierarchy = list("AEBODSYS", "AEDECOD"), overall = "Any adverse event")),
  keys_overall = list(
    nz = list(keys = list("AGE", "SEX"), overall = "Any adverse event"),
    ard = NULL, more = "keys:", row = c(overall_label = "Any adverse event"),
    # (the hierarchy the rows and label say: an overall needs one)
    back = list(keys = list("AGE", "SEX"), hierarchy = list("AEBODSYS", "AEDECOD", "AESEV"), overall = "Any adverse event")),
  hier_keys_stack = list(
    nz = list(hierarchy = list("AEBODSYS", "AEDECOD", "AESEV"), keys = list("AGE"),
              stack = list(subgroup = list("ALL", "SEX")), stack_sep = " / "),
    ard = NULL, more = "keys:", row = c(stack = "subgroup = ALL, SEX", stack_sep = "\" / \"")),
  drop_overall = list(
    nz = list(drop_contexts = list("alpha"), overall = "Any adverse event"),
    ard = NULL, more = "drop_contexts:", row = c(overall_label = "Any adverse event"),
    back = list(drop_contexts = list("alpha"), hierarchy = list("AEBODSYS", "AEDECOD", "AESEV"), overall = "Any adverse event")))

test_that("normalize_ard(): the tables row has what its columns say, the item the rest, in memory and through a workbook", {
  skip_if_not_installed("openxlsx")
  for (nm in names(tnz_cases)) {
    cs <- tnz_cases[[nm]]
    d <- t689_study(list(tfl_item("normalize_ard", cs$nz), tnz_plan), ard = cs$ard)
    sh <- tfl_sheets_from_yaml(d)
    tb <- sh$table$tables
    for (col in names(cs$row)) expect_identical(tb[[col]][tb$output_id %in% "T-1"], unname(cs$row[col]), info = paste(nm, col))
    expect_match(tnz_cm(sh), cs$more, fixed = TRUE, info = nm)
    # what the row says is not in the item as well
    expect_false(grepl("overall:|stack:|stack_sep:|stack_nest:", tnz_cm(sh)), info = nm)
    want <- .nz_canon_t(cs$back %||% tnz_nz(d))
    for (route in c("memory", "workbook")) {
      r <- if (route == "memory") t689_trip(d) else tnz_book(sh)
      expect_identical(.nz_canon_t(tnz_nz(r$to)), want, info = paste(nm, route))
      if (is.null(cs$back)) expect_identical(t689_code(r$to), t689_code(d), info = paste(nm, route))
      else expect_match(paste(t689_code(r$to), collapse = "
"), "hierarchy = c(", fixed = TRUE, info = paste(nm, route))
      # one normalize_ard(), not another from the analysis
      back <- tfl_read_outputs(r$to)[["T-1"]]
      expect_identical(sum(vapply(back$content, function(it) identical(it$fn, "normalize_ard"), NA)), 1L, info = paste(nm, route))
    }
  }
})

test_that("normalize_ard(): {overall} with a hierarchical analysis: the analysis's hierarchy, no item", {
  # (an overall runs only with a hierarchy)
  d <- t689_study(list(tfl_item("normalize_ard", list(overall = "Any adverse event")), tnz_plan),
                  ard = tnz_ard(c("AEBODSYS", "AEDECOD", "AESEV")))
  sh <- tfl_sheets_from_yaml(d)
  expect_true(is.na(tnz_cm(sh)))
  tb <- sh$table$tables
  expect_identical(tb$overall_label[tb$output_id %in% "T-1"], "Any adverse event")
  r <- t689_trip(d)
  expect_identical(.nz_canon_t(tnz_nz(r$to)),
                   .nz_canon_t(list(hierarchy = list("AEBODSYS", "AEDECOD", "AESEV"), overall = "Any adverse event")))
})

test_that("normalize_ard(): {stack} with a hierarchical analysis keeps its empty item, no hierarchy back", {
  d <- t689_study(list(tfl_item("normalize_ard", list(stack = list(subgroup = list("ALL", "SEX")))), tnz_plan),
                  ard = tnz_ard(c("AEBODSYS", "AEDECOD")))
  sh <- tfl_sheets_from_yaml(d)
  expect_identical(tnz_cm(sh), "- normalize_ard: {}")
  tb <- sh$table$tables
  expect_identical(tb$stack[tb$output_id %in% "T-1"], "subgroup = ALL, SEX")
  r <- t689_trip(d)
  expect_named(tnz_nz(r$to), "stack")
  expect_false(grepl("hierarchy", paste(t689_code(r$to), collapse = "\n"), fixed = TRUE))
})

test_that("normalize_ard(): the program written for {overall} and {keys, overall} runs on a hierarchical ARD", {
  skip_if_not_installed("cards")
  skip_if_not_installed("rtfreporter")
  set.seed(1)
  dm <- data.frame(USUBJID = sprintf("S%02d", 1:30), TRT01A = rep(c("A", "B"), 15), stringsAsFactors = FALSE)
  ae <- data.frame(USUBJID = sample(dm$USUBJID, 40, TRUE), AEBODSYS = sample(c("GI", "Skin"), 40, TRUE))
  ae$AEDECOD <- ifelse(ae$AEBODSYS == "GI", sample(c("Nausea", "Vomit"), 40, TRUE), sample(c("Rash", "Itch"), 40, TRUE))
  ae <- merge(ae, dm)
  ard <- cards::ard_stack_hierarchical(ae, variables = c(AEBODSYS, AEDECOD), by = TRT01A, denominator = dm,
                                       id = USUBJID, over_variables = TRUE, statistic = ~ c("n", "p"))
  plan <- tfl_item("table_plan", list(cols = "TRT01A", rows = list(group1 = "AEBODSYS"), label = list(label = "AEDECOD")))
  for (nz in list(list(overall = "Any adverse event"), list(keys = list("AEBODSYS"), overall = "Any adverse event"))) {
    d <- t689_study(list(tfl_item("normalize_ard", nz), plan), ard = tnz_ard(c("AEBODSYS", "AEDECOD")))
    sh <- tfl_sheets_from_yaml(d)
    code <- tfl_table_code(sh$table, "T-1", ard_spec = tfl_ard_spec(sh$ard))
    i <- grep("normalize_ard", code)[1L]
    j <- grep("table_plan", code)[1L]
    txt <- sub("\\s*\\|>\\s*$", "", paste(code[i:(j - 1L)], collapse = "\n"))
    e <- list2env(list(ard = ard, normalize_ard = rtfreporter::normalize_ard))
    expect_no_error(eval(parse(text = paste("ard |>", txt)), e))
  }
})

test_that("normalize_ard(): the overall_label cell edited after writing wins on reading", {
  skip_if_not_installed("openxlsx")
  for (nm in c("hier_overall", "keys_overall", "keys_overall_hier")) {
    cs <- tnz_cases[[nm]]
    d <- t689_study(list(tfl_item("normalize_ard", cs$nz), tnz_plan), ard = cs$ard)
    sh <- tfl_sheets_from_yaml(d)
    sh$table$tables$overall_label[sh$table$tables$output_id %in% "T-1"] <- "Any event, edited"
    for (route in c("memory", "workbook")) {
      if (route == "memory") {
        to <- withr::local_tempdir()
        suppressMessages(tfl_yaml_from_sheets(sh, to, overwrite = TRUE))
        r <- list(to = to)
      } else r <- tnz_book(sh)
      a <- tnz_nz(r$to)
      expect_identical(a$overall, "Any event, edited", info = paste(nm, route))
      # the rest of the item as written
      other <- setdiff(names(cs$nz), "overall")
      expect_identical(.nz_canon_t(a[other]), .nz_canon_t(tnz_nz(d)[other]), info = paste(nm, route))
      expect_match(paste(t689_code(r$to), collapse = "\n"), "Any event, edited", fixed = TRUE)
    }
  }
})

test_that("normalize_ard(): a table the row can fully say has no content_more item", {
  nz <- list(hierarchy = list("AEBODSYS", "AEDECOD"), overall = "Any adverse event")
  plan <- tfl_item("table_plan", list(cols = "TRT01A", rows = list(group1 = "AEBODSYS"), label = list(label = "AEDECOD")))
  for (ard in list(NULL, tnz_ard(c("AEBODSYS", "AEDECOD")))) {
    d <- t689_study(list(tfl_item("normalize_ard", nz), plan), ard = ard)
    sh <- tfl_sheets_from_yaml(d)
    expect_true(is.na(tnz_cm(sh)))
    expect_identical(sh$table$tables$overall_label[sh$table$tables$output_id %in% "T-1"], "Any adverse event")
    r <- t689_trip(d)
    expect_identical(.nz_canon_t(tnz_nz(r$to)), .nz_canon_t(tnz_nz(d)))
  }
})

test_that("normalize_ard(): a card's {overall} bridged without the report's ARD keeps the overall_label cell", {
  # (tflplanner's content-only save: the report's ARD is left out, so the
  # hierarchy the rows and label say is guessed, and the item has none)
  plan <- tfl_item("table_plan", list(cols = "TRT01A", rows = list(group1 = "AEBODSYS"), label = list(label = "AEDECOD")))
  d <- t689_study(list(tfl_item("normalize_ard", list(overall = "Any adverse event")), plan))
  s <- tfl_read_study_spec(d)
  o <- tfl_read_outputs(d)[["T-1"]]
  sh <- tfl_sheets_from_spec(s, list("T-1" = o))
  tb <- sh$table$tables
  expect_identical(tb$overall_label[tb$output_id %in% "T-1"], "Any adverse event")
  # (the guessed hierarchy is the one an overall needs: no item, as before #692)
  expect_true(is.na(tnz_cm(sh)))
})
