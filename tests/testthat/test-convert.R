# The conversion of a study's sheets to the YAML spec

# a small study folder as tflplanner writes one: a table and a report
# workbook (with the _tflplanner sheet), the ARD as JSON, a listing
# workbook, a figure design
conv_study <- function(dir) {
  spec <- file.path(dir, "spec")
  dir.create(file.path(spec, "figures"), recursive = TRUE)
  writeLines(c("study_id: CV-1", "title: A study", "phase: '2'", "created: '2026-10-14'",
               "tflplanner: 0.0.2.9222"), file.path(dir, "study.yml"))
  tb <- list(
    tables = data.frame(output_id = c(NA, "T-1"), cols = c(NA, "ARM"), rows = c(NA, "group = variable"),
                        stringsAsFactors = FALSE),
    cells = data.frame(output_id = c(NA, "T-1"), variable = c(NA, "continuous"), row = c(NA, "n"),
                       template = c("{n:.0f} ({p:.1f%})", "{N}"), stringsAsFactors = FALSE),
    codelists = data.frame(output_id = c("T-1", "T-1", "T-1"), variable = c("SEX", "ARM", "ARM"),
                           value = c("F", "A", "B"), label = c("Female", "Arm A", "Arm B"),
                           order = c("1", "1", "2"), stringsAsFactors = FALSE))
  tfl_write_table_spec(tfl_table_spec(tb), file.path(spec, "table_spec.xlsx"))
  rp <- list(
    report = data.frame(output_id = c(NA, "T-1", "L-1"), type = c("table", "table", "listing"),
                        file = c("{output_id}.rtf", NA, NA), pattern = c(NA, "@Compact", NA),
                        stringsAsFactors = FALSE),
    page = data.frame(output_id = "@Compact", margin_top_in = "0.5", stringsAsFactors = FALSE),
    header = data.frame(output_id = c(NA, NA, "T-1"), line = c("1", "2", "2"),
                        left = c("Acme", "Protocol: {STUDY_ID}", "(none)"), right = c("DRAFT", NA, NA),
                        stringsAsFactors = FALSE),
    footer = data.frame(output_id = c(NA, "T-1"), line = c("99", "1"),
                        left = c("{PROGRAM}", "SD = Standard Deviation."), stringsAsFactors = FALSE),
    tokens = data.frame(output_id = c(NA, "T-1"), name = c("COMPANY", "OUTPUT_TITLE"),
                        value = c("Acme", "Demographics"), stringsAsFactors = FALSE))
  wb <- openxlsx::createWorkbook()
  for (s in names(rp)) { openxlsx::addWorksheet(wb, s); openxlsx::writeData(wb, s, rp[[s]], keepNA = FALSE) }
  openxlsx::addWorksheet(wb, "_tflplanner")
  openxlsx::writeData(wb, "_tflplanner", data.frame(
    output_id = c(NA, "T-1", "L-1"), description = c("(every report)", "Demographics", "AEs"),
    setup_code = c("library(cards)\noptions(warn = 1)", NA, NA),
    process_code = c(NA, "tbl |> dplyr::filter(!is.na(level))", NA),
    section = c(NA, "14.1", "16.2"), population = c(NA, "SAF", NA),
    datasets = c(NA, NA, NA), batches = c(NA, "Topline | Final", NA), stringsAsFactors = FALSE), keepNA = FALSE)
  openxlsx::saveWorkbook(wb, file.path(spec, "report_spec.xlsx"))
  writeLines(c(
    '{"study": {"key": ["id", "output"], "value": ["USUBJID", "output/ard/ard.rds"]},',
    ' "datasets": {"dataset": ["ADSL"], "level": ["ADaM"], "path": ["data/adam/adsl.rds"], "derive": [null]},',
    ' "populations": {"population_id": ["SAF"], "dataset": ["ADSL"], "where": ["SAFFL == \\"Y\\""], "derive": [null], "label": ["Safety Analysis Set"]},',
    ' "analysis_data": {"output_id": ["T-1"], "data_id": ["adsl_saf"], "label": [null], "from": ["ADSL"], "population_id": ["SAF"], "subjects": [null], "where": [null], "add": [null], "derive": [null], "keep": [null], "distinct": [null], "code": [null]},',
    ' "analyses": {"output_id": ["T-1", "T-1"], "analysis_id": ["GROUPN", "AGE"], "parent": [null, null], "label": [null, null],',
    '   "method": ["categorical", "continuous"], "data": ["adsl_saf", "adsl_saf"], "dataset": [null, null], "population_id": [null, null],',
    '   "where": [null, null], "by": [null, "ARM"], "strata": [null, null], "variables": ["ARM", "AGE"], "statistics": [null, "N | mean | sd"],',
    '   "denominator": [null, null], "formats": [null, null], "args": [null, "conf.level = 0.9"], "post": [null, null], "code": [null, null],',
    '   "purpose": [null, null], "reason": [null, null]}}'), file.path(spec, "ard_definition.json"))
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "listings")
  openxlsx::writeData(wb, "listings", data.frame(output_id = "L-1", type = "multiline", dataset = "ADAE",
                                                 where = "TRTEMFL == \"Y\"", sort = "USUBJID | ASTDT", max_rows = "22",
                                                 stringsAsFactors = FALSE))
  openxlsx::addWorksheet(wb, "listing_cols")
  openxlsx::writeData(wb, "listing_cols", data.frame(output_id = c("L-1", "L-1"), vars = c("USUBJID", "AEBODSYS | AEDECOD"),
                                                     label = c("Subject", "SOC/PT"), width = c("14", "40"),
                                                     collapse_repeats = c("TRUE", NA), stringsAsFactors = FALSE), keepNA = FALSE)
  openxlsx::addWorksheet(wb, "figures")
  openxlsx::saveWorkbook(wb, file.path(spec, "listing_figure_spec.xlsx"))
  writeLines(c("template: km_simple", "data:", "- step: read", "  dataset: ADTTE", "plot:", "  x_label: Time", "  x_log: yes",
               "layers:", "- layer: km_curve", "  linewidth: 0.3", "- layer: hline", "  yintercept: 0.5"),
             file.path(spec, "figures", "F-1.yml"))
  dir
}

test_that("a study's sheets convert to the YAML files, the rest is listed", {
  skip_if_not_installed("openxlsx")
  from <- conv_study(withr::local_tempdir())
  to <- withr::local_tempdir()
  r <- tfl_convert_sheets_to_yaml(from, to, overwrite = TRUE)
  rel <- sub(paste0("^", normalizePath(to, "/"), "/"), "", normalizePath(r$files, "/"))
  expect_setequal(rel, c("study.yml", "spec/data.yml", "spec/defaults.yml",
                         "spec/patterns/Compact.yml", "spec/outputs/T-1.yml", "spec/outputs/L-1.yml",
                         "spec/outputs/F-1.yml", "spec/toc.yml", "spec/_not_converted.yml"))
  # study.yml: the meta without the stamps, the keys, the setup as !r
  st <- yaml::read_yaml(file.path(to, "study.yml"), handlers = tflspec:::.yaml_handlers())
  expect_identical(st$study_id, "CV-1")
  expect_null(st$tflplanner)
  expect_identical(st$keys$output, "output/ard/ard.rds")
  expect_identical(st$spec_version, 1L)
  # every report's file: the report sheet's study row
  expect_identical(st$settings$file, "{output_id}.rtf")
  expect_true("tflspec" %in% names(st$packages))
  expect_s3_class(st$setup, "tfl_r")
  # data.yml: the catalog, where as !r, label kept
  dt <- yaml::read_yaml(file.path(to, "spec", "data.yml"), handlers = tflspec:::.yaml_handlers())
  expect_identical(dt$datasets[[1L]]$dataset, "ADSL")
  expect_identical(dt$populations[[1L]]$id, "SAF")
  expect_s3_class(dt$populations[[1L]]$where, "tfl_r")
  expect_identical(dt$populations[[1L]]$label, "Safety Analysis Set")
  # the study spec reads what was written
  s <- tfl_read_study_spec(to)
  expect_identical(s$defaults$page$header[["1"]]$left, "Acme")
  expect_identical(s$defaults$page$footer[["99"]]$left, "{PROGRAM}")
  expect_identical(s$defaults$page$tokens$COMPANY, "Acme")
  expect_identical(s$codelists, list())        # a code list is a report's (no templates here)
  expect_identical(names(s$patterns), "Compact")
  expect_identical(s$patterns$Compact$page$rtf_page$margin_top_in, 0.5)
  expect_identical(vapply(s$toc$outputs, `[[`, "", "id"), c("T-1", "L-1", "F-1"))
  expect_identical(s$toc$outputs[[1L]]$sets, c("Topline", "Final"))
  expect_identical(s$toc$outputs[[1L]]$section, "14.1")
  # the table: extends its pattern, its ARD analyses as maps, lists split, R as !r
  outs <- tfl_read_outputs(to)
  t1 <- outs[["T-1"]]
  expect_identical(t1$kind, "table")
  expect_identical(t1$extends, c("patterns/Compact", "defaults"))
  expect_identical(t1$population, "SAF")
  expect_identical(t1$data$source, "ard")
  expect_identical(t1$ard$analysis_data[[1L]]$id, "adsl_saf")
  expect_identical(t1$ard$analyses[[2L]]$id, "AGE")
  expect_identical(t1$ard$analyses[[2L]]$statistics, c("N", "mean", "sd"))
  expect_s3_class(t1$ard$analyses[[2L]]$args, "tfl_r")
  expect_identical(t1$data$object, "ard")
  expect_s3_class(t1$data$steps[[1L]], "tfl_r")
  expect_identical(t1$codelists$ARM[[2L]]$label, "Arm B")
  expect_identical(t1$codelists$SEX[[1L]]$label, "Female")
  # the content through the package's bridge from the sheets
  expect_true(length(t1$content) > 0L)
  expect_identical(t1$content[[1L]]$fn, "table_plan")
  # the report's header line 2 is (none): an unset, resolved to nothing
  expect_s3_class(t1$page$header[["2"]], "tfl_unset")
  r1 <- tfl_resolve_output(t1, s)
  expect_identical(names(r1$page$header), "1")
  expect_identical(r1$page$footer[["1"]]$left, "SD = Standard Deviation.")
  expect_identical(r1$page$tokens$OUTPUT_TITLE, "Demographics")
  # the listing: its data, its columns as items, its spec and pages
  l1 <- outs[["L-1"]]
  expect_identical(l1$kind, "listing")
  expect_identical(l1$data$dataset, "ADAE")
  expect_identical(l1$data$sort, c("USUBJID", "ASTDT"))
  fns <- vapply(l1$content, `[[`, "", "fn")
  expect_identical(fns, c("plan_listing", "plan_paginate_rows"))
  cols <- l1$content[[1L]]$args$cols
  expect_length(cols, 2L)
  expect_s3_class(cols[[2L]], "tfl_call")
  expect_identical(cols[[2L]]$fn, "listing_col")
  expect_identical(cols[[2L]]$args$vars, c("AEBODSYS", "AEDECOD"))
  expect_identical(cols[[1L]]$args$collapse_repeats, TRUE)
  expect_identical(l1$content[[1L]]$args$type, "multiline")
  expect_identical(l1$content[[2L]]$args$max_rows, 22L)
  # the figure: its steps under data, its layers as items, plot beside
  f1 <- outs[["F-1"]]
  expect_identical(f1$kind, "figure")
  expect_identical(f1$data$source, "code")
  expect_identical(f1$data$steps[[1L]]$dataset, "ADTTE")
  expect_identical(vapply(f1$content, `[[`, "", "fn"), c("km_curve", "hline"))
  expect_identical(f1$plot$x_label, "Time")
  # an old design is read as the designer read it (YAML 1.1: yes is TRUE)
  expect_identical(f1$plot$x_log, TRUE)
  # what was not converted is listed: the contents without a bridge, the R cells
  nc <- r$not_converted
  expect_true(any(nc$column == "args" & nc$output == "T-1"))
  expect_true(file.exists(file.path(to, "spec", "_not_converted.yml")))
  # a bridge fills the content
  r2 <- tfl_convert_sheets_to_yaml(from, to, overwrite = TRUE,
    content_fn = function(sp, id) list(tfl_item("table_plan", list(cols = "ARM"))))
  expect_identical(tfl_read_outputs(to)[["T-1"]]$content[[1L]]$fn, "table_plan")
  expect_length(tfl_read_outputs(to)[["T-1"]]$content, 1L)
  # not written over by accident
  expect_error(tfl_convert_sheets_to_yaml(from, to), "not empty")
})

test_that("the objects route writes what the files route writes", {
  skip_if_not_installed("openxlsx")
  from <- conv_study(withr::local_tempdir())
  a <- withr::local_tempdir(); b <- withr::local_tempdir()
  tfl_convert_sheets_to_yaml(from, a, overwrite = TRUE)
  books <- file.path(from, "spec", c("table_spec.xlsx", "report_spec.xlsx"))
  sheets <- list(
    table = tfl_read_table_spec(books),
    planner = tflspec:::.conv_read_planner_sheet(books),
    ard = tflspec:::.conv_read_ard(file.path(from, "spec", "ard_definition.json")),
    listing = tflspec:::.conv_read_lf(file.path(from, "spec", "listing_figure_spec.xlsx")),
    figures = list(`F-1` = yaml::read_yaml(file.path(from, "spec", "figures", "F-1.yml"))),
    study = yaml::read_yaml(file.path(from, "study.yml")))
  tfl_yaml_from_sheets(sheets, b, overwrite = TRUE)
  fa <- list.files(a, recursive = TRUE); fb <- list.files(b, recursive = TRUE)
  expect_setequal(fb, fa)
  for (f in setdiff(fa, "spec/_not_converted.yml")) {
    expect_identical(readLines(file.path(b, f)), readLines(file.path(a, f)), info = f)
  }
})

test_that("a hand-written figure's datasets and ARD, and a row's note, go to YAML and back", {
  skip_if_not_installed("openxlsx")
  from <- conv_study(withr::local_tempdir())
  books <- file.path(from, "spec", c("table_spec.xlsx", "report_spec.xlsx"))
  sheets <- list(
    table = tfl_read_table_spec(books),
    planner = tflspec:::.conv_read_planner_sheet(books),
    ard = tflspec:::.conv_read_ard(file.path(from, "spec", "ard_definition.json")),
    listing = tflspec:::.conv_read_lf(file.path(from, "spec", "listing_figure_spec.xlsx")),
    figures = list(`F-1` = yaml::read_yaml(file.path(from, "spec", "figures", "F-1.yml"))),
    study = yaml::read_yaml(file.path(from, "study.yml")))
  sheets$listing$figures <- data.frame(output_id = "F-1", datasets = "ADSL | ADTTE", ard = "TRUE",
                                       stringsAsFactors = FALSE)
  sheets$ard$analyses$note <- c("why this analysis", rep(NA, nrow(sheets$ard$analyses) - 1L))
  to <- withr::local_tempdir()
  tfl_yaml_from_sheets(sheets, to, overwrite = TRUE)
  f1 <- tfl_read_outputs(to)[["F-1"]]
  expect_identical(unlist(f1$data$datasets), c("ADSL", "ADTTE"))
  expect_true(isTRUE(f1$data$ard))
  t1 <- tfl_read_outputs(to)[[sheets$ard$analyses$output_id[1L]]]
  expect_identical(t1$ard$analyses[[1L]]$note, "why this analysis")
  back <- tfl_sheets_from_yaml(to)
  expect_identical(back$listing$figures$datasets, "ADSL | ADTTE")
  expect_identical(back$listing$figures$ard, "TRUE")
  expect_identical(back$ard$analyses$note[1L], "why this analysis")
})

test_that("a figure with no design gets nothing of a table's", {
  skip_if_not_installed("openxlsx")
  from <- conv_study(withr::local_tempdir())
  books <- file.path(from, "spec", c("table_spec.xlsx", "report_spec.xlsx"))
  sheets <- list(
    table = tfl_read_table_spec(books),
    planner = tflspec:::.conv_read_planner_sheet(books),
    ard = tflspec:::.conv_read_ard(file.path(from, "spec", "ard_definition.json")),
    listing = tflspec:::.conv_read_lf(file.path(from, "spec", "listing_figure_spec.xlsx")),
    figures = list(),
    study = yaml::read_yaml(file.path(from, "study.yml")))
  # F-1 without its design file: a figure of no design yet
  sheets$planner[nrow(sheets$planner) + 1L, ] <- NA
  sheets$planner$output_id[nrow(sheets$planner)] <- "F-1"
  r <- sheets$table$report
  r[nrow(r) + 1L, ] <- NA
  r$output_id[nrow(r)] <- "F-1"
  r$type[nrow(r)] <- "figure"
  sheets$table$report <- r
  to <- withr::local_tempdir()
  tfl_yaml_from_sheets(sheets, to, overwrite = TRUE)
  f1 <- tfl_read_outputs(to)[["F-1"]]
  expect_identical(f1$kind, "figure")
  expect_identical(f1$data$source, "code")
  expect_length(f1$content, 0L)
  # and back: no figure design for it
  expect_null(tfl_sheets_from_yaml(to)$figures[["F-1"]]$template)
})

test_that("a table of label NA, rows by name over a default template, row groups: YAML code = the sheets'", {
  # (a study converted from the workbooks: a label NA, rows by name)
  tb <- list(
    tables = data.frame(output_id = "T-9", cols = "TRT01A", rows = "group = variable", label = "NA",
                        sort = "group | .label", stringsAsFactors = FALSE),
    cells = data.frame(output_id = c(NA, "T-9", "T-9"), variable = NA_character_,
                       row = c(NA, "n", "ci"),
                       template = c("{n} ({p:.1f%})", "{n}", "{estimate:.1f} ({conf.low:.1f}, {conf.high:.1f})"),
                       stringsAsFactors = FALSE),
    layout = data.frame(output_id = "T-9", group_col = "variable", group_mode = "row",
                        group_collapse = "TRUE", blank_where = "groups", blank_first = "FALSE",
                        blank_last = "FALSE", pages_max_rows = "30", pages_split = "rows",
                        stringsAsFactors = FALSE),
    columns = data.frame(output_id = "T-9", column = c("row_label", ".values"), rel_width = c("30", "10"),
                         row_title = c("TRUE", NA), stringsAsFactors = FALSE),
    style = data.frame(output_id = "T-9", align_count_pct = "TRUE", stringsAsFactors = FALSE))
  sp <- tfl_table_spec(tb)
  sheets <- list(table = unclass(sp),
                 planner = data.frame(output_id = c(NA, "T-9"), description = c("(every report)", "t"),
                                      setup_code = NA_character_, process_code = NA_character_,
                                      section = NA_character_, population = NA_character_,
                                      datasets = NA_character_, batches = NA_character_,
                                      stringsAsFactors = FALSE),
                 study = list(study_id = "U-1"))
  to <- withr::local_tempdir()
  tfl_yaml_from_sheets(sheets, to, overwrite = TRUE)
  o <- tfl_resolve_output(tfl_read_outputs(to)[["T-9"]], tfl_read_study_spec(to))
  expect_identical(o$content[[1L]]$args$label, NA)
  y <- tfl_content_code(o)
  # the same plan as the sheets' (its data normalized first, a step of its own)
  s0 <- trimws(sub("^plan <- table_plan[(]data, ", "table_plan(", tfl_table_code(sp, "T-9", data = "data", normalize = FALSE)))
  s1 <- trimws(y[-(1:2)])
  expect_identical(s1, s0)
})

test_that("an AE table with no ARD definition: its hierarchy from its rows and label, and it runs", {
  skip_if_not_installed("cards")
  # (an AE table whose ARD is made by its own program)
  tb <- list(
    tables = data.frame(output_id = "AE-1", cols = "TRTA", rows = "group1 = AEBODSYS",
                        label = "label = AEDECOD", overall_label = "Any TEAE",
                        sort = ".overall | group1 | .depth | -n | label", header_n = "table",
                        stringsAsFactors = FALSE),
    cells = data.frame(output_id = "AE-1", variable = NA, row = NA, template = "{n:d} ({p:.1f%})",
                       stringsAsFactors = FALSE),
    layout = data.frame(output_id = "AE-1", stub_name = "row_label", stub_before = "TRUE",
                        blank_where = "between_groups", blank_first = "TRUE", blank_last = "TRUE",
                        blank_counted = "TRUE", pages_max_rows = "26", pages_split = "group_force",
                        stringsAsFactors = FALSE),
    style = data.frame(output_id = "AE-1", align_count_pct = "TRUE", row_height_twips = "210",
                       stringsAsFactors = FALSE),
    col_header = data.frame(output_id = "AE-1", line = c("1", "1", "2", "2"),
                            cols = c("row_label", ".values", "row_label", ".values"),
                            text = c("", "{col}\r\n(N={n:sum})\r\n n (%)", "System Organ Class", "(N={n})"),
                            border_top = c(NA, "single", NA, NA), stringsAsFactors = FALSE))
  sp <- tfl_table_spec(tb)
  sheets <- list(table = unclass(sp),
                 planner = data.frame(output_id = c(NA, "AE-1"), description = c("(every report)", "ae"),
                                      setup_code = NA_character_, process_code = NA_character_,
                                      section = NA_character_, population = NA_character_,
                                      datasets = NA_character_, batches = NA_character_,
                                      stringsAsFactors = FALSE),
                 study = list(study_id = "U-1"))
  to <- withr::local_tempdir()
  r <- tfl_yaml_from_sheets(sheets, to, overwrite = TRUE)
  expect_true(any(grepl("hierarchy taken from the table's rows", r$not_converted$reason)))
  o <- tfl_resolve_output(tfl_read_outputs(to)[["AE-1"]], tfl_read_study_spec(to))
  code <- tfl_content_code(o)
  expect_true(any(grepl('normalize_ard(hierarchy = c("AEBODSYS", "AEDECOD"), overall = "Any TEAE")',
                        code, fixed = TRUE)))
  # (a cell's Excel line breaks, CRLF, as the YAML's)
  expect_false(any(grepl("\r", code, fixed = TRUE)))
  expect_true(any(grepl("(N={n:sum})", code, fixed = TRUE)))
  # the plan's verbs as the sheets write them
  # (the sheets' code keeps the cells' CRLF; the YAML's is LF)
  s0 <- gsub("\\r", "", trimws(tfl_table_code(sp, "AE-1")), fixed = TRUE)
  s1 <- trimws(code)
  expect_identical(s1[grep("^plan_sort", s1):length(s1)], s0[grep("^plan_sort", s0):length(s0)])
  # and it runs on an ARD of that shape
  ard <- cards::ard_stack_hierarchical(cards::ADAE, variables = c(AEBODSYS, AEDECOD), by = TRTA,
                                       denominator = cards::ADSL, id = USUBJID, over_variables = TRUE)
  expect_s3_class(tfl_content_run(o, ard), "table_plan")
})

test_that("a table's font size (the report sheet's) is its plan's plan_style(), not rtf_tables()'s", {
  # (rtf_tables(font_size_half_points = ) is deprecated)
  sp <- tfl_table_spec(list(
    tables = data.frame(output_id = "T-8", cols = "TRT", rows = "group = variable",
                        stringsAsFactors = FALSE),
    report = data.frame(output_id = "T-8", type = "table", table_font_size_half_points = "16",
                        stringsAsFactors = FALSE)))
  # the sheets: the plan says it, the document does not
  expect_true(any(grepl("plan_style(font_size_half_points = 16", tfl_table_code(sp, "T-8"), fixed = TRUE)))
  rc <- tfl_report_code(sp, "T-8", content = "plan")
  expect_false(any(grepl("font_size_half_points", rc, fixed = TRUE)))
  # the YAML: the same, in the report's content
  sheets <- list(table = unclass(sp),
                 planner = data.frame(output_id = c(NA, "T-8"), description = c("(every report)", "t"),
                                      setup_code = NA_character_, process_code = NA_character_,
                                      section = NA_character_, population = NA_character_,
                                      datasets = NA_character_, batches = NA_character_,
                                      stringsAsFactors = FALSE),
                 study = list(study_id = "U-1"))
  to <- withr::local_tempdir()
  tfl_yaml_from_sheets(sheets, to, overwrite = TRUE)
  st <- tfl_read_study_spec(to)
  o <- tfl_resolve_output(tfl_read_outputs(to)[["T-8"]], st)
  expect_identical(Filter(function(it) identical(it$fn, "plan_style"), o$content)[[1L]]$args$font_size_half_points, 16L)
  expect_null(o$page$rtf_tables$font_size_half_points)
  expect_false(any(grepl("font_size_half_points", tfl_report_code(o, content = "plan", study = st), fixed = TRUE)))
  # and back: the style sheet's
  st2 <- tfl_sheets_from_yaml(to)$table$style
  expect_identical(st2$font_size_half_points[st2$output_id %in% "T-8"], "16")
})
