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
  writeLines(c("template: km_simple", "data:", "- step: read", "  dataset: ADTTE", "plot:", "  x_label: Time",
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
  expect_identical(s$patterns$Compact$page$page$margin_top_in, 0.5)
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
  expect_identical(t1$data$analysis_data[[1L]]$id, "adsl_saf")
  expect_identical(t1$data$analyses[[2L]]$id, "AGE")
  expect_identical(t1$data$analyses[[2L]]$statistics, c("N", "mean", "sd"))
  expect_s3_class(t1$data$analyses[[2L]]$args, "tfl_r")
  expect_s3_class(t1$data$process, "tfl_r")
  expect_identical(t1$codelists$ARM[[2L]]$label, "Arm B")
  expect_identical(t1$codelists$SEX[[1L]]$label, "Female")
  expect_length(t1$content, 0L)            # no bridge given
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
  expect_identical(fns, c("listing_col", "listing_col", "listing_spec", "paginate"))
  expect_identical(l1$content[[2L]]$args$vars, c("AEBODSYS", "AEDECOD"))
  expect_identical(l1$content[[1L]]$args$collapse_repeats, TRUE)
  expect_identical(l1$content[[4L]]$args$max_rows, 22L)
  # the figure: its steps under data, its layers as items, plot beside
  f1 <- outs[["F-1"]]
  expect_identical(f1$kind, "figure")
  expect_identical(f1$data$source, "code")
  expect_identical(f1$data$steps[[1L]]$dataset, "ADTTE")
  expect_identical(vapply(f1$content, `[[`, "", "fn"), c("km_curve", "hline"))
  expect_identical(f1$plot$x_label, "Time")
  # what was not converted is listed: the contents without a bridge, the R cells
  nc <- r$not_converted
  expect_true(any(grepl("bridge", nc$reason)))
  expect_true(any(nc$column == "args" & nc$output == "T-1"))
  expect_true(file.exists(file.path(to, "spec", "_not_converted.yml")))
  # a bridge fills the content
  r2 <- tfl_convert_sheets_to_yaml(from, to, overwrite = TRUE,
    content_fn = function(sp, id) list(tfl_item("table_plan", list(cols = "ARM"))))
  expect_identical(tfl_read_outputs(to)[["T-1"]]$content[[1L]]$fn, "table_plan")
  expect_false(any(grepl("bridge", r2$not_converted$reason)))
  # not written over by accident
  expect_error(tfl_convert_sheets_to_yaml(from, to), "not empty")
})
