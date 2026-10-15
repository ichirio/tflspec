# A report's page: is the document's calls: the code and the document are
# what the report sheets saying the same make.

rp_sheets <- function() {
  tfl_table_spec(
    report = data.frame(output_id = c(NA, "T-1"), type = c(NA, "table"),
                        program = c("{output_id}.R", NA), auto_section = c(NA, "TRUE"),
                        title_font_size_half_points = c(NA, "20")),
    page = data.frame(output_id = NA, orientation = "portrait", margin_top_in = "1",
                      font = "Arial", font_size_half_points = "16"),
    header = data.frame(output_id = NA, line = c("1", "2"),
                        left = c("Sample Pharma", NA), center = c(NA, "{OUTPUT_LABEL}"),
                        right = c("Page {PAGE} of {TOTAL_PAGES}", NA)),
    footer = data.frame(output_id = NA, line = "1", left = "{PROGRAM}"),
    titles = data.frame(output_id = "T-1", line = c("1", "2"),
                        center = c("Demographics", "<{OUTPUT_POPULATION}>")),
    footnotes = data.frame(output_id = "T-1", line = "1", left = "SD = standard deviation."),
    tokens = data.frame(output_id = NA, name = "COMPANY", value = "Sample Pharma"),
    study = data.frame(key = "program_dir", value = "programs/tfl"))
}

rp_output <- function() {
  tfl_output("T-1", "table", page = list(
    rtf_page = list(orientation = "portrait", margin_top_in = 1),
    rtf_default_format = list(font_size_half_points = 16L),
    rtf_document = list(font_table = list(list(name = "Arial"))),
    rtf_tables = list(auto_section = TRUE),
    rtf_titles = list(font_size_half_points = 20L),
    header = list(`1` = list(left = "Sample Pharma", right = "Page {PAGE} of {TOTAL_PAGES}"),
                  `2` = list(center = "{OUTPUT_LABEL}")),
    footer = list(`1` = list(left = "{PROGRAM}")),
    titles = list(`1` = list(center = "Demographics"),
                  `2` = list(center = "<{OUTPUT_POPULATION}>")),
    footnotes = list(`1` = list(left = "SD = standard deviation.")),
    tokens = list(COMPANY = "Sample Pharma")))
}

rp_study <- function() {
  structure(list(study = list(settings = list(program_dir = "programs/tfl",
                                              program = "{output_id}.R"))),
            class = "tfl_study_spec")
}

test_that("a report's page: writes the code its sheets write", {
  sheets <- tfl_report_code(rp_sheets(), "T-1", content = "plan")
  yaml <- tfl_report_code(rp_output(), content = "plan", study = rp_study())
  expect_identical(yaml, sheets)
  expect_match(paste(yaml, collapse = "\n"), "rtf_titles(", fixed = TRUE)
  expect_error(tfl_report_code(rp_output(), setup = TRUE), "its `study`")
})

test_that("a line its tokens leave empty is left out, the program falls back to the ID", {
  o <- rp_output()
  code <- tfl_report_code(o, content = "plan")
  expect_false(any(grepl("<{OUTPUT_POPULATION}>", code, fixed = TRUE)))
  expect_true(any(grepl("program_fallback = \"T-1\"", code, fixed = TRUE)))
})

test_that("a report's page: makes the document its sheets make", {
  skip_if_not_installed("rtfreporter")
  pages <- rtfreporter::as_rtftables(data.frame(a = "1"))
  d_sheets <- tfl_report(rp_sheets(), "T-1", content = pages)
  d_yaml <- tfl_report(rp_output(), content = pages, study = rp_study())
  expect_identical(d_yaml, d_sheets)
})

test_that("page:'s calls check against their schema", {
  pg <- rp_output()$page
  for (fn in c("rtf_page", "rtf_default_format", "rtf_document", "rtf_tables", "rtf_titles")) {
    p <- tfl_schema_check(tfl_item(fn, pg[[fn]]))
    expect_identical(nrow(p), 0L, info = fn)
  }
  typo <- tfl_schema_check(tfl_item("rtf_page", list(orientaton = "portrait")))
  expect_identical(typo$level, "warning")
  skip_if_not_installed("rtfreporter")
  for (fn in c("rtf_page", "rtf_default_format", "rtf_document", "rtf_tables",
               "rtf_figures", "rtf_titles", "rtf_footnotes")) {
    expect_true(all(names(tfl_schema(fn)$args) %in%
                      names(formals(getExportedValue("rtfreporter", fn)))), info = fn)
  }
})

test_that("after the study's setup code, a YAML report says what its sheets say", {
  full <- rp_output()$page
  study_keys <- c("rtf_page", "rtf_default_format", "rtf_document", "header", "footer", "tokens")
  study <- rp_study()
  study$defaults <- list(page = full[study_keys])
  study$patterns <- list()
  o <- tfl_output("T-1", "table", extends = "defaults",
                  page = full[setdiff(names(full), study_keys)])
  sheets <- tfl_report_code(rp_sheets(), "T-1", content = "plan", setup = TRUE)
  yaml <- tfl_report_code(o, content = "plan", setup = TRUE, study = study)
  expect_identical(yaml, sheets)
  expect_true(any(grepl("header = study_header", yaml, fixed = TRUE)))
  expect_error(tfl_report_code(tfl_resolve_output(o, study), setup = TRUE, study = study),
               "as read")
})

test_that("the document says its RTF file: tfl_report_path()'s, in the run and in the code (#268)", {
  skip_if_not_installed("rtfreporter")
  # the sheets: the report row's file under the study's output_path
  sp <- rp_sheets()
  sp$study <- rbind(sp$study, data.frame(key = "output_path", value = "output/tfl"))
  doc <- tfl_report(sp, "T-1", content = rtfreporter::as_rtftables(data.frame(a = 1)))
  expect_identical(doc$document$file, tfl_report_path(sp, "T-1"))
  expect_identical(doc$document$file, file.path("output/tfl", "T-1.rtf"))
  code <- paste(tfl_report_code(sp, "T-1"), collapse = "\n")
  expect_match(code, 'file = "output/tfl/T-1.rtf"', fixed = TRUE)
  # a definition of no one report: no file said
  expect_null(.rtf_file_said("{output_id}.rtf"))
  # a YAML report: its file with its study; none without one
  y <- system.file("extdata", "spec-yaml", package = "tflspec")
  s <- tfl_read_study_spec(y)
  o <- tfl_read_outputs(y)[["T-14-1-1"]]
  doc <- tfl_report(o, content = rtfreporter::as_rtftables(data.frame(a = 1)), study = s)
  expect_identical(doc$document$file, tfl_report_path(o, study = s))
  expect_match(paste(tfl_report_code(o, study = s), collapse = "\n"),
               paste0('file = "', tfl_report_path(o, study = s), '"'), fixed = TRUE)
  doc0 <- tfl_report(tfl_resolve_output(o, s), content = rtfreporter::as_rtftables(data.frame(a = 1)))
  expect_null(doc0$document$file)
  # generate_rtfreport(doc) writes there
  d <- withr::local_tempdir()
  withr::local_dir(d)
  expect_identical(suppressMessages(rtfreporter::generate_rtfreport(doc)), tfl_report_path(o, study = s))
  expect_true(file.exists(tfl_report_path(o, study = s)))
})
