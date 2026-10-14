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
  expect_error(tfl_report_code(rp_output(), setup = TRUE), "not written yet")
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
