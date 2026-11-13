# The running header's and footer's look (page.rtf_header, page.rtf_footer:
# rtfreporter's rtf_header() / rtf_footer() arguments) and the footnotes'
# border, in the report's call and the study setup (#450)

bs_page <- function(style = list()) {
  c(list(header = list(`1` = list(left = "Sample Pharma", right = "Page {PAGE} of {TOTAL_PAGES}")),
         footer = list(`1` = list(left = "{PROGRAM}")),
         footnotes = list(`1` = list(left = "Note."))), style)
}

test_that("the schema takes rtf_header / rtf_footer's arguments, rtfreporter's", {
  st <- list(border = tfl_call("rtf_border", list(bottom = TRUE)), width = "page",
             font_size_half_points = 16L, font = "Arial", drop_empty_rows = TRUE,
             row_height_twips = 240L)
  for (fn in c("rtf_header", "rtf_footer")) {
    expect_identical(nrow(tfl_schema_check(tfl_item(fn, st))), 0L, info = fn)
  }
  expect_identical(nrow(tfl_schema_check(tfl_item("rtf_footnotes", list(
    border = tfl_call("rtf_border", list(top = TRUE)))))), 0L)
  skip_if_not_installed("rtfreporter")
  for (fn in c("rtf_header", "rtf_footer", "rtf_footnotes")) {
    expect_true(all(names(tfl_schema(fn)$args) %in%
                      names(formals(getExportedValue("rtfreporter", fn)))), info = fn)
  }
})

test_that("a report's bands: their look in rtf_header() / rtf_footer(), the footnotes' border", {
  o <- tfl_output("T-1", "table", page = bs_page(list(
    rtf_header = list(border = tfl_call("rtf_border", list(bottom = TRUE)), font_size_half_points = 16L),
    rtf_footer = list(width = 0.8, drop_empty_rows = TRUE),
    rtf_footnotes = list(border = tfl_call("rtf_border", list(top = TRUE))))))
  code <- paste(tfl_report_code(o, content = "content"), collapse = "\n")
  expect_match(code, "rtf_header\\([^|]*border = rtf_border\\(bottom = TRUE\\)")
  expect_match(code, "rtf_header\\([^|]*font_size_half_points = 16")
  expect_match(code, "rtf_footer\\([^|]*width = 0.8")
  expect_match(code, "rtf_footer\\([^|]*drop_empty_rows = TRUE")
  expect_match(code, "rtf_footnotes\\([^|]*border = rtf_border\\(top = TRUE\\)")
  # none said: the calls as before
  plain <- paste(tfl_report_code(tfl_output("T-1", "table", page = bs_page()), content = "content"),
                 collapse = "\n")
  expect_false(grepl("border", plain, fixed = TRUE))
})

test_that("the program runs in rtfreporter", {
  skip_if_not_installed("rtfreporter")
  o <- tfl_output("T-1", "table", page = bs_page(list(
    rtf_header = list(border = tfl_call("rtf_border", list(bottom = TRUE)), font = "Arial"),
    rtf_footnotes = list(border = tfl_call("rtf_border", list(top = TRUE))))))
  content <- rtfreporter::as_rtftables(data.frame(a = "1"))
  env <- list2env(list(content = content), parent = asNamespace("rtfreporter"))
  doc <- eval(parse(text = tfl_report_code(o, content = "content")), env)
  expect_s3_class(doc, "rtf_document")
})

test_that("the study setup: its bands' look in header_<name> / footer_<name>", {
  st <- structure(list(study = list(settings = list(style = "std")),
                       styles = list(std = list(page = bs_page(list(
                         rtf_header = list(border = tfl_call("rtf_border", list(bottom = TRUE)))))))),
                  class = "tfl_study_spec")
  code <- paste(tflspec:::.study_setup_code(st), collapse = "\n")
  expect_match(code, "header_std <- rtf_header\\([^|]*border = rtf_border\\(bottom = TRUE\\)")
})

test_that("a band's text colour: page.rtf_header.color / page.rtf_footer.color", {
  expect_identical(nrow(tfl_schema_check(tfl_item("rtf_header", list(color = "#7F7F7F")))), 0L)
  o <- tfl_output("T-1", "table", page = bs_page(list(
    rtf_header = list(color = "#7F7F7F"), rtf_footer = list(color = "#003366"))))
  code <- paste(tfl_report_code(o, content = "content"), collapse = "\n")
  expect_match(code, "rtf_header\\([^|]*color = \"#7F7F7F\"")
  expect_match(code, "rtf_footer\\([^|]*color = \"#003366\"")
  skip_if_not_installed("rtfreporter")
  skip_if_not("color" %in% names(formals(rtfreporter::rtf_header)))
  content <- rtfreporter::as_rtftables(data.frame(a = "1"))
  env <- list2env(list(content = content), parent = asNamespace("rtfreporter"))
  doc <- eval(parse(text = tfl_report_code(o, content = "content")), env)
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f))
  rtfreporter::generate_rtfreport(doc, f)
  rtf <- paste(readLines(f, warn = FALSE), collapse = "")
  # the colours are in the document's colour table
  expect_match(rtf, "\\red127\\green127\\blue127", fixed = TRUE)
  expect_match(rtf, "\\red0\\green51\\blue102", fixed = TRUE)
})
