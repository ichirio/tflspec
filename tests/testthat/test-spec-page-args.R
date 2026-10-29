# A report's page: every argument its schema takes is written into the
# program, as rtfreporter's function takes it -- none dropped (#444).

pa_code <- function(page, kind = "table", content = NULL) {
  o <- tfl_output("T-1", kind, page = c(page, list(titles = list(`1` = list(center = "Table 1")),
                                                   footnotes = list(`1` = list(left = "Note.")))))
  if (!is.null(content)) o$content <- content
  paste(tfl_report_code(o, content = "plan"), collapse = "\n")
}

test_that("rtf_titles / rtf_footnotes: their style as the page says it", {
  st <- list(font_size_half_points = 20L, row_height_twips = 300L, markup = "none",
             font = "Arial", align = "left")
  code <- pa_code(list(rtf_titles = st, rtf_footnotes = st))
  for (fn in c("rtf_titles", "rtf_footnotes")) {
    call <- regmatches(code, regexpr(paste0(fn, "\\([^|]*"), code))
    for (a in c("font_size_half_points = 20", "row_height_twips = 300", "markup = \"none\"",
                "font = \"Arial\"", "align = \"left\"")) {
      expect_match(call, a, fixed = TRUE, info = paste(fn, a))
    }
  }
})

test_that("rtf_figures: align as the page says it", {
  code <- pa_code(list(rtf_figures = list(align = "left")), "figure")
  expect_match(code, "rtf_figures\\([^|]*align = \"left\"")
  expect_false(grepl("align", pa_code(list(), "figure"), fixed = TRUE))
})

test_that("rtf_tables: a font size left on the page (a content of R code) is written", {
  o <- tfl_output("T-1", "table", page = list(rtf_tables = list(font_size_half_points = 16L)))
  o$content <- structure(list(code = "plan <- data.frame(a = 1)"), class = "tfl_r")
  o <- tflspec:::.page_font_to_plan(o)
  expect_identical(o$page$rtf_tables$font_size_half_points, 16L)
  code <- paste(tfl_report_code(o, content = "plan"), collapse = "\n")
  expect_match(code, "rtf_tables\\([^|]*font_size_half_points = 16")
})

test_that("markup takes rtfreporter's words, not TRUE / FALSE", {
  ok <- function(fn, v) nrow(tfl_schema_check(tfl_item(fn, list(markup = v)))) == 0L
  for (fn in c("rtf_default_format", "rtf_titles", "rtf_footnotes")) {
    expect_true(ok(fn, "script"), info = fn)
    expect_true(ok(fn, "none"), info = fn)
    expect_true(ok(fn, c("script", "relational")), info = fn)
    expect_false(ok(fn, TRUE), info = fn)
  }
})

test_that("the written calls run in rtfreporter", {
  skip_if_not_installed("rtfreporter")
  st <- list(font_size_half_points = 20L, row_height_twips = 300L, markup = "none",
             font = "Arial", align = "left")
  o <- tfl_output("T-1", "table", page = list(
    rtf_titles = st, rtf_footnotes = st,
    titles = list(`1` = list(center = "Table 1")), footnotes = list(`1` = list(left = "Note."))))
  content <- rtfreporter::as_rtftables(data.frame(a = "1"))
  env <- list2env(list(content = content), parent = asNamespace("rtfreporter"))
  doc <- eval(parse(text = tfl_report_code(o, content = "content")), env)
  expect_s3_class(doc, "rtf_document")
})

test_that("rtf_document's watermark: a word, or its look as an rtf_watermark() call", {
  code <- pa_code(list(rtf_document = list(watermark = "DRAFT")))
  expect_match(code, "watermark = \"DRAFT\"", fixed = TRUE)
  wm <- tfl_call("rtf_watermark", list(text = "DRAFT", color = "#FF0000", angle = -30))
  code <- pa_code(list(rtf_document = list(watermark = wm)))
  expect_match(code, "watermark = rtf_watermark(text = \"DRAFT\", color = \"#FF0000\", angle = -30)",
               fixed = TRUE)
  ok <- function(v) nrow(tfl_schema_check(tfl_item("rtf_document", list(watermark = v)))) == 0L
  expect_true(ok("DRAFT"))
  expect_true(ok(wm))
  expect_false(ok(tfl_call("rtf_border", list(top = TRUE))))
  # and it runs: the document carries the watermark's look
  skip_if_not_installed("rtfreporter")
  o <- tfl_output("T-1", "table", page = list(rtf_document = list(watermark = wm)))
  env <- list2env(list(content = rtfreporter::as_rtftables(data.frame(a = "1"))),
                  parent = asNamespace("rtfreporter"))
  doc <- eval(parse(text = tfl_report_code(o, content = "content")), env)
  expect_s3_class(doc, "rtf_document")
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f))
  rtfreporter::generate_rtfreport(doc, f)
  rtf <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_match(rtf, "DRAFT", fixed = TRUE)
})

test_that("rtf_tables: read_attributes as the page says it", {
  code <- pa_code(list(rtf_tables = list(read_attributes = FALSE)))
  expect_match(code, "rtf_tables\\([^|]*read_attributes = FALSE")
  expect_false(grepl("read_attributes", pa_code(list()), fixed = TRUE))
  ok <- function(v) nrow(tfl_schema_check(tfl_item("rtf_tables", list(read_attributes = v)))) == 0L
  expect_true(ok(FALSE))
  expect_false(ok("no"))
  # and it runs: the table's blank rows attribute is not read
  skip_if_not_installed("rtfreporter")
  o <- tfl_output("T-1", "table", page = list(rtf_tables = list(read_attributes = FALSE)))
  env <- list2env(list(content = rtfreporter::as_rtftables(data.frame(a = "1"))),
                  parent = asNamespace("rtfreporter"))
  doc <- eval(parse(text = tfl_report_code(o, content = "content")), env)
  expect_s3_class(doc, "rtf_document")
})
