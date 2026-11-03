# Vertical cell padding and a line's own height (#562, rtfreporter#125):
# the SPEC takes rtfreporter's cell_padding_top_twips /
# cell_padding_bottom_twips where it takes the left and right, and a
# header, footer, title or footnote line's row_height_twips

pr_page <- function(...) {
  utils::modifyList(list(
    header = list(`1` = list(left = "Sample Pharma", right = "Page {PAGE} of {TOTAL_PAGES}")),
    titles = list(`1` = list(center = "Table 14.1.1"), `2` = list(center = "Demographics")),
    footnotes = list(`1` = list(left = "Note."))), list(...))
}

test_that("the schema takes the top and bottom padding where it takes the left and right", {
  pad <- list(cell_padding_top_twips = 20L, cell_padding_bottom_twips = 40L)
  for (fn in c("plan_style", "rtf_default_format", "rtf_header", "rtf_footer")) {
    expect_identical(nrow(tfl_schema_check(tfl_item(fn, pad))), 0L, info = fn)
    expect_true(all(names(pad) %in% names(tfl_schema(fn)$args)), info = fn)
  }
  expect_identical(tfl_schema("plan_style")$args$cell_padding_top_twips$label,
                   "Cell padding, top (twips)")
  skip_if_not_installed("rtfreporter")
  skip_if_not("cell_padding_top_twips" %in% names(formals(rtfreporter::plan_style)),
              "rtfreporter before #125")
  for (fn in c("plan_style", "rtf_header", "rtf_footer")) {
    expect_true(all(names(pad) %in% names(formals(getExportedValue("rtfreporter", fn)))), info = fn)
  }
  # the style sheet's columns
  expect_true(all(names(pad) %in% names(tflspec:::.ard_spec_types$style)))
})

test_that("a band's padding in rtf_header(); a line's height in its row list", {
  o <- tfl_output("T-1", "table", page = pr_page(
    rtf_header = list(cell_padding_top_twips = 20L, cell_padding_bottom_twips = 20L),
    header = list(`1` = list(left = "Sample Pharma", right = "Page {PAGE} of {TOTAL_PAGES}",
                             row_height_twips = 400L)),
    titles = list(`1` = list(center = "Table 14.1.1", row_height_twips = 360L)),
    footnotes = list(`1` = list(left = "Note.", row_height_twips = 300L))))
  code <- paste(tfl_report_code(o, content = "content"), collapse = "\n")
  expect_match(code, "rtf_header\\([^|]*cell_padding_top_twips = 20")
  expect_match(code, "rtf_header\\([^|]*cell_padding_bottom_twips = 20")
  # (the program lays a long call out a line an argument)
  flat <- gsub("[[:space:]]+", " ", code)
  expect_match(flat, "list( columns = c(l = \"Sample Pharma\", r = \"Page {PAGE} of {TOTAL_PAGES}\"), row_height_twips = 400 )",
               fixed = TRUE)
  expect_match(flat, "list(text = \"Table 14.1.1\", align = \"center\", row_height_twips = 360)", fixed = TRUE)
  expect_match(flat, "list(columns = c(l = \"Note.\"), row_height_twips = 300)", fixed = TRUE)
  # a line without a height: as before
  expect_match(code, "c(c = \"Demographics\")", fixed = TRUE)
  plain <- paste(tfl_report_code(tfl_output("T-1", "table", page = pr_page()), content = "content"),
                 collapse = "\n")
  expect_false(grepl("row_height_twips|cell_padding", plain))
  # a title line with two texts cannot have a height of its own
  bad <- tfl_output("T-1", "table", page = pr_page(
    titles = list(`1` = list(left = "a", right = "b", row_height_twips = 300L))))
  expect_error(tfl_report_code(bad, content = "content"), "one text")
})

test_that("a line's height goes through the sheets and back, and the AI check takes it", {
  o <- tfl_output("T-1", "table", page = pr_page(
    titles = list(`1` = list(center = "Table 14.1.1", row_height_twips = 360L))))
  st <- tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))
  sh <- suppressMessages(tfl_sheets_from_spec(st, list(`T-1` = o)))
  ti <- sh$table$titles[sh$table$titles$output_id %in% "T-1", ]
  expect_identical(as.character(ti$row_height_twips[ti$line == "1"]), "360")
  # back to YAML: the line keeps it
  out <- withr::local_tempdir()
  suppressMessages(suppressWarnings(tfl_yaml_from_sheets(sh, out)))
  f <- list.files(out, "^T-1[.]yml$", recursive = TRUE, full.names = TRUE)
  expect_identical(tfl_read_output(f[1L])$page$titles$`1`$row_height_twips, 360L)
  # the AI check: a line's height is not a text cut at a comma
  p <- tflspec:::.ai_check_table_output(o, "outputs[1] T-1", list(), NULL)
  expect_false(any(grepl("besides left / center / right", unlist(p))))
  o$page$titles$`1`$width <- "x"
  p <- tflspec:::.ai_check_table_output(o, "outputs[1] T-1", list(), NULL)
  expect_true(any(grepl("`width` besides left / center / right", unlist(p), fixed = TRUE)))
})

test_that("the program runs in rtfreporter: the lines' heights in the RTF", {
  skip_if_not_installed("rtfreporter")
  skip_if_not("cell_padding_top_twips" %in% names(formals(rtfreporter::rtf_header)),
              "rtfreporter before #125")
  o <- tfl_output("T-1", "table", page = pr_page(
    rtf_header = list(cell_padding_top_twips = 20L),
    header = list(`1` = list(left = "Sample Pharma", right = "Page {PAGE} of {TOTAL_PAGES}",
                             row_height_twips = 400L)),
    titles = list(`1` = list(center = "Table 14.1.1", row_height_twips = 360L)),
    footnotes = list(`1` = list(left = "Note.", row_height_twips = 300L)),
    rtf_default_format = list(title_format = "table", footnote_format = "table")))
  content <- rtfreporter::as_rtftables(data.frame(a = "1"))
  env <- list2env(list(content = content), parent = asNamespace("rtfreporter"))
  # (rtf_default_format() is deprecated in rtfreporter next: rtf_style())
  doc <- suppressWarnings(eval(parse(text = tfl_report_code(o, content = "content")), env))
  f <- tempfile(fileext = ".rtf")
  suppressMessages(rtfreporter::generate_rtfreport(doc, f))
  rtf <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_match(rtf, "\\trrh400", fixed = TRUE)
  expect_match(rtf, "\\trrh360", fixed = TRUE)
  expect_match(rtf, "\\trrh300", fixed = TRUE)
})
