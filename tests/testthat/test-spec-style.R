# The document's style: page.rtf_style, rtfreporter's rtf_style() (#564,
# rtfreporter#804); an old SPEC's page.rtf_default_format is read as its
# shared level

st_report <- function(page) {
  o <- tfl_output("T-1", "table", page = c(list(titles = list(`1` = list(center = "Table 1"))), page))
  paste(tfl_report_code(o, content = "content"), collapse = "\n")
}

test_that("the program writes rtf_document(style = rtf_style()), its sections too", {
  code <- st_report(list(rtf_style = list(
    font_size_half_points = 18L, cell_padding_left_twips = 40L,
    table = list(header_row_height_twips = 260L, cell_valign = "center"),
    footnotes = list(font_size_half_points = 16L))))
  flat <- gsub("[[:space:]]+", " ", code)
  expect_match(flat, "style = rtf_style(", fixed = TRUE)
  expect_match(flat, "font_size_half_points = 18", fixed = TRUE)
  expect_match(flat, "table = list(header_row_height_twips = 260, cell_valign = \"center\")", fixed = TRUE)
  expect_match(flat, "footnotes = list(font_size_half_points = 16)", fixed = TRUE)
  expect_false(grepl("default_format", code, fixed = TRUE))
  # none said: no style
  expect_false(grepl("rtf_style", st_report(list()), fixed = TRUE))
})

test_that("an old SPEC's rtf_default_format is rtf_style's shared level; rtf_style's own keys win", {
  code <- st_report(list(rtf_default_format = list(font_size_half_points = 16L, title_format = "table")))
  expect_match(code, "rtf_style(", fixed = TRUE)
  expect_match(code, "font_size_half_points = 16", fixed = TRUE)
  expect_match(code, "title_format = \"table\"", fixed = TRUE)
  expect_false(grepl("default_format", code, fixed = TRUE))
  both <- st_report(list(rtf_default_format = list(font_size_half_points = 16L, markup = "none"),
                         rtf_style = list(font_size_half_points = 18L)))
  expect_match(both, "font_size_half_points = 18", fixed = TRUE)
  expect_match(both, "markup = \"none\"", fixed = TRUE)
  expect_false(grepl("font_size_half_points = 16", both, fixed = TRUE))
})

test_that("the schema takes rtf_style() as rtfreporter has it", {
  a <- list(font_size_half_points = 18L, title_format = "table",
            table = list(cell_valign = "center"), header = list(font_size_half_points = 14L))
  expect_identical(nrow(tfl_schema_check(tfl_item("rtf_style", a))), 0L)
  skip_if_not_installed("rtfreporter")
  skip_if_not(exists("rtf_style", envir = asNamespace("rtfreporter")), "rtfreporter before #804")
  expect_true(all(names(tfl_schema("rtf_style")$args) %in% names(formals(rtfreporter::rtf_style))))
})

test_that("the sheets: the page sheet's text columns are rtf_style's, an old SPEC's too, and back", {
  st <- tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))
  o <- tfl_output("T-1", "table", page = list(rtf_default_format = list(font_size_half_points = 16L),
                                              rtf_style = list(table = list(cell_valign = "center"))))
  sh <- suppressMessages(tfl_sheets_from_spec(st, list(`T-1` = o)))
  pg <- sh$table$page[sh$table$page$output_id %in% "T-1", ]
  expect_identical(as.character(pg$font_size_half_points), "16")
  expect_match(pg$page_more, "rtf_style", fixed = TRUE)
  out <- withr::local_tempdir()
  suppressMessages(suppressWarnings(tfl_yaml_from_sheets(sh, out)))
  f <- list.files(out, "^T-1[.]yml$", recursive = TRUE, full.names = TRUE)
  back <- tfl_read_output(f[1L])$page
  expect_null(back$rtf_default_format)
  expect_identical(back$rtf_style$font_size_half_points, 16L)
  expect_identical(back$rtf_style$table$cell_valign, "center")
})

test_that("the program runs in rtfreporter with no deprecation", {
  skip_if_not_installed("rtfreporter")
  skip_if_not(exists("rtf_style", envir = asNamespace("rtfreporter")), "rtfreporter before #804")
  o <- tfl_output("T-1", "table", page = list(
    titles = list(`1` = list(center = "Table 1")),
    rtf_default_format = list(font_size_half_points = 16L, title_format = "table"),
    rtf_style = list(table = list(cell_valign = "center"))))
  content <- rtfreporter::as_rtftables(data.frame(a = "1"))
  env <- list2env(list(content = content), parent = asNamespace("rtfreporter"))
  expect_no_warning(doc <- eval(parse(text = tfl_report_code(o, content = "content")), env))
  f <- tempfile(fileext = ".rtf")
  expect_no_warning(suppressMessages(rtfreporter::generate_rtfreport(doc, f)))
  expect_true(file.exists(f))
})

test_that("an old key of a near layer beats a new key of a far layer (each layer read first)", {
  st <- structure(list(defaults = list(), study = list(settings = list(style = "std")),
                       styles = list(std = list(page = list(rtf_style = list(font_size_half_points = 18L)))),
                       patterns = list(), data = list()),
                  class = "tfl_study_spec")
  o <- tfl_output("T-1", "table", extends = "defaults",
                  page = list(rtf_default_format = list(font_size_half_points = 16L)))
  ro <- tfl_resolve_output(o, st)
  expect_identical(ro$page$rtf_style$font_size_half_points, 16L)
  expect_null(ro$page$rtf_default_format)
})
