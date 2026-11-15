# Fonts per script and their languages (rtfreporter#715) in the SPEC: a
# font table entry's name a family per slot, rtf_document(font_lang), the
# titles' and footnotes' font, and the study setup's options (#449)

fs_slots <- list(latin = "Times New Roman", east_asian = "MS Mincho")

test_that("the schema takes a family per slot and a language per slot", {
  ok <- function(fn, a) nrow(tfl_schema_check(tfl_item(fn, a))) == 0L
  expect_true(ok("rtf_document", list(font_table = list(list(name = fs_slots)))))
  expect_true(ok("rtf_document", list(font_table = list(list(name = "Arial")))))
  expect_true(ok("rtf_document", list(font_lang = list(east_asian = "ja"))))
  expect_false(ok("rtf_document", list(font_lang = list(east_asian = "jp"))))
  expect_true(ok("rtf_titles", list(font = fs_slots)))
  expect_true(ok("rtf_footnotes", list(font = "Arial")))
})

test_that("a report's rtf_document() call: the slots as a named vector, its font_lang", {
  o <- tfl_output("T-1", "table", page = list(
    rtf_document = list(font_table = list(list(name = fs_slots)), font_lang = list(east_asian = "ja")),
    rtf_titles = list(font = fs_slots),
    titles = list(`1` = list(center = "Table 1"))))
  code <- paste(tfl_report_code(o, content = "content"), collapse = "\n")
  expect_match(code, "name = c(latin = \"Times New Roman\", east_asian = \"MS Mincho\")", fixed = TRUE)
  expect_match(code, "font_lang = c(east_asian = \"ja\")", fixed = TRUE)
  expect_match(code, "rtf_titles\\([^|]*font = c\\(latin = \"Times New Roman\"")
  # (one family: a string, as before)
  o1 <- tfl_output("T-1", "table", page = list(rtf_document = list(font_table = list(list(name = "Arial")))))
  expect_match(paste(tfl_report_code(o1, content = "content"), collapse = "\n"),
               "font_table = list(list(name = \"Arial\"))", fixed = TRUE)
})

test_that("the program runs in rtfreporter and writes the slots' fonts", {
  skip_if_not_installed("rtfreporter")
  o <- tfl_output("T-1", "table", page = list(
    rtf_document = list(font_table = list(list(name = fs_slots)), font_lang = list(east_asian = "ja"))))
  content <- rtfreporter::as_rtftables(data.frame(a = "1"))
  env <- list2env(list(content = content), parent = asNamespace("rtfreporter"))
  doc <- eval(parse(text = tfl_report_code(o, content = "content")), env)
  expect_s3_class(doc, "rtf_document")
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f))
  suppressMessages(rtfreporter::generate_rtfreport(doc, f, overwrite = TRUE, program = "p.R"))
  rtf <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_match(rtf, "MS Mincho", fixed = TRUE)
  expect_match(rtf, "Times New Roman", fixed = TRUE)
})

test_that("the study setup: options() with the slots and the languages", {
  st <- structure(list(study = list(settings = list(style = "std")),
                       styles = list(std = list(page = list(rtf_document = list(
    font_table = list(list(name = fs_slots)), font_lang = list(east_asian = "ja")))))),
    class = "tfl_study_spec")
  code <- paste(tflspec:::.study_setup_code(st), collapse = "\n")
  expect_match(code, "rtfreporter.font = c(latin = \"Times New Roman\", east_asian = \"MS Mincho\")",
               fixed = TRUE)
  expect_match(code, "rtfreporter.font_lang = c(east_asian = \"ja\")", fixed = TRUE)
  # one family: a string, as before
  st$styles$std$page$rtf_document <- list(font_table = list(list(name = "Courier New")))
  expect_match(paste(tflspec:::.study_setup_code(st), collapse = "\n"),
               "rtfreporter.font = \"Courier New\"", fixed = TRUE)
})

test_that("the sheets say the slots and the languages, and give them back", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "spec", "outputs"), recursive = TRUE)
  writeLines(c("styles:", "  std:", "    page:", "      rtf_document:", "        font_table:",
               "          - name: {latin: Times New Roman, east_asian: MS Mincho}",
               "        font_lang: {east_asian: ja}"), file.path(d, "spec", "defaults.yml"))
  writeLines("settings: {style: std}", file.path(d, "study.yml"))
  tfl_write_output(tfl_output("T-1", "table", extends = "defaults"),
                   file.path(d, "spec", "outputs", "T-1.yml"))
  sh <- tfl_sheets_from_yaml(d)
  pg <- sh$table$page
  st <- is.na(pg$output_id)
  expect_identical(pg$font[st], "latin = Times New Roman | east_asian = MS Mincho")
  expect_identical(pg$font_lang[st], "east_asian = ja")
  page <- tflspec:::.page_put(list(), "page", "font", pg$font[st], function(...) NULL, NA)
  page <- tflspec:::.page_put(page, "page", "font_lang", pg$font_lang[st], function(...) NULL, NA)
  expect_identical(page$rtf_document$font_table[[1L]]$name,
                   list(latin = "Times New Roman", east_asian = "MS Mincho"))
  expect_identical(page$rtf_document$font_lang, list(east_asian = "ja"))
  # one family: as it is
  page <- tflspec:::.page_put(list(), "page", "font", "Arial", function(...) NULL, NA)
  expect_identical(page$rtf_document$font_table[[1L]]$name, "Arial")
})

test_that("a sheets-only study's setup writes the slots and languages as the YAML route does", {
  sp <- tfl_table_spec(
    report = data.frame(output_id = c(NA, "T-1"), type = c("table", NA)),
    page = data.frame(output_id = NA,
                      font = "latin = Times New Roman | east_asian = MS Mincho",
                      font_lang = "east_asian = ja", font_size_half_points = "20"))
  st <- structure(list(study = list(settings = list(style = "std")),
                       styles = list(std = list(page = list(
    rtf_document = list(font_table = list(list(name = fs_slots)), font_lang = list(east_asian = "ja")),
    rtf_style = list(font_size_half_points = 20L))))),
    class = "tfl_study_spec")
  from_sheets <- tfl_report_setup_code(sp)
  # (the options; the YAML route also writes the configuration's header and footer)
  expect_identical(from_sheets, tflspec:::.study_setup_code(st)[seq_along(from_sheets)])
  code <- paste(from_sheets, collapse = "\n")
  expect_match(code, "rtfreporter.font = c(latin = \"Times New Roman\", east_asian = \"MS Mincho\")",
               fixed = TRUE)
  expect_match(code, "rtfreporter.font_lang = c(east_asian = \"ja\")", fixed = TRUE)
  # a program written with the setup leaves the study's font to the options
  prog <- paste(tfl_report_code(sp, "T-1", content = "pages", setup = TRUE), collapse = "\n")
  expect_false(grepl("font_lang", prog, fixed = TRUE))
})

test_that("a sheets-only study's setup gives the RTF the slots' charset and language", {
  skip_if_not_installed("rtfreporter", "0.8.2.9025")
  sp <- tfl_table_spec(
    report = data.frame(output_id = c(NA, "T-1"), type = c("table", NA)),
    page = data.frame(output_id = NA,
                      font = "latin = Courier New | east_asian = MS Mincho",
                      font_lang = "east_asian = ja"))
  old <- options(rtfreporter.font = NULL, rtfreporter.font_lang = NULL)
  on.exit(options(old), add = TRUE)
  eval(parse(text = tfl_report_setup_code(sp)), globalenv())
  # (a document that says no font of its own takes the setup's options)
  doc <- rtfreporter::rtf_tables(rtfreporter::rtf_document(),
                                 rtfreporter::as_rtftables(data.frame(a = "1")))
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  suppressMessages(rtfreporter::generate_rtfreport(doc, f, overwrite = TRUE, program = "p.R"))
  rtf <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_match(rtf, "\\fcharset128 MS Mincho", fixed = TRUE)
  expect_match(rtf, "\\langfe1041", fixed = TRUE)
})

test_that("a single font name in the sheets' setup is written as before", {
  sp <- tfl_table_spec(
    report = data.frame(output_id = NA, type = "table"),
    page = data.frame(output_id = NA, font = "Courier New", font_size_half_points = "20"))
  expect_identical(tfl_report_setup_code(sp),
                   c("options(", "  rtfreporter.font = \"Courier New\",",
                     "  rtfreporter.font_size_half_points = 20", ")"))
})

test_that("a sheets-only report program writes the slots and languages as the YAML route does", {
  sp <- tfl_table_spec(
    report = data.frame(output_id = NA, type = "table"),
    page = data.frame(output_id = NA,
                      font = "latin = Times New Roman | east_asian = MS Mincho",
                      font_lang = "east_asian = ja"))
  code <- paste(tfl_report_code(sp, "T-1", content = "content"), collapse = "\n")
  o <- tfl_output("T-1", "table", page = list(
    rtf_document = list(font_table = list(list(name = fs_slots)), font_lang = list(east_asian = "ja"))))
  yaml <- paste(tfl_report_code(o, content = "content"), collapse = "\n")
  expect_match(code, "name = c(latin = \"Times New Roman\", east_asian = \"MS Mincho\")", fixed = TRUE)
  expect_match(code, "font_lang = c(east_asian = \"ja\")", fixed = TRUE)
  expect_match(yaml, "name = c(latin = \"Times New Roman\", east_asian = \"MS Mincho\")", fixed = TRUE)
  expect_match(yaml, "font_lang = c(east_asian = \"ja\")", fixed = TRUE)
  # the study's font is the setup's: with setup = TRUE the program says none
  # (a file of defaults only, and a report narrowed from rows of its own)
  prog <- paste(tfl_report_code(sp, "T-1", content = "content", setup = TRUE), collapse = "\n")
  expect_false(grepl("font", prog, fixed = TRUE))
  sp2 <- tfl_table_spec(
    report = data.frame(output_id = c(NA, "T-1"), type = c("table", "table")),
    page = data.frame(output_id = c(NA, "T-1"), font = c("Courier New", NA),
                      font_size_half_points = c(NA, "16")))
  prog2 <- paste(tfl_report_code(sp2, "T-1", content = "content", setup = TRUE), collapse = "\n")
  expect_false(grepl("font_table", prog2, fixed = TRUE))
  expect_match(prog2, "font_size_half_points = 16", fixed = TRUE)
})

test_that("a sheets-only report program builds an RTF with the slots' charset and language", {
  skip_if_not_installed("rtfreporter", "0.8.2.9025")
  sp <- tfl_table_spec(
    report = data.frame(output_id = NA, type = "table"),
    page = data.frame(output_id = NA,
                      font = "latin = Courier New | east_asian = MS Mincho",
                      font_lang = "east_asian = ja"))
  env <- list2env(list(content = rtfreporter::as_rtftables(data.frame(a = "1"))),
                  parent = asNamespace("rtfreporter"))
  doc <- eval(parse(text = tfl_report_code(sp, "T-1", content = "content")), env)
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f))
  suppressMessages(rtfreporter::generate_rtfreport(doc, f, overwrite = TRUE, program = "p.R"))
  rtf <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_match(rtf, "\\fcharset128 MS Mincho", fixed = TRUE)
  expect_match(rtf, "\\langfe1041", fixed = TRUE)
})

test_that("a single font name in a sheets-only report program is written as before", {
  sp <- tfl_table_spec(
    report = data.frame(output_id = NA, type = "table"),
    page = data.frame(output_id = NA, font = "Courier New"))
  expect_identical(tfl_report_code(sp, "T-1", content = "content"),
                   c("doc <- rtf_document(",
                     "  font_table = list(list(name = \"Courier New\")),",
                     "  program_fallback = \"T-1\",",
                     "  file = \"T-1.rtf\"",
                     ") |>", "  rtf_tables(content)"))
})
