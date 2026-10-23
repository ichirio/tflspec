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
  st <- structure(list(defaults = list(page = list(rtf_document = list(
    font_table = list(list(name = fs_slots)), font_lang = list(east_asian = "ja"))))),
    class = "tfl_study_spec")
  code <- paste(tflspec:::.study_setup_code(st), collapse = "\n")
  expect_match(code, "rtfreporter.font = c(latin = \"Times New Roman\", east_asian = \"MS Mincho\")",
               fixed = TRUE)
  expect_match(code, "rtfreporter.font_lang = c(east_asian = \"ja\")", fixed = TRUE)
  # one family: a string, as before
  st$defaults$page$rtf_document <- list(font_table = list(list(name = "Courier New")))
  expect_match(paste(tflspec:::.study_setup_code(st), collapse = "\n"),
               "rtfreporter.font = \"Courier New\"", fixed = TRUE)
})

test_that("the sheets say the slots and the languages, and give them back", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "spec", "outputs"), recursive = TRUE)
  writeLines(c("page:", "  rtf_document:", "    font_table:",
               "      - name: {latin: Times New Roman, east_asian: MS Mincho}",
               "    font_lang: {east_asian: ja}"), file.path(d, "spec", "defaults.yml"))
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
