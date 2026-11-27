# A report's analysis step is written `analysis:` (canon 11-15 (5), `ard:` ->
# `analysis:`, no other name): the report's map, a template SPEC's part and
# a step of `extends`.  The old key `ard:` stops, saying to write
# `analysis:`.  The object keeps its field `$ard` (the ARD object).

ak_write <- function(lines) {
  f <- withr::local_tempfile(fileext = ".yml", .local_envir = parent.frame())
  writeLines(lines, f)
  f
}

ak_table <- c("id: T-1", "kind: table", "common: {population: SAF}", "extends: [defaults]",
              "data:", "  - {id: adsl_sub, from: ADSL}",
              "analysis:", "  analyses:",
              "    - {id: SEX, method: categorical, data: adsl_sub, by: [TRT01A], variables: [SEX]}",
              "content:", "  - table_plan: {cols: TRT01A}")

test_that("`analysis:` is read into the report's ARD part and written back as `analysis:`", {
  o <- tfl_read_output(ak_write(ak_table))
  expect_identical(vapply(o$ard$analyses, `[[`, "", "id"), "SEX")
  expect_null(o$analysis)
  g <- withr::local_tempfile(fileext = ".yml")
  tfl_write_output(o, g)
  txt <- readLines(g)
  expect_true("analysis:" %in% txt)
  expect_false(any(grepl("^ard:", txt)))
  back <- tfl_read_output(g)
  expect_identical(back$ard, o$ard)
  # written again: the same text
  h <- withr::local_tempfile(fileext = ".yml")
  tfl_write_output(back, h)
  expect_identical(readLines(h), txt)
})

test_that("the old key `ard:` stops, saying to write `analysis:` (a report, a template part, an extends step)", {
  old <- sub("^analysis:$", "ard:", ak_table)
  expect_error(tfl_read_output(ak_write(old)), "`ard:` is no longer read: write `analysis:`", fixed = TRUE)
  expect_error(tfl_read_output(ak_write(old)), "canon 11-15", fixed = TRUE)
  # an empty `ard:` too
  expect_error(tfl_read_output(ak_write(c("id: T-1", "kind: table", "ard:", "content: []"))),
               "write `analysis:`", fixed = TRUE)
  # a template SPEC's part: on read, and on an object made in R
  tpl <- c("id: T-2", "kind: table", "common: {population: SAF}",
           "template:", "  name: demog", "  ard: {by: TRT01A, continuous: [AGE]}")
  expect_error(tfl_read_output(ak_write(tpl)), "`template: {ard:}` is no longer read: write `template: {analysis:}`",
               fixed = TRUE)
  o <- tfl_output("T-2", "table", template = list(name = "demog", ard = list(by = "TRT01A")))
  expect_error(tflspec:::.tpl_spec_of(o), "write `template: {analysis:}`", fixed = TRUE)
  ok <- tfl_read_output(ak_write(sub("  ard:", "  analysis:", tpl, fixed = TRUE)))
  a <- tflspec:::.tpl_spec_of(ok)$args
  expect_identical(lapply(a[c("by", "continuous")], unlist), list(by = "TRT01A", continuous = "AGE"))
  # a step of `extends`
  ext <- c("id: T-3", "kind: table", "extends: {ard: none}", "content: []")
  expect_error(tfl_read_output(ak_write(ext)), "`extends: {ard:}` is no longer read: write `extends: {analysis:}`",
               fixed = TRUE)
  expect_error(tfl_output("T-3", "table", extends = list(ard = "none")), "write `extends: {analysis:}`", fixed = TRUE)
  e <- tfl_output("T-3", "table", extends = list(analysis = "none"))
  expect_identical(tfl_output_extends(e, "analysis"), character())
  expect_identical(tfl_output_extends(e, "content"), "defaults")
})

test_that("the sheets and the workbook write `analysis:`", {
  src <- system.file("extdata", "spec-yaml", package = "tflspec")
  b <- suppressMessages(tfl_sheets_from_yaml(src))
  to <- withr::local_tempdir()
  invisible(suppressWarnings(suppressMessages(tfl_yaml_from_sheets(b, to))))
  f1 <- list.files(to, "^T-14-1-1[.]yml$", recursive = TRUE, full.names = TRUE)
  expect_length(f1, 1L)
  txt <- readLines(f1[1L])
  expect_true("analysis:" %in% txt)
  expect_false(any(grepl("^ard:", txt)))
  expect_identical(vapply(tfl_read_outputs(to)[["T-14-1-1"]]$ard$analyses, `[[`, "", "id"),
                   vapply(tfl_read_outputs(src)[["T-14-1-1"]]$ard$analyses, `[[`, "", "id"))
  skip_if_not_installed("openxlsx")
  d <- withr::local_tempdir()
  file.copy(list.files(src, full.names = TRUE), d, recursive = TRUE)
  f <- file.path(d, "view.xlsx")
  tfl_write_spec_xlsx(d, f)
  wb <- openxlsx::loadWorkbook(f)
  x <- openxlsx::read.xlsx(wb, "analyses")
  x[] <- lapply(x, as.character)
  x$variables[x$id == "CAT"] <- "AGEGR1 | SEX"
  openxlsx::removeWorksheet(wb, "analyses"); openxlsx::addWorksheet(wb, "analyses")
  openxlsx::writeData(wb, "analyses", x, keepNA = FALSE)
  openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
  tfl_import_spec_xlsx(d, f)
  txt <- readLines(file.path(d, "outputs", "T-14-1-1.yml"))
  expect_true("analysis:" %in% txt)
  expect_false(any(grepl("^ard:", txt)))
  expect_identical(tfl_read_outputs(d)[["T-14-1-1"]]$ard$analyses[[3L]]$variables, c("AGEGR1", "SEX"))
})

test_that("the readers of the plain lists see `analysis:`: the AI check, the check's item paths", {
  r <- tflspec:::.ai_read_table(list(outputs = list(list(id = "T-1", kind = "table",
    ard = list(analyses = list(list(id = "A", method = "categorical")))))))
  expect_true(any(grepl("write `analysis:`", r$problems$problem, fixed = TRUE)))
  r <- tflspec:::.ai_read_table(list(outputs = list(list(id = "T-1", kind = "table",
    analysis = list(analyses = list(list(id = "A", method = "categorical")))))))
  expect_identical(nrow(r$problems), 0L)
  expect_identical(r$outputs[["T-1"]]$ard$analyses[[1L]]$id, "A")
  # the check's path is the file's: `analysis$analyses$<id>$<field>`, its part the ARD's
  w <- tflspec:::.r_walk(NULL, list(`T-1` = tfl_output("T-1", "table", ard = list(
    analyses = list(list(id = "A", method = "custom", where = tfl_r("x >")))))), "T-1")
  expect_identical(w$item, "analysis$analyses$A$where")
  expect_identical(w$part, "ard")
})
