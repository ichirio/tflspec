# An edited flat-view workbook taken back into the YAML spec, by key

test_that("edited rows update their items by key; new rows add; the rest is untouched", {
  skip_if_not_installed("openxlsx")
  src <- system.file("extdata", "spec-yaml", package = "tflspec")
  d <- withr::local_tempdir()
  file.copy(list.files(src, full.names = TRUE), d, recursive = TRUE)
  f <- file.path(d, "view.xlsx")
  tfl_write_spec_xlsx(d, f)
  # edit in "Excel": a token, a title line, an analysis's variables, a new footnote line,
  # a code list's label, a new population row
  wb <- openxlsx::loadWorkbook(f)
  ed <- function(sheet, fun) {
    x <- openxlsx::read.xlsx(wb, sheet)
    x[] <- lapply(x, as.character)
    x <- fun(x)
    openxlsx::removeWorksheet(wb, sheet); openxlsx::addWorksheet(wb, sheet)
    openxlsx::writeData(wb, sheet, x, keepNA = FALSE)
  }
  ed("tokens", function(x) { x$value[x$name == "COMPANY"] <- "Acme Pharma"; x })
  ed("titles", function(x) { x$center[x$line == "2"] <- "Demographics (edited)"; x })
  ed("analyses", function(x) { x$variables[x$id == "CAT"] <- "AGEGR1 | SEX"; x })
  ed("footnotes", function(x) rbind(x, data.frame(output_id = "T-14-1-1", line = "2", left = "New note.", stringsAsFactors = FALSE)))
  ed("codelists", function(x) { x$label[x$value == "Control"] <- "Ctrl"; x })
  openxlsx::saveWorkbook(wb, f, overwrite = TRUE)

  r <- tfl_import_spec_xlsx(d, f)
  expect_true(any(r$changes$sheet == "tokens" & r$changes$to == "Acme Pharma"))
  expect_true(any(r$changes$sheet == "titles" & r$changes$key == "T-14-1-1 / 2"))
  expect_true(any(r$changes$sheet == "analyses" & r$changes$column == "variables"))
  expect_true(any(grepl("^footnotes: T-14-1-1 / 2", r$added)))
  expect_true(any(r$changes$sheet == "codelists" & r$changes$to == "Ctrl"))
  # the files: the defaults' token, the report's title, analyses, footnote, code list
  s <- tfl_read_study_spec(d)
  expect_identical(s$defaults$page$tokens$COMPANY, "Acme Pharma")
  expect_identical(s$defaults$page$header[["1"]]$right, "DRAFT")   # untouched
  o <- tfl_read_outputs(d)[["T-14-1-1"]]
  expect_identical(o$page$titles[["2"]]$center, "Demographics (edited)")
  expect_identical(o$page$footnotes[["2"]]$left, "New note.")
  expect_identical(o$ard$analyses[[3L]]$variables, c("AGEGR1", "SEX"))
  expect_identical(o$codelists$TRT01A[[2L]]$label, "Ctrl")
  # what the workbook cannot hold is as it was
  expect_identical(vapply(o$content, `[[`, "", "fn")[6L], "plan_cell_style")
  expect_s3_class(o$content[[6L]]$args$where, "tfl_r")
  expect_identical(o$ard$analyses[[2L]]$statistic$AGE[[2L]], "mean")
  # the same workbook again: nothing changes
  r2 <- tfl_import_spec_xlsx(d, f)
  expect_identical(nrow(r2$changes), 0L)
  expect_length(r2$added, 0L)
})
