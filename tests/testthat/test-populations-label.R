# populations$label: the text a header prints, and the analysis set's name in
# CDISC ARS (blank: the id)

pl_df <- function(...) {
  rows <- list(...)
  cols <- unique(unlist(lapply(rows, names)))
  as.data.frame(lapply(stats::setNames(cols, cols), function(cn)
    vapply(rows, function(r) as.character(r[[cn]] %||% NA), "")),
    stringsAsFactors = FALSE)
}

pl_spec <- function(label = NA) {
  s <- list(
    study = pl_df(list(key = "id", value = "USUBJID"),
                  list(key = "study_id", value = "PILOT01")),
    datasets = pl_df(list(dataset = "ADSL", path = "adam/ADSL.rds")),
    populations = pl_df(list(population_id = "SAF", dataset = "ADSL",
                             where = "SAFFL == \"Y\"", label = label)),
    analyses = pl_df(list(output_id = "DM", analysis_id = "GROUPN",
                          method = "categorical", population_id = "SAF",
                          variables = "TRT01A")))
  for (n in names(.ard_spec_sheets)) {
    for (c in .ard_spec_sheets[[n]]) {
      if (!c %in% names(s[[n]])) s[[n]][[c]] <- NA_character_
    }
    s[[n]] <- s[[n]][.ard_spec_sheets[[n]]]
  }
  tfl_ard_spec(s)
}

pl_json <- function(ars) {
  f <- withr::local_tempfile(fileext = ".json", .local_envir = parent.frame())
  tfl_write_ars_json(ars, f)
  tfl_read_ars_json(f)
}

test_that("a population's label is a column of the sheet, read and written", {
  expect_identical(.ard_spec_sheets$populations[5L], "label")
  sp <- pl_spec("Safety Analysis Set")
  f <- withr::local_tempfile(fileext = ".xlsx")
  tfl_write_ard_spec(sp, f)
  back <- tfl_read_ard_spec(f)
  expect_identical(back$populations$label, "Safety Analysis Set")
  # a workbook of an earlier version (no label column) reads, label blank
  old <- as.data.frame(sp$populations)
  old$label <- NULL
  g <- withr::local_tempfile(fileext = ".xlsx")
  wb <- openxlsx::createWorkbook()
  for (n in names(sp)) {
    openxlsx::addWorksheet(wb, n)
    openxlsx::writeData(wb, n, if (n == "populations") old else as.data.frame(sp[[n]]))
  }
  openxlsx::saveWorkbook(wb, g)
  expect_true(is.na(tfl_read_ard_spec(g)$populations$label))
  expect_true("label" %in% tfl_spec_columns("populations")$column)
})

test_that("the label is the analysis set's name in ARS, and comes back", {
  ars <- suppressWarnings(tfl_ars(pl_spec("Safety Analysis Set")))
  expect_identical(ars$analysisSets[[1L]]$id, "SAF")
  expect_identical(ars$analysisSets[[1L]]$name, "Safety Analysis Set")
  back <- tfl_ars_to_specs(pl_json(ars))
  expect_identical(back$ard$populations$label, "Safety Analysis Set")
  # no label: the name is the id, and the label stays blank on the way back
  ars0 <- suppressWarnings(tfl_ars(pl_spec(NA)))
  expect_identical(ars0$analysisSets[[1L]]$name, "SAF")
  expect_true(is.na(tfl_ars_to_specs(pl_json(ars0))$ard$populations$label))
})
