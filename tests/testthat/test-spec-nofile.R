# A study has no spec/codelists.yml: a code list is a report's or a layer's.
# A file still there is not read (one message); no sheet, workbook or YAML
# file has a code list row with no report.  Package sample study.

nf_study <- function(file = TRUE, env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  src <- system.file("extdata", "spec-yaml", package = "tflspec")
  file.copy(list.files(src, full.names = TRUE), d, recursive = TRUE)
  if (file) {
    writeLines("RACE: [{value: WHITE, label: White}, {value: ASIAN, label: Asian}]",
               file.path(d, "codelists.yml"))
  }
  d
}

# a code list row with no report, as a hand-edited sheet or workbook has it
nf_bad_row <- function() {
  data.frame(output_id = NA_character_, variable = "RACE", value = "WHITE", label = "White",
             order = "1", under = NA_character_, stringsAsFactors = FALSE)
}

# the sheet's rows (all text) and one more, with no report
nf_add_bad <- function(cl) {
  new <- cl[1L, , drop = FALSE]
  new[] <- NA_character_
  new$variable <- "RACE"
  new$value <- "WHITE"
  new$label <- "White"
  rbind(cl, new)
}

test_that("a study with spec/codelists.yml is read: the file is not, and says so once", {
  d <- nf_study()
  msgs <- character()
  s <- withCallingHandlers(tfl_read_study_spec(d), message = function(m) {
    msgs <<- c(msgs, conditionMessage(m))
    invokeRestart("muffleMessage")
  })
  expect_length(grep("spec/codelists.yml is not read", msgs, fixed = TRUE), 1L)
  expect_match(msgs, "a code list is a report's or a layer's", fixed = TRUE)
  expect_match(msgs, "`codelists:` in spec/defaults.yml or the report's file", fixed = TRUE)
  expect_null(s$codelists)
  expect_false("codelists" %in% names(s))
  # no file, no message
  expect_silent(tfl_read_study_spec(nf_study(file = FALSE)))
})

test_that("the sheets of a study with the file have no row with no report; the round trip works", {
  skip_if_not_installed("openxlsx")
  d <- nf_study()
  sh <- suppressMessages(tfl_sheets_from_yaml(d))
  cl <- sh$table$codelists
  # the report's own two rows (TRT01A in T-14-1-1), none from the file
  expect_identical(nrow(cl), 2L)
  expect_false(anyNA(cl$output_id))
  expect_false("RACE" %in% cl$variable)
  expect_s3_class(tfl_table_spec(sh$table), "tfl_table_spec")
  f <- withr::local_tempfile(fileext = ".xlsx")
  expect_no_error(suppressMessages(tfl_write_specs(f, table = sh$table)))
  to <- withr::local_tempdir()
  expect_no_error(suppressMessages(tfl_yaml_from_sheets(sh, to)))
  expect_length(list.files(to, "^codelists[.]ya?ml$", recursive = TRUE), 0L)
  # the workbook of the flat view has no such row either
  w <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(tfl_write_spec_xlsx(d, w))
  wc <- as.data.frame(readxl::read_excel(w, "codelists"))
  expect_false(anyNA(wc$output_id))
  expect_identical(wc$variable, rep("TRT01A", 2L))
})

test_that("a workbook's code list row with no report is refused by the table spec, the converter and the import", {
  skip_if_not_installed("openxlsx")
  sh <- suppressMessages(tfl_sheets_from_yaml(nf_study(file = FALSE)))
  bad <- sh$table
  expect_identical(nrow(bad$codelists), 2L)
  bad$codelists <- rbind(bad$codelists, nf_bad_row())
  msg <- "Row 3 of the code lists (RACE / WHITE) has no `output_id`"
  expect_error(tfl_table_spec(bad), msg, fixed = TRUE)

  # tfl_convert_sheets_to_yaml(): a table workbook with the row
  from <- withr::local_tempdir()
  dir.create(file.path(from, "spec"))
  tb <- list(tables = data.frame(output_id = "T-1", cols = "ARM", stringsAsFactors = FALSE),
             codelists = data.frame(output_id = "T-1", variable = "SEX", value = "F", label = "Female",
                                    order = "1", stringsAsFactors = FALSE))
  book <- file.path(from, "spec", "table_spec.xlsx")
  tfl_write_table_spec(tfl_table_spec(tb), book)
  wb <- openxlsx::loadWorkbook(book)
  cl <- as.data.frame(openxlsx::read.xlsx(wb, "codelists"))
  cl[] <- lapply(cl, as.character)
  cl <- nf_add_bad(cl)
  openxlsx::removeWorksheet(wb, "codelists")
  openxlsx::addWorksheet(wb, "codelists")
  openxlsx::writeData(wb, "codelists", cl, keepNA = FALSE)
  openxlsx::saveWorkbook(wb, book, overwrite = TRUE)
  to <- withr::local_tempdir()
  expect_error(suppressMessages(tfl_convert_sheets_to_yaml(from, to, overwrite = TRUE)),
               "Row 2 of the code lists (RACE / WHITE) has no `output_id`", fixed = TRUE)

  # tfl_import_spec_xlsx(): nothing of the study is written
  d <- nf_study(file = FALSE)
  f <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(tfl_write_spec_xlsx(d, f))
  wb <- openxlsx::loadWorkbook(f)
  cl <- as.data.frame(openxlsx::read.xlsx(wb, "codelists"))
  cl[] <- lapply(cl, as.character)
  cl <- nf_add_bad(cl)
  openxlsx::removeWorksheet(wb, "codelists")
  openxlsx::addWorksheet(wb, "codelists")
  openxlsx::writeData(wb, "codelists", cl, keepNA = FALSE)
  openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
  before <- vapply(list.files(d, recursive = TRUE, full.names = TRUE),
                   function(p) unname(tools::md5sum(p)), "")
  expect_error(tfl_import_spec_xlsx(d, f), msg, fixed = TRUE)
  after <- vapply(list.files(d, recursive = TRUE, full.names = TRUE),
                  function(p) unname(tools::md5sum(p)), "")
  expect_identical(after, before)
})

test_that("tfl_write_study_part() has no codelists part", {
  s <- suppressMessages(tfl_read_study_spec(nf_study()))
  expect_error(tfl_write_study_part(s, "codelists", withr::local_tempfile(fileext = ".yml")),
               "should be one of")
  expect_false("codelists" %in% eval(formals(tfl_write_study_part)$part))
})
