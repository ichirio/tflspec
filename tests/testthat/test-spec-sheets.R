# The YAML spec back as sheets (the adapter): a workbook converted to YAML
# and read back as sheets makes the same table code

test_that("a workbook -> YAML -> sheets makes the same table code, and the page parts come back", {
  skip_if_not_installed("openxlsx")
  skip_if_not(exists(".plan_sheets_from_content", envir = asNamespace("tflspec")), "no bridge")
  books <- system.file("extdata", "ard-spec", c("AE.xlsx", "DM.xlsx", "LB.xlsx"), package = "tflspec")
  books <- books[nzchar(books)]
  skip_if(length(books) == 0L)
  for (b in books) {
    from <- withr::local_tempdir()
    file.copy(b, file.path(from, "table_spec.xlsx"))
    to <- withr::local_tempdir()
    r <- tfl_convert_sheets_to_yaml(from, to, overwrite = TRUE)
    expect_false(any(grepl("bridge", r$not_converted$reason)), info = basename(b))
    back <- tfl_sheets_from_yaml(to)
    sp0 <- tfl_read_table_spec(file.path(from, "table_spec.xlsx"))
    sp1 <- tfl_table_spec(back$table[intersect(names(back$table), c("study", names(.ard_spec_schema())))])
    ids <- unique(sp0$tables$output_id[!is.na(sp0$tables$output_id)])
    for (id in ids) {
      c0 <- tryCatch(tfl_table_code(sp0, id), error = function(e) conditionMessage(e))
      c1 <- tryCatch(tfl_table_code(sp1, id), error = function(e) conditionMessage(e))
      expect_identical(c1, c0, info = paste(basename(b), id))
    }
  }
})

test_that("the sheets of the fixture study come back: the report list, the page parts, the ARD", {
  d <- system.file("extdata", "spec-yaml", package = "tflspec")
  sh <- tfl_sheets_from_yaml(d)
  expect_identical(sh$planner$output_id, c(NA, "T-14-1-1"))
  expect_identical(sh$planner$population[2L], "SAF")
  rp <- sh$table$report
  expect_identical(rp$type[rp$output_id %in% "T-14-1-1"], "table")
  # the defaults' rtf_tables(auto_section = TRUE) as the report sheet's study row
  expect_identical(rp$auto_section[is.na(rp$output_id)], "TRUE")
  h <- sh$table$header
  expect_true(all(is.na(h$output_id)))
  expect_identical(h$left[h$line == "2"], "Protocol: {STUDY_ID}")
  expect_identical(sh$table$tokens$value[sh$table$tokens$name == "COMPANY"], "Sample Pharma")
  expect_identical(sh$table$titles$center, c("Table 14.1.1", "Demographic Characteristics"))
  expect_identical(sh$ard$analyses$analysis_id, c("GROUPN", "CONT", "CAT"))
  expect_identical(sh$ard$analyses$variables[3L], "AGEGR1 | SEX | RACE | ECOGBL")
  expect_identical(sh$ard$analysis_data$data_id, "adsl_saf")
  expect_identical(sh$ard$analysis_data$population_id, "SAF")
  expect_identical(sh$table$codelists$label[2L], "Control 100 mg")
  # a nested statistic map is not a cell: left blank
  expect_true(is.na(sh$ard$analyses$statistic[2L]) || !"statistic" %in% names(sh$ard$analyses))
})
