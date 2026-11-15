# The flat view of the YAML spec as a workbook

test_that("the rows of a YAML spec go to a workbook; the rest is listed", {
  skip_if_not_installed("openxlsx")
  d <- system.file("extdata", "spec-yaml", package = "tflspec")
  f <- withr::local_tempfile(fileext = ".xlsx")
  r <- tfl_write_spec_xlsx(d, f)
  expect_true(file.exists(f))
  expect_true(all(c("analysis_data", "analyses", "codelists", "tokens",
                    "titles", "footnotes", "_not_in_xlsx") %in% r$sheets))
  rd <- function(s) as.data.frame(readxl::read_excel(f, s, col_types = "text"))
  an <- rd("analyses")
  expect_identical(an$output_id, rep("T-14-1-1", 3L))
  expect_identical(an$id, c("GROUPN", "CONT", "CAT"))
  expect_identical(an$variables[3L], "AGEGR1 | SEX | RACE | ECOGBL")
  # the built-in default configuration's header is no row of the study; the report's titles by its id
  expect_false("header" %in% r$sheets)
  ti <- rd("titles")
  expect_identical(ti$output_id, c("T-14-1-1", "T-14-1-1"))
  tk <- rd("tokens")
  expect_true("COMPANY" %in% tk$name[is.na(tk$output_id)])
  expect_true("OUTPUT_POPULATION" %in% tk$name[tk$output_id %in% "T-14-1-1"])
  cl <- rd("codelists")
  expect_identical(cl$variable, rep("TRT01A", 2L))
  # the maps that are not one cell, the layers, the calls: listed
  nx <- r$not_in_xlsx
  expect_true(any(nx$output %in% "T-14-1-1" & nx$part == "content"))
  expect_true(any(is.na(nx$output) & nx$part == "content"))
  expect_true(any(nx$part == "analyses.statistic"))
  # (the built-in configuration's rtf_tables(auto_section) is no row of the study either)
  expect_false("report" %in% r$sheets)
})
