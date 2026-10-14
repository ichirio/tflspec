# The flat view of the YAML spec as a workbook

test_that("the rows of a YAML spec go to a workbook; the rest is listed", {
  skip_if_not_installed("openxlsx")
  d <- system.file("extdata", "spec-yaml", package = "tflspec")
  f <- withr::local_tempfile(fileext = ".xlsx")
  r <- tfl_write_spec_xlsx(d, f)
  expect_true(file.exists(f))
  expect_true(all(c("analysis_data", "analyses", "codelists", "tokens", "header", "footer",
                    "titles", "footnotes", "_not_in_xlsx") %in% r$sheets))
  rd <- function(s) as.data.frame(readxl::read_excel(f, s, col_types = "text"))
  an <- rd("analyses")
  expect_identical(an$output_id, rep("T-14-1-1", 3L))
  expect_identical(an$id, c("GROUPN", "CONT", "CAT"))
  expect_identical(an$variables[3L], "AGEGR1 | SEX | RACE | ETHNIC")
  # the defaults' header lines keyed blank, the report's titles by its id
  h <- rd("header")
  expect_true(all(is.na(h$output_id)))
  expect_identical(h$line, c("1", "2", "4", "5", "6"))
  expect_identical(h$right[1L], "DRAFT")
  ti <- rd("titles")
  expect_identical(ti$output_id, c("T-14-1-1", "T-14-1-1"))
  tk <- rd("tokens")
  expect_true("COMPANY" %in% tk$name[is.na(tk$output_id)])
  expect_true("OUTPUT_POPULATION" %in% tk$name[tk$output_id %in% "T-14-1-1"])
  cl <- rd("codelists")
  expect_identical(cl$variable, rep("TRT01A", 3L))
  # the maps that are not one cell, the layers, the calls: listed
  nx <- r$not_in_xlsx
  expect_true(any(nx$output %in% "T-14-1-1" & nx$part == "content"))
  expect_true(any(is.na(nx$output) & nx$part == "content"))
  expect_true(any(nx$part == "analyses.statistic"))
  # rtf_tables(auto_section) is a column of the report sheet now
  rp <- rd("report")
  expect_identical(rp$auto_section[is.na(rp$output_id)], "TRUE")
})
