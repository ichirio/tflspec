ex <- function(f) system.file("extdata", "ard-spec", f, package = "tflspec")

test_that("a table spec is written with its sheets only, a report spec with its", {
  skip_if_not_installed("readxl")
  sp <- tfl_read_report_spec(ex(c("report.xlsx", "study.xlsx")))
  tf <- withr::local_tempfile(fileext = ".xlsx")
  rf <- withr::local_tempfile(fileext = ".xlsx")
  tfl_write_table_spec(sp, tf)
  tfl_write_report_spec(sp, rf)
  # both halves hold rows here: each writer still writes its own half (and
  # the other's sheets only when they hold rows -- so nothing is dropped)
  expect_identical(readxl::excel_sheets(tf)[1:10],
                   c("study", "tables", "variables", "codelists", "cells", "layout",
                     "columns", "style", "cell_styles", "col_header"))
  expect_identical(readxl::excel_sheets(rf)[1:7],
                   c("study", "report", "page", "header", "footer",
                     "titles", "footnotes"))
  # a table spec alone: no report sheet at all
  t_only <- tfl_read_table_spec(ex("DM.xlsx"))
  tfl_write_table_spec(t_only, tf)
  expect_identical(readxl::excel_sheets(tf),
                   c("study", "tables", "variables", "codelists", "cells", "layout",
                     "columns", "style", "cell_styles", "col_header",
                     "about"))
  expect_false(any(startsWith(readxl::excel_sheets(tf), "_")))
  # its study sheet shows the table's key, not the report's
  st <- as.data.frame(readxl::read_excel(tf, "study", col_types = "text"))
  expect_identical(st$key, "rounding")
  expect_equal(unclass(tfl_read_table_spec(tf))[-1L], unclass(t_only)[-1L])
})

test_that("the report writer keeps the report keys and the report sheets", {
  skip_if_not_installed("readxl")
  r <- tfl_read_report_spec(ex("report.xlsx"))
  f <- withr::local_tempfile(fileext = ".xlsx")
  tfl_write_report_spec(r, f)
  expect_identical(readxl::excel_sheets(f),
                   c("study", "report", "page", "header", "footer", "titles",
                     "footnotes", "tokens", "about"))
  back <- tfl_read_report_spec(f)
  expect_identical(back$header, r$header)
  expect_identical(back$study$key, c("output_path", "program_dir"))
})

test_that("an ARD spec is its four sheets; the catalogs only on request", {
  skip_if_not_installed("readxl")
  a <- tfl_ard_spec_template()
  f <- withr::local_tempfile(fileext = ".xlsx")
  tfl_write_ard_spec(a, f)
  expect_identical(readxl::excel_sheets(f),
                   c("study", "datasets", "populations", "analyses"))
  tfl_write_ard_spec(a, f, catalogs = TRUE)
  expect_identical(readxl::excel_sheets(f),
                   c("study", "datasets", "populations", "analyses",
                     "_methods", "_statistics"))
})

test_that("the column help is a comment on each header cell", {
  skip_if_not_installed("readxl")
  f <- withr::local_tempfile(fileext = ".xlsx")
  tfl_write_table_spec(tfl_read_table_spec(ex("DM.xlsx")), f)
  wb <- openxlsx::loadWorkbook(f)
  cm <- wb$comments[[match("tables", names(wb))]]
  expect_gt(length(cm), 0L)
  help <- tfl_spec_columns("tables")
  expect_true(all(c("sheet", "column", "description", "example") %in% names(help)))
  expect_gt(nrow(tfl_spec_columns("analyses")), 0L)
  expect_true(all(c("tables", "variables", "codelists", "cells") %in%
                    unlist(strsplit(tfl_spec_columns()$sheet, " / "))))
})

test_that("several specs share one workbook, and each reader takes its own", {
  skip_if_not_installed("readxl")
  sp <- tfl_read_report_spec(ex(c("report.xlsx", "study.xlsx")))
  a <- tfl_ard_spec_template()
  a$study$value[a$study$key == "id"] <- "USUBJID"
  l <- tfl_listing_spec(list(
    listings = data.frame(output_id = "L1", type = "multiline",
                          dataset = "ADAE"),
    listing_cols = data.frame(output_id = "L1", vars = "USUBJID")),
    check = FALSE)
  f <- withr::local_tempfile(fileext = ".xlsx")
  tfl_write_specs(f, ard = a, table = sp, report = sp, listing = l)
  sh <- readxl::excel_sheets(f)
  expect_identical(sh[1L], "study")
  expect_identical(sh[length(sh)], "about")
  expect_true(all(c("datasets", "tables", "report", "listings") %in% sh))
  # one study sheet with every kind's keys
  st <- as.data.frame(readxl::read_excel(f, "study", col_types = "text"))
  expect_true(all(c("id", "output", "rounding", "output_path",
                    "program_dir") %in% st$key))
  # each reader takes its own sheets back, passing over the others'
  t2 <- tfl_read_report_spec(f)
  expect_identical(t2$tables, sp$tables)
  expect_identical(t2$header, sp$header)
  a2 <- tfl_read_ard_spec(f, check = FALSE)
  expect_identical(a2$study$value[a2$study$key == "id"], "USUBJID")
  expect_identical(tfl_read_listing_spec(f, check = FALSE)$listings$output_id,
                   "L1")
})

test_that("a line break in a cell reads back the same after any number of writes", {
  skip_if_not_installed("readxl")
  sp <- tfl_read_table_spec(ex("DM.xlsx"))
  sp$col_header$text[1] <- "Treatment\n(N={n})"
  f <- withr::local_tempfile(fileext = ".xlsx")
  tfl_write_table_spec(sp, f)
  once <- tfl_read_table_spec(f)
  expect_identical(once$col_header$text[1], "Treatment\n(N={n})")
  tfl_write_table_spec(once, f)
  expect_identical(tfl_read_table_spec(f)$col_header, once$col_header)
})

test_that("every column of every sheet is described, in one language", {
  d <- tfl_spec_columns()
  have <- paste(d$sheet, d$column)
  sheets <- c(.ard_spec_schema(), .ard_spec_sheets, .listing_sheets,
              list(about = c("key", "value")))
  for (sh in names(sheets)) {
    for (cn in setdiff(sheets[[sh]], "output_id")) {
      expect_true(paste(sh, cn) %in% have, label = paste(sh, cn))
    }
    expect_true(paste(sh, NA) %in% have, label = paste(sh, "(the sheet)"))
  }
  expect_true(any(d$sheet == "(workbook)" & d$column %in% "output_id"))
  # English: no CJK text
  expect_false(any(grepl("[\u3040-\u30ff\u4e00-\u9fff]",
                         c(d$description, d$example), perl = TRUE)))
  # a header cell's comment: its sheet's own text, or the workbook's
  h <- .spec_column_help("layout")
  expect_match(h[["pages_break_before"]], "break_before")
  expect_match(h[["output_id"]], "report a row belongs to")
})
