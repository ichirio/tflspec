# Page patterns (tflplanner's page-patterns design): a named set of the
# report half's default rows ("@<name>"), inheriting from the blank rows
# (Standard); a report chooses one with report$pattern; its own rows go
# over its pattern's.

pattern_spec <- function() {
  list(
    report = data.frame(output_id = c(NA, "T1", "T2", "@Compact"), type = "table",
                        pattern = c(NA, "Compact", NA, NA)),
    page = data.frame(output_id = c(NA, "@Compact", "T1"),
                      paper_size = c("letter", "a4", NA),
                      orientation = c("landscape", NA, NA),
                      font_size_half_points = c("18", "16", NA),
                      margin_top_in = c("1", "0.5", "0.75")),
    header = data.frame(output_id = c(NA, NA, "@Compact"), line = c("1", "2", "2"),
                        left = c("ACME", "{STUDY}", "{STUDY} (PK)")),
    footnotes = data.frame(output_id = c(NA, "@Compact", "T1"), line = c("99", "98", "98"),
                           left = c("Program: {PROGRAM}", "PK footnote", "(none)")),
    tokens = data.frame(output_id = c(NA, "@Compact"), name = c("STUDY", "STUDY"),
                        value = c("ABC-101", "ABC-101 PK")))
}

test_that("a report's own rows, then its pattern's, then Standard's", {
  s <- tfl_table_spec(pattern_spec())
  x <- .ard_spec_scope(s, "T1")
  expect_identical(attr(x, "pattern"), "Compact")
  # cell by cell: the report's margin, the pattern's paper and size, Standard's orientation
  expect_identical(x$page$paper_size, "a4")
  expect_identical(x$page$orientation, "landscape")
  expect_identical(x$page$font_size_half_points, "16")
  expect_identical(x$page$margin_top_in, "0.75")
  # by line: the pattern's line 2 over Standard's
  expect_identical(x$header$left[order(as.integer(x$header$line))], c("ACME", "{STUDY} (PK)"))
  # by name
  expect_identical(x$tokens$value, "ABC-101 PK")
  # the pattern's header differs: written in the report's program
  expect_true(attr(x, "bands_own")[["header"]])
  expect_identical(attr(x, "page_study"), character())
})

test_that("(none) works at each level: a report takes its pattern's line out", {
  s <- tfl_table_spec(pattern_spec())
  x <- .ard_spec_scope(s, "T1")
  code <- tfl_report_code(s, "T1")
  expect_true(any(grepl("Program: {PROGRAM}", code, fixed = TRUE)))
  expect_false(any(grepl("PK footnote", code, fixed = TRUE)))
  # a pattern takes Standard's line out
  sp <- pattern_spec()
  sp$footnotes <- rbind(sp$footnotes, data.frame(output_id = "@Compact", line = "99", left = "(none)"))
  sp$footnotes <- sp$footnotes[sp$footnotes$output_id %in% c(NA, "@Compact"), ]
  code <- tfl_report_code(tfl_table_spec(sp), "T1")
  expect_false(any(grepl("Program: {PROGRAM}", code, fixed = TRUE)))
  expect_true(any(grepl("PK footnote", code, fixed = TRUE)))
})

test_that("a report with no pattern is as before: Standard, its study parts left to the setup", {
  s <- tfl_table_spec(pattern_spec())
  x <- .ard_spec_scope(s, "T2")
  expect_true(is.na(attr(x, "pattern")))
  expect_identical(x$page$paper_size, "letter")
  expect_false(attr(x, "bands_own")[["header"]])
  expect_identical(attr(x, "page_study"), "font_size_half_points")
  # the same code as a workbook without the pattern's rows and column
  sp <- pattern_spec()
  for (sh in names(sp)) sp[[sh]] <- sp[[sh]][!sp[[sh]]$output_id %in% c("@Compact", "T1"), , drop = FALSE]
  sp$report$pattern <- NULL
  expect_identical(tfl_report_code(tfl_table_spec(pattern_spec()), "T2"),
                   tfl_report_code(tfl_table_spec(sp), "T2"))
  # the study's setup is Standard's only
  expect_false(any(grepl("PK", tfl_report_setup_code(s))))
})

test_that("the patterns are not reports", {
  s <- tfl_table_spec(pattern_spec())
  expect_output(print(s), "for 2 reports: T1, T2", fixed = TRUE)
})

test_that("a pattern's problems: unknown, misnamed, on the wrong sheet or row", {
  sp <- pattern_spec()
  sp$report$pattern[3] <- "Wide"
  expect_error(tfl_table_spec(sp), "no page pattern of that name.*The patterns: Compact")
  sp <- pattern_spec()
  sp$report$pattern[1] <- "Compact"
  expect_error(tfl_table_spec(sp), "on the default row")
  sp <- pattern_spec()
  sp$report$pattern[4] <- "Compact"
  expect_error(tfl_table_spec(sp), "chooses no other")
  sp <- pattern_spec()
  sp$page$output_id[2] <- "@2col"
  sp$report$output_id[4] <- "@2col"
  sp$header$output_id[3] <- "@2col"
  sp$footnotes$output_id[2] <- "@2col"
  sp$tokens$output_id[2] <- "@2col"
  sp$report$pattern[2] <- "2col"
  expect_error(tfl_table_spec(sp), "a letter, then letters, digits or _")
  sp <- pattern_spec()
  sp$tables <- data.frame(output_id = "@Compact", cols = "TRT01A", rows = "group = variable")
  expect_error(tfl_table_spec(sp), "a pattern has rows of report, page")
})
