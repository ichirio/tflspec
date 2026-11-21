# A built-in style (tfl_standard, tfl_small) prints a page's name as a
# section heading (rtf_tables: {auto_section: true}).  A content that says
# where the name prints (plan_paginate_rows / plan_paginate_group
# heading_at =) keeps rtfreporter's own choice, so heading_at = "title"
# prints the heading once, as the last title row -- not again as a section
# row.  A content with no heading_at is as it was.

ho_study <- function() tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))
ho_data <- function() data.frame(grp = c("A", "A", "B", "B"), label = c("n", "Mean", "n", "Mean"),
                                 X = c("1", "2.5", "3", "4.5"), stringsAsFactors = FALSE)
ho_output <- function(at = NULL, style = NULL) {
  o <- tfl_output("T-HO", "table", extends = "none",
                  content = list(
                    tfl_item("table_plan", list()),
                    tfl_item("plan_paginate_rows", c(list(page_by = "grp", heading = "Group {value}"),
                                                     if (!is.null(at)) list(heading_at = at)))),
                  page = list(titles = list(`1` = list(center = "Table 1"),
                                            `2` = list(center = "Example Summary"))))
  o$style <- style
  o
}
ho_pages <- function(at = NULL) {
  args <- c(list(page_by = "grp", heading = "Group {value}"), if (!is.null(at)) list(heading_at = at))
  suppressMessages(rtfreporter::plan_apply(
    do.call(rtfreporter::plan_paginate_rows, c(list(rtfreporter::table_plan(ho_data())), args)),
    "pages"))
}
# the heading's count in the written RTF
ho_count <- function(ro, s, at) {
  d <- tempfile("ho")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- file.path(d, "t.rtf")
  doc <- tfl_report(ro, content = ho_pages(at), study = s)
  suppressMessages(rtfreporter::generate_rtfreport(doc, f, overwrite = TRUE))
  x <- paste(readLines(f, warn = FALSE), collapse = "\n")
  lengths(regmatches(x, gregexpr("Group A", x, fixed = TRUE)))
}

test_that("a built-in style and heading_at: no auto_section, the heading printed once", {
  s <- ho_study()
  for (st in c("tfl_standard", "tfl_small")) {
    ro <- tfl_resolve_output(ho_output("title", st), s)
    expect_null(ro$page$rtf_tables$auto_section, label = st)
    code <- paste(tfl_report_code(ro, content = "pages"), collapse = "\n")
    expect_false(grepl("auto_section", code, fixed = TRUE), label = st)
  }
  ro <- tfl_resolve_output(ho_output("title"), s)
  expect_identical(ho_count(ro, s, "title"), 1L)
  # heading_at = "section": rtfreporter's own default is the section (once)
  ro <- tfl_resolve_output(ho_output("section"), s)
  expect_null(ro$page$rtf_tables$auto_section)
  expect_identical(ho_count(ro, s, "section"), 1L)
})

test_that("no heading_at: the built-in style's auto_section as before", {
  s <- ho_study()
  for (st in c("tfl_standard", "tfl_small")) {
    ro <- tfl_resolve_output(ho_output(NULL, st), s)
    expect_identical(ro$page$rtf_tables$auto_section, TRUE, label = st)
    code <- paste(tfl_report_code(ro, content = "pages"), collapse = "\n")
    expect_match(code, "auto_section = TRUE", fixed = TRUE, label = st)
  }
})

test_that("a study's own style and the report's own page keep their auto_section", {
  s <- ho_study()
  s$styles <- list(own = list(data = list(), codelists = list(), content = list(),
                              page = list(rtf_tables = list(auto_section = TRUE))))
  ro <- tfl_resolve_output(ho_output("title", "own"), s)
  expect_identical(ro$page$rtf_tables$auto_section, TRUE)
  o <- ho_output("title")
  o$page$rtf_tables <- list(auto_section = TRUE)
  ro <- tfl_resolve_output(o, s)
  expect_identical(ro$page$rtf_tables$auto_section, TRUE)
})
