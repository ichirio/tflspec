# A report written as a template SPEC goes to the `templates` sheet by its
# template's name and arguments; it is not expanded unless asked

st_study <- function() tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))

st_demog <- function(...) {
  tfl_output("T-9", "table", label = "Demographics", common = list(population = "SAF"),
             template = list(name = "demog", args = list(
               by = "TRT01A", continuous = c("AGE", "AGEU"), categorical = c("AGEGR1", "SEX"),
               labels = c(AGE = "Age (years)", SEX = "Sex, n (%)"), ...)))
}

st_code <- function() {
  tfl_output("T-8", "table", label = "Plain", common = list(population = "SAF"),
             content = list(tfl_item("plan_paginate_rows", list(max_rows = 24L))))
}

st_xlsx <- function(sh) {
  f <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(list(templates = sh$templates, template_rows = sh$template_rows), f)
  f
}

test_that("a template report is a row of the templates sheet, read back as the same template and args", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("readxl")
  s <- st_study()
  o <- st_demog()
  sh <- suppressMessages(tfl_sheets_from_spec(s, list(`T-9` = o, `T-8` = st_code())))
  expect_identical(sh$templates$output_id, "T-9")
  expect_identical(sh$templates$template, "demog")
  expect_identical(sh$templates$population, "SAF")
  # nothing of it expanded: no ARD analyses, no content rows
  expect_false("T-9" %in% sh$ard$analyses$output_id)
  expect_false("T-9" %in% sh$table$tables$output_id)
  # the report list and row are there as for any report
  expect_true(all(c("T-9", "T-8") %in% sh$planner$output_id))
  # (only the sample's style sets, which no sheet holds, are said)
  expect_equal(nrow(sh$not_converted[sh$not_converted$part != "styles", , drop = FALSE]), 0L)
  f <- st_xlsx(sh)
  back <- tfl_read_template_sheets(f)
  expect_identical(names(back), "T-9")
  expect_identical(back[["T-9"]]$template$name, "demog")
  expect_identical(back[["T-9"]]$label, "Demographics")
  # (demog takes the analysis set from the report's `common: population`: the
  # sheet's population column, not an argument)
  expect_identical(back[["T-9"]]$common$population, "SAF")
  a <- back[["T-9"]]$template$args
  b <- o$template$args
  expect_identical(a[sort(names(a))], b[sort(names(b))])
  # the code report is as ever: its rows, its ARD
  expect_true("T-8" %in% sh$planner$output_id)
})

test_that("the code report is the same with and without a template report beside it", {
  s <- st_study()
  one <- suppressMessages(tfl_sheets_from_spec(s, list(`T-8` = st_code())))
  two <- suppressMessages(tfl_sheets_from_spec(s, list(`T-9` = st_demog(), `T-8` = st_code())))
  for (nm in c("tables", "report")) {
    x <- two$table[[nm]]
    y <- one$table[[nm]]
    x <- x[x$output_id %in% "T-8", ]
    y <- y[y$output_id %in% "T-8", ]
    rownames(x) <- rownames(y) <- NULL
    expect_identical(x, y)
  }
  expect_equal(nrow(one$templates), 0L)
})

test_that("the sheets back to YAML: the template report keeps its template", {
  s <- st_study()
  sh <- suppressMessages(tfl_sheets_from_spec(s, list(`T-9` = st_demog(), `T-8` = st_code())))
  to <- withr::local_tempdir()
  suppressMessages(tfl_yaml_from_sheets(sh, to, overwrite = TRUE))
  b <- tfl_read_outputs(to)
  expect_identical(b[["T-9"]]$template$name, "demog")
  expect_identical(b[["T-9"]]$template$args$continuous, c("AGE", "AGEU"))
  expect_null(b[["T-8"]]$template)
})

test_that("an explicit tfl_template_to_spec() first: the sheets write code rows", {
  s <- st_study()
  o <- tfl_template_to_spec(st_demog(), s)
  expect_null(o$template)
  sh <- suppressMessages(tfl_sheets_from_spec(s, list(`T-9` = o)))
  expect_equal(nrow(sh$templates), 0L)
  expect_true("T-9" %in% sh$ard$analyses$output_id)
  expect_true("T-9" %in% sh$table$tables$output_id)
})

test_that("templates = \"expand\" is the old behaviour: the items on the table and ARD sheets", {
  s <- st_study()
  sh <- suppressMessages(tfl_sheets_from_spec(s, list(`T-9` = st_demog()), templates = "expand"))
  expect_equal(nrow(sh$templates), 0L)
  expect_true("T-9" %in% sh$ard$analyses$output_id)
})

test_that("arguments the templates sheet cannot hold are named in a warning and a note", {
  s <- st_study()
  o <- st_demog(extra = list(list(a = 1, b = 2)))
  expect_warning(sh <- tfl_sheets_from_spec(s, list(`T-9` = o)), "extra", fixed = TRUE)
  expect_true(any(sh$not_converted$part == "template" & grepl("extra", sh$not_converted$reason)))
  expect_identical(sh$templates$template, "demog")
})

test_that("a report's own content on a template report is said, not written", {
  s <- st_study()
  o <- st_demog()
  o$content <- list(tfl_item("plan_paginate_rows", list(max_rows = 24L)))
  expect_warning(sh <- tfl_sheets_from_spec(s, list(`T-9` = o)), "stay in YAML")
  expect_true(any(grepl("content", sh$not_converted$reason)))
})
