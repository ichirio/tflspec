# The spec sheets' columns live in the schema file: every sheet described,
# every column with its help, and a column that is a function's argument
# says which, so the YAML spec's help of it is the same text.

test_that("every sheet has its own description and each column its help", {
  d <- tfl_spec_columns()
  sheets <- unique(d$sheet)
  own <- d[is.na(d$column), , drop = FALSE]
  expect_setequal(own$sheet, sheets)
  expect_false(anyNA(d$description))
  expect_true(all(nzchar(d$description)))
  expect_identical(nrow(tfl_spec_columns("page")), sum(d$sheet == "page"))
})

test_that("a column that is an argument points at a function and an argument the schema has", {
  for (sh in tflspec:::.schema_sheets()) {
    for (cl in sh$columns) {
      if (is.null(cl$at)) next
      fn <- sub("[.].*$", "", cl$at)
      arg <- sub("^[^.]*[.]", "", cl$at)
      ent <- tfl_schema(fn)
      expect_false(is.null(ent), info = cl$at)
      expect_true(arg %in% names(ent$args), info = cl$at)
      expect_identical(tfl_schema_help(fn, arg), ent$args[[arg]]$description %||% cl$description,
                       info = cl$at)
    }
  }
  expect_identical(tfl_schema_help("rtf_page", "orientation"),
                   tfl_spec_columns("page")$description[tfl_spec_columns("page")$column %in% "orientation"])
  expect_identical(tfl_schema_help("rtf_page", "no_such_argument"), NA_character_)
})
