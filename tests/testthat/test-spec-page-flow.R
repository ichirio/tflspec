# A report on the page of the content before it: rtfreporter's
# rtf_tables(page_break = FALSE, space_before, first_rows, cont_title) and
# rtf_figures(page_break = FALSE, space_before), said by the report's page
# (YAML) and by the report sheet alike (#443).

pf_code <- function(page, kind = "table") {
  paste(tfl_report_code(tfl_output("T-1", kind, page = page), content = "plan"), collapse = "\n")
}

test_that("page.rtf_tables's flow arguments are written into rtf_tables()", {
  code <- pf_code(list(rtf_tables = list(page_break = FALSE, space_before = 2L,
                                         first_rows = 10L, cont_title = "(continued)")))
  expect_match(code, "page_break = FALSE", fixed = TRUE)
  expect_match(code, "space_before = 2", fixed = TRUE)
  expect_match(code, "first_rows = 10", fixed = TRUE)
  expect_match(code, "cont_title = \"(continued)\"", fixed = TRUE)
})

test_that("a new page, the default, says nothing; space_before only on the same page", {
  plain <- pf_code(list())
  code <- pf_code(list(rtf_tables = list(page_break = TRUE, space_before = 2L)))
  expect_identical(code, plain)
  expect_false(grepl("page_break", code, fixed = TRUE))
  expect_false(grepl("space_before", pf_code(list(rtf_tables = list(page_break = FALSE,
                                                                     space_before = 0L))),
                     fixed = TRUE))
})

test_that("a figure takes page_break and space_before, from rtf_figures or the sheet's rtf_tables", {
  code <- pf_code(list(rtf_figures = list(page_break = FALSE, space_before = 1L)), "figure")
  expect_match(code, "rtf_figures\\([^)]*page_break = FALSE")
  expect_match(code, "space_before = 1", fixed = TRUE)
  code2 <- pf_code(list(rtf_tables = list(page_break = FALSE, space_before = 1L)), "figure")
  expect_identical(code2, code)
})

test_that("the report sheet says the same as page: (YAML and sheets write one program)", {
  sheets <- tfl_table_spec(
    report = data.frame(output_id = "T-1", type = "table", page_break = "FALSE",
                        space_before = "1", first_rows = "12", cont_title = "(continued)"))
  yaml <- tfl_output("T-1", "table", page = list(
    rtf_tables = list(page_break = FALSE, space_before = 1L, first_rows = 12L,
                      cont_title = "(continued)")))
  # (from the content's call on: the document's file is the sheets' own default)
  expect_identical(tfl_report_code(yaml, content = "plan")[-1L],
                   tfl_report_code(sheets, "T-1", content = "plan")[-1L])
})

test_that("the flow arguments check against the schema and are rtfreporter's", {
  p <- tfl_schema_check(tfl_item("rtf_tables", list(page_break = FALSE, space_before = 1L,
                                                    first_rows = 5L, cont_title = "x")))
  expect_identical(nrow(p), 0L)
  p <- tfl_schema_check(tfl_item("rtf_figures", list(page_break = FALSE, space_before = 1L)))
  expect_identical(nrow(p), 0L)
})

test_that("the program puts the report under the content before it", {
  skip_if_not_installed("rtfreporter")
  content <- rtfreporter::as_rtftables(data.frame(b = "3"))
  o <- tfl_output("T-1", "table", page = list(rtf_tables = list(page_break = FALSE)))
  code <- paste(tfl_report_code(o, content = "content"), collapse = "\n")
  # (the document already holding a table: the report's call goes on under it)
  code <- sub("rtf_document\\([^)]*\\)", "rtfreporter::rtf_tables(rtfreporter::rtf_document(), first)",
              code, perl = TRUE)
  first <- rtfreporter::as_rtftables(data.frame(a = c("1", "2")))
  env <- list2env(list(first = first, content = content), parent = asNamespace("rtfreporter"))
  doc <- eval(parse(text = code), env)
  pb <- vapply(doc$placement, function(p) isTRUE(p$page_break), logical(1L))
  expect_identical(pb, c(TRUE, FALSE))
})

test_that(".page_flow_args() keeps a content's place only off a new page", {
  f <- tflspec:::.page_flow_args
  expect_identical(f(list(page_break = TRUE, space_before = 2L, auto_title = TRUE)), setNames(list(), character()))
  expect_identical(f(list(page_break = FALSE, space_before = 0L, first_rows = 3L)),
                   list(page_break = FALSE, first_rows = 3L))
  expect_length(f(list()), 0L)
})
