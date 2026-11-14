# A title or footnote block's border and the header rows a cell style
# chooses: rtfreporter's rtf_titles(border = ), plan_titles(border = ),
# plan_footnotes(border = ) and plan_cell_style(row = ), written by a SPEC.

bb_page <- function(style = list()) {
  c(list(titles = list(`1` = list(text = "Table 14.1.1")),
         footnotes = list(`1` = list(left = "Note."))), style)
}

test_that("the schema takes the block borders and the header rows", {
  b <- tfl_call("rtf_border", list(bottom = TRUE))
  expect_identical(nrow(tfl_schema_check(tfl_item("rtf_titles", list(border = b)))), 0L)
  expect_identical(nrow(tfl_schema_check(tfl_item("plan_titles", list(
    titles = list("Table 14.1.1"), border = b)))), 0L)
  expect_identical(nrow(tfl_schema_check(tfl_item("plan_footnotes", list(
    footnotes = list("Note."), border = tfl_call("rtf_border", list(top = TRUE)))))), 0L)
  expect_identical(nrow(tfl_schema_check(tfl_item("plan_cell_style", list(
    header = TRUE, row = list(1L), bold = TRUE)))), 0L)
  # a wrong value is said
  expect_gt(nrow(tfl_schema_check(tfl_item("plan_titles", list(
    titles = list("A"), border = "under")))), 0L)
  expect_gt(nrow(tfl_schema_check(tfl_item("plan_cell_style", list(
    header = TRUE, row = "top")))), 0L)
  skip_if_not_installed("rtfreporter")
  for (fn in c("rtf_titles", "plan_titles", "plan_footnotes", "plan_cell_style")) {
    f <- names(formals(getExportedValue("rtfreporter", fn)))
    skip_if_not(all(c("border", "row")[c(TRUE, fn == "plan_cell_style")] %in% f),
                "rtfreporter without the block borders (rtfreporter#954, #955)")
    expect_true(all(names(tfl_schema(fn)$args) %in% f), info = fn)
  }
})

test_that("page.rtf_titles.border is rtf_titles(border = ) in the report's call", {
  o <- tfl_output("T-1", "table", page = bb_page(list(
    rtf_style = list(title_format = "table"),
    rtf_titles = list(border = tfl_call("rtf_border", list(bottom = TRUE))))))
  code <- paste(tfl_report_code(o, content = "content"), collapse = "\n")
  expect_match(code, "rtf_titles\\([^|]*border = rtf_border\\(bottom = TRUE\\)")
  plain <- paste(tfl_report_code(tfl_output("T-1", "table", page = bb_page()),
                                 content = "content"), collapse = "\n")
  expect_false(grepl("border", plain, fixed = TRUE))
})

test_that("plan_titles / plan_footnotes(border =) and plan_cell_style(row =) are written", {
  out <- tfl_output("T-1", "table", content = list(
    tfl_item("table_plan", list(cols = "ARM", rows = list(group = "variable"))),
    tfl_item("plan_cell_style", list(header = TRUE, row = list(1L), bold = TRUE)),
    tfl_item("plan_titles", list(titles = list("Table 14.1.1"),
                                 border = tfl_call("rtf_border", list(bottom = TRUE)))),
    tfl_item("plan_footnotes", list(footnotes = list("Note."),
                                    border = tfl_call("rtf_border", list(top = TRUE))))))
  code <- paste(tfl_content_code(out, pipe = "|>"), collapse = "\n")
  expect_match(code, "plan_cell_style\\(header = TRUE, row = 1L?, bold = TRUE\\)")
  expect_match(code, "plan_titles\\(\"Table 14.1.1\", border = rtf_border\\(bottom = TRUE\\)\\)")
  expect_match(code, "plan_footnotes\\(\"Note.\", border = rtf_border\\(top = TRUE\\)\\)")
  skip_if_not_installed("cards")
  skip_if_not("border" %in% names(formals(rtfreporter::plan_titles)),
              "rtfreporter without plan_titles(border = ) (rtfreporter#955)")
  ard <- cards::ard_stack(cards::ADSL, .by = ARM, cards::ard_tabulate(variables = SEX))
  env <- new.env()
  env$data <- rtfreporter::normalize_ard(ard)
  eval(parse(text = c("library(rtfreporter)", code)), env)
  expect_s3_class(env$plan, "table_plan")
})
