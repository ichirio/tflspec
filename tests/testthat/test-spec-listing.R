# A listing's content (plan_listing() of listing_col()s, the blank rows, the
# pages) to its code and to the plan made directly, and the same pages as
# the listing_spec() a workbook's listing makes.

lst_out <- function() {
  tfl_output("L-1", "listing", content = list(
    tfl_item("plan_listing", list(cols = list(
      tfl_call("listing_col", list(vars = "TRTA", width = 22, label = "Treatment")),
      tfl_call("listing_col", list(vars = "USUBJID", width = 14, label = "Subject",
                                    collapse_repeats = TRUE)),
      tfl_call("listing_col", list(vars = c("AEBODSYS", "AEDECOD"), width = 40,
                                    label = "SOC/\nPT"))),
      type = "multiline")),
    tfl_item("plan_paginate_rows", list(max_rows = 22))))
}

test_that("a listing's content is its code: table_plan(data) |> plan_listing(listing_col(...))", {
  out <- lst_out()
  for (it in out$content) expect_identical(nrow(tfl_schema_check(it)), 0L, info = it$fn)
  code <- tfl_content_code(out, data = "adae", pipe = "|>")
  expect_identical(code[1L], "plan <- table_plan(adae) |>")
  expect_true(any(grepl("vars = \"USUBJID\"", code, fixed = TRUE)))
  expect_true(any(grepl("type = \"multiline\"", code, fixed = TRUE)))
  expect_identical(utils::tail(code, 1L), "  plan_paginate_rows(max_rows = 22)")
})

test_that("a listing: the code, run, is the plan made directly, and listing_spec()'s pages", {
  skip_if_not_installed("cards")
  ae <- cards::ADAE[1:120, c("TRTA", "USUBJID", "AEBODSYS", "AEDECOD")]
  out <- lst_out()
  env <- new.env()
  env$adae <- ae
  eval(parse(text = c("library(rtfreporter)", tfl_content_code(out, data = "adae", pipe = "|>"))), env)
  direct <- tfl_content_run(out, ae)
  pages <- function(p) suppressMessages(rtfreporter::plan_apply(p))
  expect_identical(pages(env$plan), pages(direct))
  # what a workbook's listing made before: listing_spec() and as_rtftables()
  old <- rtfreporter::as_rtftables(ae, max_rows = 22, listing = rtfreporter::listing_spec(list(
    rtfreporter::listing_col("TRTA", width = 22, label = "Treatment"),
    rtfreporter::listing_col("USUBJID", width = 14, label = "Subject", collapse_repeats = TRUE),
    rtfreporter::listing_col(c("AEBODSYS", "AEDECOD"), width = 40, label = "SOC/\nPT")),
    type = "multiline"))
  rtf <- function(x) {
    f <- withr::local_tempfile(fileext = ".rtf")
    rtfreporter::generate_rtfreport(rtfreporter::rtf_tables(rtfreporter::rtf_document(), x),
                                    f, overwrite = TRUE)
    readLines(f)
  }
  expect_identical(rtf(direct), rtf(old))
})
