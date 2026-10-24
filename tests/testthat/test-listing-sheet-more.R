# A listing YAML -> sheets -> YAML: the items and a column's arguments the
# sheets' columns cannot say go as YAML in content_more / more, and come
# back -- nothing lost (#447)

lsm_study <- function(content, env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  dir.create(file.path(d, "spec", "outputs"), recursive = TRUE)
  writeLines(c("datasets:", "  - dataset: ADAE", "    path: data/adae.rds"),
             file.path(d, "spec", "data.yml"))
  o <- tfl_output("L-1", "listing", label = "Adverse events", extends = "none",
                  data = list(source = "dataset", dataset = "ADAE"), content = content)
  tfl_write_output(o, file.path(d, "spec", "outputs", "L-1.yml"))
  d
}

lsm_key <- function(items) {
  vapply(items, function(it) paste(it$fn, paste(deparse(it$args[order(names(it$args))]), collapse = "")), "")
}

test_that("the items the sheets cannot say: in content_more, and back", {
  content <- list(
    tfl_item("plan_listing", list(
      cols = list(tfl_call("listing_col", list(vars = list("USUBJID"), label = "Subject", width = 12L)),
                  tfl_call("listing_col", list(vars = list("AEDECOD"), label = "Term", width = 30L,
                                               rel_width = 2L))),
      type = "listing")),
    tfl_item("plan_hide", list(cols = list("AESEQ"))),
    tfl_item("plan_paginate_rows", list(max_rows = 20L, page_by = "USUBJID")),
    tfl_item("plan_blanks", list(where = "records")))
  d <- lsm_study(content)
  sh <- tfl_sheets_from_yaml(d)
  ls <- sh$listing$listings
  expect_match(ls$content_more[ls$output_id %in% "L-1"], "plan_hide", fixed = TRUE)
  expect_match(ls$content_more[ls$output_id %in% "L-1"], "page_by", fixed = TRUE)
  lc <- sh$listing$listing_cols
  expect_match(lc$more[lc$output_id %in% "L-1"][2L], "rel_width", fixed = TRUE)
  expect_true(is.na(lc$more[lc$output_id %in% "L-1"][1L]))
  # (said by the columns: the blank row, the type)
  expect_identical(ls$blank_row[ls$output_id %in% "L-1"], "TRUE")
  to <- withr::local_tempdir()
  suppressMessages(tfl_yaml_from_sheets(sh, to, overwrite = TRUE))
  back <- tfl_read_outputs(to)[["L-1"]]
  expect_setequal(lsm_key(back$content), lsm_key(tfl_read_outputs(d)[["L-1"]]$content))
})

test_that("a listing the columns say whole: no content_more, as before", {
  content <- list(
    tfl_item("plan_listing", list(cols = list(
      tfl_call("listing_col", list(vars = list("USUBJID"), label = "Subject", width = 12L))))),
    tfl_item("plan_paginate_rows", list(max_rows = 20L)))
  d <- lsm_study(content)
  sh <- tfl_sheets_from_yaml(d)
  ls <- sh$listing$listings
  expect_true(is.null(ls$content_more) || all(is.na(ls$content_more)))
  expect_identical(ls$max_rows[ls$output_id %in% "L-1"], "20")
  to <- withr::local_tempdir()
  suppressMessages(tfl_yaml_from_sheets(sh, to, overwrite = TRUE))
  back <- tfl_read_outputs(to)[["L-1"]]
  expect_setequal(lsm_key(back$content), lsm_key(tfl_read_outputs(d)[["L-1"]]$content))
})
