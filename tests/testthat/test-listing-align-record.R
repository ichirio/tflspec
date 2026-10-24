# A listing's align and record (listing_spec(align, record);
# rtfreporter#720 plan_listing(align, record)): the listings sheet's
# columns, the program, YAML <-> sheets

la_rows <- function(align = NA, record = NA) {
  list(
    listings = data.frame(output_id = "L-1", dataset = "ADAE", align = align,
                          record = record, stringsAsFactors = FALSE),
    listing_cols = data.frame(output_id = "L-1", vars = c("USUBJID", "AEDECOD"),
                              width = c("10", "30"), stringsAsFactors = FALSE))
}

test_that("the program says them as listing_spec() takes them", {
  cat <- data.frame(dataset = "ADAE", path = "data/adae.rds", derive = NA)
  code <- paste(tfl_listing_code(tfl_listing_spec(la_rows("center", "FALSE")), "L-1", cat),
                collapse = "\n")
  expect_match(code, "align = \"center\"", fixed = TRUE)
  expect_match(code, "record = FALSE", fixed = TRUE)
  named <- paste(tfl_listing_code(tfl_listing_spec(la_rows(NA, "REC")), "L-1", cat),
                 collapse = "\n")
  expect_match(named, "record = \"REC\"", fixed = TRUE)
  none <- paste(tfl_listing_code(tfl_listing_spec(la_rows()), "L-1", cat), collapse = "\n")
  expect_false(grepl("align =|record =", none))
})

test_that("YAML <-> sheets: plan_listing()'s align and record are the listings row's", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "spec", "outputs"), recursive = TRUE)
  writeLines(c("datasets:", "  - dataset: ADAE", "    path: data/adae.rds"),
             file.path(d, "spec", "data.yml"))
  content <- list(tfl_item("plan_listing", list(
    cols = list(tfl_call("listing_col", list(vars = list("USUBJID"), width = 12L))),
    align = "right", record = FALSE)))
  o <- tfl_output("L-1", "listing", label = "AEs", extends = "none",
                  data = list(source = "dataset", dataset = "ADAE"), content = content)
  tfl_write_output(o, file.path(d, "spec", "outputs", "L-1.yml"))
  sh <- tfl_sheets_from_yaml(d)
  ls <- sh$listing$listings
  expect_identical(ls$align[ls$output_id %in% "L-1"], "right")
  expect_identical(ls$record[ls$output_id %in% "L-1"], "FALSE")
  expect_true(is.na(ls$content_more[ls$output_id %in% "L-1"]))
  to <- withr::local_tempdir()
  suppressMessages(tfl_yaml_from_sheets(sh, to))
  f <- list.files(to, "^L-1[.]yml$", recursive = TRUE, full.names = TRUE)
  pl <- Filter(function(it) identical(it$fn, "plan_listing"), tfl_read_output(f[1L])$content)[[1L]]
  expect_identical(pl$args$align, "right")
  expect_identical(pl$args$record, FALSE)
})

test_that("a YAML listing's plan_listing(align, record) runs in rtfreporter", {
  skip_if_not("record" %in% names(formals(rtfreporter::plan_listing)))
  d <- data.frame(USUBJID = c("S-1", "S-2"), AEDECOD = c("A", "B"), stringsAsFactors = FALSE)
  o <- tfl_output("L-1", "listing", content = list(tfl_item("plan_listing", list(
    cols = list(tfl_call("listing_col", list(vars = list("USUBJID"), width = 12L)),
                tfl_call("listing_col", list(vars = list("AEDECOD"), width = 12L))),
    align = "center", record = FALSE))))
  got <- suppressMessages(rtfreporter::plan_apply(tfl_content_run(o, d)))
  ref <- rtfreporter::as_rtftables(d, listing = rtfreporter::listing_spec(
    list(rtfreporter::listing_col("USUBJID", width = 12), rtfreporter::listing_col("AEDECOD", width = 12)),
    align = "center", record = FALSE))
  expect_equal(got, ref)
})
