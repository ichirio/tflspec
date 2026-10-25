# A report's ARD under a name of its own (data.object): the content's code
# starts from it; the sheets keep it (report$ard_object); blank, ard

test_that("data.object: the content's pipe starts from it; YAML <-> sheets keep it", {
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  dir.create(file.path(d, "spec", "outputs"), recursive = TRUE)
  writeLines(c("datasets:", "  - dataset: ADSL", "    path: data/adsl.rds"),
             file.path(d, "spec", "data.yml"))
  o <- tfl_output("T-1", "table", extends = "none", data = list(source = "ard", object = "ard_dm"),
                  content = list(tfl_item("table_plan", list(cols = "TRT01A"))))
  tfl_write_output(o, file.path(d, "spec", "outputs", "T-1.yml"))
  code <- paste(tfl_content_code(tfl_read_output(file.path(d, "spec", "outputs", "T-1.yml"))),
                collapse = "\n")
  expect_match(code, "table_plan(ard_dm", fixed = TRUE)
  sh <- suppressMessages(tfl_sheets_from_yaml(d))
  rr <- sh$table$report
  expect_identical(rr$ard_object[rr$output_id %in% "T-1"], "ard_dm")
  to <- tempfile()
  on.exit(unlink(to, recursive = TRUE), add = TRUE)
  suppressMessages(tfl_yaml_from_sheets(sh, to))
  f <- list.files(to, "^T-1[.]yml$", recursive = TRUE, full.names = TRUE)
  expect_identical(tfl_read_output(f[1L])$data$object, "ard_dm")
  # blank: ard, as before (no column written)
  o$data$object <- "ard"
  tfl_write_output(o, file.path(d, "spec", "outputs", "T-1.yml"))
  sh2 <- suppressMessages(tfl_sheets_from_yaml(d))
  expect_true(is.null(sh2$table$report$ard_object) || all(is.na(sh2$table$report$ard_object)))
})
