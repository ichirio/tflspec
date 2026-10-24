# The page arguments no page or report sheet column says go as YAML in the
# page sheet's page_more and come back -- none dropped on the sheets' path

test_that("page arguments without a column: in page_more, and back", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "spec", "outputs"), recursive = TRUE)
  writeLines(c("id: PM-1", "title: Page more"), file.path(d, "study.yml"))
  page <- list(
    rtf_titles = list(font_size_half_points = 20L, row_height_twips = 300L, align = "left"),
    rtf_header = list(border = tfl_call("rtf_border", list(bottom = TRUE)), font_size_half_points = 16L),
    rtf_figures = list(align = "left"),
    titles = list(`1` = list(center = "Table 1")))
  tfl_write_output(tfl_output("T-1", "table", extends = "none", page = page),
                   file.path(d, "spec", "outputs", "T-1.yml"))
  sh <- tfl_sheets_from_yaml(d)
  pg <- sh$table$page
  pm <- pg$page_more[pg$output_id %in% "T-1"]
  expect_match(pm, "row_height_twips", fixed = TRUE)
  expect_match(pm, "rtf_header", fixed = TRUE)
  # (the titles' size has its column: not in page_more)
  expect_false(grepl("font_size_half_points: 20", pm, fixed = TRUE))
  # read back as the converter reads a page sheet's cells
  back <- tflspec:::.page_put(list(), "page", "page_more", pm, function(...) NULL, "T-1")
  orig <- tfl_read_outputs(d)[["T-1"]]$page
  expect_identical(back$rtf_titles, orig$rtf_titles[c("row_height_twips", "align")])
  expect_identical(back$rtf_header, orig$rtf_header)
  expect_identical(back$rtf_figures, orig$rtf_figures)
})

test_that("a page the columns say whole: no page_more", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "spec", "outputs"), recursive = TRUE)
  writeLines(c("id: PM-1", "title: Page more"), file.path(d, "study.yml"))
  tfl_write_output(tfl_output("T-1", "table", extends = "none", page = list(
    rtf_page = list(orientation = "portrait"), rtf_titles = list(font_size_half_points = 20L))),
    file.path(d, "spec", "outputs", "T-1.yml"))
  pg <- tfl_sheets_from_yaml(d)$table$page
  expect_true(is.null(pg$page_more) || all(is.na(pg$page_more)))
})
