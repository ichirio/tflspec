# A figure's size on its report's page (tfl_fig_page_size()): a number,
# fit (the page's body) or blank (its own size); the report step places it
# so; a design's plot keeps only its dpi
sz_spec <- function(report = data.frame(output_id = "F-1", type = "figure"),
                    page = data.frame(output_id = NA, paper_size = "letter",
                                      orientation = "landscape"),
                    titles = data.frame(output_id = "F-1", line = c("1", "2"),
                                        center = c("Figure 1", "KM"))) {
  tfl_table_spec(report = report, page = page, titles = titles)
}

test_that("blank: its own size; fit: the page's body; a number: that size", {
  s <- tfl_fig_page_size(sz_spec(), "F-1")
  expect_equal(c(s$width_in, s$height_in), c(6.5, 4.5))
  expect_identical(s$mode, c(width = "native", height = "native"))
  expect_equal(tfl_fig_page_size(sz_spec(), "F-1", native = c(7.3, 4.2))$width_in, 7.3)
  fit <- data.frame(output_id = "F-1", type = "figure", figure_width_in = "fit",
                    figure_height_in = "FIT")
  s <- tfl_fig_page_size(sz_spec(report = fit), "F-1")
  # letter landscape 11 x 8.5, margins 0.75: 9.5 wide; 7 high less 2 titles
  # and 2 spacing lines at 9 pt (0.15 in each)
  expect_equal(c(s$width_in, s$height_in), c(9.5, 6.4))
  expect_identical(s$mode, c(width = "fit", height = "fit"))
  # portrait A4, margins of its own
  pg <- data.frame(output_id = NA, paper_size = "A4", orientation = "portrait",
                   margin_left_in = 1, margin_right_in = 1)
  expect_equal(tfl_fig_page_size(sz_spec(report = fit, page = pg), "F-1")$width_in, 6.27)
  # one a number, the other its own
  rep <- data.frame(output_id = "F-1", type = "figure", figure_width_in = "6")
  s <- tfl_fig_page_size(sz_spec(report = rep), "F-1")
  expect_equal(c(s$width_in, s$height_in), c(6, 4.5))
  expect_identical(s$mode, c(width = "fixed", height = "native"))
  # anything else: a problem of the spec
  bad <- data.frame(output_id = "F-1", type = "figure", figure_width_in = "-2")
  expect_error(sz_spec(report = bad), "a number of inches or fit")
})

test_that("the report step sizes only what is said", {
  code <- tfl_report_code(sz_spec(), "F-1", content = "plot")
  expect_true(any(grepl("rtf_figures(plot)", code, fixed = TRUE)))
  rep <- data.frame(output_id = "F-1", type = "figure", figure_width_in = "fit")
  code <- tfl_report_code(sz_spec(report = rep), "F-1", content = "plot")
  expect_true(any(grepl("rtf_figures(plot, width_twips = 13680)", code, fixed = TRUE)))
})

test_that("an old design's size is read as nothing: its report's now", {
  d <- tflspec:::.fig_design_from_list(list(plot = list(width = 9, height = 5, units = "in", dpi = 150)))
  expect_null(d$plot$width)
  expect_identical(d$plot$dpi, 150)
  expect_false("width" %in% tfl_fig_parts()$field[tfl_fig_parts()$piece == "plot"])
  # a script of its own: saved at the size it is given
  code <- tfl_fig_design_code(tfl_fig_template("box_by_group"), "f", width_in = 8, height_in = 3)
  expect_true("fig_width  <- 8" %in% code)
  expect_true("fig_height <- 3" %in% code)
})
