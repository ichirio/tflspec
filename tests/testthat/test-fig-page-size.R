# A figure's size on its report's page (tfl_fig_page_size()): its own, or
# the page's body; the report step places it so
sz_spec <- function(report = data.frame(output_id = "F-1", type = "figure"),
                    page = data.frame(output_id = NA, paper_size = "letter",
                                      orientation = "landscape"),
                    titles = data.frame(output_id = "F-1", line = c("1", "2"),
                                        center = c("Figure 1", "KM"))) {
  tfl_table_spec(report = report, page = page, titles = titles)
}

test_that("blank: the page's body; a number: that size", {
  s <- tfl_fig_page_size(sz_spec(), "F-1")
  # letter landscape 11 x 8.5, margins 0.75: 9.5 wide
  expect_equal(s$width_in, 9.5)
  # 7 high less 2 titles and 2 spacing lines at 9 pt (0.15 in each)
  expect_equal(s$height_in, 6.4)
  expect_identical(s$fit, c(width = TRUE, height = TRUE))
  # portrait A4, margins of its own
  pg <- data.frame(output_id = NA, paper_size = "A4", orientation = "portrait",
                   margin_left_in = 1, margin_right_in = 1)
  expect_equal(tfl_fig_page_size(sz_spec(page = pg), "F-1")$width_in, 6.27)
  # one given: kept; the other the body's
  rep <- data.frame(output_id = "F-1", type = "figure", figure_width_in = 6)
  s <- tfl_fig_page_size(sz_spec(report = rep), "F-1")
  expect_equal(s$width_in, 6)
  expect_identical(s$fit, c(width = FALSE, height = TRUE))
})

test_that("the report step places the figure at that size", {
  code <- tfl_report_code(sz_spec(), "F-1", content = "plot")
  expect_true(any(grepl("rtf_figures(doc, plot, width_twips = 13680L, height_twips = 9216L)",
                        code, fixed = TRUE)))
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
