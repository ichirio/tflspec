test_that("tfl_fig_style(study) gives the study's figure style, else the built-in one", {
  st <- list(figure_style = list(colors = list(list(palette = "treatment", colour = "#000000"),
                                               list(palette = "treatment", colour = "#111111"))))
  s <- tfl_fig_style(st)
  expect_identical(s$colors$colour, c("#000000", "#111111"))
  # a sheet the study does not give is the built-in one
  expect_identical(s$markers, tfl_fig_style(list())$markers)
  expect_named(s, names(tflspec:::.fig_style_builtin()))
})

test_that("tfl_fig_style(study) reads a study folder", {
  d <- system.file("extdata", "spec-yaml", package = "tflspec")
  s <- tfl_fig_style(d)
  expect_true(all(c("colors", "markers") %in% names(s)))
  expect_true(nrow(s$colors) > 0L)
})
