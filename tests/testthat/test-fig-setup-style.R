# The generated figure setup sets options(tflspec.fig_style =), so that
# km_curves() and forest_hr() take the study's style (#907).

test_that("the figure setup sets the style option: km_curves() takes its treatment colours", {
  skip_if_not_installed("ggsurvfit")
  skip_if_not_installed("survival")
  old <- options(tflspec.fig_style = NULL)
  on.exit(options(old), add = TRUE)
  st <- tfl_fig_style()
  # a treatment palette that is not the built-in one
  pal <- c("#112233", "#AA5500", "#335577", "#779911", "#BB2266", "#4477AA")
  st$colors$colour[st$colors$palette == "treatment"] <- pal
  expect_false(any(pal %in% tfl_fig_palettes()[["treatment"]]))

  code <- tfl_fig_setup_code(st)
  expect_true(any(grepl("options(tflspec.fig_style = list(", code, fixed = TRUE)))
  expect_null(getOption("tflspec.fig_style"))
  e <- new.env(parent = globalenv())
  eval(parse(text = code), envir = e)

  # the option is the style: tfl_fig_style() returns it as it is
  expect_identical(tfl_fig_style(), getOption("tflspec.fig_style"))
  expect_identical(tfl_fig_palettes(), tflspec:::.fs_palettes(st))
  expect_identical(unname(tfl_fig_palettes()[["treatment"]]), pal)

  # a tiny fictional survival data set, two arms
  d <- data.frame(AVAL = c(5, 8, 12, 20, 3, 6, 9, 15),
                  CNSR = c(0, 1, 0, 0, 0, 0, 1, 0),
                  ARM = rep(c("Arm A", "Arm B"), each = 4))
  fit <- ggsurvfit::survfit2(ggsurvfit::Surv_CNSR(AVAL, CNSR) ~ ARM, data = d)
  b <- ggplot2::ggplot_build(km_curves(fit))
  drawn <- unique(unlist(lapply(b$data, function(x) x$colour)))
  expect_setequal(drawn[!is.na(drawn)], pal[1:2])
})

test_that("tfl_check_fig() leaves the session's style option as it was", {
  old <- options(tflspec.fig_style = NULL)
  on.exit(options(old), add = TRUE)
  p <- ggplot2::ggplot(data.frame(x = 1:2, y = 1:2), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  suppressWarnings(tfl_check_fig(p, quiet = TRUE))
  expect_null(getOption("tflspec.fig_style"))
})
