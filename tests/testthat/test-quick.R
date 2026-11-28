test_that("every style of the quick API generates runnable code", {
  # every figure type drawn with ggplot2: about 20 s on CRAN's machines; CI
  # runs it on every configuration
  skip_on_cran()
  skip_if_not_installed("ggsurvfit")
  skip_if_not_installed("patchwork")
  adam <- tfl_example_adam()
  st <- tfl_fig_types()
  st <- st[st$type %in% c("km", "waterfall", "swimmer"), ]  # sankey / sunburst: test-sequence.R
  for (i in seq_len(nrow(st))) {
    fun <- get(c(km = "tfl_fig_km", waterfall = "tfl_fig_waterfall", swimmer = "tfl_fig_swimmer")[[st$type[i]]])
    for (use_adam in list(adam, NULL)) {
      code <- fun(use_adam, style = st$style[i])
      expect_s3_class(code, "tfl_code")
      expect_no_error(run_code(code), message = paste(st$type[i], st$style[i]))
    }
  }
})

test_that("legend presets map to legend types", {
  adam <- tfl_example_adam()
  for (lg in names(tflspec:::pp_quick_legends)) {
    code <- tfl_fig_km(adam, legend = lg)
    type <- tflspec:::pp_quick_legends[[lg]][1]
    if (type == "manual") expect_match(code, "legend_panel(", fixed = TRUE)
    if (type == "none") expect_match(code, 'legend.position = "none"', fixed = TRUE)
  }
  expect_error(tfl_fig_km(legend = "somewhere"), "legend")
})

test_that("unknown options are reported", {
  expect_error(tfl_fig_km(x_maxx = 3), "Unknown argument")
})

test_that("global options set data access and output path once", {
  withr_opts <- options(tflspec.data_expr = "adam_data${ds}")
  on.exit(options(withr_opts))
  code <- tfl_fig_km()
  expect_match(code, "adtte <- adam_data$adtte", fixed = TRUE)
})

test_that("swimmer events are named and ordered", {
  code <- tfl_fig_swimmer(events = c(Death = "DTHADY", Discontinued = "EOSDY"))
  expect_match(code, 'event_shape  <- c("Death" = 25, "Discontinued" = 24)', fixed = TRUE)
  expect_error(tfl_fig_swimmer(events = c("DTHADY")), "named")
})

test_that("plot list rows run through the quick API", {
  skip_if_not_installed("patchwork")
  adam <- tfl_example_adam()
  path <- tempfile(fileext = ".xlsx")
  tfl_fig_list_template(adam, path)
  expect_error(tfl_fig_list_template(path, adam), "the data first")
  expect_true(all(c("plots", "styles", "args") %in% openxlsx::getSheetNames(path)))
  code <- tfl_fig_list_code(path, adam)
  expect_named(code, c("F-14.2.1", "F-14.2.2", "F-14.2.3"))
  for (cc in code) expect_no_error(run_code(cc))
  bad <- data.frame(plot_id = "x", type = "km", args = "x_max = ")
  expect_error(tfl_fig_list_code(bad), "cannot read args")
})

test_that("the quick generators write an axis title past axis_title_width wrapped (the figure style's, 50)", {
  long <- "Change from baseline in the sum of the longest diameters of target lesions (%)"
  # the built-in width, 50: two lines at the spaces between words
  code <- paste(tfl_fig_waterfall(y_label = long), collapse = "\n")
  expect_match(code, 'y = "Change from baseline in the sum of the longest\\ndiameters of target lesions (%)"',
               fixed = TRUE)
  # NA, or tfl_unset(): no wrap, one line
  code <- paste(tfl_fig_waterfall(y_label = long, axis_title_width = NA), collapse = "\n")
  expect_match(code, paste0('y = "', long, '"'), fixed = TRUE)
  code <- paste(tfl_fig_km(y_label = long, axis_title_width = tfl_unset()), collapse = "\n")
  expect_match(code, paste0('y = "', long, '"'), fixed = TRUE)
  # a width of its own; the x title the same, the figure title never
  code <- paste(tfl_fig_km(x_label = long, y_label = long, title = long, axis_title_width = 30),
                collapse = "\n")
  w30 <- "Change from baseline in the\\nsum of the longest diameters\\nof target lesions (%)"
  expect_match(code, paste0('labs(x = "', w30, '", y = "', w30, '", title = "', long, '")'), fixed = TRUE)
  code <- paste(tfl_fig_swimmer(y_label = long, axis_title_width = 30), collapse = "\n")
  expect_match(code, paste0('y = "', w30, '"'), fixed = TRUE)
  # a short title is as written
  expect_match(paste(tfl_fig_km(), collapse = "\n"), 'labs(x = "Time (Months)", y = "Survival Probability")',
               fixed = TRUE)
  # a script of its own (tfl_fig_edish() ...): the figure style's width
  s <- tfl_fig_style()
  s$settings$value[s$settings$key == "axis_title_width"] <- "20"
  old <- options(tflspec.fig_style = s)
  on.exit(options(old), add = TRUE)
  code <- paste(tfl_fig_edish(), collapse = "\n")
  expect_match(code, 'y = "Maximum\\npost-baseline total\\nbilirubin (x ULN)"', fixed = TRUE)
  # and of a quick generator given none
  code <- paste(tfl_fig_waterfall(y_label = "Best percent change from baseline"), collapse = "\n")
  expect_match(code, 'y = "Best percent change\\nfrom baseline"', fixed = TRUE)
})
