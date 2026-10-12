test_that("the treatment palette: Okabe-Ito in order; the arms' shapes and line types blank", {
  st <- tfl_fig_style()
  pal <- st$colors$colour[st$colors$palette == "treatment"]
  expect_identical(pal, c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00", "#56B4E9"))
  expect_true(all(c("treatment_shapes", "treatment_linetypes") %in% st$settings$key))
  expect_true(all(is.na(st$settings$value[st$settings$key %in% c("treatment_shapes", "treatment_linetypes")])))
  # blank: a figure is as it was
  d <- tfl_fig_template("km_simple", data = "ADTTE", param = "TTDE", pop = "SAFFL", group = "TRT01A")
  code <- tfl_fig_design_code(d, setup = TRUE, save = FALSE)
  expect_false(any(grepl("lty|shp|linetype_aes|legend.key.width", code)))
})

test_that("more groups than colours: the colours repeat with a warning, and the check says which share one", {
  e <- new.env(parent = globalenv())
  eval(parse(text = tfl_fig_setup_code()), envir = e)
  expect_warning(pal <- e$tfl_colours("treatment", LETTERS[1:7]),
                 "has 6 colours for 7 groups: they repeat [(]G[)]")
  expect_identical(unname(pal[["G"]]), unname(pal[["A"]]))
  expect_null(e$tfl_shapes(LETTERS[1:3]))
  expect_null(e$tfl_linetypes(LETTERS[1:3]))
  df <- data.frame(x = 1:7, y = 1:7, g = LETTERS[1:7])
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y, colour = g)) + ggplot2::geom_point() +
    ggplot2::scale_colour_manual(values = suppressWarnings(e$tfl_colours("treatment", df$g)))
  r <- suppressWarnings(e$tfl_check(p, quiet = TRUE))
  expect_match(r$problems, "the colour legend has 7 values and 6 colours [(]A, G share one[)]")
})

test_that("the arms' shapes and line types of the standard: the layers that colour by the arms map them", {
  st <- tfl_fig_style()
  st$settings$value[st$settings$key == "treatment_linetypes"] <- "solid, dashed, dotted"
  st$settings$value[st$settings$key == "treatment_shapes"] <- "16, 17, 15"
  withr::local_options(tflspec.fig_style = st)
  km <- tfl_fig_template("km_simple", data = "ADTTE", param = "TTDE", pop = "SAFFL", group = "TRT01A")
  code <- tfl_fig_design_code(km, setup = TRUE, save = FALSE)
  expect_true("lty <- tfl_linetypes(levels(factor(adtte_ttde$TRT01A)))" %in% code)
  expect_true(any(grepl("ggsurvfit(fit, linewidth = 0.3, linetype_aes = TRUE)", code, fixed = TRUE)))
  expect_true(any(grepl("scale_linetype_manual(values = lty, breaks = names(lty))", code, fixed = TRUE)))
  expect_true(any(grepl('theme(legend.key.width = unit(2.5, "lines"))', code, fixed = TRUE)))
  # no point layer: no shapes
  expect_false(any(grepl("shp", code)))
  mp <- tfl_fig_template("mean_se", data = "ADVS", param = "SYSBP", pop = "SAFFL", group = "TRT01A")
  code <- tfl_fig_design_code(mp, setup = FALSE, save = FALSE)
  expect_true('lty <- setNames(rep_len(c("solid", "dashed", "dotted"), length(lv)), lv)' %in% code)
  expect_true("shp <- setNames(rep_len(c(16, 17, 15), length(lv)), lv)" %in% code)
  expect_true(any(grepl("shape = TRT01A", code, fixed = TRUE)))
  expect_true(any(grepl("linetype = TRT01A", code, fixed = TRUE)))
  expect_true(any(grepl("scale_shape_manual(values = shp, breaks = names(shp))", code, fixed = TRUE)))
  expect_silent(parse(text = code))
  # the setup's helpers give them to the groups in order
  e <- new.env(parent = globalenv())
  eval(parse(text = tfl_fig_setup_code(st)), envir = e)
  expect_identical(e$tfl_shapes(c("P", "L", "H")), c(P = 16, L = 17, H = 15))
  expect_identical(e$tfl_linetypes(c("P", "L")), c(P = "solid", L = "dashed"))
})
