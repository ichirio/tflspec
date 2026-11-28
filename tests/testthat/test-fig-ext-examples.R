# Round 21 of the figure SPEC coverage: the extension packages' own
# examples through a SPEC and back -- ggh4x's facets by their bare names,
# their variables as columns; a call of an extension inside a value
# (position_nudge_repel(), ggsurvfit's themes, a grob in wrap_elements());
# a value each of calls (facetted_pos_scales(y = list(NULL, ...))).

ex_item <- function(txt) .fig_lang_item(str2lang(txt))
ex_code <- function(it) .fig_deparse(.fig_item_lang(it))
ex_ok <- function(it) nrow(tfl_schema_check(it)) == 0L

test_that("ggh4x's facets read by their bare names, their variables as columns", {
  skip_if_not_installed("ggh4x")
  it <- ex_item('facet_grid2(cyl ~ drv, axes = "all")')
  expect_identical(it$args$rows, "cyl")
  expect_identical(it$args$cols, "drv")
  expect_true(ex_ok(it))
  expect_identical(ex_code(it), 'ggh4x::facet_grid2(rows = vars(cyl), cols = vars(drv), axes = "all")')
  it <- ex_item('facet_wrap2(vars(class), axes = "all", remove_labels = "x")')
  expect_identical(it$args$facets, "class")
  expect_true(ex_ok(it))
  it <- ex_item("facet_nested_wrap(vars(cyl, drv), nest_line = element_line())")
  expect_identical(it$args$facets, list("cyl", "drv"))
  expect_true(ex_ok(it))
  # a one-sided formula: the columns' variables, as ggplot2 takes it
  it <- ex_item("facet_nested(~ vs + cyl)")
  expect_null(it$args$rows)
  expect_identical(it$args$cols, list("vs", "cyl"))
  expect_identical(ex_code(it), "ggh4x::facet_nested(cols = vars(vs, cyl))")
  it <- ex_item("facet_grid(~ cyl)")
  expect_identical(it$args$cols, "cyl")
  expect_identical(ex_code(it), "facet_grid(cols = vars(cyl))")
  # by ggh4x:: as before
  expect_identical(ex_item("ggh4x::facet_wrap2(vars(class))")$args$facets, "class")
  # a strip is a typed call
  it <- ex_item("facet_wrap2(vars(cyl, drv), strip = strip_nested(bleed = TRUE))")
  expect_s3_class(it$args$strip, "tfl_call")
  expect_true(ex_ok(it))
})

test_that("ggh4x's facets draw the same through a SPEC", {
  skip_if_not_installed("ggh4x")
  # (ggh4x's functions bare, as with the package attached, not attaching it)
  env <- new.env(parent = globalenv())
  for (f in c("facet_grid2", "facet_nested", "facet_nested_wrap")) env[[f]] <- getExportedValue("ggh4x", f)
  env$vars <- ggplot2::vars
  for (txt in c('facet_grid2(cyl ~ drv, axes = "all")', "facet_nested(~ drv + cyl)",
                "facet_nested_wrap(vars(cyl, drv))")) {
    e <- str2lang(txt)
    back <- .fig_item_lang(.fig_lang_item(e))
    p0 <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy)) + ggplot2::geom_point() + eval(e, env)
    p1 <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy)) + ggplot2::geom_point() + eval(back, env)
    expect_identical(ggplot2::ggplot_build(p0)$layout$layout, ggplot2::ggplot_build(p1)$layout$layout)
  }
})

test_that("an extension's call inside a value is typed", {
  skip_if_not_installed("ggrepel")
  skip_if_not_installed("ggsurvfit")
  it <- ex_item("geom_text_repel(position = position_nudge_repel(x = c(0.1, -0.1), y = 0.2))")
  expect_s3_class(it$args$position, "tfl_call")
  expect_identical(it$args$position$args$x, c(0.1, -0.1))
  expect_true(ex_ok(it))
  expect_identical(ex_code(it),
                   "ggrepel::geom_text_repel(position = ggrepel::position_nudge_repel(x = c(0.1, -0.1), y = 0.2))")
  it <- ex_item("add_risktable(theme = theme_risktable_boxed())")
  expect_identical(it$args$theme$fn, "theme_risktable_boxed")
  expect_true(ex_ok(it))
  it <- ex_item("ggsurvfit(theme = theme_ggsurvfit_KMunicate())")
  expect_identical(it$args$theme$fn, "theme_ggsurvfit_KMunicate")
  # a list of themes stays R
  expect_s3_class(ex_item("add_risktable(theme = list(theme_risktable_default(), theme_bw()))")$args$theme,
                  "tfl_r")
  # not a position, not a theme: an error of the check
  p <- tfl_schema_check(tfl_item("geom_point", list(position = tfl_call("element_text", list()))))
  expect_identical(p$level, "error")
  p <- tfl_schema_check(tfl_item("add_risktable", list(theme = tfl_call("element_text", list()))))
  expect_identical(p$level, "error")
  # NULL for no line (add_quantile(y_value = NULL))
  expect_true(ex_ok(ex_item("add_quantile(y_value = NULL, x_value = 10)")))
})

test_that("patchwork's wrap_elements() and free() take a grob or a plot by name", {
  skip_if_not_installed("patchwork")
  it <- ex_item('wrap_elements(panel = grid::textGrob("Some text"), full = grid::rectGrob(gp = grid::gpar(fill = "grey90")))')
  expect_identical(it$args$panel$fn, "grid::textGrob")
  expect_identical(it$args$full$args$gp$fn, "grid::gpar")
  expect_true(ex_ok(it))
  expect_identical(ex_code(it), paste0('wrap_elements(panel = grid::textGrob("Some text"), ',
                                       'full = grid::rectGrob(gp = grid::gpar(fill = "grey90")))'))
  it <- ex_item("wrap_elements(panel = p_km)")
  expect_identical(it$args$panel, "p_km")
  expect_identical(ex_code(it), "wrap_elements(panel = p_km)")
  it <- ex_item('free(p_table, type = "label")')
  expect_identical(it$args$x, "p_table")
  expect_true(ex_ok(it))
  expect_identical(ex_code(it), 'free(x = p_table, type = "label")')
  it <- ex_item("inset_element(grid::circleGrob(), 0.4, 0.4, 0.6, 0.6)")
  expect_identical(it$args$p$fn, "grid::circleGrob")
  expect_true(ex_ok(it))
})

test_that("a value each of calls: a panel's scale, a strip's margin, NULL where none", {
  skip_if_not_installed("ggh4x")
  it <- ex_item('facetted_pos_scales(y = list(NULL, scale_y_continuous(transform = "reverse")))')
  y <- it$args$y
  expect_length(y, 2L)
  expect_s3_class(y[[1L]], "tfl_r")
  expect_identical(y[[2L]]$fn, "scale_y_continuous")
  expect_true(ex_ok(it))
  expect_identical(ex_code(it),
                   'ggh4x::facetted_pos_scales(y = list(NULL, scale_y_continuous(transform = "reverse")))')
  # the calls inside are checked: another scale's axis is an error, an unknown argument warns
  p <- tfl_schema_check(tfl_item("facetted_pos_scales", list(y = list(tfl_r("NULL"),
                                                                     tfl_call("scale_x_log10", list())))))
  expect_identical(p$level, "error")
  p <- tfl_schema_check(tfl_item("facetted_pos_scales", list(y = list(tfl_call("scale_y_log10",
                                                                               list(brks = 1))))))
  expect_identical(p$level, "warning")
  # scales by a condition (a formula) stay R
  expect_s3_class(ex_item("facetted_pos_scales(y = list(cyl == 4 ~ scale_y_log10()))")$args$y, "tfl_r")
  it <- ex_item('elem_list_text(family = c("mono", NA, "sans"), margin = list(NULL, margin(t = 5)))')
  expect_identical(it$args$margin[[2L]]$fn, "margin")
  expect_true(ex_ok(it))
  expect_match(ex_code(it), "margin = list(NULL, margin(t = 5))", fixed = TRUE)
  # a text a strip stays a vector
  expect_identical(ex_code(ex_item('elem_list_text(colour = c("red", "blue"))')),
                   'ggh4x::elem_list_text(colour = c("red", "blue"))')
})

test_that("a facet's rows and cols as R are written as they are", {
  it <- tfl_item("facet_grid", list(rows = tfl_r("vars(SEX)"), cols = "TRTA"))
  expect_identical(ex_code(it), "facet_grid(rows = vars(SEX), cols = vars(TRTA))")
  skip_if_not_installed("ggh4x")
  it <- tfl_item("ggh4x::facet_grid2", list(cols = tfl_r("vars(g)"), scales = "free_y"))
  expect_identical(ex_code(it), 'ggh4x::facet_grid2(cols = vars(g), scales = "free_y")')
})
