# Round 20 of the figure SPEC coverage: grid's grobs as a typed value
# (annotation_custom()'s and guide_custom()'s `grob`: {fn: grid::textGrob,
# args: {label: Favours drug, gp: {fn: grid::gpar, args: {fontsize: 9}}}},
# written grid::textGrob("Favours drug", gp = grid::gpar(fontsize = 9)))
# and guide_axis_stack()'s guides by position.

gb_item <- function(txt) .fig_lang_item(str2lang(txt))
gb_code <- function(it) .fig_deparse(.fig_item_lang(it))
gb_problems <- function(fn, args) tfl_schema_check(tfl_item(fn, args))

test_that("a grid grob reads as {fn: grid::textGrob} and is written grid::textGrob()", {
  it <- gb_item(paste0('annotation_custom(grid::textGrob("Favours drug", x = 0.25, y = -0.1, ',
                       'just = "right", gp = grid::gpar(fontsize = 8, fontface = "italic")))'))
  g <- it$args$grob
  expect_s3_class(g, "tfl_call")
  expect_identical(g$fn, "grid::textGrob")
  expect_identical(g$args$label, "Favours drug")
  expect_identical(g$args$just, "right")
  expect_identical(g$args$gp$fn, "grid::gpar")
  expect_identical(g$args$gp$args, list(fontsize = 8, fontface = "italic"))
  expect_identical(nrow(tfl_schema_check(it)), 0L)
  expect_identical(gb_code(it), paste0(
    'annotation_custom(grob = grid::textGrob("Favours drug", x = 0.25, y = -0.1, ',
    'just = "right", gp = grid::gpar(fontsize = 8, fontface = "italic")))'))
  # bare, grid attached: by its pkg::name
  it <- gb_item('annotation_custom(textGrob("Favours placebo", just = c("left", "top"), gp = gpar(col = "grey30")))')
  expect_identical(it$args$grob$fn, "grid::textGrob")
  expect_identical(it$args$grob$args$gp$fn, "grid::gpar")
  expect_identical(it$args$grob$args$just, c("left", "top"))
  # the other grobs, a unit inside
  it <- gb_item(paste0("guide_custom(grid::pointsGrob(x = 0.5, y = 0.5, pch = 17, ",
                       'size = unit(1, "char")), title = "Response")'))
  expect_identical(it$args$grob$fn, "grid::pointsGrob")
  expect_identical(it$args$grob$args$size$fn, "unit")
  expect_identical(nrow(tfl_schema_check(it)), 0L)
  it <- gb_item("annotation_custom(grid::roundrectGrob(gp = grid::gpar(fill = NA, lwd = 0.8)), xmin = 1, xmax = 2)")
  expect_identical(it$args$grob$fn, "grid::roundrectGrob")
  expect_identical(nrow(tfl_schema_check(it)), 0L)
  # another function's grob or an object: R, as before
  expect_s3_class(gb_item("annotation_custom(ggplotGrob(p), xmin = 1)")$args$grob, "tfl_r")
  expect_s3_class(gb_item("annotation_custom(g, xmin = 1)")$args$grob, "tfl_r")
})

test_that("a grob's arguments and its gpar() are checked", {
  ok <- gb_problems("annotation_custom", list(grob = tfl_call("grid::textGrob", list(
    label = "Favours drug", rot = 90, gp = tfl_call("grid::gpar", list(fontsize = 9, col = "grey30"))))))
  expect_identical(nrow(ok), 0L)
  p <- gb_problems("annotation_custom", list(grob = tfl_call("grid::textGrob", list(label = "F", rot = "up"))))
  expect_identical(p$level, "error")
  expect_match(p$message, "textGrob(rot = )", fixed = TRUE)
  p <- gb_problems("annotation_custom", list(grob = tfl_call("grid::textGrob", list(
    label = "F", gp = tfl_call("grid::gpar", list(colr = "red", fontface = "heavy"))))))
  expect_setequal(p$level, c("warning", "error"))
  expect_match(p$message[p$level == "warning"], "no argument `colr`", fixed = TRUE)
  expect_match(p$message[p$level == "error"], "fontface", fixed = TRUE)
  p <- gb_problems("annotation_custom", list(grob = tfl_call("grid::circleGrob", list(r = "big"))))
  expect_identical(p$level, "error")
  p <- gb_problems("annotation_custom", list(grob = tfl_call("grid::curvyGrob", list())))
  expect_identical(p$level, "error")
  # a function no grob of grid: not the argument's type
  p <- gb_problems("annotation_custom", list(grob = tfl_call("scales::label_percent", list())))
  expect_identical(p$level, "error")
  ent <- tfl_schema("grid::gpar")
  expect_identical(ent$figure, "grob")
  expect_true(all(c("col", "fill", "lwd", "fontsize", "fontface") %in% names(ent$args)))
  expect_identical(tfl_schema("grid::textGrob")$args$label$label$en, "Text")
})

test_that("a grob written by a SPEC draws as the original", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(x = c(0.5, 1, 2), y = c("a", "b", "c"))
  it <- tfl_item("annotation_custom", list(grob = tfl_call("grid::textGrob", list(
    label = "Favours drug", x = 0.25, y = -0.1, gp = tfl_call("grid::gpar", list(fontsize = 8))))))
  layer <- eval(.fig_item_lang(it), envir = asNamespace("ggplot2"))
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_point() + layer +
    ggplot2::coord_cartesian(clip = "off")
  g <- ggplot2::layer_grob(p, 2L)[[1L]]
  expect_s3_class(g, "text")
  expect_identical(g$label, "Favours drug")
  expect_identical(g$gp$fontsize, 8)
  expect_no_error(ggplot2::ggplotGrob(p))
  it <- tfl_item("guides", list(custom = tfl_call("guide_custom", list(
    grob = tfl_call("grid::segmentsGrob", list(x0 = 0.1, y0 = 0.5, x1 = 0.9, y1 = 0.5,
                                                arrow = tfl_call("arrow", list(type = "closed")))),
    title = "Ongoing"))))
  expect_identical(nrow(tfl_schema_check(it)), 0L)
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_point() +
    eval(.fig_item_lang(it), envir = asNamespace("ggplot2"))
  expect_no_error(ggplot2::ggplotGrob(p))
})

test_that("guide_axis_stack()'s guides after the first read by position, written so", {
  it <- gb_item('guides(x = guide_axis_stack("axis", guide_axis(cap = "both")))')
  st <- it$args$x
  expect_identical(st$fn, "guide_axis_stack")
  expect_identical(st$args$first, "axis")
  expect_identical(st$args$guides[[1L]]$fn, "guide_axis")
  expect_identical(nrow(tfl_schema_check(it)), 0L)
  expect_identical(gb_code(it), 'guides(x = guide_axis_stack(first = "axis", guide_axis(cap = "both")))')
  it <- gb_item(paste0('guides(y = guide_axis_stack(guide_axis(cap = "both"), "axis", ',
                       'guide_axis_logticks(), spacing = unit(2, "mm")))'))
  st <- it$args$y
  expect_identical(names(st$args), c("first", "guides", "spacing"))
  expect_identical(st$args$guides[[1L]], "axis")
  expect_identical(gb_code(it), paste0('guides(y = guide_axis_stack(first = guide_axis(cap = "both"), ',
                                       '"axis", guide_axis_logticks(), spacing = unit(2, "mm")))'))
  # the stacked guides' calls are checked
  p <- gb_problems("guides", list(x = tfl_call("guide_axis_stack", list(
    first = "axis", guides = list(tfl_call("guide_axis", list(capp = "both")))))))
  expect_identical(p$level, "warning")
  expect_match(p$message, "guide_axis(capp = )", fixed = TRUE)
  skip_if_not_installed("ggplot2")
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point() +
    eval(.fig_item_lang(gb_item('guides(x = guide_axis_stack("axis", guide_axis(cap = "both")))')),
         envir = asNamespace("ggplot2"))
  expect_no_error(ggplot2::ggplotGrob(p))
})
