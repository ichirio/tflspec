# Round 22 of the figure SPEC coverage: patchwork's arrangement as a term
# (wrap_plots(p1, p2), plots by a design's letters), a design of areas,
# a level's own tags, a strip's elements one a strip, a base graphics
# plot in wrap_elements(), texts picked of letters.

cl_item <- function(txt) .fig_lang_item(str2lang(txt))
cl_code <- function(it) .fig_deparse(.fig_item_lang(it))
cl_ok <- function(it) nrow(tfl_schema_check(it)) == 0L

test_that("wrap_plots() reads its plots by name and writes them back", {
  skip_if_not_installed("patchwork")
  it <- cl_item("wrap_plots(p1, p2, ncol = 1, heights = c(3, 1))")
  expect_identical(it$args$plots, c("p1", "p2"))
  expect_identical(it$args$ncol, 1)
  expect_true(cl_ok(it))
  expect_identical(cl_code(it), "wrap_plots(p1, p2, ncol = 1, heights = c(3, 1))")
  # one list of plots by its name, or written out
  it <- cl_item("wrap_plots(plot_list, ncol = 1)")
  expect_identical(it$args$plots, "plot_list")
  expect_true(cl_ok(it))
  expect_identical(cl_code(it), "wrap_plots(plot_list, ncol = 1)")
  expect_identical(cl_item("wrap_plots(list(p1, p2))")$args$plots, c("p1", "p2"))
  # by a design's letters: a map
  it <- cl_item('wrap_plots(B = p1, A = p2, design = "AB\\nCC")')
  expect_identical(it$args$plots, list(B = "p1", A = "p2"))
  expect_true(cl_ok(it))
  expect_identical(cl_code(it), 'wrap_plots(B = p1, A = p2, design = "AB\\nCC")')
  # plots picked of a list by their place
  it <- cl_item("wrap_plots(built[[1]], built[[2]], ncol = 1)")
  expect_identical(it$args$plots, c("built[[1]]", "built[[2]]"))
  expect_identical(cl_code(it), "wrap_plots(built[[1]], built[[2]], ncol = 1)")
  # a plot built in place stays R
  expect_identical(cl_item("wrap_plots(p1 + ggtitle('a'), p2)")$fn, ".r")
})

test_that("wrap_plots() by a design's letters draws the same, and in a content", {
  skip_if_not_installed("patchwork")
  p1 <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, disp)) + ggplot2::geom_point()
  p2 <- ggplot2::ggplot(mtcars, ggplot2::aes(gear)) + ggplot2::geom_bar()
  e <- str2lang('wrap_plots(B = p1, A = p2, design = "AB\\nAB")')
  back <- .fig_item_lang(.fig_lang_item(e))
  env <- environment()
  env$wrap_plots <- patchwork::wrap_plots
  a <- eval(e, env)
  b <- eval(back, env)
  expect_identical(a$patches$layout$design, b$patches$layout$design)
  expect_identical(length(a$patches$plots), length(b$patches$plots))
  # the content's arrangement: by letter, the plots' p_ names
  lines <- .fig_compose_lines("fig", list(list(name = "km"), list(name = "risk")),
                              list(tfl_item("wrap_plots", list(plots = list(A = "km", B = "risk"),
                                                               design = "A\nB"))))
  expect_match(paste(lines, collapse = "\n"), "wrap_plots(A = p_km, B = p_risk, design = ", fixed = TRUE)
})

test_that("a program's wrap_plots() by a design's letters is read into the content", {
  skip_if_not_installed("patchwork")
  plots <- list(p1 = TRUE, p2 = TRUE)
  e <- str2lang('wrap_plots(A = p1, B = p2, design = "AB")')
  expect_identical(.fig_prog_compose_plots(e, plots), c(A = "p1", B = "p2"))
  items <- .fig_prog_compose_items(e, c(p1 = "one", p2 = "two"))
  expect_identical(items[[1L]]$args$plots, list(A = "one", B = "two"))
  expect_identical(items[[1L]]$args$design, "AB")
})

test_that("plot_layout()'s design of areas is a sequence of area() calls", {
  skip_if_not_installed("patchwork")
  it <- cl_item("plot_layout(design = c(area(1, 1), area(1, 3, 3), area(3, 1, 3, 2)))")
  expect_length(it$args$design, 3L)
  expect_true(all(vapply(it$args$design, inherits, NA, "tfl_call")))
  expect_identical(it$args$design[[2L]]$args, list(t = 1, l = 3, b = 3))
  expect_true(cl_ok(it))
  code <- cl_code(it)
  expect_identical(code, paste0("plot_layout(design = c(area(t = 1, l = 1), ",
                                "area(t = 1, l = 3, b = 3), area(t = 3, l = 1, b = 3, r = 2)))"))
  env <- list2env(list(plot_layout = patchwork::plot_layout, area = patchwork::area))
  expect_identical(eval(str2lang(code), env)$design, patchwork::plot_layout(
    design = c(patchwork::area(1, 1), patchwork::area(1, 3, 3), patchwork::area(3, 1, 3, 2)))$design)
  # a design's text as before
  expect_identical(cl_item('plot_layout(design = "AB")')$args$design, "AB")
  # not an area: an error of the check
  p <- tfl_schema_check(tfl_item("plot_layout", list(design = list(tfl_call("element_text", list())))))
  expect_true("error" %in% p$level)
})

test_that("a level's own tags are a sequence of sequences, written list()", {
  skip_if_not_installed("patchwork")
  it <- cl_item('plot_annotation(tag_levels = list(c("&", "%"), "1"))')
  expect_identical(it$args$tag_levels, list(c("&", "%"), "1"))
  expect_true(cl_ok(it))
  expect_identical(cl_code(it), 'plot_annotation(tag_levels = list(c("&", "%"), "1"))')
  # as YAML gives it
  y <- yaml::yaml.load("[['&', '%'], '1']")
  expect_identical(.fig_deparse(.fig_value_lang(y, "tag_levels")), 'list(c("&", "%"), "1")')
  # tags of one level, as before
  expect_identical(cl_code(cl_item('plot_annotation(tag_levels = c("A", "1"))')),
                   'plot_annotation(tag_levels = c("A", "1"))')
  expect_identical(cl_code(cl_item('plot_annotation(tag_levels = "A")')),
                   'plot_annotation(tag_levels = "A")')
})

test_that("a strip's elements one a strip are a sequence of element calls", {
  skip_if_not_installed("ggh4x")
  it <- cl_item('ggh4x::strip_themed(background_x = list(element_blank(), element_rect(fill = "dodgerblue")))')
  bg <- it$args$background_x
  expect_length(bg, 2L)
  expect_identical(bg[[2L]]$fn, "element_rect")
  expect_true(cl_ok(it))
  expect_identical(cl_code(it), paste0('ggh4x::strip_themed(background_x = list(element_blank(), ',
                                       'element_rect(fill = "dodgerblue")))'))
  # elem_list_rect() as before
  it <- cl_item('ggh4x::strip_themed(background_x = ggh4x::elem_list_rect(fill = c("red", "blue")))')
  expect_s3_class(it$args$background_x, "tfl_call")
  # an element of another kind: an error of the check
  p <- tfl_schema_check(tfl_item("ggh4x::strip_themed",
                                 list(background_x = list(tfl_call("element_line", list())))))
  expect_true("error" %in% p$level)
})

test_that("a base graphics plot in wrap_elements() is a formula", {
  skip_if_not_installed("patchwork")
  it <- cl_item("wrap_elements(full = ~ plot(mtcars$mpg, mtcars$disp))")
  expect_identical(it$args$full, "~plot(mtcars$mpg, mtcars$disp)")
  expect_true(cl_ok(it))
  expect_identical(cl_code(it), "wrap_elements(full = ~plot(mtcars$mpg, mtcars$disp))")
  # a plot by name as before
  expect_identical(cl_code(cl_item("wrap_elements(panel = p1)")), "wrap_elements(panel = p1)")
})

test_that("texts picked of letters are texts of constants", {
  expect_identical(.schema_r_texts(tfl_r("letters[1:4]")), c("a", "b", "c", "d"))
  expect_identical(.schema_r_texts(tfl_r("LETTERS[c(1, 3)]")), c("A", "C"))
  expect_null(.schema_r_texts(tfl_r("df$x[1:4]")))
  it <- .fig_lang_item(str2lang('scale_fill_identity("trt", labels = letters[1:4])'))
  expect_true(cl_ok(it))
})
