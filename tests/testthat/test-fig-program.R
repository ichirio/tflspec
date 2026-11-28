# tfl_fig_content_from_r(): a ggplot2 program as a figure's content

fns_of <- function(items) vapply(items, function(it) it$fn, "")

test_that("a program's objects of constants are read in place of their names", {
  code <- c(
    'library(ggplot2)',
    'adlb <- read.csv("adlb.csv")',
    'sm <- aggregate(AVAL ~ AVISITN + TRTA, adlb, mean)',
    'pd <- position_dodge(width = 0.3)',
    'cols <- c(Placebo = "grey40", Drug = "#1F78B4")',
    'wk <- seq(0, 24, by = 4)',
    'p <- ggplot(sm, aes(AVISITN, AVAL, colour = TRTA)) +',
    '  geom_line(position = pd) +',
    '  geom_point(position = pd)',
    'p <- p + scale_colour_manual(values = cols) + scale_x_continuous(breaks = wk)',
    'print(p)')
  x <- tfl_fig_content_from_r(code)
  expect_identical(fns_of(x$content), c("ggplot", "geom_line", "geom_point",
                                        "scale_colour_manual", "scale_x_continuous", "tfl_image"))
  expect_identical(x$content[[1]]$args$data, "sm")
  pos <- x$content[[2]]$args$position
  expect_identical(pos$fn, "position_dodge")
  expect_equal(pos$args$width, 0.3)
  expect_identical(x$content[[4]]$args$values, list(Placebo = "grey40", Drug = "#1F78B4"))
  expect_setequal(attr(x, "objects"), c("pd", "cols", "wk"))
  # the data's lines are the data part, in order; library() is left out
  expect_identical(x$data$steps[[1]]$code,
                   c('adlb <- read.csv("adlb.csv")', 'sm <- aggregate(AVAL ~ AVISITN + TRTA, adlb, mean)'))
  expect_identical(nrow(attr(x, "problems")[attr(x, "problems")$level == "error", ]), 0L)
  # nothing of it is R
  expect_false(any(vapply(x$content, function(it) any(vapply(it$args, inherits, NA, "tfl_r")), NA)))
})

test_that("a name inside aes() is a column, an object of the data stays its name", {
  code <- c('TRTA <- "x"',
            'n <- nrow(adsl)',
            'ggplot(adsl, aes(AGE, fill = TRTA)) + geom_histogram(bins = n)')
  x <- tfl_fig_content_from_r(code)
  expect_identical(x$content[[1]]$args$mapping$fill, "TRTA")
  expect_identical(attr(x, "objects"), character())
  expect_true(inherits(x$content[[2]]$args$bins, "tfl_r"))
  # (the object named inside aes() stays the data part's)
  expect_identical(x$data$steps[[1]]$code, c('TRTA <- "x"', "n <- nrow(adsl)"))
  # an object's part stays R, and its object the data part's
  x <- tfl_fig_content_from_r(c('cols <- c("red", "blue", "green")',
                                'pd <- position_dodge(0.5)',
                                'ggplot(d, aes(x, y, colour = g)) + geom_point(position = pd) +',
                                '  scale_colour_manual(values = cols[1:2])'))
  expect_identical(x$data$steps[[1]]$code, 'cols <- c("red", "blue", "green")')
  expect_identical(attr(x, "objects"), "pd")
  expect_true(inherits(x$content[[3]]$args$values, "tfl_r"))
})

test_that("a theme object of several terms is its terms; theme_set() is the plot's theme", {
  code <- c('my_theme <- theme_bw(base_size = 9) + theme(legend.position = "bottom")',
            'theme_set(theme_classic())',
            'ggplot(d, aes(x, y)) + geom_point() + my_theme')
  x <- tfl_fig_content_from_r(code)
  expect_identical(fns_of(x$content),
                   c("ggplot", "theme_classic", "geom_point", "theme_bw", "theme", "tfl_image"))
  expect_identical(x$content[[5]]$args$legend.position, "bottom")
  expect_identical(x$data$steps, list())
})

test_that("ggsave() gives the image's size in inches, its dpi, format and file", {
  x <- tfl_fig_content_from_r(c('p <- ggplot(d, aes(x, y)) + geom_point()',
                                'ggsave("f1.png", p, width = 25.4, height = 12.7, units = "cm", dpi = 300)'))
  img <- x$content[[length(x$content)]]
  expect_identical(img$fn, "tfl_image")
  expect_equal(img$args$width_in, 10)
  expect_equal(img$args$height_in, 5)
  expect_identical(img$args$dpi, 300L)
  expect_identical(img$args$format, "png")
  # the plot saved is the figure, not the last made
  x <- tfl_fig_content_from_r(c('p1 <- ggplot(d, aes(x, y)) + geom_point()',
                                'p2 <- ggplot(d, aes(x, y)) + geom_line()',
                                'ggsave(filename = "f.pdf", plot = p1)'))
  expect_identical(fns_of(x$content), c("ggplot", "geom_point", "tfl_image"))
  expect_identical(x$content[[3]]$args$format, "pdf")
})

test_that("plots arranged by patchwork are named plots and wrap_plots", {
  code <- c('p_km <- ggplot(km, aes(time, surv)) + geom_step()',
            'p_risk <- ggplot(rt, aes(time, strata, label = n)) + geom_text()',
            'fig <- p_km / p_risk + plot_layout(heights = c(3, 1))',
            'fig')
  x <- tfl_fig_content_from_r(code)
  expect_identical(fns_of(x$content), c("km", "risk", "wrap_plots", "tfl_image"))
  expect_true(inherits(x$content[[1]], "tfl_plot"))
  w <- x$content[[3]]$args
  expect_identical(w$plots, c("km", "risk"))
  expect_identical(w$ncol, 1L)
  expect_equal(unlist(w$heights), c(3, 1))
  x <- tfl_fig_content_from_r(c('a <- ggplot(d, aes(x, y)) + geom_point()',
                                'b <- ggplot(d, aes(x, y)) + geom_line()',
                                'wrap_plots(a, b, nrow = 1) + plot_annotation(title = "T")'))
  expect_identical(fns_of(x$content), c("a", "b", "wrap_plots", "plot_annotation", "tfl_image"))
  expect_identical(x$content[[3]]$args$plots, c("a", "b"))
  # p1 + p2 is the arrangement, not a term of p1
  x <- tfl_fig_content_from_r(c('a <- ggplot(d, aes(x, y)) + geom_point()',
                                'b <- ggplot(d, aes(x, y)) + geom_line()',
                                'a + b'))
  expect_identical(x$content[[3]]$args$plots, c("a", "b"))
  expect_identical(x$content[[3]]$args$nrow, 1L)
})

test_that("the content is written back as code that draws the program's figure", {
  skip_if_not_installed("ggplot2")
  code <- c('pd <- position_dodge(width = 0.4)',
            'cols <- c(Placebo = "grey40", Drug = "#1F78B4")',
            'lab_wk <- function(x) paste("Week", x)',
            'ggplot(sm, aes(AVISITN, mean, colour = TRTA)) +',
            '  geom_point(position = pd) +',
            '  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.3, position = pd) +',
            '  scale_colour_manual(values = cols) +',
            '  scale_x_continuous(labels = lab_wk)')
  x <- tfl_fig_content_from_r(code)
  expect_true(all(c("pd", "cols", "lab_wk") %in% attr(x, "objects")))
  terms <- lapply(x$content[-length(x$content)], .fig_item_lang)
  gen <- Reduce(function(a, b) call("+", a, b), terms)
  sm <- data.frame(AVISITN = rep(c(0, 4, 8), 2), mean = c(1, 2, 3, 1.5, 2.5, 3.5),
                   se = 0.2, TRTA = rep(c("Placebo", "Drug"), each = 3))
  env <- list2env(list(sm = sm), parent = asNamespace("ggplot2"))
  orig <- eval(parse(text = code), env)
  b0 <- ggplot2::ggplot_build(orig)
  b1 <- ggplot2::ggplot_build(eval(gen, env))
  expect_equal(b1$data, b0$data)
  expect_identical(b1$layout$panel_params[[1]]$x$get_labels(),
                   b0$layout$panel_params[[1]]$x$get_labels())
})

test_that("numbers by base R's functions of numbers are typed numbers", {
  rn <- tflspec:::.schema_r_numbers
  expect_equal(rn(tfl_r("seq(0.05, 0.95, by = 0.05)")), seq(0.05, 0.95, by = 0.05))
  expect_equal(rn(tfl_r("range(c(0.25, 0.5, 1, 2, 4))")), c(0.25, 4))
  expect_equal(rn(tfl_r("rev(log10(c(1, 10, 100)))")), c(2, 1, 0))
  expect_null(rn(tfl_r("range(AVAL)")))
  expect_null(rn(tfl_r("seq(0, 1, by = step)")))
  expect_null(rn(tfl_r("seq(0, 1, bogus = 2)")))
})

test_that("tfl_fig_content_from_r() says what it cannot read", {
  expect_error(tfl_fig_content_from_r("x <- 1"), "no plot")
  expect_error(tfl_fig_content_from_r("ggplot(d, aes(x)) +"), "not R")
  expect_error(tfl_fig_content_from_r(1), "R code")
})
