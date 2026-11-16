# tfl_fig_content_from_r(): a plot made by a function of the program, its
# calls read as the lines it runs; p %+% data; last_plot()

fns_of <- function(items) vapply(items, function(it) it$fn, "")
any_r <- function(items) {
  any(vapply(items, function(it) {
    if (inherits(it, "tfl_plot")) return(any_r(it$items))
    # (NULL, `colour = NULL`, is no value of R)
    identical(it$fn, ".r") ||
      any(rapply(list(it$args), function(v) inherits(v, "tfl_r") && !identical(unclass(v)[1], "NULL"),
                 classes = "ANY", how = "unlist"))
  }, NA))
}

lab_fn <- c('lab_plot <- function(data, param, ylab = "Value") {',
            '  d <- data %>% filter(PARAMCD == param)',
            '  p <- ggplot(d, aes(AVISITN, AVAL, colour = TRTA)) +',
            '    stat_summary(fun = mean, geom = "line") +',
            '    labs(x = "Week", y = ylab, colour = NULL)',
            '  p + theme_bw()',
            '}')

test_that("a call of the program's plot function is read as the lines it runs", {
  x <- tfl_fig_content_from_r(c(lab_fn, 'p <- lab_plot(adlb, "ALT", "ALT (U/L)")',
                                'ggsave("f.png", p, width = 6, height = 4)'))
  expect_identical(fns_of(x$content), c("ggplot", "stat_summary", "labs", "theme_bw", "tfl_image"))
  # (the function's local data named for the call; its definition not kept)
  expect_identical(x$data$steps[[1]]$code, 'd_alt <- adlb %>% filter(PARAMCD == "ALT")')
  expect_identical(x$content[[1]]$args$data, "d_alt")
  expect_identical(x$content[[3]]$args$y, "ALT (U/L)")
  expect_identical(x$content[[3]]$args$x, "Week")
  expect_false(any_r(x$content))
  expect_true("lab_plot" %in% attr(x, "objects"))
  expect_identical(nrow(attr(x, "problems")), 0L)
  # a default taken; the call bare, or printed
  x <- tfl_fig_content_from_r(c(lab_fn, 'print(lab_plot(adlb, "AST"))'))
  expect_identical(x$content[[3]]$args$y, "Value")
  expect_identical(x$data$steps[[1]]$code, 'd_ast <- adlb %>% filter(PARAMCD == "AST")')
})

test_that("two calls arranged by patchwork are two plots, their data apart", {
  x <- tfl_fig_content_from_r(c(lab_fn, 'lab_plot(adlb, "ALT") / lab_plot(adlb, "AST")'))
  expect_true(all(vapply(x$content[1:2], inherits, NA, "tfl_plot")))
  expect_identical(fns_of(x$content)[3:4], c("wrap_plots", "tfl_image"))
  expect_identical(x$content[[1]]$items[[1]]$args$data, "d_alt")
  expect_identical(x$content[[2]]$items[[1]]$args$data, "d_ast")
  expect_identical(x$data$steps[[1]]$code,
                   c('d_alt <- adlb %>% filter(PARAMCD == "ALT")',
                     'd_ast <- adlb %>% filter(PARAMCD == "AST")'))
  expect_identical(x$content[[3]]$args$ncol, 1L)
})

test_that("a column named by an argument (.data[[ ]], !!sym(), {{ }}) is the column", {
  x <- tfl_fig_content_from_r(c(
    'prof <- function(d, yvar, by, title) {',
    '  ggplot(d, aes(AVISITN, .data[[yvar]], group = USUBJID)) +',
    '    geom_line(aes(colour = !!sym(by))) +',
    '    facet_wrap(vars(.data[[by]])) +',
    '    ggtitle(title)',
    '}',
    'prof(adlb, "CHG", "TRTA", title = "Change from baseline")'))
  expect_identical(fns_of(x$content), c("ggplot", "geom_line", "facet_wrap", "ggtitle", "tfl_image"))
  expect_identical(x$content[[1]]$args$data, "adlb")
  expect_identical(x$content[[1]]$args$mapping$y, "CHG")
  expect_identical(x$content[[2]]$args$mapping$colour, "TRTA")
  expect_identical(x$content[[4]]$args$label, "Change from baseline")
  expect_false(any_r(x$content))
  # {{ y }}: the column given bare; ... passed on
  x <- tfl_fig_content_from_r(c(
    'box <- function(d, y, ...) {',
    '  ggplot(d, aes(TRTA, {{ y }})) + geom_boxplot(...)',
    '}',
    'box(adlb, AVAL, width = 0.5, outlier.shape = NA)'))
  expect_identical(x$content[[1]]$args$mapping$y, "AVAL")
  expect_identical(x$content[[2]]$args$width, 0.5)
  expect_true(is.na(x$content[[2]]$args$outlier.shape))
})

test_that("a plot function that saves its plot: the save is the program's", {
  x <- tfl_fig_content_from_r(c(
    'fig <- function(param) {',
    '  p <- ggplot(adlb[adlb$PARAMCD == param, ], aes(AVISITN, AVAL)) + geom_point()',
    '  ggsave(paste0("f_", tolower(param), ".png"), p, width = 6, height = 4)',
    '}',
    'fig("ALT")'))
  expect_identical(fns_of(x$content), c("ggplot", "geom_point", "tfl_image"))
  expect_identical(x$content[[3]]$args$width_in, 6)
  expect_identical(x$data$steps[[1]]$code, 'p_alt_data <- adlb[adlb$PARAMCD == "ALT", ]')
})

test_that("a plot function called in a loop is left to the data part", {
  x <- tfl_fig_content_from_r(c(lab_fn,
                                'for (prm in c("ALT", "AST")) print(lab_plot(adlb, prm))',
                                'lab_plot(adlb, "ALT")'))
  expect_identical(fns_of(x$content)[1:2], c("ggplot", "stat_summary"))
  code <- x$data$steps[[1]]$code
  # (its definition kept: the loop calls it)
  expect_match(code[1], "^lab_plot <- function")
  expect_true(any(grepl("^for \\(prm", code)))
})

test_that("p %+% data is p with that data; last_plot() the plot made last", {
  x <- tfl_fig_content_from_r(c(
    'p_alt <- ggplot(alt, aes(AVISITN, AVAL)) + geom_point() + labs(y = "ALT")',
    'p_ast <- p_alt %+% ast + labs(y = "AST")',
    'p_ast'))
  expect_identical(fns_of(x$content), c("ggplot", "geom_point", "labs", "labs", "tfl_image"))
  expect_identical(x$content[[1]]$args$data, "ast")
  expect_identical(x$content[[4]]$args$y, "AST")
  # %+% a term: as +
  x <- tfl_fig_content_from_r(c('p <- ggplot(alt, aes(AVISITN, AVAL))', 'p %+% geom_point()'))
  expect_identical(fns_of(x$content), c("ggplot", "geom_point", "tfl_image"))
  expect_identical(x$content[[1]]$args$data, "alt")
  # last_plot()
  x <- tfl_fig_content_from_r(c('ggplot(alt, aes(AVISITN, AVAL)) + geom_point()',
                                'last_plot() + theme_minimal()',
                                'ggsave("f.png", width = 5, height = 4)'))
  expect_identical(fns_of(x$content), c("ggplot", "geom_point", "theme_minimal", "tfl_image"))
})

test_that("a program's plot function reads and draws as the program does", {
  skip_if_not_installed("dplyr")
  adlb <- data.frame(PARAMCD = rep(c("ALT", "AST"), each = 6), AVISITN = rep(c(0, 2, 4), 4),
                     AVAL = c(30, 32, 35, 28, 30, 33, 20, 22, 25, 21, 23, 24),
                     TRTA = rep(rep(c("A", "B"), each = 3), 2))
  code <- c(lab_fn, 'p <- lab_plot(adlb, "ALT", "ALT (U/L)")', 'p')
  env <- new.env()
  env$adlb <- adlb
  suppressPackageStartupMessages({
    library(dplyr)
    library(ggplot2)
  })
  orig <- eval(parse(text = code), env)
  x <- tfl_fig_content_from_r(code)
  code2 <- tflspec:::.fig_content_code(list(id = "f", data = x$data, content = x$content))
  env2 <- new.env(parent = env)
  env2$ggsave <- function(...) invisible()
  eval(parse(text = code2), env2)
  back <- env2$content
  expect_s3_class(back, "ggplot")
  expect_identical(ggplot2::layer_data(back)$y, ggplot2::layer_data(orig)$y)
  expect_identical(ggplot2::get_labs(back)$y, "ALT (U/L)")
})
