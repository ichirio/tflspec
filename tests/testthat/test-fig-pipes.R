# tfl_fig_content_from_r(): a tidyverse program -- the data piped into
# ggplot(), terms in a list or on a flag, theme_update(), ggplot2::,
# a layer's data a part of the plot's

fns_of <- function(items) vapply(items, function(it) it$fn, "")
any_r <- function(items) {
  any(vapply(items, function(it) {
    identical(it$fn, ".r") || any(rapply(list(it$args), function(v) inherits(v, "tfl_r"),
                                         classes = "ANY", how = "unlist"))
  }, NA))
}

test_that("the data piped into ggplot() (%>%, |>, a dot) is the data part's object", {
  code <- c('adlb %>%',
            '  filter(PARAMCD == "ALT") %>%',
            '  ggplot(aes(AVISITN, AVAL)) +',
            '  geom_line(aes(group = USUBJID)) +',
            '  theme_bw()')
  x <- tfl_fig_content_from_r(code)
  expect_identical(fns_of(x$content), c("ggplot", "geom_line", "theme_bw", "tfl_image"))
  expect_identical(x$content[[1]]$args$data, "plot_data")
  expect_identical(x$data$steps[[1]]$code,
                   'plot_data <- adlb %>% filter(PARAMCD == "ALT")')
  expect_false(any_r(x$content))
  # the native pipe: R's own f(x, ...); an assigned plot names its data
  x <- tfl_fig_content_from_r(c('p <- adlb |> dplyr::filter(AVISITN > 0) |> ggplot(aes(AVISITN, CHG)) +',
                                '  geom_point()',
                                'ggsave("f.png", p, width = 6, height = 4)'))
  expect_identical(x$content[[1]]$args$data, "p_data")
  expect_identical(x$data$steps[[1]]$code, "p_data <- dplyr::filter(adlb, AVISITN > 0)")
  expect_identical(x$content[[3]]$args$width_in, 6)
  # a dot where the data goes; the data computed in ggplot()'s own argument
  x <- tfl_fig_content_from_r(c('adsl %>% mutate(OLD = AGE >= 65) %>%',
                                '  ggplot(data = ., aes(AGE, fill = OLD)) + geom_histogram(binwidth = 5)'))
  expect_identical(x$data$steps[[1]]$code, "plot_data <- adsl %>% mutate(OLD = AGE >= 65)")
  x <- tfl_fig_content_from_r('ggplot(data = fst[fst$n > 10, ], aes(hr, subgroup)) + geom_point()')
  expect_identical(x$content[[1]]$args$data, "plot_data")
  expect_identical(x$data$steps[[1]]$code, "plot_data <- fst[fst$n > 10, ]")
  # a name the program uses already is not taken
  x <- tfl_fig_content_from_r(c('plot_data <- 1',
                                'adsl %>% filter(AGE > 1) %>% ggplot(aes(AGE)) + geom_histogram(bins = plot_data)'))
  expect_identical(x$content[[1]]$args$data, "plot_data2")
})

test_that("ggsurvfit()'s fit piped in is the data part's object", {
  code <- c('adtte %>%',
            '  ggsurvfit::survfit2(survival::Surv(AVAL, 1 - CNSR) ~ TRTA, data = .) %>%',
            '  ggsurvfit() +',
            '  add_confidence_interval() +',
            '  scale_ggsurvfit(x_scales = list(breaks = seq(0, 60, by = 12)))')
  x <- tfl_fig_content_from_r(code)
  expect_identical(x$content[[1]]$args$x, "plot_fit")
  expect_match(x$data$steps[[1]]$code, "^plot_fit <- adtte %>% ggsurvfit::survfit2\\(")
  # (a list of a risk table's or a scale's arguments with numbers built of
  # constants is a map still)
  xs <- x$content[[3]]$args$x_scales
  expect_identical(names(xs), "breaks")
  expect_identical(as.character(xs$breaks), "seq(0, 60, by = 12)")
  expect_identical(nrow(attr(x, "problems")), 0L)
})

test_that("a layer's data as a part of the plot's data is its formula", {
  code <- c('ggplot(km, aes(time, surv, colour = TRTA)) +',
            '  geom_step() +',
            '  geom_point(data = km[km$censored, ], shape = 3) +',
            '  geom_point(data = subset(km, censored), shape = 3) +',
            '  geom_point(data = km %>% filter(censored, time > 0), shape = 3) +',
            '  geom_point(data = dplyr::filter(km, censored), shape = 3) +',
            '  geom_point(data = . %>% filter(censored), shape = 3)')
  x <- tfl_fig_content_from_r(code)
  d <- vapply(x$content[3:7], function(it) it$args$data, "")
  expect_identical(d, c("~subset(.x, censored)", "~subset(.x, censored)",
                        "~subset(.x, censored & time > 0)", "~subset(.x, censored)",
                        "~subset(.x, censored)"))
  expect_identical(x$data$steps, list())
  # another dataset's part is an object of the data part; a name is a name
  x <- tfl_fig_content_from_r(c('ggplot(alt, aes(TRTA, AVAL)) + geom_boxplot() +',
                                '  geom_point(data = adlb %>% filter(AVAL > 40), colour = "red") +',
                                '  geom_text(data = lbl, aes(label = n))'))
  expect_identical(x$content[[3]]$args$data, "alt_geom_point")
  expect_identical(x$content[[4]]$args$data, "lbl")
  expect_identical(x$data$steps[[1]]$code, "alt_geom_point <- adlb %>% filter(AVAL > 40)")
  # a column of another object in the condition: not only the plot's data
  expect_null(.fig_prog_layer_subset(quote(km[km$censored & nrow(km) > 1, ]), "km"))
  expect_null(.fig_prog_layer_subset(quote(km[km$censored, ]), "other"))
})

test_that("terms in a list, NULL, { } and if () on the program's flags are the terms ggplot2 adds", {
  code <- c('show_ref <- TRUE',
            'show_n <- FALSE',
            'refs <- list(geom_hline(yintercept = 40), geom_vline(xintercept = 40))',
            'ggplot(d, aes(x, y)) +',
            '  geom_point() +',
            '  refs +',
            '  {if (show_ref) geom_abline(slope = 1)} +',
            '  if (show_n) geom_text(aes(label = n)) else geom_rug() +',
            '  list(labs(x = "ALT"), theme_bw() + theme(legend.position = "top")) +',
            '  NULL')
  x <- tfl_fig_content_from_r(code)
  expect_identical(fns_of(x$content),
                   c("ggplot", "geom_point", "geom_hline", "geom_vline", "geom_abline", "geom_rug",
                     "labs", "theme_bw", "theme", "tfl_image"))
  expect_setequal(attr(x, "objects"), c("refs", "show_ref", "show_n"))
  expect_identical(x$data$steps, list())
  # a condition of the data stays R, as it is
  x <- tfl_fig_content_from_r(c('ggplot(d, aes(x, y)) + geom_point() +',
                                '  if (nrow(d) > 10) geom_smooth()'))
  expect_identical(x$content[[3]]$fn, ".r")
  expect_identical(as.character(x$content[[3]]$args$code), "if (nrow(d) > 10) geom_smooth()")
})

test_that("a step on a flag (if (flag) p <- p + ...) is the branch the flag takes", {
  code <- c('show_ci <- TRUE',
            'log_y <- FALSE',
            'p <- ggplot(sm, aes(AVISITN, m)) + geom_line()',
            'if (show_ci) {',
            '  p <- p + geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.2)',
            '}',
            'if (log_y) p <- p + scale_y_log10() else p <- p + scale_y_continuous(limits = c(0, NA))',
            'p')
  x <- tfl_fig_content_from_r(code)
  expect_identical(fns_of(x$content),
                   c("ggplot", "geom_line", "geom_ribbon", "scale_y_continuous", "tfl_image"))
  expect_setequal(attr(x, "objects"), c("show_ci", "log_y"))
  expect_identical(x$data$steps, list())
})

test_that("theme_update() is the session's theme with its settings; ggplot2:: is dropped", {
  code <- c('theme_set(theme_bw(base_size = 10))',
            'theme_update(legend.position = "top", panel.grid.minor = element_blank())',
            'ggplot2::ggplot(d, ggplot2::aes(x, y, colour = g)) +',
            '  ggplot2::geom_point() +',
            '  ggplot2::labs(colour = NULL)')
  x <- tfl_fig_content_from_r(code)
  expect_identical(fns_of(x$content),
                   c("ggplot", "theme_bw", "theme", "geom_point", "labs", "tfl_image"))
  expect_identical(x$content[[1]]$args$mapping, list(x = "x", y = "y", colour = "g"))
  expect_identical(x$content[[3]]$args$legend.position, "top")
  expect_identical(nrow(attr(x, "problems")), 0L)
  # without theme_set(): ggplot2's default theme, then the settings
  x <- tfl_fig_content_from_r(c('theme_update(legend.position = "none")',
                                'ggplot(d, aes(x, y)) + geom_point()'))
  expect_identical(fns_of(x$content), c("ggplot", "theme_grey", "theme", "geom_point", "tfl_image"))
})

test_that("a piped program written back draws its figure", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")
  adlb <- data.frame(USUBJID = rep(1:6, each = 3), AVISITN = rep(c(0, 4, 8), 6),
                     PARAMCD = rep(c("ALT", "AST"), each = 9), AVAL = c(20:28, 30:38),
                     TRTA = rep(c("A", "B"), 9), cens = rep(c(TRUE, FALSE, FALSE), 6))
  code <- c('show_ref <- TRUE',
            'p <- adlb %>%',
            '  filter(PARAMCD == "ALT") %>%',
            '  ggplot(aes(AVISITN, AVAL, colour = TRTA)) +',
            '  geom_line(aes(group = USUBJID)) +',
            '  geom_point(data = . %>% filter(cens), shape = 3) +',
            '  list(labs(x = "Week"), theme_bw()) +',
            '  {if (show_ref) geom_hline(yintercept = 25)}',
            'p')
  x <- tfl_fig_content_from_r(code)
  gen <- .fig_content_code(list(id = "f", data = x$data, content = x$content))
  env <- list2env(list(adlb = adlb), parent = asNamespace("dplyr"))
  env$`%>%` <- dplyr::`%>%`
  orig <- withr::with_package("ggplot2", eval(parse(text = code), env))
  env2 <- list2env(list(adlb = adlb), parent = env)
  withr::with_package("ggplot2", eval(parse(text = gen), env2))
  b0 <- ggplot2::ggplot_build(orig)
  b1 <- ggplot2::ggplot_build(env2$content)
  expect_equal(b1$data, b0$data)
  expect_identical(length(b1$data), 3L)
})
