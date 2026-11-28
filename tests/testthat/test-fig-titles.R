# Round 13 of the figure SPEC coverage: a plotmath title
# (expression(), quote(), bquote()), a scale's labels by a function's
# name (abs, scales::percent), stat_qq()'s distribution as a function, and
# a summary of the data in a mapping (a Bland-Altman plot's lines).

ti_item <- function(txt) .fig_lang_item(str2lang(txt))
ti_code <- function(it) .fig_deparse(.fig_item_lang(it))
ti_problems <- function(fn, args) tfl_schema_check(tfl_item(fn, args))

test_that("a plotmath title reads as {fn: expression, args: {expr:}} and is written back", {
  it <- ti_item('labs(x = expression(paste("Dose (", mu, "g)")), y = quote(log[10](AUC)), title = "PK")')
  expect_s3_class(it$args$x, "tfl_call")
  expect_identical(it$args$x$fn, "expression")
  expect_identical(it$args$x$args$expr, 'paste("Dose (", mu, "g)")')
  expect_identical(it$args$y$fn, "quote")
  expect_identical(it$args$y$args$expr, "log[10](AUC)")
  expect_identical(it$args$title, "PK")
  expect_identical(ti_code(it),
                   'labs(x = expression(paste("Dose (", mu, "g)")), y = quote(log[10](AUC)), title = "PK")')
  # a scale's name, a guide's title, sec_axis()'s name, xlab() / ggtitle()
  it <- ti_item("scale_y_log10(name = expression(C[max] ~ (ng/mL)))")
  expect_identical(it$args$name$args$expr, "C[max] ~ (ng/mL)")
  expect_identical(ti_code(it), "scale_y_log10(name = expression(C[max] ~ (ng/mL)))")
  expect_identical(ti_item("ylab(bquote(AUC[0 - .(24)]))")$args$label$fn, "bquote")
  expect_identical(ti_item("ggtitle(expression(alpha))")$args$label$args$expr, "alpha")
  g <- ti_item("guides(colour = guide_legend(title = expression(beta)))")
  expect_identical(g$args$colour$args$title$fn, "expression")
  # written from a SPEC: the expression bare
  expect_identical(ti_code(tfl_item("labs", list(y = tfl_call("expression", list(expr = "AUC[0-24]"))))),
                   "labs(y = expression(AUC[0 - 24]))")
  # a text in expression() is no plotmath expression: R, as before
  expect_s3_class(ti_item('labs(x = expression("Dose"))')$args$x, "tfl_r")
})

test_that("a plotmath title is checked and draws as the original", {
  ok <- ti_problems("labs", list(y = tfl_call("expression", list(expr = "AUC[0-24]~(ng %.% h/mL)"))))
  expect_identical(nrow(ok), 0L)
  p <- ti_problems("labs", list(y = tfl_call("expression", list(expr = "AUC[0-24"))))
  expect_identical(p$level, "error")
  expect_match(p$message, "expression() takes one argument, expr", fixed = TRUE)
  p <- ti_problems("labs", list(y = tfl_call("quote", list(x = "AUC"))))
  expect_identical(p$level, "error")
  skip_if_not_installed("ggplot2")
  df <- data.frame(x = 1:3, y = 1:3)
  it <- tfl_item("labs", list(x = tfl_call("expression", list(expr = "paste(\"Dose (\", mu, \"g)\")"))))
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + eval(envir = asNamespace("ggplot2"), .fig_item_lang(it))
  expect_true(is.expression(p$labels$x %||% ggplot2::get_labs(p)$x))
})

test_that("a scale's labels take a function by its name, written bare", {
  it <- ti_item("scale_y_continuous(labels = abs, breaks = seq(-30, 30, by = 10))")
  expect_identical(it$args$labels, "abs")
  expect_identical(ti_code(it), "scale_y_continuous(labels = abs, breaks = seq(-30, 30, by = 10))")
  it <- ti_item("scale_y_continuous(labels = scales::percent)")
  expect_identical(it$args$labels, "scales::percent")
  expect_identical(ti_code(it), "scale_y_continuous(labels = scales::percent)")
  # scales' by the bare name: written scales::, the package not attached
  expect_identical(ti_code(tfl_item("scale_x_continuous", list(labels = "comma"))),
                   "scale_x_continuous(labels = scales::comma)")
  # a sec_axis()'s labels too
  expect_identical(ti_code(tfl_item("sec_axis", list(transform = "~ . * 7", labels = "abs"))),
                   "sec_axis(transform = ~. * 7, labels = abs)")
  # a single label that is no such function stays a label
  expect_identical(ti_code(tfl_item("scale_x_discrete", list(labels = "Placebo"))),
                   'scale_x_discrete(labels = "Placebo")')
  expect_identical(nrow(ti_problems("scale_y_continuous", list(labels = "abs"))), 0L)
  skip_if_not_installed("scales")
  df <- data.frame(x = c("a", "b"), y = c(-20, 20))
  sc <- tfl_item("scale_y_continuous", list(labels = "abs", breaks = c(-20, 0, 20)))
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_col() +
    eval(envir = asNamespace("ggplot2"), .fig_item_lang(sc))
  b <- ggplot2::ggplot_build(p)
  expect_equal(as.numeric(b$layout$panel_params[[1L]]$y$get_labels()), c(20, 0, 20))
})

test_that("stat_qq()'s distribution is a function by its name, written bare", {
  it <- ti_item("stat_qq(distribution = qt, dparams = list(df = 5))")
  expect_identical(it$args$distribution, "qt")
  expect_identical(ti_code(it), "stat_qq(distribution = qt, dparams = list(df = 5))")
  expect_identical(ti_item("stat_qq_line(distribution = stats::qexp)")$args$distribution, "stats::qexp")
  # a SPEC's distribution: qnorm is the function (it was the text "qnorm",
  # which ggplot2 cannot call)
  expect_identical(ti_code(tfl_item("geom_qq", list(distribution = "qnorm"))), "geom_qq(distribution = qnorm)")
  expect_identical(nrow(ti_problems("stat_qq", list(distribution = "qnorm"))), 0L)
  expect_identical(ti_problems("stat_qq", list(distribution = "q norm"))$level, "error")
  df <- data.frame(s = stats::qnorm(seq(0.05, 0.95, length.out = 19)))
  p <- ggplot2::ggplot(df, ggplot2::aes(sample = s)) +
    eval(envir = asNamespace("ggplot2"),
         .fig_item_lang(tfl_item("stat_qq", list(distribution = "qt", dparams = list(df = 5)))))
  b <- ggplot2::ggplot_build(p)
  expect_equal(range(b$data[[1L]]$theoretical), stats::qt(c(0.025, 0.975), 5), tolerance = 0.2)
})

test_that("a mapping takes a summary of the data (a Bland-Altman plot's lines)", {
  it <- ti_item("geom_hline(aes(yintercept = mean(m2 - m1) + 1.96 * sd(m2 - m1)), linetype = 'dashed')")
  expect_identical(it$args$mapping$yintercept, "mean(m2 - m1) + 1.96 * sd(m2 - m1)")
  it <- ti_item("geom_hline(aes(yintercept = median(AVAL)))")
  expect_identical(it$args$mapping$yintercept, "median(AVAL)")
  expect_identical(ti_code(it), "geom_hline(mapping = aes(yintercept = median(AVAL)))")
  expect_identical(nrow(ti_problems("geom_hline", list(mapping = list(yintercept = "mean(AVAL - BASE)")))), 0L)
})
