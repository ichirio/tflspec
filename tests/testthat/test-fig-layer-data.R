# Round 19 of the figure SPEC coverage: a layer's data as rows of a data by
# its name ({fn: subset, args: {x: km, subset: censored}}), a 2-d bin's
# one value an axis (bins = list(x = 30, y = 10)) and a facet's strips by
# label_bquote()'s plotmath expression.

ld_item <- function(txt) .fig_lang_item(str2lang(txt))
ld_code <- function(it) .fig_deparse(.fig_item_lang(it))
ld_problems <- function(fn, args) tfl_schema_check(tfl_item(fn, args))

test_that("a layer's rows of a data by name read as {fn: subset} and are written subset()", {
  for (txt in c("geom_point(data = km[km$censored, ], shape = 3)",
                "geom_point(data = subset(km, censored), shape = 3)",
                "geom_point(data = dplyr::filter(km, censored), shape = 3)")) {
    it <- ld_item(txt)
    expect_s3_class(it$args$data, "tfl_call")
    expect_identical(it$args$data$fn, "subset")
    expect_identical(it$args$data$args, list(x = "km", subset = "censored"))
    expect_identical(ld_code(it), "geom_point(data = subset(km, censored), shape = 3)")
  }
  # a condition of several columns; filter()'s conditions joined by &
  it <- ld_item('geom_segment(data = sw[sw$ongoing & sw$ARM == "A", ], aes(x = dur))')
  expect_identical(it$args$data$args$subset, 'ongoing & ARM == "A"')
  it <- ld_item("geom_text(data = filter(wf, PCHG < -30, BOR == \"PR\"), aes(label = USUBJID))")
  expect_identical(it$args$data$args, list(x = "wf", subset = 'PCHG < -30 & BOR == "PR"'))
  expect_identical(ld_code(it),
                   'geom_text(data = subset(wf, PCHG < -30 & BOR == "PR"), mapping = aes(label = USUBJID))')
  # not only the data's columns, or filter()'s own arguments: R, as before
  expect_identical(ld_item("geom_point(data = km[km$time > max(km$time) / 2, ])")$args$data$args$subset,
                   "time > max(time)/2")
  expect_s3_class(ld_item("geom_point(data = km[km$n > nrow(km) / 2, ])")$args$data, "tfl_r")
  expect_s3_class(ld_item("geom_point(data = filter(km, censored, .by = arm))")$args$data, "tfl_r")
  expect_s3_class(ld_item("geom_point(data = km[1:3, ])")$args$data, "tfl_r")
  expect_s3_class(ld_item("geom_point(data = km[order(km$time), ])")$args$data, "tfl_r")
  expect_s3_class(ld_item("geom_point(data = km[keep, ])")$args$data, "tfl_r")
  expect_identical(ld_item("geom_point(data = km[!is.na(km$cnsr), ])")$args$data$args$subset,
                   "!is.na(cnsr)")
  expect_s3_class(ld_item("geom_point(data = km[, 1:3])")$args$data, "tfl_r")
  # a data by name and the plot's data's formula stay as they were
  expect_identical(ld_item("geom_point(data = km)")$args$data, "km")
  expect_identical(ld_code(ld_item("geom_point(data = ~ subset(.x, censored))")),
                   "geom_point(data = ~subset(.x, censored))")
})

test_that("rows of a data by name are checked", {
  ok <- ld_problems("geom_point", list(data = tfl_call("subset", list(x = "km", subset = "censored"))))
  expect_identical(nrow(ok), 0L)
  p <- ld_problems("geom_point", list(data = tfl_call("subset", list(x = "km", subset = "censored &"))))
  expect_true("error" %in% p$level)
  expect_match(p$message[p$level == "error"][1L], "subset() takes the data by its name", fixed = TRUE)
  p <- ld_problems("geom_point", list(data = tfl_call("subset", list(x = "km", select = "time"))))
  expect_true("error" %in% p$level)
  p <- ld_problems("geom_point", list(data = tfl_call("subset", list(x = "a b", subset = "censored"))))
  expect_true("error" %in% p$level)
})

test_that("rows of a data by name draw as the original", {
  skip_if_not_installed("ggplot2")
  km <- data.frame(time = c(1, 2, 3, 4), surv = c(1, 0.9, 0.8, 0.7),
                   censored = c(FALSE, TRUE, NA, TRUE))
  it <- tfl_item("geom_point", list(data = tfl_call("subset", list(x = "km", subset = "censored"))))
  layer <- eval(.fig_item_lang(it), list(km = km), asNamespace("ggplot2"))
  p_spec <- ggplot2::ggplot(km, ggplot2::aes(time, surv)) + ggplot2::geom_step() + layer
  p_orig <- ggplot2::ggplot(km, ggplot2::aes(time, surv)) + ggplot2::geom_step() +
    ggplot2::geom_point(data = km[km$censored, ])
  d_spec <- ggplot2::layer_data(p_spec, 2L)
  d_orig <- suppressWarnings(ggplot2::layer_data(p_orig, 2L))
  d_orig <- d_orig[!is.na(d_orig$x), ]
  expect_equal(d_spec[, c("x", "y")], d_orig[, c("x", "y")], ignore_attr = TRUE)
})

test_that("a 2-d bin's one value an axis reads as a map, written list()", {
  it <- ld_item("geom_bin_2d(bins = list(x = 30, y = 10))")
  expect_identical(it$args$bins, list(x = 30, y = 10))
  expect_identical(ld_code(it), "geom_bin_2d(bins = list(x = 30, y = 10))")
  it <- ld_item("stat_summary_2d(aes(z = z), binwidth = list(x = 0.5, y = 2))")
  expect_identical(it$args$binwidth, list(x = 0.5, y = 2))
  expect_identical(nrow(ld_problems("geom_bin_2d", list(bins = list(x = 30L, y = 10L)))), 0L)
  expect_identical(nrow(ld_problems("geom_hex", list(binwidth = list(x = 0.5, y = 2)))), 0L)
  expect_identical(nrow(ld_problems("stat_bin_2d", list(bins = c(30L, 10L)))), 0L)
  expect_identical(ld_problems("geom_bin_2d", list(bins = list(x = "many")))$level, "error")
  # one number, as before; a 1-d bin is not by axis
  expect_identical(ld_code(ld_item("geom_bin_2d(bins = 20)")), "geom_bin_2d(bins = 20)")
  expect_s3_class(ld_item("geom_histogram(bins = list(x = 30))")$args$bins, "tfl_r")
  skip_if_not_installed("ggplot2")
  df <- data.frame(x = c(0, 10, 3, 7), y = c(0, 5, 1, 4))
  it <- tfl_item("geom_bin_2d", list(bins = list(y = 5, x = 2)))
  b <- ggplot2::layer_data(ggplot2::ggplot(df, ggplot2::aes(x, y)) +
                             eval(.fig_item_lang(it), envir = asNamespace("ggplot2")))
  o <- ggplot2::layer_data(ggplot2::ggplot(df, ggplot2::aes(x, y)) +
                             ggplot2::geom_bin_2d(bins = list(y = 5, x = 2)))
  expect_equal(b[, c("xmin", "ymin", "count")], o[, c("xmin", "ymin", "count")])
})

test_that("label_bquote()'s strips are a plotmath expression, written bare", {
  it <- ld_item("facet_grid(vs ~ ., labeller = label_bquote(alpha^.(vs)))")
  lb <- it$args$labeller
  expect_s3_class(lb, "tfl_call")
  expect_identical(lb$fn, "label_bquote")
  expect_identical(lb$args$rows, "alpha^.(vs)")
  expect_identical(ld_code(it), "facet_grid(rows = vars(vs), labeller = label_bquote(rows = alpha^.(vs)))")
  it <- ld_item('facet_grid(. ~ am, labeller = label_bquote(cols = .(am) ~ "transmission"))')
  expect_identical(it$args$labeller$args$cols, '.(am) ~ "transmission"')
  expect_identical(ld_code(it),
                   'facet_grid(cols = vars(am), labeller = label_bquote(cols = .(am) ~ "transmission"))')
  ok <- ld_problems("facet_grid", list(rows = "vs", labeller = tfl_call("label_bquote",
                                                                         list(rows = "alpha^.(vs)"))))
  expect_identical(nrow(ok), 0L)
  skip_if_not_installed("ggplot2")
  df <- data.frame(x = 1:4, y = 1:4, vs = c(0, 0, 1, 1))
  it <- tfl_item("facet_grid", list(rows = "vs", labeller = tfl_call("label_bquote",
                                                                     list(rows = "alpha^.(vs)"))))
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_point() +
    eval(.fig_item_lang(it), envir = asNamespace("ggplot2"))
  expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
  expect_no_error(ggplot2::ggplotGrob(p))
})
