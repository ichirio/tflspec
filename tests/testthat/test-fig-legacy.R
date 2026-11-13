# Round 15 of the figure SPEC coverage: ggplot2's spellings of before 3.4 /
# 3.5 in the code a SPEC is read from, read as ggplot2 4.0 writes them
# (R/fig_legacy.R), where ggplot2 takes the old spelling as the new one.

lg_item <- function(txt) .fig_lang_item(str2lang(txt))
lg_code <- function(txt) .fig_deparse(.fig_item_lang(lg_item(txt)))
lg_ok <- function(txt) expect_identical(nrow(tfl_schema_check(lg_item(txt))), 0L)

# the layers' data of a figure drawn by the code
lg_draw <- function(p) {
  suppressWarnings(suppressMessages(lapply(ggplot2::ggplot_build(p)$data, function(d) {
    d[, setdiff(names(d), c("width", "flipped_aes")), drop = FALSE]
  })))
}

# the legend's widths and heights, in cm
lg_legend_cm <- function(p) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  g <- ggplot2::ggplotGrob(p)
  box <- g$grobs[[which(grepl("^guide-box", g$layout$name) & vapply(g$grobs, inherits, NA, "gtable"))[1L]]]
  leg <- box$grobs[[which(box$layout$name == "guides")[1L]]]
  list(w = grid::convertWidth(leg$widths, "cm", valueOnly = TRUE),
       h = grid::convertHeight(leg$heights, "cm", valueOnly = TRUE))
}

test_that("a line's size is its linewidth where ggplot2 takes it so", {
  expect_identical(lg_code("geom_line(size = 1)"), "geom_line(linewidth = 1)")
  expect_identical(lg_code("geom_hline(yintercept = 0, size = 0.5)"),
                   "geom_hline(yintercept = 0, linewidth = 0.5)")
  expect_identical(lg_code("geom_errorbar(aes(ymin = l, ymax = u), size = 0.4)"),
                   "geom_errorbar(mapping = aes(ymin = l, ymax = u), linewidth = 0.4)")
  # (a box plot's size is an aesthetic of its own too: as written)
  expect_identical(lg_code("geom_boxplot(size = 0.4, outlier.size = 1)"),
                   "geom_boxplot(size = 0.4, outlier.size = 1)")
  # a stat's geom by name: a line's width, a point's size
  expect_identical(lg_code('stat_summary(fun = mean, geom = "line", size = 1)'),
                   'stat_summary(fun = mean, geom = "line", linewidth = 1)')
  expect_identical(lg_code('stat_summary(fun = mean, geom = "point", size = 3)'),
                   'stat_summary(fun = mean, geom = "point", size = 3)')
  expect_identical(lg_code("stat_ecdf(size = 0.7)"), "stat_ecdf(linewidth = 0.7)")
  # a point's and a text's size, and a linewidth given too: as written
  expect_identical(lg_code("geom_point(size = 2)"), "geom_point(size = 2)")
  expect_identical(lg_code("geom_text(size = 3)"), "geom_text(size = 3)")
  expect_identical(lg_code("geom_line(size = 1, linewidth = 2)"), "geom_line(size = 1, linewidth = 2)")
  expect_identical(lg_code("geom_line(aes(linewidth = w), size = 1)"),
                   "geom_line(mapping = aes(linewidth = w), size = 1)")
  # a bar's size ggplot2 4.0 drops: as written, the check says what it is
  expect_identical(lg_code("geom_bar(size = 1)"), "geom_bar(size = 1)")
  p <- tfl_schema_check(tfl_item("geom_bar", list(size = 1)))
  expect_match(p$message, "outline width: linewidth")
  p <- tfl_schema_check(tfl_item("geom_line", list(size = 1)))
  expect_match(p$message, "linewidth since ggplot2 3.4.0")
  expect_identical(p$level, "warning")
  # an element's line
  expect_identical(lg_code("theme(panel.border = element_rect(size = 1, fill = NA), axis.line = element_line(size = 0.3))"),
                   "theme(panel.border = element_rect(linewidth = 1, fill = NA), axis.line = element_line(linewidth = 0.3))")
  lg_ok("theme(panel.border = element_rect(size = 1, fill = NA))")
})

test_that("stat_summary()'s fun.y, fun.ymin and fun.ymax are fun, fun.min and fun.max", {
  txt <- 'stat_summary(fun.y = mean, fun.ymin = min, fun.ymax = max, geom = "pointrange")'
  expect_identical(lg_code(txt), 'stat_summary(fun = mean, fun.min = min, fun.max = max, geom = "pointrange")')
  lg_ok(txt)
  expect_identical(lg_code("stat_summary_bin(fun.y = mean)"), 'stat_summary_bin(fun = mean)')
  # fun given too: as written
  expect_identical(lg_code("stat_summary(fun.y = mean, fun = median)"),
                   'stat_summary(fun.y = mean, fun = median)')
})

test_that("a stat's variable by ..count.. or stat() is after_stat() of the value", {
  it <- lg_item("geom_bar(aes(y = ..count.. / sum(..count..)))")
  expect_identical(it$args$mapping$y, "after_stat(count/sum(count))")
  expect_identical(lg_code("geom_bar(aes(y = stat(count)))"), "geom_bar(mapping = aes(y = after_stat(count)))")
  expect_identical(lg_code("geom_text(aes(label = ..count..), stat = \"count\")"),
                   "geom_text(mapping = aes(label = after_stat(count)), stat = \"count\")")
  expect_identical(lg_code("geom_density(aes(y = after_stat(..density..)))"),
                   "geom_density(mapping = aes(y = after_stat(density)))")
  # a column of the data: as it is
  expect_identical(lg_code("geom_point(aes(y = count))"), "geom_point(mapping = aes(y = count))")
  # an empty argument in a value (a subset) is left alone
  expect_identical(lg_code("geom_point(data = km[km$censored, ], aes(y = ..count..))"),
                   "geom_point(data = km[km$censored, ], mapping = aes(y = after_stat(count)))")
})

test_that("aes_string() of texts is aes()", {
  expect_identical(lg_code('ggplot(df, aes_string("AVISITN", y = "AVAL", colour = "TRTA"))'),
                   "ggplot(data = df, mapping = aes(x = AVISITN, y = AVAL, colour = TRTA))")
  expect_identical(lg_code('geom_col(aes_string(x = "reorder(USUBJID, -PCHG)", y = "PCHG"))'),
                   "geom_col(mapping = aes(x = reorder(USUBJID, -PCHG), y = PCHG))")
  # an object's value: as written
  expect_identical(lg_code("geom_point(aes_string(x = xvar))"), "geom_point(mapping = aes_string(x = xvar))")
})

test_that("geom_errorbarh() is geom_errorbar(orientation = \"y\") and draws the same", {
  txt <- "geom_errorbarh(aes(xmin = l, xmax = u), height = 0.2, size = 0.6)"
  expect_identical(lg_code(txt),
                   'geom_errorbar(mapping = aes(xmin = l, xmax = u), width = 0.2, linewidth = 0.6, orientation = "y")')
  lg_ok(txt)
  d <- data.frame(y = c("a", "b"), l = 1:2, u = 3:4)
  old <- ggplot2::ggplot(d, ggplot2::aes(y = y)) + suppressWarnings(eval(str2lang(txt), asNamespace("ggplot2")))
  new <- ggplot2::ggplot(d, ggplot2::aes(y = y)) + eval(.fig_item_lang(lg_item(txt)), asNamespace("ggplot2"))
  expect_equal(lg_draw(new), lg_draw(old))
  # tfl_args_from_r() keeps the function asked for
  a <- tfl_args_from_r("geom_errorbarh", "height = 0.2, size = 0.6")
  expect_identical(a$height, 0.2)
  expect_identical(a$linewidth, 0.6)
})

test_that("a guide's arguments of before 3.5.0 are its theme", {
  txt <- 'guides(fill = guide_legend(title.position = "top", nrow = 2, keywidth = 2, label.hjust = 0.5))'
  expect_identical(lg_code(txt), paste0(
    'guides(fill = guide_legend(theme = theme(legend.title.position = "top", ',
    'legend.key.width = unit(2, "lines"), legend.text = element_text(hjust = 0.5)), nrow = 2))'))
  lg_ok(txt)
  # the theme given too: its own settings win, the others are added to it
  txt <- paste0('guides(colour = guide_colourbar(barheight = unit(5, "cm"), title.hjust = 0.5, ',
                'title.position = "top", theme = theme(legend.title.position = "left")))')
  expect_identical(lg_code(txt), paste0(
    'guides(colour = guide_colourbar(theme = theme(legend.title.position = "left", ',
    'legend.key.height = unit(5, "cm"), legend.title = element_text(hjust = 0.5))))'))
  # default.unit, byrow, label = FALSE, a frame
  expect_identical(lg_code('guide_legend(keyheight = 1, default.unit = "cm", byrow = TRUE, label = FALSE)'),
                   paste0('guide_legend(theme = theme(legend.byrow = TRUE, legend.key.height = unit(1, "cm"), ',
                          "legend.text = element_blank()))"))
  expect_identical(lg_code('guide_colourbar(frame.colour = "black", frame.linewidth = 0.5)'),
                   'guide_colourbar(theme = theme(legend.frame = element_rect(colour = "black", linewidth = 0.5)))')
  # in a scale's guide
  expect_identical(lg_code("scale_fill_brewer(guide = guide_legend(reverse = TRUE, label.position = \"left\"))"),
                   paste0('scale_fill_brewer(guide = guide_legend(reverse = TRUE, ',
                          'theme = theme(legend.text.position = "left")))'))
  # what ggplot2 does not set the same (ticks = FALSE), an object where a
  # number goes: as written
  expect_identical(lg_code("guide_colourbar(ticks = FALSE)"), "guide_colourbar(ticks = FALSE)")
  expect_identical(lg_code("guide_legend(keywidth = w)"), "guide_legend(keywidth = w)")
  # drawn the same
  d <- data.frame(x = 1:3, y = 1:3, g = c("a", "b", "c"))
  base <- ggplot2::ggplot(d, ggplot2::aes(x, y, colour = g)) + ggplot2::geom_point()
  txt <- 'guides(colour = guide_legend(title.position = "top", keywidth = 3, title.hjust = 0.5, nrow = 1))'
  expect_equal(lg_legend_cm(base + eval(.fig_item_lang(lg_item(txt)), asNamespace("ggplot2"))),
               suppressWarnings(lg_legend_cm(base + eval(str2lang(txt), asNamespace("ggplot2")))))
})

test_that("guide = FALSE is \"none\"", {
  expect_identical(lg_code("guides(fill = FALSE, colour = guide_legend())"),
                   'guides(fill = "none", colour = guide_legend())')
  expect_identical(lg_code('scale_fill_manual(values = c("red", "blue"), guide = FALSE)'),
                   'scale_fill_manual(values = c("red", "blue"), guide = "none")')
  lg_ok("guides(fill = FALSE)")
})

test_that("theme(): a legend's place as numbers is inside; legend.title.align its hjust", {
  txt <- "theme(legend.position = c(0.8, 0.2), legend.justification = c(1, 0))"
  expect_identical(lg_code(txt), paste0('theme(legend.position = "inside", legend.position.inside = c(0.8, 0.2), ',
                                        "legend.justification = c(1, 0))"))
  lg_ok(txt)
  expect_identical(lg_code("theme(legend.title.align = 0.5, legend.text.align = 0, legend.text = element_text(size = 8))"),
                   "theme(legend.title = element_text(hjust = 0.5), legend.text = element_text(size = 8, hjust = 0))")
  # an element that is not element_text(), or has its hjust: as written
  expect_identical(lg_code("theme(legend.title.align = 0.5, legend.title = element_blank())"),
                   "theme(legend.title.align = 0.5, legend.title = element_blank())")
  p <- tfl_schema_check(tfl_item("theme", list(legend.title.align = 0.5)))
  expect_match(p$message, "legend.title = \\{fn: element_text")
})

test_that("expand_scale() is expansion()", {
  expect_identical(lg_code("scale_y_continuous(expand = expand_scale(mult = c(0, 0.1)))"),
                   "scale_y_continuous(expand = expansion(mult = c(0, 0.1)))")
})

test_that("an old program's figure draws the same read through a SPEC", {
  d <- data.frame(t = rep(1:5, 2), m = c(1, 3, 2, 4, 3, 2, 2, 3, 5, 4), g = rep(c("A", "B"), each = 5))
  terms <- c("geom_line(size = 0.8)",
             "stat_summary(fun.y = mean, geom = \"point\", size = 2)",
             "geom_hline(yintercept = 2, size = 0.3)",
             "scale_y_continuous(expand = expand_scale(mult = c(0, 0.1)))",
             "guides(colour = guide_legend(title.position = \"top\", keywidth = 2))",
             "theme(legend.position = c(0.8, 0.2), panel.border = element_rect(size = 1, fill = NA))")
  old <- new <- ggplot2::ggplot(d, ggplot2::aes(t, m, colour = g))
  for (t in terms) {
    old <- old + suppressWarnings(eval(str2lang(t), asNamespace("ggplot2")))
    new <- new + eval(.fig_item_lang(lg_item(t)), asNamespace("ggplot2"))
  }
  expect_equal(lg_draw(new), lg_draw(old))
  expect_identical(new$theme$legend.position, "inside")
  expect_identical(new$theme$legend.position.inside, c(0.8, 0.2))
})
