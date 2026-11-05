# ggplot2's code as a SPEC and back (R/fig_content.R's reader and writer,
# R/schema.R's check): what ggplot2's own examples write, read as typed
# items, checked without a false problem, and written as code that draws
# the same (dev/spec-coverage-ggplot2-examples.R measures it over every
# example).

rt_item <- function(txt) .fig_lang_item(str2lang(txt))
rt_code <- function(txt) .fig_deparse(.fig_item_lang(rt_item(txt)))
rt_problems <- function(fn, args) tfl_schema_check(tfl_item(fn, args))

test_that("a layer's argument by ggplot2's other spelling, key_glyph, a position's nudge: no false warning", {
  expect_identical(nrow(rt_problems("geom_density", list(color = "red"))), 0L)
  expect_identical(nrow(rt_problems("geom_point", list(pch = 21, cex = 2))), 0L)
  expect_identical(nrow(rt_problems("stat_function", list(fun = tfl_r("dnorm"), color = "blue"))), 0L)
  # the spelling is typed as the aesthetic it spells
  expect_identical(rt_problems("geom_point", list(color = 1:2))$arg, "color")
  expect_identical(nrow(rt_problems("geom_line", list(key_glyph = "timeseries"))), 0L)
  expect_identical(nrow(rt_problems("geom_text", list(key_glyph = "point"))), 0L)
  expect_false(is.null(tfl_schema("geom_col")$args$key_glyph$label))
  skip_if_not(utils::packageVersion("ggplot2") >= "4.0.0")
  # ggplot2 4.0: geom_text()'s nudge goes to its position, as an argument
  # or an aesthetic; layer(layout = )
  expect_identical(nrow(rt_problems("geom_text", list(nudge_x = 0.05, hjust = 0))), 0L)
  expect_identical(nrow(rt_problems("geom_label", list(nudge_y = -0.1))), 0L)
  expect_identical(nrow(rt_problems("geom_text", list(mapping = list(label = "y", nudge_y = "dy")))), 0L)
  expect_identical(nrow(rt_problems("geom_point", list(layout = "fixed"))), 0L)
  # a misspelt one still warns
  expect_match(rt_problems("geom_text", list(nudge_z = 1))$message, "no argument `nudge_z`")
})

test_that("theme()'s default palettes (ggplot2 4.0) are its settings, typed", {
  skip_if_not(utils::packageVersion("ggplot2") >= "4.0.0")
  expect_identical(nrow(rt_problems("theme", list(
    palette.colour.discrete = c("#1B9E77", "#D95F02"),
    palette.fill.continuous = c("#DEEBF7", "#3182BD"),
    palette.shape.discrete = tfl_r("scales::pal_shape()")))), 0L)
  ent <- tfl_schema("theme")
  expect_identical(ent$args$palette.colour.discrete$label$en, "Default colour palette (discrete)")
  expect_identical(rt_problems("theme", list(palette.colour.discrete = tfl_call("element_text", list())))$level,
                   "error")
})

test_that("a number written as arithmetic of constants is a number; anything else is not", {
  expect_identical(.schema_r_number(tfl_r("1/5")), 0.2)
  expect_equal(.schema_r_number(tfl_r("-0.4 * pi")), -0.4 * pi)
  expect_identical(.schema_r_number(tfl_r("2^(1 + 1)")), 4)
  expect_null(.schema_r_number(tfl_r("x / 5")))
  expect_null(.schema_r_number(tfl_r("sqrt(2)")))
  expect_null(.schema_r_number(tfl_r("c(1, 2)")))
  expect_identical(nrow(rt_problems("geom_density", list(adjust = tfl_r("1/5")))), 0L)
  expect_identical(nrow(rt_problems("coord_fixed", list(ratio = tfl_r("1/5")))), 0L)
  expect_identical(nrow(rt_problems("geom_rug", list(alpha = tfl_r("1/2")))), 0L)
  expect_identical(rt_problems("geom_rug", list(alpha = tfl_r("a / 2")))$level, "error")
  # an integer is a whole number
  expect_identical(nrow(rt_problems("guide_legend", list(nrow = tfl_r("4/2")))), 0L)
  expect_identical(rt_problems("guide_legend", list(nrow = tfl_r("1/2")))$level, "error")
})

test_that("the vector forms ggplot2 takes: annotate()'s values, a 2-d bin width, expand, a palette, colours", {
  expect_identical(nrow(rt_problems("annotate", list(geom = "text", x = c(2, 3), y = c(20, 21),
                                                     label = c("a", "b")))), 0L)
  expect_identical(nrow(rt_problems("geom_bin_2d", list(binwidth = c(0.1, 0.1)))), 0L)
  expect_identical(nrow(rt_problems("scale_y_continuous", list(expand = c(0.1, 0.1)))), 0L)
  expect_identical(nrow(rt_problems("scale_x_continuous", list(expand = c(0, 0)))), 0L)
  expect_identical(nrow(rt_problems("scale_colour_gradientn", list(colours = tfl_r("terrain.colors(10)")))), 0L)
  expect_identical(nrow(rt_problems("geom_point", list(colour = tfl_r("alpha(\"blue\", 0.5)"),
                                                       data = tfl_r("subset(adsl, SAFFL == \"Y\")")))), 0L)
  skip_if_not(utils::packageVersion("ggplot2") >= "4.0.0")
  expect_identical(nrow(rt_problems("scale_colour_continuous", list(palette = c("#FEE0D2", "#DE2D26")))), 0L)
})

test_that("ggplot2's code reads as typed items: aes() by position, facets as columns, constructors as calls", {
  # aes(displ, hwy) is x and y
  it <- rt_item("geom_point(aes(displ, hwy))")
  expect_identical(it$args$mapping, list(x = "displ", y = "hwy"))
  expect_identical(rt_code("geom_point(aes(displ, hwy, colour = class))"),
                   "geom_point(mapping = aes(x = displ, y = hwy, colour = class))")
  expect_identical(nrow(tfl_schema_check(it)), 0L)
  # facets
  expect_identical(rt_item("facet_wrap(~PARAMCD)")$args$facets, "PARAMCD")
  expect_identical(rt_item("facet_wrap(vars(PARAMCD, AVISIT), ncol = 2)")$args$facets, list("PARAMCD", "AVISIT"))
  expect_identical(rt_item("facet_wrap(~ PARAMCD + AVISIT)")$args$facets, list("PARAMCD", "AVISIT"))
  g <- rt_item("facet_grid(SEX ~ TRTA, scales = \"free_y\")")
  expect_identical(g$args, list(rows = "SEX", cols = "TRTA", scales = "free_y"))
  expect_identical(nrow(tfl_schema_check(g)), 0L)
  expect_identical(rt_item("facet_grid(. ~ TRTA)")$args, list(cols = "TRTA"))
  expect_identical(rt_item("facet_grid(cols = vars(TRTA))")$args, list(cols = "TRTA"))
  expect_identical(rt_code("facet_grid(SEX ~ TRTA + AGEGR1)"),
                   "facet_grid(rows = vars(SEX), cols = vars(TRTA, AGEGR1))")
  # an expression of a column stays R
  expect_s3_class(rt_item("facet_wrap(vars(AGE > 65))")$args$facets, "tfl_r")
  expect_s3_class(rt_item("facet_grid(rows = vars(cut(AGE, 3)))")$args$rows, "tfl_r")
  # a constructor with positional or nested arguments: a call, written as before
  th <- rt_item("theme(axis.text = element_text(size = rel(1.5)), legend.key.size = unit(0.5, \"cm\"))")
  expect_s3_class(th$args$axis.text, "tfl_call")
  expect_identical(th$args$axis.text$args$size, tfl_call("rel", list(x = 1.5)))
  expect_identical(th$args$legend.key.size, tfl_call("unit", list(x = 0.5, units = "cm")))
  expect_identical(.fig_deparse(.fig_item_lang(th)),
                   "theme(axis.text = element_text(size = rel(1.5)), legend.key.size = unit(0.5, \"cm\"))")
  # (named as given when not in unit()'s order)
  expect_identical(.fig_deparse(.fig_item_lang(tfl_item("theme", list(
    legend.key.size = tfl_call("unit", list(units = "cm", x = 1)))))),
    "theme(legend.key.size = unit(units = \"cm\", x = 1))")
  sec <- rt_item("scale_y_continuous(sec.axis = sec_axis(~ . * 2, name = \"Twice\"))")
  expect_identical(sec$args$sec.axis$fn, "sec_axis")
  # (a formula as its text, the type tilde)
  expect_identical(sec$args$sec.axis$args$transform, "~. * 2")
  expect_identical(rt_item("guides(x = guide_axis_logticks(long = 2))")$args$x,
                   tfl_call("guide_axis_logticks", list(long = 2)))
  expect_identical(rt_item("guides(fill = guide_colourbar(theme = theme(legend.direction = \"horizontal\")))")$args$fill,
                   tfl_call("guide_colourbar", list(theme = tfl_call("theme", list(legend.direction = "horizontal")))))
  # a name in a constructor stays R in it
  d <- rt_item("geom_col(position = position_dodge(width = w))")$args$position
  expect_identical(d, tfl_call("position_dodge", list(width = tfl_r("w"))))
  # vars() and another package's function are no constructors of a value,
  # but a call of scales the argument's type names (label_*)
  expect_identical(rt_item("scale_x_continuous(labels = scales::label_percent())")$args$labels,
                   tfl_call("scales::label_percent", list()))
  expect_s3_class(rt_item("scale_x_continuous(labels = mypkg::lab())")$args$labels, "tfl_r")
  # a named vector of constants as a map; 1:5 as the numbers
  v <- rt_item("scale_colour_manual(values = c(Placebo = \"grey50\", Drug = \"red\"))")$args$values
  expect_identical(v, list(Placebo = "grey50", Drug = "red"))
  expect_identical(rt_code("scale_colour_manual(values = c(Placebo = \"grey50\", Drug = \"red\"))"),
                   "scale_colour_manual(values = c(Placebo = \"grey50\", Drug = \"red\"))")
  expect_identical(rt_item("scale_shape_manual(values = 21:25)")$args$values, c(21, 22, 23, 24, 25))
  expect_s3_class(rt_item("scale_shape_manual(values = 1:n)")$args$values, "tfl_r")
  # (fun.args and the other list() arguments stay a list)
  expect_identical(rt_code("stat_summary(fun.args = list(mult = 1))"), "stat_summary(fun.args = list(mult = 1))")
})

test_that("R of several lines is kept on its lines; margin_auto() is a margin", {
  it <- rt_item("stat_summary(fun = function(y) {\n  m <- mean(y)\n  m\n})")
  txt <- as.character(unclass(it$args$fun))
  expect_match(txt, "\n", fixed = TRUE)
  back <- as.list(.fig_item_lang(it))$fun
  expect_identical(deparse(back), deparse(str2lang("function(y) {\n  m <- mean(y)\n  m\n}")))
  # one line when it is the same R
  expect_identical(as.character(unclass(rt_item("stat_summary(fun = function(y) mean(y))")$args$fun)),
                   "function(y) mean(y)")
  skip_if_not(utils::packageVersion("ggplot2") >= "4.0.0")
  expect_identical(nrow(rt_problems("theme", list(plot.margin = tfl_call("margin_auto", list(t = 2, unit = "cm"))))), 0L)
  expect_identical(nrow(rt_problems("element_text", list(margin = tfl_call("margin_part", list(r = 4))))), 0L)
})

test_that("the code read and written back draws the same as ggplot2's", {
  df <- data.frame(x = rep(1:4, 2), y = c(1, 3, 2, 5, 2, 4, 3, 6), g = rep(c("A", "B"), each = 4),
                   f = rep(c("u", "v"), 4))
  terms <- c("geom_point(aes(x, y, colour = g), pch = 21, color = \"black\")",
             "geom_line(aes(x, y, group = g), key_glyph = \"timeseries\")",
             "facet_grid(f ~ g)",
             "facet_wrap(~ g + f)",
             "scale_colour_manual(values = c(A = \"grey50\", B = \"red\"))",
             "scale_y_continuous(expand = c(0.1, 0.1), sec.axis = sec_axis(~ . * 2))",
             "theme(axis.text = element_text(size = rel(1.5)), legend.key.size = unit(0.5, \"cm\"))",
             "guides(colour = guide_legend(theme = theme(legend.direction = \"horizontal\")))",
             "annotate(\"text\", x = 2:3, y = c(5, 6), label = c(\"a\", \"b\"))",
             "coord_fixed(ratio = 1/5)")
  built <- function(p) {
    b <- ggplot2::ggplot_build(p)
    list(b$data, b$layout$layout, p$theme, p$labels)
  }
  base <- ggplot2::ggplot(df, ggplot2::aes(x, y, colour = g)) + ggplot2::geom_blank()
  for (t in terms) {
    it <- rt_item(t)
    expect_false(identical(it$fn, ".r"), info = t)
    expect_identical(nrow(tfl_schema_check(it)), 0L, info = t)
    orig <- base + eval(str2lang(t), asNamespace("ggplot2"))
    back <- base + eval(.fig_item_lang(it), asNamespace("ggplot2"))
    expect_true(isTRUE(all.equal(built(orig), built(back), check.environment = FALSE)), info = t)
  }
})

test_that("a column that is not syntactic is read and written back as ggplot2's own: same code, same plot", {
  skip_if_not_installed("ggplot2")
  d <- data.frame(`A-B` = c(1, 2, 3), `N/A` = c(2, 3, 5), `Grade >= 3` = c("y", "n", "y"),
                  `p-value` = c(0.01, 0.2, 0.5), PCHG = c(-45, 10, -31), check.names = FALSE)
  terms <- c("geom_point(mapping = aes(x = `A-B`, y = `N/A`))",
             "geom_point(mapping = aes(colour = `Grade >= 3`))",
             "geom_point(mapping = aes(x = `p-value`))",
             "geom_point(mapping = aes(colour = PCHG <= -30))")
  for (t in terms) {
    expect_identical(rt_code(t), t)
    expect_identical(nrow(tfl_schema_check(rt_item(t))), 0L, info = t)
    orig <- ggplot2::layer_data(ggplot2::ggplot(d, ggplot2::aes(PCHG, PCHG)) +
                                  eval(str2lang(t), asNamespace("ggplot2")))
    back <- ggplot2::layer_data(ggplot2::ggplot(d, ggplot2::aes(PCHG, PCHG)) +
                                  eval(.fig_item_lang(rt_item(t)), asNamespace("ggplot2")))
    expect_equal(back, orig, info = t)
  }
  # the mapping's text of the first, and of a column that is syntactic
  expect_identical(rt_item(terms[1L])$args$mapping, list(x = "`A-B`", y = "`N/A`"))
  expect_identical(rt_item("geom_point(aes(x = PCHG))")$args$mapping, list(x = "PCHG"))
})

test_that("facet_wrap() on a column that is not syntactic is written as vars(), and draws", {
  skip_if_not_installed("ggplot2")
  d <- data.frame(x = 1:4, y = c(2, 3, 5, 4), g = c("a", "a", "b", "b"), f = c("u", "v", "u", "v"),
                  `A b` = c("a", "a", "b", "b"), `A-B` = c("p", "q", "p", "q"), check.names = FALSE)
  base <- ggplot2::ggplot(d, ggplot2::aes(x, y)) + ggplot2::geom_point()
  panels <- function(code) {
    nrow(ggplot2::ggplot_build(base + eval(code, asNamespace("ggplot2")))$layout$layout)
  }
  # ggplot2's code, read as the SPEC's text and written back: the column in vars(), as facet_grid()'s
  cases <- list("facet_wrap(~`A b`)" = c("`A b`", "facet_wrap(facets = vars(`A b`))", 2L),
                "facet_wrap(~`A-B`)" = c("`A-B`", "facet_wrap(facets = vars(`A-B`))", 2L),
                "facet_wrap(vars(`A b`), ncol = 1)" = c("`A b`", "facet_wrap(facets = vars(`A b`), ncol = 1)", 2L))
  for (t in names(cases)) {
    it <- rt_item(t)
    expect_identical(it$args$facets, cases[[t]][1L], info = t)
    expect_identical(nrow(tfl_schema_check(it)), 0L, info = t)
    expect_identical(rt_code(t), cases[[t]][2L], info = t)
    expect_identical(panels(.fig_item_lang(it)), as.integer(cases[[t]][3L]), info = t)
    expect_identical(panels(.fig_item_lang(it)), panels(str2lang(t)), info = t)
  }
  # several columns, one of them not syntactic
  expect_identical(rt_item("facet_wrap(~ `A b` + g)")$args$facets, list("`A b`", "g"))
  expect_identical(rt_code("facet_wrap(~ `A b` + g)"), "facet_wrap(facets = vars(`A b`, g))")
  expect_identical(panels(.fig_item_lang(rt_item("facet_wrap(~ `A b` + f)"))), 4L)
  # a hand-written text draws: "A b" does not parse, it is the column
  for (v in list("A b", list("A b", "f"), "`A b`", "`A-B`")) {
    it <- tfl_item("facet_wrap", list(facets = v))
    expect_identical(nrow(tfl_schema_check(it)), 0L, info = deparse(v))
    expect_match(.fig_deparse(.fig_item_lang(it)), "^facet_wrap\\(facets = vars\\(", info = deparse(v))
    expect_gte(panels(.fig_item_lang(it)), 2L)
  }
  expect_identical(.fig_deparse(.fig_item_lang(tfl_item("facet_wrap", list(facets = "A b")))),
                   "facet_wrap(facets = vars(`A b`))")
  # syntactic columns are written as before
  expect_identical(rt_code("facet_wrap(~g)"), "facet_wrap(facets = \"g\")")
  expect_identical(rt_code("facet_wrap(~ g + f)"), "facet_wrap(facets = c(\"g\", \"f\"))")
  expect_identical(rt_code("facet_wrap(vars(g, f), ncol = 2)"), "facet_wrap(facets = c(\"g\", \"f\"), ncol = 2)")
  expect_identical(.fig_deparse(.fig_item_lang(tfl_item("facet_wrap", list(facets = "`g`")))),
                   "facet_wrap(facets = \"`g`\")")
  # a formula text and an expression text are as before
  expect_identical(.fig_deparse(.fig_item_lang(tfl_item("facet_wrap", list(facets = "~g + f")))),
                   "facet_wrap(facets = \"~g + f\")")
  expect_identical(.fig_deparse(.fig_item_lang(tfl_item("facet_wrap", list(facets = "toupper(g)")))),
                   "facet_wrap(facets = \"toupper(g)\")")
  # facet_grid is as it was
  expect_identical(rt_code("facet_grid(`A b` ~ g)"), "facet_grid(rows = vars(`A b`), cols = vars(g))")
})
