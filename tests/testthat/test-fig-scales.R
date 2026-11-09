# scales' helpers a scale's value is made by (R/spec_figure.R's `scales`
# kind, inst/fig/gg_args.csv's `scales` rows), and the calls inside a
# figure item's values checked by their own entries (R/schema.R).

sc_problems <- function(fn, args) tfl_schema_check(tfl_item(fn, args))
sc_item <- function(txt) .fig_lang_item(str2lang(txt))
sc_code <- function(it) .fig_deparse(.fig_item_lang(it))
sc_has_r <- function(v) {
  if (inherits(v, "tfl_r")) return(TRUE)
  if (inherits(v, "tfl_call")) v <- v$args
  is.list(v) && any(vapply(v, sc_has_r, NA))
}

test_that("a scales helper is an entry of kind scales, its arguments typed and labelled", {
  skip_if_not_installed("scales")
  e <- tfl_schema("scales::label_percent")
  expect_identical(e$figure, "scales")
  expect_identical(e$package, "scales")
  expect_identical(e$args$accuracy$type, "number|r")
  expect_identical(e$args$accuracy$label$en, "Accuracy")
  # label_percent()'s `...` are label_number()'s (style_positive)
  expect_identical(e$args$style_positive$type, "enum[none|plus|space]")
  for (fn in c("label_number", "label_comma", "label_pvalue", "label_log", "label_math", "math_format",
               "label_date", "label_wrap", "breaks_width", "breaks_log", "breaks_pretty",
               "trans_breaks", "trans_format", "transform_log", "transform_pseudo_log",
               "minor_breaks_n", "pal_brewer", "rescale")) {
    e <- tfl_schema(paste0("scales::", fn))
    expect_identical(e$figure, "scales", info = fn)
    a <- setdiff(names(formals(getExportedValue("scales", fn))), "...")
    for (x in a) expect_false(is.null(e$args[[x]]$label), info = paste(fn, x))
  }
  # a unit of a time span is one of its names; label_number_si's any text
  expect_identical(tfl_schema("scales::breaks_timespan")$args$unit$type, "enum[secs|mins|hours|days|weeks]")
  expect_identical(tfl_schema("scales::label_number_si")$args$unit$type, "string")
  # a function of scales that makes no scale's value has no entry
  expect_null(tfl_schema("scales::alpha"))
  expect_null(tfl_schema("scales::show_col"))
})

test_that("the calls inside a figure item's values are checked by their own entries", {
  skip_if_not_installed("scales")
  pct <- function(a) list(labels = tfl_call("scales::label_percent", a))
  expect_identical(nrow(sc_problems("scale_y_continuous", pct(list(accuracy = 1, style_positive = "plus")))), 0L)
  p <- sc_problems("scale_y_continuous", pct(list(acuracy = 1)))
  expect_identical(p$level, "warning")
  expect_match(p$message, "label_percent has no argument `acuracy`")
  p <- sc_problems("scale_y_continuous", pct(list(accuracy = "one")))
  expect_identical(p$level, "error")
  expect_match(p$message, "label_percent\\(accuracy = \\)")
  p <- sc_problems("scale_y_continuous", list(labels = tfl_call("scales::label_procent", list())))
  expect_identical(p$level, "error")
  expect_match(p$message, "scales has no function label_procent()", fixed = TRUE)
  # breaks_pretty()'s `...` go to pretty(): any name
  expect_identical(nrow(sc_problems("scale_y_continuous", list(
    breaks = tfl_call("scales::breaks_pretty", list(n = 6, min.n = 3))))), 0L)
  # ggplot2's own constructors inside a value: theme()'s elements, sec_axis()
  p <- sc_problems("theme", list(axis.text = tfl_call("element_text", list(sizee = 9))))
  expect_match(p$message, "element_text has no argument `sizee`")
  p <- sc_problems("theme", list(axis.text = tfl_call("element_text", list(size = "big"))))
  expect_identical(p$level, "error")
  p <- sc_problems("scale_y_continuous", list(sec.axis = tfl_call("sec_axis", list(transform = "~ . / 7", nam = "W"))))
  expect_match(p$message, "sec_axis has no argument `nam`")
  # a call of a function with no entry (seq(), alpha()): the calls inside it
  p <- sc_problems("scale_x_continuous", list(breaks = tfl_call("seq", list(from = 0, to = 52, by = 4))))
  expect_identical(nrow(p), 0L)
  # what is right stays right: as_labeller(x =), scale_y_log10(sec.axis =), dup_axis(name = derive())
  lab <- tfl_call("as_labeller", list(x = list(ALT = "Alanine aminotransferase")))
  expect_identical(nrow(sc_problems("facet_wrap", list(facets = "PARAMCD", labeller = lab))), 0L)
  dup <- tfl_call("dup_axis", list(name = tfl_call("derive", list()),
                                   labels = tfl_call("scales::label_log", list())))
  expect_identical(nrow(sc_problems("scale_y_log10", list(sec.axis = dup))), 0L)
  leg <- tfl_call("guide_legend", list(nrow = 1L, theme = tfl_call("theme", list(legend.title.position = "top"))))
  expect_identical(nrow(sc_problems("guides", list(colour = leg))), 0L)
})

test_that("a guide's argument of before ggplot2 3.5.0 warns with the theme setting it is now", {
  p <- sc_problems("guides", list(fill = tfl_call("guide_legend", list(title.position = "top", nrow = 1L))))
  expect_identical(p$level, "warning")
  expect_match(p$message, "deprecated since ggplot2 3.5.0")
  expect_match(p$message, "legend.title.position")
  p <- sc_problems("guide_colourbar", list(barheight = 10))
  expect_identical(p$level, "warning")
  expect_match(p$message, "legend.key.height")
  # not a guide's: no such note
  p <- sc_problems("scale_y_continuous", list(title.position = "top"))
  expect_match(p$message, "has no argument")
})

test_that("a plotmath expression (type math): label_math(10^.x), checked by parsing", {
  expect_true(.schema_ok("10^.x", .schema_type("math")))
  expect_true(.schema_ok("alpha[.x]", .schema_type("math")))
  expect_false(.schema_ok("~ 10^.x", .schema_type("math")))
  expect_false(.schema_ok("function(x) 10^x", .schema_type("math")))
  expect_false(.schema_ok("10^", .schema_type("math")))
  expect_false(.schema_ok(10, .schema_type("math")))
})

test_that("a log axis' scales helpers read as typed values and are written back the same", {
  skip_if_not_installed("scales")
  for (code in c(
    "scale_y_log10(breaks = scales::trans_breaks(\"log10\", function(x) 10^x), labels = scales::trans_format(\"log10\", scales::math_format(10^.x)))",
    "scale_y_continuous(labels = scales::label_math(10^.x))",
    "scale_y_log10(breaks = scales::breaks_log(n = 6, base = 10), labels = scales::label_number(accuracy = 0.1, big.mark = \",\"))",
    "scale_x_continuous(transform = scales::transform_log(base = 2), labels = scales::label_log(base = 2))",
    "scale_y_continuous(labels = scales::label_number(style_positive = \"plus\", suffix = \"%\"))",
    "scale_y_continuous(labels = scales::label_pvalue(accuracy = 0.001, add_p = TRUE))",
    "scale_y_continuous(sec.axis = dup_axis(name = derive()))",
    "scale_y_log10(sec.axis = dup_axis(name = derive(), labels = scales::label_log()))")) {
    it <- sc_item(code)
    expect_false(sc_has_r(it$args), info = code)
    expect_identical(nrow(tfl_schema_check(it)), 0L, info = code)
    expect_identical(str2lang(sc_code(it)), str2lang(code), info = code)
  }
  it <- sc_item("scale_y_continuous(labels = scales::label_math(10^.x))")
  expect_identical(it$args$labels, tfl_call("scales::label_math", list(expr = "10^.x")))
  it <- sc_item("scale_y_log10(breaks = scales::trans_breaks(\"log10\", function(x) 10^x))")
  expect_identical(it$args$breaks$args, list(trans = "log10", inv = "function(x) 10^x"))
  it <- sc_item("scale_y_continuous(sec.axis = sec_axis(~ . / 7, name = derive()))")
  expect_identical(it$args$sec.axis$args$name, tfl_call("derive", list()))
  # written from a SPEC: the expression and the function bare, by position
  expect_identical(
    sc_code(tfl_item("scale_y_log10", list(
      breaks = tfl_call("scales::trans_breaks", list(trans = "log10", inv = "function(x) 10^x")),
      labels = tfl_call("scales::trans_format", list(
        trans = "log10", format = tfl_call("scales::math_format", list(expr = "10^.x"))))))),
    paste0("scale_y_log10(breaks = scales::trans_breaks(\"log10\", function(x) 10^x), ",
           "labels = scales::trans_format(\"log10\", scales::math_format(10^.x)))"))
})

test_that("a log axis written from a SPEC draws the labels ggplot2 draws from the code", {
  skip_if_not_installed("scales")
  df <- data.frame(t = c(1, 2, 4, 8), conc = c(1, 10, 100, 1000))
  code <- paste0("scale_y_log10(breaks = scales::trans_breaks(\"log10\", function(x) 10^x), ",
                 "labels = scales::trans_format(\"log10\", scales::math_format(10^.x)))")
  draw <- function(sc) {
    p <- ggplot2::ggplot(df, ggplot2::aes(t, conc)) + ggplot2::geom_point() + sc
    b <- suppressWarnings(ggplot2::ggplot_build(p))
    b$layout$panel_params[[1L]]$y$get_labels()
  }
  spec <- eval(.fig_item_lang(sc_item(code)), asNamespace("ggplot2"))
  orig <- eval(str2lang(code), asNamespace("ggplot2"))
  expect_identical(draw(spec), draw(orig))
  expect_true(is.expression(draw(spec)) || is.list(draw(spec)))
})
