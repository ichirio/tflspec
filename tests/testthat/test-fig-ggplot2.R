# How much of ggplot2 a figure's content can say (R/spec_figure.R,
# inst/fig/gg_args.csv): any function of a plot ggplot2 exports, a geom
# drawn by another stat (and a stat by another geom) with that one's
# parameters, the arguments a kind shares typed, list() arguments.

gg_problems <- function(fn, args) tfl_schema_check(tfl_item(fn, args))
gg_code <- function(fn, args) .fig_deparse(.fig_item_lang(tfl_item(fn, args)))

test_that("a ggplot2 function the catalog has not is written and checked by its arguments", {
  for (fn in c("scale_colour_steps", "theme_test", "guide_axis_logticks", "coord_cartesian",
               "position_dodge2", "facet_wrap", "element_text")) {
    expect_identical(nrow(gg_problems(fn, list())), 0L, info = fn)
  }
  expect_identical(nrow(gg_problems("scale_colour_steps", list(name = "Change", n.breaks = 5L))), 0L)
  p <- gg_problems("scale_colour_steps", list(nmae = "Change"))
  expect_identical(p$level, "warning")
  expect_match(p$message, "no argument `nmae`")
  expect_identical(gg_code("scale_colour_steps", list(low = "white")), "scale_colour_steps(low = \"white\")")
  # an export that is no function of a plot is not a figure's
  expect_identical(gg_problems("ggsave", list())$level, "error")
  expect_null(tfl_schema("ggsave"))
  expect_identical(tfl_schema("theme_test")$figure, "theme")
})

test_that("a geom drawn by another stat takes that stat's parameters (geom_step(stat = 'ecdf'))", {
  expect_identical(nrow(gg_problems("geom_step", list(stat = "ecdf", pad = FALSE,
                                                     mapping = list(x = "CHG", colour = "TRTA")))), 0L)
  # without the stat, pad is no argument of geom_step
  expect_identical(gg_problems("geom_step", list(pad = FALSE))$level, "warning")
  # mean +/- SE at each visit
  a <- list(stat = "summary", fun.data = "mean_se", fun.args = list(mult = 1.96),
            position = tfl_call("position_dodge", list(width = 0.3)))
  expect_identical(nrow(gg_problems("geom_pointrange", a)), 0L)
  expect_identical(gg_code("geom_pointrange", a),
                   paste0("geom_pointrange(stat = \"summary\", fun.data = \"mean_se\", ",
                          "fun.args = list(mult = 1.96), position = position_dodge(width = 0.3))"))
  expect_identical(nrow(gg_problems("geom_line", list(stat = "summary", fun = "mean"))), 0L)
  expect_identical(nrow(gg_problems("geom_path", list(stat = "ellipse", level = 0.9, type = "norm"))), 0L)
  # a stat drawn by another geom: that geom's parameters
  expect_identical(nrow(gg_problems("stat_summary", list(geom = "errorbar", width = 0.2,
                                                         fun.data = "mean_cl_normal"))), 0L)
  expect_identical(nrow(gg_problems("stat_ecdf", list(geom = "step", pad = FALSE, linewidth = 0.4))), 0L)
  # the stat's parameters are typed
  p <- gg_problems("geom_step", list(stat = "ecdf", pad = "no"))
  expect_identical(p$arg, "pad")
  expect_match(p$message, "logical")
  expect_identical(tfl_schema("stat_ecdf")$args$pad$label$en, "Pad the ends")
})

test_that("a list argument is written list() and read back as a map", {
  e <- .fig_item_lang(tfl_item("geom_smooth", list(method = "glm", method.args = list(family = "binomial"))))
  expect_identical(.fig_deparse(e), "geom_smooth(method = \"glm\", method.args = list(family = \"binomial\"))")
  back <- .fig_lang_item(str2lang("stat_summary(fun.data = mean_cl_normal, fun.args = list(conf.int = 0.9))"))
  expect_identical(back$args$fun.args, list(conf.int = 0.9))
  expect_s3_class(back$args$fun.data, "tfl_r")
  # override.aes inside a guide
  g <- .fig_lang_item(str2lang("guides(colour = guide_legend(override.aes = list(linetype = 0, size = 3)))"))
  expect_s3_class(g$args$colour, "tfl_call")
  expect_identical(g$args$colour$args$override.aes, list(linetype = 0, size = 3))
  expect_identical(.fig_deparse(.fig_item_lang(g)),
                   "guides(colour = guide_legend(override.aes = list(linetype = 0, size = 3)))")
  # any other map stays a named vector
  expect_identical(gg_code("scale_colour_manual", list(values = list(A = "red", B = "blue"))),
                   "scale_colour_manual(values = c(A = \"red\", B = \"blue\"))")
})

test_that("the arguments a kind shares are typed: scales, coordinates, theme(), guides", {
  # a scale's `...` are its constructor's arguments
  expect_identical(nrow(gg_problems("scale_colour_manual", list(
    values = c("grey50", "red"), name = "Treatment", labels = c("Placebo", "Drug"),
    limits = c("P", "D"), na.translate = FALSE))), 0L)
  expect_identical(nrow(gg_problems("scale_y_continuous", list(
    breaks = c(0, 25, 50), expand = tfl_call("expansion", list(mult = c(0, 0.05))),
    sec.axis = tfl_call("dup_axis", list(name = "")), n.breaks = 5L))), 0L)
  expect_match(gg_problems("scale_y_continuous", list(sec.axis = tfl_call("expansion", list())))$message,
               "sec_axis")
  expect_identical(nrow(gg_problems("scale_fill_brewer", list(palette = "Set1", direction = -1L))), 0L)
  expect_identical(nrow(gg_problems("coord_cartesian", list(ylim = c(0, 1), expand = FALSE, clip = "off"))), 0L)
  expect_identical(nrow(gg_problems("coord_trans", list(y = "log10"))), 0L)
  expect_identical(nrow(gg_problems("coord_fixed", list(ratio = 1, xlim = c(0, 10)))), 0L)
  # theme(): each setting its element
  expect_identical(nrow(gg_problems("theme", list(
    axis.text.x = tfl_call("element_text", list(angle = 45, hjust = 1)),
    panel.grid.minor = tfl_call("element_blank", list()),
    legend.position = "bottom", legend.direction = "horizontal",
    plot.margin = tfl_call("margin", list(t = 5, r = 5, b = 5, l = 5))))), 0L)
  p <- gg_problems("theme", list(axis.text.x = tfl_call("element_line", list())))
  expect_identical(p$level, "error")
  expect_match(p$message, "element_text")
  expect_identical(tfl_schema("theme")$args$panel.border$label$ja, "\u9762\u306e\u67a0")
  expect_identical(nrow(gg_problems("theme_bw", list(base_size = 9, base_family = "sans"))), 0L)
  expect_match(gg_problems("theme_bw", list(base_size = "9"))$message, "number")
  expect_identical(nrow(gg_problems("guide_legend", list(nrow = 1L, reverse = TRUE))), 0L)
  expect_identical(nrow(gg_problems("labs", list(x = "Week", colour = "Treatment", tag = "A"))), 0L)
  expect_identical(nrow(gg_problems("facet_wrap", list(facets = tfl_r("vars(PARAM)"), ncol = 2L,
                                                      scales = "free_y", strip.position = "top"))), 0L)
})

test_that("the catalog's new cards: labelled, their shown arguments typed, each a function of ggplot2", {
  s <- .schema_all(figure = FALSE)
  for (fn in c("geom_bar", "geom_linerange", "geom_crossbar", "geom_rect", "stat_ecdf", "stat_summary",
               "stat_summary_bin", "stat_ellipse", "stat_unique", "position_dodge", "scale_colour_brewer",
               "scale_fill_gradient2", "scale_x_date", "coord_trans", "coord_fixed", "theme_bw",
               "theme_classic", "element_text", "guide_legend", "expansion", "sec_axis")) {
    e <- s[[fn]]
    expect_false(is.null(e$figure), info = fn)
    expect_true(all(c("en", "ja") %in% names(e$label)), info = fn)
    ent <- tfl_schema(fn)
    for (a in unlist(e$visible)) {
      expect_false(is.null(ent$args[[a]]$label), info = paste(fn, a))
    }
  }
  # every row of the shared arguments is a known kind, and its type parses
  g <- .fig_gg_args()
  expect_true(all(g$kind %in% c(unname(.schema_gg_prefix), "labs", "helper")))
  expect_false(anyDuplicated(paste(g$kind, g$arg)) > 0L)
  for (t in unique(g$type)) expect_false(is.null(.schema_type(t)$kind), info = t)
})

test_that("a figure of these runs: mean +/- SE over groups, an ECDF, brewer colours, theme_bw", {
  o <- tfl_output("F-1", "figure",
    data = list(source = "code", steps = list(list(step = "read", dataset = "ADSL"))),
    content = list(
      tfl_item("ggplot", list(data = "df", mapping = list(x = "AGEGR1", y = "TRTDURD", colour = "TRT01A",
                                                         group = "TRT01A"))),
      tfl_item("geom_pointrange", list(stat = "summary", fun.data = "mean_se", fun.args = list(mult = 1),
                                       position = tfl_call("position_dodge", list(width = 0.3)))),
      tfl_item("geom_line", list(stat = "summary", fun = "mean",
                                 position = tfl_call("position_dodge", list(width = 0.3)))),
      tfl_item("stat_ecdf", list(mapping = list(x = "TRTDURD", y = tfl_r("NULL")), geom = "step", pad = FALSE,
                                 inherit.aes = FALSE)),
      tfl_item("scale_colour_brewer", list(palette = "Dark2", name = "Treatment")),
      tfl_item("theme_bw", list(base_size = 9)),
      tfl_item("theme", list(legend.position = "bottom",
                             panel.grid.minor = tfl_call("element_blank", list()))),
      tfl_item("tfl_image", list())))
  for (it in o$content) {
    p <- tfl_schema_check(it)
    expect_identical(nrow(p), 0L, info = paste(it$fn, paste(p$message, collapse = "; ")))
  }
  code <- tfl_content_code(o)
  expect_true(any(grepl("fun.args = list(mult = 1)", code, fixed = TRUE)))
  expect_true(any(grepl("scale_colour_brewer(palette = \"Dark2\", name = \"Treatment\")", code, fixed = TRUE)))
  adsl <- tfl_example_adam()$ADSL
  p <- suppressWarnings(tfl_content_run(o, list(adsl = adsl)))
  expect_s3_class(p, "ggplot")
  b <- suppressWarnings(ggplot2::ggplot_build(p))
  expect_length(b$data, 3L)
})
