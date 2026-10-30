# The extension packages a figure calls (R/spec_figure.R, inst/fig/gg_args.csv):
# ggsurvfit, ggrepel, ggh4x, ggtext, ggnewscale and patchwork -- their
# functions with a card typed and labelled, any other of a plot by its kind.

ext_problems <- function(fn, args) tfl_schema_check(tfl_item(fn, args))
ext_code <- function(fn, args) .fig_deparse(.fig_item_lang(tfl_item(fn, args)))

test_that("the extensions' cards are labelled and every argument they show is typed", {
  s <- .schema_all(figure = FALSE)
  cards <- c("add_risktable", "add_pvalue", "add_quantile", "add_legend_title",
             "add_risktable_strata_symbol", "scale_ggsurvfit", "theme_ggsurvfit_KMunicate",
             "theme_risktable_boxed", "stat_stepribbon", "geom_text_repel", "position_nudge_repel",
             "facet_grid2", "facet_wrap2", "force_panelsizes", "strip_nested", "strip_themed",
             "elem_list_rect", "element_markdown", "element_textbox", "geom_textbox", "new_scale",
             "plot_layout", "plot_annotation", "plot_spacer", "wrap_elements", "inset_element",
             "guide_area", "area", "free")
  for (fn in cards) {
    e <- s[[fn]]
    expect_false(is.null(e$figure), info = fn)
    expect_false(is.null(e$rank), info = fn)
    expect_true(all(c("en", "ja") %in% names(e$label)), info = fn)
    skip_if_not_installed(e$package)
    expect_true(fn %in% getNamespaceExports(e$package), info = fn)
    ent <- tfl_schema(fn)
    for (a in unlist(e$visible)) expect_false(is.null(ent$args[[a]]$label), info = paste(fn, a))
  }
})

test_that("an extension's function with no card is its kind's, by pkg::fn", {
  skip_if_not_installed("patchwork")
  skip_if_not_installed("ggh4x")
  e <- tfl_schema("ggh4x::facet_manual")
  expect_identical(e$figure, "facet")
  expect_identical(e$args$scales$label$en, "Axes")
  expect_identical(tfl_schema("patchwork::wrap_table")$figure, "compose")
  expect_identical(nrow(ext_problems("ggh4x::facet_manual", list(scales = "free_y"))), 0L)
  p <- ext_problems("ggh4x::facet_manual", list(scales = "loose"))
  expect_identical(p$arg, "scales")
  # an export that is no function of a plot is none
  expect_null(tfl_schema("ggh4x::help_secondary"))
  expect_identical(ext_code("ggh4x::facet_manual", list(scales = "free_y")),
                   "ggh4x::facet_manual(scales = \"free_y\")")
})

test_that("ggrepel's own arguments are typed: its direction is not geom_step's", {
  withr::local_pdf(NULL)
  skip_if_not_installed("ggrepel")
  a <- list(mapping = list(x = "x", y = "y", label = "id"), direction = "y", max.overlaps = tfl_r("Inf"),
            box.padding = 0.5, min.segment.length = 0, seed = 1, force = 2)
  expect_identical(nrow(ext_problems("geom_text_repel", a)), 0L)
  expect_identical(ext_problems("geom_label_repel", list(direction = "hv"))$arg, "direction")
  expect_identical(ext_problems("geom_text_repel", list(force = "strong"))$arg, "force")
  code <- ext_code("geom_text_repel", a)
  expect_identical(substr(code, 1L, 26L), "ggrepel::geom_text_repel(m")
  df <- data.frame(x = 1:5, y = c(2, 3, 3, 4, 5), id = letters[1:5])
  p <- eval(str2lang(paste0("ggplot2::ggplot(df) + ", code)), list(df = df), asNamespace("ggplot2"))
  expect_s3_class(p, "ggplot")
  expect_no_error(suppressWarnings(ggplot2::ggplot_build(p)))
})

test_that("a KM figure of ggsurvfit's functions: risk table, p-value, axes, themes", {
  withr::local_pdf(NULL)
  skip_if_not_installed("ggsurvfit")
  skip_if_not_installed("survival")
  items <- list(
    list("add_confidence_interval", list(type = "lines")),
    list("add_risktable", list(risktable_stats = list("n.risk", "cum.event"),
                               stats_label = list(n.risk = "At risk", cum.event = "Events"),
                               risktable_group = "risktable_stats", combine_groups = FALSE,
                               theme = tfl_call("theme_risktable_boxed", list(axis.text.y.size = 9)))),
    list("add_risktable_strata_symbol", list(symbol = "\u25cf", vjust = 0.3)),
    list("add_quantile", list(y_value = 0.5, linetype = 2)),
    list("add_pvalue", list(caption = "Log-rank {p.value}", rho = 0, location = "caption")),
    list("add_legend_title", list(title = "Treatment")),
    list("scale_ggsurvfit", list(x_scales = list(breaks = list(0, 250, 500, 750, 1000)))),
    list("theme_ggsurvfit_KMunicate", list()))
  for (it in items) {
    p <- ext_problems(it[[1L]], it[[2L]])
    expect_identical(nrow(p), 0L, info = paste(it[[1L]], paste(p$message, collapse = "; ")))
  }
  expect_identical(ext_problems("add_pvalue", list(location = "legend"))$arg, "location")
  expect_identical(ext_problems("add_risktable", list(risktable_group = "arm"))$arg, "risktable_group")
  codes <- vapply(items, function(it) ext_code(it[[1L]], it[[2L]]), "")
  expect_identical(codes[[7L]], "scale_ggsurvfit(x_scales = list(breaks = c(0, 250, 500, 750, 1000)))")
  expect_identical(codes[[2L]], paste0(
    "add_risktable(risktable_stats = c(\"n.risk\", \"cum.event\"), ",
    "stats_label = list(n.risk = \"At risk\", cum.event = \"Events\"), ",
    "risktable_group = \"risktable_stats\", combine_groups = FALSE, ",
    "theme = theme_risktable_boxed(axis.text.y.size = 9))"))
  env <- new.env(parent = asNamespace("ggsurvfit"))
  env$fit <- ggsurvfit::survfit2(survival::Surv(time, status) ~ sex, data = survival::lung)
  p <- eval(str2lang(paste(c("ggsurvfit(fit)", codes), collapse = " + ")), env)
  expect_s3_class(p, "ggsurvfit")
  expect_no_error(suppressWarnings(ggsurvfit::ggsurvfit_build(p)))
})

test_that("ggh4x's facets and strips, ggtext's elements, ggnewscale's new scale run", {
  withr::local_pdf(NULL)
  skip_if_not_installed("ggh4x")
  skip_if_not_installed("ggtext")
  skip_if_not_installed("ggnewscale")
  strip <- tfl_call("strip_themed", list(
    background_x = tfl_call("elem_list_rect", list(fill = list("grey90", "white"))),
    text_x = tfl_call("elem_list_text", list(colour = list("red", "blue")))))
  items <- list(
    list("ggh4x::facet_grid2", list(cols = tfl_r("vars(g)"), scales = "free_y", independent = "y",
                                    axes = "all", strip = strip)),
    list("force_panelsizes", list(cols = list(3, 1), respect = FALSE)),
    list("new_scale", list(new_aes = "colour")),
    list("theme", list(plot.title = tfl_call("element_markdown", list(halign = 0.5, padding = tfl_call(
      "margin", list(t = 2)))))))
  for (it in items) {
    p <- ext_problems(it[[1L]], it[[2L]])
    expect_identical(nrow(p), 0L, info = paste(it[[1L]], paste(p$message, collapse = "; ")))
  }
  expect_identical(ext_problems("strip_nested", list(size = "variable"))$level, character())
  expect_identical(ext_problems("strip_nested", list(size = 2))$arg, "size")
  expect_identical(ext_problems("force_panelsizes", list(rows = "big"))$arg, "rows")
  codes <- vapply(items, function(it) ext_code(it[[1L]], it[[2L]]), "")
  expect_match(codes[[1L]], "strip = ggh4x::strip_themed(background_x = ggh4x::elem_list_rect(fill = c(\"grey90\", \"white\"))",
               fixed = TRUE)
  expect_identical(codes[[2L]], "ggh4x::force_panelsizes(cols = c(3, 1), respect = FALSE)")
  expect_identical(codes[[3L]], "ggnewscale::new_scale(new_aes = \"colour\")")
  expect_match(codes[[4L]], "ggtext::element_markdown(halign = 0.5, padding = margin(t = 2))", fixed = TRUE)
  df <- data.frame(x = rep(1:3, 2), y = c(1, 2, 3, 10, 20, 30), g = rep(c("A", "B"), each = 3))
  code <- paste(c("ggplot(df, aes(x, y)) + geom_point(aes(colour = g)) + labs(title = \"**Mean**\")",
                  codes, "geom_line(aes(colour = g))"), collapse = " + ")
  env <- new.env(parent = asNamespace("ggplot2"))
  env$df <- df
  p <- eval(str2lang(code), env)
  expect_no_error(suppressWarnings(ggplot2::ggplotGrob(p)))
})

test_that("patchwork's arrangement: an inset, a spacer, a layout; a ggsurvfit theme attaches ggsurvfit", {
  withr::local_pdf(NULL)
  skip_if_not_installed("patchwork")
  items <- list(
    list("inset_element", list(p = "p_inset", left = 0.6, bottom = 0.6, right = 1, top = 1,
                               align_to = "panel")),
    list("plot_layout", list(widths = list(3, 1), guides = "collect", axes = "collect")),
    list("plot_annotation", list(title = "Change from baseline", tag_levels = "A")))
  for (it in items) {
    p <- ext_problems(it[[1L]], it[[2L]])
    expect_identical(nrow(p), 0L, info = paste(it[[1L]], paste(p$message, collapse = "; ")))
  }
  expect_identical(ext_problems("plot_layout", list(guides = "merge"))$arg, "guides")
  expect_identical(ext_problems("area", list(t = 1.5))$arg, "t")
  codes <- vapply(items, function(it) ext_code(it[[1L]], it[[2L]]), "")
  # the plot inset is an object by its name, not a string
  expect_identical(codes[[1L]], paste0("inset_element(p = p_inset, left = 0.6, bottom = 0.6, right = 1, ",
                                       "top = 1, align_to = \"panel\")"))
  env <- new.env(parent = asNamespace("patchwork"))
  env$p_inset <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  p_main <- ggplot2::ggplot(mtcars, ggplot2::aes(hp, mpg)) + ggplot2::geom_point()
  env$p_main <- p_main
  p <- eval(str2lang(paste(c("p_main", codes[[1L]]), collapse = " + ")), env)
  expect_s3_class(p, "patchwork")
  p2 <- eval(str2lang(paste(c("p_main + plot_spacer() + p_main", codes[-1L]), collapse = " + ")), env)
  expect_no_error(patchwork::patchworkGrob(p2))
  # a ggsurvfit theme on a ggplot: the code attaches ggsurvfit
  o <- tfl_output("F-1", "figure",
    data = list(source = "code", steps = list(list(step = "read", dataset = "ADSL"))),
    content = list(
      tfl_item("ggplot", list(data = "df", mapping = list(x = "AGE", y = "TRTDURD"))),
      tfl_item("geom_point", list()),
      tfl_item("theme_ggsurvfit_default", list()),
      tfl_item("tfl_image", list())))
  expect_true("ggsurvfit" %in% attr(.content_fig_code_out(o), "libs"))
})
