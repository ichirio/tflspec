# A figure's content as function calls, plot by plot (R/fig_content.R):
# the file's form, its rules, the code, and the designer's designs
# converted to it (the same figure).

fc_template <- function(t, adam = tfl_example_adam()) {
  kinds <- tfl_fig_templates()
  kind <- kinds$kind[kinds$template == t]
  prm <- unique(adam$ADTTE$PARAMCD)[1]
  args <- switch(kind, km = list(param = prm, group = if (t != "km_single_arm") "TRT01P"),
                 forest = list(param = prm), list())
  do.call(tfl_fig_template, c(list(t), args))
}

fc_output <- function(x, id = "F-1") {
  o <- tfl_output(id, "figure", data = x$data, content = x$content)
  f <- tempfile(fileext = ".yml")
  tfl_write_output(o, f)
  tfl_read_output(f)
}

test_that("a figure's plots, its R items and tfl_image read and write as they are", {
  o <- tfl_output("F-1", "figure", content = list(
    tfl_plot("km", list(tfl_item("ggsurvfit", list(x = "fit")),
                        tfl_item("add_censor_mark", list(shape = 4)),
                        tfl_item(".r", list(code = tfl_r("annotate(\"text\", x = 1, y = 1, label = \"A\")"))))),
    tfl_plot("risk", list(tfl_item("tfl_risk_panel", list(fit = "fit", title = "At risk")))),
    tfl_item("wrap_plots", list(plots = c("km", "risk"), ncol = 1L, heights = c(3, 1))),
    tfl_item("tfl_image", list(dpi = 300L))))
  f <- withr::local_tempfile(fileext = ".yml")
  tfl_write_output(o, f)
  y <- readLines(f)
  expect_true(any(y == "  - km:"))
  expect_true(any(grepl("^      - !r annotate", y)))
  back <- tfl_read_output(f)
  expect_s3_class(back$content[[1L]], "tfl_plot")
  expect_identical(back$content[[1L]]$fn, "km")
  expect_identical(vapply(back$content[[1L]]$items, `[[`, "", "fn"), c("ggsurvfit", "add_censor_mark", ".r"))
  expect_identical(back$content[[4L]]$fn, "tfl_image")
  expect_true(.fig_new_content(back$content))
  # the designer's pieces are not this form
  expect_false(.fig_new_content(list(tfl_item("km_curve", list()))))
})

test_that("the reading rules: plots or items, a plot starts with what makes it, tfl_image last", {
  p <- function(name, fn) tfl_plot(name, list(tfl_item(fn, list())))
  expect_error(.fig_split_content(list(p("a", "ggplot"), tfl_item("labs", list()))), "not both")
  expect_error(.fig_split_content(list(p("labs", "ggplot"))), "a function's name")
  expect_error(.fig_split_content(list(p("a", "geom_line"))), "starts with the function")
  expect_error(.fig_split_content(list(tfl_item("ggplot", list()), tfl_item("tfl_image", list()),
                                       tfl_item("labs", list()))), "last")
  s <- .fig_split_content(list(tfl_item("ggplot", list()), tfl_item("labs", list(x = "X")),
                               tfl_item("tfl_image", list(dpi = 300))))
  expect_length(s$plots, 1L)
  expect_null(s$plots[[1L]]$name)
  expect_identical(s$image$dpi, 300)
})

test_that("an item and its call, both ways", {
  it <- tfl_item("geom_errorbar", list(mapping = list(x = "AVISIT", ymin = "lo", ymax = tfl_r("mean + se")),
                                       data = "sm", width = 0.2,
                                       position = tfl_call("position_dodge", list(width = 0.3))))
  e <- .fig_item_lang(it)
  expect_identical(.fig_deparse(e), paste0("geom_errorbar(mapping = aes(x = AVISIT, ymin = lo, ymax = mean + se), ",
                                           "data = sm, width = 0.2, position = position_dodge(width = 0.3))"))
  back <- .fig_lang_item(e)
  expect_identical(back$fn, "geom_errorbar")
  expect_identical(back$args$mapping$x, "AVISIT")
  expect_s3_class(back$args$position, "tfl_call")
  # ggsurvfit's fit is its `x`, an object, written bare
  expect_identical(.fig_deparse(.fig_item_lang(tfl_item("ggsurvfit", list(x = "fit", linewidth = 0.3)))),
                   "ggsurvfit(x = fit, linewidth = 0.3)")
  # NULL is !r NULL; !unset is a layering's and warns
  expect_identical(as.character(.fig_lang_value(NULL)), "NULL")
  expect_warning(.fig_value_lang(tfl_unset(), "colour"), "!r NULL")
})

test_that("a template converted is the same figure: layers, labels and ranges", {
  skip_on_cran()
  skip_if_not_installed("ggsurvfit")
  skip_if_not_installed("patchwork")
  adam <- tfl_example_adam()
  run <- function(code, ard) {
    e <- new.env(parent = globalenv())
    for (n in names(adam)) assign(tolower(n), adam[[n]], envir = e)
    eval(parse(text = tfl_fig_setup_code()), e)
    if (!is.null(ard)) {
      assign("ard", ard, envir = e)
      eval(parse(text = tfl_helpers_code()), e)
    }
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    set.seed(1)
    suppressPackageStartupMessages(suppressWarnings(suppressMessages(
      eval(parse(text = c("library(dplyr)", "library(ggplot2)", "library(ggsurvfit)",
                          "library(patchwork)", code)), e))))
    e$content
  }
  parts <- function(p) {
    if (!inherits(p, "patchwork")) return(list(p))
    lapply(seq_len(length(p$patches$plots) + 1L), function(i) p[[i]])
  }
  built <- function(p) {
    set.seed(2)
    b <- suppressWarnings(suppressMessages(ggplot2::ggplot_build(p)))
    list(layers = lapply(b$data, function(d) { d$group <- NULL; d$PANEL <- NULL; d }),
         labels = p$labels[order(names(p$labels))],
         x = b$layout$panel_params[[1]]$x.range, y = b$layout$panel_params[[1]]$y.range)
  }
  tp <- tfl_fig_templates()
  n_same <- 0L
  for (t in tp$template) {
    d <- fc_template(t, adam)
    x <- tfl_fig_content_from_design(d)
    if (inherits(x$content, "tfl_r")) next
    ard <- if (!is.null(attr(d, "analyses"))) forest_example_ard(adam, d)
    old <- parts(run(tfl_fig_design_code(d, setup = TRUE, save = FALSE, name = "content"), ard))
    new <- parts(run(.fig_content_code(fc_output(x)), ard))
    expect_identical(length(new), length(old), info = t)
    for (k in seq_along(old)) {
      expect_equal(built(new[[k]]), built(old[[k]]), ignore_attr = TRUE, info = paste(t, k))
    }
    n_same <- n_same + 1L
  }
  expect_identical(n_same, 28L)
})

test_that("a figure of items runs and is written by the report's functions", {
  skip_if_not_installed("ggsurvfit")
  skip_if_not_installed("patchwork")
  adam <- tfl_example_adam()
  x <- tfl_fig_content_from_design(fc_template("km_risk_table", adam))
  o <- fc_output(x)
  code <- tfl_content_code(o)
  expect_true(any(startsWith(code, "p_plot <- ggsurvfit(x = fit")))
  expect_true(any(startsWith(code, "p_risk <- ggplot(risk")))
  expect_true(any(grepl("^content <- wrap_plots\\(p_plot, p_risk, ncol = 1", code)))
  expect_true(any(grepl("scale_colour_tfl(palette = \"treatment\"", code, fixed = TRUE)))
  p <- suppressWarnings(tfl_content_run(o, list(adtte = adam$ADTTE, adsl = adam$ADSL)))
  expect_s3_class(p, "patchwork")
  # one plot: no p_, no arrangement
  y <- tfl_fig_content_from_design(fc_template("mean_se", adam))
  code <- tfl_content_code(fc_output(y))
  expect_true(any(startsWith(code, "content <- ggplot() +")))
  expect_true(any(code == "pd <- position_dodge(width = 0.3)"))
  expect_false(any(grepl("wrap_plots|p_content", code)))
})

test_that("the figure catalog: every function and argument is the installed package's", {
  s <- .schema_all(figure = FALSE)
  fig <- Filter(function(e) !is.null(e$figure) && !is.null(e$rank), s)
  expect_gte(length(fig), 60L)
  ranks <- vapply(fig, function(e) as.numeric(e$rank), 0)
  expect_identical(names(fig)[which.max(ranks)], "tfl_image")
  for (fn in names(fig)) {
    e <- fig[[fn]]
    expect_true(all(c("en", "ja") %in% names(e$label)), info = fn)
    if (identical(e$package, "tflspec")) next
    if (!requireNamespace(e$package, quietly = TRUE)) next
    f <- getExportedValue(e$package, fn)
    known <- c(names(formals(f)), names(tfl_schema(fn)$args))
    for (a in c(names(e$args), unlist(e$visible))) {
      expect_true(a %in% known || "..." %in% names(formals(f)), info = paste(fn, a))
    }
  }
  # every converted template's items check clean
  for (t in c("mean_se", "km_risk_table", "forest_hr", "waterfall_response", "swimmer_full")) {
    x <- tfl_fig_content_from_design(fc_template(t))
    for (it in x$content) {
      p <- tfl_schema_check(it)
      expect_identical(nrow(p), 0L, info = paste(t, it$fn, paste(p$message, collapse = "; ")))
    }
  }
})

test_that("an item's arguments as R text and back (the builder's R form)", {
  expect_identical(tfl_args_to_r("table_plan", list(cols = "TRT", rows = list(group = "variable"))),
                   'cols = "TRT", rows = c(group = "variable")')
  a <- tfl_args_from_r("table_plan", 'cols = "TRT", rows = c(group = "variable")')
  expect_identical(a$rows, list(group = "variable"))
  expect_identical(nrow(attr(a, "problems")), 0L)
  a <- tfl_args_from_r("plan_filter", "p >= 0.05")
  expect_s3_class(a$where, "tfl_r")
  g <- tfl_args_from_r("geom_line", "aes(x = AVISIT, y = mean), linewidth = 0.5, position = position_dodge(width = 0.3)")
  expect_identical(g$mapping, list(x = "AVISIT", y = "mean"))
  expect_identical(tfl_args_to_r("geom_line", g),
                   "mapping = aes(x = AVISIT, y = mean), position = position_dodge(width = 0.3), linewidth = 0.5")
  expect_true(any(attr(tfl_args_from_r("geom_line", "linewidht = 1"), "problems")$level == "warning"))
  expect_error(tfl_args_from_r("labs", "x = ("), "not R")
})

test_that("tfl_image: the figure drawn at its size on the page and its dpi, saved when it names a file", {
  skip_if_not_installed("rtfreporter")
  x <- tfl_fig_content_from_design(fc_template("mean_se"))
  n <- length(x$content)
  x$content[[n]] <- tfl_item("tfl_image", list(width_in = "fit", height_in = 4, dpi = 150,
                                               file = "{output_id}.png"))
  o <- tfl_output("F-1", "figure", data = x$data, content = x$content,
                  page = list(rtf_page = list(orientation = "landscape"),
                              titles = list(`1` = list(center = "Figure 1"))))
  st <- structure(list(study = list(), data = list(),
                       defaults = list(content = list(), page = list()), codelists = list(),
                       patterns = list(), toc = list(), dir = tempdir()), class = "tfl_study_spec")
  code <- paste(tfl_report_code(o, study = st, content = "content"), collapse = "\n")
  # the body of a landscape letter page (11 - 2 x 0.75), at 4 in and 150 dpi
  expect_match(code, "rtfplot(content, render_width = 9.5, render_height = 4, render_dpi = 150)",
               fixed = TRUE)
  cc <- tfl_content_code(o)
  expect_identical(utils::tail(cc, 1L),
                   'ggsave("F-1.png", content, width = 9.5, height = 4, dpi = 150, device = "png")')
  # no file: nothing saved; no tfl_image: drawn at the figure's own size, 300 dpi
  x$content[[n]] <- tfl_item("tfl_image", list(dpi = 300))
  o2 <- tfl_output("F-1", "figure", data = x$data, content = x$content)
  expect_false(any(grepl("ggsave", tfl_content_code(o2))))
  expect_match(paste(tfl_report_code(o2, study = st, content = "content"), collapse = "\n"),
               "render_width = 6.5, render_height = 4.5, render_dpi = 300", fixed = TRUE)
})

test_that("tfl_fig_template(form = \"items\"): the content of calls, marked as the template's, with its arguments", {
  x <- tfl_fig_template("km_risk_table", param = "OS", group = "TRT01P", form = "items")
  expect_identical(x$template, "km_risk_table")
  expect_identical(x$args, list(param = "OS", group = "TRT01P"))
  expect_identical(x$data$source, "code")
  expect_true(length(x$data$steps) > 0L)
  froms <- unlist(lapply(x$content, function(it) c(it$from, vapply(it$items %||% list(), `[[`, "", "from"))))
  expect_true(all(froms == "template:km_risk_table"))
  expect_identical(vapply(x$content, `[[`, "", "fn"), c("plot", "risk", "wrap_plots", "tfl_image"))
  # the same figure as the design converted
  d <- tfl_fig_template("km_risk_table", param = "OS", group = "TRT01P")
  expect_identical(lapply(.fig_mark_from(tfl_fig_content_from_design(d)$content, "own"), `[[`, "fn"),
                   lapply(x$content, `[[`, "fn"))
  # a template of its own ARD keeps its analyses; a figure type of its own is R
  expect_false(is.null(tfl_fig_template("forest_hr", param = "OS", form = "items")$analyses))
  expect_s3_class(tfl_fig_template("sankey_grey_links", form = "items")$content, "tfl_r")
  # the design stays the default
  expect_s3_class(tfl_fig_template("mean_se"), "tfl_fig_design")
})

test_that("the extension packages' functions: in the catalog, called as pkg::fn", {
  for (fn in c("geom_text_repel", "geom_label_repel", "geom_richtext", "new_scale_colour", "new_scale_fill",
               "facet_nested", "facet_nested_wrap", "facetted_pos_scales", "element_markdown",
               "element_textbox_simple")) {
    expect_false(is.null(tfl_schema(fn)$figure), info = fn)
  }
  it <- tfl_item("theme", list(axis.title.y = tfl_call("element_markdown", list(size = 9))))
  expect_identical(.fig_deparse(.fig_item_lang(it)), "theme(axis.title.y = ggtext::element_markdown(size = 9))")
  expect_identical(.fig_deparse(.fig_item_lang(tfl_item("new_scale_colour", list()))),
                   "ggnewscale::new_scale_colour()")
  o <- tfl_output("F-1", "figure", data = list(source = "code", steps = list(list(step = "read", dataset = "ADSL"))),
                  content = list(tfl_item("ggplot", list(data = "df", mapping = list(x = "AGE", y = "WEIGHTBL", label = "USUBJID"))),
                                 tfl_item("geom_point", list()),
                                 tfl_item("geom_text_repel", list(size = 2, max.overlaps = 5)),
                                 tfl_item("tfl_image", list())))
  code <- tfl_content_code(o)
  expect_true(any(grepl("ggrepel::geom_text_repel(size = 2, max.overlaps = 5)", code, fixed = TRUE)))
  expect_true("ggrepel" %in% attr(.fig_content_code(o), "needs"))
  skip_if_not_installed("ggrepel")
  adsl <- tfl_example_adam()$ADSL
  p <- suppressWarnings(tfl_content_run(o, list(adsl = adsl[1:20, ])))
  expect_s3_class(p, "ggplot")
})

test_that("a figure type of its own converts to its R, the same figure; a composed design to named plots", {
  skip_on_cran()
  skip_if_not_installed("patchwork")
  adam <- tfl_example_adam()
  data <- stats::setNames(adam, tolower(names(adam)))
  built <- function(p) {
    if (inherits(p, "patchwork")) return(lapply(seq_len(length(p$patches$plots) + 1L), function(i) built(p[[i]])))
    set.seed(2)
    b <- suppressWarnings(suppressMessages(ggplot2::ggplot_build(p)))
    lapply(b$data, function(x) { x$group <- NULL; x$PANEL <- NULL; x })
  }
  # (the old script of a figure type saves its PNG: in a folder of its own)
  withr::local_dir(withr::local_tempdir())
  run_old <- function(d) {
    e <- new.env(parent = globalenv())
    for (n in names(data)) assign(n, data[[n]], envir = e)
    eval(parse(text = tfl_fig_setup_code()), e)
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    set.seed(1)
    suppressPackageStartupMessages(suppressWarnings(suppressMessages(eval(parse(text = c(
      "library(dplyr)", "library(ggplot2)", "library(patchwork)",
      tfl_fig_design_code(d, setup = TRUE, save = FALSE, name = "content"))), e))))
    e$fig %||% e$content
  }
  tp <- tfl_fig_templates()
  for (t in tp$template[!tp$parts]) {
    d <- tfl_fig_template(t)
    x <- tfl_fig_content_from_design(d)
    expect_s3_class(x$content, "tfl_r")
    # the report's content saves nothing and prints nothing
    expect_false(grepl("ggsave|fig_path", x$content), info = t)
    o <- tfl_output("F-1", "figure", data = x$data, content = x$content)
    set.seed(1)
    new <- suppressWarnings(suppressMessages(tfl_content_run(o, data)))
    expect_equal(built(new), built(run_old(d)), ignore_attr = TRUE, info = t)
  }
  # several designs, one data part: each plot's objects renamed after it
  skip_if_not_installed("ggsurvfit")
  km <- tfl_fig_template("km_simple", param = unique(adam$ADTTE$PARAMCD)[1], group = "TRT01P")
  d <- tfl_fig_design(plots = list(km = km, box = tfl_fig_template("box_by_group")),
                      compose = list(layout = "km | box",
                                     add = list(list(fn = "plot_annotation", args = list(tag_levels = "A")),
                                                list(fn = "theme", args = list(legend.position = "bottom"), op = "&"))),
                      plot = list(dpi = 300))
  x <- tfl_fig_content_from_design(d)
  expect_identical(vapply(x$content, `[[`, "", "fn"), c("km", "box", "wrap_plots", "plot_annotation", "theme", "tfl_image"))
  o <- fc_output(x)
  code <- tfl_content_code(o)
  expect_true(any(grepl("^content <- wrap_plots\\(p_km, p_box, nrow = 1\\) \\+$", code)))
  expect_true(any(grepl("plot_annotation(tag_levels = \"A\") &", code, fixed = TRUE)))
  expect_true(any(grepl("fit_km <- survfit2", code, fixed = TRUE)))
  for (it in o$content) expect_identical(nrow(tfl_schema_check(it)), 0L, info = it$fn)
  new <- suppressWarnings(tfl_content_run(o, data))
  expect_equal(built(new), built(run_old(d)), ignore_attr = TRUE)
  # a layout of more than one row or column is not converted
  d$compose$layout <- "(km | box) / km"
  expect_error(tfl_fig_content_from_design(d), "one row")
})
