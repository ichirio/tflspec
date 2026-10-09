# Composed figures (plots + compose), ggsurvfit add_* checks and the call
# catalog (issue #39).

compose_run <- function(design, adam = tfl_example_adam()) {
  code <- tfl_fig_design_code(design, "c")
  e <- new.env()
  for (n in names(adam)) assign(tolower(n), adam[[n]], envir = e)
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  suppressWarnings(suppressMessages(eval(parse(text = code), envir = e)))
  list(code = code, fig = e$fig, png = file.exists(e$fig_path))
}

km_part <- function(risk = "panel") {
  d <- tfl_fig_template("km_risk_table", param = "OS", group = "TRT01P")
  if (risk == "add") {
    d$layers <- Filter(function(l) !identical(l$layer, "risk_table"), d$layers)
    d$layers <- c(d$layers, list(list(layer = "call", fn = "add_risktable", package = "ggsurvfit")))
  }
  d
}

two_up <- function(km = km_part()) {
  tfl_fig_design(
    plot = list(width = 10, height = 4.5),
    plots = list(km = km, box = tfl_fig_template("box_by_group")),
    compose = list(layout = "km | box",
                   add = list(list(fn = "plot_layout", args = list(widths = list(3, 2))),
                              list(fn = "plot_annotation", args = list(tag_levels = "A")),
                              list(op = "&", fn = "theme", args = list(plot.tag = list(fn = "element_text", args = list(face = "bold")))))))
}

test_that("a composed figure's script: each figure, the layout, one save", {
  code <- tfl_fig_design_code(two_up(), "c")
  expect_equal(code[1], "# c: composed figure (km, box)")
  expect_true("library(patchwork)" %in% code)
  expect_true(any(grepl("# ---- figure 1 (km): data", code, fixed = TRUE)))
  expect_true(any(grepl("# ---- figure 2 (box): plot", code, fixed = TRUE)))
  # the KM figure has its number at risk below it: wrapped as one figure
  expect_true("fig_km <- wrap_elements(full = fig)" %in% code)
  # the other one is made as fig_box itself (no fig_box <- fig)
  expect_true(any(startsWith(code, "fig_box <- ggplot()")))
  expect_false("fig_box <- fig" %in% code)
  expect_true("fig <- fig_km | fig_box" %in% code)
  expect_true("fig <- fig + plot_layout(widths = c(3, 2))" %in% code)
  expect_true("fig <- fig & theme(plot.tag = element_text(face = \"bold\"))" %in% code)
  expect_equal(sum(grepl("^ggsave\\(", code)), 1L)
  expect_true(any(grepl("^# ---- saving the figure", code)))
  expect_true("fig_width  <- 10" %in% code)
})

test_that("a composed figure draws", {
  skip_if_not_installed("ggsurvfit")
  skip_if_not_installed("patchwork")
  skip_if_not_installed("survival")
  r <- compose_run(two_up())
  expect_true(r$png)
  expect_s3_class(r$fig, "patchwork")
  # add_risktable: built, then wrapped
  d <- two_up(km_part("add"))
  code <- tfl_fig_design_code(d, "c")
  expect_true("fig_km <- wrap_elements(full = ggsurvfit_build(fig))" %in% code)
  expect_true(compose_run(d)$png)
})

test_that("layouts: names and | / + - ( ) only", {
  expect_equal(tflspec:::.fig_layout_code("(a | b) / c", c("a", "b", "c")), "(fig_a | fig_b) / fig_c")
  expect_error(tflspec:::.fig_layout_code("a * b", c("a", "b")), "only the plots")
  expect_error(tflspec:::.fig_layout_code("a | z", c("a", "b")), "no plot named z")
  # no layout: side by side
  d <- two_up()
  d$compose$layout <- NULL
  expect_true("fig <- fig_km | fig_box" %in% tfl_fig_design_code(d))
})

test_that("the check of a composed figure", {
  adam <- tfl_example_adam()
  expect_equal(nrow(tfl_check_fig_design(two_up(), adam)), 0L)
  d <- two_up()
  d$compose$layout <- "km / nope"
  d$plot$title <- "x"
  d$layers <- list(list(layer = "hline", yintercept = 1))
  d$plots$box$layers[[1]]$x <- "NOPE"
  d$plots$box$ggplot2_version <- "4.0"
  d$compose$add[[1]]$op <- "*"
  p <- tfl_check_fig_design(d, adam)
  expect_true(any(p$part == "compose" & p$problem == "no plot named nope"))
  expect_true(any(p$part == "compose" & p$problem == "plot box is not in the layout"))
  expect_true(any(p$part == "plot" & p$field == "title"))
  expect_true(any(p$part == "layers"))
  expect_true(any(startsWith(p$part, "plots$box layers[1]") & grepl("NOPE", p$problem)))
  expect_true(any(p$part == "plots$box" & p$field == "ggplot2_version"))
  expect_true(any(p$part == "compose.add[1]" & p$field == "op"))
  # the compose calls are checked like any call
  d <- two_up()
  d$compose$add <- list(list(fn = "plot_annotation", args = list(tag_levls = "A")))
  p <- tfl_check_fig_design(d, adam)
  expect_true(any(p$part == "compose.add[1]" & grepl("tag_levls", p$field)))
})

test_that("composed designs go through YAML and advice", {
  d <- two_up()
  f <- tempfile(fileext = ".yml")
  tfl_write_fig_design(d, f)
  back <- tfl_read_fig_design(f)
  expect_identical(tfl_fig_design_code(back, "c"), tfl_fig_design_code(d, "c"))
  expect_named(back$plots, c("km", "box"))
  # advice per plot, with fixes aimed at the plot
  d$plots$km$layers <- Filter(function(l) !identical(l$layer, "censor_mark"), d$plots$km$layers)
  a <- tfl_fig_advice(d, tfl_example_adam())
  i <- which(a$rule == "km_censor")
  expect_equal(a$part[i], "plots$km layers")
  d2 <- tfl_fig_apply_fix(d, a$fix[[i]])
  expect_true("censor_mark" %in% vapply(d2$plots$km$layers, `[[`, "", "layer"))
})

# ---- ggsurvfit add_* --------------------------------------------------------

test_that("ggsurvfit add_* need the KM curves; add_risktable only once", {
  adam <- tfl_example_adam()
  d <- tfl_fig_template("box_by_group")
  d$layers <- c(d$layers, list(list(layer = "call", fn = "add_quantile", package = "ggsurvfit")))
  p <- tfl_check_fig_design(d, adam)
  expect_true(any(grepl("add_quantile\\(\\) needs the KM curves layer", p$problem)))
  km <- km_part()
  km$layers <- c(km$layers, list(list(layer = "call", fn = "add_risktable", package = "ggsurvfit")))
  p <- tfl_check_fig_design(km, adam)
  expect_true(any(grepl("the number at risk twice", p$problem)))
  expect_equal(nrow(tfl_check_fig_design(km_part("add"), adam)), 0L)
})

test_that("add_risktable alone: ggsave keeps the table", {
  skip_if_not_installed("ggsurvfit")
  skip_if_not_installed("survival")
  d <- km_part("add")
  code <- tfl_fig_design_code(d, "k")
  expect_true(any(startsWith(code, "fig <- ggsurvfit(")))
  e <- new.env()
  adam <- tfl_example_adam()
  for (n in names(adam)) assign(tolower(n), adam[[n]], envir = e)
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  suppressWarnings(suppressMessages(eval(parse(text = code), envir = e)))
  expect_true(file.exists(e$fig_path))
  expect_s3_class(e$fig, "ggsurvfit")
})

# ---- the call catalog -------------------------------------------------------

test_that("catalog functions resolve to their package", {
  cc <- tfl_fig_calls()
  expect_named(cc, c("package", "fn", "where", "label", "help"))
  expect_true(all(c("ggh4x", "ggtext", "ggforce", "ggnewscale") %in% cc$package))
  expect_true(all(cc$where %in% c("layers", "plot.add", "nested")))
  expect_false(any(c("cowplot", "ggpubr", "ggbreak") %in% cc$package))
  d <- tfl_fig_template("box_by_group")
  d$plot$add <- list(list(fn = "theme", args = list(axis.title.y = list(fn = "element_markdown"))))
  d$layers <- c(d$layers, list(list(layer = "call", fn = "new_scale_colour")))
  code <- tfl_fig_design_code(d)
  expect_true(any(grepl("axis.title.y = ggtext::element_markdown()", code, fixed = TRUE)))
  expect_true(any(grepl("^  ggnewscale::new_scale_colour\\(\\)", code)))
  expect_true(any(grepl("# also needs: .*ggnewscale", code)))
  expect_true(any(grepl("ggtext", code[grepl("^# also needs", code)])))
})

test_that("the sina layer is in the geom catalog", {
  expect_true("sina" %in% tfl_fig_parts()$piece)
  d <- tfl_fig_template("box_by_group")
  d$layers <- list(list(layer = "sina", data = "df", x = "TRT01A", y = "AVAL"))
  code <- tfl_fig_design_code(d)
  expect_true(any(grepl("ggforce::geom_sina(", code, fixed = TRUE)))
})

test_that("an empty axis label is untitled: ggplot2 4.0's label attribute advice", {
  adam <- tfl_example_adam()
  b <- tfl_fig_template("box_by_group")
  a <- tfl_fig_advice(b, adam, ggplot2_version = "4.0")
  if (!is.null(attr(adam$ADLB$TRT01A, "label")) || !is.null(attr(adam$ADSL$TRT01A, "label"))) {
    expect_true("gg_label_attr" %in% a$rule)
  }
  expect_false("gg_label_attr" %in% tfl_fig_advice(b, adam, ggplot2_version = "3.5")$rule)
})
