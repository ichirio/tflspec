# fig_km(): the Kaplan-Meier figure template function (R/fig_km.R) and its one
# definition, the schema's fig_km entry.  Fictional data only.

.fkm_adtte <- function(arms = c("Control", "Drug X")) {
  set.seed(20261119)
  n <- 30L * length(arms)
  rate <- rep(seq(1 / 200, 1 / 400, length.out = length(arms)), each = 30L)
  data.frame(
    USUBJID = sprintf("S-%03d", seq_len(n)),
    TRT01A = rep(arms, each = 30L),
    AVAL = round(stats::rexp(n, rate) + 1),
    CNSR = stats::rbinom(n, 1L, 0.3),
    stringsAsFactors = FALSE)
}

.fkm_fit <- function(d = .fkm_adtte()) {
  ggsurvfit::survfit2(ggsurvfit::Surv_CNSR(AVAL, CNSR) ~ TRT01A, data = d)
}

.fkm_bign <- function(d) {
  b <- cards::ard_tabulate(d, variables = "TRT01A")
  b$analysis_id <- "BIGN"
  as.data.frame(b)
}

.fkm_hr <- function(d) {
  a <- cardx::ard_regression(survival::coxph(ggsurvfit::Surv_CNSR(AVAL, CNSR) ~ TRT01A, data = d),
                             exponentiate = TRUE)
  a$analysis_id <- "HR"
  as.data.frame(a)
}

.fkm_need <- function() {
  skip_if_not_installed("ggplot2", "3.5.2")
  skip_if_not_installed("ggsurvfit")
  skip_if_not_installed("survival")
  skip_if_not_installed("patchwork")
}

# the colour scale's labels of a figure without a table (a ggplot)
.fkm_labels <- function(p) {
  unname(ggplot2::ggplot_build(p)$plot$scales$get_scales("colour")$get_labels())
}

test_that("fig_km()'s formals and defaults are its schema entry's (one definition)", {
  ent <- tfl_schema("fig_km")
  args <- ent$args
  top <- names(args)[vapply(args, function(a) is.null(a$member_of), NA)]
  f <- formals(fig_km)
  expect_setequal(names(f), top)
  for (k in top) {
    if (identical(k, "km_fit")) {
      expect_true(isTRUE(args[[k]]$required))
      next
    }
    d <- eval(f[[k]])
    expect_true("default" %in% names(args[[k]]), info = k)
    expect_equal(d, args[[k]]$default, ignore_attr = TRUE, info = k)
  }
  # the lists' entries: "<list>.<name>", the same names and defaults
  od <- .fig_km_option_defaults()
  mem <- names(args)[!names(args) %in% top]
  expect_setequal(unique(vapply(args[mem], `[[`, "", "member_of")), names(od))
  for (k in names(od)) {
    keys <- mem[vapply(args[mem], function(a) identical(a$member_of, k), NA)]
    expect_identical(keys, paste0(k, ".", names(od[[k]])), info = k)
    for (nm in names(od[[k]])) {
      expect_equal(od[[k]][[nm]], unlist(args[[paste0(k, ".", nm)]]$default), ignore_attr = TRUE,
                   info = paste(k, nm))
    }
  }
  # 25 arguments of their own, 8 lists of 40 entries
  expect_length(setdiff(top, c("km_fit", "ard", names(od))), 25L)
  expect_length(od, 8L)
  expect_length(mem, 40L)
  # each argument's group is one of the function's groups, in their order
  gid <- vapply(ent$groups, `[[`, "", "id")
  g <- vapply(args, `[[`, "", "group")
  expect_true(all(g %in% gid))
  expect_false(is.unsorted(match(g, gid)))
  # every argument has an English and a Japanese label
  for (k in names(args)) {
    expect_true(nzchar(args[[k]]$label$en %||% "") && nzchar(args[[k]]$label$ja %||% ""), info = k)
  }
})

test_that("tfl_arg_defs(fn = \"fig_km\") gives the card's columns from its schema entry", {
  d <- tfl_arg_defs(fn = "fig_km")
  ent <- tfl_schema("fig_km")
  expect_identical(d$name[d$name != "..."], names(ent$args))
  # the groups, in the entry's order, with their headings
  g <- attr(d, "groups")
  expect_identical(g$id, vapply(ent$groups, `[[`, "", "id"))
  expect_false(anyNA(g$label_en) || anyNA(g$label_ja))
  expect_true(all(d$group %in% g$id))
  # labels in English and Japanese; help, where an argument has it, in both
  expect_false(anyNA(d$label_en) || anyNA(d$label_ja))
  expect_identical(is.na(d$help_en), is.na(d$help_ja))
  # a YAML word read as a logical ("n") stays a word
  expect_identical(d$default[[which(d$name == "annotation_options.summary_stats")]][[1L]], "n")
  # tiers: the fit required, the main settings key, the rest other
  tier <- stats::setNames(d$tier, d$name)
  expect_identical(unname(tier["km_fit"]), "required")
  expect_true(all(tier[c("time_unit", "type", "ci", "risktable", "legend", "pvalue")] == "key"))
  expect_identical(unname(tier["censor_options.shape"]), "other")
  # nothing set: the fit stops, the rest are left out
  expect_identical(d$unset[d$name == "km_fit"], "stop")
  expect_true(all(d$unset[d$name != "km_fit"] == "omit"))
  # passes_to and member_of
  pt <- stats::setNames(d$passes_to, d$name)
  expect_identical(pt[["km_fit"]], "ggsurvfit.x")
  expect_identical(pt[["time_unit"]], character())
  expect_identical(d$member_of[d$name == "legend_options.ncol"], "legend_options")
  expect_true(is.na(d$member_of[d$name == "legend_options"]))
  expect_true(all(d$member_of[grepl("[.]", d$name)] == sub("[.].*$", "", d$name[grepl("[.]", d$name)])))
  # the figure style's choices
  expect_identical(d$choices[[which(d$name == "colours")]], "figure_style(palette)")
})

test_that("fig_km() builds the KM figure: N in the legend, the titles, the class", {
  .fkm_need()
  skip_if_not_installed("cards")
  d <- .fkm_adtte()
  p <- fig_km(.fkm_fit(d), .fkm_bign(d))
  expect_s3_class(p, "tfl_figure")
  expect_s3_class(p, "ggplot")
  expect_equal(.fkm_labels(p), c("Control (N=30)", "Drug X (N=30)"), ignore_attr = TRUE)
  expect_identical(ggplot2::get_labs(p)$x, "Time (Days)")
  expect_identical(ggplot2::get_labs(p)$y, "Survival Probability")
  expect_identical(p$theme$legend.position, "inside")
  # no ARD, or legend_n = FALSE: the groups alone; their own names
  expect_equal(.fkm_labels(fig_km(.fkm_fit(d))), c("Control", "Drug X"), ignore_attr = TRUE)
  expect_equal(.fkm_labels(fig_km(.fkm_fit(d), .fkm_bign(d), legend_n = FALSE,
                                  group_labels = list("Drug X" = "Drug X 10 mg", Control = "Placebo"))),
               c("Placebo", "Drug X 10 mg"), ignore_attr = TRUE)
  expect_equal(.fkm_labels(fig_km(.fkm_fit(d), .fkm_bign(d), group_labels = c("Placebo", "X"))),
               c("Placebo (N=30)", "X (N=30)"), ignore_attr = TRUE)
})

test_that("fig_km(): the y axis types, the axes and the percent", {
  .fkm_need()
  fit <- .fkm_fit()
  for (ty in c("survival", "risk", "cumhaz", "cloglog")) {
    p <- fig_km(fit, type = ty)
    expect_no_error(ggplot2::ggplot_build(p))
  }
  expect_identical(ggplot2::get_labs(fig_km(fit, type = "risk"))$y, "Cumulative Incidence")
  expect_identical(ggplot2::get_labs(fig_km(fit, type = "cumhaz", y_label = "H(t)"))$y, "H(t)")
  p <- fig_km(fit, y_percent = TRUE, x_max = 600, x_breaks = 100, time_unit = "weeks",
              x_label = "Weeks ({time_unit})", y_breaks = c(0, 0.5, 1))
  b <- ggplot2::ggplot_build(p)
  expect_identical(ggplot2::get_labs(p)$x, "Weeks (Weeks)")
  expect_equal(b$layout$panel_params[[1L]]$x$breaks[!is.na(b$layout$panel_params[[1L]]$x$breaks)],
               seq(0, 600, by = 100))
  expect_true(any(grepl("%", b$layout$panel_params[[1L]]$y$get_labels(), fixed = TRUE)))
  # cumhaz and cloglog: no percent, no median line
  expect_error(fig_km(fit, type = "cumhaz", y_percent = TRUE), "probability")
  expect_error(fig_km(fit, type = "cloglog", median_line = "h"), "probability")
  expect_error(fig_km(fit, y_limits = 1), "two numbers")
})

test_that("fig_km(): colours, line types and the figure style's defaults", {
  .fkm_need()
  fit <- .fkm_fit()
  # the curves' colours, in the groups' order
  vals <- function(p) {
    l <- ggplot2::ggplot_build(p)$data[[1L]]
    unique(l$colour[order(l$group)])
  }
  # the figure style's treatment palette by default
  p <- fig_km(fit)
  expect_identical(vals(p), unname(tfl_fig_palettes()[["treatment"]][1:2]))
  expect_identical(vals(fig_km(fit, colours = c("red", "blue"))), c("red", "blue"))
  expect_identical(vals(fig_km(fit, colours = list("Drug X" = "red", Control = "blue"))), c("blue", "red"))
  expect_error(fig_km(fit, colours = list(Control = "blue")), "Drug X")
  p <- fig_km(fit, linetypes = c("solid", "dashed"))
  expect_no_error(ggplot2::ggplot_build(p))
  expect_false(is.null(ggplot2::ggplot_build(p)$plot$scales$get_scales("linetype")))
  # a company's figure style (the option) changes the NULL defaults
  st <- tfl_fig_style()
  st$colors <- rbind(st$colors[st$colors$palette != "treatment", ],
                     data.frame(palette = "treatment", value = NA, colour = c("#111111", "#222222"),
                                note = "", stringsAsFactors = FALSE))
  old <- options(tflspec.fig_style = st)
  on.exit(options(old), add = TRUE)
  expect_identical(vals(fig_km(fit)), c("#111111", "#222222"))
})

test_that("fig_km(): the censor marks, the band, the median and landmark lines", {
  .fkm_need()
  fit <- .fkm_fit()
  geoms <- function(p) vapply(p$layers, function(l) class(l$geom)[1L], "")
  expect_true("GeomPoint" %in% geoms(fig_km(fit)))
  expect_false("GeomPoint" %in% geoms(fig_km(fit, censor_mark = FALSE)))
  p <- fig_km(fit, censor_options = list(shape = "x", size = 2, stroke = 1))
  pt <- p$layers[[which(geoms(p) == "GeomPoint")[1L]]]
  expect_equal(pt$aes_params$shape, 4)
  expect_equal(pt$aes_params$size, 2)
  for (ci in c("ribbon", "lines")) {
    expect_no_error(ggplot2::ggplot_build(fig_km(fit, ci = ci, ci_options = list(alpha = 0.3))))
  }
  expect_true("GeomHline" %in% geoms(fig_km(fit, median_line = "h")))
  for (ml in c("h", "hv")) {
    expect_no_error(ggplot2::ggplot_build(fig_km(fit, median_line = ml, landmark_times = c(100, 200),
                                                 median_options = list(colour = "red", linetype = "dotted",
                                                                       linewidth = 0.5))))
  }
})

test_that("fig_km(): the number at risk table below, its options", {
  .fkm_need()
  fit <- .fkm_fit()
  p <- fig_km(fit, risktable = TRUE)
  expect_s3_class(p, "tfl_figure")
  expect_s3_class(p, "patchwork")
  f <- tempfile(fileext = ".png")
  on.exit(unlink(f), add = TRUE)
  expect_no_error(ggplot2::ggsave(f, p, width = 7, height = 5, dpi = 72))
  expect_true(file.exists(f))
  p2 <- fig_km(fit, risktable = TRUE, risktable_stats = c("n.risk", "cum.event"),
               risktable_times = c(0, 200, 400),
               risktable_options = list(title = c("At risk", "Events"), height = 0.25, size = 2.5,
                                        group = "risktable_stats", symbol = TRUE))
  expect_no_error(ggplot2::ggsave(f, p2, width = 7, height = 5, dpi = 72))
  expect_error(fig_km(fit, risktable = TRUE, risktable_options = list(group = "rows")), "auto, strata")
})

test_that("fig_km(): the legend's place, title, corner, median", {
  .fkm_need()
  fit <- .fkm_fit()
  for (lg in c("inside", "bottom", "right", "none")) {
    p <- fig_km(fit, legend = lg)
    expect_identical(p$theme$legend.position, lg)
  }
  p <- fig_km(fit, legend_options = list(corner = "bottom_left", title = "Arm"))
  expect_equal(p$theme$legend.position.inside, c(0.02, 0.02))
  expect_no_error(ggplot2::ggplot_build(p))
  expect_equal(fig_km(fit, type = "risk")$theme$legend.position.inside, c(0.02, 0.98))
  lab <- .fkm_labels(fig_km(fit, legend_median = TRUE, legend_options = list(median_digits = 0)))
  expect_match(lab, "^(Control|Drug X): ([0-9]+|NE) \\(([0-9]+|NE), ([0-9]+|NE)\\)$")
  expect_error(fig_km(fit, legend = "left"), "inside, bottom, right, none")
  expect_error(fig_km(fit, legend_options = list(where = "top")), "has no where")
  expect_error(fig_km(fit, legend_options = "top"), "named list")
})

test_that("fig_km(): the p-value and the hazard ratio in a corner", {
  .fkm_need()
  skip_if_not_installed("cardx")
  skip_if_not_installed("broom.helpers")
  d <- .fkm_adtte(c("Control", "Drug X", "Drug Y"))
  fit <- .fkm_fit(d)
  txt <- function(p) {
    l <- p$layers[vapply(p$layers, function(x) inherits(x$geom, "GeomText"), NA)]
    if (length(l)) l[[1L]]$aes_params$label else NULL
  }
  expect_null(txt(fig_km(fit)))
  p <- fig_km(fit, pvalue = "logrank")
  sd <- survival::survdiff(ggsurvfit::Surv_CNSR(AVAL, CNSR) ~ TRT01A, data = d)
  pv <- stats::pchisq(sd$chisq, length(sd$n) - 1L, lower.tail = FALSE)
  expect_identical(txt(p), paste("Log-rank", .fig_km_p(pv, 3)))
  # the ARD's: a stratified p-value, the hazard ratios (one line an arm)
  ard <- rbind(
    data.frame(analysis_id = "LOGRANK", variable = "TRT01A", variable_level = NA, stat_name = "p.value",
               stat = I(list(0.00042)), stringsAsFactors = FALSE),
    .fkm_hr(d)[, c("analysis_id", "variable", "variable_level", "stat_name", "stat")])
  p <- fig_km(fit, ard, pvalue = "ard", hr = TRUE,
              annotation_options = list(pvalue_caption = "Stratified log-rank p {p.value}", hr_digits = 1,
                                        position = "top_right"))
  lines <- strsplit(txt(p), "\n", fixed = TRUE)[[1L]]
  expect_identical(lines[1L], "Stratified log-rank p <0.001")
  expect_length(lines, 3L)
  expect_match(lines[2:3], "^Drug [XY]: HR \\(95% CI\\): [0-9.]+ \\([0-9.]+, [0-9.]+\\)$")
  expect_no_error(ggplot2::ggplot_build(p))
  expect_error(fig_km(fit, pvalue = "ard"), "`ard` is NULL")
  expect_error(fig_km(fit, ard[ard$analysis_id == "HR", ], pvalue = "ard"), "no LOGRANK")
  expect_error(fig_km(fit, hr = TRUE), "`ard` is NULL")
})

test_that("fig_km(): the theme and what it stops on", {
  .fkm_need()
  fit <- .fkm_fit()
  for (th in c("boxed", "L_axis", "minimal", "classic")) {
    expect_no_error(ggplot2::ggplot_build(fig_km(fit, theme_options = list(theme = th, base_size = 12))))
  }
  expect_error(fig_km(list()), "survfit")
  expect_error(fig_km(fit, type = "hazard"), "survival, risk, cumhaz, cloglog")
  expect_error(fig_km(fit, curve_options = list(width = 1)), "has no width")
  expect_error(fig_km(fit, time_unit = "hours"), "days, weeks, months, years")
})

# the default figure's parts, for the four options off by default
.fkm_parts <- function(p) {
  list(geoms = vapply(p$layers, function(l) class(l$geom)[1L], ""),
       shape = !is.null(p$scales$get_scales("shape")),
       sizes = list(p$theme$axis.title$size, p$theme$axis.text$size, p$theme$legend.text$size),
       class = class(p))
}

test_that("fig_km(): the summary table, off by default, inside or outside", {
  .fkm_need()
  fit <- .fkm_fit()
  base <- .fkm_parts(fig_km(fit))
  expect_identical(.fkm_parts(fig_km(fit, summary_table = "none")), base)
  expect_false(any(base$geoms == "GeomText"))
  p <- fig_km(fit, summary_table = "bottom_left")
  l <- p$layers[[which(vapply(p$layers, function(x) inherits(x$geom, "GeomText"), NA))[1L]]]
  expect_identical(l$aes_params$family, "mono")
  lines <- strsplit(l$aes_params$label, "\n", fixed = TRUE)[[1L]]
  expect_length(lines, 3L)
  expect_match(lines[1L], "N +Events +Censored +Median [(]95% CI[)]")
  expect_match(lines[2L], "^Control +30 +[0-9]+ +[0-9]+ ")
  p <- fig_km(fit, summary_table = "below", annotation_options = list(summary_stats = c("n", "median")))
  expect_s3_class(p, "patchwork")
  expect_s3_class(p, "tfl_figure")
  f <- tempfile(fileext = ".png")
  on.exit(unlink(f), add = TRUE)
  expect_no_error(ggplot2::ggsave(f, p, width = 7, height = 5, dpi = 72))
  expect_no_error(ggplot2::ggsave(f, fig_km(fit, risktable = TRUE, summary_table = "right"),
                                  width = 9, height = 5, dpi = 72))
  expect_error(fig_km(fit, summary_table = "top"), "none, top_right")
  expect_error(fig_km(fit, summary_table = "below", annotation_options = list(summary_stats = "mean")),
               "summary_stats")
})

test_that("fig_km(): the censor mark explained in the legend, off by default", {
  .fkm_need()
  fit <- .fkm_fit()
  expect_false(.fkm_parts(fig_km(fit))$shape)
  p <- fig_km(fit, censor_options = list(legend = TRUE, legend_label = "Censored subject", shape = "x"))
  sc <- p$scales$get_scales("shape")
  expect_false(is.null(sc))
  b <- ggplot2::ggplot_build(p)
  expect_identical(unname(b$plot$scales$get_scales("shape")$get_labels()), "Censored subject")
  # no censor mark drawn: no entry
  expect_null(fig_km(fit, censor_mark = FALSE, censor_options = list(legend = TRUE))$scales$get_scales("shape"))
})

test_that("fig_km(): the combined legend, a grid of every item; the data legend as today", {
  .fkm_need()
  fit <- .fkm_fit()
  base <- fig_km(fit)
  expect_identical(.fkm_parts(fig_km(fit, legend_options = list(style = "data"))), .fkm_parts(base))
  expect_identical(base$theme$legend.position, "inside")
  # below the figure: a patchwork of the figure and the grid, the data legend off
  p <- fig_km(fit, legend = "bottom", censor_options = list(legend = TRUE),
              legend_options = list(style = "combined", ncol = 2, border = TRUE,
                                    item_labels = list(colour = list("Drug X" = "Drug X 10 mg"),
                                                       shape = list(Censored = "Censored subject"))))
  expect_s3_class(p, "patchwork")
  leg <- p[[2L]]
  txt <- ggplot2::ggplot_build(leg)$data[[length(leg$layers)]]
  expect_identical(txt$label, c("Control", "Drug X 10 mg", "Censored subject"))
  # two columns: three items in two rows
  expect_identical(sort(unique(txt$y)), c(-1, 0))
  expect_s3_class(leg$theme$plot.background, "element_rect")
  f <- tempfile(fileext = ".png")
  on.exit(unlink(f), add = TRUE)
  expect_no_error(ggplot2::ggsave(f, p, width = 7, height = 5, dpi = 72))
  # inside: drawn in the curves' panel; right: beside the figure
  p <- fig_km(fit, legend_options = list(style = "combined"))
  expect_true(any(vapply(p$layers, function(l) inherits(l$geom, "GeomCustomAnn"), NA)))
  expect_identical(p$theme$legend.position, "none")
  expect_s3_class(fig_km(fit, legend = "right", legend_options = list(style = "combined")), "patchwork")
  expect_error(fig_km(fit, legend_options = list(style = "grid")), "data, combined")
  expect_error(fig_km(fit, legend_options = list(style = "combined", item_labels = list(fill = list(A = "a")))),
               "no aesthetic fill")
})

test_that("fig_km(): the parts' text sizes, the theme's by default", {
  .fkm_need()
  fit <- .fkm_fit()
  base <- .fkm_parts(fig_km(fit))
  p <- fig_km(fit, theme_options = list(axis_title_size = 10, axis_text_size = 9, legend_text_size = 8),
              annotation_options = list(size = 7), pvalue = "logrank")
  expect_identical(p$theme$axis.title$size, 10)
  expect_identical(p$theme$axis.text$size, 9)
  expect_identical(p$theme$legend.text$size, 8)
  l <- p$layers[[which(vapply(p$layers, function(x) inherits(x$geom, "GeomText"), NA))[1L]]]
  expect_equal(l$aes_params$size, 7 / ggplot2::.pt)
  q <- fig_km(fit, pvalue = "logrank")
  l <- q$layers[[which(vapply(q$layers, function(x) inherits(x$geom, "GeomText"), NA))[1L]]]
  expect_equal(l$aes_params$size, 10 * 0.9 / ggplot2::.pt)
  expect_false(identical(.fkm_parts(p)$sizes, base$sizes))
})

test_that("fig_km(): a simultaneous confidence band, off by default (km.ci)", {
  .fkm_need()
  fit <- .fkm_fit()
  base <- ggplot2::ggplot_build(fig_km(fit, ci = "ribbon"))
  expect_identical(ggplot2::ggplot_build(fig_km(fit, ci = "ribbon", ci_options = list(band = "none")))$data,
                   base$data)
  expect_error(fig_km(fit, ci_options = list(band = "logep")), "set ci")
  expect_error(fig_km(fit, ci = "ribbon", ci_options = list(band = "nair")), "hall-wellner")
  skip_if_not_installed("km.ci")
  for (m in c("hall-wellner", "epband", "loghall", "logep")) {
    b <- ggplot2::ggplot_build(fig_km(fit, ci = "ribbon", ci_options = list(band = m)))
    rib <- b$data[[which(vapply(b$plot$layers, function(l) inherits(l$geom, "GeomRibbon"), NA))[1L]]]
    rib0 <- base$data[[which(vapply(base$plot$layers, function(l) inherits(l$geom, "GeomRibbon"), NA))[1L]]]
    expect_false(isTRUE(all.equal(rib$ymin, rib0$ymin)), info = m)
  }
  fit90 <- ggsurvfit::survfit2(ggsurvfit::Surv_CNSR(AVAL, CNSR) ~ TRT01A, data = .fkm_adtte(), conf.int = 0.8)
  expect_error(fig_km(fit90, ci = "lines", ci_options = list(band = "logep")), "0.90, 0.95, 0.99")
})

test_that("the flat legend helper: items of several aesthetics, relabelled by aesthetic then value", {
  skip_if_not_installed("ggplot2", "3.5.2")
  skip_if_not_installed("patchwork")
  it <- rbind(.fig_legend_table(aes = "fill", value = c("CR", "PR"), label = c("CR", "PR"), key = "rect",
                                fill = c("green", "blue")),
              .fig_legend_table(aes = "colour", value = "CR", label = "CR", key = "point", shape = 23))
  lab <- .fig_legend_relabel(it, list(fill = list(CR = "Best overall CR")))
  expect_identical(lab, c("Best overall CR", "PR", "CR"))
  expect_error(.fig_legend_relabel(it, list(shape = list(CR = "x"))), "no aesthetic shape")
  expect_error(.fig_legend_table(aes = "fill", value = "a", label = "a", key = "bar"), "line, point or rect")
  expect_identical(.fig_legend_grid(5, n_rows = 2), c(cols = 3, rows = 2))
  expect_identical(.fig_legend_grid(3), c(cols = 3, rows = 1))
  it$label <- lab
  p <- .fig_legend_plot(it, n_cols = 2, border = TRUE)
  b <- ggplot2::ggplot_build(p)
  expect_identical(unname(vapply(p$layers, function(l) class(l$geom)[1L], "")), c("GeomRect", "GeomPoint", "GeomText"))
  tx <- b$data[[3L]]
  expect_identical(tx$label, c("Best overall CR", "PR", "CR"))
  # row by row; the second column starts after the first's longest text
  expect_equal(tx$y, c(0, 0, -1))
  expect_equal(tx$x[2L] - tx$x[1L], 2 + 1 + nchar("Best overall CR") + 3)
  expect_equal(b$data[[1L]]$fill, c("green", "blue"))
  expect_s3_class(.fig_legend_attach(ggplot2::ggplot(), p, "bottom"), "patchwork")
  expect_s3_class(.fig_legend_attach(ggplot2::ggplot(), p, "right", rel_size = 0.5), "patchwork")
  # from a plot's scales: a colour and its fill merge into one item, a shape is its own
  d <- data.frame(x = 1:4, y = 1:4, g = c("A", "A", "B", "B"), s = "Censored")
  gp <- ggplot2::ggplot(d, ggplot2::aes(.data$x, .data$y, colour = .data$g, fill = .data$g)) +
    ggplot2::geom_line() + ggplot2::geom_point(ggplot2::aes(shape = .data$s), size = 3) +
    ggplot2::scale_colour_manual(values = c(A = "red", B = "blue")) +
    ggplot2::scale_fill_manual(values = c(A = "pink", B = "cyan")) +
    ggplot2::scale_shape_manual(values = c(Censored = 3))
  its <- .fig_legend_items(gp)
  expect_identical(its$aes, c("colour", "colour", "shape"))
  expect_identical(its$key, c("line", "line", "point"))
  expect_equal(its$fill[1:2], c("pink", "cyan"), ignore_attr = TRUE)
  expect_equal(its$size[3L], 3)
})
