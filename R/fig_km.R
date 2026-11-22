# ---- fig_km(): the Kaplan-Meier figure template function ---------------------------
#
# A figure template function (canon 310c8d6 and 3e2d421, named fig_<kind>()
# by 3583a6d): one call makes the whole KM figure -- the curves, the number at
# risk table, the legend, the annotations and the theme -- from a survfit2()
# fit and the ARD, and returns the figure built once (ggsurvfit_build(): a
# `+` after the build no longer reaches the curves).  It returns the figure
# object, never an image: the image's size, dpi and format are the content's
# tfl_image() (canon 3e2d421).
#
# The arguments: what a statistician decides a figure (one argument each),
# and the look and the wording a part (a list a part: censor_options ...,
# each name the argument it passes to).  A look argument left NULL takes the
# figure style standard's value (tfl_fig_style(): the study's figure_style
# when tflspec writes the call, else the company's option, else tflspec's
# own).  The one definition of the arguments is inst/spec/schema.yml's
# `fig_km:` entry (tfl_arg_defs(fn = "fig_km")); a test keeps the formals
# and its defaults the same.

# The options a part's list takes, and their defaults (NULL: the figure
# style's value)
.fig_km_option_defaults <- function() {
  list(
    curve_options = list(linewidth = NULL),
    censor_options = list(shape = NULL, size = NULL, stroke = NULL, legend = FALSE,
                          legend_label = "Censored"),
    ci_options = list(alpha = 0.2, band = "none", band_times = NULL),
    median_options = list(linetype = NULL, colour = NULL, linewidth = NULL),
    risktable_options = list(title = NULL, height = NULL, size = NULL, group = "auto",
                             symbol = FALSE),
    legend_options = list(title = NULL, corner = NULL,
                          median_text = "{arm}: {median} ({lcl}, {ucl})", median_digits = 1,
                          style = "data", ncol = NULL, nrow = NULL, border = FALSE,
                          item_labels = NULL, rel_size = NULL),
    annotation_options = list(position = NULL, pvalue_caption = "Log-rank {p.value}",
                              p_digits = 3, hr_text = "HR (95% CI): {hr} ({lcl}, {ucl})",
                              hr_digits = 2, size = NULL,
                              summary_stats = c("n", "events", "censored", "median"),
                              summary_digits = 1),
    theme_options = list(theme = NULL, base_size = NULL, axis_title_size = NULL,
                         axis_text_size = NULL, legend_text_size = NULL))
}

# A part's list over its defaults; a name the part does not take stops
.fig_km_options <- function(x, part) {
  d <- .fig_km_option_defaults()[[part]]
  if (is.null(x)) x <- list()
  if (!is.list(x) || (length(x) && (is.null(names(x)) || any(!nzchar(names(x)))))) {
    stop(sprintf("fig_km(): `%s` is a named list (%s).", part, paste(names(d), collapse = ", ")),
         call. = FALSE)
  }
  bad <- setdiff(names(x), names(d))
  if (length(bad)) {
    stop(sprintf("fig_km(): `%s` has no %s; it takes %s.", part, paste(bad, collapse = ", "),
                 paste(names(d), collapse = ", ")), call. = FALSE)
  }
  for (k in names(x)) if (!is.null(x[[k]])) d[[k]] <- x[[k]]
  d
}

.fig_km_corners <- c("top_right", "top_left", "bottom_left", "bottom_right")

# One value of a choice, said as the function's error
.fig_km_pick <- function(x, choices, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !x %in% choices) {
    stop(sprintf("fig_km(): `%s` is one of %s, not %s.", arg, paste(choices, collapse = ", "),
                 paste(format(x), collapse = " ")), call. = FALSE)
  }
  x
}

# A figure style setting (km's row over the blank one) as a number or text
.fig_km_fs <- function(key, num = TRUE) {
  v <- .fs_get(key, "km")
  if (is.na(v) || !nzchar(v)) return(NULL)
  if (num) as.numeric(v) else v
}

# A number or a name of the shapes the spec takes (plus, x, circle ...)
.fig_km_shape <- function(shape) {
  if (is.character(shape) && shape %in% names(pp_shape_names)) return(unname(pp_shape_names[[shape]]))
  if (is.character(shape) && !is.na(suppressWarnings(as.numeric(shape)))) return(as.numeric(shape))
  shape
}

# Values a level (colours, line types): a map by the level, or in order
.fig_km_by_level <- function(v, lv, arg) {
  v <- unlist(v)
  if (!is.null(names(v)) && all(nzchar(names(v)))) {
    miss <- setdiff(lv, names(v))
    if (length(miss)) {
      stop(sprintf("fig_km(): `%s` names no value for %s.", arg, paste(miss, collapse = ", ")),
           call. = FALSE)
    }
    return(stats::setNames(unname(v[lv]), lv))
  }
  stats::setNames(rep_len(unname(v), length(lv)), lv)
}

# breaks: one number an interval from `from`, several the positions; NULL
# ggplot2's
.fig_km_breaks <- function(b, from = 0, to = NULL) {
  if (is.null(b)) return(ggplot2::waiver())
  b <- as.numeric(unlist(b))
  if (length(b) == 1L) {
    if (!is.null(to) && is.finite(to)) return(seq(from, to, by = b))
    return(function(lim) seq(from, lim[2L], by = b))
  }
  b
}

# A number as the text a legend or an annotation shows; NA as NE
.fig_km_num <- function(x, digits) {
  ifelse(is.na(x), "NE", formatC(round(x, digits), format = "f", digits = digits))
}

.fig_km_p <- function(p, digits) {
  if (is.na(p)) return("NE")
  if (p < 10^-digits) return(paste0("<", formatC(10^-digits, format = "f", digits = digits)))
  formatC(round(p, digits), format = "f", digits = digits)
}

# {name} in a text filled from a named list
.fig_km_glue <- function(text, values) {
  for (k in names(values)) text <- gsub(paste0("{", k, "}"), values[[k]], text, fixed = TRUE)
  text
}

# Each level's median and its interval from the fit (quantile(): the fit's
# conf.int and conf.type)
.fig_km_medians <- function(km_fit, lv) {
  q <- stats::quantile(km_fit, probs = 0.5)
  pick <- function(m) {
    v <- as.numeric(if (is.matrix(m)) m[, 1L] else m)
    if (length(v) == length(lv)) v else rep(NA_real_, length(lv))
  }
  list(median = pick(q$quantile), lcl = pick(q$lower), ucl = pick(q$upper))
}

# The ARD's p-value of the log-rank test (analysis_id LOGRANK)
.fig_km_ard_p <- function(ard) {
  if (is.null(ard)) stop("fig_km(): pvalue = \"ard\" reads the ARD's LOGRANK rows; `ard` is NULL.", call. = FALSE)
  a <- .fig_fn_rows(ard, "LOGRANK")
  if (!NROW(a) || !all(c("stat_name", "stat") %in% names(a))) {
    stop("fig_km(): pvalue = \"ard\": the ARD has no LOGRANK rows (stat_name, stat).", call. = FALSE)
  }
  p <- .fig_fn_num(a$stat)[.fig_fn_chr(a$stat_name) %in% c("p.value", "p")]
  if (!length(p)) stop("fig_km(): the ARD's LOGRANK rows have no p.value.", call. = FALSE)
  p[[1L]]
}

# The log-rank p-value of the fit's groups (survival::survdiff() of the
# fit's own formula and data, as ggsurvfit's add_pvalue() computes it)
.fig_km_logrank_p <- function(km_fit) {
  if (is.null(km_fit$strata)) return(NA_real_)
  cl <- km_fit$call
  if (is.null(cl$formula) || is.null(cl$data)) {
    stop("fig_km(): pvalue = \"logrank\" needs a fit of survfit2(formula, data = ).", call. = FALSE)
  }
  # (survfit2() keeps the frame it was called from; else fig_km()'s caller)
  env <- km_fit[[".Environment"]] %||% attr(km_fit, ".Environment") %||% parent.frame(2L)
  data <- eval(cl$data, env)
  f <- eval(cl$formula, env)
  sd <- survival::survdiff(f, data = data)
  stats::pchisq(sd$chisq, df = length(sd$n) - 1L, lower.tail = FALSE)
}

# The ARD's hazard ratios (analysis_id HR): one line a compared arm
.fig_km_hr_lines <- function(ard, o) {
  if (is.null(ard)) stop("fig_km(): hr = TRUE reads the ARD's HR rows; `ard` is NULL.", call. = FALSE)
  a <- .fig_fn_rows(ard, "HR")
  if (!NROW(a) || !all(c("variable_level", "stat_name", "stat") %in% names(a))) {
    stop("fig_km(): hr = TRUE: the ARD has no HR rows (variable_level, stat_name, stat).", call. = FALSE)
  }
  arm <- .fig_fn_chr(a$variable_level)
  sn <- .fig_fn_chr(a$stat_name)
  val <- .fig_fn_num(a$stat)
  arms <- unique(arm[!is.na(arm) & sn == "conf.low" & !is.na(val)])
  if (!length(arms)) stop("fig_km(): the ARD's HR rows have no arm with an interval.", call. = FALSE)
  get <- function(a_lv, s) {
    v <- val[arm %in% a_lv & sn == s]
    if (length(v)) v[[1L]] else NA_real_
  }
  vapply(arms, function(x) {
    txt <- .fig_km_glue(o$hr_text, list(
      arm = x, hr = .fig_km_num(get(x, "estimate"), o$hr_digits),
      lcl = .fig_km_num(get(x, "conf.low"), o$hr_digits),
      ucl = .fig_km_num(get(x, "conf.high"), o$hr_digits)))
    if (length(arms) > 1L && !grepl("{arm}", o$hr_text, fixed = TRUE)) paste0(x, ": ", txt) else txt
  }, "", USE.NAMES = FALSE)
}

# The fit with its interval replaced by a simultaneous confidence band
# (km.ci::km.ci(), at the fit's level)
.fig_km_band <- function(km_fit, o, ci) {
  methods <- c("hall-wellner", "epband", "loghall", "logep")
  band <- .fig_km_pick(o$band, c("none", methods), "ci_options: band")
  if (ci == "none") {
    stop("fig_km(): ci_options: band draws the interval; set ci = \"ribbon\" or \"lines\".", call. = FALSE)
  }
  if (!requireNamespace("km.ci", quietly = TRUE)) {
    stop(paste0("fig_km(): ci_options: band = \"", band, "\" needs the package km.ci; ",
                "install it with install.packages(\"km.ci\")."), call. = FALSE)
  }
  level <- km_fit$conf.int %||% 0.95
  if (!any(abs(level - c(0.90, 0.95, 0.99)) < 1e-8)) {
    stop(sprintf(paste0("fig_km(): a simultaneous band is at the fit's confidence level, one of ",
                        "0.90, 0.95, 0.99 (km.ci's tables), not %s."), format(level)), call. = FALSE)
  }
  tw <- as.numeric(unlist(o$band_times))
  if (length(tw) && length(tw) != 2L) stop("fig_km(): ci_options: band_times is two numbers.", call. = FALSE)
  # km.ci() takes one curve whose first class is survfit (data.class()):
  # a survfit2 is passed as a plain survfit, a stratified fit one stratum
  # at a time
  one <- function(f) {
    class(f) <- "survfit"
    b <- km.ci::km.ci(f, conf.level = level, tl = if (length(tw)) tw[1L] else NA,
                      tu = if (length(tw)) tw[2L] else NA, method = band)
    # the band covers tl..tu only: km.ci() cuts the curve to those times;
    # put it back on the fit's times, NA (no band) outside them
    at <- match(b$time, f$time)
    if (anyNA(at)) {
      stop("fig_km(): km.ci() gave a band at times the fit has not got.", call. = FALSE)
    }
    lower <- upper <- rep(NA_real_, length(f$surv))
    lower[at] <- b$lower
    upper[at] <- b$upper
    list(lower = lower, upper = upper)
  }
  parts <- if (is.null(km_fit$strata)) list(one(km_fit))
           else lapply(seq_along(km_fit$strata), function(i) one(km_fit[i]))
  km_fit$lower <- unlist(lapply(parts, `[[`, "lower"), use.names = FALSE)
  km_fit$upper <- unlist(lapply(parts, `[[`, "upper"), use.names = FALSE)
  km_fit
}

# The y axis's range: the limits given, else the data's
.fig_km_y_range <- function(p, y_limits) {
  y_limits %||% range(ggplot2::layer_scales(p)$y$range$range %||% c(0, 1))
}

# A corner's place: the inside legend's position and justification, or an
# annotation's x, y, hjust, vjust within the panel's ranges
.fig_km_corner_xy <- function(corner) {
  switch(corner,
         top_right = c(0.98, 0.98, 1, 1), top_left = c(0.02, 0.98, 0, 1),
         bottom_left = c(0.02, 0.02, 0, 0), bottom_right = c(0.98, 0.02, 1, 0))
}

# The summary table's lines, a column a statistic, padded for a mono font:
# a group's N, events, censored and median (CI) (the fit's numbers)
.fig_km_summary_lines <- function(km_fit, lv, lab, stats, digits) {
  known <- c("n", "events", "censored", "median")
  bad <- setdiff(stats, known)
  if (length(bad) || !length(stats)) {
    stop(sprintf("fig_km(): annotation_options: summary_stats are some of %s, not %s.",
                 paste(known, collapse = ", "), paste(format(bad), collapse = ", ")), call. = FALSE)
  }
  strata <- if (is.null(km_fit$strata)) rep(1L, length(km_fit$time)) else
    rep(seq_along(km_fit$strata), km_fit$strata)
  n <- as.numeric(km_fit$n)
  ev <- vapply(seq_along(lv), function(i) sum(km_fit$n.event[strata == i]), 0)
  m <- .fig_km_medians(km_fit, lv)
  cols <- list(
    n = c("N", format(n, trim = TRUE)),
    events = c("Events", format(ev, trim = TRUE)),
    censored = c("Censored", format(n - ev, trim = TRUE)),
    median = c("Median (95% CI)", sprintf("%s (%s, %s)", .fig_km_num(m$median, digits),
                                          .fig_km_num(m$lcl, digits), .fig_km_num(m$ucl, digits))))
  cols <- c(list(c("", lab)), cols[stats])
  pad <- function(x) formatC(x, width = -max(nchar(x)))
  do.call(paste, c(lapply(cols, pad), sep = "  "))
}

# A plot of text lines alone (the summary table outside the curves)
.fig_km_text_plot <- function(lines, size) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0.5, label = paste(lines, collapse = "\n"), hjust = 0,
                      vjust = 0.5, family = "mono", size = size) +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::theme_void()
}

#' Kaplan-Meier figure: curves, number at risk, legend and annotations
#'
#' The figure template function of Kaplan-Meier curves: one call makes the
#' whole figure of a `survfit2()` fit -- the curves, the censor marks, the
#' confidence band, the median and landmark lines, the number at risk
#' table below, the legend (with N and the median), the p-value and the
#' hazard ratio, the theme -- and returns it built once (a patchwork of
#' the curves and the table when there is a table).  It is the figure
#' content of the template `km_simple` (`figure: fig_km`).
#'
#' The arguments are of two kinds.  What a statistician decides a figure
#' is one argument each (`type`, `ci`, `risktable`, `legend`, `pvalue` ...).
#' The look and the wording of a part is that part's list
#' (`censor_options = list(shape = "plus", size = 3)`): a name given
#' changes that one, the others keep their defaults.  A look left `NULL`
#' takes the figure style standard's value ([tfl_fig_style()]: the study's
#' `figure_style:` when tflspec writes the call, else the option
#' `tflspec.fig_style`, else tflspec's own).  What the arguments cannot
#' say is written with ggsurvfit's plot functions themselves (a code SPEC):
#' `+` after `fig_km()` does not reach the curves.
#'
#' The numbers come from the fit (the medians, the number at risk, the
#' unstratified log-rank p-value) and from the ARD (the N of the legend,
#' `BIGN`; the hazard ratio, `HR`; a stratified p-value, `LOGRANK`).  No
#' model is fitted here.
#'
#' @param km_fit The fit, `ggsurvfit::survfit2(Surv_CNSR(AVAL, CNSR) ~ <by>,
#'   data = adtte_sub)`.
#' @param ard The ARD, or `NULL`: the rows of `analysis_id` `"BIGN"`
#'   (stat `n` a group: the legend's N), `"HR"` (stats `estimate`,
#'   `conf.low`, `conf.high` an arm compared: `hr`) and `"LOGRANK"` (stat
#'   `p.value`: `pvalue = "ard"`).  An ARD without `analysis_id` is all
#'   BIGN rows.
#' @param time_unit The time's unit, `"days"`, `"weeks"`, `"months"` or
#'   `"years"`: the x axis title's `{time_unit}` (the time is in this unit
#'   already).
#' @param group_labels The groups' names in the legend, a map from the
#'   group's value (`list("Drug X" = "Drug X 10 mg")`) or in the groups'
#'   order; `NULL`: the values.
#' @param type The y axis: `"survival"` (survival probability), `"risk"`
#'   (cumulative incidence, 1 - S(t)), `"cumhaz"` (cumulative hazard) or
#'   `"cloglog"` (log(-log S(t))), ggsurvfit's types.
#' @param y_label The y axis title; `NULL`: the type's (`"Survival
#'   Probability"`, `"Cumulative Incidence"`, `"Cumulative Hazard"`,
#'   `"log(-log(Survival Probability))"`).
#' @param y_percent `TRUE`: the y axis in percent.
#' @param y_limits The y axis range, two numbers; `NULL`: the data's.
#'   `"cumhaz"` and `"cloglog"` take the data's unless it is given.
#' @param y_breaks The y axis ticks: one number, the interval; several, the
#'   positions.  `NULL`: the figure style's interval (`km.y_by`), ggplot2's
#'   for `"cumhaz"` and `"cloglog"`.
#' @param x_label The x axis title; `{time_unit}` is filled, title-cased
#'   (`"Time (Months)"`).
#' @param x_max The x axis's right end; `NULL`: the last time.
#' @param x_breaks The x axis ticks, one number the interval or several the
#'   positions; `NULL`: ggplot2's.  They are the number at risk table's
#'   times too, unless `risktable_times` says others.
#' @param colours The curves' colours, in the groups' order or a map by the
#'   group's value; `NULL`: the figure style's treatment palette.
#' @param linetypes The curves' line types, the same way; `NULL`: the figure
#'   style's `treatment_linetypes` (none: solid).
#' @param censor_mark `TRUE`: a mark at each censored time.
#' @param ci The confidence interval: `"none"`, `"ribbon"` (a band) or
#'   `"lines"` (dotted lines); the fit's level and transformation.
#' @param median_line `"none"`, `"h"` (a line at 50%) or `"hv"` (and a line
#'   down to each group's median).  Not for `"cumhaz"` and `"cloglog"`.
#' @param risktable `TRUE`: the number at risk table below the curves.
#' @param risktable_stats The table's rows, ggsurvfit's statistics
#'   (`"n.risk"`, `"cum.event"`, `"cum.censor"`, `"n.event"`, `"n.censor"`)
#'   or a glue of them (`"{n.risk} ({cum.event})"`).
#' @param risktable_times The table's times; `NULL`: the x axis ticks.
#' @param legend The legend's place: `"inside"`, `"bottom"`, `"right"` or
#'   `"none"`.
#' @param legend_n `TRUE`: each group's N (the ARD's `BIGN`) in the legend.
#' @param legend_median `TRUE`: each group's median (CI) in the legend.
#' @param pvalue `"none"`, `"logrank"` (the fit's unstratified log-rank
#'   test) or `"ard"` (the ARD's `LOGRANK` row: a stratified test, as the
#'   table shows it).
#' @param hr `TRUE`: the ARD's hazard ratio (CI), a line an arm compared.
#' @param landmark_times Times with a line up to the curves, as the median
#'   line looks; `NULL`: none.
#' @param summary_table A small table of each group's N, events, censored
#'   and median (95% CI) (the fit's numbers): `"none"`, inside the curves in
#'   a corner (`"top_right"`, `"top_left"`, `"bottom_left"`,
#'   `"bottom_right"`), or outside, `"below"` or `"right"` of the figure.
#' @param curve_options The curves' look: `linewidth`.
#' @param censor_options The censor marks' look: `shape` (a name, `"plus"`,
#'   `"x"`, `"circle"` ..., or a ggplot2 shape number), `size`, `stroke`;
#'   `legend` (`TRUE`: an entry in the legend explains the mark) and its
#'   text `legend_label` (`"Censored"`).
#' @param ci_options The interval's: `alpha` (the band's transparency,
#'   0.2); `band`, a simultaneous confidence band in place of the
#'   pointwise interval: `"none"`, `"hall-wellner"`, `"epband"` (equal
#'   precision), `"loghall"` or `"logep"` (their log forms), from
#'   `km.ci::km.ci()` (a Suggests package: install it with
#'   `install.packages("km.ci")`), at the fit's confidence level, which is
#'   then one of 0.90, 0.95, 0.99 (km.ci's tables); `band_times`, the
#'   band's time window, two numbers (`NULL`: km.ci's, the first to the last
#'   event time); outside it the curve has no band.  The legend's and
#'   the summary table's medians keep the pointwise interval.
#' @param median_options The median and landmark lines' look: `linetype`,
#'   `colour`, `linewidth`.
#' @param risktable_options The table's look: `title` (its heading),
#'   `height` (its share of the figure's height), `size` (its text),
#'   `group` (with several rows: `"auto"`, `"strata"` a table a group,
#'   `"risktable_stats"` a table a statistic), `symbol` (`TRUE`: the groups
#'   named by a mark of their curve's colour).
#' @param legend_options The legend's: `title` (`NULL`: none), `corner` of
#'   an inside legend (`"top_right"`, `"top_left"`, `"bottom_left"`,
#'   `"bottom_right"`; `NULL`: top right for `"survival"`, else top left),
#'   `median_text` (`"{arm}: {median} ({lcl}, {ucl})"`), `median_digits`
#'   (1); its `style`: `"data"` (ggplot2's legend of the curves, laid out
#'   as ggplot2 lays it out at its position) or `"combined"` (every item of every aesthetic -- the curves'
#'   colours, the censor mark's shape -- in one grid, below the figure,
#'   beside it (`legend = "right"`) or inside it).  The combined legend's
#'   grid is `ncol` x `nrow` (`NULL`: one row), `border` (`TRUE`: a box
#'   around it), `rel_size` its height below (its width beside) for the
#'   figure's 1 (`NULL`: 0.07 a row below, 0.3 beside), and `item_labels`
#'   relabels its items by aesthetic, then
#'   value (`list(colour = list("Drug X" = "Drug X 10 mg"), shape =
#'   list(Censored = "Censored subject"))`).
#' @param annotation_options The p-value and hazard ratio's: `position`
#'   (a corner; `NULL`: bottom left for `"survival"`, else top left, the
#'   bottom right when an inside legend is there), `pvalue_caption`
#'   (`"Log-rank {p.value}"`), `p_digits` (3), `hr_text`
#'   (`"HR (95% CI): {hr} ({lcl}, {ucl})"`; `{arm}` the arm), `hr_digits`
#'   (2), `size` (their text in points; `NULL`: 0.9 of the base size); the
#'   summary table's `summary_stats` (some of `"n"`, `"events"`,
#'   `"censored"`, `"median"`) and `summary_digits` (1).
#' @param theme_options The theme: `theme` (`"boxed"`, `"L_axis"`,
#'   `"minimal"`, `"classic"`) and `base_size`; the text sizes of the parts
#'   in points, `axis_title_size`, `axis_text_size` (the tick labels),
#'   `legend_text_size` (`NULL`: the theme's).  The risk table's text is
#'   `risktable_options`' `size`.
#' @return The figure, built: a ggplot (a patchwork of the curves and the
#'   table when `risktable = TRUE`, or a summary table or a combined legend
#'   outside) with the class `tfl_figure` added.
#'   Print it, or `ggplot2::ggsave()` it.
#' @seealso [tfl_figure_fn()], [forest_hr()], [tfl_fig_style()]
#' @examplesIf requireNamespace("ggsurvfit", quietly = TRUE) && requireNamespace("cards", quietly = TRUE)
#' fit <- ggsurvfit::survfit2(survival::Surv(time, status) ~ surg, data = ggsurvfit::df_colon)
#' bign <- cards::ard_tabulate(ggsurvfit::df_colon, variables = "surg")
#' fig_km(fit, bign, time_unit = "years", risktable = TRUE, ci = "ribbon",
#'        median_line = "hv", pvalue = "logrank", legend = "bottom")
#' @export
fig_km <- function(km_fit, ard = NULL,
                   time_unit = "days", group_labels = NULL,
                   type = "survival", y_label = NULL, y_percent = FALSE, y_limits = c(0, 1),
                   y_breaks = NULL,
                   x_label = "Time ({time_unit})", x_max = NULL, x_breaks = NULL,
                   colours = NULL, linetypes = NULL,
                   censor_mark = TRUE, ci = "none", median_line = "none",
                   risktable = FALSE, risktable_stats = "n.risk", risktable_times = NULL,
                   legend = "inside", legend_n = TRUE, legend_median = FALSE,
                   pvalue = "none", hr = FALSE, landmark_times = NULL, summary_table = "none",
                   curve_options = list(), censor_options = list(), ci_options = list(),
                   median_options = list(), risktable_options = list(), legend_options = list(),
                   annotation_options = list(), theme_options = list()) {
  .fig_fn_need(c("ggsurvfit", "survival"), "fig_km")
  if (!inherits(km_fit, "survfit")) stop("fig_km(): `km_fit` is a survfit object (survfit2()).", call. = FALSE)
  time_unit <- .fig_km_pick(time_unit, c("days", "weeks", "months", "years"), "time_unit")
  type <- .fig_km_pick(type, c("survival", "risk", "cumhaz", "cloglog"), "type")
  ci <- .fig_km_pick(ci, c("none", "ribbon", "lines"), "ci")
  median_line <- .fig_km_pick(median_line, c("none", "h", "hv"), "median_line")
  legend <- .fig_km_pick(legend, c("inside", "bottom", "right", "none"), "legend")
  pvalue <- .fig_km_pick(pvalue, c("none", "logrank", "ard"), "pvalue")
  summary_table <- .fig_km_pick(summary_table, c("none", .fig_km_corners, "below", "right"), "summary_table")
  co <- .fig_km_options(curve_options, "curve_options")
  cn <- .fig_km_options(censor_options, "censor_options")
  cio <- .fig_km_options(ci_options, "ci_options")
  mo <- .fig_km_options(median_options, "median_options")
  ro <- .fig_km_options(risktable_options, "risktable_options")
  lo <- .fig_km_options(legend_options, "legend_options")
  ao <- .fig_km_options(annotation_options, "annotation_options")
  th <- .fig_km_options(theme_options, "theme_options")
  ro$group <- .fig_km_pick(ro$group, c("auto", "strata", "risktable_stats"), "risktable_options: group")
  lo$style <- .fig_km_pick(lo$style, c("data", "combined"), "legend_options: style")
  base_size <- th$base_size %||% .fig_km_fs("base_size") %||% 10
  # the annotations' text (mm), 0.9 of the base size unless given (points)
  ann_size <- (ao$size %||% (base_size * 0.9)) / ggplot2::.pt
  # a probability's axis, or the data's
  prob <- type %in% c("survival", "risk")
  if (!prob) {
    if (isTRUE(y_percent)) stop(sprintf("fig_km(): y_percent is for a probability, not type = \"%s\".", type), call. = FALSE)
    if (median_line != "none") stop(sprintf("fig_km(): median_line is for a probability, not type = \"%s\".", type), call. = FALSE)
    if (missing(y_limits)) y_limits <- NULL
  }
  if (!is.null(y_limits) && (length(y_limits) != 2L || !is.numeric(unlist(y_limits)))) {
    stop("fig_km(): `y_limits` is two numbers.", call. = FALSE)
  }
  y_limits <- if (!is.null(y_limits)) as.numeric(unlist(y_limits))
  if (is.null(y_breaks) && prob) y_breaks <- .fig_km_fs("y_by")
  # the groups: a level of the by variable each (strata "TRT01A=Drug X")
  lv <- if (is.null(km_fit$strata)) "All" else sub("^[^=]*=", "", names(km_fit$strata))
  by <- if (is.null(km_fit$strata)) NULL else sub("=.*$", "", names(km_fit$strata)[[1L]])
  lab <- if (is.null(group_labels)) lv else unname(.fig_km_by_level(group_labels, lv, "group_labels"))
  lab <- as.character(lab)
  if (isTRUE(legend_n)) {
    n_lab <- .km_legend_labels(lv, ard, by)
    lab <- ifelse(n_lab == lv, lab, paste0(lab, substring(n_lab, nchar(lv) + 1L)))
  }
  if (isTRUE(legend_median)) {
    m <- .fig_km_medians(km_fit, lv)
    d <- lo$median_digits
    lab <- vapply(seq_along(lv), function(i) .fig_km_glue(lo$median_text, list(
      arm = lab[i], median = .fig_km_num(m$median[i], d),
      lcl = .fig_km_num(m$lcl[i], d), ucl = .fig_km_num(m$ucl[i], d))), "")
  }
  lab <- stats::setNames(lab, lv)
  pal <- .fig_km_by_level(colours %||% tfl_fig_palettes()[["treatment"]], lv, "colours")
  lt <- linetypes %||% {
    v <- .fig_km_fs("treatment_linetypes", num = FALSE)
    if (!is.null(v)) trimws(strsplit(v, ",", fixed = TRUE)[[1L]])
  }
  # the curves
  # a simultaneous band: the plotted fit's interval is the band's
  plot_fit <- if (!identical(cio$band, "none")) .fig_km_band(km_fit, cio, ci) else km_fit
  p <- ggsurvfit::ggsurvfit(plot_fit, type = type, linetype_aes = !is.null(lt),
                            linewidth = co$linewidth %||% .fig_km_fs("line_width") %||% 0.3)
  if (ci != "none") p <- p + ggsurvfit::add_confidence_interval(type = ci, alpha = cio$alpha)
  c_look <- list(shape = .fig_km_shape(cn$shape %||% .fig_km_fs("censor_shape", FALSE) %||% "plus"),
                 size = cn$size %||% .fig_km_fs("censor_size") %||% 3,
                 stroke = cn$stroke %||% .fig_km_fs("censor_stroke") %||% 0.6)
  if (isTRUE(censor_mark)) {
    p <- p + do.call(ggsurvfit::add_censor_mark, c_look)
  }
  # the censor mark explained in the legend: a shape key of its own (a
  # layer with no point drawn: its one row has no y)
  censor_key <- isTRUE(censor_mark) && isTRUE(cn$legend)
  if (censor_key) {
    p <- p +
      ggplot2::geom_point(data = data.frame(x = 0, y = NA_real_, key = cn$legend_label),
                          mapping = ggplot2::aes(x = .data$x, y = .data$y, shape = .data$key),
                          inherit.aes = FALSE, na.rm = TRUE, size = c_look$size, stroke = c_look$stroke) +
      ggplot2::scale_shape_manual(values = stats::setNames(c_look$shape, cn$legend_label), name = NULL)
  }
  m_look <- list(linetype = mo$linetype %||% .fig_km_fs("median_linetype", FALSE) %||% "dashed",
                 colour = mo$colour %||% .fig_km_fs("median_colour", FALSE) %||% "grey50",
                 linewidth = mo$linewidth %||% .fig_km_fs("median_line_width") %||% 0.3)
  if (median_line == "hv") {
    p <- p + do.call(ggsurvfit::add_quantile, c(list(y_value = 0.5), m_look))
  } else if (median_line == "h") {
    p <- p + do.call(ggplot2::geom_hline, c(list(yintercept = 0.5), m_look))
  }
  for (t in as.numeric(unlist(landmark_times))) {
    p <- p + do.call(ggsurvfit::add_quantile, c(list(x_value = t), m_look))
  }
  # the scales: colours (and fills, lines) a group, one legend
  p <- p + ggplot2::scale_colour_manual(values = pal, breaks = lv, labels = lab)
  if (ci == "ribbon") p <- p + ggplot2::scale_fill_manual(values = pal, breaks = lv, labels = lab)
  if (!is.null(lt)) {
    p <- p + ggplot2::scale_linetype_manual(values = .fig_km_by_level(lt, lv, "linetypes"),
                                            breaks = lv, labels = lab)
  }
  t_end <- if (is.null(x_max)) max(km_fit$time, na.rm = TRUE) else as.numeric(x_max)
  p <- p +
    ggplot2::scale_x_continuous(breaks = .fig_km_breaks(x_breaks, 0, t_end),
                                expand = ggplot2::expansion(mult = c(0, 0.03))) +
    ggplot2::scale_y_continuous(
      breaks = if (prob || !is.null(y_breaks)) .fig_km_breaks(y_breaks, y_limits[1L] %||% 0, y_limits[2L]) else ggplot2::waiver(),
      labels = if (isTRUE(y_percent)) scales::label_percent() else ggplot2::waiver(),
      expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::coord_cartesian(xlim = c(0, t_end), ylim = y_limits)
  x_label <- gsub("{time_unit}", tools::toTitleCase(time_unit), x_label, fixed = TRUE)
  y_label <- y_label %||% switch(type, survival = "Survival Probability", risk = "Cumulative Incidence",
                                 cumhaz = "Cumulative Hazard", cloglog = "log(-log(Survival Probability))")
  p <- p + ggplot2::labs(x = x_label, y = y_label)
  # the annotation: the p-value and the hazard ratios, in a corner
  ann <- c(
    if (pvalue != "none") {
      pv <- if (pvalue == "ard") .fig_km_ard_p(ard) else .fig_km_logrank_p(km_fit)
      .fig_km_glue(ao$pvalue_caption, list(p.value = .fig_km_p(pv, ao$p_digits)))
    },
    if (isTRUE(hr)) .fig_km_hr_lines(ard, ao))
  corner <- lo$corner %||% (if (type == "survival") "top_right" else "top_left")
  corner <- .fig_km_pick(corner, .fig_km_corners, "legend_options: corner")
  if (length(ann)) {
    pos <- ao$position %||% {
      a <- if (type == "survival") "bottom_left" else "top_left"
      if (legend == "inside" && a == corner) "bottom_right" else a
    }
    xy <- .fig_km_corner_xy(.fig_km_pick(pos, .fig_km_corners, "annotation_options: position"))
    yr <- .fig_km_y_range(p, y_limits)
    p <- p + ggplot2::annotate("text", x = xy[1L] * t_end, y = yr[1L] + xy[2L] * diff(yr),
                               label = paste(ann, collapse = "\n"), hjust = xy[3L], vjust = xy[4L],
                               size = ann_size)
  }
  # the summary table: in a corner, or outside (below, right) after the build
  sum_lines <- if (summary_table != "none") {
    .fig_km_summary_lines(km_fit, lv, unname(lab[lv]), ao$summary_stats, ao$summary_digits)
  }
  if (summary_table %in% .fig_km_corners) {
    xy <- .fig_km_corner_xy(summary_table)
    yr <- .fig_km_y_range(p, y_limits)
    p <- p + ggplot2::annotate("text", x = xy[1L] * t_end, y = yr[1L] + xy[2L] * diff(yr),
                               label = paste(sum_lines, collapse = "\n"), hjust = xy[3L], vjust = xy[4L],
                               family = "mono", size = ann_size)
  }
  # the theme, then the parts' text sizes, then the legend's place
  p <- p + .fig_fn_theme(th$theme %||% .fig_km_fs("theme", FALSE) %||% "boxed", base_size)
  sizes <- list(axis.title = th$axis_title_size, axis.text = th$axis_text_size,
                legend.text = th$legend_text_size)
  sizes <- Filter(Negate(is.null), sizes)
  if (length(sizes)) {
    p <- p + do.call(ggplot2::theme, lapply(sizes, function(v) ggplot2::element_text(size = v)))
  }
  if (!is.null(lo$title)) {
    p <- p + ggsurvfit::add_legend_title(lo$title)
  } else {
    p <- p + ggplot2::theme(legend.title = ggplot2::element_blank())
  }
  p <- p + switch(legend,
    inside = {
      xy <- .fig_km_corner_xy(corner)
      ggplot2::theme(legend.position = "inside", legend.position.inside = xy[1:2],
                     legend.justification.inside = xy[3:4])
    },
    none = ggplot2::theme(legend.position = "none"),
    ggplot2::theme(legend.position = legend))
  # the combined legend: every item of every aesthetic (the curves'
  # colour, the censor mark's shape) in one grid of its own, the data
  # legend off; an item's text relabelled by its aesthetic, then its value
  comb <- NULL
  if (lo$style == "combined" && legend != "none") {
    # (the items of the plot's scales: the curves' colour -- its fill and
    # line type alike -- and the censor mark's shape)
    items <- .fig_legend_items(p, keys = list(colour = "line", shape = "point"))
    items$label <- .fig_legend_relabel(items, lo$item_labels, "fig_km(): legend_options: item_labels")
    comb <- .fig_legend_plot(items, lo$ncol, lo$nrow, lo$border,
                             (th$legend_text_size %||% (base_size * 0.9)) / ggplot2::.pt)
    n_rows <- .fig_legend_grid(nrow(items), lo$ncol, lo$nrow)[["rows"]]
    p <- p + ggplot2::theme(legend.position = "none")
    if (legend == "inside") {
      xy <- .fig_km_corner_xy(corner)
      yr <- .fig_km_y_range(p, y_limits)
      w <- 0.45 * t_end
      h <- 0.08 * n_rows * diff(yr)
      x0 <- if (xy[3L] == 1) xy[1L] * t_end - w else xy[1L] * t_end
      y0 <- yr[1L] + xy[2L] * diff(yr) - if (xy[4L] == 1) h else 0
      p <- p + ggplot2::annotation_custom(ggplot2::ggplotGrob(comb), xmin = x0, xmax = x0 + w,
                                          ymin = y0, ymax = y0 + h)
      comb <- NULL
    }
  }
  # the number at risk table, below
  if (isTRUE(risktable)) {
    title <- ro$title %||% .fig_km_fs("risk_title", FALSE)
    rt <- list(risktable_stats = risktable_stats,
               risktable_height = ro$height %||% .fig_km_fs("risk_height") %||% 0.167,
               size = ro$size %||% .fig_km_fs("text_size") %||% 3,
               risktable_group = ro$group)
    if (!is.null(risktable_times)) rt$times <- as.numeric(unlist(risktable_times))
    if (!is.null(title) && length(title) == length(risktable_stats)) rt$stats_label <- title
    p <- p + do.call(ggsurvfit::add_risktable, rt)
    if (isTRUE(ro$symbol)) p <- p + ggsurvfit::add_risktable_strata_symbol()
  }
  out <- ggsurvfit::ggsurvfit_build(p)
  # what goes outside: the combined legend, the summary table
  if (!is.null(comb)) {
    out <- .fig_legend_attach(out, comb, if (legend == "right") "right" else "bottom", lo$rel_size)
  }
  if (summary_table %in% c("below", "right")) {
    tp <- .fig_km_text_plot(sum_lines, ann_size)
    out <- if (summary_table == "right") {
      patchwork::wrap_plots(out, tp, nrow = 1L, widths = c(1, 0.45))
    } else {
      patchwork::wrap_plots(out, tp, ncol = 1L, heights = c(1, 0.06 * length(sum_lines)))
    }
  }
  class(out) <- unique(c("tfl_figure", class(out)))
  out
}
