# ---- km: the Kaplan-Meier layout ------------------------------------------------
# (registered at the end of tpl_registry.R, with the other figure templates)
#
# One layout, `km`: the curves, the censor marks, the confidence interval
# (bands, or bars at the axis's times), the number at risk, the median's
# line and the log-rank p-value -- each an argument.  The display
# templates (`km_disc`, FDA STF Figure 1; `km_ae_onset`, Figure 4;
# `km_lfu`, Figure 13) and the former templates `km_simple`,
# `km_risk_table`, `km_ci` are its variants: parameter sets, no body.  A
# report's own layers go after the call with `+`.

# The arguments the call takes that are the figure's settings (`plot:`)
# by the same name: a report's own `plot:` value of one is written as the
# argument in its program
.tpl_km_plot_args <- c("title", "x_label", "y_label", "legend", "theme", "base_size", "palette")

# The figure style standard's keys a parameter takes its study value from
# (tfl_param(study = "km.<key>")); TRUE: a number
.tpl_km_style_keys <- c(censor_shape = FALSE, censor_size = TRUE, censor_stroke = TRUE,
                        median_linetype = FALSE, median_colour = FALSE,
                        median_line_width = TRUE, risk_title = FALSE, risk_height = TRUE,
                        text_size = TRUE, line_width = TRUE, y_by = TRUE, base_size = TRUE,
                        theme = FALSE)

# The study's figure style (options(tflspec.fig_style =)): a km key's value
# when it is not tflspec's own (NULL: the template's default, the same)
.tpl_km_style_sources <- function() {
  lapply(stats::setNames(nm = paste0("km.", names(.tpl_km_style_keys))), function(src) {
    key <- sub("^km[.]", "", src)
    function(output, study) {
      v <- .fs_get(key, "km")
      if (is.na(v) || identical(v, .fs_get(key, "km", .fig_style_builtin()))) return(NULL)
      if (.tpl_km_style_keys[[key]]) as.numeric(v) else v
    }
  })
}

.tpl_km_params <- function() {
  d <- function(p) {
    p$stage <- "data"
    p
  }
  list(
    d(tfl_param("population", NULL, type = "text", tier = "usual",
                doc = "the analysis set's id (SAF): its subjects kept; NULL: every record")),
    d(.tpl_subset_param()),
    d(tfl_param("data", "ADTTE", type = "text", doc = "the dataset (ADTTE; ADSL with `event`)")),
    d(tfl_param("param", NULL, type = "levels", of = "PARAMCD", tier = "usual",
                doc = "the PARAMCD of ADTTE (filter(PARAMCD == )); NULL: no step (a dataset not BDS)")),
    d(.tpl_where_param(doc = "the records kept besides the parameter (R)")),
    d(tfl_param("event", NULL, type = "text",
                doc = "the event, in R (EOTSTT == \"DISCONTINUED\"): CNSR made from it (data without CNSR: ADSL)")),
    tfl_param("time", "AVAL", type = "vars", tier = "usual",
              doc = "the time variable (Surv(time, ...)), in `time_unit`"),
    tfl_param("time_unit", "months", type = "choice", choices = c("days", "weeks", "months", "years"),
              tier = "usual", doc = "the time's unit: the data step's days -> unit, the x axis's title"),
    tfl_param("censor", "CNSR", type = "vars", tier = "usual",
              doc = "the censoring variable, 1 censored (Surv(time, censor == 0))"),
    tfl_param("group", "TRT01A", type = "vars", tier = "usual",
              doc = "the curves' variable (survfit2(~ group), the colours)"),
    tfl_param("y_type", "survival", type = "choice", choices = c("survival", "risk"), tier = "usual",
              doc = "the y axis: survival S(t), or risk 1 - S(t) (ggsurvfit(type = ))"),
    tfl_param("y_percent", FALSE, type = "flag",
              doc = "TRUE: the y axis in % (scale_y_continuous(labels = scales::label_percent()))"),
    tfl_param("ci", "none", type = "choice", choices = c("none", "band", "bars"), tier = "usual",
              doc = "the confidence interval: band (add_confidence_interval()), bars at the axis's times (geom_errorbar())"),
    tfl_param("ci_alpha", NULL, type = "number",
              doc = "the band's transparency (add_confidence_interval(alpha = )); NULL: 0.2"),
    tfl_param("ci_times", NULL, type = "number", multiple = TRUE,
              doc = "the bars' times (summary(fit, times = )); NULL: the x axis's breaks"),
    tfl_param("conf_level", NULL, type = "number",
              doc = "the interval's level (survfit2(conf.int = )); NULL: 0.95"),
    tfl_param("conf_type", NULL, type = "choice", choices = c("log", "log-log", "plain"),
              doc = "the interval's transformation (survfit2(conf.type = )); NULL: log"),
    tfl_param("censor_marks", TRUE, type = "flag", tier = "usual",
              doc = "the censor marks (add_censor_mark())"),
    tfl_param("censor_shape", NULL, type = "text", study = "km.censor_shape",
              doc = "the censor mark's shape (add_censor_mark(shape = )); NULL: the figure style's"),
    tfl_param("censor_size", NULL, type = "number", study = "km.censor_size",
              doc = "the censor mark's size (add_censor_mark(size = )); NULL: the figure style's"),
    tfl_param("censor_stroke", NULL, type = "number", study = "km.censor_stroke",
              doc = "the censor mark's stroke (add_censor_mark(stroke = )); NULL: the figure style's"),
    tfl_param("median_line", FALSE, type = "flag", tier = "usual",
              doc = "the line at 50% (geom_hline(yintercept = 0.5))"),
    tfl_param("median_linetype", NULL, type = "text", study = "km.median_linetype",
              doc = "the median line's type (geom_hline(linetype = )); NULL: the figure style's"),
    tfl_param("median_colour", NULL, type = "text", study = "km.median_colour",
              doc = "the median line's colour (geom_hline(colour = )); NULL: the figure style's"),
    tfl_param("median_linewidth", NULL, type = "number", study = "km.median_line_width",
              doc = "the median line's width (geom_hline(linewidth = )); NULL: the figure style's"),
    tfl_param("risk_table", TRUE, type = "flag", tier = "usual",
              doc = "the number at risk under the curves (add_risktable(times = x_breaks))"),
    tfl_param("risk_title", NULL, type = "text", study = "km.risk_title",
              doc = "the risk table's title (add_risktable(stats_label = )); NULL: the figure style's"),
    tfl_param("risk_stats", NULL, type = "text", multiple = TRUE,
              doc = "the risk table's rows (add_risktable(risktable_stats = )); NULL: n.risk"),
    tfl_param("risk_height", NULL, type = "number", study = "km.risk_height",
              doc = "the risk table's height, a share (add_risktable(risktable_height = ))"),
    tfl_param("risk_size", NULL, type = "number", study = "km.text_size",
              doc = "the risk table's text size (add_risktable(size = ))"),
    tfl_param("x_max", NULL, type = "number", tier = "usual",
              doc = "the x axis's end (x_breaks <- seq(0, x_max, by = x_by)); NULL: the last time"),
    tfl_param("x_by", NULL, type = "number", tier = "usual",
              doc = "the x axis's step, the risk table's times; NULL: pretty()"),
    tfl_param("x_label", NULL, type = "text", tier = "usual",
              doc = "the x axis's title (labs(x = )); NULL: Time (<unit>)"),
    tfl_param("y_min", NULL, type = "number", doc = "the y axis's start (coord_cartesian(ylim = )); NULL: 0"),
    tfl_param("y_max", NULL, type = "number", doc = "the y axis's end; NULL: 1"),
    tfl_param("y_by", NULL, type = "number", study = "km.y_by",
              doc = "the y axis's step (scale_y_continuous(breaks = )); NULL: the figure style's"),
    tfl_param("y_label", NULL, type = "text", tier = "usual",
              doc = "the y axis's title (labs(y = )); NULL: by y_type"),
    tfl_param("linewidth", NULL, type = "number", study = "km.line_width",
              doc = "the curves' width (ggsurvfit(linewidth = )); NULL: the figure style's"),
    tfl_param("palette", NULL, type = "choice", choices = names(tfl_fig_palettes()),
              doc = "the curves' colours (scale_colour_manual(values = pal)); NULL: treatment"),
    tfl_param("linetype_by", FALSE, type = "flag",
              doc = "TRUE: a line type an arm too (ggsurvfit(linetype_aes = TRUE))"),
    tfl_param("legend", NULL, type = "choice", choices = .fig_legends, tier = "usual",
              doc = "the legend's place (theme(legend.position = )); NULL: inside"),
    tfl_param("legend_title", NULL, type = "text",
              doc = "the legend's title (labs(colour = )); NULL: none"),
    tfl_param("pvalue", FALSE, type = "flag", tier = "usual",
              doc = "the log-rank p-value (add_pvalue())"),
    tfl_param("pvalue_location", "annotation", type = "choice", choices = c("annotation", "caption"),
              doc = "where the p-value goes (add_pvalue(location = ))"),
    tfl_param("title", NULL, type = "text", doc = "the figure's title (labs(title = ))"),
    tfl_param("theme", NULL, type = "choice", choices = pp_themes, study = "km.theme",
              doc = "the theme; NULL: the figure style's"),
    tfl_param("base_size", NULL, type = "number", study = "km.base_size",
              doc = "the theme's base font size; NULL: the figure style's"),
    tfl_param("plot", NULL, type = "list",
              doc = "any other figure setting (plot:): dpi, facet_by, add ..., over the template's")
  )
}

# A list's items without those of `fn`; an item's args changed by `set`
.tpl_km_drop <- function(items, fn) Filter(function(it) !identical(it$fn, fn), items)
.tpl_km_set <- function(items, fn, ...) {
  set <- Filter(Negate(is.null), list(...))
  lapply(items, function(it) {
    if (identical(it$fn, fn)) for (k in names(set)) it$args[[k]] <- set[[k]]
    it
  })
}

# The layout's design: the preset's (tfl_fig_template("km_ci"), every
# layer) with the arguments' changes.  The steps before the fit are the
# program's; the fit and after it the call's (`stats`).
.tpl_km_layout <- function(a) {
  censor <- if (!is.null(a$event)) "CNSR" else a$censor %||% "CNSR"
  unit <- a$time_unit %||% "months"
  f <- .tpl_fig_design("km_ci", list(data = a$data, param = a$param %||% "OS", group = a$group,
                                     time = a$time, censor = censor, time_unit = unit), "survfit")
  kinds <- function() vapply(f$steps, function(s) s$step %||% "", "")
  # no parameter: no step (a dataset not BDS, ADSL)
  if (is.null(a$param)) f$steps <- f$steps[kinds() != "param"]
  # the event: CNSR made from it, before the time's unit
  if (!is.null(a$event)) {
    at <- match("time_unit", kinds(), nomatch = length(f$steps) + 1L) - 1L
    f$steps <- append(f$steps, list(list(step = "derive", variable = "CNSR",
                                         expr = sprintf("as.integer(!(%s))", a$event))), at)
  }
  # (days: no step)
  if (identical(unit, "days")) f$steps <- f$steps[kinds() != "time_unit"]
  if (!is.null(a$conf_type)) f$stats[[1L]]$conf_type <- a$conf_type
  if (!is.null(a$conf_level)) f$stats[[1L]]$conf_level <- a$conf_level
  # the layers
  ct <- f$content
  ct <- .tpl_km_set(ct, "km_curve", linewidth = a$linewidth,
                    type = if (identical(a$y_type, "risk")) "risk",
                    .linetype_by = if (isTRUE(a$linetype_by)) TRUE)
  ct <- if (identical(a$ci, "band")) .tpl_km_set(ct, "km_ci", alpha = a$ci_alpha) else .tpl_km_drop(ct, "km_ci")
  ct <- if (isTRUE(a$censor_marks %||% TRUE)) {
    .tpl_km_set(ct, "censor_mark", shape = a$censor_shape, size = a$censor_size, stroke = a$censor_stroke)
  } else .tpl_km_drop(ct, "censor_mark")
  ct <- if (isTRUE(a$median_line)) {
    .tpl_km_set(ct, "hline", linetype = a$median_linetype, colour = a$median_colour,
                linewidth = a$median_linewidth)
  } else .tpl_km_drop(ct, "hline")
  rt <- isTRUE(a$risk_table %||% TRUE)
  ct <- if (rt) .tpl_km_set(ct, "risk_table", title = a$risk_title, size = a$risk_size,
                            height = a$risk_height) else .tpl_km_drop(ct, "risk_table")
  f$content <- ct
  # the figure's settings the arguments say
  pl <- list(title = a$title, x_label = a$x_label, legend = a$legend, theme = a$theme,
             base_size = a$base_size, palette = a$palette, y_min = a$y_min, y_max = a$y_max,
             y_by = a$y_by)
  if (identical(a$y_type, "risk") && is.null(a$y_label)) {
    pl$y_label <- if (isTRUE(a$y_percent)) "Cumulative Percentage of Subjects" else "Cumulative Probability"
  }
  if (!is.null(a$y_label)) pl$y_label <- a$y_label
  for (k in names(pl)) if (!is.null(pl[[k]])) f$plot[[k]] <- pl[[k]]
  # the x axis's breaks (the call's, after the fit): the risk table's times,
  # the bars' times, the axis to the last of them
  bars <- identical(a$ci, "bars")
  if (rt || bars || !is.null(a$x_max) || !is.null(a$x_by)) {
    to <- if (!is.null(a$x_max)) format(a$x_max) else "max(fit$time)"
    br <- if (!is.null(a$x_by)) sprintf("seq(0, %s, by = %s)", to, format(a$x_by))
          else sprintf("pretty(c(0, %s))", to)
    f$stats <- c(f$stats, list(list(step = "code", label = if (rt) "the x axis's breaks, the times at risk"
                                    else "the x axis's breaks", code = paste("x_breaks <-", br))))
    f$plot$add <- c(f$plot$add, list(list(fn = "scale_x_continuous", args = c(list(
      breaks = tfl_r("x_breaks"), limits = tfl_r("range(x_breaks)"),
      expand = tfl_r("expansion(mult = c(0.02, 0.02))")),
      # (an axis shorter than the data: the curves kept, cut at its end)
      if (!is.null(a$x_max)) list(oob = tfl_r("scales::oob_keep"))))))
  }
  # the confidence interval as bars at the axis's times: from the fit
  if (bars) {
    times <- if (length(a$ci_times)) vec_code(a$ci_times) else "x_breaks"
    risk <- identical(a$y_type, "risk")
    grp <- !is.null(f$stats[[1L]]$by)
    f$stats <- c(f$stats, list(list(step = "code",
      label = "the confidence interval at the axis's times, as vertical bars",
      code = paste0(
        "ci_pts <- summary(fit, times = ", times, ", extend = TRUE)\n",
        "ci_pts <- data.frame(\n",
        "  time  = ci_pts$time,\n",
        if (grp) "  strata = sub(\"^[^=]*=\", \"\", as.character(ci_pts$strata)),\n",
        if (risk) "  lower = 1 - ci_pts$upper,\n  upper = 1 - ci_pts$lower\n)"
        else "  lower = ci_pts$lower,\n  upper = ci_pts$upper\n)"))))
    at <- max(which(vapply(f$content, function(it) it$fn %in% c("km_curve", "censor_mark"), NA)))
    eb <- list(fn = "geom_errorbar", data = "ci_pts",
               aes = c(list(x = "time", ymin = "lower", ymax = "upper"),
                       if (grp) list(colour = "strata")),
               args = c(list(width = tfl_r("diff(range(x_breaks)) / 60"), inherit.aes = FALSE),
                        if (!grp) list(colour = tfl_r("pal[[1]]"))))
    f$content <- append(f$content, list(tfl_item("call", eb)), at)
  }
  # the number at risk: ggsurvfit's own table under the curves (a layer of
  # the plot, so the report's own items `+`-ed after the call go on the
  # curves), its title, height and text as the study standard's panel
  is_rt <- vapply(f$content, function(it) identical(it$fn, "risk_table"), NA)
  if (any(is_rt)) {
    r <- f$content[[which(is_rt)[1L]]]$args
    base <- as.numeric(f$plot$base_size %||% 10)
    f$content[[which(is_rt)[1L]]] <- tfl_item("call", list(fn = "add_risktable", args = list(
      times = tfl_r("x_breaks"), risktable_stats = a$risk_stats %||% "n.risk", size = r$size,
      stats_label = r$title,
      risktable_height = r$height,
      theme = tfl_r(sprintf(paste0("list(theme_risktable_default(axis.text.y.size = %s, ",
                                   "plot.title.size = %s), theme(plot.title.position = \"plot\"))"),
                            format(base), format(base * 0.9))))))
  }
  if (isTRUE(a$pvalue)) {
    f$content <- c(f$content, list(tfl_item("call", list(
      fn = "add_pvalue", args = c(list(location = a$pvalue_location %||% "annotation"),
                                  # (an axis shorter than the data: at its end)
                                  if (!is.null(a$x_max) && !identical(a$pvalue_location, "caption"))
                                    list(x = a$x_max, hjust = 1))))))
  }
  if (isTRUE(a$y_percent)) {
    by <- as.numeric(f$plot$y_by %||% 0.2)
    f$plot$y_by <- NULL
    f$plot$add <- c(f$plot$add, list(list(fn = "scale_y_continuous", args = list(
      breaks = tfl_r(sprintf("seq(%s, %s, by = %s)", format(f$plot$y_min %||% 0),
                             format(f$plot$y_max %||% 1), format(by))),
      labels = tfl_r("scales::label_percent()")))))
  }
  if (!is.null(a$legend_title)) {
    f$plot$add <- c(f$plot$add, list(
      list(fn = "labs", args = list(colour = a$legend_title, fill = a$legend_title)),
      list(fn = "theme", args = list(legend.title = tfl_r("element_text()")))))
  }
  # the escape hatch: any other setting, over the template's
  if (length(a$plot)) f$plot <- .merge_map(f$plot, a$plot)
  f
}

.tpl_km_layout_body <- function(a, ctx) .tpl_fig_body_of(.tpl_km_layout(a), a, ctx)

# The former km presets as the layout's arguments (tpl_km_simple() ...)
.tpl_km_preset_args <- list(km_simple = list(risk_table = FALSE),
                            km_risk_table = list(median_line = TRUE),
                            km_ci = list(median_line = TRUE, ci = "band"))

.tpl_km_design <- function(group = NULL, time = NULL, censor = "CNSR", time_unit = "months",
                           data = NULL, param = NULL, preset = "km_simple") {
  .tpl_km_layout(c(list(group = group, time = time, censor = censor, time_unit = time_unit,
                        data = data, param = param), .tpl_km_preset_args[[preset]]))
}

# The display templates: FDA STF Figure 1 (and 4, 13 its variants)
.tpl_km_disc_defaults <- list(param = "TTDISC", time_unit = "weeks",
                              y_type = "risk", y_percent = TRUE, ci = "bars", pvalue = TRUE,
                              risk_table = TRUE, median_line = FALSE,
                              x_label = "Time (Weeks)",
                              y_label = "Cumulative Percentage of Subjects Discontinued",
                              legend = "bottom")

.tpl_km_register <- function() {
  .tfl_template("km", kind = "figure", layout = "km", params = .tpl_km_params(),
                body = .tpl_km_layout_body, call = "tfl_tpl",
                label = "Kaplan-Meier curves (the layout)",
                example = list(param = "PFS", group = "TRT01A"))
  tfl_template_variant("km_simple", from = "km", defaults = .tpl_km_preset_args$km_simple,
                       label = "Kaplan-Meier curves")
  tfl_template_variant("km_risk_table", from = "km", defaults = .tpl_km_preset_args$km_risk_table,
                       label = "Kaplan-Meier curves + number at risk")
  tfl_template_variant("km_ci", from = "km", defaults = .tpl_km_preset_args$km_ci,
                       label = "Kaplan-Meier curves + confidence bands + number at risk")
  tfl_template_variant("km_disc", from = "km", ai = TRUE, defaults = .tpl_km_disc_defaults,
    label = "Kaplan-Meier plot: time to permanent discontinuation of treatment (FDA STF Figure 1)",
    example = list(param = "TTDISC", group = "TRT01A"))
  # (from km, km_disc's defaults again: its parameter, no default, the study's)
  tfl_template_variant("km_ae_onset", from = "km",
    defaults = utils::modifyList(.tpl_km_disc_defaults, keep.null = TRUE, val = list(
      param = NULL, y_label = "Cumulative Percentage of Subjects With the Event")),
    label = "Kaplan-Meier plot: time to onset of an AE of interest (FDA STF Figure 4)",
    example = list(param = "TTSAE", group = "TRT01A"))
  tfl_template_variant("km_lfu", from = "km_disc", ai = FALSE,
    defaults = list(param = "TTLFU", y_label = "Cumulative Percentage of Subjects Lost to Follow-Up"),
    label = "Kaplan-Meier plot: time to last follow-up (FDA STF Figure 13)",
    example = list(param = "TTLFU", group = "TRT01A"))
  # (the examples a company's package tests its wrapped templates with)
  .tpl_examples <<- c(.tpl_examples, list(km = list(param = "PFS", group = "TRT01A"),
                                          km_disc = list(param = "TTDISC", group = "TRT01A"),
                                          km_ae_onset = list(param = "TTSAE", group = "TRT01A"),
                                          km_lfu = list(param = "TTLFU", group = "TRT01A")))
}
