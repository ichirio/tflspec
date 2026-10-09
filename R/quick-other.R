# Scatter plots and PK concentration-time profiles.

#' Scatter plot code
#'
#' @inheritParams tfl_fig_mean
#' @param style `shift` (baseline vs post-baseline value at one visit, with
#'   the identity line) or `xy` (any two variables with a linear fit per group).
#' @param x,y Variables of `xy` (defaults `BASE` and `CHG`).
#' @param at_visit Visit label kept for `shift` (default: last visit).
#' @export
tfl_fig_scatter <- function(adam = NULL, style = c("shift", "xy"), param = "ALT", data = "ADLB",
                       x = NULL, y = NULL, visit = "AVISITN", visit_label = "AVISIT", at_visit = NULL,
                       group = "TRT01A", pop = "SAFFL", where = NULL, legend = "inside_tl", palette = "treatment",
                       key = "USUBJID", theme = "boxed", title = NULL, width = 6, height = 5.5,
                       dpi = 300, units = "in", file = NULL, plot_id = "scatter") {
  style <- match.arg(style)
  adam <- pp_prep_adam(adam)
  x <- x %or% "BASE"
  y <- y %or% if (style == "shift") "AVAL" else "CHG"
  pp_q_check_cols(adam, data, c(x, y, visit, visit_label))
  df <- pp_q_eval(adam, data, list(PARAMCD = param), c(group, pop), where = where)
  if (!is.null(pop) && !is.null(df)) df <- df[df[[pop]] %in% "Y", , drop = FALSE]
  plab <- pp_q_param_label(adam, data, param, param)
  steps <- c(list(pp_ds_name(data), pp_q_filter(list(PARAMCD = param), adam[[data]])),
             as.list(pp_q_adsl_join(c(group, pop), key)),
             list(pp_q_pop(pop), pp_q_where(where), sprintf("filter(%s > 0, !is.na(%s), !is.na(%s))", visit, x, y),
                  if (!is.null(at_visit)) sprintf("filter(%s == %s)", visit_label, q(at_visit))
                  else sprintf("filter(%s == max(%s))", visit, visit)))
  data_lines <- c(pp_q_load(c(data, "ADSL")), pipe_code("sc_df", steps),
                  pp_q_group_pal("pal_grp", df, group, "sc_df", palette))
  if (style == "shift") {
    data_lines <- c(data_lines, sprintf("lims <- range(c(sc_df$%s, sc_df$%s), na.rm = TRUE)", x, y))
  }
  plot <- plus_code("p", c(list(
    sprintf("ggplot(sc_df, aes(x = %s, y = %s, colour = %s))", x, y, group),
    if (style == "shift") 'geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50")',
    if (style == "xy" && y %in% c("CHG", "PCHG")) 'geom_hline(yintercept = 0, colour = "grey60")',
    "geom_point(size = 1.8, alpha = 0.8)",
    if (style == "xy") 'geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 0.7)',
    "scale_colour_manual(values = pal_grp)",
    if (style == "shift") "coord_equal(xlim = lims, ylim = lims)",
    sprintf("labs(x = %s, y = %s%s)",
            q(if (style == "shift") paste("Baseline", plab) else paste(x, "of", plab)),
            q(if (style == "shift") paste("Post-baseline", plab) else paste(y, "of", plab)),
            if (!is.null(title)) paste0(", title = ", q(title)) else "")),
    as.list(pp_theme_lines(theme)), as.list(pp_q_legend(legend))))
  pp_q_script(plot_id, "Scatter plot", title, c("dplyr", "ggplot2"),
              data_lines, c(plot, "fig <- p"), width, height, dpi, units, file)
}

#' PK concentration-time plot code
#'
#' @inheritParams tfl_fig_mean
#' @param style `mean` (mean +/- SD by nominal time and group, linear axis),
#'   `mean_log` (the same on a log axis) or `individual` (one line per
#'   subject, log axis, one panel per group).
#' @param time Nominal time variable.
#' @param time_label Axis label of `time`.
#' @export
tfl_fig_pk <- function(adam = NULL, style = c("mean", "mean_log", "individual"), param = NULL,
                  data = "ADPC", value = "AVAL", time = "NFRLT", time_label = "Nominal time (h)",
                  group = "TRT01A", pop = "SAFFL", where = NULL, legend = "inside", palette = "treatment",
                  key = "USUBJID", theme = "boxed", title = NULL, width = 7.5, height = 4.5,
                  dpi = 300, units = "in", file = NULL, plot_id = "pk") {
  style <- match.arg(style)
  adam <- pp_prep_adam(adam)
  pp_q_check_cols(adam, data, c(value, time))
  df <- pp_q_eval(adam, data, if (!is.null(param)) list(PARAMCD = param) else list(), c(group, pop), where = where)
  if (!is.null(pop) && !is.null(df)) df <- df[df[[pop]] %in% "Y", , drop = FALSE]
  plab <- if (!is.null(param)) pp_q_param_label(adam, data, param, "Concentration") else {
    if (!is.null(adam) && "PARAM" %in% names(adam[[data]]) && length(unique(adam[[data]]$PARAM)) == 1) unique(adam[[data]]$PARAM) else "Concentration"
  }
  log_y <- style != "mean"
  steps <- c(list(pp_ds_name(data), if (!is.null(param)) pp_q_filter(list(PARAMCD = param), adam[[data]])),
             as.list(pp_q_adsl_join(c(group, pop), key)),
             list(pp_q_pop(pop), pp_q_where(where), sprintf("filter(!is.na(%s))", value),
                  if (log_y) sprintf("filter(%s > 0)  # log axis: drop zero / BLQ values", value)))
  data_lines <- c(pp_q_load(c(data, "ADSL")), pipe_code("pk_df", steps),
                  pp_q_group_pal("pal_grp", df, group, "pk_df", palette))
  if (style != "individual") {
    data_lines <- c(data_lines, paste0(
      "sum_df <- pk_df |>\n",
      sprintf("  group_by(%s, %s) |>\n", group, time),
      sprintf("  summarise(n = n(), mean = mean(%s), sd = sd(%s), .groups = \"drop\")", value, value),
      if (log_y) " |>\n  mutate(lo = ifelse(mean - sd > 0, mean - sd, NA))  # no lower bar where it would be <= 0" else ""))
    plot <- plus_code("p", c(list(
      sprintf("ggplot(sum_df, aes(x = %s, y = mean, colour = %s))", time, group),
      "geom_line()",
      "geom_point(size = 1.8)",
      sprintf("geom_errorbar(aes(ymin = %s, ymax = mean + sd), width = 0.3, na.rm = TRUE)", if (log_y) "lo" else "mean - sd"),
      "scale_colour_manual(values = pal_grp)",
      if (log_y) "scale_y_log10()",
      sprintf("labs(x = %s, y = %s%s)", q(time_label), q(paste("Mean (SD)", plab)),
              if (!is.null(title)) paste0(", title = ", q(title)) else "")),
      as.list(pp_theme_lines(theme)), as.list(pp_q_legend(legend))))
  } else {
    plot <- plus_code("p", c(list(
      sprintf("ggplot(pk_df, aes(x = %s, y = %s, group = %s, colour = %s))", time, value, key, group),
      "geom_line(alpha = 0.5)",
      "geom_point(size = 0.8, alpha = 0.5)",
      sprintf("facet_wrap(vars(%s))", group),
      "scale_colour_manual(values = pal_grp)",
      "scale_y_log10()",
      sprintf("labs(x = %s, y = %s%s)", q(time_label), q(plab), if (!is.null(title)) paste0(", title = ", q(title)) else "")),
      as.list(pp_theme_lines(theme)), list('theme(legend.position = "none")')))
  }
  pp_q_script(plot_id, "PK concentration-time plot", title, c("dplyr", "ggplot2"),
              data_lines, c(plot, "fig <- p"), width, height, dpi, units, file)
}
