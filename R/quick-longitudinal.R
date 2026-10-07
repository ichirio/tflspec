# Longitudinal figures: mean over visits, individual profiles, box plots.

pp_q_visit_step <- function(visit, visit_label) {
  if (is.null(visit_label)) return(NULL)
  sprintf("mutate(%s = reorder(factor(%s), %s))", visit_label, visit_label, visit)
}

#' Mean over time code
#'
#' Mean with error bars by visit and group.
#'
#' @inheritParams tfl_fig_bar
#' @param style `se` (mean +/- SE), `sd` (mean +/- SD), `ci` (mean with 95%
#'   CI) or `se_n` (mean +/- SE with a table of n below).
#' @param value Analysis variable (`AVAL`, `CHG`, `PCHG`, ...).
#' @param visit,visit_label Visit order variable and its label.
#' @param flag Optional record flag (`== "Y"`), e.g. `ANL01FL`.
#' @export
tfl_fig_mean <- function(adam = NULL, style = c("se", "sd", "ci", "se_n"), param = "ALT", data = "ADLB",
                    value = "AVAL", visit = "AVISITN", visit_label = "AVISIT", group = "TRT01A",
                    pop = "SAFFL", where = NULL, flag = NULL, legend = "bottom", palette = "treatment", key = "USUBJID",
                    theme = "boxed", title = NULL, width = 7.5, height = 4.5, dpi = 300, units = "in",
                    file = NULL, plot_id = "mean") {
  style <- match.arg(style)
  adam <- pp_prep_adam(adam)
  pp_q_check_cols(adam, data, c(value, visit, visit_label))
  pp_q_check_cols(adam, "ADSL", c(group, pop))
  df <- pp_q_eval(adam, data, list(PARAMCD = param), c(group, pop), where = where)
  if (!is.null(pop) && !is.null(df)) df <- df[df[[pop]] %in% "Y", , drop = FALSE]
  bar <- switch(style,
    se = , se_n = c("se", "mean - se", "mean + se", "Mean (+/- SE)"),
    sd = c("sd", "mean - sd", "mean + sd", "Mean (+/- SD)"),
    ci = c("ci", "mean - qt(0.975, n - 1) * se", "mean + qt(0.975, n - 1) * se", "Mean (95% CI)"))
  plab <- pp_q_param_label(adam, data, param, param)
  data_lines <- c(
    pp_q_load(c(data, "ADSL")),
    pipe_code("mn_df", c(list(pp_ds_name(data),
                              pp_q_filter(c(list(PARAMCD = param), if (!is.null(flag)) stats::setNames(list("Y"), flag)), adam[[data]])),
                         as.list(pp_q_adsl_join(c(group, pop), key)),
                         list(pp_q_pop(pop), pp_q_where(where), sprintf("filter(!is.na(%s), !is.na(%s))", value, visit),
                              pp_q_visit_step(visit, visit_label)))),
    pp_q_group_pal("pal_grp", df, group, "mn_df", palette),
    paste0("sum_df <- mn_df |>\n",
           sprintf("  group_by(%s, %s, %s) |>\n", group, visit, visit_label %or% visit),
           sprintf("  summarise(n = n(), mean = mean(%s), sd = sd(%s), .groups = \"drop\") |>\n", value, value),
           sprintf("  mutate(se = sd / sqrt(n), lo = %s, hi = %s)", bar[2], bar[3])))
  x <- visit_label %or% visit
  y_lab <- sprintf("%s %s%s", bar[4], if (value == "AVAL") "" else paste0(value, " of "), plab)
  plot <- c(
    "pd <- position_dodge(width = 0.3)",
    plus_code("p", c(list(
      sprintf("ggplot(sum_df, aes(x = %s, y = mean, colour = %s, group = %s))", x, group, group),
      if (value %in% c("CHG", "PCHG")) 'geom_hline(yintercept = 0, colour = "grey60")',
      "geom_line(position = pd)",
      "geom_point(position = pd, size = 2)",
      "geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.2, position = pd, na.rm = TRUE)",
      "scale_colour_manual(values = pal_grp)",
      sprintf("labs(x = \"Visit\", y = %s%s)", q(y_lab), if (!is.null(title)) paste0(", title = ", q(title)) else "")),
      as.list(pp_theme_lines(theme)), as.list(pp_q_legend(legend)))))
  if (style == "se_n") {
    plot <- c(plot, "# ---- n at each visit ----",
      plus_code("p_n", list(
        sprintf("ggplot(sum_df, aes(x = %s, y = factor(%s, levels = rev(names(pal_grp))), label = n, colour = %s))", x, group, group),
        "geom_text(size = 3)",
        'scale_colour_manual(values = pal_grp, guide = "none")',
        'labs(title = "n", x = NULL, y = NULL)',
        "theme_void(base_size = 10)",
        "theme(axis.text.y = element_text(hjust = 1, margin = margin(r = 5)), plot.title = element_text(size = rel(0.9)))")),
      "fig <- p / p_n + plot_layout(heights = c(0.82, 0.18))")
  } else {
    plot <- c(plot, "fig <- p")
  }
  pp_q_script(plot_id, "Mean over time", title,
              c("dplyr", "ggplot2", if (style == "se_n") "patchwork"),
              data_lines, plot, width, height, dpi, units, file)
}

#' Individual profile code (spaghetti / spider)
#'
#' @inheritParams tfl_fig_mean
#' @param style `spaghetti` (one line per subject by visit, coloured by
#'   group, with the group means) or `spider` (percent change in tumour size
#'   over time, coloured by best overall response, with +20% / -30% lines).
#' @param x Time variable (`AVISITN` for spaghetti, `ADY` for spider).
#' @param time_unit Unit shown for a day-based `x` (`days`, `weeks`,
#'   `months`).
#' @param response PARAMCD of the best overall response in `ADRS` (spider).
#' @export
tfl_fig_individual <- function(adam = NULL, style = c("spaghetti", "spider"), param = NULL, data = NULL,
                          value = NULL, x = NULL, group = NULL, pop = NULL, where = NULL,
                          response = "BOR", time_unit = "weeks", legend = "right", palette = NULL,
                          key = "USUBJID", theme = "boxed", title = NULL, width = 7.5, height = 4.5,
                          dpi = 300, units = "in", file = NULL, plot_id = "individual") {
  style <- match.arg(style)
  adam <- pp_prep_adam(adam)
  spider <- style == "spider"
  data <- data %or% if (spider) "ADTR" else "ADLB"
  param <- param %or% if (spider) "SDIAM" else "ALT"
  value <- value %or% if (spider) "PCHG" else "AVAL"
  x <- x %or% if (spider) "ADY" else "AVISITN"
  pop <- pop %or% if (spider) "FASFL" else "SAFFL"
  group <- group %or% if (spider) NULL else "TRT01A"
  palette <- palette %or% if (spider) "response" else "treatment"
  pp_q_check_cols(adam, data, c(value, x))
  div <- c(days = 1, weeks = 7, months = 30.4375)[[time_unit]]
  df <- pp_q_eval(adam, data, list(PARAMCD = param), c(group, pop), where = where)
  if (!is.null(pop) && !is.null(df)) df <- df[df[[pop]] %in% "Y", , drop = FALSE]
  steps <- c(list(pp_ds_name(data), pp_q_filter(list(PARAMCD = param), adam[[data]])),
             as.list(pp_q_adsl_join(c(group, pop), key)),
             list(pp_q_pop(pop), pp_q_where(where), sprintf("filter(!is.na(%s), !is.na(%s))", value, x)))
  if (spider) {
    bor_df <- pp_q_eval(adam, "ADRS", list(PARAMCD = response))
    steps <- c(steps,
      sprintf("left_join(adrs |> filter(PARAMCD == %s) |> select(%s, %s = AVALC), by = %s)", q(response), key, response, q(key)),
      if (div != 1) sprintf("mutate(%s = %s / %s)", x, x, format(div)))
    colour_var <- response
    pal <- pp_q_pal("pal", bor_df, "AVALC", "id_df", palette)
    if (is.null(bor_df)) pal <- sub("id_df\\$AVALC", paste0("id_df$", response), pal)
  } else {
    colour_var <- group
    pal <- pp_q_group_pal("pal", df, group, "id_df", palette)
  }
  x_lab <- if (spider) sprintf("Time (%s)", tools::toTitleCase(time_unit)) else {
    lab <- if (!is.null(adam[[data]]) && x %in% names(adam[[data]])) pp_var_label(adam[[data]][[x]]) else ""
    if (nzchar(lab)) lab else x
  }
  y_lab <- if (spider) "Change from baseline in sum of diameters (%)" else pp_q_param_label(adam, data, param, param)
  data_lines <- c(pp_q_load(c(data, "ADSL", if (spider) "ADRS")), pipe_code("id_df", steps), pal)
  plot <- plus_code("p", c(list(
    sprintf("ggplot(id_df, aes(x = %s, y = %s, group = %s, colour = %s))", x, value, key, colour_var),
    if (spider) 'geom_hline(yintercept = 0, colour = "grey40")',
    if (spider) 'geom_hline(yintercept = c(20, -30), linetype = "dashed", colour = "grey60")',
    sprintf("geom_line(alpha = %s)", if (spider) "0.8" else "0.35"),
    if (spider) "geom_point(size = 1)",
    if (!spider) sprintf('stat_summary(aes(group = %s), fun = mean, geom = "line", linewidth = 1.2)', group),
    sprintf("scale_colour_manual(values = pal, na.value = \"grey70\"%s)", if (spider) ", breaks = names(pal)" else ""),
    sprintf("labs(x = %s, y = %s%s)", q(x_lab), q(y_lab), if (!is.null(title)) paste0(", title = ", q(title)) else "")),
    as.list(pp_theme_lines(theme)), as.list(pp_q_legend(legend))))
  pp_q_script(plot_id, if (spider) "Spider plot" else "Spaghetti plot", title, c("dplyr", "ggplot2"),
              data_lines, c(plot, "fig <- p"), width, height, dpi, units, file)
}

#' Box plot code
#'
#' @inheritParams tfl_fig_mean
#' @param style `by_visit` (boxes by visit and group), `by_group` (one box
#'   per group at one visit, with the data points) or `change` (change from
#'   baseline by visit and group, with a zero line).
#' @param at_visit Visit label kept for `by_group` (default: last visit).
#' @export
tfl_fig_box <- function(adam = NULL, style = c("by_visit", "by_group", "change"), param = "ALT", data = "ADLB",
                   value = NULL, visit = "AVISITN", visit_label = "AVISIT", at_visit = NULL,
                   group = "TRT01A", pop = "SAFFL", where = NULL, legend = "bottom", palette = "treatment",
                   key = "USUBJID", theme = "boxed", title = NULL, width = 7.5, height = 4.5, dpi = 300,
                   units = "in", file = NULL, plot_id = "box") {
  style <- match.arg(style)
  adam <- pp_prep_adam(adam)
  value <- value %or% if (style == "change") "CHG" else "AVAL"
  pp_q_check_cols(adam, data, c(value, visit, visit_label))
  df <- pp_q_eval(adam, data, list(PARAMCD = param), c(group, pop), where = where)
  if (!is.null(pop) && !is.null(df)) df <- df[df[[pop]] %in% "Y", , drop = FALSE]
  steps <- c(list(pp_ds_name(data), pp_q_filter(list(PARAMCD = param), adam[[data]])),
             as.list(pp_q_adsl_join(c(group, pop), key)),
             list(pp_q_pop(pop), pp_q_where(where), sprintf("filter(!is.na(%s))", value)))
  if (style == "change") steps <- c(steps, sprintf("filter(%s > 0)", visit))
  if (style == "by_group") {
    steps <- c(steps, if (!is.null(at_visit)) sprintf("filter(%s == %s)", visit_label, q(at_visit))
               else sprintf("filter(%s == max(%s))", visit, visit))
  } else {
    steps <- c(steps, pp_q_visit_step(visit, visit_label))
  }
  plab <- pp_q_param_label(adam, data, param, param)
  data_lines <- c(pp_q_load(c(data, "ADSL")), pipe_code("bx_df", steps),
                  pp_q_group_pal("pal_grp", df, group, "bx_df", palette))
  y_lab <- if (value == "AVAL") plab else paste(value, "of", plab)
  plot <- if (style == "by_group") {
    plus_code("p", c(list(
      sprintf("ggplot(bx_df, aes(x = %s, y = %s, fill = %s))", group, value, group),
      "geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.6)",
      "geom_jitter(width = 0.12, size = 1.2, alpha = 0.6)",
      'stat_summary(fun = mean, geom = "point", shape = 3, size = 3)',
      "scale_fill_manual(values = pal_grp)",
      sprintf("labs(x = NULL, y = %s%s)", q(y_lab), if (!is.null(title)) paste0(", title = ", q(title)) else "")),
      as.list(pp_theme_lines(theme)), list('theme(legend.position = "none")')))
  } else {
    plus_code("p", c(list(
      sprintf("ggplot(bx_df, aes(x = %s, y = %s, fill = %s))", visit_label, value, group),
      if (style == "change") 'geom_hline(yintercept = 0, colour = "grey60")',
      "geom_boxplot(position = position_dodge(width = 0.8), width = 0.7, outlier.size = 0.8)",
      'stat_summary(fun = mean, geom = "point", shape = 3, size = 2, position = position_dodge(width = 0.8))',
      "scale_fill_manual(values = pal_grp)",
      sprintf("labs(x = \"Visit\", y = %s%s)", q(y_lab), if (!is.null(title)) paste0(", title = ", q(title)) else "")),
      as.list(pp_theme_lines(theme)), as.list(pp_q_legend(legend))))
  }
  pp_q_script(plot_id, "Box plot", title, c("dplyr", "ggplot2"),
              data_lines, c(plot, "fig <- p"), width, height, dpi, units, file)
}
