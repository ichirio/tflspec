#' Forest plot code
#'
#' Estimates by subgroup with a text column (N, estimate and 95% CI).
#'
#' @inheritParams tfl_fig_km
#' @inheritParams tfl_fig_sankey
#' @param key Subject key used to join ADSL.
#' @param style `hr` (Cox hazard ratio from a time-to-event parameter), `or`
#'   (odds ratio of response from logistic regression) or `estimates` (a data
#'   frame `est_df` you provide with `label`, `est`, `lcl`, `ucl` and
#'   optionally `n`).
#' @param subgroups ADSL variables defining the subgroups.
#' @param group Treatment variable; its first value is the reference.
#' @param where Extra record condition as R code, e.g.
#'   `'AVISIT != "Retrieval"'`; `NULL` for none.
#' @param responders Values of `AVALC` counted as responders (`or`).
#' @param theme Theme preset.
#' @export
tfl_fig_forest <- function(adam = NULL, style = c("hr", "or", "estimates"), param = NULL,
                      group = "TRT01P", subgroups = c("SEX", "AGEGR1"), pop = "FASFL", where = NULL,
                      data = NULL, responders = c("CR", "PR"), key = "USUBJID",
                      theme = "boxed", title = NULL, width = 8, height = 5, dpi = 300,
                      units = "in", file = NULL, plot_id = "forest") {
  style <- match.arg(style)
  adam <- pp_prep_adam(adam)
  data <- data %or% switch(style, hr = "ADTTE", or = "ADRS", estimates = NULL)
  param <- param %or% switch(style, hr = "OS", or = "BOR", estimates = NULL)
  what <- "Forest plot"

  if (style == "estimates") {
    data_lines <- c(
      "# est_df: one row per line of the plot, with columns",
      "#   label (text on the y axis), est, lcl, ucl (NA for heading rows) and optionally n",
      "est_df <- est_df |>",
      "  mutate(y = rev(seq_len(n())), txt = ifelse(is.na(est), \"\", sprintf(\"%.2f (%.2f, %.2f)\", est, lcl, ucl)))")
    x_lab <- "Estimate (95% CI)"
    ref <- 1
    libs <- c("dplyr", "ggplot2", "patchwork")
  } else {
    pp_q_check_cols(adam, "ADSL", c(group, subgroups, pop))
    df <- pp_q_eval(adam, data, list(PARAMCD = param), c(group, pop), where = where)
    if (!is.null(pop) && !is.null(df)) df <- df[df[[pop]] %in% "Y", , drop = FALSE]
    arms <- if (!is.null(df)) pp_values(df, group) else NULL
    fit <- if (style == "hr") {
      c("fit_est <- function(d) {",
        sprintf("  if (n_distinct(d$%s) < 2 || sum(d$CNSR == 0) < 2) return(data.frame(est = NA, lcl = NA, ucl = NA))", group),
        "  # a model that does not converge (e.g. no events in one arm) is not estimable",
        sprintf("  fit <- tryCatch(coxph(Surv(AVAL, CNSR == 0) ~ %s, data = d), warning = function(w) NULL)", group),
        "  if (is.null(fit)) return(data.frame(est = NA, lcl = NA, ucl = NA))",
        "  s <- summary(fit)$conf.int",
        "  data.frame(est = s[1, 1], lcl = s[1, 3], ucl = s[1, 4])",
        "}")
    } else {
      c("fit_est <- function(d) {",
        sprintf("  if (n_distinct(d$%s) < 2 || n_distinct(d$RESP) < 2) return(data.frame(est = NA, lcl = NA, ucl = NA))", group),
        sprintf("  m <- tryCatch(glm(RESP ~ %s, family = binomial, data = d), warning = function(w) NULL)", group),
        "  if (is.null(m)) return(data.frame(est = NA, lcl = NA, ucl = NA))",
        "  ci <- exp(confint.default(m)[2, ])",
        "  data.frame(est = exp(coef(m)[[2]]), lcl = ci[[1]], ucl = ci[[2]])",
        "}")
    }
    data_lines <- c(
      pp_q_load(c(data, "ADSL")),
      sprintf("subgroups <- %s", vec_code(subgroups)),
      pipe_code("fr_df", c(list(pp_ds_name(data), pp_q_filter(list(PARAMCD = param), adam[[data]])),
                           as.list(pp_q_adsl_join(c(group, pop, subgroups), key)),
                           list(pp_q_pop(pop), pp_q_where(where)),
                           list(if (style == "or") sprintf("mutate(RESP = as.integer(AVALC %%in%% %s))", vec_code(responders))),
                           list(sprintf("mutate(%s = factor(%s, levels = %s))", group, group,
                                        if (!is.null(arms)) vec_code(arms) else sprintf("sort(unique(%s))", group)),
                                "droplevels()  # groups removed by the filters drop out of the model"))),
      sprintf("# %s: %s vs the reference (first level of %s)", if (style == "hr") "hazard ratio" else "odds ratio",
              "each other arm", group),
      paste(fit, collapse = "\n"),
      paste0("rows <- list(cbind(data.frame(label = \"All subjects\", head = FALSE, n = nrow(fr_df)), fit_est(fr_df)))\n",
             "for (v in subgroups) {\n",
             "  rows[[length(rows) + 1]] <- data.frame(label = v, head = TRUE, n = NA, est = NA, lcl = NA, ucl = NA)\n",
             "  for (lv in sort(unique(na.omit(fr_df[[v]])))) {\n",
             "    d <- fr_df[fr_df[[v]] %in% lv, ]\n",
             "    rows[[length(rows) + 1]] <- cbind(data.frame(label = paste0(\"   \", lv), head = FALSE, n = nrow(d)), fit_est(d))\n",
             "  }\n",
             "}"),
      paste0("est_df <- bind_rows(rows) |>\n",
             "  mutate(\n",
             "    # not estimable (e.g. no events / all responders in a subgroup): no point, \"NE\"\n",
             "    ok  = is.finite(est) & is.finite(lcl) & is.finite(ucl) & lcl > 0 & ucl < 1000,\n",
             "    txt = ifelse(head, \"\", ifelse(ok, sprintf(\"%.2f (%.2f, %.2f)\", est, lcl, ucl), \"NE\")),\n",
             "    across(c(est, lcl, ucl), ~ ifelse(ok, .x, NA)),\n",
             "    y   = rev(seq_len(n()))\n",
             "  )"))
    x_lab <- if (style == "hr") "Hazard Ratio (95% CI)" else "Odds Ratio (95% CI)"
    if (!is.null(arms) && length(arms) >= 2) x_lab <- sprintf("%s, %s vs %s", x_lab, arms[2], arms[1])
    ref <- 1
    libs <- c("dplyr", "ggplot2", "patchwork", if (style == "hr") "survival")
  }

  plot <- c(
    plus_code("p", c(list(
      "ggplot(est_df, aes(y = y))",
      sprintf('geom_vline(xintercept = %s, linetype = "dashed", colour = "grey50")', ref),
      'geom_errorbar(aes(xmin = lcl, xmax = ucl), width = 0.25, orientation = "y", na.rm = TRUE)',
      "geom_point(aes(x = est), shape = 15, size = 2.5, na.rm = TRUE)",
      "scale_x_log10()",
      "scale_y_continuous(breaks = est_df$y, labels = est_df$label, expand = expansion(add = 0.6))",
      sprintf("labs(x = %s, y = NULL%s)", q(x_lab), if (!is.null(title)) paste0(", title = ", q(title)) else "")),
      as.list(pp_theme_lines(theme)),
      list("theme(axis.text.y = element_text(hjust = 0), panel.grid.major.y = element_blank())"))),
    "# ---- text columns: N and estimate (95% CI) ----",
    plus_code("p_txt", list(
      "ggplot(est_df, aes(y = y))",
      if (style != "estimates") 'geom_text(aes(x = 0, label = ifelse(is.na(n), "", n)), size = 3)'
      else 'geom_text(aes(x = 0, label = if ("n" %in% names(est_df)) ifelse(is.na(n), "", n) else ""), size = 3)',
      "geom_text(aes(x = 1, label = txt), size = 3)",
      'scale_x_continuous(limits = c(-0.4, 1.6), breaks = c(0, 1), labels = c("N", "Estimate (95% CI)"), position = "top")',
      "scale_y_continuous(expand = expansion(add = 0.6))",
      "theme_void(base_size = 10)",
      'theme(axis.text.x.top = element_text(face = "bold"))')),
    "fig <- p + p_txt + plot_layout(widths = c(3, 2))")
  pp_q_script(plot_id, what, title, libs, data_lines, plot, width, height, dpi, units, file)
}

#' Bar chart code for rates and category percentages
#'
#' @inheritParams tfl_fig_forest
#' @param style `rate_ci` (response rate by group with exact 95% CI),
#'   `stacked` (100% stacked bars of a category by group) or `dodged`
#'   (percentages of each category, groups side by side).
#' @param category Categorical variable (default `AVALC`).
#' @param legend Legend preset (`none`, `right`, `bottom`, `top`, `inside`, ...).
#' @param palette Palette preset of the group (`rate_ci`, `dodged`) or of the
#'   category (`stacked`, default `response`).
#' @export
tfl_fig_bar <- function(adam = NULL, style = c("rate_ci", "stacked", "dodged"), param = "BOR",
                   data = "ADRS", category = "AVALC", group = "TRT01P", pop = "FASFL", where = NULL,
                   responders = c("CR", "PR"), legend = NULL, palette = NULL, key = "USUBJID",
                   theme = "boxed", title = NULL, width = 7, height = 4.5, dpi = 300, units = "in",
                   file = NULL, plot_id = "bar") {
  style <- match.arg(style)
  adam <- pp_prep_adam(adam)
  pp_q_check_cols(adam, "ADSL", c(group, pop))
  legend <- legend %or% if (style == "rate_ci") "none" else "right"
  palette <- palette %or% if (style == "stacked") "response" else "treatment"
  df <- pp_q_eval(adam, data, list(PARAMCD = param), c(group, pop), where = where)
  if (!is.null(pop) && !is.null(df)) df <- df[df[[pop]] %in% "Y", , drop = FALSE]

  prep <- pipe_code("bar_df", c(list(pp_ds_name(data), pp_q_filter(list(PARAMCD = param), adam[[data]])),
                                as.list(pp_q_adsl_join(c(group, pop), key)),
                                list(pp_q_pop(pop), pp_q_where(where))))
  pal_var <- if (style == "stacked") category else group
  pal <- if (style == "stacked") pp_q_pal("pal", df, pal_var, "bar_df", palette) else pp_q_group_pal("pal", df, pal_var, "bar_df", palette)
  lab <- pp_q_param_label(adam, data, param, param)
  if (style == "rate_ci") {
    data_lines <- c(pp_q_load(c(data, "ADSL")), prep, pal,
      paste0("rate_df <- bar_df |>\n",
             sprintf("  group_by(%s) |>\n", group),
             sprintf("  summarise(n = n(), x = sum(%s %%in%% %s), .groups = \"drop\") |>\n", category, vec_code(responders)),
             "  mutate(\n",
             "    rate = 100 * x / n,\n",
             "    lcl  = 100 * mapply(function(x, n) binom.test(x, n)$conf.int[1], x, n),\n",
             "    ucl  = 100 * mapply(function(x, n) binom.test(x, n)$conf.int[2], x, n)\n",
             "  )"))
    plot <- plus_code("p", c(list(
      sprintf("ggplot(rate_df, aes(x = %s, y = rate, fill = %s))", group, group),
      "geom_col(width = 0.6)",
      "geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0.15)",
      'geom_text(aes(y = ucl, label = sprintf("%.1f%%\\n(%d/%d)", rate, x, n)), vjust = -0.3, size = 3)',
      "scale_fill_manual(values = pal)",
      "scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, 20), expand = expansion(mult = c(0, 0.02)))",
      sprintf("labs(x = NULL, y = %s%s)", q(sprintf("Response rate (%%) with 95%% CI [%s]", paste(responders, collapse = "+"))),
              if (!is.null(title)) paste0(", title = ", q(title)) else "")),
      as.list(pp_theme_lines(theme)), as.list(pp_q_legend(legend))))
  } else {
    data_lines <- c(pp_q_load(c(data, "ADSL")), prep, pal,
      paste0("pct_df <- bar_df |>\n",
             sprintf("  count(%s, %s) |>\n", group, category),
             sprintf("  group_by(%s) |>\n", group),
             "  mutate(pct = 100 * n / sum(n)) |>\n",
             "  ungroup()",
             if (style == "stacked") {
               sprintf(" |>\n  mutate(%s = factor(%s, levels = rev(intersect(names(pal), %s))))", category, category, category)
             } else {
               sprintf(" |>\n  # response categories in their usual order, other values after them\n  mutate(%s = factor(%s, levels = unique(c(intersect(%s, %s), sort(%s)))))",
                       category, category, vec_code(names(tfl_fig_palettes()$response)), category, category)
             }))
    plot <- if (style == "stacked") {
      plus_code("p", c(list(
        sprintf("ggplot(pct_df, aes(x = %s, y = pct, fill = %s))", group, category),
        'geom_col(width = 0.6, colour = "white")',
        'geom_text(aes(label = ifelse(pct >= 5, sprintf("%.0f%%", pct), "")), position = position_stack(vjust = 0.5), size = 3)',
        "scale_fill_manual(values = pal, breaks = names(pal))",
        "scale_y_continuous(expand = expansion(mult = c(0, 0.02)))",
        sprintf("labs(x = NULL, y = \"Subjects (%%)\"%s)", if (!is.null(title)) paste0(", title = ", q(title)) else "")),
        as.list(pp_theme_lines(theme)), as.list(pp_q_legend(legend))))
    } else {
      plus_code("p", c(list(
        sprintf("ggplot(pct_df, aes(x = %s, y = pct, fill = %s))", category, group),
        "geom_col(width = 0.7, position = position_dodge(width = 0.75))",
        'geom_text(aes(label = sprintf("%.0f", pct)), position = position_dodge(width = 0.75), vjust = -0.3, size = 2.8)',
        "scale_fill_manual(values = pal)",
        "scale_y_continuous(expand = expansion(mult = c(0, 0.08)))",
        sprintf("labs(x = %s, y = \"Subjects (%%)\"%s)", q(lab), if (!is.null(title)) paste0(", title = ", q(title)) else "")),
        as.list(pp_theme_lines(theme)), as.list(pp_q_legend(legend))))
    }
  }
  pp_q_script(plot_id, "Bar chart", title, c("dplyr", "ggplot2"),
              data_lines, c(plot, "fig <- p"), width, height, dpi, units, file)
}
