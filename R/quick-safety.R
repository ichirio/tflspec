# Safety figures: AE dot plot, AE butterfly, eDISH.

# Step1 shared by the AE figures: incidence per term and arm, with the
# safety population as denominator.
pp_q_ae_data <- function(adam, data, term, group, pop, tefl, key, top, min_pct, palette, where = NULL) {
  pp_q_check_cols(adam, "ADSL", c(group, pop))
  pp_q_check_cols(adam, data, c(term, tefl))
  sl <- if (!is.null(adam)) adam$ADSL else NULL
  if (!is.null(pop) && !is.null(sl)) sl <- sl[sl[[pop]] %in% "Y", , drop = FALSE]
  c(pp_q_load(c(data, "ADSL")),
    pipe_code("pop_df", list("adsl", pp_q_pop(pop))),
    pp_q_pal("pal_grp", sl, group, "pop_df", palette),
    sprintf("arms <- names(pal_grp)[1:2]  # reference first; only two arms are compared"),
    sprintf("N_df <- pop_df |> count(%s, name = \"N\")", group),
    pipe_code("ae_df", c(list(pp_ds_name(data), if (!is.null(tefl)) sprintf('filter(%s == "Y")', tefl), pp_q_where(where),
                              sprintf("select(-any_of(%s))", q(group)),
                              sprintf("inner_join(pop_df |> select(%s, %s), by = %s)", key, group, q(key)),
                              sprintf("distinct(%s, %s, %s)", key, group, term)))),
    paste0("inc_df <- ae_df |>\n",
           sprintf("  count(%s, %s, name = \"n\") |>\n", term, group),
           sprintf("  right_join(expand.grid(%s = unique(ae_df$%s), %s = arms, stringsAsFactors = FALSE),\n", term, term, group),
           sprintf("             by = c(%s, %s)) |>\n", q(term), q(group)),
           "  mutate(n = coalesce(n, 0L)) |>\n",
           sprintf("  left_join(N_df, by = %s) |>\n", q(group)),
           "  mutate(pct = 100 * n / N)"),
    paste0(sprintf("# terms shown: incidence >= %s%% in any arm, top %s by the highest incidence\n", format(min_pct), top),
           "terms <- inc_df |>\n",
           sprintf("  group_by(%s) |>\n", term),
           "  summarise(m = max(pct), .groups = \"drop\") |>\n",
           sprintf("  filter(m >= %s) |>\n", format(min_pct)),
           sprintf("  slice_max(m, n = %s, with_ties = FALSE) |>\n", top),
           "  arrange(m) |>\n",
           sprintf("  pull(%s)", term)),
    sprintf("inc_df <- inc_df |> filter(%s %%in%% terms) |> mutate(%s = factor(%s, levels = terms))", term, term, term))
}

#' AE dot plot code
#'
#' Incidence by preferred term for two arms, with the risk difference and
#' its 95% CI (Wald) in a second panel.
#'
#' @inheritParams tfl_fig_bar
#' @param style `risk_diff` (incidence + risk difference) or `incidence`.
#' @param term AE term variable.
#' @param tefl Treatment-emergent flag (`NULL` for all records).
#' @param top,min_pct Show at most `top` terms with incidence of at least
#'   `min_pct` percent in any arm.
#' @export
tfl_fig_ae_dot <- function(adam = NULL, style = c("risk_diff", "incidence"), data = "ADAE",
                      term = "AEDECOD", group = "TRT01A", pop = "SAFFL", where = NULL, tefl = "TRTEMFL",
                      top = 20, min_pct = 5, legend = "bottom", palette = "treatment", key = "USUBJID",
                      theme = "boxed", title = NULL, width = 8, height = 5.5, dpi = 300, units = "in",
                      file = NULL, plot_id = "ae_dot") {
  style <- match.arg(style)
  adam <- pp_prep_adam(adam)
  data_lines <- pp_q_ae_data(adam, data, term, group, pop, tefl, key, top, min_pct, palette, where)
  inc_plot <- plus_code("p", c(list(
    sprintf("ggplot(inc_df, aes(x = pct, y = %s, colour = %s, shape = %s))", term, group, group),
    "geom_point(size = 2.5)",
    "scale_colour_manual(values = pal_grp)",
    "scale_shape_manual(values = setNames(c(16, 17, 15, 18)[seq_along(pal_grp)], names(pal_grp)))",
    sprintf("labs(x = \"Incidence (%%)\", y = NULL%s)", if (!is.null(title)) paste0(", title = ", q(title)) else "")),
    as.list(pp_theme_lines(theme)), as.list(pp_q_legend(legend))))
  if (style == "incidence") {
    plot <- c(inc_plot, "fig <- p")
  } else {
    data_lines <- c(data_lines, paste0(
      "rd_df <- inc_df |>\n",
      sprintf("  filter(%s == arms[1]) |>\n", group),
      sprintf("  select(%s, n_a = n, N_a = N) |>\n", term),
      sprintf("  inner_join(inc_df |> filter(%s == arms[2]) |> select(%s, n_b = n, N_b = N), by = %s) |>\n",
              group, term, q(term)),
      "  mutate(\n",
      "    pa = n_a / N_a, pb = n_b / N_b,\n",
      "    rd = 100 * (pb - pa),\n",
      "    se = 100 * sqrt(pa * (1 - pa) / N_a + pb * (1 - pb) / N_b),\n",
      "    lcl = rd - 1.96 * se, ucl = rd + 1.96 * se\n",
      "  )"))
    plot <- c(inc_plot, "# ---- risk difference (95% CI) ----",
      plus_code("p_rd", c(list(
        sprintf("ggplot(rd_df, aes(x = rd, y = %s))", term),
        'geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50")',
        'geom_errorbar(aes(xmin = lcl, xmax = ucl), width = 0.3, orientation = "y")',
        "geom_point(size = 2)",
        'labs(x = paste0("Risk difference (%) with 95% CI\\n", arms[2], " - ", arms[1]), y = NULL)'),
        as.list(pp_theme_lines(theme)),
        list("theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())"))),
      "fig <- p + p_rd + plot_layout(widths = c(3, 2))")
  }
  pp_q_script(plot_id, "AE dot plot", title,
              c("dplyr", "ggplot2", if (style == "risk_diff") "patchwork"),
              data_lines, plot, width, height, dpi, units, file)
}

#' AE butterfly plot code
#'
#' Incidence of two arms as mirrored horizontal bars.
#'
#' @inheritParams tfl_fig_ae_dot
#' @param style `soc` (system organ class) or `pt` (preferred term).
#' @export
tfl_fig_butterfly <- function(adam = NULL, style = c("soc", "pt"), data = "ADAE", term = NULL,
                         group = "TRT01A", pop = "SAFFL", where = NULL, tefl = "TRTEMFL", top = 20, min_pct = 0,
                         legend = "bottom", palette = "treatment", key = "USUBJID", theme = "boxed",
                         title = NULL, width = 8, height = 5, dpi = 300, units = "in",
                         file = NULL, plot_id = "butterfly") {
  style <- match.arg(style)
  adam <- pp_prep_adam(adam)
  term <- term %or% if (style == "soc") "AEBODSYS" else "AEDECOD"
  data_lines <- c(pp_q_ae_data(adam, data, term, group, pop, tefl, key, top, min_pct, palette, where),
                  sprintf("inc_df <- inc_df |> mutate(x = ifelse(%s == arms[1], -pct, pct))", group),
                  "x_lim <- max(inc_df$pct) * 1.2")
  plot <- c(plus_code("p", c(list(
    sprintf("ggplot(inc_df, aes(x = x, y = %s, fill = %s))", term, group),
    "geom_col(width = 0.7)",
    'geom_vline(xintercept = 0, colour = "grey30")',
    'geom_text(aes(label = ifelse(pct > 0, sprintf("%.1f", pct), ""), hjust = ifelse(x < 0, 1.15, -0.15)), size = 2.6)',
    "scale_x_continuous(limits = c(-x_lim, x_lim), labels = function(v) abs(v))",
    "scale_fill_manual(values = pal_grp)",
    sprintf("labs(x = \"Subjects with event (%%)\", y = NULL%s)", if (!is.null(title)) paste0(", title = ", q(title)) else "")),
    as.list(pp_theme_lines(theme)), as.list(pp_q_legend(legend)))), "fig <- p")
  pp_q_script(plot_id, "AE butterfly plot", title, c("dplyr", "ggplot2"),
              data_lines, plot, width, height, dpi, units, file)
}

#' eDISH plot code
#'
#' Maximum post-baseline transaminase vs total bilirubin, both as multiples
#' of the upper limit of normal, with the Hy's law reference lines.
#'
#' @inheritParams tfl_fig_ae_dot
#' @param style `alt` (ALT on the x axis) or `alt_ast` (the larger of ALT
#'   and AST).
#' @param alt,ast,bili PARAMCD of ALT, AST and total bilirubin.
#' @param uln Upper limit of normal variable.
#' @param post_baseline Condition selecting post-baseline records (R code).
#' @export
tfl_fig_edish <- function(adam = NULL, style = c("alt", "alt_ast"), data = "ADLB", alt = "ALT", ast = "AST",
                     bili = "BILI", uln = "ANRHI", post_baseline = "AVISITN > 0", group = "TRT01A",
                     pop = "SAFFL", where = NULL, legend = "inside", palette = "treatment", key = "USUBJID",
                     theme = "boxed", title = NULL, width = 7, height = 6, dpi = 300, units = "in",
                     file = NULL, plot_id = "edish") {
  style <- match.arg(style)
  adam <- pp_prep_adam(adam)
  pp_q_check_cols(adam, data, c("AVAL", uln))
  x_params <- if (style == "alt") alt else c(alt, ast)
  sl <- if (!is.null(adam)) adam$ADSL else NULL
  if (!is.null(pop) && !is.null(sl)) sl <- sl[sl[[pop]] %in% "Y", , drop = FALSE]
  data_lines <- c(
    pp_q_load(c(data, "ADSL")),
    pipe_code("lb_df", c(list(pp_ds_name(data), pp_q_filter(list(PARAMCD = c(x_params, bili)), adam[[data]]),
                              sprintf("filter(%s)", post_baseline)),
                         as.list(pp_q_adsl_join(c(group, pop), key)),
                         list(pp_q_pop(pop), pp_q_where(where), sprintf("mutate(XULN = AVAL / %s)", uln), "filter(!is.na(XULN))"))),
    pp_q_group_pal("pal_grp", sl, group, "lb_df", palette),
    paste0("max_df <- lb_df |>\n",
           sprintf("  group_by(%s, %s, PARAMCD) |>\n", key, group),
           "  summarise(m = max(XULN, na.rm = TRUE), .groups = \"drop\")"),
    paste0("ed_df <- max_df |>\n",
           sprintf("  filter(PARAMCD %%in%% %s) |>\n", vec_code(x_params)),
           sprintf("  group_by(%s, %s) |>\n", key, group),
           "  summarise(x = max(m), .groups = \"drop\") |>\n",
           sprintf("  inner_join(max_df |> filter(PARAMCD == %s) |> select(%s, y = m), by = %s)", q(bili), key, q(key))))
  x_lab <- if (style == "alt") "Maximum post-baseline ALT (x ULN)" else "Maximum post-baseline ALT or AST (x ULN)"
  plot <- c(plus_code("p", c(list(
    sprintf("ggplot(ed_df, aes(x = x, y = y, colour = %s, shape = %s))", group, group),
    'geom_vline(xintercept = 3, linetype = "dashed", colour = "grey50")',
    'geom_hline(yintercept = 2, linetype = "dashed", colour = "grey50")',
    "geom_point(size = 2)",
    'annotate("text", x = max(ed_df$x, 3), y = max(ed_df$y, 2), label = "Potential Hy\'s Law", hjust = 1, vjust = 1, size = 3)',
    'annotate("text", x = min(ed_df$x), y = max(ed_df$y, 2), label = "Hyperbilirubinemia", hjust = 0, vjust = 1, size = 3)',
    'annotate("text", x = max(ed_df$x, 3), y = min(ed_df$y), label = "Temple\'s Corollary", hjust = 1, vjust = 0, size = 3)',
    "scale_x_log10()",
    "scale_y_log10()",
    "scale_colour_manual(values = pal_grp)",
    sprintf("labs(x = %s, y = \"Maximum post-baseline total bilirubin (x ULN)\"%s)", q(x_lab),
            if (!is.null(title)) paste0(", title = ", q(title)) else "")),
    as.list(pp_theme_lines(theme)), as.list(pp_q_legend(if (legend == "inside") "inside_bl" else legend)))), "fig <- p")
  pp_q_script(plot_id, "eDISH plot", title, c("dplyr", "ggplot2"),
              data_lines, plot, width, height, dpi, units, file)
}
