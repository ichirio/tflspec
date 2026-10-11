pp_gen_waterfall <- function(ctx) {
  id <- pp_role(ctx, "id", required = TRUE)$variable
  value <- pp_role(ctx, "value", required = TRUE)$variable
  fill <- pp_role(ctx, "fill")
  src <- pp_use_ds(ctx, ctx$ds)
  flist <- pp_filters(ctx, "base", ctx$ds)

  steps <- list(src, pp_filter_code(ctx, flist, ctx$ds))
  f_var <- NULL
  sv <- NULL
  if (!is.null(fill)) {
    if (fill$dataset != ctx$ds) {
      j <- pp_join(ctx, "base", fill$dataset, fill$variable, pp_opt(ctx, "id_var"), taken = c(id, value))
      steps <- c(steps, j$code)
      f_var <- j$name
      f_values <- j$values
    } else {
      f_var <- fill$variable
      f_values <- pp_values_or_null(pp_filtered_data(ctx, ctx$ds, flist), f_var)
    }
    sv <- pp_scale_values(ctx, "pal_fill", f_var, f_values, "wf_df", "response")
  }
  steps <- c(steps,
    sprintf("filter(!is.na(%s))", value),
    sprintf("arrange(desc(%s))", value),
    if (!is.null(f_var) && !is.null(sv$values)) {
      sprintf("mutate(INDEX = row_number(), %s = factor(%s, levels = names(pal_fill)))", f_var, f_var)
    } else "mutate(INDEX = row_number())"
  )

  y_min <- pp_opt_num(ctx, "y_min", -100)
  y_max <- pp_opt_num(ctx, "y_max", 100)
  y_by <- pp_opt_num(ctx, "y_by", 20)
  y_lab <- ctx$prow$y_label %or% pp_var_label_of(ctx, ctx$ds, value) %or% value
  x_lab <- ctx$prow$x_label %or% pp_opt(ctx, "x_label") %or% "Patients"

  aes_fill <- if (!is.null(f_var)) sprintf(", fill = %s", f_var) else ""
  base_terms <- list(
    sprintf("ggplot(wf_df, aes(x = INDEX, y = %s%s))", value, aes_fill),
    if (!is.null(f_var)) sprintf("geom_col(width = %s)", pp_opt(ctx, "bar_width"))
    else sprintf('geom_col(width = %s, fill = "#4F6D7A")', pp_opt(ctx, "bar_width")),
    if (!is.null(f_var)) pp_scale_manual("fill", sv, 'na.value = "grey80"'),
    sprintf("geom_hline(yintercept = 0, colour = %s, linewidth = %s)",
            q(pp_opt(ctx, "zero_line_colour")), pp_opt(ctx, "zero_line_width")),
    "scale_y_continuous(breaks = seq(y_min, y_max, by = y_by))",
    'coord_cartesian(ylim = c(y_min, y_max), clip = "off")',
    sprintf("labs(x = %s, y = %s%s)", q(x_lab), q(y_lab),
            if (!is.na(ctx$prow$title)) paste0(", title = ", q(ctx$prow$title)) else ""),
    pp_theme_code(ctx),
    "theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())"
  )
  plot <- c("# ---- base: waterfall ----", plus_code("p", base_terms))

  refs <- as.numeric(split_list(pp_opt(ctx, "ref_lines")))
  if (has_layer(ctx, "ref_lines") && length(refs)) {
    plot <- c(plot, "# ---- layer: ref_lines ----",
              sprintf("ref_y <- %s", vec_code(refs)),
              sprintf("p <- p + geom_hline(yintercept = ref_y, colour = %s, linetype = %s, linewidth = %s)",
                      q(pp_opt(ctx, "ref_line_colour")), q(pp_opt(ctx, "ref_linetype")),
                      pp_opt(ctx, "ref_line_width")))
  }
  if (has_layer(ctx, "ref_labels") && length(refs)) {
    if (!has_layer(ctx, "ref_lines")) plot <- c(plot, sprintf("ref_y <- %s", vec_code(refs)))
    plot <- c(plot, "# ---- layer: ref_labels (outside the panel, right) ----",
              plus_code("p", list(
                sprintf('annotate("text", x = Inf, y = ref_y, label = paste0(ref_y, "%%"), hjust = %s, size = %s)',
                        pp_opt(ctx, "ref_label_hjust"), pp_opt(ctx, "ref_label_size")),
                sprintf("theme(plot.margin = margin(t = 5.5, r = %s, b = 5.5, l = 5.5))",
                        pp_opt(ctx, "right_margin"))
              ), append = TRUE))
  }

  parts <- list()
  if (!is.null(sv)) {
    parts <- if (!is.null(sv$values)) list(legend_part(sv$labels, "rect", fill = sv$colours))
    else list('tibble::tibble(label = names(pal_fill), glyph = "rect", shape = NA, colour = NA, fill = unname(pal_fill), linetype = NA)')
  }

  list(
    data = c(pal_before(sv), pipe_code("wf_df", steps), pal_after(sv),
             sprintf("y_min <- %s\ny_max <- %s\ny_by  <- %s", format(y_min), format(y_max), format(y_by))),
    plot = plot,
    panels = list(),
    parts = parts,
    libs = c("dplyr", "ggplot2", "patchwork")
  )
}
