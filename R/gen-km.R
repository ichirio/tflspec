pp_time_mutate <- function(ctx, vars) {
  unit <- pp_opt(ctx, "time_unit")
  if (!unit %in% names(pp_time_divisor)) stop("Unknown time_unit: ", unit, call. = FALSE)
  if (unit == "as_is" || !length(vars)) return(NULL)
  d <- format(pp_time_divisor[[unit]])
  if (length(vars) == 1) return(sprintf("mutate(%s = %s / %s)", vars, vars, d))
  sprintf("mutate(across(c(%s), ~ .x / %s))", paste(vars, collapse = ", "), d)
}

pp_time_label <- function(ctx) unname(pp_time_axis_label[pp_opt(ctx, "time_unit")])

# x axis range / breaks lines shared by time-based plots.
pp_x_axis_lines <- function(ctx, df, var) {
  x_max <- pp_opt(ctx, "x_max")
  x_by <- pp_opt(ctx, "x_by")
  paste(c(
    if (is.na(x_max)) sprintf("x_max    <- ceiling(max(%s$%s, na.rm = TRUE))", df, var)
    else sprintf("x_max    <- %s", x_max),
    if (is.na(x_by)) "x_breaks <- pretty(c(0, x_max))"
    else sprintf("x_breaks <- seq(0, x_max, by = %s)", x_by)
  ), collapse = "
")
}

pp_gen_km <- function(ctx) {
  time <- pp_role(ctx, "time", required = TRUE)$variable
  cnsr <- pp_role(ctx, "censor", required = TRUE)$variable
  strata <- pp_role(ctx, "strata")
  src <- pp_use_ds(ctx, ctx$ds)
  flist <- pp_filters(ctx, "base", ctx$ds)
  id <- pp_opt(ctx, "id_var")

  steps <- list(src, pp_filter_code(ctx, flist, ctx$ds))
  s_var <- NULL
  s_values <- NULL
  if (!is.null(strata)) {
    if (strata$dataset != ctx$ds) {
      j <- pp_join(ctx, "base", strata$dataset, strata$variable, id, taken = c(time, cnsr))
      steps <- c(steps, j$code)
      s_var <- j$name
      s_values <- j$values
    } else {
      s_var <- strata$variable
      s_values <- pp_values_or_null(pp_filtered_data(ctx, ctx$ds, flist), s_var)
    }
  }
  steps <- c(steps, list(pp_time_mutate(ctx, time)))

  # colours: one per stratum (single arm: arm_label)
  if (is.null(s_var)) {
    arm <- pp_opt(ctx, "arm_label")
    pal <- tfl_fig_palettes()[[ctx$prow$palette %or% "treatment"]]
    sv <- list(code = sprintf("pal_strata <- %s", vec_code(stats::setNames(unname(pal[1]), arm))),
               name = "pal_strata", values = arm, colours = unname(pal[1]), labels = arm)
  } else {
    sv <- pp_scale_values(ctx, "pal_strata", s_var, s_values, "km_df", "treatment")
    if (!is.null(sv$values)) steps <- c(steps, sprintf("mutate(%s = factor(%s, levels = names(pal_strata)))", s_var, s_var))
  }

  data <- c(pipe_code("km_df", steps))
  fit <- sprintf("km_fit <- survfit2(Surv(%s, %s == 0) ~ %s, data = km_df)", time, cnsr, s_var %or% "1")

  lw <- pp_opt(ctx, "line_width")
  cshape <- pp_shape_code(pp_opt(ctx, "censor_shape"), "x")
  y_lab <- ctx$prow$y_label %or% "Survival Probability"
  x_lab <- ctx$prow$x_label %or% pp_time_label(ctx)

  base_terms <- list(
    if (is.null(s_var)) sprintf("ggsurvfit(km_fit, colour = pal_strata[[1]], linewidth = %s)", lw)
    else sprintf("ggsurvfit(km_fit, linewidth = %s)", lw),
    if (!is.null(s_var)) pp_scale_manual("colour", sv),
    "scale_x_continuous(breaks = x_breaks, expand = expansion(mult = c(0.02, 0.02)))",
    sprintf("scale_y_continuous(breaks = seq(0, 1, by = %s))", pp_opt(ctx, "y_by")),
    "coord_cartesian(xlim = c(0, x_max), ylim = c(0, 1))",
    sprintf("labs(x = %s, y = %s%s)", q(x_lab), q(y_lab),
            if (!is.na(ctx$prow$title)) paste0(", title = ", q(ctx$prow$title)) else "")
  )
  plot <- c("# ---- base: km ----", plus_code("p", c(base_terms, list(pp_theme_code(ctx)))))

  if (has_layer(ctx, "ci")) {
    plot <- c(plot, "# ---- layer: ci ----",
              if (is.null(s_var)) "p <- p + add_confidence_interval(fill = pal_strata[[1]])"
              else 'p <- p + add_confidence_interval() + scale_fill_manual(values = pal_strata, guide = "none")')
  }
  if (has_layer(ctx, "censor_mark")) {
    col <- if (is.null(s_var)) ", colour = pal_strata[[1]]" else ""
    plot <- c(plot, "# ---- layer: censor_mark ----",
              sprintf("p <- p + add_censor_mark(shape = %s, size = %s, stroke = %s%s)",
                      format(cshape), pp_opt(ctx, "censor_size"),
                      pp_opt(ctx, "censor_stroke"), col))
  }
  if (has_layer(ctx, "median_line")) {
    plot <- c(plot, "# ---- layer: median_line ----",
              sprintf("p <- p + geom_hline(yintercept = 0.5, linetype = %s, colour = %s, linewidth = %s)",
                      q(pp_opt(ctx, "median_linetype")), q(pp_opt(ctx, "median_colour")),
                      pp_opt(ctx, "median_line_width")))
  }

  panels <- list()
  risk_ard <- pp_opt(ctx, "risk_ard")
  if (has_layer(ctx, "n_at_risk")) {
    single <- is.null(s_var)
    risk_code <- if (is.na(risk_ard)) c(
      "sr <- summary(km_fit, times = x_breaks, extend = TRUE)",
      paste0("risk_df <- data.frame(\n",
             "  time   = sr$time,\n",
             if (single) sprintf("  strata = %s,\n", q(sv$values[1]))
             else "  strata = sub(\"^[^=]*=\", \"\", as.character(sr$strata)),\n",
             "  n_risk = sr$n.risk\n)")) else c(
      "# the number at risk is the KM table's (its ARD, cardx::ard_survival_survfit()),",
      "# so the figure and the table agree; the curve's own count is checked against it",
      sprintf("km_ard  <- %s", risk_ard),
      "km_ard  <- km_ard[km_ard$variable == \"time\" & km_ard$stat_name == \"n.risk\", ]",
      paste0("risk_df <- data.frame(\n",
             "  time   = as.numeric(unlist(km_ard$variable_level))",
             if (!identical(pp_opt(ctx, "time_unit"), "as_is"))
               paste0(" / ", format(pp_time_divisor[[pp_opt(ctx, "time_unit")]]),
                      ",   # in the axis's unit\n")
             else ",\n",
             if (single) sprintf("  strata = %s,\n", q(sv$values[1]))
             else "  strata = as.character(unlist(km_ard$group1_level)),\n",
             "  n_risk = as.numeric(unlist(km_ard$stat))\n)"),
      "sr <- summary(km_fit, times = sort(unique(risk_df$time)), extend = TRUE)",
      paste0("curve_n <- data.frame(time = sr$time, n = sr$n.risk, strata = ",
             if (single) q(sv$values[1])
             else "sub(\"^[^=]*=\", \"\", as.character(sr$strata))", ")"),
      "chk <- merge(risk_df, curve_n, by = c(\"strata\", \"time\"))",
      "if (nrow(chk) != nrow(risk_df) || any(chk$n_risk != chk$n)) {",
      "  warning(\"Figure check: the number at risk of the ARD differs from the curve's (\",",
      "          sum(chk$n_risk != chk$n) + nrow(risk_df) - nrow(chk), \" cell(s))\", call. = FALSE)",
      "}")
    plot <- c(plot, "# ---- layer: n_at_risk (separate panel, combined below the curve) ----",
      risk_code,
      "risk_df$strata <- factor(risk_df$strata, levels = rev(names(pal_strata)))",
      plus_code("p_risk", list(
        "ggplot(risk_df, aes(x = time, y = strata, label = n_risk, colour = strata))",
        sprintf("geom_text(size = %s)", pp_opt(ctx, "text_size")),
        'scale_colour_manual(values = pal_strata, guide = "none")',
        "scale_x_continuous(breaks = x_breaks, expand = expansion(mult = c(0.02, 0.02)))",
        'coord_cartesian(xlim = c(0, x_max), clip = "off")',
        sprintf("labs(title = %s, x = NULL, y = NULL)", q(pp_opt(ctx, "risk_title"))),
        sprintf("theme_void(base_size = %s)", pp_opt(ctx, "base_size")),
        paste0("theme(\n",
               "  plot.title          = element_text(hjust = 0, size = rel(0.9)),\n",
               "  plot.title.position = \"plot\",\n",
               "  axis.text.y         = element_text(hjust = 1, margin = margin(r = 5))\n)")
      )))
    panels <- list(list(name = "p_risk", size = pp_opt_num(ctx, "risk_height")))
  }

  # auto legend items: one line per stratum (+ censor mark)
  parts <- list()
  if (!is.null(sv$values)) {
    parts <- list(legend_part(sv$labels, "line", colour = sv$colours, linetype = "solid"))
  } else {
    parts <- list('tibble::tibble(label = names(pal_strata), glyph = "line", shape = NA, colour = unname(pal_strata), fill = NA, linetype = "solid")')
  }
  if (has_layer(ctx, "censor_mark")) {
    parts <- c(parts, list(legend_part("Censored", "point", shape = cshape,
                                       colour = if (is.null(s_var)) sv$colours[1] else "black")))
  }

  list(
    data = c(pal_before(sv), data, pal_after(sv), fit, pp_x_axis_lines(ctx, "km_df", time)),
    plot = plot,
    panels = panels,
    parts = parts,
    libs = c("dplyr", "ggplot2", "ggsurvfit", "patchwork")
  )
}
