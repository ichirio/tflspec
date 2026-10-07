pp_gen_swimmer <- function(ctx) {
  id <- pp_role(ctx, "id", required = TRUE)$variable
  end <- pp_role(ctx, "end", required = TRUE)$variable
  # subtype: bars from 0 (default) or from a start variable to `end`
  start <- pp_role(ctx, "start")$variable
  x0 <- start %or% "0"
  bar_col <- pp_role(ctx, "colour")
  key <- pp_opt(ctx, "id_var")  # join key; `id` is only the y-axis label
  src <- pp_use_ds(ctx, ctx$ds)
  flist <- pp_filters(ctx, "base", ctx$ds)
  taken <- c(id, end)

  # ---- subject-level bar data ----
  steps <- list(src, pp_filter_code(ctx, flist, ctx$ds))
  time_vars <- c(start, end)
  c_var <- NULL
  sv_bar <- NULL
  if (!is.null(bar_col)) {
    if (bar_col$dataset != ctx$ds) {
      j <- pp_join(ctx, "base", bar_col$dataset, bar_col$variable, key, taken)
      steps <- c(steps, j$code)
      c_var <- j$name
      c_values <- j$values
    } else {
      c_var <- bar_col$variable
      c_values <- pp_values_or_null(pp_filtered_data(ctx, ctx$ds, flist), c_var)
    }
    taken <- c(taken, c_var)
    sv_bar <- pp_scale_values(ctx, "pal_bar", c_var, c_values, "bar_df", "response_light")
  }

  # event markers: variables on the subject-level data (joined when elsewhere)
  events <- NULL
  if (has_layer(ctx, "event_marker")) {
    ev <- pp_roles(ctx, "event_marker")
    ev <- ev[!is.na(ev$variable), , drop = FALSE]
    if (!nrow(ev)) stop("Plot ", ctx$pid, ": layer event_marker needs role rows (role = x).", call. = FALSE)
    ev_names <- character(nrow(ev))
    for (i in seq_len(nrow(ev))) {
      if (ev$dataset[i] != ctx$ds) {
        j <- pp_join(ctx, "event_marker", ev$dataset[i], ev$variable[i], key, taken)
        steps <- c(steps, j$code)
        ev_names[i] <- j$name
      } else {
        ev_names[i] <- ev$variable[i]
      }
      taken <- c(taken, ev_names[i])
    }
    events <- data.frame(
      var = ev_names,
      label = ifelse(is.na(ev$label), ev$variable, ev$label),
      shape = vapply(ev$shape, pp_shape_code, numeric(1), default = "triangle_down"),
      colour = ifelse(is.na(ev$colour), "black", ev$colour),
      stringsAsFactors = FALSE
    )
    time_vars <- c(time_vars, ev_names)
  }
  steps <- c(steps, list(pp_time_mutate(ctx, time_vars)),
             sprintf("filter(!is.na(%s))", end),
             paste0("mutate(\n  Y_ID = reorder(", id, ", ", end, ")",
                    if (!is.null(c_var) && !is.null(sv_bar$values)) sprintf(",\n  %s = factor(%s, levels = names(pal_bar))", c_var, c_var) else "",
                    "\n)"))
  data <- c(pipe_code("bar_df", steps))

  # ---- assessment-level data ----
  sv_as <- NULL
  if (has_layer(ctx, "assessment_marker")) {
    ax <- pp_role(ctx, "x", "assessment_marker", required = TRUE)
    af <- pp_role(ctx, "fill", "assessment_marker")
    a_ds <- ax$dataset
    a_src <- pp_use_ds(ctx, a_ds)
    a_flist <- pp_filters(ctx, "assessment_marker", a_ds)
    a_steps <- list(a_src, pp_filter_code(ctx, a_flist, a_ds), pp_time_mutate(ctx, ax$variable),
                    sprintf("inner_join(bar_df |> select(%s, Y_ID), by = %s)", key, q(key)))
    if (!is.null(af)) {
      sv_as <- pp_scale_values(ctx, "pal_assess", af$variable,
                               pp_values_or_null(pp_filtered_data(ctx, a_ds, a_flist), af$variable),
                               "assess_df", "response", palette = pp_opt(ctx, "marker_palette"))
      if (!is.null(sv_as$values)) {
        a_steps <- c(a_steps, sprintf("mutate(%s = factor(%s, levels = names(pal_assess)))", af$variable, af$variable))
      }
    }
    data <- c(data, pipe_code("assess_df", a_steps))
  }

  if (!is.null(events)) {
    data <- c(data,
      sprintf("event_shape  <- %s", vec_code(stats::setNames(events$shape, events$label))),
      sprintf("event_colour <- %s", vec_code(stats::setNames(events$colour, events$label))),
      paste0("event_df <- bind_rows(\n",
             paste(sprintf("  bar_df |> transmute(Y_ID, X = %s, EVENT = %s)", events$var, q(events$label)), collapse = ",\n"),
             "\n) |>\n  filter(!is.na(X)) |>\n  mutate(EVENT = factor(EVENT, levels = names(event_shape)))"))
  }
  data <- c(pal_before(sv_bar, sv_as), data, pal_after(sv_bar, sv_as), pp_x_axis_lines(ctx, "bar_df", end))
  grid <- has_layer(ctx, "visit_grid")
  if (grid) {
    ve <- pp_opt(ctx, "visit_every")
    if (is.na(ve)) stop("Plot ", ctx$pid, ": layer visit_grid needs option visit_every.", call. = FALSE)
    data <- c(data, sprintf("visit_x <- seq(%s, x_max, by = %s)", ve, ve))
  }

  # ---- plot ----
  bw <- pp_opt(ctx, "bar_width")
  ms <- pp_opt(ctx, "marker_size")
  x_lab <- ctx$prow$x_label %or% pp_time_label(ctx)
  y_lab <- ctx$prow$y_label
  base_terms <- list(
    "ggplot(bar_df, aes(y = Y_ID))",
    if (!is.null(c_var)) {
      c(sprintf("geom_segment(aes(x = %s, xend = %s, yend = Y_ID, colour = %s), linewidth = %s)", x0, end, c_var, bw),
        pp_scale_manual("colour", sv_bar, 'na.value = "grey80"'))
    } else sprintf('geom_segment(aes(x = %s, xend = %s, yend = Y_ID), colour = "#9DB4C0", linewidth = %s)', x0, end, bw),
    if (grid) {
      paste0("scale_x_continuous(
",
             "  breaks = x_breaks, expand = expansion(mult = c(0, 0.02)),
",
             "  sec.axis = dup_axis(breaks = visit_x, labels = paste(", q(pp_opt(ctx, "visit_label")), ", visit_x), name = NULL)
)")
    } else "scale_x_continuous(breaks = x_breaks, expand = expansion(mult = c(0, 0.02)))",
    "coord_cartesian(xlim = c(0, x_max))",
    sprintf("labs(x = %s, y = %s%s)", q(x_lab), if (is.na(y_lab)) "NULL" else q(y_lab),
            if (!is.na(ctx$prow$title)) paste0(", title = ", q(ctx$prow$title)) else ""),
    pp_theme_code(ctx),
    "theme(axis.text.y = element_text(size = rel(0.7)))"
  )
  plot <- c("# ---- base: swimmer (bars ordered by length, longest on top) ----", plus_code("p", base_terms))

  if (has_layer(ctx, "assessment_marker")) {
    ax <- pp_role(ctx, "x", "assessment_marker")$variable
    af <- pp_role(ctx, "fill", "assessment_marker")
    plot <- c(plot, "# ---- layer: assessment_marker ----",
      if (!is.null(af)) {
        plus_code("p", list(
          sprintf('geom_point(data = assess_df, aes(x = %s, fill = %s), shape = 23, size = %s, colour = "transparent")', ax, af$variable, ms),
          pp_scale_manual("fill", sv_as),
          'guides(fill = guide_legend(override.aes = list(shape = 23, colour = "transparent", size = 2.5)))'
        ), append = TRUE)
      } else {
        sprintf('p <- p + geom_point(data = assess_df, aes(x = %s), shape = 23, size = %s, fill = "black", colour = "transparent")', ax, ms)
      })
  }
  if (!is.null(events)) {
    ev_code <- plus_code("p", list(
        paste0("geom_point(
",
               "  data = event_df,
",
               "  aes(x = X, shape = EVENT,
",
               "      colour = I(event_colour[as.character(EVENT)]),
",
               "      fill   = I(event_colour[as.character(EVENT)])),
",
               "  size = ", pp_opt(ctx, "event_size"), "
)"),
        "scale_shape_manual(values = event_shape, breaks = names(event_shape))",
        "guides(shape = guide_legend(override.aes = list(colour = unname(event_colour), fill = unname(event_colour))))"
      ), append = TRUE)
    plot <- c(plot, "# ---- layer: event_marker (one row per event variable; skipped when no event) ----",
      paste0("if (nrow(event_df) > 0) {
", paste(indent(ev_code), collapse = "
"), "
}"))
  }
  if (has_layer(ctx, "ongoing_arrow")) {
    o_flist <- pp_filters(ctx, "ongoing_arrow", ctx$ds)
    if (!length(o_flist)) stop("Plot ", ctx$pid, ": layer ongoing_arrow needs a filter row (layer = ongoing_arrow), e.g. EOSSTT = ONGOING.", call. = FALSE)
    plot <- c(plot, "# ---- layer: ongoing_arrow ----",
      paste0("p <- p + geom_segment(\n",
             "  data = bar_df |> ", pp_filter_code(ctx, o_flist, ctx$ds), ",\n",
             "  aes(x = ", end, ", xend = ", end, " + x_max * 0.03, yend = Y_ID),\n",
             "  arrow = arrow(length = unit(0.12, \"cm\"), type = \"closed\"), linewidth = 0.3\n)"))
  }
  if (grid) {
    plot <- c(plot, "# ---- layer: visit_grid (labels on the top axis) ----",
      plus_code("p", list(
        sprintf('geom_vline(xintercept = visit_x, linetype = %s, colour = %s)',
                q(pp_opt(ctx, "visit_linetype")), q(pp_opt(ctx, "visit_colour"))),
        "theme(axis.ticks.x.top = element_blank(), axis.line.x.top = element_blank())"
      ), append = TRUE))
  }

  # auto legend items: assessment responses, bar colours, events
  parts <- list()
  if (!is.null(sv_as)) {
    parts <- c(parts, list(if (!is.null(sv_as$values)) legend_part(sv_as$labels, "point", shape = 23, fill = sv_as$colours)
      else 'tibble::tibble(label = names(pal_assess), glyph = "point", shape = 23, colour = NA, fill = unname(pal_assess), linetype = NA)'))
  }
  if (!is.null(sv_bar)) {
    prefix <- pp_opt(ctx, "bar_legend_prefix") %or% ""
    if (nzchar(trimws(prefix))) prefix <- paste0(trimws(prefix), " ")
    parts <- c(parts, list(if (!is.null(sv_bar$values)) legend_part(paste0(prefix, sv_bar$labels), "rect", fill = sv_bar$colours)
      else sprintf('tibble::tibble(label = paste0(%s, names(pal_bar)), glyph = "rect", shape = NA, colour = NA, fill = unname(pal_bar), linetype = NA)', q(prefix))))
  }
  if (!is.null(events)) {
    parts <- c(parts, list(legend_part(events$label, "point", shape = events$shape,
                                       colour = events$colour, fill = events$colour)))
  }

  list(data = data, plot = plot, panels = list(), parts = parts,
       libs = c("dplyr", "ggplot2", "patchwork"))
}
