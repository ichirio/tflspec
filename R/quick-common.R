# Shared building blocks of the template-based quick functions
# (forest, bar, mean, individual, box, ae_dot, butterfly, edish, scatter, pk).
#
# Conventions of the generated scripts:
# - group / population / subgroup variables always come from ADSL (joined by
#   the subject key after dropping any copies in the analysis dataset), so a
#   BDS dataset does not need to carry them;
# - Step1 data, Step2 figure, Step3 ggsave, like the engine-based types.

pp_q_legends <- c("none", "right", "bottom", "top", "inside", "inside_tl", "inside_br", "inside_bl")

# Theme terms of a preset (shared with the engine).
pp_theme_lines <- function(theme = "boxed", base_size = NULL, opt = NULL) {
  # sizes and line widths come from the figure style standard (tfl_fig_style()),
  # or from `opt(key)` -- a plot's own options -- when the caller has them
  if (is.null(opt)) {
    d <- pp_default_options()
    opt <- function(k) d[[k]]
  }
  o <- opt
  bs <- base_size %||% o("base_size")
  text_sizes <- paste0(
    "  axis.title        = element_text(size = ", o("axis_title_size"), "),
",
    "  axis.text         = element_text(size = ", o("axis_text_size"), "),
",
    "  legend.text       = element_text(size = ", o("legend_text_size"), "),
",
    "  legend.key.size   = unit(", o("legend_key_size"), ", \"lines\"),
")
  switch(theme,
    boxed = c(
      sprintf("theme_minimal(base_size = %s)", bs),
      paste0("theme(
", text_sizes,
             "  panel.border      = element_rect(colour = \"black\", fill = NA, linewidth = ", o("panel_border_width"), "),
",
             "  panel.grid        = element_blank(),
",
             "  axis.ticks        = element_line(linewidth = ", o("tick_width"), "),
",
             "  axis.ticks.length = unit(", o("tick_length"), ", \"mm\")
)")
    ),
    L_axis = c(
      sprintf("theme_minimal(base_size = %s)", bs),
      paste0("theme(
", text_sizes,
             "  panel.grid = element_blank(),
",
             "  axis.line  = element_line(colour = \"black\", linewidth = ", o("axis_line_width"), "),
",
             "  axis.ticks = element_line(linewidth = ", o("axis_line_width"), ")
)")
    ),
    minimal = sprintf("theme_minimal(base_size = %s)", bs),
    classic = sprintf("theme_classic(base_size = %s)", bs),
    stop("Unknown theme: ", theme, call. = FALSE)
  )
}

pp_q_legend <- function(legend, title_blank = TRUE) {
  if (!legend %in% pp_q_legends) {
    stop("`legend` must be one of: ", paste(pp_q_legends, collapse = ", "), call. = FALSE)
  }
  out <- if (legend == "none") {
    'theme(legend.position = "none")'
  } else if (grepl("^inside", legend)) {
    pos <- if (legend == "inside") "inside_tr" else legend
    j <- pp_inside_just[[pos]]
    sprintf(paste0("theme(\n",
                   "  legend.position        = \"inside\",\n",
                   "  legend.position.inside = %s,\n",
                   "  legend.justification   = %s,\n",
                   "  legend.background      = element_rect(colour = \"black\", fill = \"white\", linewidth = 0.3)\n)"),
            vec_code(abs(j - 0.02)), vec_code(j))
  } else {
    sprintf('theme(legend.position = "%s")', legend)
  }
  if (title_blank && legend != "none") out <- c(out, "theme(legend.title = element_blank())")
  out
}

pp_q_data_expr <- function(data) {
  expr <- getOption("tflspec.data_expr") %or% "{ds}"
  gsub("{ds}", pp_ds_name(data), gsub("{DS}", data, expr, fixed = TRUE), fixed = TRUE)
}

pp_q_load <- function(datasets) {
  datasets <- unique(datasets)
  out <- character()
  for (d in datasets) {
    rhs <- pp_q_data_expr(d)
    if (rhs != pp_ds_name(d)) out <- c(out, sprintf("%s <- %s", pp_ds_name(d), rhs))
  }
  if (!length(out)) out <- sprintf("# Input data frames: %s", paste(pp_ds_name(datasets), collapse = ", "))
  out
}

# filter() code from a named list of values; numbers stay unquoted.
pp_q_filter <- function(conds, df = NULL) {
  conds <- conds[!vapply(conds, is.null, logical(1))]
  if (!length(conds)) return(NULL)
  terms <- vapply(names(conds), function(v) {
    x <- conds[[v]]
    num <- if (!is.null(df) && v %in% names(df)) is.numeric(df[[v]]) else is.numeric(x)
    lit <- if (num) format(x) else q(x)
    if (length(x) == 1) sprintf("%s == %s", v, lit) else sprintf("%s %%in%% c(%s)", v, paste(lit, collapse = ", "))
  }, character(1))
  sprintf("filter(%s)", paste(terms, collapse = ", "))
}

pp_q_pop <- function(pop) if (is.null(pop)) NULL else sprintf('filter(%s == "Y")', pop)

# Extra record condition written by the user (R code), e.g. 'AVISIT != "Retrieval"'.
pp_q_where <- function(where) if (is.null(where)) NULL else sprintf("filter(%s)", where)

# ADSL variables joined into a BDS dataset.
pp_q_adsl_join <- function(vars, key = "USUBJID") {
  vars <- unique(vars[!vapply(vars, is.null, logical(1))])
  vars <- setdiff(unlist(vars), key)
  if (!length(vars)) return(NULL)
  v <- paste(q(vars), collapse = ", ")
  c(sprintf("select(-any_of(c(%s)))", v),
    sprintf("left_join(adsl |> select(%s, all_of(c(%s))), by = %s)", key, v, q(key)))
}

# Evaluate a data step on the ADaM data at generation time (for literal
# codelists). Returns NULL when data or columns are missing.
pp_q_eval <- function(adam, data, conds = list(), adsl_vars = NULL, key = "USUBJID", where = NULL) {
  if (is.null(adam) || is.null(adam[[data]])) return(NULL)
  df <- adam[[data]]
  if (length(adsl_vars) && data != "ADSL" && !is.null(adam$ADSL)) {
    sl <- adam$ADSL
    # paired numeric codes (TRT01AN for TRT01A) give the order of the values
    keep <- intersect(c(key, adsl_vars, paste0(adsl_vars, "N")), names(sl))
    df <- df[setdiff(names(df), setdiff(keep, key))]
    df <- merge(df, sl[keep], by = key, all.x = TRUE, sort = FALSE)
  }
  for (v in names(conds)) {
    if (is.null(conds[[v]])) next
    if (!v %in% names(df)) return(NULL)
    df <- df[as.character(df[[v]]) %in% as.character(conds[[v]]), , drop = FALSE]
  }
  if (!is.null(where)) {
    keep <- tryCatch(eval(parse(text = where)[[1]], df, baseenv()), error = function(e) NULL)
    if (is.logical(keep) && length(keep) == nrow(df)) df <- df[keep %in% TRUE, , drop = FALSE]
  }
  df
}

# Named colour vector for the values of `var`. Literal when `df` is known,
# otherwise computed when the script runs from `df_name`.
pp_q_pal <- function(obj, df, var, df_name, palette = "treatment", values = NULL) {
  pals <- tfl_fig_palettes()
  if (!palette %in% names(pals)) stop("Unknown palette: ", palette, call. = FALSE)
  pal <- pals[[palette]]
  fallback <- if (is.null(names(pal))) pal else pals$okabe_ito
  if (is.null(values) && !is.null(df) && var %in% names(df)) values <- pp_values(df, var)
  if (is.null(values)) {
    if (!is.null(names(pal))) {
      return(sprintf("%s <- %s", obj, vec_code(pal)))
    }
    return(sprintf("%s_lv <- if (is.factor(%s$%s)) levels(%s$%s) else sort(unique(na.omit(%s$%s)))\n%s <- setNames(%s[seq_along(%s_lv)], %s_lv)",
                   obj, df_name, var, df_name, var, df_name, var, obj, vec_code(unname(fallback)), obj, obj))
  }
  k <- 0
  cols <- vapply(values, function(v) {
    if (!is.null(names(pal)) && v %in% names(pal)) return(unname(pal[v]))
    k <<- k + 1
    fallback[(k - 1) %% length(fallback) + 1]
  }, character(1))
  sprintf("%s <- %s", obj, vec_code(stats::setNames(cols, values)))
}

# Group palette plus a factor() so that plots follow the palette order
# (e.g. TRT01AN order instead of alphabetical).
pp_q_group_pal <- function(obj, df, var, df_name, palette) {
  c(pp_q_pal(obj, df, var, df_name, palette),
    sprintf("%s <- %s |> mutate(%s = factor(%s, levels = names(%s)))", df_name, df_name, var, var, obj))
}

pp_q_script <- function(pid, what, title, libs, data, plot, width, height, dpi = 300, units = "in",
                        file = NULL) {
  code <- paste(c(
    pp_seq_header(pid, what, title, libs),
    section("data"),
    paste(data[!vapply(data, is.null, logical(1))], collapse = "\n\n"),
    "",
    section("the figure"),
    paste(plot, collapse = "\n"),
    "fig",
    "",
    pp_seq_save(pid, width, height, dpi, units)
  ), collapse = "\n")
  pp_seq_finish(code, file)
}

pp_q_check_cols <- function(adam, data, cols) {
  if (is.null(adam)) return(invisible())
  if (is.null(adam[[data]])) stop("Dataset ", data, " not in `adam`.", call. = FALSE)
  miss <- setdiff(unlist(cols), names(adam[[data]]))
  if (length(miss)) stop(data, " has no column(s): ", paste(miss, collapse = ", "), call. = FALSE)
  invisible()
}

pp_q_param_label <- function(adam, data, param, fallback) {
  df <- if (!is.null(adam)) adam[[data]] else NULL
  if (!is.null(df) && all(c("PARAMCD", "PARAM") %in% names(df))) {
    lab <- unique(df$PARAM[df$PARAMCD %in% param])
    if (length(lab) == 1) return(lab)
  }
  fallback
}
