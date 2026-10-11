# Quick API: pick a plot type and a style, get a skeleton script.
#
# Each style is a fixed combination of layer patterns with ADaM-standard
# defaults (AVAL/CNSR, TRT01P, ADRS BOR/OVR, ...). Only arguments that differ
# from the standard need to be given; everything else is finished by hand in
# the generated script.

pp_style_layers <- list(
  km = list(
    risk_table = c("censor_mark", "median_line", "n_at_risk"),
    simple     = "censor_mark",
    ci         = c("censor_mark", "ci", "n_at_risk"),
    single_arm = c("censor_mark", "median_line", "n_at_risk")
  ),
  waterfall = list(
    response = c("ref_lines", "ref_labels"),
    plain    = c("ref_lines", "ref_labels")
  ),
  swimmer = list(
    full       = c("assessment_marker", "event_marker", "ongoing_arrow"),
    assessment = c("assessment_marker", "ongoing_arrow"),
    response   = "ongoing_arrow",
    bar        = "ongoing_arrow"
  )
)

# legend presets of the quick API -> (legend_type, legend_pos)
pp_quick_legends <- list(
  none         = c("none", "right"),
  right        = c("mapped", "right"),
  bottom       = c("mapped", "bottom"),
  inside       = c("mapped", "inside_tr"),
  inside_bl    = c("mapped", "inside_bl"),
  panel        = c("manual", "below"),
  panel_right  = c("manual", "right"),
  panel_inside = c("manual", "inside_tr")
)

pp_time_units <- c(days = "as_is", weeks = "days_to_weeks", months = "days_to_months", years = "days_to_years")

#' Code object returned by the quick API
#' @param x A `tfl_code` object.
#' @param ... Unused.
#' @return `x`, invisibly (the script is printed).
#' @export
print.tfl_code <- function(x, ...) {
  cat(x, sep = "\n\n")
  invisible(x)
}

pp_finish <- function(spec, adam, file) {
  code <- tfl_fig_code(spec, adam = adam)
  if (!is.null(file)) {
    dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
    writeLines(code, file, useBytes = TRUE)
  }
  structure(unname(code), class = "tfl_code", spec = spec)
}

# Global defaults a study sets once, e.g.
# options(tflspec.data_expr = "adam_data${ds}",
#         tflspec.fig_path  = 'file.path(output_path, "{plot_id}.png")')
pp_global_options <- function() {
  keys <- names(pp_default_options())
  vals <- lapply(keys, function(k) getOption(paste0("tflspec.", k)))
  names(vals) <- keys
  vals[!vapply(vals, is.null, logical(1))]
}

# `...` of the quick functions = options of the engine (x_max = 24, ...).
pp_options_df <- function(pid, dots) {
  dots <- c(pp_global_options()[setdiff(names(pp_global_options()), names(dots))], dots)
  dots <- dots[!vapply(dots, is.null, logical(1))]
  bad <- setdiff(names(dots), names(pp_default_options()))
  if (length(bad)) {
    stop("Unknown argument(s): ", paste(bad, collapse = ", "),
         ". Options are: ", paste(names(pp_default_options()), collapse = ", "), call. = FALSE)
  }
  if (!length(dots)) return(NULL)
  data.frame(plot_id = pid, key = names(dots),
             value = vapply(dots, function(v) paste(v, collapse = ","), character(1)),
             stringsAsFactors = FALSE)
}

pp_legend_cols <- function(legend) {
  if (!legend %in% names(pp_quick_legends)) {
    stop("`legend` must be one of: ", paste(names(pp_quick_legends), collapse = ", "), call. = FALSE)
  }
  pp_quick_legends[[legend]]
}

pp_rows <- function(...) {
  rows <- list(...)
  rows <- rows[!vapply(rows, is.null, logical(1))]
  do.call(rbind, lapply(rows, function(r) as.data.frame(r, stringsAsFactors = FALSE)))
}

# Population flag filter on the base dataset; skipped (with a warning) when
# the dataset has no such variable.
pp_pop_filter <- function(pid, pop, data, adam) {
  if (is.null(pop) || is.na(pop)) return(NULL)
  if (!is.null(adam) && !is.null(adam[[data]]) && !pop %in% names(adam[[data]])) {
    warning(pop, " is not in ", data, "; population filter skipped.", call. = FALSE)
    return(NULL)
  }
  list(plot_id = pid, layer = NA, dataset = NA, variable = pop, value = "Y")
}

pp_prep_adam <- function(adam) if (is.null(adam)) NULL else tfl_read_adam(adam)

#' Kaplan-Meier plot code (ggsurvfit)
#'
#' @param adam Optional ADaM data ([tfl_read_adam()]); values found in the data
#'   (groups, colours) are written literally into the code.
#' @param param PARAMCD of the time-to-event parameter.
#' @param group Grouping variable (`NULL` for one curve).
#' @param style One of [tfl_fig_types()] for `km`.
#' @param legend `none`, `right`, `bottom`, `inside`, `inside_bl`, `panel`,
#'   `panel_right`, `panel_inside` (`panel*` = legend drawn from an item table).
#' @param pop Population flag (`== "Y"`); `NULL` for none.
#' @param data Dataset name.
#' @param time,censor Time and censor (1 = censored) variables.
#' @param time_unit Unit of `time` in the data is days; the axis is shown in
#'   `days`, `weeks`, `months` or `years`.
#' @param title Figure title.
#' @param file Write the script to this file.
#' @param plot_id Used in the output file name.
#' @param ard R code (a string) that gives the ARD of the KM table --
#'   `cardx::ard_survival_survfit(times = )` -- e.g.
#'   `'readRDS("output/ard/ard.rds") |> subset(output_id == "T-14-2-2")'`.
#'   The number at risk is then read from it, so the figure and the table
#'   agree, and checked against the curve's own count.  The risk table's
#'   times are the ARD's; the ARD's time unit must be the axis's.
#' @param ... Engine options, e.g. `x_max = 24`, `x_by = 3`, `palette = "grey"`,
#'   `theme = "classic"`, `width = 7`, `height = 5`.
#' @return A `tfl_code` object (character; printed as the script).
#' @export
tfl_fig_km <- function(adam = NULL, param = "OS", group = "TRT01P",
                  style = c("risk_table", "simple", "ci", "single_arm"),
                  legend = NULL, pop = "FASFL", data = "ADTTE",
                  time = "AVAL", censor = "CNSR", time_unit = "months",
                  title = NULL, file = NULL, plot_id = "km", ard = NULL, ...) {
  style <- match.arg(style)
  adam <- pp_prep_adam(adam)
  if (style == "single_arm") group <- NULL
  legend <- legend %or% if (is.null(group)) "none" else "inside"
  pp_finish(pp_quick_spec(
    pid = plot_id, type = "km", style = style, data = data, legend = legend, title = title,
    roles = pp_rows(
      list(plot_id = plot_id, layer = NA, role = "time", dataset = NA, variable = time),
      list(plot_id = plot_id, layer = NA, role = "censor", dataset = NA, variable = censor),
      if (!is.null(group)) list(plot_id = plot_id, layer = NA, role = "strata", dataset = NA, variable = group)
    ),
    filters = pp_rows(
      if (!is.null(param)) list(plot_id = plot_id, layer = NA, dataset = NA, variable = "PARAMCD", value = param),
      pp_pop_filter(plot_id, pop, data, adam)
    ),
    time_unit = time_unit,
    dots = c(if (!is.null(ard)) list(risk_ard = ard), list(...)),
    default_palette = "treatment"
  ), adam, file)
}

#' Waterfall plot code
#'
#' @inheritParams tfl_fig_km
#' @param style One of [tfl_fig_types()] for `waterfall`.
#' @param param PARAMCD of the one-row-per-subject parameter holding the
#'   best percent change (`NULL` when the dataset has no PARAMCD).
#' @param value Variable with the best percent change.
#' @param response PARAMCD of the best overall response in `response_data`
#'   (value taken from `AVALC`).
#' @param response_data Dataset of the response.
#' @param id Subject key.
#' @return A `tfl_code` object: the figure's 'ggplot2' script, a character
#'   vector of class `tfl_code` with its specification in attribute `spec`;
#'   printing it shows the script.
#' @export
tfl_fig_waterfall <- function(adam = NULL, style = c("response", "plain"),
                         param = "BPCHG", value = "AVAL", response = "BOR",
                         legend = "inside", pop = "FASFL", data = "ADTR",
                         response_data = "ADRS", id = "USUBJID",
                         title = NULL, file = NULL, plot_id = "waterfall", ...) {
  style <- match.arg(style)
  adam <- pp_prep_adam(adam)
  if (style == "plain") legend <- "none"
  pp_finish(pp_quick_spec(
    pid = plot_id, type = "waterfall", style = style, data = data, legend = legend, title = title,
    roles = pp_rows(
      list(plot_id = plot_id, layer = NA, role = "id", dataset = NA, variable = id),
      list(plot_id = plot_id, layer = NA, role = "value", dataset = NA, variable = value),
      if (style == "response") list(plot_id = plot_id, layer = NA, role = "fill", dataset = response_data, variable = "AVALC")
    ),
    filters = pp_rows(
      if (!is.null(param)) list(plot_id = plot_id, layer = NA, dataset = NA, variable = "PARAMCD", value = param),
      pp_pop_filter(plot_id, pop, data, adam),
      if (style == "response") list(plot_id = plot_id, layer = NA, dataset = response_data, variable = "PARAMCD", value = response)
    ),
    time_unit = "days", dots = c(list(id_var = id), list(...)), default_palette = "response"
  ), adam, file)
}

#' Swimmer plot code
#'
#' @inheritParams tfl_fig_km
#' @param style One of [tfl_fig_types()] for `swimmer`.
#' @param duration Bar length variable (days) on `data`; bars start at 0.
#' @param start,end Subtype "from start": bars run from `start` to `end`
#'   (days on a common origin, e.g. randomization) instead of 0 to `duration`.
#' @param id Y-axis label variable.
#' @param response PARAMCD of the best overall response (bar colour).
#' @param assessment PARAMCD of the response at each assessment.
#' @param day Assessment day variable in `response_data`.
#' @param events Named vector of event day variables on `data`, e.g.
#'   `c(Death = "DTHADY", Discontinued = "EOSDY")`. Shapes and colours are
#'   assigned in order; change them in the script.
#' @param ongoing Named value marking ongoing subjects, e.g.
#'   `c(EOSSTT = "ONGOING")`; `NULL` for no arrows.
#' @param response_data Dataset with `response` and `assessment`.
#' @param visit_every Draw dotted visit lines every this many time units.
#' @param key Join key between datasets.
#' @return A `tfl_code` object: the figure's 'ggplot2' script, a character
#'   vector of class `tfl_code` with its specification in attribute `spec`;
#'   printing it shows the script.
#' @export
tfl_fig_swimmer <- function(adam = NULL, style = c("full", "assessment", "response", "bar"),
                       duration = "TRTDURD", start = NULL, end = NULL, id = "SUBJID", response = "BOR",
                       assessment = "OVR", day = "ADY",
                       events = c(Death = "DTHADY"), ongoing = c(EOSSTT = "ONGOING"),
                       legend = "panel", pop = "FASFL", data = "ADSL", response_data = "ADRS",
                       time_unit = "months", visit_every = NULL, key = "USUBJID",
                       title = NULL, file = NULL, plot_id = "swimmer", ...) {
  style <- match.arg(style)
  adam <- pp_prep_adam(adam)
  if (!is.null(start) && is.null(end)) stop("`start` needs `end` (bars run from `start` to `end`).", call. = FALSE)
  with_resp <- style != "bar"
  with_assess <- style %in% c("full", "assessment")
  with_events <- style == "full" && length(events) > 0
  if (with_events && (is.null(names(events)) || any(names(events) == ""))) {
    stop("`events` must be named, e.g. c(Death = \"DTHADY\").", call. = FALSE)
  }
  ev_shapes <- c("triangle_down", "circle", "triangle", "square", "x", "diamond")
  ev_cols <- c("black", "#FF00FF", "black", "#00A0A0", "black", "#E69F00")
  k <- seq_along(events)
  ev_shapes <- ev_shapes[(k - 1) %% 6 + 1]
  ev_cols <- ev_cols[(k - 1) %% 6 + 1]
  # an event named like a marker of the figure style standard takes it
  for (i in k) {
    m <- .fs_marker(names(events)[i])
    if (!is.null(m)) {
      ev_shapes[i] <- m$shape
      if (!is.na(m$fill)) ev_cols[i] <- m$fill
    }
  }
  roles <- pp_rows(
    list(plot_id = plot_id, layer = NA, role = "id", dataset = NA, variable = id, label = NA, shape = NA, colour = NA),
    list(plot_id = plot_id, layer = NA, role = "end", dataset = NA, variable = end %or% duration, label = NA, shape = NA, colour = NA),
    if (!is.null(start)) list(plot_id = plot_id, layer = NA, role = "start", dataset = NA, variable = start, label = NA, shape = NA, colour = NA),
    if (with_resp) list(plot_id = plot_id, layer = NA, role = "colour", dataset = response_data, variable = "AVALC", label = NA, shape = NA, colour = NA),
    if (with_assess) list(plot_id = plot_id, layer = "assessment_marker", role = "x", dataset = response_data, variable = day, label = NA, shape = NA, colour = NA),
    if (with_assess) list(plot_id = plot_id, layer = "assessment_marker", role = "fill", dataset = response_data, variable = "AVALC", label = NA, shape = NA, colour = NA),
    if (with_events) data.frame(plot_id = plot_id, layer = "event_marker", role = "x", dataset = NA,
                                variable = unname(events), label = names(events),
                                shape = ev_shapes, colour = ev_cols)
  )
  filters <- pp_rows(
    pp_pop_filter(plot_id, pop, data, adam),
    if (with_resp) list(plot_id = plot_id, layer = NA, dataset = response_data, variable = "PARAMCD", value = response),
    if (with_assess) list(plot_id = plot_id, layer = "assessment_marker", dataset = response_data, variable = "PARAMCD", value = assessment),
    if (length(ongoing)) list(plot_id = plot_id, layer = "ongoing_arrow", dataset = NA, variable = names(ongoing)[1], value = unname(ongoing)[1])
  )
  layers <- pp_style_layers$swimmer[[style]]
  if (!with_events) layers <- setdiff(layers, "event_marker")
  if (!length(ongoing)) layers <- setdiff(layers, "ongoing_arrow")
  if (!is.null(visit_every)) layers <- c(layers, "visit_grid")
  if (!with_resp && !with_assess && !with_events) legend <- "none"
  pp_finish(pp_quick_spec(
    pid = plot_id, type = "swimmer", style = style, data = data, legend = legend, title = title,
    roles = roles, filters = filters, time_unit = time_unit, layers = layers,
    dots = c(list(id_var = key, visit_every = visit_every), list(...)), default_palette = "response_light",
    theme = .fs_get("theme", "swimmer") %or% "L_axis"
  ), adam, file)
}

# Spec-level arguments that may come through `...` of the quick functions.
pp_plot_args <- c("palette", "theme", "width", "height", "units", "dpi", "x_label", "y_label")

pp_quick_spec <- function(pid, type, style, data, legend, title, roles, filters, time_unit,
                          dots, default_palette, layers = NULL,
                          theme = .fs_get("theme", type) %or% "boxed") {
  if (!time_unit %in% names(pp_time_units)) {
    stop("`time_unit` must be one of: ", paste(names(pp_time_units), collapse = ", "), call. = FALSE)
  }
  lg <- pp_legend_cols(legend)
  plot_dots <- dots[intersect(names(dots), pp_plot_args)]
  dots <- dots[setdiff(names(dots), pp_plot_args)]
  dots$time_unit <- dots$time_unit %or% unname(pp_time_units[time_unit])
  plots <- data.frame(
    plot_id = pid, plot_type = type, dataset = data,
    layers = paste(layers %or% pp_style_layers[[type]][[style]], collapse = ", "),
    title = title %or% NA, x_label = plot_dots$x_label %or% NA, y_label = plot_dots$y_label %or% NA,
    theme = plot_dots$theme %or% theme, palette = plot_dots$palette %or% default_palette,
    legend_type = lg[1], legend_pos = lg[2],
    width = plot_dots$width %or% NA, height = plot_dots$height %or% NA,
    units = plot_dots$units %or% NA, dpi = plot_dots$dpi %or% NA,
    stringsAsFactors = FALSE
  )
  tfl_fig_spec(plots = plots, roles = roles, filters = filters, options = pp_options_df(pid, dots))
}
