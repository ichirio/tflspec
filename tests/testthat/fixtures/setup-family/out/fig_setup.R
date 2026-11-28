# ============================================================================
#  The figure style standard and the helpers of the figure programs
#  (theme_tfl, scale_colour_tfl, tfl_marker, tfl_save, tfl_check).
#  Generated  : <masked>
# ============================================================================

# the study's setup: the company's, the study's folders and id, your own
source("programs/study_setup.R")

# the packages the figures' code uses
library(ggplot2)
library(patchwork)
library(dplyr)

tfl_settings <- data.frame(
  type = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "km", "km", "km", "km", "km", "km", "km", "km", "km", "km", "km", "waterfall", "waterfall", "waterfall", "waterfall", "waterfall", "waterfall", "waterfall", "waterfall", "waterfall", "swimmer", "swimmer", "swimmer", "swimmer", "swimmer", "swimmer", "swimmer", "swimmer", "swimmer", "swimmer", "swimmer", "swimmer", "swimmer", "swimmer", "swimmer"),
  key = c("base_size", "axis_title_size", "axis_text_size", "legend_text_size", "legend_key_size", "legend_ncol", "legend_height", "text_size", "theme", "panel_border_width", "axis_line_width", "tick_width", "tick_length", "line_width", "zero_line_colour", "zero_line_width", "ref_line_colour", "ref_linetype", "ref_line_width", "month_days", "width", "height", "units", "dpi", "treatment_shapes", "treatment_linetypes", "censor_shape", "censor_size", "censor_stroke", "median_linetype", "median_colour", "median_line_width", "y_by", "risk_title", "risk_height", "width", "height", "bar_width", "ref_lines", "ref_label_size", "ref_label_hjust", "right_margin", "y_min", "y_max", "y_by", "x_label", "theme", "bar_width", "marker_size", "marker_palette", "event_size", "visit_linetype", "visit_colour", "visit_label", "bar_legend_prefix", "legend_ncol", "legend_height", "legend_point_size", "legend_label_size", "width", "height"),
  value = c("10", "10", "8", "9", "0.7", "3", "0.2", "3", "boxed", "0.5", "0.2", "0.4", "2", "0.3", "black", "0.5", "grey", "dashed", "0.5", "30.4375", "7.3", "4.2", "in", "300", NA, NA, "plus", "2.5", "0.6", "twodash", "grey50", "0.3", "0.2", "Patients at risk", "0.167", "8.33", "4.79", "0.8", "20,-30", "3.7", "-0.3", "50", "-100", "100", "20", "Patients", "L_axis", "1.2", "1.5", "response_assessment", "1.2", "dotted", "gray", "Month", "Best response", "3", "0.2", "2.2", "2.2", "7.71", "4.17"),
  stringsAsFactors = FALSE)

tfl_palettes <- list(
  `treatment` = c("#123456", "#D55E00", "#009E73", "#CC79A7", "#E69F00", "#56B4E9"),
  `response` = c("CR" = "#008000", "PR" = "#0000FF", "SD" = "#FFA500", "PD" = "#800080", "NE" = "#A0A0A0", "NON-CR/NON-PD" = "#20B2AA"),
  `response_assessment` = c("CR" = "#008000", "PR" = "#0000CD", "SD" = "#FF8C00", "PD" = "#9400D3", "NE" = "gray"),
  `response_light` = c("CR" = "#99CC99", "PR" = "#9999FF", "SD" = "#FFCC99", "PD" = "#CC99FF", "NE" = "#D9D9D9", "NON-CR/NON-PD" = "#99D8D3"),
  `okabe_ito` = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000"),
  `grey` = c("#000000", "#555555", "#999999", "#CCCCCC")
)

tfl_markers <- data.frame(
  marker = c("Death", "Subsequent anti-cancer therapy", "Discontinued", "assessment", "best_response", "censor"),
  shape = c("25", "21", "24", "23", "15", "4"),
  fill = c("black", "#FF00FF", "black", NA, NA, NA),
  colour = c("transparent", "transparent", "transparent", "transparent", NA, NA),
  size = c("1.6", "1.2", "1.2", "1.5", "2.2", "3"),
  stringsAsFactors = FALSE)

# km_curves() and forest_hr() read the style from this option: the same tables
options(tflspec.fig_style = list(
  settings = tfl_settings,
  colors = data.frame(
    palette = rep(names(tfl_palettes), lengths(tfl_palettes)),
    value = unlist(lapply(tfl_palettes, function(p)
      if (is.null(names(p))) rep(NA_character_, length(p)) else names(p)),
      use.names = FALSE),
    colour = unlist(tfl_palettes, use.names = FALSE),
    stringsAsFactors = FALSE),
  markers = tfl_markers))

# the days in a week, a month, a year (the study's setup may set them)
if (!exists("days_per_week")) days_per_week <- 7
if (!exists("days_per_month")) days_per_month <- 30.4375
if (!exists("days_per_year")) days_per_year <- 365.25

# ---- helpers ---------------------------------------------------------------
# (tflspec writes the style above from the figure style standard; this part
# is the same for every study)

# One setting: the figure type's own value, else the value for every type.
tfl_setting <- function(key, type = NA) {
  s <- tfl_settings
  v <- s$value[s$key %in% key & s$type %in% type]
  if (!length(v) || is.na(v[1])) v <- s$value[s$key %in% key & is.na(s$type)]
  if (!length(v)) return(NA_character_)
  v[1]
}
tfl_num <- function(key, type = NA) as.numeric(tfl_setting(key, type))

# The theme of the standard: "boxed" (panel border) or "L_axis" (left and
# bottom axis lines), with its font sizes, line widths and ticks.
theme_tfl <- function(type = NA, theme = tfl_setting("theme", type)) {
  n <- function(k) tfl_num(k, type)
  sizes <- ggplot2::theme(
    axis.title      = ggplot2::element_text(size = n("axis_title_size")),
    axis.text       = ggplot2::element_text(size = n("axis_text_size")),
    legend.text     = ggplot2::element_text(size = n("legend_text_size")),
    legend.key.size = ggplot2::unit(n("legend_key_size"), "lines"),
    panel.grid      = ggplot2::element_blank())
  base <- ggplot2::theme_minimal(base_size = n("base_size"))
  switch(theme,
    boxed = base + sizes + ggplot2::theme(
      panel.border = ggplot2::element_rect(colour = "black", fill = NA,
                                           linewidth = n("panel_border_width")),
      axis.ticks = ggplot2::element_line(linewidth = n("tick_width")),
      axis.ticks.length = ggplot2::unit(n("tick_length"), "mm")),
    L_axis = base + sizes + ggplot2::theme(
      axis.line = ggplot2::element_line(colour = "black",
                                        linewidth = n("axis_line_width")),
      axis.ticks = ggplot2::element_line(linewidth = n("axis_line_width"))),
    classic = ggplot2::theme_classic(base_size = n("base_size")) + sizes,
    base + sizes)
}

# The colours of a palette of the standard.  A named palette (response:
# CR, PR ...) colours the values it names; an ordered one (treatment) is
# given the values in order -- its rows' order in the standard is the
# groups' -- tfl_colours("treatment", levels(adsl$TRT01A)).  More values
# than colours: the colours repeat, with a warning.
tfl_colours <- function(palette, values = NULL) {
  pal <- tfl_palettes[[palette]]
  if (is.null(pal)) stop("No palette '", palette, "' in the figure style.")
  if (is.null(names(pal)) && !is.null(values)) {
    pal <- tfl_in_order(pal, values, sprintf("palette '%s'", palette), "colours")
  }
  pal
}

# An ordered list of the standard (a palette, the treatment shapes or line
# types) given the values in order; more values than it has: it repeats,
# with a warning
tfl_in_order <- function(x, values, what, things) {
  if (length(values) > length(x)) {
    warning(sprintf(
      "The figure style's %s has %d %s for %d groups: they repeat (%s). Add rows to the standard.",
      what, length(x), things, length(values),
      paste(values[-seq_along(x)], collapse = ", ")), call. = FALSE)
  }
  stats::setNames(rep_len(x, length(values)), values)
}

# The treatment groups' shapes and line types of the standard
# (treatment_shapes, treatment_linetypes: blank = not used, NULL), given
# the groups in order: tfl_shapes(levels(adsl$TRT01A)).
tfl_treatment_values <- function(key, values, things) {
  v <- tfl_setting(key)
  if (is.na(v) || !nzchar(trimws(v))) return(NULL)
  v <- trimws(strsplit(v, ",", fixed = TRUE)[[1]])
  if (key == "treatment_shapes") v <- as.numeric(v)
  tfl_in_order(v, values, key, things)
}
tfl_shapes <- function(values) tfl_treatment_values("treatment_shapes", values, "shapes")
tfl_linetypes <- function(values) tfl_treatment_values("treatment_linetypes", values, "line types")
scale_colour_tfl <- function(palette, values = NULL, ...) {
  pal <- tfl_colours(palette, values)
  if (is.null(names(pal))) ggplot2::scale_colour_manual(values = pal, ...)
  else ggplot2::scale_colour_manual(values = pal, breaks = names(pal), ...)
}
scale_fill_tfl <- function(palette, values = NULL, ...) {
  pal <- tfl_colours(palette, values)
  if (is.null(names(pal))) ggplot2::scale_fill_manual(values = pal, ...)
  else ggplot2::scale_fill_manual(values = pal, breaks = names(pal), ...)
}

# A marker of the standard (Death, Discontinued ..., censor, assessment).
tfl_marker <- function(marker) {
  m <- tfl_markers[tolower(tfl_markers$marker) %in% tolower(marker), ,
                   drop = FALSE]
  if (!nrow(m)) stop("No marker '", marker, "' in the figure style.")
  list(shape = as.numeric(m$shape[1]), fill = m$fill[1],
       colour = m$colour[1], size = as.numeric(m$size[1]))
}

# Save at the standard's size (a type's own size, else the common one).
tfl_save <- function(plot, file, type = NA,
                     width = tfl_num("width", type),
                     height = tfl_num("height", type),
                     dpi = tfl_num("dpi", type),
                     units = tfl_setting("units", type)) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(file, plot, width = width, height = height, dpi = dpi,
                  units = units)
  invisible(file)
}

# Checks a figure before it goes out:
#   - rows ggplot2 dropped (missing or out-of-scale values)
#   - data beyond the visible axes (a note: often deliberate, e.g. an axis
#     cut at 12 months)
#   - legend colours against the standard: two values drawn the same colour
#     (more groups than colours), and a value a named palette of the
#     standard colours (CR, PR ...) must be drawn in that colour
#   - `levels`: the legend shows these values, in this order
# A problem is a warning starting "Figure check:", so the run's log counts
# it; the result (problems, notes, a fingerprint of the drawn data) is
# returned invisibly.
tfl_check <- function(plot, levels = NULL, quiet = FALSE) {
  plots <- list(plot)
  if (inherits(plot, "patchwork")) {
    main <- plot
    main$patches <- NULL
    class(main) <- setdiff(class(main), "patchwork")
    plots <- c(plot$patches$plots, list(main))
  }
  problems <- character()
  notes <- character()
  drawn <- list()
  named <- tfl_palettes[vapply(tfl_palettes, function(p) !is.null(names(p)),
                               logical(1))]
  same_colour <- function(a, b) {
    ok <- !is.na(a) & !is.na(b)
    out <- rep(TRUE, length(a))
    out[ok] <- vapply(which(ok), function(i)
      isTRUE(all(grDevices::col2rgb(a[i], alpha = TRUE) ==
                   grDevices::col2rgb(b[i], alpha = TRUE))), logical(1))
    out
  }
  for (k in seq_along(plots)) {
    p <- plots[[k]]
    if (!inherits(p, "ggplot")) next
    msgs <- character()
    # built and drawn (ggplot2 drops rows with missing values when it draws),
    # on a null device so no Rplots.pdf is left behind
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off(), add = TRUE)
    b <- withCallingHandlers({
      bb <- ggplot2::ggplot_build(p)
      ggplot2::ggplot_gtable(bb)
      bb
    }, warning = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
    for (m in grep("Removed [0-9]+ rows?", msgs, value = TRUE)) {
      problems <- c(problems, paste0("panel ", k, ": ", sub("\\s+$", "", m)))
    }
    # beyond the visible axes
    pp <- b$layout$panel_params[[1]]
    for (i in seq_along(b$data)) {
      d <- b$data[[i]]
      for (ax in intersect(c("x", "xend", "y", "yend", "ymin", "ymax"),
                           names(d))) {
        r <- if (startsWith(ax, "x")) pp$x.range else pp$y.range
        v <- suppressWarnings(as.numeric(d[[ax]]))
        v <- v[is.finite(v)]
        if (is.null(r) || !length(v)) next
        eps <- diff(r) * 1e-6
        n_out <- sum(v < r[1] - eps | v > r[2] + eps)
        if (n_out) {
          notes <- c(notes, sprintf("panel %d, layer %d: %d %s value(s) beyond the visible axis",
                                    k, i, n_out, ax))
        }
      }
      keep <- vapply(d, function(v) is.atomic(v), logical(1))
      drawn[[length(drawn) + 1L]] <- d[keep]
    }
    # legend colours against the standard, and the legend's values
    for (aes in c("colour", "fill")) {
      sc <- b$plot$scales$get_scales(aes)
      if (is.null(sc) || !sc$is_discrete()) next
      br <- sc$get_breaks()
      br <- br[!is.na(br)]
      if (!length(br)) next
      got <- sc$map(br)
      # two values drawn the same colour: a palette shorter than the groups
      same <- !is.na(got) & (duplicated(got) | duplicated(got, fromLast = TRUE))
      if (any(same)) {
        problems <- c(problems, sprintf(
          "panel %d: the %s legend has %d values and %d colours (%s share one)",
          k, aes, length(br), length(unique(got)), paste(br[same], collapse = ", ")))
      }
      # a value some named palette colours must be drawn in one of the
      # colours the standard gives it (response, response_light ...)
      for (j in seq_along(br)) {
        std <- unlist(lapply(names(named), function(pn) {
          pal <- named[[pn]]
          if (br[j] %in% names(pal)) stats::setNames(pal[[br[j]]], pn)
        }))
        if (!length(std)) next
        if (!any(same_colour(rep(got[j], length(std)), unname(std)))) {
          problems <- c(problems, sprintf(
            "panel %d: %s '%s' is drawn %s; the standard has %s",
            k, aes, br[j], got[j],
            paste(sprintf("%s (%s)", std, names(std)), collapse = ", ")))
        }
      }
      if (!is.null(levels) && any(br %in% levels) &&
          !identical(as.character(br), as.character(levels))) {
        problems <- c(problems, sprintf(
          "panel %d: the %s legend shows %s; expected %s", k, aes,
          paste(br, collapse = ", "), paste(levels, collapse = ", ")))
      }
    }
  }
  f <- tempfile()
  saveRDS(drawn, f, version = 2)
  fingerprint <- unname(tools::md5sum(f))
  unlink(f)
  for (x in problems) warning("Figure check: ", x, call. = FALSE)
  if (!quiet) {
    for (x in notes) message("Figure check (note): ", x)
    message(sprintf("Figure check: %d problem(s), %d note(s); drawn data %s",
                    length(problems), length(notes), fingerprint))
  }
  invisible(list(problems = problems, notes = notes,
                 fingerprint = fingerprint))
}

# The number at risk of a KM figure, from the KM table's ARD
# (cardx::ard_survival_survfit(times = ) -- e.g. its rows of the study ARD),
# so the figure and the table agree.  With `fit`, the curve's own count at
# the same times is checked against it (a warning when they differ).
# The ARD's times are in the analysis data's unit; `divisor` turns them into
# the axis's (1 when they already are, e.g. when the table's data was
# converted by tte_time()).  A data frame: time, strata, n_risk.
tfl_km_risk <- function(ard, fit = NULL, divisor = 1) {
  a <- ard[ard$variable == "time" & ard$stat_name == "n.risk", , drop = FALSE]
  strata <- if ("group1_level" %in% names(a)) {
    vapply(a$group1_level, function(v) if (length(v)) as.character(v[[1]]) else NA_character_,
           character(1))
  } else rep("All", nrow(a))
  risk <- data.frame(time = as.numeric(unlist(a$variable_level)) / divisor,
                     strata = strata, n_risk = as.numeric(unlist(a$stat)),
                     stringsAsFactors = FALSE)
  if (!is.null(fit)) {
    sr <- summary(fit, times = sort(unique(risk$time)), extend = TRUE)
    curve <- data.frame(time = sr$time, n = sr$n.risk,
                        strata = if (is.null(sr$strata)) "All" else
                          sub("^[^=]*=", "", as.character(sr$strata)),
                        stringsAsFactors = FALSE)
    chk <- merge(risk, curve, by = c("strata", "time"))
    n_bad <- sum(chk$n_risk != chk$n) + nrow(risk) - nrow(chk)
    if (n_bad) {
      warning("Figure check: the number at risk of the ARD differs from the curve's (",
              n_bad, " cell(s))", call. = FALSE)
    }
  }
  risk
}


# ---- the content contract of a report's own code: a table's code gives a
# table_plan; a figure's a ggplot, an rtfplot, a data frame or rtftable pages
# (or a list of them); anything else stops (class tfl_content_contract)
report_content_check <- function(x, kind, id) {
  fig <- identical(kind, "figure")
  ok <- function(p) if (fig) is.data.frame(p) || inherits(p, c("rtftable", "rtfplot", "ggplot")) else
    inherits(p, "table_plan")
  items <- if (fig && is.list(x) && !ok(x)) x else list(x)
  bad <- if (length(items)) which(!vapply(items, ok, NA))[1L] else 0L
  if (!is.na(bad)) {
    got <- if (identical(bad, 0L)) "an empty list" else if (!identical(items, list(x)))
      sprintf("a list whose item %d is a %s", bad, class(items[[bad]])[1L]) else
      sprintf("a %s", class(x)[1L])
    want <- if (fig) "a ggplot (or rtfplot figures, a data frame or rtftable pages, or a list of them)" else
      "a table_plan (rtfreporter::table_plan())"
    stop(structure(class = c("tfl_content_contract", "error", "condition"), list(
      message = sprintf("%s: a %s's content code must return %s; it returned %s.", id, kind, want, got),
      call = NULL)))
  }
  x
}

# ---- a report of your own code: its `content` as rtfreporter takes it
# (a ggplot becomes a figure; what is not a data frame, rtftable pages, a
# ggplot or an rtfplot, or a list of them, stops, saying which item)
report_content <- function(x) {
  ok <- function(p) is.data.frame(p) ||
    inherits(p, c("rtftable", "rtfplot", "ggplot"))
  fig <- function(p) if (inherits(p, "ggplot")) rtfplot(p) else p
  one <- ok(x)
  items <- if (one) list(x) else if (is.list(x)) x else list(x)
  for (i in seq_along(items)) if (!ok(items[[i]])) {
    stop(sprintf("`content`%s is %s: make it a data frame, rtftable pages, a ggplot or an rtfplot (or a list of them).",
                 if (one || !is.list(x)) "" else sprintf(" (item %d)", i),
                 class(items[[i]])[1L]), call. = FALSE)
  }
  if (one) fig(x) else lapply(items, fig)
}

# ---- a figure at its size on the page (the report's, step 3): drawn so,
# its text the size its theme says (a ggplot; anything else as it is)
fig_sized <- function(x, width_in, height_in, dpi = 300) {
  one <- function(p) if (inherits(p, "ggplot")) rtfplot(p,
    width_twips = round(width_in * 1440), height_twips = round(height_in * 1440),
    render_width = width_in, render_height = height_in, render_dpi = dpi) else p
  if (inherits(x, "ggplot") || !is.list(x) || is.data.frame(x)) one(x) else lapply(x, one)
}

# ---- a data step of your own code: it left a data frame
report_data <- function(x, what = "data") {
  if (!is.data.frame(x)) {
    stop(sprintf("The data step's code leaves `%s` as %s: make it a data frame.",
                 what, class(x)[1L]), call. = FALSE)
  }
  x
}
