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
# `divisor` turns the ARD's time unit into the axis's (30.4375: days to
# months).  A data frame: time, strata, n_risk.
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
