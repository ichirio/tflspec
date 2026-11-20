# ---- the flat legend: one grid of every item, shared by the figure template functions ----
#
# A figure template function (fig_km(); later fig_waterfall(), fig_swimmer())
# may show, instead of ggplot2's legends -- one a scale, linked to the data
# -- one flat legend: the items of several layers and aesthetics (the curves
# by group, the censor mark "Censored"; the bars by best response and the
# marks by the response at each assessment), in a grid of n columns filled
# row by row, each column as wide as its longest text, with an optional
# border, drawn as a plot of its own (identity scales, theme_void()) and
# joined to the figure with patchwork below or beside it.
#
#   .fig_legend_items(plot, keys = NULL)       the items, from the plot's scales
#   .fig_legend_relabel(items, labels, who)    texts by aesthetic, then value
#   .fig_legend_plot(items, n_cols, n_rows, border, text_size)
#   .fig_legend_attach(fig, legend, where, rel_size)
#
# The items are one table, a row an item:
#   aes        the aesthetic it comes from ("colour", "shape", "fill" ...);
#              with `value`, the key a relabel finds it by
#   value      its value in that aesthetic ("Drug X", "Censored", "CR")
#   label      its text
#   key        the key drawn: "line", "point" or "rect"
#   colour, fill, linetype, linewidth, shape, size   the key's look (NA:
#              the default)
# Equal values of two aesthetics are two items, each relabelled on its own
# (colour "CR" and fill "CR" can read "CR" and "Best overall CR"); scales
# with the same values and texts (a curve's colour and line type) are one
# item, as ggplot2 merges their legends.

.fig_legend_cols <- c("aes", "value", "label", "key", "colour", "fill", "linetype", "linewidth",
                      "shape", "size")

# An items table from vectors, the columns left out NA
.fig_legend_table <- function(...) {
  d <- data.frame(..., stringsAsFactors = FALSE)
  for (k in setdiff(.fig_legend_cols, names(d))) d[[k]] <- NA
  if (!all(d$key %in% c("line", "point", "rect"))) {
    stop("A legend item's key is line, point or rect.", call. = FALSE)
  }
  d[.fig_legend_cols]
}

# The items of a plot's legends: each discrete scale of colour, fill,
# linetype, shape that has a legend, its values in order with their texts
# and looks.  A scale whose values and texts are another's (before it) adds
# its look to that one's items.  The key: `keys` by aesthetic, else a line
# for a colour or line type drawn by a line layer, a rect for a fill, a
# point otherwise.  The layers' fixed looks (a line's width, a point's size
# and stroke) are the first such layer's.
.fig_legend_items <- function(plot, keys = NULL) {
  b <- ggplot2::ggplot_build(plot)
  geoms <- vapply(plot$layers, function(l) class(l$geom)[1L], "")
  is_line <- geoms %in% c("GeomStep", "GeomLine", "GeomPath")
  fixed <- function(geom_ok, k) {
    for (i in which(geom_ok)) {
      v <- plot$layers[[i]]$aes_params[[k]]
      if (!is.null(v)) return(v)
    }
    NA
  }
  out <- NULL
  for (a in c("colour", "fill", "linetype", "shape")) {
    sc <- b$plot$scales$get_scales(a)
    if (is.null(sc) || !sc$is_discrete() || identical(sc$guide, "none")) next
    br <- sc$get_breaks()
    br <- br[!is.na(br)]
    if (!length(br)) next
    lab <- as.character(sc$get_labels(br))
    look <- sc$map(br)
    if (!is.null(out)) {
      same <- out$aes == out$aes[match(TRUE, out$value %in% br)] & out$value %in% br
      if (sum(same) == length(br) && identical(out$label[same], lab)) {
        out[same, a] <- look
        next
      }
    }
    key <- (keys %||% list())[[a]] %||%
      if (a %in% c("colour", "linetype") && any(is_line)) "line" else if (a == "fill") "rect" else "point"
    d <- .fig_legend_table(aes = a, value = as.character(br), label = lab, key = key)
    d[[a]] <- look
    out <- rbind(out, d)
  }
  if (is.null(out)) return(.fig_legend_table(aes = character(), value = character(), label = character(),
                                             key = character()))
  ln <- out$key == "line"
  out$linewidth[ln & is.na(out$linewidth)] <- fixed(is_line, "linewidth")
  pt <- out$key == "point"
  out$size[pt & is.na(out$size)] <- fixed(geoms == "GeomPoint", "size")
  out
}

# The items' texts relabelled: `labels` a map by aesthetic, then by value
# (list(colour = list("Drug X" = "Drug X 10 mg"), shape = list(Censored =
# "Censored subject"))); an item it does not name keeps its text.  `who`
# names the argument in the errors.
.fig_legend_relabel <- function(items, labels, who = "item_labels") {
  if (is.null(labels)) return(items$label)
  ok_map <- function(m) is.null(m) || (is.list(m) || is.character(m)) && length(m) && !is.null(names(m))
  if (!is.list(labels) || is.null(names(labels)) || !all(vapply(labels, ok_map, NA))) {
    stop(sprintf("%s is a map by aesthetic, each a map by value.", who), call. = FALSE)
  }
  bad <- setdiff(names(labels), unique(items$aes))
  if (length(bad)) {
    stop(sprintf("%s has no aesthetic %s; the legend's are %s.", who, paste(bad, collapse = ", "),
                 paste(unique(items$aes), collapse = ", ")), call. = FALSE)
  }
  vapply(seq_len(nrow(items)), function(i) {
    v <- labels[[items$aes[i]]][[items$value[i]]]
    if (is.null(v)) items$label[i] else as.character(v)
  }, "")
}

# The grid's columns and rows for k items: n_cols given, else from n_rows,
# else one row
.fig_legend_grid <- function(k, n_cols = NULL, n_rows = NULL) {
  nc <- n_cols %||% (if (is.null(n_rows)) k else ceiling(k / n_rows))
  nc <- max(1L, min(as.integer(nc), k))
  c(cols = nc, rows = ceiling(k / nc))
}

# The legend as a plot of its own: a key and its text an item, row by row
# in the grid; x in characters, each column as wide as its longest text
# (the key 2, a gap of 1 before the text and 3 after the column); the looks
# by identity scales; a box around it when `border`; `text_size` in mm
.fig_legend_plot <- function(items, n_cols = NULL, n_rows = NULL, border = FALSE, text_size = 3) {
  g <- .fig_legend_grid(nrow(items), n_cols, n_rows)
  i <- seq_len(nrow(items)) - 1L
  items$col <- i %% g[["cols"]]
  items$y <- -(i %/% g[["cols"]])
  w <- vapply(seq_len(g[["cols"]]) - 1L, function(j) max(nchar(items$label[items$col == j])), 0)
  start <- c(0, cumsum(2 + 1 + w + 3))[seq_len(g[["cols"]])]
  items$x <- start[items$col + 1L]
  na_to <- function(v, d) ifelse(is.na(v), d, v)
  items$colour <- na_to(items$colour, "black")
  items$fill <- na_to(items$fill, "grey50")
  items$linetype <- na_to(items$linetype, "solid")
  items$linewidth <- na_to(as.numeric(items$linewidth), 0.5)
  items$shape <- na_to(items$shape, 19)
  items$size <- na_to(as.numeric(items$size), 2)
  p <- ggplot2::ggplot(items, ggplot2::aes(y = .data$y))
  ln <- items[items$key == "line", , drop = FALSE]
  if (nrow(ln)) {
    p <- p + ggplot2::geom_segment(data = ln, ggplot2::aes(x = .data$x, xend = .data$x + 2, yend = .data$y,
                                                           colour = .data$colour, linetype = .data$linetype,
                                                           linewidth = .data$linewidth))
  }
  rc <- items[items$key == "rect", , drop = FALSE]
  if (nrow(rc)) {
    p <- p + ggplot2::geom_rect(data = rc, ggplot2::aes(xmin = .data$x + 0.3, xmax = .data$x + 1.7,
                                                        ymin = .data$y - 0.3, ymax = .data$y + 0.3,
                                                        fill = .data$fill), colour = NA)
  }
  pt <- items[items$key == "point", , drop = FALSE]
  if (nrow(pt)) {
    p <- p + ggplot2::geom_point(data = pt, ggplot2::aes(x = .data$x + 1, colour = .data$colour,
                                                         fill = .data$fill, shape = .data$shape,
                                                         size = .data$size))
  }
  p <- p + ggplot2::geom_text(ggplot2::aes(x = .data$x + 3, label = .data$label), hjust = 0, size = text_size) +
    ggplot2::scale_colour_identity() + ggplot2::scale_fill_identity() +
    ggplot2::scale_linetype_identity() + ggplot2::scale_shape_identity() +
    ggplot2::scale_size_identity() + ggplot2::scale_linewidth_identity() +
    ggplot2::scale_x_continuous(limits = c(-0.5, sum(2 + 1 + w + 3)), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(-g[["rows"]] + 0.5, 0.5), expand = c(0, 0)) +
    ggplot2::theme_void()
  if (isTRUE(border)) {
    p <- p + ggplot2::theme(plot.background = ggplot2::element_rect(colour = "black", fill = NA))
  }
  p
}

# The figure with the legend under it ("bottom") or beside it ("right"): a
# patchwork; `rel_size` the legend's height (bottom) or width (right) for
# the figure's 1, NULL: 0.07 a row below, 0.3 beside
.fig_legend_attach <- function(fig, legend, where = c("bottom", "right"), rel_size = NULL) {
  where <- match.arg(where)
  if (where == "right") {
    return(patchwork::wrap_plots(fig, legend, nrow = 1L, widths = c(1, rel_size %||% 0.3)))
  }
  rows <- length(unique(legend$data$y))
  patchwork::wrap_plots(fig, legend, ncol = 1L, heights = c(1, rel_size %||% (0.07 * rows)))
}
