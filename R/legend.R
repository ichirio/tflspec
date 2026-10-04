# Legends
#
# legend_type:
#   mapped - ggplot2 legends of the mapped aesthetics (colour / fill / shape)
#   manual - a legend panel drawn from a table of items (label, glyph, shape,
#            colour, fill, linetype), independent of the data. Items come from
#            the `legend` sheet; when that is empty they are built from the spec.
#   none   - no legend
# legend_pos: right / bottom / top / left / below / inside_tr / inside_tl /
#             inside_br / inside_bl

pp_legend_item_cols <- c("label", "glyph", "shape", "colour", "fill", "linetype")

# An auto legend part known at generation time.
legend_part <- function(label, glyph, shape = NA, colour = NA, fill = NA, linetype = NA) {
  n <- length(label)
  rep_n <- function(x) if (length(x) == 1) rep(x, n) else x
  data.frame(label = label, glyph = rep_n(glyph), shape = rep_n(shape), colour = rep_n(colour),
             fill = rep_n(fill), linetype = rep_n(linetype), stringsAsFactors = FALSE)
}

# Items from the `legend` sheet (NULL when none for this plot).
pp_sheet_legend_items <- function(ctx) {
  lg <- ctx$spec$legend
  lg <- lg[lg$plot_id %in% ctx$pid, , drop = FALSE]
  if (!nrow(lg)) return(NULL)
  ord <- suppressWarnings(as.numeric(lg$order))
  lg <- lg[order(is.na(ord), ord, seq_len(nrow(lg))), , drop = FALSE]
  bad <- setdiff(lg$glyph[!is.na(lg$glyph)], pp_glyphs)
  if (length(bad)) stop("Plot ", ctx$pid, ": unknown glyph(s): ", paste(bad, collapse = ", "), call. = FALSE)
  glyph <- ifelse(is.na(lg$glyph), "point", lg$glyph)
  shape <- vapply(seq_len(nrow(lg)), function(i) {
    if (glyph[i] != "point") return(NA_real_)
    pp_shape_code(lg$shape[i], "circle")
  }, numeric(1))
  # a filled point without explicit fill takes its colour, and vice versa
  colour <- lg$colour
  fill <- lg$fill
  pt21 <- glyph == "point" & shape %in% 21:25
  fill[pt21 & is.na(fill)] <- colour[pt21 & is.na(fill)]
  rect_nofill <- glyph == "rect" & is.na(fill)
  fill[rect_nofill] <- colour[rect_nofill]
  colour[glyph %in% c("point", "line") & is.na(colour)] <- "black"
  legend_part(lg$label, glyph, shape, colour, fill, ifelse(glyph == "line" & is.na(lg$linetype), "solid", lg$linetype))
}

tribble_code <- function(df) {
  cell <- function(x, col) {
    if (is.na(x)) return("NA")
    if (col == "shape") return(format(as.numeric(x)))
    q(x)
  }
  rows <- vapply(seq_len(nrow(df)), function(i) {
    paste(vapply(pp_legend_item_cols, function(cc) cell(df[[cc]][i], cc), character(1)), collapse = ", ")
  }, character(1))
  paste0("tibble::tribble(\n  ", paste(paste0("~", pp_legend_item_cols), collapse = ", "), ",\n  ",
         paste(rows, collapse = ",\n  "), "\n)")
}

legend_panel_fun <- '# Draw a legend panel from a table of items (independent of the plot data).
#   glyph: "point" (shape / colour / fill), "line" (colour / linetype), "rect" (fill)
legend_panel <- function(items, ncol = 3, width_chars = 100, text_size = 2.6,
                         key_size = 2.2, box = FALSE, n_rows = NA) {
  items <- as.data.frame(items)
  n <- nrow(items)
  items$col <- (seq_len(n) - 1) %% ncol
  items$row <- -((seq_len(n) - 1) %/% ncol)
  # x in "characters": each column is as wide as its longest label
  w <- as.numeric(tapply(nchar(items$label), items$col, max)) + 5
  items$x <- c(0, cumsum(w))[items$col + 1]
  p <- ggplot(items, aes(x = x, y = row))
  rc <- items[items$glyph == "rect", ]
  ln <- items[items$glyph == "line", ]
  pt <- items[items$glyph == "point", ]
  pt$colour[is.na(pt$colour)] <- "transparent"
  pt$fill[is.na(pt$fill)] <- "transparent"
  if (nrow(rc)) p <- p + geom_tile(data = rc, aes(fill = fill), width = 1.6, height = 0.55)
  if (nrow(ln)) p <- p + geom_segment(data = ln, aes(x = x - 1, xend = x + 1, yend = row,
                                                     colour = colour, linetype = linetype), linewidth = 0.6)
  if (nrow(pt)) p <- p + geom_point(data = pt, aes(shape = shape, colour = colour, fill = fill), size = key_size)
  p <- p +
    geom_text(aes(x = x + 1.8, label = label), hjust = 0, size = text_size) +
    scale_shape_identity() + scale_colour_identity() +
    scale_fill_identity() + scale_linetype_identity() +
    coord_cartesian(xlim = c(-1.5, max(sum(w), width_chars)),
                    ylim = c(min(min(items$row) - 0.6, 0.4 - n_rows, na.rm = TRUE), 0.6),
                    expand = FALSE) +
    theme_void()
  if (box) p <- p + theme(plot.background = element_rect(colour = "black", fill = "white", linewidth = 0.3))
  p
}'

pp_inside_just <- list(
  inside_tr = c(1, 1), inside_tl = c(0, 1), inside_br = c(1, 0), inside_bl = c(0, 0)
)

# Returns list(theme = ggplot terms added to the main plot,
#              pre = code lines before assembly, panel = list(name, pos, size)
#              or NULL, inset = code or NULL).
pp_legend_code <- function(ctx, auto_parts, fig_width_in) {
  type <- ctx$prow$legend_type %or% "mapped"
  pos <- ctx$prow$legend_pos %or% (if (type == "manual") "below" else "right")
  if (!type %in% pp_legend_types) stop("Unknown legend_type: ", type, call. = FALSE)
  if (!pos %in% pp_legend_positions) stop("Unknown legend_pos: ", pos, call. = FALSE)
  title_blank <- is.na(pp_opt(ctx, "legend_title"))

  if (type == "none") {
    return(list(theme = 'theme(legend.position = "none")'))
  }

  if (type == "mapped") {
    terms <- if (pos %in% names(pp_inside_just)) {
      j <- vec_code(pp_inside_just[[pos]])
      sprintf(paste0("theme(\n",
                     "  legend.position        = \"inside\",\n",
                     "  legend.position.inside = %s,\n",
                     "  legend.justification   = %s,\n",
                     "  legend.background      = element_rect(colour = \"black\", fill = \"white\", linewidth = 0.3)\n)"),
              vec_code(abs(pp_inside_just[[pos]] - 0.02)), j)
    } else {
      side <- if (pos == "below") "bottom" else pos
      if (side %in% c("bottom", "top")) {
        sprintf('theme(legend.position = "%s", legend.box = "vertical", legend.box.just = "left")', side)
      } else sprintf('theme(legend.position = "%s")', side)
    }
    if (title_blank) terms <- c(terms, "theme(legend.title = element_blank())")
    else terms <- c(terms, sprintf("labs(%s)", paste(sprintf("%s = %s", c("colour", "fill", "shape"), q(pp_opt(ctx, "legend_title"))), collapse = ", ")))
    hide <- split_list(pp_opt(ctx, "legend_hide"))
    if (length(hide)) terms <- c(terms, sprintf("guides(%s)", paste(sprintf('%s = "none"', hide), collapse = ", ")))
    return(list(theme = terms))
  }

  # manual
  items <- pp_sheet_legend_items(ctx)
  source <- "legend sheet"
  if (is.null(items)) {
    source <- "built from the spec"
    lit <- Filter(is.data.frame, auto_parts)
    if (length(lit) == length(auto_parts)) {
      items <- if (length(lit)) do.call(rbind, lit) else NULL
    }
  }
  if (!is.null(items)) {
    items_code <- sprintf("legend_items <- %s", tribble_code(items))
    n_items <- nrow(items)
  } else {
    parts <- vapply(auto_parts, function(p) if (is.data.frame(p)) tribble_code(p) else p, character(1))
    items_code <- paste0("legend_items <- dplyr::bind_rows(\n",
                         paste(indent(parts), collapse = "\n") |> gsub(pattern = "\n  (tibble::)", replacement = ",\n  \\1"), "\n)")
    n_items <- NA
  }
  ncol <- as.integer(pp_opt(ctx, "legend_ncol"))
  inside <- pos %in% names(pp_inside_just)
  # inside the plot or beside it, the items stack in one column
  if ((inside || pos %in% c("right", "left")) && !is.na(n_items)) ncol <- min(ncol, 1L)
  # legend text is ~17 characters per inch; the panel is ~85% of the figure width
  frac <- if (pos %in% c("right", "left")) pp_opt_num(ctx, "legend_width") else 1
  width_chars <- round(fig_width_in * frac * 17)
  inside_w <- pp_opt_num(ctx, "legend_width")
  if (inside && !is.na(n_items) && is.na(pp_opt_explicit(ctx, "legend_width"))) {
    width_chars <- max(nchar(items$label)) + 6
    inside_w <- min(0.6, max(0.08, width_chars / (0.85 * fig_width_in * 17)))
  } else if (inside) {
    width_chars <- round(0.85 * fig_width_in * inside_w * 17)
  }
  sizes <- c(text_size = pp_opt(ctx, "legend_label_size"),
             key_size = pp_opt(ctx, "legend_point_size"))
  sizes <- sizes[!is.na(sizes) & nzchar(sizes)]
  extra <- if (length(sizes)) paste0(", ", names(sizes), " = ", sizes, collapse = "") else ""
  # beside the plot the panel is as tall as the plot: rows of a fixed height
  # (n_rows to the panel), the items at the top, not stretched over it
  side <- pos %in% c("right", "left")
  call <- sprintf("p_legend <- legend_panel(legend_items, ncol = %d, width_chars = %d%s%s%s)",
                  ncol, width_chars, extra, if (inside) ", box = TRUE" else "",
                  if (side) ", n_rows = 20" else "")
  pre <- c(legend_panel_fun,
           sprintf("# ---- legend: manual (%s) - edit the items freely ----", source),
           items_code, call)
  out <- list(theme = 'theme(legend.position = "none")', pre = pre)
  if (inside) {
    w <- inside_w
    h <- if (!is.na(n_items)) min(0.9, ceiling(n_items / ncol) * 0.075 + 0.03) else pp_opt_num(ctx, "legend_height")
    j <- pp_inside_just[[pos]]
    left <- if (j[1] == 1) 0.99 - w else 0.01
    bottom <- if (j[2] == 1) 0.99 - h else 0.01
    out$inset <- sprintf(
      "p <- p + inset_element(p_legend, left = %s, bottom = %s, right = %s, top = %s, align_to = \"panel\")",
      format(round(left, 3)), format(round(bottom, 3)), format(round(left + w, 3)), format(round(bottom + h, 3)))
  } else {
    size <- if (pos %in% c("right", "left")) pp_opt_num(ctx, "legend_width") else pp_opt_num(ctx, "legend_height")
    out$panel <- list(name = "p_legend", pos = pos, size = size)
  }
  out
}
