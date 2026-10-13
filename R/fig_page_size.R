#' A figure's size on its report's page
#'
#' How big a figure report's figure is drawn and placed: the report sheet's
#' `figure_width_in` / `figure_height_in`, each a number of inches (that
#' size), `fit` (the page's body) or blank (the figure's own size, as
#' rtfreporter places an image: `native`).  The body is the page (the page
#' sheet's `width_in` / `height_in`, else its `paper_size` in its
#' `orientation`) less its margins; its height less, too, the report's
#' title and footnote lines (and a line between each and the figure), at
#' the page's font size.  A report program draws the figure at this size
#' (its text the size its theme says) and the report places it so; the
#' dpi only sets how many pixels it has.
#'
#' @param spec A report definition ([tfl_read_report_spec()]), or the
#'   path(s) to read it from.
#' @param output_id The report, when `spec` defines several.
#' @param native The figure's own size in inches, `c(width, height)`: what
#'   a blank gives (a study's figure style says it by the figure's kind;
#'   rtfreporter draws a plot at 6.5 x 4.5).
#' @return A list: `width_in`, `height_in` (inches, two decimals) and
#'   `mode` (`c(width = , height = )`: `"native"`, `"fit"` or `"fixed"`).
#' @examples
#' sp <- tfl_table_spec(
#'   report = data.frame(output_id = "F-1", type = "figure",
#'                       figure_width_in = "fit"),
#'   page = data.frame(output_id = NA, paper_size = "letter",
#'                     orientation = "landscape"),
#'   titles = data.frame(output_id = "F-1", line = c("1", "2"),
#'                       center = c("Figure 14.2.1", "Kaplan-Meier")))
#' tfl_fig_page_size(sp, "F-1")
#' @export
tfl_fig_page_size <- function(spec, output_id = NULL, native = c(6.5, 4.5)) {
  sp <- .ard_spec_scope(.as_spec(spec, "table", "tfl_fig_page_size",
                                 output_id = output_id), output_id)
  r <- .ard_spec_report_row(sp)
  pg <- if (nrow(sp$page)) .ard_spec_typed(sp$page[1L, ], "page") else list()
  lines <- length(.ard_spec_band(sp, "titles")) + length(.ard_spec_band(sp, "footnotes"))
  .fig_page_size(r, pg, lines, native)
}

# the paper sizes rtfreporter knows, portrait (inches)
.fig_papers <- list(letter = c(8.5, 11), legal = c(8.5, 14), a4 = c(8.27, 11.69),
                    a3 = c(11.69, 16.54), a5 = c(5.83, 8.27))

# A figure's size cell: a number of inches, "fit", or blank (NA)
.fig_size_cell <- function(v) {
  v <- trimws(as.character(v %||% NA)[1L])
  if (is.na(v) || !nzchar(v)) return(NA)
  if (identical(tolower(v), "fit")) return("fit")
  n <- suppressWarnings(as.numeric(v))
  if (is.na(n) || n <= 0) stop("not a size", call. = FALSE)
  n
}

# The size from a report row (`figure_width_in`, `figure_height_in`), a page
# row, the number of title and footnote lines and the figure's own size
.fig_page_size <- function(r, pg, lines = 0L, native = c(6.5, 4.5)) {
  num <- function(v, d) {
    v <- suppressWarnings(as.numeric(v %||% NA))
    if (length(v) != 1L || is.na(v)) d else v
  }
  paper <- .fig_papers[[tolower(pg$paper_size %||% "letter")]] %||% .fig_papers$letter
  landscape <- !identical(tolower(pg$orientation %||% "landscape"), "portrait")
  if (landscape) paper <- rev(paper)
  pw <- num(pg$width_in, paper[1L])
  ph <- num(pg$height_in, paper[2L])
  m <- function(side) num(pg[[paste0("margin_", side, "_in")]], 0.75)
  # a line: the page's font size (half-points; rtfreporter's 9 pt) and a
  # little leading; a line between the titles and the figure, and the
  # figure and the footnotes
  hp <- num(pg$font_size_half_points, getOption("rtfreporter.font_size_half_points", 18))
  line_in <- hp / 2 * 1.2 / 72
  body <- c(pw - m("left") - m("right"),
            max(ph - m("top") - m("bottom") - (lines + 2L) * line_in, 1))
  cell <- list(tryCatch(.fig_size_cell(r$figure_width_in), error = function(e) NA),
               tryCatch(.fig_size_cell(r$figure_height_in), error = function(e) NA))
  mode <- vapply(cell, function(v) if (identical(v, "fit")) "fit" else
    if (is.na(v)) "native" else "fixed", "")
  size <- vapply(1:2, function(k) switch(mode[k], fit = body[k], native = native[k],
                                         as.numeric(cell[[k]])), 1)
  list(width_in = round(size[1L], 2L), height_in = round(size[2L], 2L),
       mode = c(width = mode[1L], height = mode[2L]))
}
