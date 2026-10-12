#' A figure's size on its report's page
#'
#' Where a figure report's figure goes and how big it is drawn: the report
#' sheet's `figure_width_in` / `figure_height_in`, or -- blank -- the page's
#' body.  The body is the page (the page sheet's `width_in` / `height_in`,
#' else its `paper_size` in its `orientation`) less its margins; its height
#' less, too, the report's title and footnote lines (and a line between
#' each and the figure), at the page's font size.  One of the two blank
#' keeps the other given.  A report program draws the figure at this size
#' (so its text is the size its theme says) and the report places it so.
#'
#' @param spec A report definition ([tfl_read_report_spec()]), or the
#'   path(s) to read it from.
#' @param output_id The report, when `spec` defines several.
#' @return A list: `width_in`, `height_in` (inches, two decimals) and `fit`
#'   (`c(width = , height = )`: `TRUE` where the size is the body's).
#' @examples
#' sp <- tfl_table_spec(
#'   report = data.frame(output_id = "F-1", type = "figure"),
#'   page = data.frame(output_id = NA, paper_size = "letter",
#'                     orientation = "landscape"),
#'   titles = data.frame(output_id = "F-1", line = c("1", "2"),
#'                       center = c("Figure 14.2.1", "Kaplan-Meier")))
#' tfl_fig_page_size(sp, "F-1")
#' @export
tfl_fig_page_size <- function(spec, output_id = NULL) {
  sp <- .ard_spec_scope(.as_spec(spec, "table", "tfl_fig_page_size",
                                 output_id = output_id), output_id)
  r <- .ard_spec_report_row(sp)
  pg <- if (nrow(sp$page)) .ard_spec_typed(sp$page[1L, ], "page") else list()
  lines <- length(.ard_spec_band(sp, "titles")) + length(.ard_spec_band(sp, "footnotes"))
  .fig_page_size(r, pg, lines)
}

# the paper sizes rtfreporter knows, portrait (inches)
.fig_papers <- list(letter = c(8.5, 11), legal = c(8.5, 14), a4 = c(8.27, 11.69),
                    a3 = c(11.69, 16.54), a5 = c(5.83, 8.27))

# The size from a report row (`figure_width_in`, `figure_height_in`), a page
# row and the number of title and footnote lines
.fig_page_size <- function(r, pg, lines = 0L) {
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
  body_w <- pw - m("left") - m("right")
  body_h <- ph - m("top") - m("bottom") - (lines + 2L) * line_in
  w <- num(r$figure_width_in, NA)
  h <- num(r$figure_height_in, NA)
  fit <- c(width = is.na(w), height = is.na(h))
  list(width_in = round(if (is.na(w)) body_w else w, 2L),
       height_in = round(if (is.na(h)) max(body_h, 1) else h, 2L),
       fit = fit)
}
