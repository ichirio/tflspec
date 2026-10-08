# The helpers a figure program sources: the style standard as data, and
# theme_tfl() / scale_colour_tfl() / tfl_marker() / tfl_save() / tfl_check()
# as plain ggplot2 code.  Programs written by hand -- or by an AI -- get the
# same look and the same checks as the generated ones, and depend on
# ggplot2 only, not on tflspec.

#' The helper script of a study's figure programs
#'
#' `tfl_fig_setup_code()` writes the figure style standard ([tfl_fig_style()]) as
#' data -- `tfl_settings`, `tfl_palettes`, `tfl_markers` -- followed by
#' helpers in plain ggplot2:
#'
#' * `theme_tfl(type)` -- the standard's theme, font sizes, lines and ticks;
#' * `scale_colour_tfl(palette)` / `scale_fill_tfl()` / `tfl_colours()` --
#'   the standard's palettes;
#' * `tfl_marker(name)` -- an event or symbol marker (`Death`, `censor` ...);
#' * `tfl_save(plot, file, type)` -- `ggsave()` at the standard's size;
#' * `tfl_km_risk(ard, fit)` -- a KM figure's number at risk from the KM
#'   table's ARD (`cardx::ard_survival_survfit(times = )`), checked against
#'   the curve;
#' * `tfl_check(plot, levels = )` -- the checks of [tfl_check_fig()].
#'
#' A figure program sources it (`source("programs/tfl/fig_setup.R")`) and
#' ends with `tfl_check(plot)`.  `tfl_check_fig()` runs the same checks from
#' R, against the style in use.
#'
#' @param style A figure style ([tfl_fig_style()]).
#' @param date The date stamped in the banner.
#' @param plot A ggplot (or a patchwork of them).
#' @param levels The values the legend must show, in this order.
#' @param quiet `TRUE` leaves out the summary message.
#' @return `tfl_fig_setup_code()`: the script, one element per line.
#'   `tfl_check_fig()`: invisibly, a list: `problems` (each also a warning
#'   starting "Figure check:"), `notes`, `fingerprint` (an md5 of the drawn
#'   data, to tell whether a figure changed between two runs).
#' @export
tfl_fig_setup_code <- function(style = tfl_fig_style(), date = Sys.Date()) {
  q <- function(x) ifelse(is.na(x), "NA", encodeString(as.character(x), quote = "\""))
  vec <- function(x) paste0("c(", paste(q(x), collapse = ", "), ")")
  df_code <- function(name, d) {
    d <- d[setdiff(names(d), "note")]
    cols <- vapply(names(d), function(k) sprintf("  %s = %s", k, vec(d[[k]])),
                   character(1))
    c(sprintf("%s <- data.frame(", name),
      paste0(cols, ","),
      "  stringsAsFactors = FALSE)")
  }
  pals <- .fs_palettes(style)
  pal_code <- vapply(names(pals), function(n) {
    p <- pals[[n]]
    v <- if (is.null(names(p))) vec(p) else
      paste0("c(", paste(sprintf("%s = %s", q(names(p)), q(p)), collapse = ", "), ")")
    sprintf("  %s = %s", encodeString(n, quote = "`"), v)
  }, character(1))
  helpers <- readLines(system.file("fig", "fig_helpers.R", package = "tflspec"),
                       warn = FALSE, encoding = "UTF-8")
  .drop_attached_ns(c(
    "# ============================================================================",
    "#  The figure style standard and the helpers of the figure programs",
    "#  (theme_tfl, scale_colour_tfl, tfl_marker, tfl_save, tfl_check).",
    paste0("#  Generated  : tflspec ", utils::packageVersion("tflspec"), ", ",
           format(date, "%Y-%m-%d")),
    "# ============================================================================",
    "",
    df_code("tfl_settings", style$settings),
    "",
    "tfl_palettes <- list(",
    paste0(pal_code, c(rep(",", length(pal_code) - 1L), "")),
    ")",
    "",
    df_code("tfl_markers", style$markers),
    "",
    helpers,
    ""))
}

#' @rdname tfl_fig_setup_code
#' @export
tfl_check_fig <- function(plot, levels = NULL, quiet = FALSE) {
  e <- new.env(parent = globalenv())
  eval(parse(text = tfl_fig_setup_code(), encoding = "UTF-8"), envir = e)
  e$tfl_check(plot, levels = levels, quiet = quiet)
}
