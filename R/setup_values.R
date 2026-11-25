# What a study's setup files say that the generated programs depend on
# (tflspec#910, G5-1a): the packages they attach (a program calls those
# without `pkg::`, option tflspec.attached) and the days in a week, a month
# and a year (the figure programs' time units).  Read from the files' text,
# never run, and never from a file or an option: the caller gives the text.

#' What a study's setup files say: their packages and time units
#'
#' The setup files a program runs first (a study's company setup, its
#' generated setup and the user's own, in that order) attach the packages
#' the programs use and may set the days in a week, a month and a year.
#' This reads both from the files' text, without running it.
#'
#' @param files The files' text, in the order a program runs them: a list,
#'   an element a file (its lines, or one string); `NULL` for a file that is
#'   not there.  One character vector is one file.
#' @return A list.  `attached`: the packages the files attach with
#'   `library()` or `require()` (wherever the call is: inside
#'   `suppressPackageStartupMessages()` too), in the order they run, each
#'   once; none when the files' code does not parse.  `time_units`:
#'   [tfl_time_unit_days()], with the values the files set on a line of
#'   their own (`days_per_month <- 30`), the last one winning as when they
#'   run.
#' @examples
#' tfl_setup_values(list(
#'   c("library(cards)", "suppressPackageStartupMessages(library(dplyr))"),
#'   NULL,
#'   c("days_per_month <- 30  # our convention", "library(cards)")))
#' @keywords internal
#' @export
tfl_setup_values <- function(files) {
  if (is.character(files)) files <- list(files)
  if (!is.list(files) ||
      !all(vapply(files, function(x) is.null(x) || is.character(x), NA))) {
    stop("tfl_setup_values(): `files` is a list of the setup files' text ",
         "(a character vector a file, or NULL).", call. = FALSE)
  }
  files <- Filter(Negate(is.null), files)
  list(attached = .setup_attached(files), time_units = .setup_time_units(files))
}

# the library() and require() calls of the files' code, in order
.setup_attached <- function(files) {
  ex <- tryCatch(do.call(c, c(list(expression()), lapply(files, function(x)
    parse(text = x, keep.source = FALSE, encoding = "UTF-8")))),
    error = function(e) NULL)
  out <- character()
  walk <- function(e) {
    if (!is.call(e)) return(invisible())
    fn <- e[[1L]]
    nm <- if (is.name(fn)) as.character(fn) else
      if (is.call(fn) && identical(fn[[1L]], as.name("::"))) as.character(fn[[3L]]) else ""
    if (nm %in% c("library", "require")) {
      m <- tryCatch(match.call(base::library, e), error = function(err) NULL)
      pk <- m$package
      if (is.name(pk) || (is.character(pk) && length(pk) == 1L)) {
        out <<- c(out, as.character(pk))
      }
    }
    for (a in as.list(e)[-1L]) walk(a)
  }
  for (e in ex) walk(e)
  unique(out)
}

# the time units the files' lines set (the last one wins), else tflspec's
.setup_time_units <- function(files) {
  d <- tfl_time_unit_days()
  lines <- unlist(lapply(files, function(x) unlist(strsplit(x, "\r?\n"))),
                  use.names = FALSE)
  lines <- lines[!is.na(lines)]
  if (!length(lines)) return(d)
  for (k in names(d)) {
    rx <- sprintf("^[[:space:]]*%s[[:space:]]*(<-|=)[[:space:]]*([0-9]+([.][0-9]*)?)[[:space:]]*(#.*)?$", k)
    hit <- regmatches(lines, regexec(rx, lines))
    v <- unlist(lapply(hit, function(m) if (length(m)) m[3L]))
    if (length(v)) d[[k]] <- as.numeric(v[length(v)])
  }
  d
}
