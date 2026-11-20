# The series a study can use, and a study's values of one (study.yml
# series_settings:): what the screen lists and edits.

#' The series found, and a study's values of one
#'
#' `tfl_series_list()` lists the series in the folders [tfl_series_dir()]
#' looks in (the first folder that has a series's name wins).
#' `tfl_series_settings()` gives a study's values of a series: the series's
#' `values:` with the study's `series_settings:` over them (a series value
#' written `!study K` is the study's `settings:` value, or its fallback).
#' `tfl_series_settings_set()` writes a study's values of a series into its
#' `study.yml` (`series_settings: <series>:`); the rest of the file is kept
#' as it is.
#'
#' @param dir A folder (or folders) of series, looked in first.
#' @param study The study: `study.yml` read as a list, the file or the study
#'   folder.
#' @param series A series's name; for `tfl_series_settings()`, `NULL` gives
#'   the study's whole `series_settings:` block as written.
#' @param study_dir The study folder (holds `study.yml`).
#' @param values A named list: the values to set (a key of the series's
#'   `values:`); `NULL` for a key removes the study's own value.
#' @return `tfl_series_list()`: a data frame -- `name`, `label`, `released`,
#'   `path`, `source` (`"dir"`, `"option"`, `"user"` or `"package"`).
#'   `tfl_series_settings()`: a named list of the values, with attribute
#'   `"origins"` (`"study"` or `"series"` for each).
#'   `tfl_series_settings_set()`: the `study.yml` path, invisibly.
#' @seealso [tfl_series_dir()], [tfl_template_resolve()]
#' @examples
#' tfl_series_list()[, c("name", "label", "source")]
#' @export
tfl_series_list <- function(dir = NULL) {
  src <- c(rep("dir", length(dir)), if (!is.null(getOption("tflspec.series_dir"))) "option",
           "user", "package")
  dirs <- .series_search_dirs(dir)
  rows <- list()
  seen <- character()
  for (i in seq_along(dirs)) {
    d <- dirs[[i]]
    if (!nzchar(d) || !dir.exists(d)) next
    for (s in sort(list.files(d))) {
      f <- file.path(d, s, "series.yml")
      if (s %in% seen || !file.exists(f) || !grepl(.series_name_re, s)) next
      seen <- c(seen, s)
      y <- tryCatch(.series_read_yaml(f), error = function(e) list())
      rows[[length(rows) + 1L]] <- data.frame(
        name = s, label = as.character(y$label %||% NA_character_),
        released = as.character(y$released %||% NA_character_),
        path = normalizePath(file.path(d, s), winslash = "/", mustWork = TRUE),
        source = src[[i]], stringsAsFactors = FALSE)
    }
  }
  if (!length(rows)) {
    return(data.frame(name = character(), label = character(), released = character(),
                      path = character(), source = character(), stringsAsFactors = FALSE))
  }
  do.call(rbind, rows)
}

#' @rdname tfl_series_list
#' @export
tfl_series_settings <- function(study, series = NULL) {
  study <- .tr_study(study, "tfl_series_settings()")
  if (is.null(series)) return(study$series_settings %||% list())
  s <- tfl_series_read(series)
  .series_check_settings(study, s)
  own <- study$series_settings[[series]] %||% list()
  out <- s$values
  origins <- stats::setNames(rep("series", length(out)), names(out))
  # a series value that is the study's (!study): its value for this study
  # (still the series's value: the study's series_settings: do not hold it)
  for (k in names(out)) {
    if (!inherits(out[[k]], "tfl_study_ref")) next
    out[k] <- list(.series_study_ref_value(study, out[[k]])$value)
  }
  for (k in intersect(names(own), names(out))) {
    out[k] <- list(own[[k]])
    origins[[k]] <- "study"
  }
  attr(out, "origins") <- origins
  out
}

#' @rdname tfl_series_list
#' @export
tfl_series_settings_set <- function(study_dir, series, values) {
  fn <- "tfl_series_settings_set()"
  f <- file.path(study_dir, "study.yml")
  if (!file.exists(f)) stop(fn, ": no file ", f, call. = FALSE)
  s <- tfl_series_read(series)
  if (!is.list(values) || (length(values) && (is.null(names(values)) || any(!nzchar(names(values)))))) {
    stop(fn, ": `values` is a named list.", call. = FALSE)
  }
  bad <- setdiff(names(values), names(s$values))
  if (length(bad)) {
    stop(fn, ": ", paste(bad, collapse = ", "), " is not a value of series ", series,
         " (series.yml values:).", call. = FALSE)
  }
  lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
  y <- yaml::yaml.load(paste(lines, collapse = "\n"), handlers = .yaml_handlers()) %||% list()
  block <- y$series_settings %||% list()
  own <- block[[series]] %||% list()
  for (k in names(values)) own[k] <- list(values[[k]])
  own <- own[!vapply(own, is.null, NA)]
  if (length(own)) block[[series]] <- own else block[[series]] <- NULL
  # the old block out (its line to the next top-level key), the new one in
  top <- grep("^[^[:space:]#][^:]*:", lines)
  at <- grep("^series_settings:", lines)
  if (length(at)) {
    nxt <- top[top > at[1L]]
    end <- if (length(nxt)) nxt[1L] - 1L else length(lines)
    lines <- lines[-seq(at[1L], end)]
  }
  if (length(block)) {
    new <- strsplit(sub("\n$", "", yaml::as.yaml(list(series_settings = block))), "\n")[[1L]]
    lines <- if (length(at)) append(lines, new, after = at[1L] - 1L) else c(lines, new)
  }
  con <- file(f, open = "wb")
  on.exit(close(con), add = TRUE)
  writeLines(lines, con, sep = "\n")
  invisible(f)
}
