# The series sources (the company's and tflspec's own folders): what a
# screen lists.  A study's own copies are in R/study_templates.R.

#' The series found
#'
#' `tfl_series_list()` lists the series in the source folders
#' [tfl_series_dir()] looks in (the first folder that has a series's name
#' wins).  A study does not read these folders: it reads its own copy of a
#' series ([tfl_copy_templates()]).
#'
#' @param dir A folder (or folders) of series, looked in first.
#' @return A data frame -- `name`, `label`, `released`, `path`, `source`
#'   (`"dir"`, `"option"`, `"user"` or `"package"`).
#' @seealso [tfl_series_dir()], [tfl_copy_templates()]
#' @examples
#' tfl_series_list()[, c("name", "label", "source")]
#' @export
tfl_series_list <- function(dir = NULL) {
  src <- c(rep("dir", length(dir)), rep("option", length(getOption("tflspec.series_dir"))),
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
