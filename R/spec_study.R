# The study's files of the YAML spec, and the layering of a report over its
# defaults (design: _archive/spec-format-redesign-2026-10-14.md, 3.2-3.4).
#
#   study.yml                 the study: id, title ..., keys, setup
#   spec/data.yml             datasets, populations (the data catalog)
#   spec/defaults.yml         what every report starts from (a report's shape)
#   spec/codelists.yml        code lists to copy from (templates)
#   spec/patterns/<name>.yml  a layer a report may extend (a report's shape)
#   spec/toc.yml              the reports in order
#   spec/outputs/**/*.yml     one report a file (or several, `outputs:`)
#
# A report file says only what differs from the layers it extends.  The
# layering is here: maps merge by key (an `!unset` removes the key), the
# content's items of one function replace the defaults' items of that
# function, and the rest of the defaults stay in their order.

#' Read a study's YAML spec
#'
#' `tfl_read_study_spec()` reads the study-level files (every one optional)
#' and `tfl_read_outputs()` the report files under `spec/outputs/`
#' (subfolders too; a file with `outputs:` holds several).  An id in two
#' files stops the read.
#'
#' @param dir The study folder (holds `study.yml` and `spec/`).
#' @return `tfl_read_study_spec()`: a list of class `tfl_study_spec` --
#'   `study`, `data`, `defaults`, `codelists`, `patterns` (by name), `toc`,
#'   `dir`.  `tfl_read_outputs()`: a named list of `tfl_output`, by id.
#' @examples
#' d <- system.file("extdata", "spec-yaml", package = "tflspec")
#' s <- tfl_read_study_spec(d)
#' names(s$defaults$content)
#' @export
tfl_read_study_spec <- function(dir) {
  if (!dir.exists(dir)) stop("tfl_read_study_spec(): no folder ", dir, call. = FALSE)
  spec <- .spec_dir(dir)
  rd <- function(f) {
    if (!file.exists(f)) return(NULL)
    y <- yaml::read_yaml(f, handlers = .yaml_handlers())
    if (is.null(y)) list() else y
  }
  pat_files <- list.files(file.path(spec, "patterns"), "\\.ya?ml$", full.names = TRUE)
  patterns <- stats::setNames(
    lapply(pat_files, function(f) .layer_from_list(
      rd(f), paste0("pattern:", tools::file_path_sans_ext(basename(f))))),
    tools::file_path_sans_ext(basename(pat_files)))
  structure(list(
    study = rd(file.path(dir, "study.yml")),
    data = .calls_in(rd(file.path(spec, "data.yml")) %||% list()),
    defaults = .layer_from_list(rd(file.path(spec, "defaults.yml")), "defaults"),
    codelists = rd(file.path(spec, "codelists.yml")) %||% list(),
    patterns = patterns,
    toc = rd(file.path(spec, "toc.yml")) %||% list(),
    dir = normalizePath(dir, "/", mustWork = FALSE)),
    class = "tfl_study_spec")
}

# spec/ of a study folder, or the folder itself when it is a spec folder
# (the fixtures: defaults.yml beside outputs/)
.spec_dir <- function(dir) {
  if (dir.exists(file.path(dir, "spec"))) file.path(dir, "spec") else dir
}

# a defaults or pattern file as a layer: content items marked `from`, the
# rest (data, codelists, page) as plain lists with calls resolved
.layer_from_list <- function(y, from) {
  if (is.null(y)) y <- list()
  items <- lapply(y$content %||% list(), .item_from_list)
  items <- lapply(items, function(it) { it$from <- from; it })
  list(data = .calls_in(y$data %||% list()), codelists = y$codelists %||% list(),
       content = items, page = .calls_in(y$page %||% list()))
}

#' @rdname tfl_read_study_spec
#' @export
tfl_read_outputs <- function(dir) {
  out_dir <- file.path(.spec_dir(dir), "outputs")
  files <- list.files(out_dir, "\\.ya?ml$", recursive = TRUE, full.names = TRUE)
  outs <- list()
  where <- character()
  for (f in files) {
    r <- tfl_read_output(f)
    if (inherits(r, "tfl_output")) r <- list(r)
    for (o in r) {
      if (o$id %in% names(outs)) {
        stop("tfl_read_outputs(): the report ", o$id, " is in two files: ",
             where[[o$id]], " and ", f, ".", call. = FALSE)
      }
      outs[[o$id]] <- o
      where[[o$id]] <- f
    }
  }
  outs
}

# -- layering ---------------------------------------------------------------

#' A report over the layers it extends
#'
#' `tfl_resolve_output()` puts a report's own file over the layers its
#' `extends` names (`defaults`, `patterns/<name>`), nearest last.  Maps
#' (`page`'s lines by number, `tokens`, `codelists`, a call's arguments)
#' merge by key, and an [tfl_unset()] removes the key.  In `content`, the
#' report's items of one function replace every item of that function in
#' the layers below, in the place the first of them had; a function the
#' report does not have keeps the layers' items; a function only the
#' report has goes after them.  `extends: none` uses no layer.  Each item's
#' `from` says where it came from.  The result holds no `tfl_unset`.
#'
#' @param output A `tfl_output` as read from its file.
#' @param study A `tfl_study_spec` (its `defaults` and `patterns`).
#' @return The `tfl_output`, resolved (attribute `resolved` = `TRUE`).
#' @examples
#' d <- system.file("extdata", "spec-yaml", package = "tflspec")
#' s <- tfl_read_study_spec(d)
#' out <- tfl_resolve_output(tfl_read_outputs(d)[["T-14-1-1"]], s)
#' vapply(out$content, function(it) paste(it$fn, it$from), "")
#' @export
tfl_resolve_output <- function(output, study) {
  if (!inherits(output, "tfl_output")) stop("tfl_resolve_output(): `output` is a tfl_output.", call. = FALSE)
  if (!inherits(study, "tfl_study_spec")) stop("tfl_resolve_output(): `study` is a tfl_study_spec.", call. = FALSE)
  chain <- output$extends
  if (identical(chain, "none")) chain <- character()
  layers <- lapply(chain, function(nm) .layer_of(nm, study))
  # the farthest first, the report's own last
  acc <- list(data = list(), codelists = list(), content = list(), page = list())
  for (l in rev(layers)) acc <- .layer_over(acc, l)
  own <- list(data = output$data %||% list(), codelists = output$codelists,
              content = output$content, page = output$page)
  acc <- .layer_over(acc, own)
  output$data <- if (length(acc$data)) acc$data else output$data
  output$codelists <- acc$codelists
  output$content <- acc$content
  output$page <- acc$page
  attr(output, "resolved") <- TRUE
  output
}

.layer_of <- function(nm, study) {
  if (identical(nm, "defaults")) return(study$defaults)
  if (startsWith(nm, "patterns/")) {
    p <- study$patterns[[substring(nm, 10L)]]
    if (is.null(p)) stop("No pattern ", sQuote(nm), " in spec/patterns/.", call. = FALSE)
    return(p)
  }
  stop("`extends` names \"defaults\" or \"patterns/<name>\", not ", sQuote(nm), ".",
       call. = FALSE)
}

# `over` on top of `base`, part by part
.layer_over <- function(base, over) {
  list(data = .merge_map(base$data, over$data),
       codelists = .merge_map(base$codelists, over$codelists),
       content = .merge_content(base$content, over$content),
       page = .merge_map(base$page, over$page))
}

# a map over a map: by key; an !unset removes the key; two maps recurse; a
# sequence (unnamed list or vector) replaces whole
.merge_map <- function(base, over) {
  if (is.null(over) || !length(over)) return(.drop_unset(base))
  if (is.null(base) || !length(base) || !.is_map(base) || !.is_map(over)) return(.drop_unset(over))
  for (k in names(over)) {
    v <- over[[k]]
    if (inherits(v, "tfl_unset")) {
      base[[k]] <- NULL
    } else if (.is_map(v) && .is_map(base[[k]])) {
      base[[k]] <- .merge_map(base[[k]], v)
    } else {
      base[[k]] <- v
    }
  }
  base
}

.is_map <- function(x) {
  is.list(x) && !inherits(x, c("tfl_call", "tfl_item", "tfl_r", "tfl_unset")) &&
    length(x) > 0L && !is.null(names(x)) && all(nzchar(names(x)))
}

# an !unset with nothing under it: gone (a report may unset a key no layer set)
.drop_unset <- function(x) {
  if (!is.list(x) || !length(x)) return(x)
  keep <- !vapply(x, inherits, NA, "tfl_unset")
  x <- x[keep]
  if (.is_map(x)) x[] <- lapply(x, .drop_unset)
  x
}

# the content: the report's items of a function replace the layers' items
# of it, in the place the first of them had; the rest stay; a new function
# goes last
.merge_content <- function(base, over) {
  if (!length(over)) return(base)
  if (!length(base)) return(over)
  fns_over <- unique(vapply(over, `[[`, "", "fn"))
  out <- list()
  placed <- character()
  for (it in base) {
    if (it$fn %in% fns_over) {
      if (!it$fn %in% placed) {
        out <- c(out, Filter(function(o) identical(o$fn, it$fn), over))
        placed <- c(placed, it$fn)
      }
    } else {
      out <- c(out, list(it))
    }
  }
  rest <- Filter(function(o) !o$fn %in% placed, over)
  c(out, rest)
}
