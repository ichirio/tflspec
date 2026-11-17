# The sheets taken back onto the YAML spec: the sheets are a view that
# holds what is often changed, the YAML everything.  With the spec as it
# was when the sheets were written out (tfl_yaml_from_sheets()'s `base`),
# what the sheets change is applied to the spec as it is now (`to`), and
# what they cannot hold stays as the spec has it.  The change is told
# against what the sheets made of the base (`was`: the base's own sheets,
# back through tfl_yaml_from_sheets()): a cell deleted is a key `was` has
# and the sheets have not; a key the sheets cannot hold is in neither.

#' Merge the sheets' changes onto a spec
#'
#' The three-way merge [tfl_yaml_from_sheets()] runs with `base`, for one
#' file's content (a report, read by [tfl_read_output()], or a study file
#' as a list): `was` and `now` are what the sheets made of the spec, before
#' and after the edits; `yaml` is the spec as it is.  What `was` and `now`
#' say the same is `yaml`'s -- a key the sheets cannot hold is in neither,
#' and stays; what they differ in is `now`'s -- a value changed, added or
#' taken away (a cell deleted).  Maps are merged key by key (`yaml`'s keys
#' first, in its order), lists of maps by `id` (analyses, data rows), lists
#' of items by verb, other lists row by row when all are as long; anything
#' else is taken whole.
#'
#' A value the sheets change that `yaml` has changed too since the base
#' (`orig`, the spec when the sheets were written out), to something else,
#' is a conflict: the sheets' value is taken, and the place is named in the
#' attribute `conflicts`.
#'
#' @param yaml The spec's content as it is.
#' @param was,now What the sheets made of the spec: before the edits (from
#'   the base) and after.
#' @param orig The base's content (the spec when the sheets were written
#'   out), to tell conflicts; `NULL`: none told.
#' @return The merged content, with the attribute `conflicts`: the places
#'   (keys joined by ` > `) where the sheets' value was taken over a change
#'   of `yaml`'s.
#' @examples
#' yaml <- list(label = "Demographics", formats = list(mean = 1))
#' was <- list(label = "Demographics")
#' now <- list(label = "Demographics (edited)")
#' tfl_spec_merge3(yaml, was, now)
#' @export
tfl_spec_merge3 <- function(yaml, was, now, orig = NULL) {
  hit <- character()
  tell <- if (!is.null(orig)) function(path) hit <<- c(hit, paste(path, collapse = " > "))
  out <- .merge3(yaml, was, now, orig, character(), tell)
  attr(out, "conflicts") <- unique(hit)
  out
}

.merge3 <- function(spec, was, now, orig, path, conflict) {
  if (identical(was, now)) return(spec)
  if (identical(spec, was) || identical(spec, now)) return(now)
  if (.merge_is_map(spec) && .merge_is_map(was) && .merge_is_map(now)) {
    out <- list()
    for (k in union(names(spec), names(now))) {
      v <- .merge3_one(k %in% names(spec), spec[[k]], k %in% names(was), was[[k]],
                       k %in% names(now), now[[k]], if (.merge_is_map(orig)) orig[[k]], c(path, k), conflict)
      if (!is.null(v)) out[k] <- v
    }
    attributes(out) <- c(list(names = names(out)), attributes(now)[setdiff(names(attributes(now)), "names")])
    return(out)
  }
  ks <- .merge_keys(list(spec = spec, was = was, now = now, orig = if (is.list(orig)) orig else list()))
  if (!is.null(ks)) {
    out <- list()
    for (key in union(ks$now, ks$spec)) {
      at <- function(x, k) { i <- match(key, k); if (is.na(i)) NULL else x[[i]] }
      v <- .merge3_one(key %in% ks$spec, at(spec, ks$spec), key %in% ks$was, at(was, ks$was),
                       key %in% ks$now, at(now, ks$now), at(orig, ks$orig), c(path, key), conflict)
      if (!is.null(v)) out[length(out) + 1L] <- v
    }
    return(out)
  }
  if (!is.null(conflict) && !identical(spec, orig)) conflict(path)
  now
}

# One key (or one element) of the merge: list(value) to keep, NULL for
# none.  `in_s`, `in_w`, `in_n`: whether the spec, was and now have it;
# `conflict(path)` (NULL: none told) names a change of both sides.
.merge3_one <- function(in_s, s, in_w, w, in_n, n, o, path, conflict) {
  # a key the sheets cannot hold (in neither): the spec's
  if (!in_w && !in_n) return(if (in_s) list(s))
  # the sheets did not change it: the spec's (or none, the spec took it away)
  if (in_w && in_n && identical(w, n)) return(if (in_s) list(s))
  if (in_s && in_w && in_n) return(list(.merge3(s, w, n, o, path, conflict)))
  # added or taken away on the sheets: theirs, said when the spec changed it
  # too (to something else)
  if (!is.null(conflict) && !identical(s, o) && !identical(s, if (in_n) n)) conflict(path)
  if (in_n) list(n)
}

.merge_is_map <- function(x) {
  is.list(x) && length(x) && !is.null(names(x)) && all(nzchar(names(x))) &&
    !anyDuplicated(names(x)) &&
    (is.null(oldClass(x)) || inherits(x, c("tfl_output", "tfl_item")))
}

# The keys unnamed lists' elements are matched by: items by verb (and its
# occurrence), maps by id, else by row when all are as long (`orig` may be
# empty); NULL when none fits
.merge_keys <- function(lists) {
  plain <- function(x) is.list(x) && is.null(names(x)) && is.null(oldClass(x))
  if (!all(vapply(lists, plain, NA))) return(NULL)
  all_of <- function(f) all(vapply(lists, function(x) all(vapply(x, f, NA)), NA))
  if (all_of(function(e) inherits(e, "tfl_item"))) {
    return(lapply(lists, function(x) {
      fn <- vapply(x, function(e) as.character(e$fn %||% "")[1L], "")
      paste(fn, stats::ave(seq_along(fn), fn, FUN = seq_along))
    }))
  }
  id_of <- function(e) is.list(e) && is.character(e$id) && length(e$id) == 1L
  if (all_of(id_of)) {
    k <- lapply(lists, function(x) vapply(x, function(e) e$id, ""))
    if (!any(vapply(k, anyDuplicated, 1L) > 0L)) return(k)
  }
  n <- vapply(lists[c("spec", "was", "now")], length, 1L)
  if (all(n == n[[1L]]) && length(lists$orig) %in% c(0L, n[[1L]])) {
    return(lapply(lists, function(x) as.character(seq_along(x))))
  }
  NULL
}

# -- tfl_yaml_from_sheets()'s side ---------------------------------------------

# The merge's state for a `base` folder: the base's own sheets back through
# tfl_yaml_from_sheets() (`was`, a folder to remove), the base folder, the
# folder written to, and the conflicts found (an environment).  A base that
# is no YAML spec stops.
.conv_merge_state <- function(base, to, content_fn, figures) {
  fn <- "tfl_yaml_from_sheets()"
  s <- tryCatch(tfl_read_study_spec(base), error = function(e) e)
  outs <- if (!inherits(s, "error")) tryCatch(tfl_read_outputs(base, skip_bad = TRUE), error = function(e) e)
  if (inherits(s, "error") || inherits(outs, "error")) {
    .ard_stop(sprintf("%s: `base` (%s) is the study's YAML spec as the sheets were written from; it does not read: %s",
                      fn, base, conditionMessage(if (inherits(s, "error")) s else outs)))
  }
  was <- tempfile("tfl-was")
  dir.create(was)
  sh <- suppressWarnings(suppressMessages(tfl_sheets_from_spec(s, outs)))
  suppressWarnings(suppressMessages(tfl_yaml_from_sheets(sh, was, content_fn = content_fn,
                                                         overwrite = TRUE, figures = figures)))
  st <- new.env()
  st$was <- was
  st$base <- base
  st$base_outputs <- outs
  st$to <- to
  st$conflicts <- character()
  st
}

# A file the sheets give (`now`, written by `write(x, path)`, read back by
# `read(path)`) at `rel` under `to`, merged: NULL when the sheets change
# nothing of it (the file in `to` stays as it is, byte for byte); `now` when
# `to` has no such file or the base gave none; else the file's content with
# the sheets' changes (tfl_spec_merge3()).  `orig`: the base's content.
.conv_merge_file <- function(now, rel, st, write, read, orig) {
  f <- file.path(st$to, rel)
  was_f <- file.path(st$was, rel)
  tmp <- tempfile(fileext = ".yml")
  on.exit(unlink(tmp), add = TRUE)
  write(now, tmp)
  if (!file.exists(f)) return(now)
  if (file.exists(was_f) && identical(readLines(tmp, warn = FALSE), readLines(was_f, warn = FALSE))) return(NULL)
  rd <- function(p) if (file.exists(p)) tryCatch(read(p), error = function(e) NULL)
  spec <- rd(f)
  was <- rd(was_f)
  nw <- rd(tmp)
  if (is.null(spec) || is.null(was) || is.null(nw)) return(now)
  m <- tfl_spec_merge3(spec, was, nw, orig)
  hit <- attr(m, "conflicts")
  if (length(hit)) st$conflicts <- c(st$conflicts, paste0(rel, ": ", hit))
  attr(m, "conflicts") <- NULL
  m
}

# A report file's reader: the one report of that id, else NULL
.conv_read_one <- function(id) function(p) {
  x <- tfl_read_output(p)
  if (inherits(x, "tfl_output") && identical(x$id, id)) x
}

# A study file's reader and writer, as tfl_yaml_from_sheets() writes them
.conv_read_yaml <- function(p) .yaml_read_file(p, handlers = .yaml_handlers())
.conv_write_yaml <- function(x, p) {
  writeLines(yaml::as.yaml(x, indent.mapping.sequence = TRUE, handlers = .yaml_out_handlers()),
             p, useBytes = TRUE)
}
