# The sheets taken back onto the YAML spec already there: the sheets are a
# simple view that holds what is often changed, the YAML everything.  What
# the sheets change is applied to the YAML; what they cannot hold stays as
# the YAML has it.  A change is told by a baseline: what the sheets make of
# the YAML as it is (tfl_sheets_from_spec() of it, back through
# tfl_yaml_from_sheets()).  A cell deleted is a key the baseline has and
# the sheets do not; a key the sheets cannot hold is in neither, and is
# the YAML's.  (Report files; the study's files are rebuilt as before.)

# The baseline of a study folder `to` that has a YAML spec: a folder (to
# remove) with what tfl_yaml_from_sheets() makes of the spec's own sheets;
# NULL when `to` has no spec, or it cannot be read or written as sheets
.conv_base <- function(to, content_fn, figures) {
  if (!dir.exists(file.path(to, "spec", "outputs"))) return(NULL)
  s <- tryCatch(tfl_read_study_spec(to), error = function(e) NULL)
  outs <- if (!is.null(s)) tryCatch(tfl_read_outputs(to, skip_bad = TRUE), error = function(e) NULL)
  if (!length(outs)) return(NULL)
  dir <- tempfile("tfl-was")
  dir.create(dir)
  ok <- tryCatch({
    sh <- suppressWarnings(suppressMessages(tfl_sheets_from_spec(s, outs)))
    suppressWarnings(suppressMessages(tfl_yaml_from_sheets(sh, dir, content_fn = content_fn,
                                                           overwrite = TRUE, figures = figures)))
    TRUE
  }, error = function(e) FALSE)
  if (!ok) {
    unlink(dir, recursive = TRUE)
    return(NULL)
  }
  dir
}

# A report the sheets give (`now`) onto its file `f` in the folder, with
# the baseline's file `was_f`: NULL when the sheets change nothing of it
# (the file stays as it is, byte for byte); else the file's report with
# the sheets' changes (.merge3()).  `now` as it is when the file or the
# baseline has no such one report.
.conv_merge_output <- function(now, f, was_f) {
  if (!file.exists(f) || !file.exists(was_f)) return(now)
  tmp <- tempfile(fileext = ".yml")
  on.exit(unlink(tmp), add = TRUE)
  tfl_write_output(now, tmp)
  if (identical(readLines(tmp, warn = FALSE), readLines(was_f, warn = FALSE))) return(NULL)
  one <- function(p) {
    x <- tryCatch(tfl_read_output(p), error = function(e) NULL)
    if (inherits(x, "tfl_output") && identical(x$id, now$id)) x
  }
  spec <- one(f)
  was <- one(was_f)
  nw <- one(tmp)
  if (is.null(spec) || is.null(was) || is.null(nw)) return(now)
  .merge3(spec, was, nw)
}

# Three-way merge: `spec` (the YAML), `was` (the baseline: what the sheets
# made of it), `now` (the sheets).  What was and now say the same is the
# spec's; what they differ in is now's.  Maps key by key (the spec's keys
# first, in its order; a key was has and now has not is taken away);
# lists of items by verb, lists of maps by id (or by row when all three
# are as long); anything else whole.
.merge3 <- function(spec, was, now) {
  if (identical(was, now)) return(spec)
  if (identical(spec, was)) return(now)
  if (.merge_is_map(spec) && .merge_is_map(was) && .merge_is_map(now)) {
    out <- list()
    for (k in union(names(spec), names(now))) {
      if (k %in% names(now)) {
        if (!k %in% names(was)) {
          out[k] <- list(now[[k]])
        } else if (k %in% names(spec)) {
          out[k] <- list(.merge3(spec[[k]], was[[k]], now[[k]]))
        } else if (!identical(was[[k]], now[[k]])) {
          out[k] <- list(now[[k]])
        }
      } else if (!k %in% names(was)) {
        # (a key the sheets cannot hold)
        out[k] <- list(spec[[k]])
      }
    }
    attributes(out) <- c(list(names = names(out)), attributes(now)[setdiff(names(attributes(now)), "names")])
    return(out)
  }
  keys <- .merge_keys(spec, was, now)
  if (is.null(keys)) return(now)
  out <- list()
  for (i in seq_along(now)) {
    j <- match(keys$now[i], keys$spec)
    w <- match(keys$now[i], keys$was)
    out[[length(out) + 1L]] <- if (is.na(w)) now[[i]] else if (is.na(j)) {
      if (identical(was[[w]], now[[i]])) NULL else now[[i]]
    } else .merge3(spec[[j]], was[[w]], now[[i]])
  }
  # (an element the sheets cannot hold: in neither)
  for (j in which(!keys$spec %in% c(keys$was, keys$now))) out[[length(out) + 1L]] <- spec[[j]]
  Filter(Negate(is.null), out)
}

.merge_is_map <- function(x) {
  is.list(x) && length(x) && !is.null(names(x)) && all(nzchar(names(x))) &&
    !anyDuplicated(names(x)) &&
    (is.null(oldClass(x)) || inherits(x, c("tfl_output", "tfl_item")))
}

# The keys three unnamed lists' elements are matched by: items by verb (and
# its occurrence), maps by id, else by row when all three are as long;
# NULL when none fits
.merge_keys <- function(spec, was, now) {
  lists <- list(spec = spec, was = was, now = now)
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
  n <- vapply(lists, length, 1L)
  if (all(n == n[[1L]])) return(lapply(lists, function(x) as.character(seq_along(x))))
  NULL
}
