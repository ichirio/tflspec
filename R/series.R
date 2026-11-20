# Series: versioned folders of template SPECs (tflspec_v1, acme_v1, ...).
#
#   <series>/series.yml            name, label, released, needs, fixes, values,
#                                  descriptions (a value key's meaning, for a screen)
#   <series>/templates/<name>.yml  the templates (`!series K` reads values:)
#   <series>/styles/<name>.yml
#
# A study changes a series's values in study.yml `series_settings:`
# ({<series>: {<keys of values:>}}), beside `settings:`.  A report SPEC
# names a template as `template: name: <series>/<template>`.
# tfl_pin_series() writes spec/series.lock (the md5 of series.yml and of
# each template the reports use); tfl_series_check_lock() compares.
#
# Tags (the handlers below, added to .yaml_handlers()):
#   !series K   study series_settings[[series]][[K]], else values[[K]]
#   !study K    study.yml settings: at key path K (dotted); unset if missing
#   !fixed v    v, marked fixed: a later layer cannot override it
#   !unset      tfl_unset()

.series_name_re <- "^[a-z][a-z0-9_]*$"

.series_check_name <- function(series, fn) {
  if (!is.character(series) || length(series) != 1L || is.na(series) ||
      !grepl(.series_name_re, series)) {
    shown <- if (is.character(series) && length(series) == 1L) series else deparse(series)[1L]
    stop(fn, ": series \"", shown, "\" is not a series name (lower-case letters, digits and _, ",
         "starting with a letter).", call. = FALSE)
  }
  invisible(series)
}

# the folders a series is looked for in, in order
.series_search_dirs <- function(dir = NULL) {
  c(dir,
    getOption("tflspec.series_dir"),
    file.path(tools::R_user_dir("tflplanner", "data"), "standards", "series"),
    system.file("series", package = "tflspec"))
}

#' Series of templates
#'
#' A series is a versioned folder of template SPECs (`tflspec_v1`,
#' `acme_v1`, ...): `series.yml` (`name`, `label`, `released`, `needs`,
#' `fixes`, the values its templates read with `!series K` under `values:`,
#' and, optionally, what each value means under `descriptions:` (a value's
#' key and one line of English, for a screen that shows the values),
#' `templates/<name>.yml` and `styles/<name>.yml`.  A report SPEC
#' names a template as `template: name: tflspec_v1/demog`, and a study
#' changes a series's values in `study.yml`:
#'
#' ```yaml
#' series_settings:
#'   tflspec_v1:
#'     max_rows: 25
#' ```
#'
#' `tfl_series_dir()` finds the folder of a series.  It looks in `dir`, then
#' `getOption("tflspec.series_dir")`, then
#' `file.path(tools::R_user_dir("tflplanner", "data"), "standards", "series")`,
#' then the series that come with tflspec (`system.file("series", package =
#' "tflspec")`); the first folder that holds `<series>/series.yml` wins.
#' `tfl_series_read()` reads its `series.yml`.
#'
#' @param series The series's name: lower-case letters, digits and `_`,
#'   starting with a letter.
#' @param dir A folder (or folders) of series, looked in first; `NULL` for
#'   the default search only.
#' @return `tfl_series_dir()`: the folder of the series (stops if none has
#'   it).  `tfl_series_read()`: a list of class `tfl_series` -- `name`,
#'   `label`, `released`, `needs`, `fixes`, `values` (a named list),
#'   `descriptions` (a named character vector, a value's key to its meaning;
#'   empty if the series has none), `path`
#'   (the series's folder) and `file` (its `series.yml`).
#' @seealso [tfl_pin_series()]
#' @examples
#' d <- tempfile("series")
#' dir.create(file.path(d, "demo_v1"), recursive = TRUE)
#' writeLines(c("name: demo_v1", "label: Demo", "values:", "  max_rows: 30",
#'              "descriptions:", "  max_rows: The most rows a page holds."),
#'            file.path(d, "demo_v1", "series.yml"))
#' tfl_series_dir("demo_v1", dir = d)
#' tfl_series_read("demo_v1", dir = d)$values
#' tfl_series_read("demo_v1", dir = d)$descriptions
#' unlink(d, recursive = TRUE)
#' @export
tfl_series_dir <- function(series, dir = NULL) {
  .series_check_name(series, "tfl_series_dir()")
  dirs <- .series_search_dirs(dir)
  dirs <- dirs[nzchar(dirs)]
  for (d in dirs) {
    f <- file.path(d, series, "series.yml")
    if (file.exists(f)) return(normalizePath(file.path(d, series), winslash = "/", mustWork = TRUE))
  }
  stop("tfl_series_dir(): no series \"", series, "\" (no ", series, "/series.yml in ",
       if (length(dirs)) paste(dirs, collapse = ", ") else "any folder", ").", call. = FALSE)
}

#' @rdname tfl_series_dir
#' @export
tfl_series_read <- function(series, dir = NULL) {
  .series_check_name(series, "tfl_series_read()")
  path <- tfl_series_dir(series, dir = dir)
  file <- file.path(path, "series.yml")
  y <- .yaml_read_file(file, handlers = .yaml_handlers())
  if (is.null(y)) y <- list()
  if (!.series_is_map(y)) stop("tfl_series_read(): ", file, " is not a map.", call. = FALSE)
  if (!is.null(y$name) && !identical(y$name, series)) {
    stop("tfl_series_read(): series \"", series, "\": its series.yml says name: ", y$name,
         " (the name is the folder's).", call. = FALSE)
  }
  values <- y$values %||% list()
  if (!.series_is_map(values) && length(values)) {
    stop("tfl_series_read(): series \"", series, "\": values: is a map ({key: value}).", call. = FALSE)
  }
  docs <- y$descriptions %||% list()
  if (length(docs) && (!.series_is_map(docs) ||
                       !all(vapply(docs, function(d) is.character(d) && length(d) == 1L && !is.na(d),
                                   logical(1))))) {
    stop("tfl_series_read(): series \"", series, "\": descriptions: is a map ({key: text}), ",
         "one text a key of values:.", call. = FALSE)
  }
  docs <- if (length(docs)) unlist(docs) else stats::setNames(character(), character())
  structure(list(name = series, label = y$label, released = y$released, needs = y$needs,
                 fixes = y$fixes %||% list(), values = if (length(values)) values else list(),
                 descriptions = docs,
                 path = path, file = normalizePath(file, winslash = "/", mustWork = TRUE)),
            class = "tfl_series")
}

#' @export
print.tfl_series <- function(x, ...) {
  cat("<tfl_series> ", x$name, if (!is.null(x$label)) paste0(" (", x$label, ")"), "\n", sep = "")
  cat("  path:   ", x$path, "\n", sep = "")
  cat("  values: ", paste(names(x$values), collapse = ", "), "\n", sep = "")
  invisible(x)
}

.series_is_map <- function(x) is.list(x) && !is.null(names(x)) && all(nzchar(names(x)))

# ---- tags -------------------------------------------------------------------

# .yaml_handlers() and the series tags: !series K, !study K, !fixed v.
# A tagged value is kept as a marker (a list, so that .yaml_seq() does not
# fold it into a vector); .series_resolve() replaces it.
.series_handlers <- function() {
  c(.yaml_handlers(),
    list(series = function(x) structure(list(key = as.character(x)), class = "tfl_series_ref"),
         study = function(x) structure(list(key = as.character(x)), class = "tfl_study_ref"),
         fixed = function(x) structure(list(value = .series_fixed_value(x)), class = "tfl_fixed_ref")))
}

# The value of a `!fixed` tag.  The reader gives a tagged scalar as its
# text (`!fixed 3` is "3"): it is read again as plain YAML.  A sequence is
# folded as .yaml_seq() folds one.
.series_fixed_value <- function(x) {
  if (is.character(x) && length(x) == 1L && is.null(attributes(x))) {
    return(yaml::yaml.load(x, handlers = .yaml_handlers()))
  }
  if (is.list(x) && is.null(names(x))) return(.yaml_seq(x))
  x
}

# a series template (or any file with the series tags) as a plain list
.series_read_yaml <- function(path) {
  .yaml_read_file(path, handlers = .series_handlers())
}

# ---- resolution -------------------------------------------------------------

# Replaces the tagged values of `x` (a parsed YAML value: a list or a vector).
#   series    a tfl_series (tfl_series_read()), or a series's name
#   study     the parsed study.yml (settings:, series_settings:), or NULL
#   template  the template's name, for the messages
#   origin    the origin of an untagged value ("template", or "report" for a
#             report SPEC)
# The result has attribute "origins": a named character vector, by the
# dotted path of each value (a list's element without a name by its
# position), of "report" / "template" / "study" / "series" / "fixed".  A
# `!fixed` value also carries attribute "fixed" = TRUE and "origin" = "fixed".
.series_resolve <- function(x, series, study = NULL, template = NULL, origin = "template") {
  if (is.character(series)) series <- tfl_series_read(series)
  if (!inherits(series, "tfl_series")) {
    stop(".series_resolve(): `series` is a tfl_series or a series's name.", call. = FALSE)
  }
  .series_check_settings(study, series)
  sname <- series$name
  own <- study$series_settings[[sname]]
  where <- if (is.null(template)) paste0("series ", sname) else paste0("template ", template)
  origins <- character()
  note <- function(path, o) origins[[if (length(path)) paste(path, collapse = ".") else "."]] <<- o
  lookup <- function(key) {
    if (!is.null(own) && key %in% names(own)) return(list(value = own[[key]], origin = "study"))
    if (key %in% names(series$values)) return(list(value = series$values[[key]], origin = "series"))
    stop(where, ": !series ", key, ": no value ", key, " in series ", sname,
         " (series.yml values:) or in study.yml series_settings: ", sname, ":.", call. = FALSE)
  }
  walk <- function(v, path, o) {
    if (inherits(v, "tfl_series_ref")) {
      hit <- lookup(v$key)
      note(path, hit$origin)
      return(hit$value)
    }
    if (inherits(v, "tfl_study_ref")) {
      val <- .series_study_value(study, v$key)
      note(path, "study")
      return(val)
    }
    if (inherits(v, "tfl_fixed_ref")) {
      inner <- walk(v$value, path, "fixed")
      note(path, "fixed")
      attr(inner, "fixed") <- TRUE
      attr(inner, "origin") <- "fixed"
      return(inner)
    }
    if (is.list(v) && !inherits(v, c("tfl_unset", "tfl_r", "tfl_call"))) {
      if (!length(v)) {
        note(path, o)
        return(v)
      }
      nms <- names(v)
      for (i in seq_along(v)) {
        key <- if (!is.null(nms) && nzchar(nms[[i]])) nms[[i]] else as.character(i)
        r <- walk(v[[i]], c(path, key), o)
        if (is.null(r)) v[i] <- list(NULL) else v[[i]] <- r
      }
      return(v)
    }
    note(path, o)
    v
  }
  out <- walk(x, character(), origin)
  if (is.null(out)) return(out)
  attr(out, "origins") <- origins
  out
}

# study.yml settings: at a dotted key path; tfl_unset() if it is missing
.series_study_value <- function(study, key) {
  v <- study$settings
  for (k in strsplit(key, ".", fixed = TRUE)[[1L]]) {
    if (!is.list(v) || !k %in% names(v)) return(tfl_unset())
    v <- v[[k]]
  }
  v
}

# study.yml series_settings: a key of a series that its values: do not have
.series_check_settings <- function(study, series) {
  own <- study$series_settings[[series$name]]
  if (is.null(own)) return(invisible(character()))
  if (!.series_is_map(own)) {
    stop("study.yml series_settings: ", series$name, ": is a map ({key: value}).", call. = FALSE)
  }
  bad <- setdiff(names(own), names(series$values))
  for (k in bad) {
    warning("study.yml series_settings: ", series$name, ": ", k, " is not a value of series ",
            series$name, " (series.yml values:); it is not read.", call. = FALSE)
  }
  invisible(bad)
}

# ---- formats ----------------------------------------------------------------

# {N: 0, mean: 2, ..., "AGE:mean": 1} -> list(base = list(N = 0L, ...),
# vars = list(AGE = list(mean = 1L))); a whole number becomes an integer
.series_fmt_list <- function(formats) {
  if (is.null(formats) || !length(formats)) return(list(base = list(), vars = list()))
  formats <- as.list(formats)
  if (is.null(names(formats)) || !all(nzchar(names(formats)))) {
    stop("formats: is a map ({stat: digits or pattern}).", call. = FALSE)
  }
  as_fmt <- function(v) {
    if (is.numeric(v) && length(v) == 1L && !is.na(v) && v == round(v) &&
        abs(v) <= .Machine$integer.max) return(as.integer(v))
    v
  }
  base <- list()
  vars <- list()
  for (k in names(formats)) {
    v <- as_fmt(formats[[k]])
    if (grepl(":", k, fixed = TRUE)) {
      var <- sub(":.*$", "", k)
      stat <- sub("^[^:]*:", "", k)
      if (!nzchar(var) || !nzchar(stat)) stop("formats: ", k, " is VAR:stat.", call. = FALSE)
      if (is.null(vars[[var]])) vars[[var]] <- list()
      vars[[var]][[stat]] <- v
    } else {
      base[[k]] <- v
    }
  }
  list(base = base, vars = vars)
}

# ---- lock ---------------------------------------------------------------------

# the series/template pairs that report SPECs name (`template: name: s/t`)
.series_templates_used <- function(files) {
  found <- list()
  walk <- function(v) {
    if (!is.list(v)) return(invisible())
    tp <- if (!is.null(names(v)) && "template" %in% names(v)) v[["template"]]
    if (is.list(tp) && is.character(tp$name) && length(tp$name) == 1L &&
        grepl("^[^/]+/[^/]+$", tp$name)) {
      found[[length(found) + 1L]] <<- tp$name
    }
    for (e in v) walk(e)
    invisible()
  }
  for (f in files) walk(.series_read_yaml(f))
  u <- unique(unlist(found))
  if (!length(u)) return(data.frame(series = character(), template = character()))
  data.frame(series = sub("/.*$", "", u), template = sub("^.*/", "", u))
}

.series_md5 <- function(f) unname(as.character(tools::md5sum(f)))

#' Pin the series a study uses
#'
#' `tfl_pin_series()` writes `<study_dir>/spec/series.lock`: one entry for
#' each series the report SPECs (`spec/outputs/*.yml`) name a template of
#' (`template: name: <series>/<template>`), with the series's folder
#' (`path`), the md5 of its `series.yml` (`series`) and the md5 of each
#' template file used (`templates`, `<series>/templates/<template>.yml`).
#' The file is written by this function and never edited by hand.
#' `tfl_series_check_lock()` compares the current files with the lock and
#' warns, naming the file, for each one that changed or is missing.
#'
#' ```yaml
#' tflspec_v1:
#'   path: <series folder>/tflspec_v1
#'   series: 41d7e2...
#'   templates:
#'     demog: 3f9c1a...
#' ```
#'
#' @param study_dir The study folder (holds `study.yml` and `spec/`).
#' @param reports The report SPEC files to read; `NULL` (default) for every
#'   `.yml` file under `spec/outputs/`.  A relative path that is not a file
#'   is read from `spec/outputs/`.
#' @return `tfl_pin_series()`: the lock file's path, invisibly.
#'   `tfl_series_check_lock()`: invisibly, a data frame of the files of the
#'   lock -- `series`, `template` (`NA` for `series.yml`), `file`, `locked`
#'   and `current` (md5s) and `status` (`"ok"`, `"changed"` or `"missing"`).
#' @seealso [tfl_series_dir()]
#' @export
tfl_pin_series <- function(study_dir, reports = NULL) {
  if (!dir.exists(study_dir)) stop("tfl_pin_series(): no folder ", study_dir, call. = FALSE)
  out_dir <- file.path(study_dir, "spec", "outputs")
  if (is.null(reports)) {
    reports <- list.files(out_dir, "\\.ya?ml$", full.names = TRUE, recursive = TRUE)
  } else {
    alt <- file.path(out_dir, reports)
    reports <- ifelse(file.exists(reports), reports, alt)
    miss <- reports[!file.exists(reports)]
    if (length(miss)) stop("tfl_pin_series(): no report file ", miss[[1L]], call. = FALSE)
  }
  used <- .series_templates_used(reports)
  lock <- list()
  for (s in sort(unique(used$series))) {
    .series_check_name(s, "tfl_pin_series()")
    path <- tfl_series_dir(s)
    tpls <- sort(unique(used$template[used$series == s]))
    md5 <- vapply(tpls, function(t) {
      f <- file.path(path, "templates", paste0(t, ".yml"))
      if (!file.exists(f)) stop("tfl_pin_series(): no template ", s, "/", t, " (", f, ").", call. = FALSE)
      .series_md5(f)
    }, "")
    lock[[s]] <- list(path = path, series = .series_md5(file.path(path, "series.yml")),
                      templates = as.list(stats::setNames(md5, tpls)))
  }
  f <- file.path(study_dir, "spec", "series.lock")
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  body <- if (length(lock)) yaml::as.yaml(lock) else "{}\n"
  con <- file(f, open = "wb")
  on.exit(close(con), add = TRUE)
  writeLines(c("# spec/series.lock: written by tflspec::tfl_pin_series() from the report SPECs;",
               "# do not edit by hand.", sub("\n$", "", body)), con, sep = "\n")
  invisible(f)
}

#' @rdname tfl_pin_series
#' @export
tfl_series_check_lock <- function(study_dir) {
  f <- file.path(study_dir, "spec", "series.lock")
  if (!file.exists(f)) stop("tfl_series_check_lock(): no ", f, " (see tfl_pin_series()).", call. = FALSE)
  lock <- yaml::read_yaml(f)
  rows <- list()
  add <- function(s, t, file, locked) {
    cur <- if (!is.na(file) && file.exists(file)) .series_md5(file) else NA_character_
    st <- if (is.na(cur)) "missing" else if (identical(cur, locked)) "ok" else "changed"
    if (st == "changed") warning("series ", s, ": ", file, " changed since tfl_pin_series().", call. = FALSE)
    if (st == "missing") {
      warning("series ", s, ": ", if (is.na(file)) "the series" else file,
              " is missing (locked by tfl_pin_series()).", call. = FALSE)
    }
    rows[[length(rows) + 1L]] <<- data.frame(series = s, template = t, file = file,
                                             locked = locked, current = cur, status = st)
  }
  for (s in names(lock)) {
    e <- lock[[s]]
    path <- tryCatch(tfl_series_dir(s), error = function(err) NA_character_)
    if (!is.na(path) && !is.null(e$path) && !identical(path, e$path)) {
      message("series ", s, " is read from ", path, " (lock: ", e$path, ").")
    }
    add(s, NA_character_, if (is.na(path)) NA_character_ else file.path(path, "series.yml"),
        as.character(e$series))
    for (t in names(e$templates)) {
      add(s, t, if (is.na(path)) NA_character_ else file.path(path, "templates", paste0(t, ".yml")),
          as.character(e$templates[[t]]))
    }
  }
  out <- if (length(rows)) do.call(rbind, rows) else
    data.frame(series = character(), template = character(), file = character(),
               locked = character(), current = character(), status = character())
  invisible(out)
}
