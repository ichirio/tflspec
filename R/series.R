# Series: versioned folders of template SPECs (tflspec_v1, acme_v1, ...).
#
#   <series>/series.yml            name, label, released, needs, fixes, values,
#                                  descriptions (a value key's meaning, for a screen)
#   <series>/templates/<name>.yml  the templates (`!series K` reads values:)
#   <series>/styles/<name>.yml
#
# These folders are the sources.  A study copies the series.yml and the
# templates its reports use into <study>/spec/series/<series>/ (R/study_templates.R,
# tfl_copy_templates()) and reads only its copy; it changes a value in its
# copy's values:.  A report SPEC names a template as
# `template: name: <series>/<template>`.
#
# Tags (the handlers below, added to .yaml_handlers()):
#   !series K   values[[K]] of the series.yml read (the study's copy)
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
#' names a template as `template: name: tflspec_v1/demog`.
#'
#' These folders are the sources (the company's and tflspec's own).  A
#' study does not read them: it copies the `series.yml` and the templates
#' its reports use into `<study>/spec/series/<series>/`
#' ([tfl_copy_templates()]) and reads its copy, and it changes a value in
#' its copy's `values:` ([tfl_series_values_set()]).
#'
#' `tfl_series_dir()` finds the source folder of a series.  It looks in `dir`, then
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
#' @seealso [tfl_series_list()], [tfl_copy_templates()]
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
  hit <- .series_find(series, dir)
  if (!is.null(hit)) return(hit$path)
  dirs <- .series_search_dirs(dir)
  dirs <- dirs[nzchar(dirs)]
  stop("tfl_series_dir(): no series \"", series, "\" (no ", series, "/series.yml in ",
       if (length(dirs)) paste(dirs, collapse = ", ") else "any folder", ").", call. = FALSE)
}

# The source folder of a series and where it is from: list(path, source)
# with source "dir", "option", "user" or "package"; NULL when none has it.
.series_find <- function(series, dir = NULL) {
  opt <- getOption("tflspec.series_dir")
  src <- c(rep("dir", length(dir)), rep("option", length(opt)), "user", "package")
  dirs <- .series_search_dirs(dir)
  for (i in seq_along(dirs)) {
    d <- dirs[[i]]
    if (!nzchar(d)) next
    if (file.exists(file.path(d, series, "series.yml"))) {
      return(list(path = normalizePath(file.path(d, series), winslash = "/", mustWork = TRUE),
                  source = src[[i]]))
    }
  }
  NULL
}

#' @rdname tfl_series_dir
#' @export
tfl_series_read <- function(series, dir = NULL) {
  .series_check_name(series, "tfl_series_read()")
  .series_read_at(tfl_series_dir(series, dir = dir), series, "tfl_series_read()")
}

# a series's series.yml read from the folder `path` (a source's, or a
# study's copy), as tfl_series_read() gives it
.series_read_at <- function(path, series, fn) {
  file <- file.path(path, "series.yml")
  y <- .yaml_read_file(file, handlers = .yaml_handlers())
  if (is.null(y)) y <- list()
  if (!.series_is_map(y)) stop(fn, ": ", file, " is not a map.", call. = FALSE)
  if (!is.null(y$name) && !identical(y$name, series)) {
    stop(fn, ": series \"", series, "\": its series.yml says name: ", y$name,
         " (the name is the folder's).", call. = FALSE)
  }
  values <- y$values %||% list()
  if (!.series_is_map(values) && length(values)) {
    stop(fn, ": series \"", series, "\": values: is a map ({key: value}).", call. = FALSE)
  }
  docs <- y$descriptions %||% list()
  if (length(docs) && (!.series_is_map(docs) ||
                       !all(vapply(docs, function(d) is.character(d) && length(d) == 1L && !is.na(d),
                                   logical(1))))) {
    stop(fn, ": series \"", series, "\": descriptions: is a map ({key: text}), ",
         "one text a key of values:.", call. = FALSE)
  }
  docs <- if (length(docs)) unlist(docs) else stats::setNames(character(), character())
  structure(list(name = series, label = y$label, released = y$released, needs = y$needs,
                 fixes = y$fixes %||% list(), values = if (length(values)) values else list(),
                 descriptions = docs,
                 path = normalizePath(path, winslash = "/", mustWork = TRUE),
                 file = normalizePath(file, winslash = "/", mustWork = TRUE)),
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
#   series    a tfl_series (the study's copy, or a source's), or a series's name
#   study     the parsed study.yml (settings:), or NULL
#   template  the template's name, for the messages
#   origin    the origin of an untagged value ("template", or "report" for a
#             report SPEC)
# The result has attribute "origins": a named character vector, by the
# dotted path of each value (a list's element without a name by its
# position), of one of .series_origins: "report" / "template" (untagged),
# "series" (!series, from the series.yml read: the study's copy),
# "study_settings" (!study, the study's settings:), "fallback" (!study, the
# study has no value: unset), "fixed".  A `!fixed` value also carries
# attribute "fixed" = TRUE and "origin" = "fixed".
.series_resolve <- function(x, series, study = NULL, template = NULL, origin = "template") {
  if (is.character(series)) series <- tfl_series_read(series)
  if (!inherits(series, "tfl_series")) {
    stop(".series_resolve(): `series` is a tfl_series or a series's name.", call. = FALSE)
  }
  sname <- series$name
  where <- if (is.null(template)) paste0("series ", sname) else paste0("template ", template)
  origins <- character()
  note <- function(path, o) origins[[if (length(path)) paste(path, collapse = ".") else "."]] <<- o
  lookup <- function(key) {
    if (key %in% names(series$values)) return(series$values[[key]])
    stop(where, ": !series ", key, ": no value ", key, " in series ", sname,
         " (", series$file, " values:).", call. = FALSE)
  }
  walk <- function(v, path, o) {
    if (inherits(v, "tfl_series_ref")) {
      val <- lookup(v$key)
      note(path, "series")
      return(val)
    }
    if (inherits(v, "tfl_study_ref")) {
      val <- .series_study_value(study, v$key)
      note(path, if (inherits(val, "tfl_unset")) "fallback" else "study_settings")
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

# Where a resolved value comes from (the words of .series_resolve()'s and
# tfl_template_resolve()'s "origins", and tfl_template_values()'s source)
.series_origins <- c("report", "template", "series", "study_settings", "fallback", "fixed")

# study.yml settings: at a dotted key path; tfl_unset() if it is missing
.series_study_value <- function(study, key) {
  v <- study$settings
  for (k in strsplit(key, ".", fixed = TRUE)[[1L]]) {
    if (!is.list(v) || !k %in% names(v)) return(tfl_unset())
    v <- v[[k]]
  }
  v
}

# study.yml `series_settings:` is no longer read: a study changes a series's
# values in its copy of series.yml.  Stops naming each series in it.
# `study` is study.yml read as a list; `study_dir` the study folder, or NULL.
.series_settings_gone <- function(study, study_dir = NULL) {
  if (is.null(study) || !is.list(study) || is.null(study$series_settings)) return(invisible())
  ss <- study$series_settings
  where <- if (is.null(study_dir)) "<study>" else study_dir
  sn <- if (.series_is_map(ss)) names(ss) else "<series>"
  to <- vapply(sn, function(s) {
    paste0(s, ": move these values into ", where, "/spec/series/", s,
           "/series.yml values: (tfl_series_values_set()), after tfl_copy_templates()")
  }, "")
  stop("study.yml series_settings: is no longer read. ", paste(to, collapse = "; "), ".",
       call. = FALSE)
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

# ---- the templates the reports use -----------------------------------------

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
