# A study's own copy of the templates it uses (canon 2026-11-19).
#
#   <study>/spec/series/<series>/series.yml            a whole copy; the study
#                                                       changes values: here
#   <study>/spec/series/<series>/templates/<name>.yml  a whole copy of each
#                                                       template the reports use
#   <study>/spec/series/<series>/copied.yml            written by tflspec only:
#                                                       one entry a copied file
#
# copied.yml, by the file's path in the series folder:
#
#   templates/demog.yml:
#     from: {folder: <source>/tflspec_v1, source: package, version: 0.1.0,
#            template: demog}
#     at: 2026-11-19 10:00:00
#     md5: 3f9c1a...
#
# `from:` is the source folder, where it was found ("package", "user", "dir"
# or "option"), tflspec's version for the package's own series, and, for a
# template, the source template's name (another one for a copy made with
# `as`).  `md5` is the file's when it was copied.  When a study is given, a
# series is read only from its copy (tfl_template_resolve()).

# The folder of a study's copied series: <study>/spec/series (a spec folder
# given as the study, with defaults.yml or outputs/ in it: its series/)
.study_series_root <- function(study_dir) {
  spec <- file.path(study_dir, "spec")
  if (!dir.exists(spec) && (file.exists(file.path(study_dir, "defaults.yml")) ||
                            dir.exists(file.path(study_dir, "outputs")))) {
    spec <- study_dir
  }
  file.path(spec, "series")
}

.study_check_dir <- function(study_dir, fn) {
  if (!is.character(study_dir) || length(study_dir) != 1L || is.na(study_dir) ||
      !dir.exists(study_dir)) {
    stop(fn, ": no study folder ", paste(as.character(study_dir), collapse = ", "), ".",
         call. = FALSE)
  }
  normalizePath(study_dir, winslash = "/", mustWork = TRUE)
}

# "<series>/<name>" -> c(series, template), checked
.study_tpl_id <- function(id, fn) {
  if (!is.character(id) || length(id) != 1L || is.na(id) || !grepl("^[^/]+/[^/]+$", id)) {
    stop(fn, ": template ", encodeString(as.character(id)[1L], quote = "\""),
         ": write <series>/<name>, e.g. \"tflspec_v1/demog\".", call. = FALSE)
  }
  s <- sub("/.*$", "", id)
  t <- sub("^.*/", "", id)
  .series_check_name(s, fn)
  if (!grepl(.series_name_re, t)) {
    stop(fn, ": template \"", id, "\": a template's name is lower-case letters, digits and _, ",
         "starting with a letter.", call. = FALSE)
  }
  c(series = s, template = t)
}

# the next step for a template the study has not copied
.study_copy_hint <- function(study_dir, id) {
  sprintf("tfl_copy_templates(\"%s\", \"%s\")", study_dir, id)
}

# ---- copied.yml ---------------------------------------------------------------

.copied_path <- function(root, series) file.path(root, series, "copied.yml")

.copied_read <- function(root, series) {
  f <- .copied_path(root, series)
  if (!file.exists(f)) return(list())
  y <- yaml::read_yaml(f)
  if (is.null(y) || !length(y)) list() else y
}

.copied_write <- function(root, series, entries) {
  f <- .copied_path(root, series)
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  body <- if (length(entries)) yaml::as.yaml(entries[order(names(entries))]) else "{}\n"
  con <- file(f, open = "wb")
  on.exit(close(con), add = TRUE)
  writeLines(c("# copied.yml: written by tflspec (tfl_copy_templates(), tfl_recopy_templates());",
               "# do not edit by hand.", sub("\n$", "", body)), con, sep = "\n")
  invisible(f)
}

# the source file an entry was copied from (NA when the entry has none)
.copied_from_file <- function(entry, key) {
  folder <- entry$from$folder
  if (is.null(folder)) return(NA_character_)
  if (identical(key, "series.yml")) return(file.path(folder, "series.yml"))
  file.path(folder, "templates", paste0(entry$from$template %||% .copied_key_name(key), ".yml"))
}

# "templates/demog.yml" -> "demog"
.copied_key_name <- function(key) sub("^templates/(.*)[.]yml$", "\\1", key)

# The source of a study's series now: `dir` first, then the folder it was
# copied from when that was a folder given (source "dir"), then the
# option, tflplanner's home and the package (.series_find()).
.study_source <- function(series, entries, dir = NULL) {
  e <- entries[["series.yml"]]
  rec <- if (identical(e$from$source, "dir") && !is.null(e$from$folder)) dirname(e$from$folder)
  .series_find(series, unique(c(dir, rec)))
}

# copies `from` to `to` (over it) and gives the copy's copied.yml entry
.copied_copy <- function(from, to, src, template = NULL) {
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  if (!isTRUE(file.copy(from, to, overwrite = TRUE, copy.date = FALSE))) {
    stop("cannot copy ", from, " to ", to, ".", call. = FALSE)
  }
  fr <- list(folder = src$path, source = src$source)
  if (identical(src$source, "package")) fr$version <- as.character(utils::packageVersion("tflspec"))
  if (!is.null(template)) fr$template <- template
  list(from = fr, at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), md5 = .series_md5(to))
}

# Keeps the study's file `to` before `from` is copied over it: in
# <spec>/.backup/<yyyymmdd-HHMMSS>/<its path under spec/> (tflplanner's
# naming; a second one in the same second gets -2, -3, ...), so a later
# backup never replaces an earlier one.  Gives the kept file's path, or NULL
# when there is no `to` or its content is the same as `from`'s.
.copied_backup <- function(from, to, spec_dir) {
  if (!file.exists(to) || identical(.series_md5(to), .series_md5(from))) return(NULL)
  rel <- substr(to, nchar(spec_dir) + 2L, nchar(to))
  base <- file.path(spec_dir, ".backup", format(Sys.time(), "%Y%m%d-%H%M%S"))
  dir <- base
  n <- 1L
  while (file.exists(file.path(dir, rel))) {
    n <- n + 1L
    dir <- paste0(base, "-", n)
  }
  kept <- file.path(dir, rel)
  dir.create(dirname(kept), recursive = TRUE, showWarnings = FALSE)
  if (!isTRUE(file.copy(to, kept, copy.date = TRUE))) {
    stop("cannot keep ", to, " in ", kept, " before copying over it.", call. = FALSE)
  }
  kept
}

.copied_row <- function(series, template, file, from, md5, status) {
  data.frame(series = series, template = template, file = file, from = from, md5 = md5,
             status = status, stringsAsFactors = FALSE)
}

.copied_rows <- function(rows) {
  if (length(rows)) return(do.call(rbind, rows))
  data.frame(series = character(), template = character(), file = character(),
             from = character(), md5 = character(), status = character(),
             stringsAsFactors = FALSE)
}

# a copied file's state against its entry and its source file
.copied_state <- function(copy, source, md5) {
  if (is.na(source) || !file.exists(source)) return("source_missing")
  now <- if (file.exists(copy)) .series_md5(copy) else NA_character_
  study <- !identical(now, md5)
  src <- !identical(.series_md5(source), md5)
  if (study && src) "both_changed" else if (study) "study_changed" else if (src) "source_changed" else "same"
}

#' Copy the templates a study uses into the study
#'
#' A study reads its templates from its own copy, never from the source
#' folders (the company's series and tflspec's own, [tfl_series_dir()]).
#' `tfl_copy_templates()` copies a series's `series.yml` (once) and each
#' template asked for, whole, into
#' `<study_dir>/spec/series/<series>/series.yml` and
#' `<study_dir>/spec/series/<series>/templates/<name>.yml`, and records in
#' `<study_dir>/spec/series/<series>/copied.yml` (written by tflspec only)
#' where each file came from (the source folder, `package`, `user`, `dir`
#' or `option`, and tflspec's version for the package's series), when, and
#' its md5.  It never overwrites a file the study has.
#'
#' The study changes a template by editing its copy, and a series's values
#' in its copy's `values:` ([tfl_series_values_set()]).  A report SPEC
#' still names a template as `template: name: tflspec_v1/demog`; a report
#' that names a template the study has not copied stops, with the call
#' that copies it.  With `as`, a template is copied under a new name: a
#' template of this study only (`copied.yml` keeps the original's name).
#'
#' When a source changes, `tfl_template_status()` lists each copied file's
#' state, `tfl_template_diff()` shows a copy's lines against its source,
#' and `tfl_recopy_templates()` copies the chosen templates again (the
#' study's own edits of them are lost, with a warning).
#'
#' @param study_dir The study folder (holds `study.yml` and `spec/`).
#' @param templates Template ids, `"<series>/<name>"`.  `NULL`
#'   (`tfl_copy_templates()`): every template the report SPECs
#'   (`spec/outputs/*.yml`) name that the study has not copied yet.
#' @param as A new name for the copy (one template only): a template of this
#'   study only.
#' @param dir A folder (or folders) of series, looked in first for the
#'   source, as [tfl_series_dir()] does.
#' @param template One template id, `"<series>/<name>"`.
#' @return `tfl_copy_templates()` and `tfl_recopy_templates()`: a data
#'   frame, one row a file -- `series`, `template` (the copy's name; `NA`
#'   for `series.yml`), `file` (the study's copy), `from` (the source file),
#'   `md5` (the copy's) and `status` (`"copied"` or `"present"`, already in
#'   the study and left as it is; `"recopied"`).
#'
#'   `tfl_template_status()`: a data frame, one row a copied file --
#'   `series`, `template` (`NA` for `series.yml`), `file`, `from`,
#'   `copied_at` and `status`: `"same"`, `"source_changed"` (the source's
#'   md5 differs from `copied.yml`), `"study_changed"` (the copy's md5
#'   differs from `copied.yml`), `"both_changed"` or `"source_missing"`.
#'
#'   `tfl_template_diff()`: the lines of the copy against the current
#'   source, in unified style: `"--- copy"`, `"+++ source"`, then each line
#'   starting with `" "` (in both), `"-"` (the copy's only) or `"+"` (the
#'   source's only).
#' @seealso [tfl_series_values_set()], [tfl_template_resolve()],
#'   [tfl_series_list()]
#' @examples
#' study <- tempfile("study")
#' dir.create(file.path(study, "spec", "outputs"), recursive = TRUE)
#' writeLines(c("id: T-1", "kind: table", "template:", "  name: tflspec_v1/demog",
#'              "  analysis: {by: TRT01A, continuous: [AGE]}"),
#'            file.path(study, "spec", "outputs", "T-1.yml"))
#' tfl_copy_templates(study)[, c("series", "template", "status")]
#' tfl_copy_templates(study, "tflspec_v1/demog", as = "demog_wide")$status
#' tfl_template_status(study)[, c("template", "status")]
#'
#' # the study edits its copy; the copy now differs from the source
#' f <- file.path(study, "spec", "series", "tflspec_v1", "templates", "demog.yml")
#' writeLines(c(readLines(f), "# the study's note"), f)
#' tfl_template_status(study)$status
#' tail(tfl_template_diff(study, "tflspec_v1/demog"), 2)
#' suppressWarnings(tfl_recopy_templates(study, "tflspec_v1/demog"))$status
#' unlink(study, recursive = TRUE)
#' @export
tfl_copy_templates <- function(study_dir, templates = NULL, as = NULL, dir = NULL) {
  fn <- "tfl_copy_templates()"
  study_dir <- .study_check_dir(study_dir, fn)
  root <- .study_series_root(study_dir)
  if (is.null(templates)) {
    reports <- list.files(file.path(.spec_dir(study_dir), "outputs"), "\\.ya?ml$",
                          full.names = TRUE, recursive = TRUE)
    used <- .series_templates_used(reports)
    templates <- if (nrow(used)) paste0(used$series, "/", used$template) else character()
  }
  if (!is.character(templates)) stop(fn, ": `templates` is template ids, \"<series>/<name>\".", call. = FALSE)
  templates <- unique(templates)
  if (!is.null(as)) {
    if (length(templates) != 1L) {
      stop(fn, ": `as` names the copy of one template; give one in `templates`.", call. = FALSE)
    }
    if (!is.character(as) || length(as) != 1L || is.na(as) || !grepl(.series_name_re, as)) {
      stop(fn, ": `as` is a template's name: lower-case letters, digits and _, starting with a ",
           "letter.", call. = FALSE)
    }
  }
  rows <- list()
  seen <- character()
  for (id in templates) {
    p <- .study_tpl_id(id, fn)
    s <- p[["series"]]
    t <- p[["template"]]
    to <- as %||% t
    entries <- .copied_read(root, s)
    src <- NULL
    source_of <- function() {
      if (is.null(src)) src <<- .study_source(s, entries, dir)
      if (is.null(src)) {
        stop(fn, ": no series \"", s, "\" in the series folders (", s, "/series.yml in ",
             paste(.series_search_dirs(dir), collapse = ", "), ").", call. = FALSE)
      }
      src
    }
    if (!s %in% seen) {
      seen <- c(seen, s)
      dest <- file.path(root, s, "series.yml")
      if (file.exists(dest)) {
        e <- entries[["series.yml"]]
        rows[[length(rows) + 1L]] <- .copied_row(s, NA_character_, dest, .copied_from_file(e, "series.yml"),
                                                 .series_md5(dest), "present")
      } else {
        sc <- source_of()
        from <- file.path(sc$path, "series.yml")
        entries[["series.yml"]] <- .copied_copy(from, dest, sc)
        rows[[length(rows) + 1L]] <- .copied_row(s, NA_character_, dest, from, entries[["series.yml"]]$md5,
                                                 "copied")
      }
    }
    key <- paste0("templates/", to, ".yml")
    dest <- file.path(root, s, key)
    if (file.exists(dest)) {
      rows[[length(rows) + 1L]] <- .copied_row(s, to, dest, .copied_from_file(entries[[key]], key),
                                               .series_md5(dest), "present")
    } else {
      sc <- source_of()
      from <- file.path(sc$path, "templates", paste0(t, ".yml"))
      if (!file.exists(from)) {
        stop(fn, ": no template ", s, "/", t, " in series ", s, " (no ", from, ").", call. = FALSE)
      }
      entries[[key]] <- .copied_copy(from, dest, sc, template = t)
      rows[[length(rows) + 1L]] <- .copied_row(s, to, dest, from, entries[[key]]$md5, "copied")
    }
    .copied_write(root, s, entries)
  }
  out <- .copied_rows(rows)
  if (nrow(out)) out$file <- normalizePath(out$file, winslash = "/", mustWork = TRUE)
  out
}

#' @rdname tfl_copy_templates
#' @export
tfl_template_status <- function(study_dir, dir = NULL) {
  fn <- "tfl_template_status()"
  study_dir <- .study_check_dir(study_dir, fn)
  root <- .study_series_root(study_dir)
  rows <- list()
  series <- if (dir.exists(root)) sort(list.dirs(root, full.names = FALSE, recursive = FALSE)) else character()
  for (s in series) {
    entries <- .copied_read(root, s)
    if (!length(entries)) next
    src <- .study_source(s, entries, dir)
    keys <- names(entries)
    keys <- c(intersect("series.yml", keys), sort(setdiff(keys, "series.yml")))
    for (key in keys) {
      e <- entries[[key]]
      copy <- file.path(root, s, key)
      is_series <- identical(key, "series.yml")
      source <- if (is.null(src)) NA_character_ else if (is_series) file.path(src$path, "series.yml") else
        file.path(src$path, "templates", paste0(e$from$template %||% .copied_key_name(key), ".yml"))
      rows[[length(rows) + 1L]] <- data.frame(
        series = s, template = if (is_series) NA_character_ else .copied_key_name(key),
        file = normalizePath(copy, winslash = "/", mustWork = FALSE),
        from = .copied_from_file(e, key), copied_at = as.character(e$at %||% NA_character_),
        status = .copied_state(copy, source, as.character(e$md5 %||% NA_character_)),
        stringsAsFactors = FALSE)
    }
  }
  if (length(rows)) return(do.call(rbind, rows))
  data.frame(series = character(), template = character(), file = character(),
             from = character(), copied_at = character(), status = character(),
             stringsAsFactors = FALSE)
}

# a copied template: its series, name, copy, entries and current source file
.study_copied_tpl <- function(study_dir, template, fn) {
  p <- .study_tpl_id(template, fn)
  root <- .study_series_root(study_dir)
  key <- paste0("templates/", p[["template"]], ".yml")
  entries <- .copied_read(root, p[["series"]])
  copy <- file.path(root, p[["series"]], key)
  if (is.null(entries[[key]]) || !file.exists(copy)) {
    stop(fn, ": template ", template, " is not copied into the study: ",
         .study_copy_hint(study_dir, template), ".", call. = FALSE)
  }
  src <- .study_source(p[["series"]], entries)
  source <- if (is.null(src)) NA_character_ else
    file.path(src$path, "templates", paste0(entries[[key]]$from$template %||% p[["template"]], ".yml"))
  if (is.na(source) || !file.exists(source)) {
    stop(fn, ": the source of template ", template, " is missing (",
         .copied_from_file(entries[[key]], key), ").", call. = FALSE)
  }
  list(series = p[["series"]], template = p[["template"]], key = key, root = root,
       entries = entries, copy = copy, source = source, src = src)
}

#' @rdname tfl_copy_templates
#' @export
tfl_template_diff <- function(study_dir, template) {
  fn <- "tfl_template_diff()"
  study_dir <- .study_check_dir(study_dir, fn)
  x <- .study_copied_tpl(study_dir, template, fn)
  c("--- copy", "+++ source",
    .line_diff(readLines(x$copy, warn = FALSE, encoding = "UTF-8"),
               readLines(x$source, warn = FALSE, encoding = "UTF-8")))
}

# The lines of `a` against `b` by their longest common subsequence: each
# line with " " (in both), "-" (in a only) or "+" (in b only).
.line_diff <- function(a, b) {
  n <- length(a)
  m <- length(b)
  lcs <- matrix(0L, n + 1L, m + 1L)
  for (i in rev(seq_len(n))) {
    for (j in rev(seq_len(m))) {
      lcs[i, j] <- if (identical(a[[i]], b[[j]])) lcs[i + 1L, j + 1L] + 1L else
        max(lcs[i + 1L, j], lcs[i, j + 1L])
    }
  }
  out <- character(n + m)
  k <- 0L
  i <- 1L
  j <- 1L
  while (i <= n || j <= m) {
    k <- k + 1L
    if (i <= n && j <= m && identical(a[[i]], b[[j]])) {
      out[[k]] <- paste0(" ", a[[i]])
      i <- i + 1L
      j <- j + 1L
    } else if (j > m || (i <= n && lcs[i + 1L, j] >= lcs[i, j + 1L])) {
      out[[k]] <- paste0("-", a[[i]])
      i <- i + 1L
    } else {
      out[[k]] <- paste0("+", b[[j]])
      j <- j + 1L
    }
  }
  out[seq_len(k)]
}

#' @rdname tfl_copy_templates
#' @export
tfl_recopy_templates <- function(study_dir, templates) {
  fn <- "tfl_recopy_templates()"
  study_dir <- .study_check_dir(study_dir, fn)
  if (!is.character(templates) || !length(templates)) {
    stop(fn, ": `templates` is template ids, \"<series>/<name>\".", call. = FALSE)
  }
  rows <- list()
  for (id in unique(templates)) {
    x <- .study_copied_tpl(study_dir, id, fn)
    e <- x$entries[[x$key]]
    st <- .copied_state(x$copy, x$source, as.character(e$md5 %||% NA_character_))
    kept <- .copied_backup(x$source, x$copy, dirname(x$root))
    if (st %in% c("study_changed", "both_changed") && !is.null(kept)) {
      warning(fn, ": ", id, ": the study's edits of ", x$copy, " are lost (copied again from ",
              x$source, "); the file as it was is kept in ", kept, ".", call. = FALSE)
    }
    entries <- x$entries
    entries[[x$key]] <- .copied_copy(x$source, x$copy, x$src, template = e$from$template %||% x$template)
    .copied_write(x$root, x$series, entries)
    rows[[length(rows) + 1L]] <- .copied_row(x$series, x$template,
                                             normalizePath(x$copy, winslash = "/", mustWork = TRUE),
                                             x$source, entries[[x$key]]$md5, "recopied")
  }
  .copied_rows(rows)
}

#' Change a series's values in a study's copy
#'
#' `tfl_series_values_set()` writes `values:` in the study's copy of a
#' series's `series.yml` (`<study_dir>/spec/series/<series>/series.yml`,
#' made by [tfl_copy_templates()]): each value given is set, and a `NULL`
#' value removes its key.  The rest of the file is kept as it is, its
#' comments too (a `values:` written in flow style, `values: {...}`, is
#' written again in block style).  A template's `!series K` reads the
#' study's value from there.
#'
#' @param study_dir The study folder (holds `study.yml` and `spec/`).
#' @param series The series's name.
#' @param values A named list: the values to set; `NULL` for a key removes
#'   it.
#' @return The `series.yml` path, invisibly.
#' @seealso [tfl_copy_templates()], [tfl_series_read()]
#' @examples
#' study <- tempfile("study")
#' dir.create(study)
#' invisible(tfl_copy_templates(study, "tflspec_v1/ae_soc_pt"))
#' tfl_series_values_set(study, "tflspec_v1",
#'                       list(any_row_label = "Subjects with at least one TEAE"))
#' f <- file.path(study, "spec", "series", "tflspec_v1", "series.yml")
#' yaml::read_yaml(f)$values$any_row_label
#' unlink(study, recursive = TRUE)
#' @export
tfl_series_values_set <- function(study_dir, series, values) {
  fn <- "tfl_series_values_set()"
  study_dir <- .study_check_dir(study_dir, fn)
  .series_check_name(series, fn)
  f <- file.path(.study_series_root(study_dir), series, "series.yml")
  if (!file.exists(f)) {
    stop(fn, ": series ", series, " is not copied into the study (no ", f, "). Copy a template ",
         "of it first: ", .study_copy_hint(study_dir, paste0(series, "/<name>")), ".", call. = FALSE)
  }
  if (!is.list(values) || (length(values) && (is.null(names(values)) || any(!nzchar(names(values)))))) {
    stop(fn, ": `values` is a named list.", call. = FALSE)
  }
  raw <- readBin(f, "raw", file.size(f))
  crlf <- any(raw == as.raw(13L))
  lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
  lines <- .values_block_set(lines, values)
  con <- file(f, open = "wb")
  writeLines(enc2utf8(lines), con, sep = if (crlf) "\r\n" else "\n")
  close(con)
  .series_read_at(dirname(f), series, fn)
  invisible(f)
}

# The lines of a series.yml with `values` set in its `values:` block (a
# NULL removes the key); the other lines as they are.
.values_block_set <- function(lines, values) {
  as_lines <- function(k, v, ind) {
    y <- yaml::as.yaml(stats::setNames(list(v), k))
    paste0(ind, strsplit(sub("\n$", "", y), "\n", fixed = TRUE)[[1L]])
  }
  at <- grep("^values:", lines)
  if (!length(at)) {
    keep <- values[!vapply(values, is.null, NA)]
    if (!length(keep)) return(lines)
    return(c(lines, "values:", unlist(lapply(names(keep), function(k) as_lines(k, keep[[k]], "  ")))))
  }
  at <- at[1L]
  rest <- sub("[[:space:]]*#.*$", "", sub("^values:[[:space:]]*", "", lines[[at]]))
  if (nzchar(rest)) {
    # flow style: the block written again from the values read
    cur <- yaml::yaml.load(paste(lines, collapse = "\n"), handlers = .yaml_handlers())$values %||% list()
    for (k in names(values)) cur[k] <- list(values[[k]])
    cur <- cur[!vapply(cur, is.null, NA)]
    new <- c("values:", unlist(lapply(names(cur), function(k) as_lines(k, cur[[k]], "  "))))
    return(append(lines[-at], new, after = at - 1L))
  }
  # the block: the lines after `values:` up to the next top-level line
  block_end <- function(lines) {
    top <- which(grepl("^[^[:space:]]", lines) & seq_along(lines) > at)
    if (length(top)) top[[1L]] - 1L else length(lines)
  }
  indent_of <- function(x) nchar(sub("^( *).*$", "\\1", x))
  end <- block_end(lines)
  body <- if (end > at) lines[(at + 1L):end] else character()
  firsts <- body[grepl("^ +[^ #]", body)]
  ind <- if (length(firsts)) strrep(" ", indent_of(firsts[[1L]])) else "  "
  for (k in names(values)) {
    end <- block_end(lines)
    idx <- if (end > at) (at + 1L):end else integer()
    hit <- idx[startsWith(lines[idx], paste0(ind, k, ":"))]
    new <- if (!is.null(values[[k]])) as_lines(k, values[[k]], ind)
    if (length(hit)) {
      from <- hit[[1L]]
      after <- idx[idx > from & nzchar(trimws(lines[idx])) & indent_of(lines[idx]) <= nchar(ind)]
      to <- if (length(after)) after[[1L]] - 1L else end
      # (blank lines at the key's end stay)
      while (to > from && !nzchar(trimws(lines[[to]]))) to <- to - 1L
      lines <- append(lines[-(from:to)], new, after = from - 1L)
    } else if (length(new)) {
      filled <- idx[nzchar(trimws(lines[idx]))]
      last <- if (length(filled)) max(filled) else at
      lines <- append(lines, new, after = last)
    }
  }
  lines
}
