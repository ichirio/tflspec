# ai/prepare.yml: the settings of the last tfl_ai_prepare() call that wrote the
# requests, kept with the study; tfl_ai_prepare(dir) repeats them.

# The arguments kept, in the file's order.  Not kept: dir, overwrite, dry_run,
# quiet, progress and add (they describe the call, not the settings),
# adam_summary (an R object; `datasets` and the ADaM say it again) and
# numbers (a data frame; ai/sheet_numbers.csv keeps the result).
.ai_prepare_keys <- c("toc", "toc_sheet", "sheets", "shells", "toc_cols", "sheet_roles",
                      "adam", "adam_spec", "define", "docs",
                      "study_id", "title", "size", "language", "workbook", "max_chars",
                      "unit", "templates", "area", "datasets", "max_levels",
                      "agent_reads", "agent_max_reads")
# paths (inside the study: from its folder)
.ai_prepare_paths <- c("toc", "shells", "adam", "adam_spec", "define", "docs")
# the ones that are a list in the file, whatever their length
.ai_prepare_lists <- c("sheets", "shells", "docs", "datasets", "agent_reads")
# the workbook's own: with `add`, the file keeps the first call's
.ai_prepare_workbook <- c("toc", "toc_sheet", "sheets", "shells", "toc_cols", "sheet_roles", "unit")
.ai_prepare_version <- 1L

.ai_prepare_file <- function(dir) file.path(dir, "ai", "prepare.yml")

# A value as yaml keeps it, else NULL: text, numbers, TRUE / FALSE, a vector
# of them (named: a map), a list of those.
.ai_prepare_plain <- function(x) {
  ok1 <- function(v) is.atomic(v) && !is.factor(v) && !is.null(v) &&
    length(setdiff(names(attributes(v)), "names")) == 0L
  if (is.list(x) && !is.data.frame(x)) return(all(vapply(x, ok1, NA)))
  ok1(x)
}

# A path as the file says it: from the study folder when inside it
# (forward slashes), else absolute.  `adam` is from `dir` when relative.
.ai_prepare_path_out <- function(dir, p, rel_to_dir = FALSE) {
  p <- as.character(p)
  if (rel_to_dir) {
    abs <- grepl("^(/|[A-Za-z]:|~)", p)
    p <- ifelse(abs, p, file.path(dir, p))
  }
  ifelse(is.na(p), NA_character_, .ai_from_study(dir, p))
}

# A path of the file as it is used: absolute, a relative one from `dir`
.ai_prepare_path_in <- function(dir, p) {
  p <- as.character(p)
  abs <- grepl("^(/|[A-Za-z]:|~)", p)
  p[!abs] <- file.path(dir, p[!abs])
  normalizePath(p, winslash = "/", mustWork = FALSE)
}

# The given arguments in the file's form: a named list, and what was left out
.ai_prepare_out <- function(dir, given) {
  out <- list()
  left <- character()
  for (k in .ai_prepare_keys) {
    if (!k %in% names(given)) next
    v <- given[[k]]
    if (is.null(v)) next
    if (!.ai_prepare_plain(v)) {
      left <- c(left, k)
      next
    }
    if (k %in% .ai_prepare_paths) {
      v <- .ai_prepare_path_out(dir, v, rel_to_dir = identical(k, "adam"))
    }
    if (k %in% c("size", "max_chars", "max_levels", "agent_max_reads") && is.numeric(v) &&
        all(!is.na(v) & v == round(v))) v <- as.integer(v)
    if (identical(k, "toc_cols") && is.character(v) && !is.null(names(v))) {
      nm <- unique(names(v))
      v <- stats::setNames(lapply(nm, function(n) unname(v[names(v) == n])), nm)
      v <- lapply(v, function(e) if (length(e) == 1L) e else as.list(e))
    } else if (k %in% c("sheet_roles") && is.character(v)) {
      v <- as.list(v)
    } else if (is.character(v) && k %in% .ai_prepare_lists) {
      v <- as.list(v)
    } else if (is.list(v)) {
      v <- lapply(v, function(e) if (length(e) == 1L) e else as.list(e))
    }
    out[[k]] <- v
  }
  list(settings = out, left = left)
}

# Write ai/prepare.yml after a call that wrote the requests.  With `add`,
# merged into the file there: the workbook's own settings (toc, toc_sheet,
# sheets, shells, toc_cols, sheet_roles, unit) stay the first call's, as the
# study's first workbook; the others come from this call when given, `docs`,
# `agent_reads` and `datasets` the union (in order); with no file, nothing is
# written (a repeat would then take an added workbook for the study's).
.ai_prepare_write <- function(dir, given, add = FALSE) {
  f <- .ai_prepare_file(dir)
  new <- .ai_prepare_out(dir, given)
  set <- new$settings
  if (isTRUE(add)) {
    if (!file.exists(f)) return(invisible(NULL))
    old <- tryCatch(yaml::read_yaml(f), error = function(e) NULL)
    if (is.null(old)) return(invisible(NULL))
    for (k in .ai_prepare_workbook) set[[k]] <- NULL
    for (k in c("docs", "agent_reads", "datasets")) {
      if (!is.null(set[[k]]) && !is.null(old[[k]])) {
        set[[k]] <- as.list(unique(c(unlist(old[[k]]), unlist(set[[k]]))))
      }
    }
    keep <- setdiff(names(old), c("version", "written", "tflspec", names(set)))
    set <- c(old[keep], set)
    set <- set[intersect(.ai_prepare_keys, names(set))]
  }
  head <- list(version = .ai_prepare_version,
               written = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
               tflspec = as.character(utils::packageVersion("tflspec")))
  quote_chr <- function(x) {
    attr(x, "quoted") <- TRUE
    x
  }
  body <- yaml::as.yaml(c(head, set), handlers = list(character = quote_chr))
  lines <- c(
    "# The settings of the last tfl_ai_prepare() call that wrote this study's requests.",
    "# tfl_ai_prepare(dir) with no other argument repeats them (tfl_ai_prepare_settings()",
    "# reads this file); the arguments of a call win over them.",
    "# A path inside the study is from its folder (forward slashes); one outside is absolute.",
    "# A key that is missing is the argument's default.",
    "# Not kept: dir, overwrite, dry_run, quiet, progress, add (they describe the call, not",
    "# the settings), adam_summary (an R object) and numbers (ai/sheet_numbers.csv holds them).",
    if (length(new$left)) paste0("# Left out, not plain text: ", paste(new$left, collapse = ", "), "."),
    if (isTRUE(add)) paste0("# Added to the study (add = TRUE): the workbook's own keys (toc, toc_sheet, sheets, ",
                            "shells, toc_cols, sheet_roles, unit) are the first call's."),
    strsplit(body, "\n", fixed = TRUE)[[1L]])
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  .ai_write_text(lines, f)
  invisible(f)
}

# The arguments a call was given by name, evaluated: those the file keeps
.ai_prepare_given <- function(call, env) {
  nm <- intersect(names(call)[-1L], .ai_prepare_keys)
  stats::setNames(lapply(nm, function(n) get(n, envir = env)), nm)
}

#' The settings `tfl_ai_prepare()` kept with a study
#'
#' Reads `ai/prepare.yml` of the study folder: the arguments of the last
#' [tfl_ai_prepare()] call that wrote the requests, as that function takes
#' them, for [tfl_ai_prepare()] (`tfl_ai_prepare(dir)` repeats them) and
#' for a screen that fills its fields from them.
#'
#' The file is plain YAML, keys in a fixed order, strings quoted, the
#' `version:` of the format first (1), then `written:` (the date-time) and
#' `tflspec:` (the version that wrote it), then the settings under the
#' argument names of [tfl_ai_prepare()]:
#'
#' * paths -- `toc`, `shells`, `adam`, `adam_spec`, `define`, `docs`: from
#'   the study folder, with forward slashes, when inside it; else as given
#'   (absolute);
#' * `toc_sheet`, `sheets`, `toc_cols`, `sheet_roles`, `study_id`, `title`,
#'   `size`, `language`, `workbook`, `max_chars`, `unit`, `templates`,
#'   `area`, `datasets`, `max_levels`, `agent_reads`, `agent_max_reads`: the
#'   values; a list (`sheets`, `shells`, `docs`, `datasets`, `agent_reads`) is
#'   a YAML list, `toc_cols` and `sheet_roles` a YAML map.
#'
#' Only the arguments the call was given are in the file; a key that is
#' missing is the argument's default.  Left out: `dir`, `overwrite`,
#' `dry_run`, `quiet`, `progress` and `add` (they describe the call, not the
#' settings), `adam_summary` (an R object) and `numbers` (a data frame:
#' `ai/sheet_numbers.csv` keeps the result and the next call reads it); a
#' value that is not plain text, numbers or `TRUE` / `FALSE` is left out too,
#' and named in a comment of the file.  With `add = TRUE` the file keeps the
#' study's first workbook's settings (`toc`, `toc_sheet`, `sheets`,
#' `shells`, `toc_cols`, `sheet_roles`, `unit`); the other settings the call
#' gave replace the file's, and `docs`, `agent_reads` and `datasets` are
#' joined (in order, once each).  A study with no file, `add = TRUE`: none is
#' written.  The agent does not read the file.
#'
#' @param dir The study folder.
#' @return A named list of the settings, the paths absolute again (a path
#'   of the file that is relative is from `dir`), `toc_cols` a list; with the
#'   attributes `file` (the file's path), `written` and `tflspec` -- or `NULL`
#'   when the study has no `ai/prepare.yml`.  The paths are not checked.
#' @seealso [tfl_ai_prepare()]
#' @examples
#' d <- file.path(tempdir(), "no-prepare-yml")
#' tfl_ai_prepare_settings(d)
#' @export
tfl_ai_prepare_settings <- function(dir) {
  f <- .ai_prepare_file(dir)
  if (!file.exists(f)) return(NULL)
  x <- tryCatch(yaml::read_yaml(f), error = function(e) {
    .ard_stop(sprintf("tfl_ai_prepare_settings(): %s is not readable YAML (%s).", f,
                      conditionMessage(e)))
  })
  v <- suppressWarnings(as.integer(x$version %||% NA))
  if (is.na(v) || v > .ai_prepare_version) {
    .ard_stop(sprintf("tfl_ai_prepare_settings(): %s is version %s of the format; this tflspec reads %d.",
                      f, format(x$version %||% "(none)"), .ai_prepare_version))
  }
  chr <- function(e) as.character(unlist(e, use.names = FALSE))
  out <- list()
  for (k in .ai_prepare_keys) {
    e <- x[[k]]
    if (is.null(e) && !k %in% names(x)) next
    if (is.null(e)) e <- if (k %in% .ai_prepare_lists) list() else next
    out[[k]] <-
      if (identical(k, "toc_cols")) {
        lapply(e, chr)
      } else if (identical(k, "sheet_roles")) {
        stats::setNames(vapply(e, function(r) chr(r)[1L], ""), names(e))
      } else if (k %in% c("size", "max_chars", "max_levels", "agent_max_reads")) {
        as.integer(e)
      } else if (k %in% c(.ai_prepare_lists, .ai_prepare_paths) || is.list(e)) {
        chr(e)
      } else e
    if (k %in% .ai_prepare_paths) out[[k]] <- .ai_prepare_path_in(dir, out[[k]])
  }
  structure(out, file = f, written = as.character(x$written %||% NA),
            tflspec = as.character(x$tflspec %||% NA))
}

# tfl_ai_prepare(dir) with no `toc`: the last call's settings from the study,
# the arguments given now over them
.ai_prepare_repeat <- function(dir, call, env, fn) {
  st <- tfl_ai_prepare_settings(dir)
  if (is.null(st)) {
    stop(simpleError(paste0("argument \"toc\" is missing, with no default\n", fn, ": ",
                            .ai_prepare_file(dir),
                            " is not there, so there is no earlier call to repeat."),
                     call = sys.call(-1L)))
  }
  st <- unclass(st)
  attributes(st) <- list(names = names(st))
  now <- as.list(call)[-1L]
  now$dir <- NULL
  now <- lapply(now, eval, envir = env)
  # (a path of the file that is not there any more, unless given again)
  for (k in intersect(.ai_prepare_paths, setdiff(names(st), names(now)))) {
    gone <- st[[k]][!file.exists(st[[k]])]
    if (length(gone)) {
      .ard_stop(sprintf("%s: %s: `%s` is '%s', which is not there any more; give its new path, %s(dir, %s = ).",
                        fn, .ai_prepare_file(dir), k, gone[1L], sub("[(][)]$", "", fn), k))
    }
  }
  for (k in names(now)) st[k] <- list(now[[k]])
  st <- st[!vapply(st, is.null, NA)]
  do.call(tfl_ai_prepare, c(list(dir = dir), st))
}
