# A report SPEC that names a series template, as a code SPEC (package C).
#
# The loader is thin: the template's params (tfl_arg_defs(template =), the
# one definition) take the report's values over their defaults, the series'
# and the study's values fill the tags (.series_resolve()), and the
# template's `data:` rows, analysis items, content and page get their
# "{name}" spots filled.  The result is what tfl_read_output() gives for a
# code SPEC file; the generator reads it as it reads any report.
#
#   "{name}"        a whole value: the param's value as it is (a vector is
#                   spliced into a list; NULL drops the key)
#   "... {name} ..." inside text or R: the value as text
#   "{{" / "}}"     a brace
# Inside an item with `params:`, a "{p}" of one of those params is the
# item's (the generator's one-row function's argument): left as written.
# The item's `conditions:` rows are the template's values, filled in.

#' A report's template with its values filled in
#'
#' Reads the report's template (`template: name: <series>/<template>`), puts
#' the report's arguments (`template: analysis:`, `content:`, ... or
#' directly under `template:`) over the template's defaults, resolves the
#' series' and the study's values (`!series`, `!study`, `!fixed`), fills in
#' the `"{name}"` spots of the template's `data:` rows, analysis items,
#' content and page, and merges the report's own `data:` rows into the
#' template's by id, key by key.  The result is a plain code SPEC, as
#' [tfl_read_output()] reads one.
#'
#' An analysis item's `analyses:` (`per`, `ids`) says its analyses: `per:
#' variable` one a variable (its id the variable's entry in `ids`, else the
#' variable's name); `per: level` or `per: [columns]` one row whose
#' `analysis_per` tells the generator how to name the analyses; no `per`
#' one analysis, the item's id.  An item's `conditions:` is a data frame,
#' one row a run, its columns the item's `params` (and `analysis_id`, a
#' row's own id, when a row gives one): the template's rows first, then the
#' report's (`"{conditions}"`).
#'
#' @param report A report SPEC (a list, as read) or its file.
#' @param template A template, `"<series>/<template>"`; default the report's
#'   `template: name:`.
#' @param study The study: its `study.yml` read as a list (`settings:`,
#'   `series_settings:`), the file, or the study folder; `NULL` for none.
#' @return A `tfl_output` (the report as a code SPEC).  Its attribute
#'   `"args"` holds the template's arguments as used, and `"origins"` where
#'   each came from (`"report"`, `"template"`, `"series"`, `"study"`,
#'   `"fixed"`).
#' @seealso [tfl_arg_defs()], [tfl_series_dir()]
#' @export
tfl_template_resolve <- function(report, template = NULL, study = NULL) {
  who <- "tfl_template_resolve()"
  if (is.character(report) && length(report) == 1L) {
    if (!file.exists(report)) stop(who, ": no file ", report, call. = FALSE)
    report <- .series_read_yaml(report)
  }
  if (!is.list(report)) stop(who, ": `report` is a report SPEC (a list) or its file.", call. = FALSE)
  study <- .tr_study(study, who)
  template <- template %||% report$template$name
  if (is.null(template)) stop(who, ": the report names no template (template: name:).", call. = FALSE)
  tf <- .template_file(template)
  series <- tfl_series_read(sub("/.*$", "", template), dir = dirname(dirname(dirname(tf))))
  tpl <- .series_resolve(.series_read_yaml(tf), series, study, template)
  torig <- attr(tpl, "origins")
  report <- .series_resolve(report, series, study, template, origin = "report")
  defs <- tfl_arg_defs(template = template)

  given <- .tr_given(report$template, defs$name, template, who)
  env <- list()
  origins <- character()
  missing <- character()
  for (i in seq_len(nrow(defs))) {
    p <- defs$name[[i]]
    def <- tpl$params[[p]]$default
    if (p %in% names(given)) {
      if (isTRUE(attr(def, "fixed"))) {
        stop(who, ": ", template, "'s ", p, " is fixed (!fixed); a report cannot change it.",
             call. = FALSE)
      }
      val <- given[[p]]
      o <- "report"
    } else {
      val <- def
      o <- .tr_origin(torig, paste0("params.", p, ".default"))
    }
    if (inherits(val, "tfl_unset")) val <- NULL
    if (!is.null(val)) attr(val, "fixed") <- attr(val, "origin") <- attr(val, "origins") <- NULL
    if (is.null(val) && isTRUE(defs$required[[i]])) missing <- c(missing, p)
    env[p] <- list(val)
    origins[[p]] <- o
  }
  if (length(missing)) {
    stop(who, ": ", template, " needs ", paste(missing, collapse = ", "),
         " (required: the report gives no value and the template no default).", call. = FALSE)
  }

  # data: rows -- the template's, the report's merged in by id key by key,
  # then filled in
  rows <- tpl$data %||% list()
  for (r in report$data %||% list()) {
    k <- match(r$id %||% NA_character_, vapply(rows, function(e) e$id %||% NA_character_, ""))
    if (is.na(k)) {
      rows <- c(rows, list(r))
    } else {
      for (key in names(r)) rows[[k]][key] <- list(r[[key]])
    }
  }
  rows <- lapply(rows, function(r) {
    r <- .tr_fill(r, env)
    # (the templates' subjects_in: the code SPEC's subjects, until it is
    # renamed there)
    if (!is.null(r$subjects_in)) {
      r$subjects <- r$subjects_in
      r$subjects_in <- NULL
    }
    r[!vapply(r, function(v) inherits(v, "tfl_unset"), NA)]
  })
  rows <- lapply(rows, .derive_map_in)

  items <- list()
  for (it in tpl$analysis %||% list()) {
    items <- c(items, .tr_item(it, env, template))
  }
  # a denominator that is an item (BIGN, canon 23a41ff): that item's data
  ids <- vapply(items, function(a) a$id %||% NA_character_, "")
  for (i in seq_along(items)) {
    dn <- items[[i]]$denominator
    if (is.character(dn) && length(dn) == 1L && dn %in% ids) {
      items[[i]]$denominator <- items[[match(dn, ids)]]$data
    }
  }
  # a figure's analyses read the report's data rows as their analysis data
  kind <- report$kind %||% tpl$kind %||% "table"

  content <- .tr_fill(tpl$content %||% list(), env)
  if (is.list(report$content)) content <- c(content, report$content)
  page <- .tr_fill(tpl$page %||% list(), env)
  for (k in names(report$page)) page[k] <- list(report$page[[k]])

  y <- list(id = report$id, kind = report$kind %||% tpl$kind %||% "table",
            label = report$label %||% tpl$label, common = report$common,
            extends = report$extends %||% tpl$extends, style = report$style %||% tpl$style,
            figure = tpl$figure, data = rows,
            ard = if (length(items)) c(if (identical(kind, "figure")) list(analysis_data = rows),
                                       list(analyses = items)),
            codelists = report$codelists, content = content, page = page)
  y <- y[!vapply(y, is.null, NA)]
  out <- .output_from_list(y)
  attr(out, "template") <- template
  attr(out, "args") <- env
  attr(out, "origins") <- origins
  out
}

# the study as a list: a list as given, study.yml, or a folder that holds it
.tr_study <- function(study, who) {
  if (is.null(study) || is.list(study)) return(study)
  if (!is.character(study) || length(study) != 1L) {
    stop(who, ": `study` is study.yml read as a list, the file or the study folder.", call. = FALSE)
  }
  f <- if (dir.exists(study)) file.path(study, "study.yml") else study
  if (!file.exists(f)) stop(who, ": no file ", f, call. = FALSE)
  .series_read_yaml(f) %||% list()
}

# where a param's default came from (the first value under its path)
.tr_origin <- function(origins, prefix) {
  hit <- origins[names(origins) == prefix | startsWith(names(origins), paste0(prefix, "."))]
  if (length(hit)) hit[[1L]] else "template"
}

# The report's arguments: `template:` without `name`, its step sections
# (data, analysis, content, page) read as one map
.tr_given <- function(t, names_ok, template, who) {
  t$name <- NULL
  out <- list()
  for (k in names(t)) {
    v <- t[[k]]
    if (k %in% c("data", "analysis", "content", "page") && !k %in% names_ok &&
        is.list(v) && !is.null(names(v))) {
      for (p in names(v)) out[p] <- list(v[[p]])
    } else {
      out[k] <- list(v)
    }
  }
  bad <- setdiff(names(out), names_ok)
  if (length(bad)) {
    stop(who, ": ", template, " has no argument ", paste(bad, collapse = ", "),
         " (its arguments: ", paste(names_ok, collapse = ", "), ").", call. = FALSE)
  }
  out
}

# One analysis item as its analyses (canon fc7bd00).  `analyses: {per, ids}`:
#   per: variable   an analysis a variable, its id ids[var] else the variable
#   per: level      one row, analysis_per "level" (the generator's ids a level)
#   per: [cols]     one row, analysis_per the columns |-joined (an id a value)
#   no per          one analysis, the item's id
.tr_item <- function(it, env, template) {
  shadow <- as.character(unlist(it$params))
  cond <- it$conditions
  it$conditions <- NULL
  an <- it$analyses
  it$analyses <- NULL
  an <- .tr_fill(an, env, shadow)
  f <- .tr_fill(it, env, shadow)
  if (!is.null(f$params)) f$params <- as.character(unlist(f$params))
  if (!is.null(cond)) f$conditions <- .tr_conditions(.tr_fill(cond, env), shadow, f$id, template)
  if (is.null(f$type)) f$type <- "ard"
  f <- .derive_map_in(f)
  per <- as.character(unlist(an$per))
  if (!length(per)) return(list(f))
  if (!identical(per, "variable")) {
    f$analysis_per <- paste(per, collapse = "|")
    return(list(f))
  }
  ids <- an$ids
  lapply(as.character(f$variables), function(v) {
    g <- f
    g$id <- if (!is.null(ids[[v]])) as.character(ids[[v]]) else v
    g$variables <- v
    g
  })
}

# An item's conditions: rows as a data frame, its columns the item's params
# and, when a row gives one, analysis_id (canon b5db722: a row's own id; an
# empty cell or no column, the item's analyses: per rule)
# (an !r cell as its text; a cell of several values a list column)
.tr_conditions <- function(rows, params, id, template) {
  rows <- Filter(Negate(is.null), rows)
  cols <- unique(unlist(lapply(rows, names)))
  bad <- setdiff(cols, c(params, "analysis_id"))
  if (length(bad)) {
    stop("template ", template, ": item ", id, ": conditions: ", paste(bad, collapse = ", "),
         " is not one of its params (", paste(params, collapse = ", "), ").", call. = FALSE)
  }
  out <- data.frame(row.names = seq_along(rows))
  for (p in c(params, intersect("analysis_id", cols))) {
    col <- lapply(rows, function(r) {
      v <- r[[p]]
      if (is.null(v)) NA_character_ else if (inherits(v, "tfl_r")) as.character(unclass(v)) else unlist(v)
    })
    if (all(lengths(col) == 1L)) out[[p]] <- unlist(col) else out[[p]] <- I(col)
  }
  out
}

.tr_ref_re <- "\\{([A-Za-z_][A-Za-z0-9_.]*)\\}"

# a value as text, for a "{name}" inside text or R
.tr_as_text <- function(v) {
  if (is.null(v) || inherits(v, "tfl_unset")) return("NULL")
  if (inherits(v, "tfl_r")) return(as.character(unclass(v)))
  if (is.logical(v)) return(paste(ifelse(v, "TRUE", "FALSE"), collapse = ", "))
  if (is.list(v)) return(paste(vapply(v, .tr_as_text, ""), collapse = ", "))
  paste(as.character(v), collapse = ", ")
}

# the "{name}" spots of a text (not the shadowed ones, not unknown names)
.tr_text <- function(s, env, shadow = character()) {
  s <- gsub("}}", "\002", gsub("{{", "\001", s, fixed = TRUE), fixed = TRUE)
  m <- gregexpr(.tr_ref_re, s, perl = TRUE)
  regmatches(s, m) <- lapply(regmatches(s, m), function(hits) {
    vapply(hits, function(h) {
      nm <- sub(paste0("^", .tr_ref_re, "$"), "\\1", h, perl = TRUE)
      if (!nm %in% names(env) || nm %in% shadow) return(h)
      .tr_as_text(env[[nm]])
    }, "", USE.NAMES = FALSE)
  })
  gsub("\002", "}", gsub("\001", "{", s, fixed = TRUE), fixed = TRUE)
}

# a whole "{name}" value: list(hit = TRUE, value = ...) or list(hit = FALSE)
.tr_whole <- function(s, env, shadow) {
  if (!grepl(paste0("^", .tr_ref_re, "$"), s, perl = TRUE)) return(list(hit = FALSE))
  nm <- sub(paste0("^", .tr_ref_re, "$"), "\\1", s, perl = TRUE)
  if (!nm %in% names(env) || nm %in% shadow) return(list(hit = FALSE))
  list(hit = TRUE, value = env[[nm]])
}

# a value with its "{name}" spots filled in
.tr_fill <- function(x, env, shadow = character()) {
  if (is.null(x) || inherits(x, "tfl_unset")) return(x)
  if (inherits(x, "tfl_r")) return(tfl_r(.tr_text(as.character(unclass(x)), env, shadow)))
  if (is.character(x) && is.null(attributes(x))) {
    if (length(x) == 1L) {
      w <- .tr_whole(x, env, shadow)
      return(if (w$hit) w$value else .tr_text(x, env, shadow))
    }
    return(unlist(lapply(x, function(e) {
      w <- .tr_whole(e, env, shadow)
      if (w$hit) unlist(w$value) else .tr_text(e, env, shadow)
    })))
  }
  if (!is.list(x)) return(x)
  if (!is.null(names(x)) && all(nzchar(names(x)))) {
    out <- x
    for (k in names(x)) {
      v <- .tr_fill(x[[k]], env, shadow)
      if (is.null(v)) out[[k]] <- NULL else out[[k]] <- v
    }
    if (!length(out) && length(x)) return(NULL)
    return(out)
  }
  out <- list()
  for (e in x) {
    if (is.character(e) && length(e) == 1L && is.null(attributes(e))) {
      w <- .tr_whole(e, env, shadow)
      if (w$hit) {
        v <- w$value
        if (is.null(v)) next
        if (is.list(v) && is.null(names(v))) {
          out <- c(out, v)
        } else if (is.atomic(v) && length(v) != 1L) {
          out <- c(out, as.list(v))
        } else {
          out <- c(out, list(v))
        }
        next
      }
    }
    v <- .tr_fill(e, env, shadow)
    if (!is.null(v)) out <- c(out, list(v))
  }
  out
}
