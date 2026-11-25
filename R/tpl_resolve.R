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
# The template's `derive:` names (one operation each, in order) are filled
# in the same way; they are not arguments (.tr_derive()).  Its `check:`
# items (fail: one operation, message:) stop the rules between arguments.

#' A report's template with its values filled in
#'
#' Reads the report's template (`template: name: <series>/<template>`), puts
#' the report's arguments (`template: args:`, the step sections
#' `template: analysis:`, `content:`, ... or directly under `template:`) over the template's defaults, resolves the
#' series' and the study's values (`!series`, `!study`, `!fixed`), fills in
#' the `"{name}"` spots of the template's `data:` rows, analysis items,
#' content and page, and merges the report's own `data:` rows into the
#' template's by id, key by key; a report row with `code:` makes the whole
#' data, and replaces the template's row of its id.  The result is a plain
#' code SPEC, as [tfl_read_output()] reads one.
#'
#' A report's value replaces the default, but for an argument of a map type
#' whose default is the series' (`!series K`, such as `formats`): the
#' report's map is put over the series' map name by name.  A name the report
#' writes replaces the series' value of that name (a map value merges into
#' the series' map of that name, in the same way), a name whose value is
#' `!unset` is removed, and the names the report does not write keep the
#' series' values.
#'
#' An argument the report writes `~` (YAML null) is not given: the
#' default applies, as when it is left out.  None (the argument not
#' passed) is `!unset`; `NULL` itself is `!r NULL`.  A list argument
#' (`list<...>`) the report gives is never empty, has no `""` or `NA`
#' entry, and is not a map where a sequence is declared: such a value
#' stops, naming the argument.
#'
#' An analysis item's `analyses:` (`per`, `ids`) says its analyses: `per:
#' variable` one a variable (its id the variable's entry in `ids`, else the
#' variable's name); `per: level` or `per: [columns]` one row whose
#' `analysis_per` tells the generator how to name the analyses; no `per`
#' one analysis, the item's id.  `per: level` with an `id_prefix` (may be
#' empty) names the depths `<prefix>_ANY`, `<prefix>_L1`, ... (no prefix: `ANY`,
#' `L1`, ...), each replaced by its entry in `ids`.  `per: [columns]` makes an
#' analysis a value of the columns (joined by `_`: `ALT`, `ALT_HIGH`), on an
#' item with condition rows or without (each call's rows by their own value);
#' its `ids` give some values an id of their own (`{ALT: ALT_SHIFT}`).  An item's `conditions:` is a data frame,
#' one row a run, its columns the item's `params` (and `analysis_id`, a
#' row's own id, when a row gives one): the template's rows first, then the
#' report's (`"{conditions}"`).
#'
#' A template's `derive:` map gives names of its own, each one operation on
#' the arguments' values, worked out in the order written once the
#' arguments are known; the template's `data:`, `analysis:`, `content:`,
#' `page:` and later `derive:` entries use them as `"{name}"`, as an
#' argument.  A derived name is not an argument: it has no card, is not in
#' the result's `"args"`, a report cannot give it and it cannot be a param's
#' name.  The operations (one a name; another key stops):
#' \describe{
#'   \item{`value: x`}{`x` as it is filled in.}
#'   \item{`if: x, then: a, else: b`}{`a` when `x` is something (not
#'     nothing, `FALSE`, `""` or empty), else `b` (default nothing).  With
#'     `is: v`, when `x` is the value `v`.}
#'   \item{`last: x`}{the last element of the list `x`.}
#'   \item{`but_last: x`}{all but the last (nothing for fewer than two).}
#'   \item{`join: x, sep: s`}{the elements as one text, `s` between them
#'     (default `", "`).}
#'   \item{`keys: x, prefix: p`}{a map `p1: x1, p2: x2, ...`.}
#'   \item{`each: x, to: t`}{a list, `t` filled in once an element of `x`:
#'     a row of a table (a map) gives its columns as names inside `t` (a
#'     column the row leaves out is nothing), any other element is
#'     `"{item}"`.  A row's column (or `item`) named as an argument or a
#'     derived name stops: a name inside `t` means one thing.}
#'   \item{`given: [name, ...]`}{`TRUE` when the report itself set any of
#'     these arguments (a default, a series' or a study's value does not
#'     count), else nothing.  A name that is not a param of the template
#'     stops.}
#' }
#' Each gives nothing for an empty `x`.  A value that is nothing drops its
#' key, its list element, and an item whose map is left empty; so a content
#' item written `plan_total: "{total_args}"` with `total_args: {if:
#' "{total}", then: {position: last}}` is left out when `total` is off.
#'
#' A template's `check:` list states rules between its arguments.  Each
#' item has two keys: `fail:` (one `derive:` operation, as above) and
#' `message:` (text, `"{name}"` filled in).  The items are worked out in
#' order once `derive:` is; when an item's `fail` is something (the `if:`
#' rule), resolving stops with `<template>: <message>`.  An unknown key, a
#' missing `fail` or `message`, or a `check:` that is not a list stops too.
#' An operation inside `then`/`else` is not worked out: derive a name for it
#' and write `"{name}"`.
#'
#' A study reads its own copy of the series and the template
#' (`<study>/spec/series/<series>/`, [tfl_copy_templates()]) and nothing
#' else: a template it has not copied stops, with the call that copies it.
#' Only a study given without its folder (`study.yml` read as a list, or
#' `NULL`) reads the source folders ([tfl_series_dir()]).  A `study.yml`
#' with `series_settings:` stops: a study's series values are in its copy
#' of `series.yml` ([tfl_series_values_set()]).
#'
#' @param report A report SPEC (a list, as read) or its file.
#' @param template A template, `"<series>/<template>"`; default the report's
#'   `template: name:`.
#' @param study The study: the study folder, its `study.yml` file, or
#'   `study.yml` read as a list (`settings:`; with attribute `"study_dir"`,
#'   the study folder, it reads that study's copies); `NULL` for none.
#' @return A `tfl_output` (the report as a code SPEC).  Its attribute
#'   `"args"` holds the template's arguments as used, and `"origins"` where
#'   each came from (`"report"`, `"template"`, `"series"` -- a `!series`
#'   value, from the study's copy of `series.yml` --, `"study"` -- a
#'   `!study` value, from `study.yml`, or its fallback's origin --, `"fixed"`).
#' @seealso [tfl_arg_defs()], [tfl_copy_templates()], [tfl_series_dir()]
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
  study_dir <- attr(study, "study_dir")
  tf <- .template_file(template, study_dir)
  series <- .tr_series(template, tf, study_dir, who)
  tpl <- .series_resolve(.series_read_yaml(tf), series, study, template)
  torig <- attr(tpl, "origins")
  report <- .series_resolve(report, series, study, template, origin = "report")
  defs <- tfl_arg_defs(template = template, study_dir = study_dir)

  given <- .tr_given(report$template, defs$name, template, who)
  # the report's analysis set fills the template's `population` (one rule,
  # every template: .tr_report_set()); its data map is no rows
  rs <- .tr_report_set(report, given, defs$name, template, who)
  report$data <- rs$data
  given <- rs$given
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
      # a map whose default is the series' (!series K) or the study's
      # (!study K, with or without a fallback): the report's names
      # over that map, the rest as the series has them (canon e023312)
      if (defs$default_kind[[i]] %in% c("series", "study") && startsWith(defs$type[[i]], "map<") &&
          .tr_is_map(val)) {
        val <- .tr_merge_map(def, val)
      }
      o <- "report"
    } else {
      val <- def
      o <- .tr_origin(torig, paste0("params.", p, ".default"))
    }
    if (inherits(val, "tfl_unset")) val <- NULL
    if (!is.null(val)) attr(val, "fixed") <- attr(val, "origin") <- attr(val, "origins") <- NULL
    .tr_one_value(val, defs$type[[i]], defs$values[[i]], p, o, template, who)
    .tr_list_value(val, defs$type[[i]], p, o, template, who)
    if (is.null(val) && isTRUE(defs$required[[i]])) missing <- c(missing, p)
    env[p] <- list(val)
    origins[[p]] <- o
  }
  if (length(missing)) {
    stop(who, ": ", template, " needs ", paste(missing, collapse = ", "),
         " (required: the report gives no value and the template no default).", call. = FALSE)
  }

  args_env <- env
  env <- .tr_derive(tpl$derive, env, defs$name, template, origins)
  .tr_check(tpl$check, env, defs$name, origins, template, who)

  # data: rows -- the template's, the report's merged in by id key by key
  # (a report row with `code:` makes the whole data: it replaces the
  # template's row of its id), then filled in
  # (a figure's data written as its steps, `data: {source, steps}` as a
  # code SPEC writes it, is filled in as it is; a report's rows have no
  # place in it)
  if (is.list(tpl$data) && !is.null(names(tpl$data)) && length(report$data)) {
    stop(who, ": ", template, "'s data are its steps (data: steps:); a report adds no data rows to them.",
         call. = FALSE)
  }
  rows <- tpl$data %||% list()
  for (r in report$data %||% list()) {
    k <- match(r$id %||% NA_character_, vapply(rows, function(e) e$id %||% NA_character_, ""))
    if (is.na(k)) {
      rows <- c(rows, list(r))
    } else if (!is.null(r$code) && !inherits(r$code, "tfl_unset")) {
      rows[[k]] <- r
    } else {
      for (key in names(r)) rows[[k]][key] <- list(r[[key]])
    }
  }
  rows <- lapply(rows, function(r) {
    r <- .tr_fill(r, env)
    r[!vapply(r, function(v) inherits(v, "tfl_unset"), NA)]
  })
  rows <- lapply(rows, .derive_map_in)
  # (a figure's data map: a key filled with no value is no key -- its
  # `population: "{population}"` with no set is no set)
  if (!is.null(names(rows))) {
    rows <- rows[!vapply(rows, function(v) is.null(v) || inherits(v, "tfl_unset"), NA)]
  }

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

  # (labels and value_labels are one plan_labels: a later plan_labels
  # replaces an entry of the same name whole, a heading would be lost;
  # a value_labels entry may be a reference to a study list,
  # !r study$codelists$X, as demog's default)
  cenv <- env
  if ("value_labels" %in% names(env)) {
    keys <- .tr_plan_keys(.tr_fill(tpl$content %||% list(), env))
    cenv["labels"] <- list(.tr_merge_labels(env$labels, env$value_labels, template, keys))
  }
  content <- .tr_fill(tpl$content %||% list(), cenv)
  # (the plan_labels items as one; a reference to a study list for a column
  # no analysis reads left out)
  cols <- unique(as.character(unlist(lapply(items, function(a) c(a$variables, a$by)))))
  content <- .tr_labels_join(content, cols)
  # (a variable's heading under `variable`, as every path writes it)
  content <- .tr_labels_args(content)
  if (is.list(report$content)) content <- c(content, report$content)
  page <- .tr_fill(tpl$page %||% list(), env)
  for (k in names(report$page)) page[k] <- list(report$page[[k]])
  # the style: the report's, else the template's; `!unset` (a study's
  # `settings.style: !unset` read by the template's `!study`) is no style,
  # so the report gets the study's default as a report without one does
  style <- report$style %||% tpl$style
  if (inherits(style, "tfl_unset")) style <- NULL

  y <- list(id = report$id, kind = report$kind %||% tpl$kind %||% "table",
            label = report$label %||% tpl$label, common = report$common,
            extends = report$extends %||% tpl$extends, style = style,
            figure = tpl$figure, data = rows,
            ard = if (length(items)) c(if (identical(kind, "figure")) list(analysis_data = rows),
                                       list(analyses = items)),
            codelists = report$codelists, content = content, page = page)
  y <- y[!vapply(y, is.null, NA)]
  out <- .output_from_list(y)
  attr(out, "template") <- template
  attr(out, "args") <- args_env
  attr(out, "origins") <- origins
  out
}

# The template's derive: names, in order, each one operation of
# .tr_derive_ops on filled values (its options .tr_derive_opts); added to
# `env` after the arguments, so later entries and the template's sections
# read them as "{name}".  Nothing (NULL) stays a name whose value is nothing.
.tr_derive_ops <- list(value = character(), "if" = c("then", "else", "is"), last = character(),
                       but_last = character(), join = "sep", keys = "prefix", each = "to",
                       given = character())

.tr_derive <- function(derive, env, params, template, origins = character()) {
  if (is.null(derive)) return(env)
  where <- sprintf("template %s: derive:", template)
  if (!is.list(derive) || is.null(names(derive)) || !all(nzchar(names(derive)))) {
    stop(where, " is a map, a name to one operation.", call. = FALSE)
  }
  for (nm in names(derive)) {
    if (nm %in% params) {
      stop(where, " ", nm, " is an argument's name; a derived value takes a name of its own.",
           call. = FALSE)
    }
    env[nm] <- list(.tr_derive_one(derive[[nm]], env, paste0(where, " ", nm), params, origins))
  }
  env
}

# The template's check: list, in order, after derive: -- each item
# {fail: <one derive operation>, message: text}; a fail value that is
# something stops with the filled message.
.tr_check <- function(check, env, params, origins, template, who) {
  if (is.null(check)) return(invisible(NULL))
  where <- sprintf("template %s: check:", template)
  ok <- is.list(check) && is.null(names(check))
  if (!ok) stop(where, " is a list of items, each {fail: <operation>, message: text}.", call. = FALSE)
  for (i in seq_along(check)) {
    it <- check[[i]]
    at <- paste0(where, " item ", i)
    if (!is.list(it) || is.null(names(it))) {
      stop(at, " is a map with fail: and message:.", call. = FALSE)
    }
    bad <- setdiff(names(it), c("fail", "message"))
    if (length(bad)) {
      stop(at, ": ", paste(bad, collapse = ", "), " is not a key of a check item (its keys: fail, message).",
           call. = FALSE)
    }
    miss <- setdiff(c("fail", "message"), names(it))
    if (length(miss)) stop(at, " needs ", paste(miss, collapse = ", "), ".", call. = FALSE)
    if (!is.character(it$message) || length(it$message) != 1L) {
      stop(at, ": message is one text.", call. = FALSE)
    }
    x <- .tr_derive_one(it$fail, env, paste0(at, " fail"), params, origins)
    if (.tr_is_something(x)) {
      stop(who, ": ", template, ": ", .tr_fill(it$message, env), call. = FALSE)
    }
  }
  invisible(NULL)
}

.tr_derive_one <- function(d, env, where, params = names(env), origins = character()) {
  ops <- names(.tr_derive_ops)
  op <- if (is.list(d) && !is.null(names(d))) intersect(names(d), ops) else character()
  if (length(op) != 1L) {
    stop(where, ": write one operation, {op: value}; the operations are ",
         paste(ops, collapse = ", "), ".", call. = FALSE)
  }
  bad <- setdiff(names(d), c(op, .tr_derive_ops[[op]]))
  if (length(bad)) {
    ok <- .tr_derive_ops[[op]]
    stop(where, ": ", op, " takes no ", paste(bad, collapse = ", "),
         if (length(ok)) paste0(" (its options: ", paste(ok, collapse = ", "), ")"), ".", call. = FALSE)
  }
  if (op == "given") return(.tr_derive_given(d$given, params, origins, where))
  x <- .tr_fill(d[[op]], env)
  n <- length(x)
  switch(op,
    value = x,
    "if" = .tr_fill(if (.tr_derive_test(x, d)) d$then else d[["else"]], env),
    last = if (n) x[[n]],
    but_last = if (n > 1L) x[-n],
    join = if (n) paste(as.character(unlist(x)), collapse = .tr_option(d$sep, ", ")),
    keys = if (n) stats::setNames(as.list(x), paste0(.tr_option(d$prefix, ""), seq_len(n))),
    each = if (n) .tr_derive_each(x, d$to, env, where))
}

# `given:` -- TRUE when the report itself set any of the named arguments
# (origin "report"), else nothing
.tr_derive_given <- function(x, params, origins, where) {
  nm <- as.character(unlist(x))
  if (!length(nm) || anyNA(nm)) stop(where, ": given takes argument names.", call. = FALSE)
  unknown <- setdiff(nm, params)
  if (length(unknown)) {
    stop(where, ": given: ", paste(unknown, collapse = ", "), " is not a param of the template.",
         call. = FALSE)
  }
  if (any(origins[nm] %in% "report")) TRUE
}

# `if:`'s test: with `is:`, x is that one value; else x is something
.tr_derive_test <- function(x, d) {
  if (is.null(d$is)) return(.tr_is_something(x))
  v <- as.character(unlist(x))
  length(v) == 1L && identical(v, as.character(unlist(d$is))[1L])
}

# `each:` -- `to` filled in once an element: a row (a map) lends its
# columns, another element is "{item}"; none may be a name already known
.tr_derive_each <- function(x, to, env, where) {
  out <- lapply(x, function(e) {
    own <- if (is.list(e) && !is.null(names(e))) e else list(item = e)
    clash <- intersect(names(own), names(env))
    if (length(clash)) {
      stop(where, ": each: ", paste(clash, collapse = ", "),
           " is an argument's or a derived name; a row's column takes a name of its own.",
           call. = FALSE)
    }
    # a column the row leaves out is nothing
    cols <- setdiff(.tr_spots(to), names(env))
    own[setdiff(cols, names(own))] <- list(NULL)
    .tr_fill(to, c(env, own))
  })
  out <- out[!vapply(out, is.null, NA)]
  # (one text each: a vector of them, as a list of names is written)
  if (length(out) && all(vapply(out, function(v) is.character(v) && length(v) == 1L, NA))) {
    return(unlist(out))
  }
  out
}

# the "{name}" names of a value (its text, at any depth)
.tr_spots <- function(x) {
  s <- unlist(x, use.names = FALSE)
  s <- as.character(s[vapply(s, is.character, NA)])
  m <- unlist(regmatches(s, gregexpr(.tr_ref_re, s, perl = TRUE)), use.names = FALSE)
  unique(sub(paste0("^", .tr_ref_re, "$"), "\\1", m, perl = TRUE))
}

# an operation's option as one text (its default when not given)
.tr_option <- function(v, default) {
  if (is.null(v)) default else as.character(unlist(v))[1L]
}

# `if:`'s test: something, not nothing, FALSE / NA, "" or an empty list
.tr_is_something <- function(x) {
  if (is.null(x) || inherits(x, "tfl_unset") || !length(x)) return(FALSE)
  if (is.logical(x)) return(isTRUE(all(x)))
  if (is.character(x) && length(x) == 1L) return(!is.na(x) && nzchar(x))
  TRUE
}

# The study as a list: a list as given, study.yml, or a folder that holds
# it.  Read from a file or a folder, it has attribute "study_dir" (the
# study folder: the series are read from its copies).  A study.yml with
# series_settings: stops (.series_settings_gone()).
.tr_study <- function(study, who) {
  if (is.null(study)) return(study)
  if (is.list(study)) {
    .series_settings_gone(study, attr(study, "study_dir"))
    return(study)
  }
  if (!is.character(study) || length(study) != 1L) {
    stop(who, ": `study` is study.yml read as a list, the file or the study folder.", call. = FALSE)
  }
  is_dir <- dir.exists(study)
  f <- if (is_dir) file.path(study, "study.yml") else study
  sd <- normalizePath(if (is_dir) study else dirname(study), winslash = "/", mustWork = FALSE)
  if (!file.exists(f)) stop(who, ": no file ", f, call. = FALSE)
  out <- .series_read_yaml(f) %||% list()
  .series_settings_gone(out, sd)
  structure(out, study_dir = sd)
}

# A study spec's study.yml as .tr_study() gives it: with its folder, so
# that its series are read from its copies
.tr_study_of <- function(study) {
  structure(study$study %||% list(), study_dir = study$dir)
}

# The series of a template file `tf`: the study's copy of series.yml (next
# to the copied template) when `study_dir` is given, else the source's.
.tr_series <- function(template, tf, study_dir, who) {
  s <- sub("/.*$", "", template)
  if (is.null(study_dir)) return(tfl_series_read(s, dir = dirname(dirname(dirname(tf)))))
  path <- dirname(dirname(tf))
  if (!file.exists(file.path(path, "series.yml"))) {
    stop(who, ": series ", s, " is not copied into the study (no ", file.path(path, "series.yml"),
         "): ", .study_copy_hint(study_dir, template), ".", call. = FALSE)
  }
  .series_read_at(path, s, who)
}

# where a param's default came from (the first value under its path)
.tr_origin <- function(origins, prefix) {
  hit <- origins[names(origins) == prefix | startsWith(names(origins), paste0(prefix, "."))]
  if (length(hit)) hit[[1L]] else "template"
}

# An argument of a one-value type (string, number, logical, column, level,
# enum[...]) is one value, and an enum's one of its values: by the declared
# type, before the template's derive: and steps use it (a list there would
# stop later in a call's argument, or an enum's other value leave its
# steps out without a word).  Rules between arguments are not here.
.tr_one_value <- function(val, type, values, p, origin, template, who) {
  if (is.null(val) || is.na(type)) return(invisible())
  one <- type %in% c("string", "number", "logical", "column", "level") ||
    startsWith(type, "enum[")
  if (!one) return(invisible())
  from <- if (identical(origin, "report")) "the report" else origin
  if (length(val) != 1L || (is.list(val) && !inherits(val, "tfl_r"))) {
    stop(who, ": ", template, "'s ", p, " is one value (", type, "); ", from, " gives ",
         length(val), ".", call. = FALSE)
  }
  if (length(values) && !inherits(val, "tfl_r") && !as.character(val) %in% values) {
    stop(who, ": ", template, "'s ", p, " is one of ", paste(values, collapse = ", "),
         "; ", from, " gives ", encodeString(as.character(val), quote = "\""), ".",
         call. = FALSE)
  }
  invisible()
}

# An argument of a list type (list<...>, alone or beside other members) the
# report gives: an empty field is "not given" (the argument left out, the
# template's default) and "(none)" is !unset, so a given list is never empty,
# has no "" or NA entry, and is not a map where only sequences are declared
# (a map of "" is what a field read as "name | text" rows gives).  A value
# from the template, the series or the study is not checked here; R code
# (!r) and other tagged values are not either.
.tr_list_value <- function(val, type, p, origin, template, who) {
  if (is.null(val) || is.na(type) || !identical(origin, "report") || is.object(val)) {
    return(invisible())
  }
  m <- .type_members(type)
  if (!any(startsWith(m, "list<"))) return(invisible())
  seq_only <- !any(grepl("^(map|vec|rows|call|step)<", m))
  blank <- function(e) {
    is.null(e) || (is.atomic(e) && !is.object(e) &&
                     (!length(e) || (length(e) == 1L && (is.na(e) || identical(as.character(e), "")))))
  }
  entries <- if (is.list(val)) val else as.list(val)
  why <- if (!length(val)) {
    "an empty list"
  } else if (seq_only && any(nzchar(names(val) %||% ""))) {
    "a map (name: value)"
  } else if (any(vapply(entries, function(e) is.atomic(e) && length(e) == 1L && !is.object(e) &&
                          is.na(e), NA))) {
    "an NA entry"
  } else if (any(vapply(entries, blank, NA))) {
    "an empty entry (\"\")"
  }
  if (is.null(why)) return(invisible())
  stop(who, ": ", template, "'s ", p, " is a list (", type, "); the report gives ", why,
       ". Leave ", p, " out for the template's default, or write !unset for none.",
       call. = FALSE)
}

# A map as read (a named list, not a tagged value); an empty list is one too
.tr_is_map <- function(x) {
  is.list(x) && !is.object(x) && (!length(x) || .series_is_map(x))
}

# `over` put over `base` name by name (canon e023312): a name `over` writes
# replaces the base's, a map value merges into the base's map of that name,
# and `!unset` removes the name; the base's other names stay.  Nothing left
# is NULL (as `!unset` for the whole argument); a map inside left empty
# drops its name.
.tr_merge_map <- function(base, over) {
  out <- if (.tr_is_map(base)) base else list()
  attributes(out) <- if (length(out)) list(names = names(out))
  for (k in names(over)) {
    v <- over[[k]]
    if (inherits(v, "tfl_unset")) {
      out[[k]] <- NULL
    } else if (.tr_is_map(v)) {
      out[[k]] <- .tr_merge_map(out[[k]], v)
    } else {
      out[k] <- list(v)
    }
  }
  if (length(out)) out
}

# The report's analysis set and the template's `population` argument (A2,
# canon 11-15: the set lives on the report's data; the template's data-step
# argument `population` is that value, its default the template's).  One
# rule for every template:
# - the report's set as the one reader reads it (.report_set(): its data
#   map's `population`, `data: {population: SAF}`, else `common:
#   population` until `common` goes) is the argument's value when the
#   report does not give the argument itself; given both, they are one
#   value, or it stops (the header and the program would disagree);
# - a template with no such argument takes no set on the data map (it
#   stops: no claim the program does not keep; `common:` is left as it was);
# - the data map says the set only: it is no data rows (those are a list,
#   merged by id), and another key stops, naming it.
# list(data = the report's data rows, given = its arguments)
.tr_report_set <- function(report, given, names_ok, template, who) {
  d <- report[["data"]]
  map <- is.list(d) && length(d) && !is.null(names(d))
  if (map) {
    other <- setdiff(names(d), "population")
    if (length(other)) {
      stop(who, ": ", template, ": the report's data is a map (", paste(other, collapse = ", "),
           "); a template report's data map says its analysis set only (data: {population: SAF}), ",
           "its own data are rows by id (data: [{id: ..., ...}]).", call. = FALSE)
    }
  }
  set <- .report_set(report)
  rows <- if (!map) d
  if (is.na(set)) return(list(data = rows, given = given))
  if (!"population" %in% names_ok) {
    if (map) {
      stop(who, ": ", template, " takes no analysis set (it has no argument population); ",
           "the report's data says ", set, ".", call. = FALSE)
    }
    return(list(data = rows, given = given))
  }
  if ("population" %in% names(given)) {
    g <- given[["population"]]
    gv <- if (inherits(g, "tfl_unset")) NA_character_ else .na_scalar(unlist(g))
    if (!identical(gv, set)) {
      stop(who, ": ", template, ": the report's analysis set is ", set, " (its data's, or `common:`) ",
           "and its template argument population ", if (is.na(gv)) "none" else gv,
           ": write it once, on the report's data.", call. = FALSE)
    }
    return(list(data = rows, given = given))
  }
  given$population <- set
  list(data = rows, given = given)
}

# The report's arguments: `template:` without `name`, its step sections
# (data, analysis, content, page) and a flat `args:` read as one map
.tr_given <- function(t, names_ok, template, who) {
  t$name <- NULL
  out <- list()
  for (k in names(t)) {
    v <- t[[k]]
    if (k %in% c("args", "data", "analysis", "content", "page") && !k %in% names_ok &&
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
  # (an argument written `~`, YAML null, is not given: the template's default,
  # as when it is left out.  None -- not passed -- is `!unset`; NULL itself
  # is `!r NULL`.  Canon 2026-11-24, the 318 distinction kept)
  out[!vapply(out, is.null, NA)]
}

# One analysis item as its analyses (canon fc7bd00).  `analyses: {per, ids}`:
#   per: variable   an analysis a variable, its id ids[var] else the variable
#   per: level      one row, analysis_per "level" (the generator's ids a level)
#   per: [cols]     one row, analysis_per the columns |-joined (an id a value);
#                   ids after them as ids:VALUE=ID,...
#   no per          one analysis, the item's id
# labels ({variable: heading}) and value_labels ({variable: {code: text}}) as
# one map for plan_labels(): a variable in both keeps its heading -- under
# `variable` for a table whose rows are `variable` (`keys`, the table's cols
# and rows: .plan_labels_args()), else (and for a key column) written under
# its own name in front of its values (the heading a value_labels entry
# gives itself under that name wins)
.tr_merge_labels <- function(labels, value_labels, template, keys = character()) {
  if (is.null(value_labels)) return(labels)
  out <- .plan_labels_args(labels, keys)
  for (v in names(value_labels)) {
    vl <- value_labels[[v]]
    if (is.null(vl) || inherits(vl, "tfl_unset")) next
    if (.has_study_ref(vl)) {
      # a study list (R): the heading goes in front of it in R
      head <- out[[v]]
      out[[v]] <- if (is.null(head)) vl else .tr_label_pair(v, head, vl)
      next
    }
    if (!is.list(vl) || is.null(names(vl)) || !all(nzchar(names(vl)))) {
      stop("template ", template, ": value_labels: ", v, " is a map ({code: text}).", call. = FALSE)
    }
    head <- out[[v]]
    if (is.character(head) && length(head) == 1L && !v %in% names(vl)) {
      vl <- c(stats::setNames(list(head), v), vl)
    }
    out[[v]] <- vl
  }
  if (length(out)) out else NULL
}

# A template's labels map ({name: text or value labels}) as plan_labels()'
# arguments: the one rule every template takes (tfl_template_resolve(), the
# registry's demog), so a report's program says the same call whichever way
# it is made (tflspec#856) -- the call the sheets write too (spec_code.R:
# the variables' headings under `variable`).  For a table whose rows are
# `variable` (`keys`: its cols and rows), every variable's heading goes
# under `variable`, first, after any `variable` entry the map has: one text
# named by a variable, and a value-labels map's entry under its own
# variable's name (`SEX: {SEX: Sex, F: Female}`).  rtfreporter reads the
# headings from that one entry once it is there, so none may stay outside
# it.  A key column's heading and the value labels (a named text, a list of
# them, a reference to a study list) stay under their own name, in order;
# a table without `variable` keeps the map as it is.
.plan_labels_args <- function(labels, keys = character()) {
  if (!length(labels)) return(list())
  labels <- as.list(labels)
  if (!"variable" %in% keys) return(labels)
  nm <- names(labels) %||% rep("", length(labels))
  plain <- function(x) !inherits(x, c("tfl_r", "tfl_unset", "tfl_call"))
  # (a `variable` entry in R cannot take more names: the map as it is)
  if (!plain(labels[["variable"]])) return(labels)
  var <- if (!is.null(labels[["variable"]])) as.list(labels[["variable"]]) else list()
  rest <- list()
  for (i in seq_along(labels)) {
    v <- nm[i]
    x <- labels[[i]]
    if (identical(v, "variable")) next
    if (!nzchar(v) || v %in% keys || !plain(x)) {
      rest <- c(rest, stats::setNames(list(x), v))
      next
    }
    if (is.character(x) && length(x) == 1L && !is.na(x) && is.null(names(x))) {
      var[[v]] <- x
      next
    }
    # (a value-labels map with the variable's own heading in it)
    xn <- names(x)
    if (!is.null(xn) && v %in% xn && (is.character(x) || is.list(x))) {
      h <- x[[match(v, xn)]]
      if (is.character(h) && length(h) == 1L && !is.na(h)) {
        var[[v]] <- h
        x <- x[xn != v]
      }
    }
    if (length(x)) rest <- c(rest, stats::setNames(list(x), v))
  }
  if (!length(var)) return(rest)
  c(list(variable = var), rest)
}

# The table's keys (its table_plan item's cols and rows) in a template's
# content as its file writes it (`- table_plan: {cols, rows}`)
.tr_plan_keys <- function(content) {
  first <- function(it) if (is.list(it) && length(it) == 1L && !is.null(names(it))) names(it) else ""
  tp <- match("table_plan", vapply(content, first, ""))
  if (is.na(tp) || !is.list(content[[tp]]$table_plan)) return(character())
  as.character(unlist(content[[tp]]$table_plan[c("cols", "rows")], use.names = FALSE))
}

# The template's plan_labels items in plan_labels()' arguments
# (.plan_labels_args()), with the table's keys
.tr_labels_args <- function(content) {
  keys <- .tr_plan_keys(content)
  if (!"variable" %in% keys) return(content)
  for (k in seq_along(content)) {
    it <- content[[k]]
    if (!is.list(it) || !identical(names(it), "plan_labels")) next
    a <- it$plan_labels
    if (is.list(a) && !is.null(names(a))) content[[k]] <- list(plan_labels = .plan_labels_args(a, keys))
  }
  content
}

.tr_item <- function(it, env, template) {
  shadow <- as.character(unlist(it$params))
  # (a param is the row's function's argument, written bare inside
  # dplyr::filter() and dplyr::mutate(): a name the ARD has itself -- a
  # cards column, analysis_id, output_id, population_id -- would read the
  # column instead, so it stops; so do data and ard, the function's own
  # names for its rows and its result)
  own <- shadow[.ard_own_name(shadow) | shadow %in% c("data", "ard")]
  if (length(own)) {
    stop("template ", template, ": item ", it$id %||% "?", ": params: ",
         paste(own, collapse = ", "), " is a name the ARD uses itself (",
         "a cards column, analysis_id, output_id or population_id) or the ",
         "row function's own (data, ard); ",
         "give the param another name.", call. = FALSE)
  }
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
    # per: level with id_prefix: ids are <prefix>_ANY, <prefix>_L1, ...
    # (no prefix: ANY, L1, ...), each one replaced by its entry in `ids`;
    # written `level:<the ids, the any row's first>`
    if (identical(per, "level") && !is.null(an$id_prefix)) {
      pre <- as.character(unlist(an$id_prefix))
      pre <- if (length(pre) && nzchar(pre[1L])) paste0(pre[1L], "_") else ""
      d <- c("ANY", paste0("L", seq_along(f$variables)))
      ids <- an$ids
      new <- vapply(d, function(k)
        if (is.list(ids) && !is.null(ids[[k]])) as.character(ids[[k]]) else paste0(pre, k), "")
      f$analysis_per <- paste0("level:", paste(new, collapse = ","))
    } else if (!identical(per, "level") && is.list(an$ids) && length(an$ids)) {
      # per: [cols] with ids: the ids given for some values, after the
      # columns as `ids:VALUE=ID,...` (the rest take the values)
      ids <- vapply(an$ids, function(v) as.character(unlist(v))[1L], "")
      f$analysis_per <- paste(c(per, paste0("ids:", paste(names(ids), ids, sep = "=",
                                                         collapse = ","))), collapse = "|")
    }
    return(list(f))
  }
  .item_per_variable(f, an$ids)
}

# An item as an item a variable: each its variable's id in `ids`, else the
# variable's name (a template's `per: variable`, an analysis's analysis_ids)
.item_per_variable <- function(f, ids) {
  lapply(as.character(unlist(f$variables)), function(v) {
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
    parts <- lapply(x, function(e) {
      w <- .tr_whole(e, env, shadow)
      if (w$hit) w$value else .tr_text(e, env, shadow)
    })
    # (a whole value that is a list, listing_col calls, stays a list)
    if (!any(vapply(parts, is.list, NA))) return(unlist(parts))
    return(do.call(c, lapply(parts, function(v) {
      if (is.null(v)) NULL else if (is.list(v) && is.null(names(v))) v else as.list(v)
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
