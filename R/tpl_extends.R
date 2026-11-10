# A template's `extends` and the one layer order (canon 1.27, PLAN-5B):
# a template whose set holds its `extends` (tfl_template(extends = )) has
# its reports laid as a code SPEC's are -- the study's files of each step
# (layer 1) < the style set its `style` names (layer 2) < the template's
# items (layer 3) < the report's own (layer 4).  The style is resolved
# first; a study reference reads the content step's range; a parameter's
# stop value takes its `maps_to` out of layers 1 and 2; every value's route
# is recorded once (tfl_template_routes()), and tfl_study_value() is the
# one reader of a study reference.

# Is a report's `template:` a template SPEC (a map with one `name`, not a
# figure design's preset)?
.is_tpl_spec <- function(t) {
  is.list(t) && is.character(t$name) && length(t$name) == 1L && !.fig_preset_named(t)
}

# The layers below a report.  A template with an `extends` in its set:
# the style its `style` parameter resolves to first (.tpl_style_first()),
# over the files each step of the report's `extends` names; any other
# report (a template not yet migrated too): its own `style:` field.
# `style_name` and `style_route` say which style and where it came from.
.tpl_layers_below <- function(output, study) {
  ts <- .tpl_spec_of(output)
  t <- if (!is.null(ts)) .tpl_get(ts$name, sprintf("%s: template:", output$id))
  if (!is.null(t$extends)) {
    sf <- .tpl_style_first(output, t, study)
    lb <- .layers_below(output, study, style = sf$set)
    lb$style_name <- sf$name
    lb$style_route <- sf$route
    return(lb)
  }
  lb <- .layers_below(output, study)
  own <- length(output$style) == 1L && !is.na(output$style)
  lb$style_name <- if (!is.null(lb$style)) if (own) output$style else "standard"
  lb$style_route <- if (own) "own" else "template"
  lb
}

# The style of a template report, resolved before anything else (canon
# 1.32): the report's `style:` field, else its `style` argument, else the
# set's value -- the study's `settings.style` the parameter refers to, else
# its fallback -- else "standard".  A template that fixes its style stops
# on another one.  `set`: the style set (.tpl_style_set()), NULL when the
# style is unset or the template has no `style`; `route`: where the name is
# from (.tpl_param_route()).
.tpl_style_first <- function(output, t, study) {
  p <- t$params$style
  if (is.null(p)) return(list(name = NULL, set = NULL, route = NA_character_))
  args <- .tpl_spec_of(output)$args
  field <- output$style
  has_field <- length(field) == 1L && !is.na(field)
  has_arg <- "style" %in% names(args)
  given <- if (has_field) field else if (has_arg) args$style
  if (isTRUE(p$fixed_value) && (has_field || has_arg) && !identical(given, p$default)) {
    .ard_stop(sprintf("%s: template %s fixes its style (%s); leave `style:` out.",
                      output$id, t$name, p$default))
  }
  if (has_field || has_arg) {
    nm <- given
    route <- .tpl_param_route(p, list(given))
  } else {
    keys <- if (is.character(p$study)) p$study
    hit <- if (length(keys)) .tpl_study_find(keys, output, study)
    if (!is.null(hit$value)) {
      nm <- hit$value
      route <- .tpl_param_route(p, NULL, hit$key)
    } else {
      nm <- p$default %||% "standard"
      route <- .tpl_param_route(p)
    }
  }
  if (is.null(nm) || inherits(nm, "tfl_unset")) return(list(name = NULL, set = NULL, route = route))
  st <- .tpl_style_set(nm, study)
  if (!identical(output$kind, "table")) st$content <- list()
  list(name = nm, set = st, route = route)
}

# Where a parameter's value comes from, one word (the route record's, the
# help's and the cards' one source): "own" (the report gives it), "unset"
# (`!unset`, or a reference with no value and no fallback), "fixed" (the
# template fixes it), "study:<key>" (the study standard's value),
# "fallback" (a reference's value when the study has none), "template"
# (the template's own default).  `given`: the report's value in a list
# (NULL: none); `key`: the study key a value was read from (NA: none).
.tpl_param_route <- function(p, given = NULL, key = NA_character_) {
  if (length(given)) return(if (inherits(given[[1L]], "tfl_unset")) "unset" else "own")
  if (isTRUE(p$fixed_value)) return("fixed")
  if (!is.na(key)) return(if (startsWith(key, "output.")) "own" else paste0("study:", key))
  d <- p$default
  if (inherits(d, "tfl_unset")) return("unset")
  if (is.character(p$study) && length(p$study)) return(if (is.null(d)) "unset" else "fallback")
  "template"
}

# The step a parameter acts in: "data" (a data-stage parameter), else its
# part (`ard`, `content`) when the template declares parts; NA otherwise
.tpl_param_step <- function(t, p) {
  if (identical(p$stage, "data")) return("data")
  for (part in c("ard", "content")) if (p$name %in% t$parts[[part]]) return(part)
  NA_character_
}

# A value that turns a parameter's step off (canon 1.20): `!unset`, FALSE,
# "none"
.tpl_stop_value <- function(v) {
  inherits(v, "tfl_unset") || identical(v, FALSE) || identical(v, "none")
}

# The plan arguments the parameters' stop values take out of the study's
# layers: their declared `maps_to` ("plan_blanks.where"), each with why
# ("blanks: FALSE").  A parameter the report does not give acts by its
# value as the template takes it (its default).
.tpl_step_off <- function(t, args) {
  out <- character()
  for (p in t$params) {
    if (!length(p$maps_to)) next
    v <- if (p$name %in% names(args)) args[[p$name]] else p$default
    if (!.tpl_stop_value(v)) next
    out[p$maps_to] <- sprintf("%s: %s", p$name, if (inherits(v, "tfl_unset")) "!unset" else .tpl_short(v))
  }
  out
}

# Two arguments of a verb that cannot both be in its call: the template's
# one takes the other away from the layers below it (Q9 of PLAN-5B: a
# study's plan_col_header(lines) under the template's header)
.tpl_exclusive_args <- list(plan_col_header = c("header", "lines"))

# The layers below the template's items (the style set, the files of
# `extends`), with the arguments `strip` names ("verb.arg" = why) and the
# exclusive partners of the template's arguments taken out of their content.
# An item left with no argument by it goes.  Returns the layers and
# `removed`: what was taken out of some layer ("verb.arg" = why).
.tpl_strip_lower <- function(layers, strip, tpl_items) {
  drop <- strip
  for (it in if (inherits(tpl_items, "tfl_r")) list() else tpl_items) {
    ex <- .tpl_exclusive_args[[it$fn]]
    has <- intersect(ex, names(it$args))
    for (o in setdiff(ex, has)) if (length(has)) drop[paste0(it$fn, ".", o)] <- paste0(it$fn, ": ", has[1L])
  }
  removed <- character()
  if (!length(drop)) return(list(layers = layers, removed = removed))
  verb <- sub("[.].*$", "", names(drop))
  arg <- sub("^[^.]*[.]", "", names(drop))
  layers <- lapply(layers, function(l) {
    if (inherits(l$content, "tfl_r") || !length(l$content)) return(l)
    keep <- rep(TRUE, length(l$content))
    for (j in seq_along(l$content)) {
      it <- l$content[[j]]
      hit <- which(verb == it$fn & arg %in% names(it$args))
      if (!length(hit)) next
      it$args[arg[hit]] <- NULL
      removed[names(drop)[hit]] <<- drop[hit]
      keep[j] <- length(it$args) > 0L
      l$content[[j]] <- it
    }
    l$content <- l$content[keep]
    l
  })
  list(layers = layers, removed = removed)
}

# A code SPEC's items with `!unset` for each argument taken out of the
# layers below ("verb.arg"): its layers would give it back
.tpl_unset_removed <- function(items, removed) {
  for (k in names(removed)) {
    verb <- sub("[.].*$", "", k)
    arg <- sub("^[^.]*[.]", "", k)
    at <- which(vapply(items, function(it) identical(it$fn, verb), NA))
    if (!length(at)) {
      items <- c(items, list(tfl_item(verb, stats::setNames(list(tfl_unset()), arg))))
      next
    }
    for (i in at) if (!arg %in% names(items[[i]]$args)) items[[i]]$args[[arg]] <- tfl_unset()
  }
  items
}

# The route record of a template report: one row a parameter, in the
# template's order (its `style` as the report's style, last), the
# arguments the stop values took out of the study's layers, then the
# report's `extends`.  `prov`: the resolved items with their provenance.
.tpl_routes <- function(tpl, lb, output, prov = list()) {
  t <- tpl$t
  row <- function(param, step, value, route, key = NA_character_, fallback = NA_character_,
                  fixed = FALSE, reaches = NA_integer_) {
    data.frame(param = param, step = step, value = value, route = route, key = key,
               fallback = fallback, fixed = fixed, reaches = reaches, stringsAsFactors = FALSE)
  }
  ps <- Filter(function(p) !identical(p$name, "style"), t$params)
  rows <- lapply(ps, function(p) {
    keys <- if (is.character(p$study)) p$study else character()
    k <- tpl$study_keys[p$name]
    v <- if (p$name %in% names(tpl$args)) tpl$args[[p$name]] else p$default
    row(p$name, .tpl_param_step(t, p), .tpl_short(v), tpl$route[[p$name]] %||% .tpl_param_route(p),
        key = if (!is.na(k)) unname(k) else if (length(keys)) keys[1L] else NA_character_,
        fallback = if (length(keys)) .tpl_short(p$default) else NA_character_,
        fixed = isTRUE(p$fixed_value))
  })
  for (k in names(tpl$removed)) {
    rows <- c(rows, list(row(k, "content", "(none)", sprintf("removed (%s)", tpl$removed[[k]]))))
  }
  # (`reaches`: the resolved items the study's files give to, under the template)
  study_from <- function(f) f %in% "defaults" | startsWith(f, "pattern:")
  reaches <- sum(vapply(if (inherits(prov, "tfl_r")) list() else prov, function(it) {
    any(study_from(c(it$from %||% "", unname(it$arg_from))))
  }, NA))
  rows <- c(rows, list(row("extends", NA_character_, .extends_text(.extends_compact(lb$extends)),
                           if (!is.null(output$extends)) "own" else "template",
                           fixed = length(t$extends_fixed) > 0L, reaches = reaches)))
  if (!is.null(t$params$style)) {
    p <- t$params$style
    rows <- c(rows, list(row("style", .tpl_param_step(t, p), lb$style_name %||% "(none)",
                             lb$style_route %||% NA_character_,
                             key = if (is.character(p$study)) p$study[1L] else NA_character_,
                             fallback = if (is.character(p$study)) .tpl_short(p$default) else NA_character_,
                             fixed = isTRUE(p$fixed_value))))
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Where a template report's values come from
#'
#' Where each value of a template report comes from, as the report
#' resolves ([tfl_resolve_output()]): one row a parameter of its template,
#' then the plan arguments a parameter's stop value took out of the study's
#' layers, then the report's `extends` and `style`.  The program's notes
#' and an input form's labels read this one record.
#'
#' @param output A template report ([tfl_read_output()]).
#' @param study Its study ([tfl_read_study_spec()]).
#' @return A data frame: `param`, `step` (`"data"`, `"ard"`, `"content"`;
#'   `NA` when the template does not say), `value` (a short text), `route`
#'   (`"own"`, `"study:<key>"`, `"fallback"`, `"template"`, `"fixed"`,
#'   `"unset"` or `"removed (<param>: <value>)"`), `key` (its reference to
#'   the study standard, the key read when one was), `fallback` (the value
#'   when the study has none), `fixed` (`TRUE`: the template fixes it) and
#'   `reaches` (for `extends`: the items the study's layers add under the
#'   template).
#' @examples
#' d <- system.file("extdata", "spec-yaml", package = "tflspec")
#' s <- tfl_read_study_spec(d)
#' o <- tfl_output("T-9", "table", population = "SAF",
#'                 template = list(name = "demog", args = list(by = "TRT01A", continuous = "AGE")))
#' r <- tfl_template_routes(o, s)
#' r[r$route != "template", c("param", "value", "route")]
#' @export
tfl_template_routes <- function(output, study) {
  fn <- "tfl_template_routes()"
  if (!inherits(output, "tfl_output")) .ard_stop(sprintf("%s: `output` is a tfl_output.", fn))
  if (!inherits(study, "tfl_study_spec")) .ard_stop(sprintf("%s: `study` is a tfl_study_spec.", fn))
  if (is.null(.tpl_spec_of(output))) {
    .ard_stop(sprintf("%s: %s is not a template report (no `template: {name, args}`).", fn, output$id))
  }
  attr(tfl_resolve_output(output, study), "template")$routes
}

#' A study standard's value
#'
#' A study standard's value, as a template's reference to it reads it
#' ([tfl_param()]`(study = )`): the one reader of the study standard.
#'
#' @param study The study ([tfl_read_study_spec()]).
#' @param key A study-standard key (`<verb>.<argument>[.<name>]`,
#'   `settings.<name>`, `figure_style.<...>`); several are read in order.
#' @param output The report: its `extends` and `style:` say which layers
#'   are read, nearest first; `NULL`: the study's defaults and the
#'   `settings.style` set.
#' @return A list: `value` (`NULL` when the study has none), `key` (the
#'   key read, `NA` none) and `from` (the layer it came from: `"defaults"`,
#'   `"pattern:<name>"`, `"style:<name>"`, `"settings"`, `"figure_style"`;
#'   `NA` none).
#' @examples
#' s <- tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))
#' tfl_study_value(s, "plan_cells.continuous")$from
#' @export
tfl_study_value <- function(study, key, output = NULL) {
  fn <- "tfl_study_value()"
  if (!inherits(study, "tfl_study_spec")) .ard_stop(sprintf("%s: `study` is a tfl_study_spec.", fn))
  if (!is.character(key) || !length(key) || anyNA(key)) {
    .ard_stop(sprintf("%s: `key` is a study-standard key (\"plan_cells.continuous\"), or several.", fn))
  }
  if (!is.null(output) && !inherits(output, "tfl_output")) {
    .ard_stop(sprintf("%s: `output` is a tfl_output, or NULL.", fn))
  }
  rng <- .tpl_ref_range(output, study)
  for (k in key) {
    v <- .tpl_study_read1(k, output, study, rng$content)
    if (!is.null(v)) return(list(value = v, key = k, from = .tpl_ref_from(k, rng)))
  }
  list(value = NULL, key = NA_character_, from = NA_character_)
}

# The range a report's references read (.tpl_study_read()): `content`, the
# layers' content merged, and `layers`, each layer's content nearest first.
# No report: the `settings.style` set (else `standard`) over the defaults.
# NULL: the old range (study$defaults$content) -- a template not yet
# migrated, a report whose content step lays nothing (5b-1).
.tpl_ref_range <- function(output, study) {
  if (is.null(output)) {
    nm <- .tpl_study_read1("settings.style", NULL, study) %||% "standard"
    st <- tryCatch(.tpl_style_set(nm, study), error = function(e) NULL)
    layers <- c(if (!is.null(st)) list(st$content), list(study$defaults$content))
  } else {
    ts <- .tpl_spec_of(output)
    if (!is.null(ts) && !.tpl_layered(ts$name)) return(NULL)
    lb <- .tpl_layers_below(output, study)
    if (is.null(lb$range)) return(NULL)
    layers <- lapply(lb$layers, function(l) l$content)
  }
  layers <- Filter(function(x) !inherits(x, "tfl_r"), layers)
  acc <- list()
  for (x in rev(layers)) acc <- .merge_content(acc, x)
  list(content = acc, layers = layers)
}

# The layer a key's value was read from: the nearest layer with the verb's
# argument (its items' `from`), or the key's root for the study's other parts
.tpl_ref_from <- function(key, rng) {
  root <- sub("[.].*$", "", key)
  if (root %in% c("settings", "figure_style", "km", "output")) return(root)
  if (is.null(rng)) return("defaults")
  path <- strsplit(sub("^[^.]*[.]", "", key), ".", fixed = TRUE)[[1L]]
  has <- function(x) {
    for (k in path) {
      if (!is.list(x) || !k %in% names(x)) return(FALSE)
      x <- x[[k]]
    }
    TRUE
  }
  for (l in rng$layers) for (it in l) {
    if (identical(it$fn, root) && has(it$args)) return(it$from %||% "own")
  }
  NA_character_
}
