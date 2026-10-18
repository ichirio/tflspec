# A report written as a template SPEC: its YAML file's
#
#   template:
#     name: demog
#     args: {population: SAF, continuous: AGE, ...}
#
# with `ard:` / `data:` / `content:` holding only what the report adds or
# changes.  tfl_resolve_output() puts the template's items between the
# study's layers (defaults, patterns) and the report's own; the program is
# the template's call (A, tfl_content_code()) with the study's and the
# report's lines after it; tfl_template_to_spec() writes the items into
# the file instead (B).  (A figure designer's `template: km_simple`, a
# name, is something else: a template SPEC is a map with `name`.)

# The report's template SPEC, or NULL
.tpl_spec_of <- function(output) {
  t <- output$template
  if (!is.list(t) || is.null(t$name)) return(NULL)
  if (!is.character(t$name) || length(t$name) != 1L || !nzchar(t$name)) {
    .ard_stop(sprintf("%s: `template: name:` is one template's name.", output$id))
  }
  list(name = t$name, args = as.list(t$args %||% list()))
}

# The study's default of a parameter that says where it is
# (tfl_param(study = "<verb>.<argument>")): that argument of the verb's
# item in the layers below the report, NULL when none has it
.tpl_study_value <- function(src, base) {
  fn <- sub("[.].*$", "", src)
  arg <- sub("^[^.]*[.]", "", src)
  hit <- Filter(function(it) identical(it$fn, fn) && arg %in% names(it$args),
                if (inherits(base, "tfl_r")) list() else base %||% list())
  if (length(hit)) hit[[length(hit)]]$args[[arg]] else NULL
}

# The template SPEC's arguments with the study's defaults: a parameter the
# SPEC does not give takes the study's value (tfl_param(study = )); one it
# gives over a different study value says so (a message).  Returns the
# arguments and the names the study gave.
.tpl_study_args <- function(t, args, base, id) {
  from_study <- character()
  for (p in t$params) {
    if (is.null(p$study)) next
    v <- .tpl_study_value(p$study, base)
    if (is.null(v)) next
    if (!p$name %in% names(args)) {
      args[[p$name]] <- v
      from_study <- c(from_study, p$name)
    } else if (!identical(args[[p$name]], v)) {
      message(sprintf("%s: template %s's %s overrides the study default (%s).",
                      id, t$name, p$name, p$study))
    }
  }
  list(args = args, study = from_study)
}

# The template expanded for a report: its arguments with the study's
# defaults (`base`: the layers' content below the report) and the report's
# code lists (`codelists`: the layers' and its own)
.tpl_expand_for <- function(output, study, base, codelists) {
  ts <- .tpl_spec_of(output)
  t <- .tpl_get(ts$name, sprintf("%s: template:", output$id))
  sa <- .tpl_study_args(t, ts$args, base, output$id)
  x <- tfl_template_expand(ts$name, sa$args, codelists = codelists,
                           population = study$data$populations)
  if (!identical(t$kind, output$kind)) {
    .ard_stop(sprintf("%s: template %s makes a %s; the report is a %s.",
                      output$id, ts$name, t$kind, output$kind))
  }
  # a table's ARD normalized first, as tpl_*() does: the template's own
  # step when neither it nor the study's layers have one
  nz <- function(items) any(vapply(if (inherits(items, "tfl_r")) list() else items,
                                   function(it) identical(it$fn, "normalize_ard"), NA))
  if (identical(t$kind, "table") && !nz(x$content) && !nz(base)) {
    x$content <- c(list(tfl_item("normalize_ard", list(), from = paste0("template:", ts$name))),
                   x$content)
  }
  list(name = ts$name, args = sa$args, study = sa$study, x = x)
}

# A report's ard: over the template's: an entry of the report's replaces the
# template's of the same id, the others go after
.merge_ard <- function(tpl, own) {
  if (!length(tpl)) return(own)
  if (!length(own)) return(tpl)
  out <- tpl
  for (part in union(names(tpl), names(own))) {
    a <- tpl[[part]] %||% list()
    b <- own[[part]] %||% list()
    ids <- vapply(a, function(e) e$id %||% NA_character_, "")
    for (e in b) {
      k <- match(e$id %||% NA_character_, ids)
      if (!is.na(k)) a[[k]] <- e else a <- c(a, list(e))
    }
    out[[part]] <- a
  }
  out
}

#' A template report written out as its items
#'
#' The B use of a report's template SPEC (`template: {name, args}` in its
#' YAML file): the template's items -- with the study's defaults for the
#' parameters that take them, as [tfl_resolve_output()] gives them --
#' merged with the report's own `ard:`, `data:` and `content:`, and the
#' `template:` taken out.  Each item the template made keeps the mark
#' `from = "template:<name>"`.  The report is then a code SPEC: later
#' changes of the template do not reach it.
#'
#' @param output A `tfl_output` as read from its file ([tfl_read_output()]).
#' @param study Its study ([tfl_read_study_spec()]).
#' @return The `tfl_output`, unresolved (its `extends` kept): write it
#'   with [tfl_write_output()].  A report with no template SPEC comes back
#'   as it is.
#' @examples
#' d <- system.file("extdata", "spec-yaml", package = "tflspec")
#' s <- tfl_read_study_spec(d)
#' o <- tfl_output("T-9", "table", population = "SAF",
#'                 template = list(name = "demog", args = list(population = "SAF")))
#' b <- tfl_template_to_spec(o, s)
#' vapply(b$content, function(it) it$fn, "")
#' @export
tfl_template_to_spec <- function(output, study) {
  if (!inherits(output, "tfl_output")) .ard_stop("tfl_template_to_spec(): `output` is a tfl_output.")
  if (!inherits(study, "tfl_study_spec")) .ard_stop("tfl_template_to_spec(): `study` is a tfl_study_spec.")
  if (is.null(.tpl_spec_of(output))) return(output)
  below <- .layers_below(output, study)
  e <- .tpl_expand_for(output, study, below$acc$content,
                       .merge_map(below$acc$codelists, output$codelists))
  # the content: what the resolved report has from the template and its own
  # file, argument by argument -- the study's layers (a template's look they
  # win over among them) stay the layers', so the report resolves the same
  ro <- tfl_resolve_output(output, study, provenance = TRUE)
  output$ard <- .merge_ard(e$x$ard, output$ard)
  output$data <- .merge_map(e$x$data %||% list(), output$data %||% list())
  output$content <- if (inherits(ro$content, "tfl_r")) ro$content else .tpl_own_part(ro$content)
  output$template <- NULL
  output
}

# The resolved items' part from the template and the report's own file:
# a named-dots verb's keys from them, any other verb's item when it is
# theirs (its provenance, `arg_from`, dropped)
.tpl_own_part <- function(items) {
  mine <- function(f) startsWith(f, "template:") || identical(f, "own")
  out <- list()
  for (it in items) {
    keys <- names(it$args) %||% character()
    af <- it$arg_from %||% stats::setNames(rep(it$from %||% "own", length(keys)), keys)
    k <- if (.dots_named(it$fn) && length(keys)) keys[vapply(af[keys], mine, NA)] else NULL
    keep <- if (is.null(k)) mine(it$from %||% "own") else length(k) > 0L
    if (!keep) next
    if (!is.null(k)) it$args <- it$args[k]
    it$arg_from <- NULL
    it$below <- NULL
    out <- c(out, list(it))
  }
  out
}

# The A use's lines after the template's call: what the resolved items have
# that the template did not make -- a verb's whole item, or for a verb whose
# `...` is named its keys, by where they come from (`arg_from`).  `ok` is
# FALSE when the call cannot say it: the table's roles (table_plan()) or
# the normalization's arguments come from elsewhere.
.tpl_rest <- function(items, name) {
  tag <- paste0("template:", name)
  rest <- list()
  ok <- TRUE
  for (it in items) {
    af <- it$arg_from %||% character()
    keys <- names(it$args) %||% character()
    not_tpl <- if (length(keys)) keys[af[keys] != tag] else character()
    if (it$fn %in% c("normalize_ard", "table_plan")) {
      if (length(not_tpl) || (identical(it$fn, "table_plan") && !identical(it$from, tag))) ok <- FALSE
      next
    }
    if (.dots_named(it$fn) && length(keys)) {
      for (s in unique(af[not_tpl])) {
        rest <- c(rest, list(tfl_item(it$fn, it$args[keys[af == s]], from = s)))
      }
    } else if (!identical(it$from, tag)) {
      rest <- c(rest, list(it))
    }
  }
  list(items = rest, ok = ok)
}

# A template report's program (the A use): the template's call, the
# arguments its SPEC gives and the study's defaults it took (not the ones
# left at the template's own default), the code lists it reads at the head;
# then the study's and the report's own lines, each marked
.content_tpl_code <- function(output, data = "ard", plan = "plan", pipe = NULL) {
  tp <- attr(output, "template")
  op <- .ard_pipe_op(pipe)
  t <- .tpl_get(tp$name, "tfl_content_code()")
  args <- tp$args[names(tp$args) %in% names(t$params)]
  # the code lists the template reads: those its plan_levels() item names
  lv <- Filter(function(it) identical(it$fn, "plan_levels"), tp$items)
  vars <- setdiff(unlist(lapply(lv, function(it) names(it$args))), c("variable", ".drop_empty"))
  vars <- intersect(vars, names(output$codelists))
  levels <- .tpl_levels(vars, output$codelists)
  cl <- if (length(levels)) c("# the code lists: each value in its order, as the table shows it",
                              .codelists_lines(levels), "")
  call <- do.call(.spec_call, c(list(t$call %||% paste0("tpl_", tp$name), .spec_sym(data)), args,
                                if (length(levels)) list(codelists = stats::setNames(
                                  lapply(names(levels), function(v) .spec_sym(.cl_name(v))),
                                  names(levels)))))
  head <- c(sprintf("# template %s%s", tp$name,
                    if (length(tp$study)) paste0("; study defaults: ", paste(tp$study, collapse = ", "))
                    else ""),
            paste(plan, "<-", .spec_call_code(call, 0L, nchar(plan) + 4L)))
  rest <- .levels_prune(tp$rest, output)
  if (!length(rest)) return(.drop_attached_ns(c(cl, head)))
  steps <- .content_calls(rest)
  from <- vapply(rest, function(it) it$from %||% "own", "")
  lines <- vapply(steps, function(s) paste0("  ", .spec_call_code(.content_print(s))), "")
  note <- vapply(from, function(f) {
    if (identical(f, "own")) return("  # (own)")
    if (identical(f, "defaults")) return("  # (study default)")
    if (startsWith(f, "style:")) return(paste0("  # (study style: ", sub("^style:", "", f), ")"))
    paste0("  # (pattern ", sub("^pattern:", "", f), ")")
  }, "")
  body <- c(head[2L], unlist(lapply(seq_along(lines), function(i) c(note[i], lines[i]))))
  # the pipe: after every call but the last (a comment line takes none)
  is_call <- !startsWith(trimws(body), "#")
  last <- max(which(is_call))
  body[is_call & seq_along(body) < last] <- paste(body[is_call & seq_along(body) < last], op)
  .drop_attached_ns(c(cl, head[1L], unlist(strsplit(body, "\n", fixed = TRUE))))
}
