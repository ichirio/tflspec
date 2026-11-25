# ============================================================================
#  tfl_check_spec()'s look at a report as written, before any build: what
#  stops the build later with a message far from the cause, or what a
#  draft says twice (the aitest-03 drafts, #534)
# ----------------------------------------------------------------------------
#  - a cell template in Python's percent form, `{p:.1%}`: an error, the
#    tflspec form `{p:.1f%}` in the message;
#  - a figure template's own argument off (`censor_marks: false`) and the
#    same piece added as a layer: an error, one of the two;
#  - a KM figure's time axis label with no unit: a warning;
#  - a listing whose columns all carry a width: a note, the widths to look
#    at (a narrow subject id breaks every record over lines);
#  - (A2, R5) a figure or listing that reads subjects' records with no
#    analysis set: a warning; a data step that keeps rows by a variable of a
#    study population's condition (`flag: SAFFL` on the records): a warning,
#    the analysis set is the report's `common: population`.
# ============================================================================

# The variables each study population's condition reads (`populations:` of
# spec/data.yml: a list of maps, `id` and `where`), by its id; a population
# with no condition reads none
.check_pop_vars <- function(pops) {
  out <- list()
  for (p in pops) {
    if (!is.list(p) || is.null(p$id)) next
    w <- .na_scalar(unlist(p$where))
    v <- if (!is.na(w)) tryCatch(all.vars(str2lang(w)), error = function(e) character())
    if (length(v)) out[[as.character(p$id)[1L]]] <- v
  }
  out
}

# The data steps that keep rows by a condition (the figure design's pieces,
# R/fig_model.R) and the field each one's condition is in: a flag step its
# variable, a filter its R
.check_row_keeps <- c(flag = "variable", filter = "expr")

# A2 R5: the analysis set of a figure or listing (`o`, as written) and of
# its data steps: the rows of the check's frame (.check_rows())
.check_population_rows <- function(o, id, pops) {
  rows <- list(.check_rows())
  add <- function(...) rows[[length(rows) + 1L]] <<- .check_rows(output_id = id, ...)
  if (!identical(o$kind, "figure") && !identical(o$kind, "listing")) return(rows[[1L]])
  d <- if (is.list(o$data)) o$data else list()
  rp <- .out_population(o)
  # (Q3) subjects' records -- a dataset, an analysis data, the report's code
  # -- and no analysis set: neither the report's nor the data's, nor the
  # text of one that shows all subjects (`common: population_label`)
  reads <- !is.null(d$dataset) || !is.null(d$data) ||
    (as.character(d$source %||% "ard")[1L] %in% c("dataset", "code"))
  if (reads && all(is.na(unlist(.data_population(d, o)))) &&
      is.na(.na_scalar(o$common$population_label))) {
    add(part = "data", item = "common$population", level = "warning", message = paste0(
      "the ", o$kind, " reads subjects' records and has no analysis set: write `common: population` ",
      "(a population of spec/data.yml), whose subjects the records are joined to; for all subjects, ",
      "`common: population_label`"))
  }
  # (Q4) a step that keeps the rows of a population by the records' own
  # columns (the subjects outside it, and a record's own arm, stay)
  pv <- .check_pop_vars(pops)
  steps <- if (is.list(d$steps)) d$steps else list()
  for (k in seq_along(steps)) {
    s <- steps[[k]]
    if (!is.list(s) || !is.character(s$step) || !s$step[1L] %in% names(.check_row_keeps)) next
    v <- .na_scalar(unlist(s[[.check_row_keeps[[s$step[1L]]]]]))
    if (is.na(v)) next
    used <- tryCatch(all.vars(str2lang(v)), error = function(e) character())
    hit <- names(pv)[vapply(pv, function(p) any(used %in% p), NA)]
    if (!length(hit)) next
    add(part = "data", item = sprintf("data$steps[%d]", k), level = "warning", message = sprintf(paste0(
      "step %d (`%s: %s`) keeps the rows of population %s by the records' own %s: the analysis set is ",
      "the report's `common: population`%s (its subjects from their dataset, the records joined to ",
      "them); remove the step%s (a `flag` step is for a record flag, ANL01FL)"),
      k, s$step[1L], v, paste(hit, collapse = " / "), paste(intersect(used, unlist(pv[hit])), collapse = ", "),
      if (!is.na(rp)) sprintf(" (%s)", rp) else "",
      if (is.na(rp)) sprintf(" and write `common: population: %s`", hit[1L]) else ""))
  }
  do.call(rbind, rows)
}

# Every text in a report's content and template arguments, with where it is
.check_texts <- function(x, at = character()) {
  if (is.character(x)) {
    return(if (length(x)) data.frame(at = paste(at, collapse = "$"), text = as.character(x),
                                     stringsAsFactors = FALSE))
  }
  if (!is.list(x) || !length(x)) return(NULL)
  nm <- names(x) %||% rep("", length(x))
  # (an item by its function's name: content$plan_cells$args$categorical)
  fn <- vapply(x, function(e) if (is.list(e) && is.character(e$fn)) e$fn[1L] else "", "")
  nm[!nzchar(nm)] <- ifelse(nzchar(fn), fn, as.character(seq_along(x)))[!nzchar(nm)]
  do.call(rbind, lapply(seq_along(x), function(i) .check_texts(x[[i]], c(at, nm[i]))))
}

# The items of a list, nested anywhere, whose `fn` is one of `fns`
.check_items <- function(x, fns) {
  if (!is.list(x)) return(list())
  own <- if (is.character(x$fn) && x$fn %in% fns) list(x)
  c(own, unlist(lapply(unclass(x), .check_items, fns = fns), recursive = FALSE))
}

.check_static_rows <- function(o, id, study = NULL) {
  rows <- list(.check_rows())
  add <- function(...) rows[[length(rows) + 1L]] <<- .check_rows(output_id = id, ...)
  # (a template by name alone, `template: km_simple`, or with its arguments)
  tpl <- o$template
  args <- if (is.list(tpl)) {
    tryCatch(.tpl_spec_of(o)$args, error = function(e) NULL) %||% tpl$args %||% list()
  } else {
    list()
  }
  # the analysis set is the report's `common: population`: a template argument that
  # differs from it is a conflict (the one equal to it, or with no report
  # `common: population`, is used and not said)
  if ("population" %in% names(args) && is.list(tpl) && is.character(tpl$name) &&
      !is.null(tryCatch(.tpl_get(tpl$name, "tfl_check_spec()")$params$population,
                        error = function(e) NULL))) {
    rp <- .out_population(o)
    if (length(rp) == 1L && !is.na(rp) && !identical(as.character(args$population), as.character(rp))) {
      add(part = "template", item = "population", level = "warning", message = sprintf(paste0(
        "template argument `population` (%s) differs from the report's `common: population` (%s): the report is built ",
        "with the template's; make the report's `common: population` the one to use and remove the template's"),
        args$population[1L], rp))
    }
  }
  tx <- .check_texts(list(content = o$content, template = args, layers = o$layers))
  # {p:.1%}: a precision then `%`, no type between them
  bad <- if (!is.null(tx)) tx[grepl("[{][^{}:]+:[^{}]*[.][0-9]+%[}]", tx$text), , drop = FALSE]
  for (i in seq_len(NROW(bad))) {
    # (Python's form prints a sign; tflspec's `.1f%` does not: the sign after the brace)
    fix <- gsub("(?<=[.][0-9]|[.][0-9][0-9])%[}]", "f%}%", bad$text[i], perl = TRUE)
    add(part = "content", item = bad$at[i], level = "error", message = sprintf(paste0(
      "the cell template \"%s\" is Python's percent form, which the build refuses ",
      "(Unknown format spec): write \"%s\" (`.1f%%`: times 100, one decimal, no %% sign; ",
      "the sign after the brace)"), bad$text[i], fix))
  }
  # every cell template: its statistics, its formats (R/check_spec_cells.R)
  rows[[length(rows) + 1L]] <- .check_cell_rows(o, id)
  # a figure's piece turned off by its argument and added as a layer
  if (identical(o$kind, "figure") && length(o$layers)) {
    lay <- vapply(o$layers, function(l) as.character(l$layer %||% ""), "")
    both <- c(censor_marks = "censor_mark", risk_table = "risk_table")
    for (a in names(both)) {
      if (isFALSE(args[[a]]) && both[[a]] %in% lay) {
        add(part = "figure", item = paste0("template$args$", a), level = "error",
            message = sprintf(paste0(
              "`%s: false` and a `%s` layer say opposite things: the figure shows neither. ",
              "Write one: `%s: true` (and no layer), or the layer with no `%s`"),
              a, both[[a]], a, a))
      }
    }
  }
  # a KM figure's time axis with no unit
  tn <- as.character((if (is.list(tpl)) tpl$name else tpl) %||% "")
  xl <- args$x_label %||% o$plot$x_label
  if (identical(o$kind, "figure") && length(tn) == 1L && grepl("^km", tn) &&
      is.character(xl) && length(xl) == 1L && !grepl("(", xl, fixed = TRUE)) {
    add(part = "figure", item = "x_label", level = "warning", message = sprintf(paste0(
      "the time axis \"%s\" has no unit: write it with the data's (AVALU), ",
      "\"%s (Months)\""), xl, xl))
  }
  # a listing whose every column has a width
  if (identical(o$kind, "listing")) {
    lc <- .check_items(o$content, "listing_col")
    w <- vapply(lc, function(it) !is.null(it$args$width), NA)
    if (length(w) > 2L && all(w)) {
      add(part = "content", item = "listing_col(width)", level = "note", message = paste0(
        "every column has a width: check that each is wide enough for its longest value ",
        "(a subject id on one line), or leave the widths to the default"))
    }
  }
  # a figure's or listing's analysis set (A2 R5)
  rows[[length(rows) + 1L]] <- .check_population_rows(o, id, study$data$populations)
  do.call(rbind, rows)
}
