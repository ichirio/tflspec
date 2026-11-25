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
#    analysis set: a warning.
#  - (A2-T Q3) a report whose data rows carry several sets and that has no
#    BIGN analysis (the heading's set is BIGN's row's): a warning.
#  - (A2-T Q6) a template report's own row over the template's subjects row
#    naming another set than its template argument: an error.
#  - (A2, with the study: .check_set_rows()) a set that is none of
#    data.yml's, a data that names an analysis data and a set or dataset
#    beside it: errors; a TOC line naming another set than the report's
#    data: a warning.
# ============================================================================

# A2 R5: the analysis set of a figure or listing (`o`, as written): the rows
# of the check's frame (.check_rows())
.check_population_rows <- function(o, id) {
  rows <- list(.check_rows())
  add <- function(...) rows[[length(rows) + 1L]] <<- .check_rows(output_id = id, ...)
  # (A2-T Q3) several sets on the rows that make subjects: the heading's is
  # that of the row BIGN reads; no BIGN, none
  sets <- .rows_sets(.data_rows(o))
  if (length(sets) > 1L && is.na(.rows_set(.data_rows(o), o[["ard"]][["analyses"]])) &&
      is.na(.na_scalar(o$common$population_label))) {
    add(part = "data", item = "data$population", level = "warning", message = sprintf(paste0(
      "the %s's data rows carry several analysis sets (%s) and no BIGN analysis says which is the ",
      "report's: the page heading names none; add the BIGN analysis of the report's set, or write ",
      "the heading's set (`common: population_label`)"), o$kind %||% "report", paste(sets, collapse = ", ")))
  }
  if (!identical(o$kind, "figure") && !identical(o$kind, "listing")) return(do.call(rbind, rows))
  d <- if (is.list(o$data)) o$data else list()
  # (Q3) subjects' records -- a dataset, an analysis data, the report's code
  # -- and no analysis set on the data (canon 11-15): on the data, or on
  # its analysis data rows (the report's own ARD's, a template's rows).
  # Until `common` goes, its `population` and `population_label` (all
  # subjects) still count
  reads <- !is.null(d[["dataset"]]) || !is.null(d[["data"]]) ||
    (as.character(d[["source"]] %||% "ard")[1L] %in% c("dataset", "code"))
  row_sets <- unlist(lapply(.data_rows(o), function(a) a[["population"]]))
  if (reads && is.na(.report_set(o)) && !length(stats::na.omit(row_sets)) &&
      is.na(.na_scalar(o$common$population_label))) {
    add(part = "data", item = "data$population", level = "warning", message = paste0(
      "the ", o$kind, " reads subjects' records and has no analysis set: write `population:` on its ",
      "data or its analysis data rows (a population of spec/data.yml)"))
  }
  do.call(rbind, rows)
}

# A2 (the Codex review of 2026-11-26): a report's analysis set against the
# study (`study`, tfl_read_study_spec()'s), `o` as written.  The set its data
# says (.report_set()) is one of data.yml's (.pop_unknown_text(); its data
# rows' sets are the ARD definition's check); a data that reads one of the
# report's analysis data writes no dataset or set beside it (.data_both(),
# the rule of the sheets and of the AI's check); the TOC's line names the set
# the report's data names (the data's wins in the sheets)
.check_set_rows <- function(o, id, study) {
  rows <- list(.check_rows())
  add <- function(...) rows[[length(rows) + 1L]] <<- .check_rows(output_id = id, ...)
  pops <- unlist(lapply(study$data$populations %||% list(), function(p) .na_scalar(unlist(p$id))))
  pops <- pops[!is.na(pops)]
  d <- o[["data"]]
  map <- if (is.list(d) && !is.null(names(d))) d
  both <- .data_both(map[["data"]], map[["dataset"]], map[["population"]])
  if (length(both)) {
    add(part = "data", item = "data$data", level = "error",
        message = paste("the", o$kind %||% "report", .data_both_text(both)))
  } else if (is.na(.na_scalar(unlist(map[["data"]])))) {
    m <- .pop_unknown_text(.report_set(o), pops)
    if (!is.null(m)) add(part = "data", item = "data$population", level = "error",
                         message = paste("the report's analysis set:", m))
  }
  toc <- Filter(function(e) identical(e$id, id), study$toc$outputs %||% list())
  tp <- if (length(toc)) .na_scalar(unlist(toc[[1L]][["population"]])) else NA_character_
  op <- .report_set(o)
  if (!is.na(tp) && !is.na(op) && !identical(tp, op)) {
    add(part = "data", item = "toc$population", level = "warning", message = sprintf(paste0(
      "the TOC says analysis set %s and the report's data %s: the data's is the report's set (the ",
      "sheets and the page take it); write %s in the TOC, or the set the report means on its data"),
      tp, op, op))
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

.check_static_rows <- function(o, id) {
  rows <- list(.check_rows())
  add <- function(...) rows[[length(rows) + 1L]] <<- .check_rows(output_id = id, ...)
  # (a template by name alone, `template: km_simple`, or with its arguments)
  tpl <- o$template
  args <- if (is.list(tpl)) {
    tryCatch(.tpl_spec_of(o)$args, error = function(e) NULL) %||% tpl$args %||% list()
  } else {
    list()
  }
  # (A2-T Q6) the template's set argument and the report's own row over the
  # template's subjects row: one set, written once
  tw <- tryCatch(.tpl_set_twice_rows(o, id), error = function(e) NULL)
  if (!is.null(tw) && nrow(tw)) rows[[length(rows) + 1L]] <- tw
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
  rows[[length(rows) + 1L]] <- .check_population_rows(o, id)
  do.call(rbind, rows)
}
