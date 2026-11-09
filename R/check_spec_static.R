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
#    at (a narrow subject id breaks every record over lines).
# ============================================================================

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
  args <- if (is.list(tpl)) (.tpl_spec_of(o)$args %||% tpl$args %||% list()) else list()
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
  do.call(rbind, rows)
}
