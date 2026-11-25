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
#  - plan_digits() by the values of a variable no analysis groups its rows
#    by (`PARAM` where the ARD is by `PARAMCD`): an error, the variables it
#    may name (.check_digit_key_rows(), on the report resolved).
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
  do.call(rbind, rows)
}

# plan_digits() by a row key's value (`PARAMCD: {ALT: 1, BILI: {mean: 2}}`):
# rtfreporter reaches a value through a column of the ARD, and the ARD's row
# columns are its analyses' `by` (and `strata`).  A key given values (a map
# whose names are not statistics) that is none of those -- `PARAM` where the
# analyses are by `PARAMCD`, as param_change's and shift's are -- matches
# nothing and stops the build: an error here, with the variables it may name.
# `ro` is the report resolved (a template's analyses with the report's).  Not
# judged where the ARD's columns cannot be known: no analyses, a report that
# reads a data frame of its own, or an analysis written as code.
.check_digit_key_rows <- function(ro, id) {
  an <- ro$ard$analyses
  if (!identical(ro$kind %||% "table", "table") || !length(an) || !is.list(an) ||
      inherits(an, "tfl_r") || !is.list(ro$content) || inherits(ro$content, "tfl_r") ||
      !identical((if (is.list(ro$data)) ro$data$source) %||% "ard", "ard")) return(NULL)
  coded <- vapply(an, function(a) !is.list(a) || identical(a$method, "custom") || !is.null(a$code), NA)
  if (any(coded)) return(NULL)
  cols <- function(f) unique(unlist(lapply(an, function(a) lapply(unlist(a[[f]]), .split_bar))))
  keys <- setdiff(c(cols("by"), cols("strata")), "")
  vars <- cols("variables")
  stats <- tfl_ard_statistics()$statistic
  out <- list()
  for (it in Filter(function(i) is.list(i) && identical(i$fn, "plan_digits"), ro$content)) {
    for (k in setdiff(names(it$args), c("", "default", ".rows", "rounding", keys, vars))) {
      v <- it$args[[k]]
      if (!is.list(v) || is.null(names(v)) || all(names(v) %in% stats)) next
      out[[length(out) + 1L]] <- .check_rows(
        output_id = id, part = "content", item = sprintf("plan_digits(%s)", k), level = "error",
        message = sprintf(paste0(
          "plan_digits(%s = ) gives digits by a value of %s, but no analysis groups its rows by %s, ",
          "so it matches nothing and the build stops. Key the digits by a variable of the analyses' ",
          "`by` (%s), each value as the data holds it, the code: `<variable>: {<code>: 1}`"),
          k, k, k, if (length(keys)) paste(keys, collapse = ", ") else "none"))
    }
  }
  do.call(rbind, out)
}
