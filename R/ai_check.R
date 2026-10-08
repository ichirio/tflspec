# ============================================================================
#  AI-assisted drafting: the checks and the repair prompt
# ----------------------------------------------------------------------------
#  No new validation language: an answer is checked by the readers and the
#  checks a user's own spec goes through (tfl_read_toc(),
#  tfl_check_fig_design()), in the study's context, so its problems are
#  the ones the user would get on save.
# ============================================================================

#' Check a drafting answer in its context
#'
#' Runs tflspec's own checks on a read answer ([tfl_ai_parse()]), with the
#' study's context ([tfl_ai_context()]), and adds their problems to the
#' answer's.  Errors block applying the answer; warnings do not.
#'
#' * `toc`: the rows are written to a temporary `.csv` and read back with
#'   [tfl_read_toc()] (its errors -- a row with no output id but other
#'   cells filled, an id twice -- become problems on their rows); a kind
#'   it had to guess, and a row with no id it passes over as a heading,
#'   are warnings; a column outside the context's `toc_fields` is an
#'   error.
#' * `figure`: [tfl_check_fig_design()] on the design (with `adam`, the
#'   data-aware checks too: the variables and PARAMCDs it names); an
#'   `output_id` other than the context's is an error; a template not
#'   known, or not among the context's `templates`, and a dataset not in
#'   the context's `datasets` are warnings.
#'
#' @param answer A `tfl_ai_answer` ([tfl_ai_parse()]).
#' @param context The [tfl_ai_context()] the prompt was made with; `NULL`
#'   for none (only what needs no context is checked).
#' @param adam `figure`: the study's data ([tfl_read_adam()]), for the
#'   checks that look into it; `NULL` for none.
#' @return A data frame of the problems: `where` (`toc row 2 field type`,
#'   `layers[3] point field size` ...), `problem`, `severity` (`error` or
#'   `warning`); no rows when there are none.
#' @seealso [tfl_ai_repair_prompt()] to send them back.
#' @examples
#' answer <- c(
#'   "```yaml",
#'   "tflspec_ai: {task: toc, version: 1}",
#'   "toc:",
#'   "- {output_id: T-14-1-1, type: table, title: Demographics}",
#'   "- {output_id: T-14-1-1, type: tbl, title: Disposition}",
#'   "- {output_id: L-16-2-1, type: list, title: Deaths}",
#'   "assumptions: []",
#'   "```")
#' tfl_ai_check(tfl_ai_parse(answer, "toc"), tfl_ai_context("toc"))
#' @export
tfl_ai_check <- function(answer, context = NULL, adam = NULL) {
  if (!inherits(answer, "tfl_ai_answer")) {
    .ard_stop("tfl_ai_check(): `answer` is a tfl_ai_answer (tfl_ai_parse()).")
  }
  if (!is.null(context)) {
    if (!inherits(context, "tfl_ai_context")) {
      .ard_stop("tfl_ai_check(): `context` is a tfl_ai_context().")
    }
    if (!identical(context$task, answer$task)) {
      .ard_stop(sprintf(
        "tfl_ai_check(): the context is for the %s task; the answer is %s.",
        context$task, answer$task
      ))
    }
  }
  p <- answer$problems
  more <- switch(answer$task,
    toc = if (!is.null(answer$sheets)) .ai_check_toc(answer, context),
    figure = if (!is.null(answer$design)) {
      .ai_check_figure(answer, context, adam)
    }
  )
  p <- rbind(p, more %||% .ai_problems())
  p <- p[!duplicated(p), , drop = FALSE]
  rownames(p) <- NULL
  p
}

# ---- toc --------------------------------------------------------------------

.ai_check_toc <- function(answer, context) {
  p <- .ai_problems()
  d <- answer$sheets$toc
  if (!nrow(d)) return(.ai_problem(p, "toc", "has no rows"))
  filled <- names(d)[vapply(d, function(v) any(!is.na(v)), NA)]
  allowed <- context$toc_fields
  for (f in setdiff(filled, allowed %||% .toc_fields)) {
    rows <- which(!is.na(d[[f]]))
    p <- .ai_problem(p, sprintf("toc row %d field %s", rows, f), sprintf(
      "is not a column of this TOC; its columns are %s",
      paste(allowed, collapse = ", ")
    ))
  }
  if (!"output_id" %in% filled) {
    return(.ai_problem(p, "toc", "no row has an output_id"))
  }
  keep <- filled[filled %in% .toc_fields]
  csv <- tempfile(fileext = ".csv")
  on.exit(unlink(csv), add = TRUE)
  utils::write.csv(d[keep], csv, row.names = FALSE, na = "",
                   fileEncoding = "UTF-8")
  map <- stats::setNames(as.list(keep), keep)
  # a blank row is not a row of the file: the file's rows by the answer's
  rows <- which(rowSums(!is.na(d[keep])) > 0L)
  sp <- tryCatch(tfl_read_toc(csv, map = map), error = function(e) e)
  if (inherits(sp, "error")) {
    return(rbind(p, .ai_toc_error(conditionMessage(sp), d, rows)))
  }
  # a row with no id that says one thing at most is a heading to the reader
  for (i in which(is.na(d$output_id) & rowSums(!is.na(d[keep])) > 0L)) {
    p <- .ai_problem(p, sprintf("toc row %d field output_id", i), paste(
      "has no output_id: read as a section heading and left out; give the",
      "report its number, or put the heading in `section`"
    ), "warning")
  }
  for (id in attr(sp, "guessed")) {
    i <- which(d$output_id == id)[1L]
    tp <- sp$report$type[sp$report$output_id == id]
    p <- .ai_problem(p, sprintf("toc row %d field type", i), sprintf(
      "%s is not table, figure or listing; read as %s",
      if (is.na(d$type[i])) "blank" else sQuote(d$type[i], FALSE), tp
    ), "warning")
  }
  p
}

# tfl_read_toc()'s error as problems on the answer's rows.
.ai_toc_error <- function(msg, d, rows) {
  msg <- sub("^tfl_read_toc\\(\\): ", "", msg)
  first <- trimws(strsplit(msg, "\n", fixed = TRUE)[[1L]][1L])
  if (grepl("have no output id", msg, fixed = TRUE)) {
    n <- as.integer(strsplit(sub("^row\\(s\\) ([0-9, ]+) .*$", "\\1", first),
                             ",\\s*")[[1L]])
    at <- rows[n - 1L]
    return(.ai_problems(
      sprintf("toc row %d field output_id", at),
      "has no output_id but other cells filled", "error"
    ))
  }
  if (grepl("given twice", msg, fixed = TRUE)) {
    ids <- d$output_id[duplicated(d$output_id) & !is.na(d$output_id)]
    at <- which(d$output_id %in% ids)
    return(.ai_problems(
      sprintf("toc row %d field output_id", at),
      paste(sQuote(d$output_id[at], FALSE), "is given twice"), "error"
    ))
  }
  .ai_problems("toc", first, "error")
}

# ---- figure -----------------------------------------------------------------

.ai_check_figure <- function(answer, context, adam) {
  p <- .ai_problems()
  design <- answer$design
  want <- context$output_id
  other <- !is.null(answer$output_id) && !identical(answer$output_id, want)
  if (!is.null(want) && other) {
    p <- .ai_problem(p, "tflspec_ai", sprintf(
      "the answer is for %s; the prompt asked for %s", answer$output_id, want
    ))
  }
  tp <- design$template
  if (!is.null(tp)) {
    ok <- context$templates %||% tfl_fig_templates()$template
    if (!tp %in% ok) {
      p <- .ai_problem(p, "template", sprintf(
        "%s is not one of the templates offered", sQuote(tp, FALSE)
      ), "warning")
    }
  }
  if (!is.null(context$datasets)) {
    known <- unique(context$datasets$dataset)
    named <- .ai_fig_datasets(design)
    for (i in seq_len(nrow(named))) {
      if (!toupper(named$dataset[i]) %in% c(known, "DF")) {
        p <- .ai_problem(p, paste(named$part[i], "field dataset"), sprintf(
          "%s is not among the datasets given (%s)", named$dataset[i],
          paste(known, collapse = ", ")
        ), "warning")
      }
    }
  }
  chk <- tryCatch(tfl_check_fig_design(design, adam = adam),
                  error = function(e) e)
  if (inherits(chk, "error")) {
    return(.ai_problem(p, "design", paste(
      "could not be checked:", conditionMessage(chk)
    )))
  }
  if (nrow(chk)) {
    p <- rbind(p, .ai_problems(
      ifelse(is.na(chk$field) | !nzchar(chk$field), chk$part,
             paste(chk$part, "field", chk$field)),
      chk$problem, "error"
    ))
  }
  p
}

# The datasets a design reads: part, dataset.
.ai_fig_datasets <- function(design) {
  rows <- list()
  for (s in c("data", "stats")) {
    for (i in seq_along(design[[s]])) {
      pc <- design[[s]][[i]]
      if (!is.null(pc$dataset) && length(pc$dataset) == 1L) {
        rows[[length(rows) + 1L]] <- data.frame(
          part = sprintf("%s[%d] %s", s, i, pc$step %||% ""),
          dataset = as.character(pc$dataset), stringsAsFactors = FALSE
        )
      }
    }
  }
  if (length(rows)) do.call(rbind, rows) else
    data.frame(part = character(), dataset = character())
}

# ---- the repair prompt ------------------------------------------------------

#' The prompt that sends an answer's problems back
#'
#' One user turn for the same chat: the problems of an answer as a table,
#' the schema of the parts they are in (only those: the chat already has
#' the first prompt), and the request to return the whole block again,
#' corrected.  Paste it as the next message of the chat; in API mode it is
#' the next user message of the conversation.
#'
#' @param answer A `tfl_ai_answer` ([tfl_ai_parse()]).
#' @param problems The problems to send: by default those of
#'   [tfl_ai_check()] with `context`.
#' @param context The [tfl_ai_context()] of the prompt, for the checks.
#' @return The prompt, one string of class `tfl_ai_text`.
#' @examples
#' answer <- c("```yaml",
#'             "tflspec_ai: {task: toc, version: 1}",
#'             "toc:",
#'             "- {output_id: T-14-1-1, title: Demographics, colour: red}",
#'             "```")
#' tfl_ai_repair_prompt(tfl_ai_parse(answer, "toc"))
#' @export
tfl_ai_repair_prompt <- function(answer, problems = NULL, context = NULL) {
  if (!inherits(answer, "tfl_ai_answer")) {
    .ard_stop(paste(
      "tfl_ai_repair_prompt(): `answer` is a tfl_ai_answer",
      "(tfl_ai_parse())."
    ))
  }
  problems <- problems %||% tfl_ai_check(answer, context)
  ok <- is.data.frame(problems) &&
    all(c("where", "problem") %in% names(problems))
  if (!ok) {
    .ard_stop(paste(
      "tfl_ai_repair_prompt(): `problems` is a data frame with `where` and",
      "`problem`."
    ))
  }
  if (!nrow(problems)) {
    .ard_stop(paste(
      "tfl_ai_repair_prompt(): the answer has no problems to send back."
    ))
  }
  if (is.null(problems$severity)) problems$severity <- "error"
  problems <- problems[order(problems$severity != "error"), , drop = FALSE]
  task <- answer$task
  parts <- .ai_problem_parts(problems$where, task)
  schema <- if (length(parts)) {
    paste0("\nThe schema of ", paste(parts, collapse = ", "), ":\n\n",
           .ai_schema(task, parts), "\n")
  } else {
    ""
  }
  syntax <- any(problems$where %in% c("block", "answer"))
  fmt <- if (is.na(answer$format)) "yaml" else answer$format
  fill <- list(
    problems = paste(.ai_md_table(problems[c("severity", "where", "problem")]),
                     collapse = "\n"),
    schema = schema,
    format_rules = if (syntax) paste0("\n", .ai_format_rules(fmt)) else ""
  )
  .ai_text(sub("\n+$", "", .ai_fill(.ai_prompt_file("repair"), fill)))
}

# The schema parts the problems are in: `toc` for a toc; for a figure the
# pieces named (`layers[2] point field size` -> point), or the whole section
# when the piece is not known (`unknown layer`); `plot.add[1]` -> plot.
.ai_problem_parts <- function(where, task) {
  if (task == "toc") {
    return(if (any(grepl("^toc\\b", where))) "toc" else character())
  }
  re <- "^(template|data|stats|plot|layers)(\\[[0-9]+\\] ([A-Za-z0-9_.]+))?.*$"
  hit <- grepl(re, where)
  sec <- sub(re, "\\1", where[hit])
  pc <- sub(re, "\\3", where[hit])
  known <- unique(tfl_fig_parts()$piece)
  whole <- unique(sec[!nzchar(pc) | !pc %in% known])
  out <- c(whole, unique(pc[nzchar(pc) & pc %in% known & !sec %in% whole]))
  parts <- .ai_schema_parts(task)
  c(parts[parts %in% out], setdiff(out, parts))
}
