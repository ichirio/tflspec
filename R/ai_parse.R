# ============================================================================
#  AI-assisted drafting: reading the answer
# ----------------------------------------------------------------------------
#  The answer is one fenced block (YAML or JSON) that starts with the
#  header `tflspec_ai: {task, version, output_id}`.  It is read as it is:
#  never repaired silently (no quoting of an unquoted `{mean}`), since a
#  repair would hide a wrong answer.  What cannot be read is a problem with
#  its place, for the repair prompt to send back.
# ============================================================================

# The keys an answer may have, besides the header and the assumptions.
.ai_answer_keys <- list(
  toc = "toc",
  figure = c("template", "ggplot2_version", "data", "stats", "plot",
             "layers", "plots", "compose", "note")
)

#' Read a drafting answer
#'
#' Reads the text a chat model returned for a drafting prompt
#' ([tfl_ai_prompt()]): the first fenced block (```` ```yaml ```` or
#' ```` ```json ````) that can be read and starts with the `tflspec_ai`
#' header; the whole text when no block does.  YAML and JSON are both
#' read, whichever the prompt asked for.
#'
#' Nothing is repaired: a block YAML cannot read (a `{mean}` not quoted, a
#' tab) is a problem naming the parser's line, as is a header of another
#' task or version, a column or key the task does not have, a mapping
#' where a value is wanted, or a block that is not closed (an answer cut
#' off).  The problems go to [tfl_ai_repair_prompt()].
#'
#' * `toc`: the rows become a data frame with [tfl_read_toc()]'s fields as
#'   columns, every value text (numbers as text, a list joined with
#'   `" | "`, missing `NA`).
#' * `figure`: the design becomes a [tfl_fig_design()] (the 0.0.12 shape
#'   `type` / `style` / `args` is read as one `figure` layer, with a
#'   warning); a `note` key is kept as the answer's `note`.
#'
#' @param text The answer: a string, or lines of text.
#' @param task One of [tfl_ai_tasks()]: the task the prompt asked for.
#' @return A `tfl_ai_answer`: a list with `task`, `header` (the
#'   `tflspec_ai` mapping), `output_id`, `sheets` (`toc`: a list holding the
#'   `toc` data frame) or `design` (`figure`), `note`, `assumptions`
#'   (character), `problems` (a data frame `where`, `problem`, `severity`
#'   -- `error` or `warning`), `format` (`yaml` or `json`) and `raw` (the
#'   block's text).  `sheets` / `design` is `NULL` when the answer could
#'   not be read.
#' @seealso [tfl_ai_check()] for the checks that need the study's context.
#' @examples
#' answer <- c(
#'   "Here is the TOC:",
#'   "```yaml",
#'   "tflspec_ai: {task: toc, version: 1}",
#'   "toc:",
#'   "- {output_id: T-14-1-1, type: table, title: Demographics,",
#'   "   population: Safety Population}",
#'   "- {output_id: F-14-2-1, type: figure, title: Time to Death}",
#'   "assumptions:",
#'   "- No footnotes are given in the SAP.",
#'   "```")
#' a <- tfl_ai_parse(answer, "toc")
#' a$sheets$toc[c("output_id", "type", "title")]
#' a$assumptions
#' @export
tfl_ai_parse <- function(text, task) {
  task <- .ai_task(task, "tfl_ai_parse()")
  if (!is.character(text)) {
    .ard_stop("tfl_ai_parse(): `text` is the answer as text.")
  }
  text <- paste(text[!is.na(text)], collapse = "\n")
  out <- list(
    task = task, header = NULL, output_id = NULL, sheets = NULL,
    design = NULL, note = NULL, assumptions = character(),
    problems = .ai_problems(), format = NA_character_, raw = NA_character_
  )
  found <- .ai_find_answer(text, task)
  out$problems <- found$problems
  if (is.null(found$value)) return(structure(out, class = "tfl_ai_answer"))
  out$format <- found$format
  out$raw <- found$raw
  x <- found$value
  out$header <- x$tflspec_ai
  out$output_id <- .ai_scalar_text(out$header$output_id)
  prob <- .ai_check_header(out$header, task)
  if (task == "figure" && is.null(out$output_id)) {
    prob <- .ai_problem(prob, "tflspec_ai", "the header has no output_id")
  }
  known <- c("tflspec_ai", "assumptions", .ai_answer_keys[[task]])
  if (task == "figure") known <- c(known, "type", "style", "args")
  for (k in setdiff(names(x), known)) {
    prob <- .ai_problem(prob, k, sprintf(
      "is not a key of a %s answer; the keys are %s", task,
      paste(setdiff(known, c("type", "style", "args")), collapse = ", ")
    ))
  }
  out$assumptions <- .ai_assumptions(x$assumptions)
  if (task == "toc") {
    toc <- .ai_rows(x$toc, "toc", .toc_fields)
    prob <- rbind(prob, toc$problems)
    out$sheets <- list(toc = toc$rows)
  } else {
    fig <- .ai_read_design(x)
    prob <- rbind(prob, fig$problems)
    out$design <- fig$design
    out$note <- .ai_scalar_text(x$note)
  }
  out$problems <- rbind(out$problems, prob)
  structure(out, class = "tfl_ai_answer")
}

#' @export
print.tfl_ai_answer <- function(x, ...) {
  cat(sprintf("<tfl_ai_answer: %s%s>\n", x$task,
              if (!is.null(x$output_id)) paste0(" ", x$output_id) else ""))
  if (!is.null(x$sheets)) {
    for (nm in names(x$sheets)) {
      cat(sprintf("%s: %d row(s)\n", nm, nrow(x$sheets[[nm]])))
    }
  }
  if (!is.null(x$design)) {
    n <- vapply(x$design[c("data", "stats", "layers")], length, 1L)
    cat(sprintf("design: %s\n", paste(names(n), n, sep = " ", collapse = ", ")))
  }
  if (length(x$assumptions)) {
    cat("assumptions:\n", paste0("- ", x$assumptions, "\n"), sep = "")
  }
  .ai_print_problems(x$problems)
  invisible(x)
}

.ai_print_problems <- function(p) {
  if (!nrow(p)) {
    cat("No problems.\n")
    return(invisible())
  }
  cat(sprintf("%d problem(s):\n", nrow(p)))
  cat(sprintf("- %s %s: %s\n", p$severity, p$where, p$problem), sep = "")
}

# ---- problems ---------------------------------------------------------------

.ai_problems <- function(where = character(), problem = character(),
                         severity = character()) {
  data.frame(where = where, problem = problem, severity = severity,
             stringsAsFactors = FALSE)
}

.ai_problem <- function(p, where, problem, severity = "error") {
  rbind(p, .ai_problems(where, problem, severity))
}

# ---- finding the block ------------------------------------------------------

# The fenced blocks of a text: list of (lang, text, closed, start line).
.ai_blocks <- function(text) {
  lines <- strsplit(text, "\r?\n")[[1L]]
  fence <- grepl("^\\s*(```|~~~)", lines)
  out <- list()
  i <- 1L
  while (i <= length(lines)) {
    if (!fence[i]) {
      i <- i + 1L
      next
    }
    lang <- tolower(trimws(sub("^\\s*(```|~~~)", "", lines[i])))
    j <- i + 1L
    while (j <= length(lines) && !fence[j]) j <- j + 1L
    closed <- j <= length(lines)
    body <- if (j > i + 1L) lines[seq(i + 1L, j - 1L)] else character()
    out[[length(out) + 1L]] <- list(
      lang = lang, text = paste(body, collapse = "\n"), closed = closed,
      line = i
    )
    i <- j + 1L
  }
  out
}

# The answer: the first block that reads and has the header; else the whole
# text.  A block that has the header but does not read is the problem.
.ai_find_answer <- function(text, task) {
  blocks <- .ai_blocks(text)
  failed <- NULL
  for (b in blocks) {
    r <- .ai_read_text(b$text, b$lang, task)
    if (.ai_has_header(r$value)) {
      p <- .ai_problems()
      if (!b$closed) {
        p <- .ai_problem(p, "block", paste0(
          "the block is not closed (no ``` after it): the answer may be cut ",
          "off; return it whole"
        ))
      }
      return(list(value = r$value, format = r$format, raw = b$text,
                  problems = p))
    }
    if (is.null(failed) && grepl("tflspec_ai", b$text, fixed = TRUE)) {
      failed <- list(block = b, error = r$error)
    }
  }
  if (is.null(failed)) {
    r <- .ai_read_text(text, "", task)
    if (.ai_has_header(r$value)) {
      return(list(value = r$value, format = r$format, raw = text,
                  problems = .ai_problems()))
    }
    if (grepl("tflspec_ai", text, fixed = TRUE)) {
      failed <- list(block = NULL, error = r$error)
    }
  }
  p <- if (!is.null(failed) && !is.null(failed$error)) {
    .ai_problems("block", paste0("the block could not be read: ", failed$error),
                 "error")
  } else if (!is.null(failed)) {
    .ai_problems("block", "the block does not start with the tflspec_ai header",
                 "error")
  } else {
    .ai_problems("answer", paste0(
      "no answer block found: one fenced block that starts with ",
      "`tflspec_ai: {task: ", task, ", version: 1 ...}`"
    ), "error")
  }
  list(value = NULL, problems = p)
}

.ai_has_header <- function(x) is.list(x) && !is.null(x$tflspec_ai)

# A block's text read as JSON (when its language says so or it starts with
# `{`) or YAML; the error message, with its line, when it does not read.
.ai_read_text <- function(text, lang, task) {
  json <- lang == "json" || (!nzchar(lang) && grepl("^\\s*\\{", text) &&
                               grepl("\\}\\s*$", text))
  if (json) {
    v <- tryCatch(
      jsonlite::fromJSON(text, simplifyVector = TRUE,
                         simplifyDataFrame = FALSE, simplifyMatrix = FALSE),
      error = function(e) e
    )
    if (inherits(v, "error")) {
      return(list(error = paste("JSON:", .ai_one_line(conditionMessage(v)))))
    }
    return(list(value = .ai_from_json(v), format = "json"))
  }
  handlers <- if (task == "figure") list(r = tfl_fig_r)
  v <- tryCatch(
    yaml::yaml.load(text, handlers = handlers),
    error = function(e) e, warning = function(w) w
  )
  if (inherits(v, "condition")) {
    return(list(error = paste("YAML:", .ai_one_line(conditionMessage(v)))))
  }
  list(value = v, format = "yaml")
}

.ai_one_line <- function(x) trimws(gsub("\\s+", " ", x))

# JSON's "!r code" strings as raw R (tfl_fig_r), as YAML's !r tag.
.ai_from_json <- function(v) {
  if (is.list(v)) return(lapply(v, .ai_from_json))
  tagged <- is.character(v) && length(v) == 1L && !is.na(v) &&
    startsWith(v, "!r ")
  if (tagged) {
    return(tfl_fig_r(substring(v, 4L)))
  }
  v
}

.ai_check_header <- function(h, task) {
  p <- .ai_problems()
  if (!is.list(h) || is.null(names(h))) {
    return(.ai_problem(p, "tflspec_ai", paste0(
      "the header is a mapping: {task: ", task, ", version: 1 ...}"
    )))
  }
  v <- .ai_scalar_text(h$version)
  if (is.null(v)) {
    p <- .ai_problem(p, "tflspec_ai", "the header has no version")
  } else if (!identical(v, as.character(.ai_version))) {
    p <- .ai_problem(p, "tflspec_ai", sprintf(
      "version %s is not known; this tflspec reads version %d", v, .ai_version
    ))
  }
  tk <- .ai_scalar_text(h$task)
  if (!identical(tk, task)) {
    p <- .ai_problem(p, "tflspec_ai", sprintf(
      "the answer is for the task %s; the prompt asked for %s",
      if (is.null(tk)) "(none)" else sQuote(tk, FALSE), sQuote(task, FALSE)
    ))
  }
  p
}

# ---- values -----------------------------------------------------------------

# One value as text: NULL for none; a number as R writes it.
.ai_scalar_text <- function(v) {
  if (is.null(v) || !length(v) || is.list(v)) return(NULL)
  v <- v[1L]
  if (is.na(v)) return(NULL)
  if (is.logical(v)) return(if (v) "TRUE" else "FALSE")
  if (is.numeric(v)) return(format(v, scientific = FALSE, trim = TRUE))
  as.character(v)
}

# A cell as text: NA for none; several values joined with " | "; NULL with
# a problem for a mapping or a nested list.
.ai_cell <- function(v) {
  if (is.null(v) || !length(v)) return(list(value = NA_character_))
  if (is.list(v)) {
    if (!is.null(names(v)) || any(vapply(v, function(z) {
      is.list(z) || length(z) != 1L
    }, NA))) {
      return(list(
        problem = "a mapping or a nested list where a value is wanted"
      ))
    }
    v <- unlist(v)
  }
  txt <- vapply(seq_along(v), function(i) .ai_scalar_text(v[i]) %||% "", "")
  txt <- trimws(txt[nzchar(trimws(txt))])
  list(value = if (length(txt)) paste(txt, collapse = " | ") else NA_character_)
}

# A list of rows as a data frame with `cols`, all character.
.ai_rows <- function(x, sheet, cols) {
  p <- .ai_problems()
  empty <- as.data.frame(
    stats::setNames(rep(list(character()), length(cols)), cols),
    stringsAsFactors = FALSE
  )
  if (is.null(x)) {
    return(list(rows = empty, problems = .ai_problem(
      p, sheet, sprintf("the answer has no `%s`", sheet)
    )))
  }
  if (!is.list(x) || !is.null(names(x))) {
    return(list(rows = empty, problems = .ai_problem(
      p, sheet, "is a list of rows, one mapping a row"
    )))
  }
  rows <- vector("list", length(x))
  for (i in seq_along(x)) {
    r <- x[[i]]
    at <- sprintf("%s row %d", sheet, i)
    if (!is.list(r) || is.null(names(r))) {
      p <- .ai_problem(p, at, "is not a mapping (`{column: value, ...}`)")
      next
    }
    row <- stats::setNames(rep(NA_character_, length(cols)), cols)
    for (f in names(r)) {
      if (!f %in% cols) {
        p <- .ai_problem(p, paste(at, "field", f), sprintf(
          "is not a column of %s; its columns are %s", sheet,
          paste(cols, collapse = ", ")
        ))
        next
      }
      cell <- .ai_cell(r[[f]])
      if (!is.null(cell$problem)) {
        p <- .ai_problem(p, paste(at, "field", f), cell$problem)
      } else {
        row[[f]] <- cell$value
      }
    }
    rows[[i]] <- row
  }
  rows <- rows[!vapply(rows, is.null, NA)]
  d <- if (length(rows)) {
    as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  } else {
    empty
  }
  rownames(d) <- NULL
  list(rows = d, problems = p)
}

.ai_assumptions <- function(x) {
  if (is.null(x) || !length(x)) return(character())
  v <- vapply(as.list(unlist(x)), function(z) .ai_scalar_text(z) %||% "", "")
  v <- trimws(v)
  v[nzchar(v)]
}

# ---- a figure ---------------------------------------------------------------

.ai_read_design <- function(x) {
  p <- .ai_problems()
  for (k in c("data", "stats", "layers")) {
    v <- x[[k]]
    if (is.null(v)) next
    maps <- vapply(v, function(z) is.list(z) && !is.null(names(z)), NA)
    if (!is.list(v) || !is.null(names(v)) || !all(maps)) {
      p <- .ai_problem(p, k, "is a list of pieces, each a mapping")
      x[[k]] <- NULL
    }
  }
  if (!is.null(x$plot) && (!is.list(x$plot) || is.null(names(x$plot)))) {
    p <- .ai_problem(p, "plot", "is one mapping of settings")
    x$plot <- NULL
  }
  if (!is.null(x$type)) {
    p <- .ai_problem(p, "design", paste0(
      "the 0.0.12 shape (type / style / args) was read as one `figure` layer; ",
      "write the design's parts instead"
    ), "warning")
  }
  design <- tryCatch(.fig_design_from_list(x[setdiff(names(x), c(
    "tflspec_ai", "assumptions", "note"
  ))]), error = function(e) e)
  if (inherits(design, "error")) {
    return(list(design = NULL, problems = .ai_problem(
      p, "design", paste("could not be read:", conditionMessage(design))
    )))
  }
  list(design = design, problems = p)
}
