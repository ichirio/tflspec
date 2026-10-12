# ============================================================================
#  AI-assisted drafting: the tasks and the study facts a prompt carries
# ----------------------------------------------------------------------------
#  An AI chat (or, later, an API call made by tflplanner) drafts part of a
#  specification from the study's documents.  tflspec holds the network-free
#  core: the prompt is text, the answer is text.
#
#    tfl_ai_context()        the study facts, checked     (this file)
#    tfl_ai_prompt()         the prompt                   (R/ai_prompt.R)
#    tfl_ai_parse()          the answer read              (R/ai_parse.R)
#    tfl_ai_check()          its problems                 (R/ai_check.R)
#    tfl_ai_repair_prompt()  the problems sent back       (R/ai_check.R)
#    tfl_ai_diff()           what it changes              (R/ai_diff.R)
#
#  The design: issues 182 of tflspec and 283 of tflplanner.
# ============================================================================

# The version of the answer's shape (the header's `version`).
.ai_version <- 1L

# The tasks: what each fills, what checks it, what keys its items.
.ai_task_table <- function() {
  data.frame(
    task = c("toc", "figure"),
    fills = c(
      paste(
        "TOC rows: output_id, type, title lines, population, footnote lines,",
        "section, datasets, label"
      ),
      "one figure's whole design (data, stats, plot, layers)"
    ),
    checked_by = c("tfl_read_toc()", "tfl_check_fig_design()"),
    keyed_by = c("output_id", "section, then step / layer and position"),
    per = c("study", "report"),
    stringsAsFactors = FALSE
  )
}

# The tasks designed for a later version (ichirio/tflspec#182).
.ai_tasks_later <- c("ard", "table")

#' The tasks AI-assisted drafting knows
#'
#' What each task of the drafting functions ([tfl_ai_prompt()],
#' [tfl_ai_parse()], [tfl_ai_check()], [tfl_ai_diff()]) fills, which of
#' tflspec's checks reads its answer, and what its items are matched by.
#'
#' * `toc`: the study's table of contents, one row a report, read through
#'   [tfl_read_toc()] (one prompt for the study).
#' * `figure`: one report's whole figure design ([tfl_fig_design()]),
#'   checked by [tfl_check_fig_design()].
#'
#' The `ard` and `table` tasks come in a later version.
#'
#' @return A data frame: `task`, `fills`, `checked_by`, `keyed_by`, `per`
#'   (`study` or `report`: one prompt a study, or one a report).
#' @examples
#' tfl_ai_tasks()
#' @export
tfl_ai_tasks <- function() .ai_task_table()

# `task` checked: one of the tasks.
.ai_task <- function(task, fn) {
  if (!is.character(task) || length(task) != 1L || is.na(task)) {
    .ard_stop(sprintf(
      "%s: `task` is one of %s.", fn,
      paste(sQuote(.ai_task_table()$task), collapse = ", ")
    ))
  }
  if (task %in% .ai_tasks_later) {
    .ard_stop(sprintf(
      "%s: the `%s` task is not available yet; the tasks are %s.",
      fn, task, paste(sQuote(.ai_task_table()$task), collapse = ", ")
    ))
  }
  if (!task %in% .ai_task_table()$task) {
    .ard_stop(sprintf(
      "%s: no task %s; the tasks are %s.", fn, sQuote(task),
      paste(sQuote(.ai_task_table()$task), collapse = ", ")
    ))
  }
  task
}

# The context's fields a task reads.
.ai_context_fields <- list(
  toc = c("study", "toc_fields", "reports", "language"),
  figure = c(
    "study", "output_id", "titles", "population", "datasets", "design",
    "templates", "language"
  )
)

# The TOC columns a toc answer fills when the context names none.
.ai_toc_default_fields <- c(
  "output_id", "type", "title", "population", "footnote", "section",
  "datasets", "label"
)

#' The study facts a drafting prompt carries
#'
#' One constructor for what [tfl_ai_prompt()] tells the model about the
#' study and the report, and what [tfl_ai_check()] checks the answer
#' against, so a caller cannot pass the wrong shape and a prompt cannot
#' read a field that is not there.  Each task reads its own fields; a
#' field another task reads is an error.  Every field is optional but
#' `output_id` for a figure.
#'
#' Nothing here is sent anywhere: the context becomes text in the prompt,
#' which the user reads before copying it.  Only names and labels are
#' carried, never the data's values: a dataset's `levels` (a variable's
#' values, such as its PARAMCDs) are there only when the caller gives them.
#'
#' @param task One of [tfl_ai_tasks()]: `"toc"` or `"figure"`.
#' @param study The study: a named list or character vector with
#'   `study_id` and `title` (either may be missing).
#' @param output_id `figure`: the report's id (required).
#' @param titles `figure`: the report's title lines, as the TOC has them.
#' @param population `figure`: its analysis set: one label, or a named list
#'   or vector with `id`, `label` and `where` (the condition, such as
#'   `FASFL == "Y"`).
#' @param datasets `figure`: the datasets and variables the design may
#'   name.  Either a data frame, one row a variable, with `dataset` and
#'   `variable` and optionally `label`, `kind`, `levels` (` | ` between the
#'   values) and `dataset_label`; or the ADaM datasets themselves, a named
#'   list of data frames, of which only the names, the labels (the
#'   `label` attribute) and the kinds are taken.
#' @param design `figure`: the current design (a [tfl_fig_design()]), when
#'   there is one: the prompt shows it and asks for it whole, changed.
#' @param templates `figure`: the templates the answer may start from
#'   (names of [tfl_fig_templates()]); `NULL` for all.
#' @param toc_fields `toc`: the columns the answer fills, among
#'   `output_id` (always), `type`, `title`, `population`, `footnote`,
#'   `program`, `file`, `note`, `section`, `datasets`, `label`
#'   ([tfl_read_toc()]'s fields); `NULL` for `output_id`, `type`, `title`,
#'   `population`, `footnote`, `section`, `datasets`, `label`.
#' @param reports `toc`: the reports already in the TOC: their ids, or a
#'   data frame with `output_id` and optionally `title`.
#' @param language The language the model writes its assumptions in, such
#'   as `"Japanese"`; `NULL`: the language of the user's message.  The
#'   prompt itself is English.
#' @return A `tfl_ai_context`: a list with `task` and the fields given.
#' @seealso [tfl_ai_prompt()].
#' @examples
#' tfl_ai_context("toc", study = list(
#'   study_id = "CDISCPILOT01", title = "Xanomeline in Alzheimer's disease"
#' ))
#'
#' adam <- tfl_example_adam()
#' tfl_ai_context("figure", output_id = "F-14-2-1",
#'                titles = "Kaplan-Meier Plot of Time to Death",
#'                population = list(id = "FAS", label = "Full Analysis Set",
#'                                  where = "FASFL == \"Y\""),
#'                datasets = adam["ADTTE"])
#' @export
tfl_ai_context <- function(task, study = NULL, output_id = NULL,
                           titles = NULL, population = NULL, datasets = NULL,
                           design = NULL, templates = NULL, toc_fields = NULL,
                           reports = NULL, language = NULL) {
  fn <- "tfl_ai_context()"
  task <- .ai_task(task, fn)
  given <- list(
    study = study, output_id = output_id, titles = titles,
    population = population, datasets = datasets, design = design,
    templates = templates, toc_fields = toc_fields, reports = reports,
    language = language
  )
  given <- given[!vapply(given, is.null, NA)]
  other <- setdiff(names(given), .ai_context_fields[[task]])
  if (length(other)) {
    .ard_stop(sprintf(
      "%s: a %s context has no %s; its fields are %s.", fn, task,
      paste(sQuote(other), collapse = ", "),
      paste(.ai_context_fields[[task]], collapse = ", ")
    ))
  }
  out <- list(task = task)
  out$study <- .ai_ctx_study(given$study, fn)
  out$output_id <- .ai_ctx_string(given$output_id, "output_id", fn)
  if (task == "figure" && is.null(out$output_id)) {
    .ard_stop(sprintf("%s: a figure context needs the `output_id`.", fn))
  }
  if (!is.null(given$titles)) {
    out$titles <- .ai_ctx_text(given$titles, "titles", fn)
  }
  out$population <- .ai_ctx_population(given$population, fn)
  out$datasets <- .ai_ctx_datasets(given$datasets, fn)
  if (!is.null(given$design)) {
    if (!inherits(given$design, "tfl_fig_design")) {
      .ard_stop(sprintf("%s: `design` is a tfl_fig_design().", fn))
    }
    out$design <- given$design
  }
  out$templates <- .ai_ctx_templates(given$templates, fn)
  if (task == "toc") {
    out$toc_fields <- .ai_ctx_toc_fields(given$toc_fields, fn)
  }
  out$reports <- .ai_ctx_reports(given$reports, fn)
  out$language <- .ai_ctx_string(given$language, "language", fn)
  out <- out[!vapply(out, is.null, NA)]
  structure(out, class = "tfl_ai_context")
}

#' @export
print.tfl_ai_context <- function(x, ...) {
  cat(sprintf("<tfl_ai_context: %s>\n", x$task))
  cat(.ai_context_text(x), sep = "\n")
  invisible(x)
}

# one non-blank string, or NULL
.ai_ctx_string <- function(x, what, fn) {
  if (is.null(x)) return(NULL)
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    .ard_stop(sprintf("%s: `%s` is one non-blank string.", fn, what))
  }
  trimws(x)
}

# lines of text: a character vector, blanks dropped
.ai_ctx_text <- function(x, what, fn) {
  if (!is.character(x)) {
    .ard_stop(sprintf("%s: `%s` is a character vector.", fn, what))
  }
  x <- trimws(x[!is.na(x)])
  x[nzchar(x)]
}

# a named list or vector, the names among `keys`, each one string
.ai_ctx_record <- function(x, keys, what, fn) {
  x <- as.list(x)
  if (!length(x) || is.null(names(x)) || any(!nzchar(names(x)))) {
    .ard_stop(sprintf(
      "%s: `%s` is a named list: %s.", fn, what, paste(keys, collapse = ", ")
    ))
  }
  bad <- setdiff(names(x), keys)
  if (length(bad)) {
    .ard_stop(sprintf(
      "%s: `%s` has no %s; its names are %s.", fn, what,
      paste(sQuote(bad), collapse = ", "), paste(keys, collapse = ", ")
    ))
  }
  for (k in names(x)) {
    v <- x[[k]]
    if (!(is.character(v) || is.numeric(v)) || length(v) != 1L) {
      .ard_stop(sprintf("%s: `%s$%s` is one string.", fn, what, k))
    }
  }
  x <- lapply(x, function(v) trimws(as.character(v)))
  x[!is.na(x) & nzchar(x)]
}

.ai_ctx_study <- function(x, fn) {
  if (is.null(x)) return(NULL)
  .ai_ctx_record(x, c("study_id", "title"), "study", fn)
}

.ai_ctx_population <- function(x, fn) {
  if (is.null(x)) return(NULL)
  if (is.character(x) && length(x) == 1L && is.null(names(x))) {
    x <- list(label = x)
  }
  .ai_ctx_record(x, c("id", "label", "where"), "population", fn)
}

# The datasets as one data frame, one row a variable: dataset,
# dataset_label, variable, label, kind, levels (all character, NA = none).
.ai_ctx_datasets <- function(x, fn) {
  if (is.null(x)) return(NULL)
  cols <- c("dataset", "dataset_label", "variable", "label", "kind", "levels")
  if (is.data.frame(x)) {
    bad <- setdiff(names(x), cols)
    if (length(bad) || !all(c("dataset", "variable") %in% names(x))) {
      .ard_stop(sprintf(paste0(
        "%s: `datasets` as a data frame has the columns `dataset` and ",
        "`variable`, and may have %s."
      ), fn, paste(setdiff(cols, c("dataset", "variable")), collapse = ", ")))
    }
    d <- x
  } else if (.ai_is_data_list(x)) {
    d <- .ai_ctx_from_data(x)
  } else {
    .ard_stop(sprintf(paste0(
      "%s: `datasets` is a data frame (one row a variable) or a named list ",
      "of data frames (the datasets)."
    ), fn))
  }
  for (cn in setdiff(cols, names(d))) d[[cn]] <- NA_character_
  d <- d[cols]
  d[] <- lapply(d, function(v) {
    v <- trimws(as.character(v))
    v[!is.na(v) & !nzchar(v)] <- NA_character_
    v
  })
  d$dataset <- toupper(d$dataset)
  if (anyNA(d$dataset) || anyNA(d$variable)) {
    .ard_stop(sprintf(
      "%s: `datasets` has a row with no dataset or no variable.", fn
    ))
  }
  rownames(d) <- NULL
  d
}

.ai_is_data_list <- function(x) {
  is.list(x) && !is.null(names(x)) && all(vapply(x, is.data.frame, NA))
}

# Names, labels and kinds of the datasets, never their values.
.ai_ctx_from_data <- function(x) {
  rows <- lapply(names(x), function(nm) {
    d <- x[[nm]]
    lab <- vapply(d, function(v) {
      l <- attr(v, "label", exact = TRUE)
      if (is.character(l) && length(l) == 1L) l else NA_character_
    }, "")
    kind <- vapply(d, function(v) {
      if (is.factor(v)) "factor"
      else if (inherits(v, "Date") || inherits(v, "POSIXt")) "date"
      else if (is.logical(v)) "logical"
      else if (is.numeric(v)) "numeric"
      else "character"
    }, "")
    dl <- attr(d, "label", exact = TRUE)
    data.frame(
      dataset = nm,
      dataset_label = if (is.character(dl) && length(dl) == 1L) dl else NA,
      variable = names(d), label = unname(lab), kind = unname(kind),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

.ai_ctx_templates <- function(x, fn) {
  if (is.null(x)) return(NULL)
  known <- tfl_fig_templates()$template
  bad <- setdiff(x, known)
  if (!is.character(x) || length(bad)) {
    .ard_stop(sprintf(
      "%s: `templates` names templates of tfl_fig_templates(); not %s.", fn,
      paste(sQuote(bad), collapse = ", ")
    ))
  }
  unique(x)
}

.ai_ctx_toc_fields <- function(x, fn) {
  if (is.null(x)) return(.ai_toc_default_fields)
  bad <- setdiff(x, .toc_fields)
  if (!is.character(x) || length(bad)) {
    .ard_stop(sprintf(
      "%s: `toc_fields` are among %s; not %s.", fn,
      paste(.toc_fields, collapse = ", "), paste(sQuote(bad), collapse = ", ")
    ))
  }
  .toc_fields[.toc_fields %in% c("output_id", x)]
}

# The reports already there: a data frame output_id, title (NA = none).
.ai_ctx_reports <- function(x, fn) {
  if (is.null(x)) return(NULL)
  if (is.character(x)) x <- data.frame(output_id = x, stringsAsFactors = FALSE)
  ok <- is.data.frame(x) && "output_id" %in% names(x) &&
    !length(setdiff(names(x), c("output_id", "title")))
  if (!ok) {
    .ard_stop(sprintf(paste0(
      "%s: `reports` is a character vector of ids, or a data frame with ",
      "`output_id` and optionally `title`."
    ), fn))
  }
  if (is.null(x$title)) x$title <- NA_character_
  x <- data.frame(
    output_id = trimws(as.character(x$output_id)),
    title = trimws(as.character(x$title)), stringsAsFactors = FALSE
  )
  x[!is.na(x$output_id) & nzchar(x$output_id), , drop = FALSE]
}
