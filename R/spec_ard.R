# ---- the ARD's analyses as calls: code and run from one list ----------------
#
# An analysis of the ARD definition, as .ard_analysis_call() gives it (the
# call, the steps of its pipe, the Total column), written as the program
# writes it (.ard_call_lines()) and made without its code
# (.ard_call_run()).  The two read the same calls, so the code, run, makes
# what the run makes -- the ARD's half of tfl_content_code() and
# tfl_content_run().

# The analysis's call without `by`: the Total column's, bound under it
.ard_call_no_by <- function(cl) {
  nm <- names(cl$args) %||% rep("", length(cl$args))
  cl$args <- cl$args[nm != "by"]
  cl
}

# the steps after the call, those that are calls (a step that is nothing,
# no format to apply, is left out)
.ard_call_steps <- function(res) {
  Filter(function(s) !is.null(s$fn) || !is.null(s$code), res$steps)
}

# a step as the program writes it: as it was written (a post step), a call,
# or R as it was written
.ard_step_cl <- function(s) {
  if (!is.null(s$text)) return(s$text)
  if (is.null(s$fn)) as.character(s$code) else .call_as_cl(s)
}

# One analysis as code: `name <- data |> call(...) |> steps`, its id (and
# label) a comment above it -- what .ard_analysis_lines() writes, for an
# analysis that is a call (not a custom one's code, not a subject flag).
.ard_call_lines <- function(res, r, data, name) {
  node <- .call_as_cl(res$call)
  comment <- if (!is.na(r$label)) sprintf("# %s: %s", r$analysis_id, r$label) else
    sprintf("# %s", r$analysis_id)
  if (isTRUE(res$overall)) {
    comment <- c(comment, "# (and the Total column: the same call without `by`)")
    node <- .cl("dplyr::bind_rows", list(.piped(node, data),
                                         .piped(.call_as_cl(.ard_call_no_by(res$call)), data)))
  }
  steps <- lapply(.ard_call_steps(res), .ard_step_cl)
  c(comment, .data_pipe(name, node, data, steps))
}

# One analysis made: the call applied with do.call() on the objects the
# program has made (`env`: the analysis data, the analysis sets, the
# code lists, the company standards' formats), then each step on its
# result.  An argument is R as the program writes it, given as that
# expression -- a tidyselect argument (`by = ARM`) selects as it does in
# the program -- and evaluated in `env`.
.ard_call_run <- function(res, env) {
  .ard_need("dplyr", "running an analysis call")
  if (is.null(res$call)) {
    return(.ard_steps_run(eval(str2lang(as.character(res$code)), env), res, env))
  }
  x <- .ard_one_call(res$call, env)
  if (isTRUE(res$overall)) {
    x <- dplyr::bind_rows(x, .ard_one_call(.ard_call_no_by(res$call), env))
  }
  .ard_steps_run(x, res, env)
}

.ard_steps_run <- function(x, res, env) {
  for (s in .ard_call_steps(res)) {
    if (is.null(s$fn)) {
      f <- eval(str2lang(as.character(s$code)), env)
      x <- f(x)
    } else {
      x <- do.call(.ard_call_fun(s$fn, env), c(list(x), .ard_call_args(s$args)), envir = env)
    }
  }
  x
}

.ard_one_call <- function(cl, env) {
  do.call(.ard_call_fun(cl$fn, env), .ard_call_args(cl$args), envir = env)
}

# the arguments as expressions: R text parsed, a nested call as the
# program writes it (`~ cards::continuous_summary_fns(...)`, its
# function by name, so the program's own helpers are found)
.ard_call_args <- function(args) {
  lapply(args, function(a) {
    if (is.list(a) && !inherits(a, "tfl_r") && !is.null(a$fn)) {
      return(str2lang(.lay(.call_as_cl(a))))
    }
    str2lang(as.character(a))
  })
}

# a function by its name as the program writes it: `pkg::fn`, or one the
# program has (its helpers), else the attached packages'
.ard_call_fun <- function(fn, env) {
  if (grepl("::", fn, fixed = TRUE)) {
    return(getExportedValue(sub("::.*$", "", fn), sub("^.*::", "", fn)))
  }
  get(fn, envir = env, mode = "function")
}

# ---- the study's ARD definition from its spec files ------------------------

# A YAML study's ARD definition (its data catalog and each report's
# `ard.analysis_data` / `ard.analyses`) as the definition the ARD program
# is written from, and the reports' code lists.  The reports in the order
# of the TOC (toc.yml): the program's sections follow it.  `output_id`:
# those reports' rows alone (and the study's own), so that one report's
# mistake stops that report, not every other.
.ard_spec_of_study <- function(study, output_id = NULL, sheets = NULL) {
  if (is.null(study$dir)) {
    .ard_stop("A YAML study's ARD: read the study with tfl_read_study_spec(dir).")
  }
  # (`sheets`: the study's sheets, when the caller has them already)
  b <- sheets %||% .sheets_of_study(study$dir)

  ids <- vapply(study$toc$outputs %||% list(), function(e) e$id %||% NA_character_, "")
  in_toc <- function(d) {
    if (is.null(d) || !nrow(d) || is.null(d$output_id)) return(d)
    k <- match(d$output_id, ids)
    # (a row of no report first, a report the TOC does not list last)
    k[is.na(d$output_id)] <- 0L
    k[is.na(k)] <- length(ids) + 1L
    d[order(k, seq_len(nrow(d))), , drop = FALSE]
  }
  ard <- b$ard
  for (s in c("objects", "analysis_data", "analyses")) ard[[s]] <- in_toc(ard[[s]])
  if (!is.null(output_id)) {
    for (s in c("objects", "analysis_data", "analyses")) {
      d <- ard[[s]]
      if (is.null(d) || !nrow(d) || is.null(d$output_id)) next
      ard[[s]] <- d[is.na(d$output_id) | d$output_id %in% output_id, , drop = FALSE]
    }
  }
  # a table's analyses have its columns' by
  p <- .by_needed_problems(.normalize_ard_sheet(ard$analyses, "analyses"), b$table$tables)
  if (nrow(p)) {
    .spec_stop(paste(c("The ARD definition is not valid:", p$message), collapse = "\n  "),
               p, "tflspec_ard_spec_error")
  }
  # the study's rounding, for cards' formats in the ARD program's setup
  rnd <- .study_rounding(study)
  if (!is.null(rnd) && !"rounding" %in% ard$study$key) {
    ard$study <- rbind(ard$study, data.frame(key = "rounding", value = rnd, stringsAsFactors = FALSE))
  }
  list(spec = tfl_ard_spec(ard), codelists = .report_codelists(study, tfl_read_outputs(study$dir, skip_bad = TRUE),
                               b$table$codelists, output_id))
}

#' A report's code lists, with the ones its layers give
#'
#' `tfl_report_codelists()` gives the code lists the ARD program of each
#' report is written with.  A report's code lists can come from the layers
#' it extends -- the study's `defaults.yml`, a pattern -- as well as from its
#' own file ([tfl_resolve_output()] puts them together for the RTF); the
#' sheets ([tfl_sheets_from_yaml()]) hold a report's own only, so that they
#' stay the round trip back to the YAML.  This is the sheets' `codelists`
#' rows with the layers' lists added, report by report.
#'
#' For each report and variable there is one list: the report's own when
#' it has one (a template's own lists among them, as the sheets hold them),
#' else the one of the layers it extends (a pattern's over the defaults').
#' A report that extends nothing (`extends: none`) has only its own.
#'
#' @param study The study's spec, as [tfl_read_study_spec()] reads it.
#' @param outputs The reports, as [tfl_sheets_from_spec()] takes them: a
#'   list of `tfl_output` named by id.  `NULL` (default): the study's own
#'   files, read as [tfl_sheets_from_yaml()] reads them.
#' @param rows The reports' own code list rows (the sheets' `codelists`,
#'   a template's lists among them), when the caller has them: the sheets are
#'   then not made again.  The result keeps the columns of `rows` (a layer's
#'   rows have `NA` there), and a row of a report not in `outputs` stays.
#'   `NULL` (default): made from `outputs`.
#' @return A data frame of the sheets' `codelists` rows (`output_id`,
#'   `variable`, `value`, `label`, `order`, `under`), the reports in the
#'   TOC's order; every row has its report (no `NA` `output_id`), and zero
#'   rows when no report has a list.  [tfl_ard_code()] and [tfl_build_ard()]
#'   write a study's ARD with these.
#' @examples
#' d <- system.file("extdata", "spec-yaml", package = "tflspec")
#' tfl_report_codelists(tfl_read_study_spec(d))
#' @export
tfl_report_codelists <- function(study, outputs = NULL, rows = NULL) {
  if (!inherits(study, "tfl_study_spec")) {
    .ard_stop("tfl_report_codelists(): `study` is a study's spec, tfl_read_study_spec().")
  }
  if (is.null(outputs)) {
    if (is.null(study$dir)) {
      .ard_stop("tfl_report_codelists(): give the reports (`outputs`), or read the study with tfl_read_study_spec(dir).")
    }
    outputs <- tfl_read_outputs(study$dir, skip_bad = TRUE)
  }
  if (is.null(rows)) rows <- tfl_sheets_from_spec(study, outputs)$table$codelists
  .report_codelists(study, outputs, rows)
}

# (`rows`: the reports' own code list rows; `output_id`: only these reports')
.report_codelists <- function(study, outputs, rows, output_id = NULL) {
  own <- rows
  if (is.data.frame(own) && nrow(own) && "output_id" %in% names(own)) own <- own[!is.na(own$output_id), , drop = FALSE]
  if (is.null(own)) own <- data.frame(output_id = character())
  for (cn in setdiff(c("output_id", "variable", "value", "label", "order", "under"), names(own))) {
    own[[cn]] <- rep(NA_character_, nrow(own))
  }
  toc_ids <- vapply(study$toc$outputs %||% list(), function(e) e$id %||% NA_character_, "")
  ids <- vapply(outputs, function(o) o$id, "")
  ids_all <- ids
  ord <- order(match(ids, toc_ids, nomatch = length(toc_ids) + 1L), seq_along(ids))
  outputs <- outputs[ord]
  ids <- ids[ord]
  if (!is.null(output_id)) {
    outputs <- outputs[ids %in% output_id]
    ids <- ids[ids %in% output_id]
  }
  made <- lapply(seq_along(outputs), function(i) {
    mine <- own[own$output_id == ids[i], , drop = FALSE]
    below <- tryCatch(.layers_below(outputs[[i]], study)$acc$codelists, error = function(e) {
      .ard_stop(sprintf("%s: %s", ids[i], conditionMessage(e)))
    })
    below <- .codelists_norm(below)
    below <- below[setdiff(names(below), mine$variable)]
    rbind(mine, .codelist_rows_of(ids[i], below, names(own)))
  })
  # (a report of the rows that is not among the outputs: kept, last)
  rest <- own[!own$output_id %in% ids_all & (is.null(output_id) | own$output_id %in% output_id), , drop = FALSE]
  out <- do.call(rbind, c(list(own[0L, , drop = FALSE]), made, list(rest)))
  rownames(out) <- NULL
  out
}

# Code lists (a variable's entries: value, label, under) as the sheets'
# rows of one report
.codelist_rows_of <- function(id, lists, cols) {
  rows <- list()
  for (v in names(lists)) for (i in seq_along(lists[[v]])) {
    e <- lists[[v]][[i]]
    rows[[length(rows) + 1L]] <- list(
      output_id = id, variable = v, value = .sheet_cell(e$value), label = .sheet_cell(e$label),
      order = as.character(i), under = .sheet_cell(e$under))
  }
  .rows_df(rows, cols)
}
