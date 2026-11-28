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
  p <- .by_needed_problems(.normalize_ard_sheet(ard$analyses, "analyses"), b$table$tables,
                            ards = ard$ards)
  if (nrow(p)) {
    .spec_stop(paste(c("The ARD definition is not valid:", p$message), collapse = "\n  "),
               p, "tflspec_ard_spec_error")
  }
  # the study's rounding, for cards' formats in the ARD program's setup
  rnd <- .study_rounding(study)
  if (!is.null(rnd) && !"rounding" %in% ard$study$key) {
    ard$study <- rbind(ard$study, data.frame(key = "rounding", value = rnd, stringsAsFactors = FALSE))
  }
  spec <- .tfl_ard_spec(ard, blank_notes = FALSE)
  # (the study's ADaM metadata, for the datasets of its catalog: the levels
  # study$adam has, which the program's own reads put on too, and its
  # fingerprint counts)
  spec$adam_meta <- .adam_meta_used(study)
  list(spec = spec, codelists = .report_codelists(study, tfl_read_outputs(study$dir, skip_bad = TRUE),
                               b$table$codelists, output_id))
}

#' A report's code lists
#'
#' `tfl_report_codelists()` gives the code lists the ARD program of each
#' report is written with: the report's own (`codelists:` in its file, a
#' template's own lists among them), as the sheets' `codelists` rows hold
#' them, the reports in the TOC's order.  No layer gives a report a list:
#' the study's code lists (`spec/defaults.yml` `codelists:`) are
#' `study$codelists`, which a report or a template names where it uses one
#' (`study$codelists$SEX`), and the levels of the ADaM columns come from the
#' study's ADaM metadata (`adam_meta:`, `study$adam`); a pattern holds no
#' list.
#'
#' The ARD holds each row's `value` (the code); its `label` is applied by the
#' report's table plan (`plan_labels`), and by a figure's legend.
#'
#' The ARD side takes the codes only: the ARD program writes each list as
#' its codes in their order (`cl_sex <- c("F", "M")`), and the ARD's
#' fingerprint ([tfl_ard_spec_hash()]) counts the codes, their order and
#' `under`, never a label.  A change of labels alone leaves the ARD up to
#' date; the labels go into the report program's `plan_labels()`.  A
#' report's list that none of its steps uses (its data, analyses, content or
#' figure) is a warning, naming the variable.
#'
#' @param study The study's spec, as [tfl_read_study_spec()] reads it.
#' @param outputs The reports, as [tfl_sheets_from_spec()] takes them: a
#'   list of `tfl_output` named by id.  `NULL` (default): the study's own
#'   files, read as [tfl_sheets_from_yaml()] reads them.
#' @param rows The reports' own code list rows (the sheets' `codelists`,
#'   a template's lists among them), when the caller has them: the sheets are
#'   then not made again.  The result keeps the columns of `rows`, and a row
#'   of a report not in `outputs` stays.  The rows of a variable with a
#'   `list` cell (a reference to one of the study's lists) are made again
#'   from the study's list, whatever their codes and labels say; a list or code
#'   the study has not got stops.  `NULL` (default): made from
#'   `outputs`.
#' @return A data frame of the sheets' `codelists` rows (`output_id`,
#'   `variable`, `value`, `label`, `order`, `under`, `list`: a reference to
#'   one of the study's lists as its rows, the study's codes and labels,
#'   `list` naming it), the reports in the
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
  if (is.null(rows)) rows <- tfl_sheets_from_spec(study, outputs, templates = "expand")$table$codelists
  .report_codelists(study, outputs, rows)
}

# (`rows`: the reports' own code list rows; `output_id`: only these reports')
.report_codelists <- function(study, outputs, rows, output_id = NULL) {
  own <- rows
  if (is.data.frame(own) && nrow(own) && "output_id" %in% names(own)) own <- own[!is.na(own$output_id), , drop = FALSE]
  if (is.null(own)) own <- data.frame(output_id = character())
  for (cn in setdiff(c("output_id", "variable", "value", "label", "order", "under", "list"), names(own))) {
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
  # (a list that no step of its report uses: a warning, once)
  .cl_unused_warn(outputs)
  # (a report's reference with no rows here: the study must have it all the
  # same; rows with no `list` cell are a list written in place -- a
  # template's code SPEC that adds codes to a reference -- and are the list)
  for (o in outputs) {
    for (v in names(o$codelists)) {
      ref <- o$codelists[[v]]
      if (.cl_is_ref(ref) && !any(!is.na(own$output_id) & own$output_id == o$id & own$variable %in% v)) {
        .cl_ref_rows(ref, v, study$codelists %||% list(), o$id, "tfl_report_codelists()")
      }
    }
  }
  # (each report's own rows; no layer gives a list)
  made <- lapply(seq_along(outputs), function(i) own[own$output_id == ids[i], , drop = FALSE])
  # (a report of the rows that is not among the outputs: kept, last)
  rest <- own[!own$output_id %in% ids_all & (is.null(output_id) | own$output_id %in% output_id), , drop = FALSE]
  out <- do.call(rbind, c(list(own[0L, , drop = FALSE]), made, list(rest)))
  rownames(out) <- NULL
  # (a reference's rows are the study's list's, whatever the rows given
  # say; a list or code the study has not got stops, naming it: the sheets
  # keep its cell with no code, the programs written from these rows stop)
  .cl_rows_from_study(out, study$codelists %||% list(), "tfl_report_codelists()")
}
