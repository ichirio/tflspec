# The ARD each report reads (G4 B): what a run must build before a report
# is made, worked out from the study's SPEC alone, for tflplanner's run and
# for the runner's manifest (`needs`, `unresolved`).

#' The ARD each report of a study reads
#'
#' @description
#' Works out, from the study's SPEC, whose ARD each report reads, so that a
#' run builds those ARD before the report:
#'
#' * a report with analyses of its own (its `analysis: analyses:`, or a
#'   template's) reads its own ARD;
#' * a figure whose ARD source (`page: report: ard_source:`) is `own` or
#'   `table:<id>` reads that report's ARD; one with no source but a
#'   template (`template: {name: ...}`) or a `figure:` content key and
#'   analyses of its own reads its own ARD; `import:<file>` reads an ARD
#'   taken in, no report's.
#'
#' @details
#' A read that cannot be worked out is a row with `reads = NA` and a
#' reason; a run must not start on it.  What a report's own R code reads of
#' another report's ARD is not found here: that read stops the run when it
#' happens.  A report file that cannot be read is named by its file's name
#' (`T-14-1-1.yml`: `T-14-1-1`).
#'
#' @param study A study folder, or a `tfl_study_spec`
#'   ([tfl_read_study_spec()]).
#' @param outputs The reports, a named list of `tfl_output` (by id): what is
#'   on screen and not yet saved.  The caller passes every report of the
#'   study: a report that is not in the list is not looked at, and a
#'   source that names it is "source report not found".  `NULL` (default)
#'   reads the study's report files ([tfl_read_outputs()]); a file that
#'   cannot be read is a row with its reason.
#' @return A data frame, one row per read: `report` (the output id), `reads`
#'   (the id whose ARD it reads; `NA` when that cannot be worked out),
#'   `kind` (`"own"` or `"source"`, a figure's ARD source) and `reason`
#'   (`NA` when worked out; otherwise `"no source"` -- the design reads an
#'   ARD and no source is chosen --, `"source not understood: <value>"`
#'   -- the source is none of `own`, `table:<id>` and `import:<file>` --,
#'   `"source not in analyses"`, `"source report not found"`,
#'   `"source lookup failed"` -- the source report's analyses could not be
#'   worked out --, `"ref analysis: <ref>"` -- an analysis that refers to
#'   another report's analysis (`ref`), not yet worked out --,
#'   `"file not read: <message>"` or `"lookup failed: <message>"`).  A
#'   report that reads none has no row.
#' @examples
#' d <- system.file("extdata", "spec-yaml", package = "tflspec")
#' tfl_ard_reads(d)
#' @export
tfl_ard_reads <- function(study, outputs = NULL) {
  st <- if (inherits(study, "tfl_study_spec")) study else tfl_read_study_spec(study)
  bad <- .reads_row(character(), character(), character(), character())
  if (!is.null(outputs) && !is.list(outputs)) {
    stop("tfl_ard_reads(): `outputs` is a named list of tfl_output.", call. = FALSE)
  }
  if (is.null(outputs)) {
    if (is.null(st$dir)) {
      stop("tfl_ard_reads(): give `outputs`, or a study read from its folder.", call. = FALSE)
    }
    msgs <- character()
    # (m7: the unreadable files are found by the text of tfl_read_outputs()'s
    # warning; the test of the "file not read" row catches a change of it)
    left <- "^tfl_read_outputs\\(\\): (.+) is left out, it cannot be read: "
    outputs <- withCallingHandlers(
      tfl_read_outputs(st$dir, skip_bad = TRUE),
      warning = function(w) {
        m <- conditionMessage(w)
        if (grepl(left, m)) {
          msgs <<- c(msgs, m)
          invokeRestart("muffleWarning")
        }
      })
    if (length(msgs)) {
      f <- sub(paste0(left, ".*$"), "\\1", msgs)
      why <- sub("^.* is left out, it cannot be read: ", "", msgs)
      bad <- .reads_row(tools::file_path_sans_ext(f), NA_character_, "own",
                        paste0("file not read: ", why))
    }
  }
  if (length(outputs) && (is.null(names(outputs)) || !all(vapply(outputs, inherits, NA, "tfl_output")))) {
    stop("tfl_ard_reads(): `outputs` is a named list of tfl_output.", call. = FALSE)
  }
  # (m7: one id in two files is a stop, not a row)
  if (length(outputs) && (anyDuplicated(names(outputs)) ||
      !identical(unname(names(outputs)), unname(vapply(outputs, function(o) as.character(o$id), ""))))) {
    stop("tfl_ard_reads(): `outputs` is named by the reports' ids, each once.", call. = FALSE)
  }
  ids <- names(outputs)
  # each report's analyses: its own, or its template's (resolved, once: the
  # resolved report is kept for the design's look)
  an <- lapply(outputs, function(o) tryCatch(.reads_analyses(o, st), error = function(e) e))
  # the reports that read their own ARD: an analysis that is not a `ref`
  has <- ids[vapply(an, function(a) {
    !inherits(a, "error") && any(!vapply(a$analyses, .reads_is_ref, NA))
  }, NA)]
  one <- function(id) {
    o <- outputs[[id]]
    a <- an[[id]]
    if (inherits(a, "error")) {
      return(.reads_row(id, NA_character_, "own",
                        paste0("lookup failed: ", conditionMessage(a))))
    }
    got <- if (id %in% has) .reads_row(id, id, "own")
    # an analysis that is `ref:` another report's: not defined yet, so not
    # worked out (fail closed)
    refs <- unique(unlist(lapply(Filter(.reads_is_ref, a$analyses),
                                 function(x) as.character(unlist(x[["ref"]])))))
    if (length(a$analyses) && any(vapply(a$analyses, .reads_is_ref, NA))) {
      got <- rbind(got, .reads_row(id, NA_character_, "own",
                                   paste0("ref analysis: ", paste(refs, collapse = ", "))))
    }
    # (m6: `ard_source: table:<id>` on a table or a listing is ignored: no row)
    if (!identical(o$kind, "figure")) return(got)
    fig <- tryCatch({
      src <- .reads_fig_source(o, id, length(a$analyses) > 0L)
      if (identical(src$kind, "bad")) {
        .reads_row(id, NA_character_, "source", paste0("source not understood: ", src$value))
      } else if (src$kind %in% c("own", "table")) {
        if (src$id %in% has) {
          if (!src$id %in% got$reads) .reads_row(id, src$id, "source")
        } else {
          why <- if (!src$id %in% ids) "source report not found" else
            if (inherits(an[[src$id]], "error")) "source lookup failed" else "source not in analyses"
          .reads_row(id, NA_character_, "source", why)
        }
      } else if (identical(src$kind, "none") && .reads_fig_uses_ard(o, st, a$resolved)) {
        .reads_row(id, NA_character_, "source", "no source")
      }
    }, error = function(e) {
      # (the own row stays: `got`)
      .reads_row(id, NA_character_, "source", paste0("lookup failed: ", conditionMessage(e)))
    })
    rbind(got, fig)
  }
  rows <- lapply(ids, function(id) tryCatch(one(id), error = function(e) {
    .reads_row(id, NA_character_, "source", paste0("lookup failed: ", conditionMessage(e)))
  }))
  out <- do.call(rbind, c(list(bad), rows))
  rownames(out) <- NULL
  out
}

.reads_row <- function(report, reads, kind, reason = NA_character_) {
  data.frame(report = report, reads = reads, kind = kind, reason = reason,
             stringsAsFactors = FALSE)
}

# an analysis that refers to another report's analysis (`ref:`)
.reads_is_ref <- function(a) is.list(a) && !is.null(a[["ref"]])

# a report's analyses: its file's, or (a template report) the template's
# with them, as the report resolves -- list(analyses, resolved), `resolved`
# the resolved report when it was resolved here
.reads_analyses <- function(o, st) {
  a <- o$ard$analyses %||% list()
  if (length(a) || is.null(.tpl_spec_of(o))) return(list(analyses = a, resolved = NULL))
  r <- suppressMessages(tfl_resolve_output(o, st))
  list(analyses = r$ard$analyses %||% list(), resolved = r)
}

# a figure's ARD source: kind (none, own, table, import, bad) and the report
# whose ARD it is; `bad`: a value that is none of them (`value`)
.reads_fig_source <- function(o, id, has_own) {
  src <- o$page$report$ard_source
  if (length(src) > 1L) stop("`page: report: ard_source:` is one value.", call. = FALSE)
  src <- if (is.null(src) || !length(src) || is.na(src[[1L]])) "" else trimws(as.character(src[[1L]]))
  if (identical(src, "own")) return(list(kind = "own", id = id))
  if (startsWith(src, "table:") && nzchar(trimws(sub("^table:", "", src)))) {
    return(list(kind = "table", id = trimws(sub("^table:", "", src))))
  }
  if (startsWith(src, "import:") && nzchar(trimws(sub("^import:", "", src)))) return(list(kind = "import"))
  if (nzchar(src)) return(list(kind = "bad", value = src))
  # a template figure (any `template: {name: }`, as tflplanner reads it, or
  # a template SPEC), or one with the content key `figure:`, with analyses
  # of its own (forest_hr): it reads its own ARD, though no source is said
  tpl <- (is.list(o$template) && !is.null(o$template$name)) || !is.null(.tpl_spec_of(o))
  if ((tpl || !is.null(o$figure)) && has_own) {
    return(list(kind = "own", id = id))
  }
  list(kind = "none")
}

# whether a figure reads an ARD: a `figure:` content key (its program always
# passes `ard`), an ard_number item (top level or inside a named plot), or
# the design (an ard_stats step, an ard_number layer, a composed figure's
# plots).  The report as it resolves (a template's layers too), and its
# file: a report with no template that does not resolve is its file (m2: a
# pattern's content is then not seen); a template report that does not
# resolve stops: its layers are not known.  `resolved`: the report already
# resolved, or NULL.
.reads_fig_uses_ard <- function(o, st, resolved = NULL) {
  r <- resolved
  if (is.null(r)) {
    r <- if (is.null(.tpl_spec_of(o))) {
      tryCatch(suppressMessages(tfl_resolve_output(o, st)), error = function(e) o)
    } else {
      suppressMessages(tfl_resolve_output(o, st))
    }
  }
  if (!is.null(o$figure) || !is.null(r$figure)) return(TRUE)
  isTRUE(.reads_design_uses_ard(r)) || isTRUE(.reads_design_uses_ard(o))
}

# one report's content and design: its content items (a tfl_r content is
# the report's own code: not looked into, its data steps still are), then
# the design as tfl_fig_design() joins it (data and stats; the plots of a
# composed figure are normalised there)
.reads_design_uses_ard <- function(x) {
  if (inherits(x$content, "tfl_r")) x$content <- list()
  if (.reads_items_use_ard(x$content)) return(TRUE)
  .fig_uses_ard(.fig_as_design(.fig_design_of_output(x)))
}

# an item (or an item of a named plot) that is tfl_ard_number
.reads_items_use_ard <- function(content) {
  if (inherits(content, "tfl_r") || !is.list(content)) return(FALSE)
  for (it in content) {
    if (!is.list(it)) next
    fn <- it[["fn"]]
    if (is.character(fn) && length(fn) == 1L && identical(.schema_fn_name(fn), "tfl_ard_number")) return(TRUE)
    if (inherits(it, "tfl_plot") && .reads_items_use_ard(it[["items"]])) return(TRUE)
  }
  FALSE
}
