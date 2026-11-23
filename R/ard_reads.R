# The ARD each report reads (G4 B): what a run must build before a report
# is made, worked out from the study's SPEC alone, for tflplanner's run and
# for the runner's manifest (`needs`, `unresolved`).

#' The ARD each report of a study reads
#'
#' @description
#' Works out, from the study's SPEC, whose ARD each report reads, so that a
#' run builds those ARD before the report:
#'
#' * a report with analyses of its own (its `ard: analyses:`, or a
#'   template's) reads its own ARD;
#' * a figure whose ARD source (`page: report: ard_source:`) is `own` or
#'   `table:<id>` reads that report's ARD; one with no source but a
#'   template or a `figure:` content key and analyses of its own reads its
#'   own ARD; `import:<file>` reads an ARD taken in, no report's.
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
#'   on screen and not yet saved.  `NULL` (default) reads the study's report
#'   files ([tfl_read_outputs()]); a file that cannot be read is a row with
#'   its reason.
#' @return A data frame, one row per read: `report` (the output id), `reads`
#'   (the id whose ARD it reads; `NA` when that cannot be worked out),
#'   `kind` (`"own"` or `"source"`, a figure's ARD source) and `reason`
#'   (`NA` when worked out; otherwise `"no source"` -- the design reads an
#'   ARD and no source is chosen --, `"source not in analyses"`,
#'   `"source report not found"`, `"file not read: <message>"` or
#'   `"lookup failed: <message>"`).  A report that reads none has no row.
#' @examples
#' d <- system.file("extdata", "spec-yaml", package = "tflspec")
#' tfl_ard_reads(d)
#' @export
tfl_ard_reads <- function(study, outputs = NULL) {
  st <- if (inherits(study, "tfl_study_spec")) study else tfl_read_study_spec(study)
  bad <- .reads_row(character(), character(), character(), character())
  if (is.null(outputs)) {
    if (is.null(st$dir)) {
      stop("tfl_ard_reads(): give `outputs`, or a study read from its folder.", call. = FALSE)
    }
    msgs <- character()
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
  if (anyDuplicated(names(outputs)) ||
      !identical(unname(names(outputs)), unname(vapply(outputs, function(o) as.character(o$id), "")))) {
    stop("tfl_ard_reads(): `outputs` is named by the reports' ids, each once.", call. = FALSE)
  }
  ids <- names(outputs)
  # each report's analyses: its own, or its template's (resolved)
  an <- lapply(outputs, function(o) tryCatch(.reads_analyses(o, st), error = function(e) e))
  has <- ids[vapply(an, function(a) !inherits(a, "error") && length(a) > 0L, NA)]
  one <- function(id) {
    o <- outputs[[id]]
    if (inherits(an[[id]], "error")) {
      return(.reads_row(id, NA_character_, "own",
                        paste0("lookup failed: ", conditionMessage(an[[id]]))))
    }
    got <- if (id %in% has) .reads_row(id, id, "own")
    if (!identical(o$kind, "figure")) return(got)
    src <- .reads_fig_source(o, id, id %in% has)
    if (src$kind %in% c("own", "table")) {
      if (src$id %in% has) {
        if (!src$id %in% got$reads) got <- rbind(got, .reads_row(id, src$id, "source"))
      } else {
        why <- if (src$id %in% ids) "source not in analyses" else "source report not found"
        got <- rbind(got, .reads_row(id, NA_character_, "source", why))
      }
    } else if (identical(src$kind, "none") && .reads_fig_uses_ard(o, st)) {
      got <- rbind(got, .reads_row(id, NA_character_, "source", "no source"))
    }
    got
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

# a report's analyses: its file's, or (a template report) the template's
# with them, as the report resolves
.reads_analyses <- function(o, st) {
  a <- o$ard$analyses %||% list()
  if (length(a) || is.null(.tpl_spec_of(o))) return(a)
  tfl_resolve_output(o, st)$ard$analyses %||% list()
}

# a figure's ARD source: kind (none, own, table, import) and the report
# whose ARD it is
.reads_fig_source <- function(o, id, has_own) {
  src <- o$page$report$ard_source
  if (length(src) > 1L) stop("`page: report: ard_source:` is one value.", call. = FALSE)
  src <- if (is.null(src) || !length(src) || is.na(src[[1L]])) "" else trimws(as.character(src[[1L]]))
  if (identical(src, "own")) return(list(kind = "own", id = id))
  if (startsWith(src, "table:")) return(list(kind = "table", id = trimws(sub("^table:", "", src))))
  if (startsWith(src, "import:")) return(list(kind = "import"))
  # a template figure, or one with the content key `figure:`, with analyses
  # of its own (forest_hr): it reads its own ARD, though no source is said
  if ((!is.null(.tpl_spec_of(o)) || !is.null(o$figure)) && has_own) {
    return(list(kind = "own", id = id))
  }
  list(kind = "none")
}

# whether a figure's design reads an ARD (an ard_stats step, an ard_number
# layer): the report as it resolves (a template's layers too); a report with
# no template that does not resolve, its file (a template report that does
# not resolve stops: its layers are not known)
.reads_fig_uses_ard <- function(o, st) {
  r <- if (is.null(.tpl_spec_of(o))) tryCatch(tfl_resolve_output(o, st), error = function(e) o) else
    tfl_resolve_output(o, st)
  isTRUE(.fig_uses_ard(.fig_design_of_output(r))) || isTRUE(.fig_uses_ard(.fig_design_of_output(o)))
}
