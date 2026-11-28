# ============================================================================
#  The column headers' N and the percentages' denominator, once the ARD of a
#  report is made (tfl_build_ard(), canon 11-28, CANON-6)
# ----------------------------------------------------------------------------
#  The header's N is the report's BIGN analysis, the subjects of its data
#  row (a template's col_header_n_data); each other analysis has its own
#  denominator (a template's denominator, by default the same row).  Two
#  looks, on the objects the ARD program left and the ARD it made:
#  - BIGN's data row is one row a subject in each header group: else BIGN
#    counts rows, not subjects, and the build stops, naming the row;
#  - an analysis whose percentages divide by that same row has the same N
#    as BIGN for each header group: else a warning, once, naming the
#    analysis.  An analysis whose denominator is another row is the
#    author's choice (the subjects tested for each parameter): no warning.
#    Not looked at: a descriptive N (a continuous summary's), and a slice
#    finer than the header's (a group of the call that the row has too: its
#    N is that slice's own).
#  Only for the ARD of one report: a study's program makes the reports one
#  after another under the same names, so its objects are the last one's.
# ============================================================================

# Both looks, for the report `output_id` (one), its ARD spec `spec` and the
# environment `e` the program ran in
.ard_bign_checks <- function(spec, e, output_id) {
  if (length(output_id) != 1L || is.na(output_id)) return(invisible())
  if (inherits(spec, "tfl_study_spec")) spec <- .ard_spec_of_study(spec, output_id = output_id)$spec
  a <- tryCatch(.as_spec(spec, "ard", "tfl_build_ard")$analyses, error = function(err) NULL)
  if (!is.data.frame(a) || !nrow(a)) return(invisible())
  a <- a[a$output_id %in% output_id, , drop = FALSE]
  b <- a[a$analysis_id %in% .bign_id, , drop = FALSE]
  if (nrow(b) != 1L) return(invisible())
  hd <- .data_col(b)
  hdat <- if (!is.na(hd)) e[[hd]]
  if (!is.data.frame(hdat)) return(invisible())
  hv <- intersect(.split_bar(b$variables), names(hdat))
  subj <- .study_value(spec, "id", "USUBJID")
  .bign_one_per_subject(hdat, hd, unique(c(subj, hv)), output_id)
  ard <- e$ard
  if (!is.data.frame(ard) || length(hv) != 1L) return(invisible())
  .bign_den_n_check(a, ard, hd, hv, hdat, output_id)
}

# BIGN's data row: one row a subject in each header group, else a stop
.bign_one_per_subject <- function(d, id, key, output_id) {
  key <- intersect(key, names(d))
  if (length(key) < 2L) return(invisible())
  k <- do.call(paste, c(lapply(d[key], as.character), sep = "\r"))
  n <- sum(duplicated(k))
  if (n) {
    .ard_stop(sprintf(paste0(
      "%s: BIGN, the column headers' N, counts the data row %s, which has more than one ",
      "row for a subject in a header group (%d row(s) more than %s's subjects). Name a row ",
      "of one row a subject (col_header_n_data), such as the analysis set's (adsl_sub)."),
      output_id, id, n, paste(key, collapse = " / ")))
  }
  invisible()
}

# The ARD rows of each analysis whose denominator is BIGN's row `hd`: its N
# against BIGN's n for each value of the header's variable `hv`; a warning
# once an analysis that differs
.bign_den_n_check <- function(a, ard, hd, hv, hdat, output_id) {
  if (!all(c("analysis_id", "stat_name", "stat") %in% names(ard))) return(invisible())
  if ("output_id" %in% names(ard)) ard <- ard[ard$output_id %in% output_id, , drop = FALSE]
  aid <- as.character(ard$analysis_id)
  lvl <- function(v) vapply(v, function(x) if (is.null(x) || !length(x) || is.na(x[[1L]])) NA_character_ else
    as.character(x[[1L]]), "")
  bn <- ard[aid %in% .bign_id & ard$stat_name %in% "n", , drop = FALSE]
  if (!nrow(bn) || !"variable_level" %in% names(bn)) return(invisible())
  big <- stats::setNames(as.numeric(unlist(bn$stat)), lvl(bn$variable_level))
  den <- a$denominator %||% rep(NA_character_, nrow(a))
  items <- a[!a$analysis_id %in% .bign_id & den %in% hd, , drop = FALSE]
  # (an ARD row's item: its own id, else -- an item of analyses per value or
  # per variable -- the item whose variables it counts, else the one such)
  own <- setdiff(a$analysis_id, .bign_id)
  per <- a[!is.na(a$analysis_per %||% rep(NA_character_, nrow(a))), , drop = FALSE]
  ctx <- if ("context" %in% names(ard)) as.character(ard$context) else rep("", nrow(ard))
  keep <- ard$stat_name %in% "N" & !aid %in% .bign_id & !ctx %in% c("summary", "continuous")
  gcols <- grep("^group[0-9]+$", names(ard), value = TRUE)
  for (i in seq_len(nrow(items))) {
    it <- items[i, , drop = FALSE]
    mine <- aid %in% it$analysis_id
    if (!is.na(it$analysis_per %||% NA)) {
      others <- !aid %in% own
      if (nrow(per) > 1L) {
        calls <- tryCatch(.item_ards(it), error = function(err) NULL)
        vars <- unique(c(.split_bar(it$variables), unlist(lapply(calls, function(e)
          .split_bar(e$variables %||% NA)))))
        others <- others & lvl(ard$variable) %in% vars
      }
      mine <- mine | others
    }
    rows <- ard[mine & keep, , drop = FALSE]
    if (!nrow(rows)) next
    # the header's value of each row, and whether a finer group of it is
    # one the row has (that slice's own N)
    at <- rep(NA_character_, nrow(rows))
    finer <- rep(FALSE, nrow(rows))
    for (g in gcols) {
      nm <- lvl(rows[[g]])
      gl <- lvl(rows[[paste0(g, "_level")]])
      at[nm %in% hv] <- gl[nm %in% hv]
      finer <- finer | (!is.na(nm) & !nm %in% hv & nm %in% names(hdat))
    }
    ok <- !is.na(at) & !finer & at %in% names(big)
    if (!any(ok)) next
    n <- as.numeric(unlist(rows$stat))[ok]
    want <- unname(big[at[ok]])
    bad <- which(n != want)
    if (length(bad)) {
      j <- bad[1L]
      warning(sprintf(paste0(
        "%s / %s: its percentages divide by %s, the column headers' N data, but its N is not ",
        "BIGN's (%s for %s, BIGN %s): the header's N and the percentages count different ",
        "subjects. Look at the analysis's data and denominator."),
        output_id, it$analysis_id, hd, format(n[j]), at[ok][j], format(want[j])), call. = FALSE)
    }
  }
  invisible()
}
