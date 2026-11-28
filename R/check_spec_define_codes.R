# ---- tfl_check_spec(): a report's code list against the define's codes -------
#
# A report's code list written in place (its `codelists:`, a list of values)
# for a column that adam_meta maps to a code list of values: a code the
# define's list does not have is a warning (a typo, or a value the data do
# not hold).  Codes only: labels may differ on purpose (A7, 2026-11-25).
# The column is the one adam_meta's `variables:` names, as the programs put
# its levels on (.adam_meta_vars()); no list is matched to a column by the
# list's own name.

# The define's codes of a column: the code lists of values adam_meta maps
# `variable` to in `datasets` (the ones the report reads; in any dataset
# when none is known) and their codes, a code of any of them counting (a
# dictionary's list has no values); NULL when it maps none there.  The
# mapping is the programs' (.adam_meta_vars(), whose set_levels() put the
# levels on).
.define_codes_of <- function(meta, variable, datasets = NULL) {
  if (!length(datasets)) datasets <- meta$variables$dataset
  v <- .adam_meta_vars(meta, datasets)
  v <- v[v$variable %in% variable, , drop = FALSE]
  if (!nrow(v)) return(NULL)
  cls <- unique(v$codelist)
  list(codelists = cls, where = unique(paste0(v$dataset, ".", v$variable)),
       codes = unique(unlist(lapply(cls, .adam_codes, meta = meta), use.names = FALSE)))
}

# The datasets a report reads: the `from` of its data definitions (one that
# names another definition is not a dataset), its analyses' `dataset` and a
# listing's records (`data: {source: dataset, dataset: ...}`); none when it
# says none.  `o` is the report resolved (.define_codes_report()): a
# template's data rows are there, not in the report as written.
.report_datasets <- function(o) {
  one <- function(e, key) {
    x <- if (is.list(e)) e[[key]] else NULL
    if (is.character(x) && length(x) == 1L && !inherits(x, "tfl_r")) x else NA_character_
  }
  defs <- o$ard$analysis_data
  defs <- if (is.list(defs) && is.null(names(defs))) defs else list()
  ids <- vapply(defs, one, "", key = "id")
  m <- o$data
  rec <- if (is.list(m) && !is.null(names(m)) &&
             identical(as.character(m$source %||% "dataset"), "dataset")) one(m, "dataset")
  ds <- c(vapply(defs, one, "", key = "from"), vapply(o$ard$analyses %||% list(), one, "", key = "dataset"),
          rec)
  ds <- unique(ds[!is.na(ds) & nzchar(ds)])
  ds[!ds %in% ids]
}

# A report as its program reads its data: resolved (tfl_resolve_output():
# a template expanded, its data rows and analyses put in), so its datasets
# are its own and not every dataset's; the report as written when it cannot
# be resolved (that is an error of its own in tfl_check_spec())
.define_codes_report <- function(o, study) {
  r <- tryCatch(suppressWarnings(suppressMessages(tfl_resolve_output(o, study))),
                error = function(e) NULL)
  if (inherits(r, "tfl_output")) r else o
}

# The codes of a report's code list written in place (a list of {value,
# label} entries, or the values alone); NULL for one that is not (a map: a
# reference to a study list, not written in the report)
.inplace_codes <- function(cl) {
  if (is.character(cl)) return(names(cl) %||% unname(cl))
  if (!is.list(cl) || !is.null(names(cl))) return(NULL)
  vals <- vapply(cl, function(e) {
    x <- if (is.list(e)) e$value else e
    if (length(x)) as.character(x[[1L]]) else NA_character_
  }, "")
  vals[!is.na(vals)]
}

# The check's rows: one warning a report's in-place list of a column adam_meta
# maps (in the datasets the report reads) that has codes the define's list
# does not have (`part` "codelists", `item` the column).  `outs`: the
# reports as read, by id.  No adam_meta, or a column it does not map: no row.
.check_define_code_rows <- function(study, outs) {
  meta <- study$adam_meta
  if (is.null(meta) || !nrow(meta$variables %||% data.frame())) return(.check_rows())
  rows <- list(.check_rows())
  for (id in names(outs)) {
    cls <- outs[[id]]$codelists %||% list()
    ds <- if (length(cls)) .report_datasets(.define_codes_report(outs[[id]], study))
    for (var in names(cls)) {
      codes <- .inplace_codes(cls[[var]])
      def <- if (length(codes)) .define_codes_of(meta, var, ds)
      if (is.null(def)) next
      extra <- unique(setdiff(codes, def$codes))
      if (!length(extra)) next
      rows[[length(rows) + 1L]] <- .check_rows(
        output_id = id, part = "codelists", item = var, level = "warning",
        message = sprintf(paste0(
          "report %s: its code list of %s has %s %s, which the define's code list %s does not ",
          "have (adam_meta maps %s to it; codes are compared, not labels). Correct the code in ",
          "the report's codelists:, or add it to the define (spec/defaults.yml adam_meta: ",
          "codelists:)."),
          id, var, if (length(extra) > 1L) "the codes" else "the code",
          paste0("\"", extra, "\"", collapse = ", "), paste(def$codelists, collapse = " / "),
          paste(def$where, collapse = ", ")))
    }
  }
  do.call(rbind, rows)
}
