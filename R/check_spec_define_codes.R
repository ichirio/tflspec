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
# `variable` to (in any dataset; a dictionary's list has no values) and
# their codes, a code of any of them counting; NULL when it maps none
.define_codes_of <- function(meta, variable) {
  v <- meta$variables
  if (is.null(v) || !nrow(v)) return(NULL)
  keep <- v$variable %in% variable & !is.na(v$codelist) & v$codelist %in% meta$codelists$codelist
  if (!any(keep)) return(NULL)
  cls <- unique(v$codelist[keep])
  list(codelists = cls, where = unique(paste0(v$dataset[keep], ".", v$variable[keep])),
       codes = unique(unlist(lapply(cls, .adam_codes, meta = meta), use.names = FALSE)))
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
# maps that has codes the define's list does not have (`part` "codelists",
# `item` the column).  `outs`: the reports as read, by id.  No adam_meta, or
# a column it does not map: no row.
.check_define_code_rows <- function(study, outs) {
  meta <- study$adam_meta
  if (is.null(meta) || !nrow(meta$variables %||% data.frame())) return(.check_rows())
  rows <- list(.check_rows())
  for (id in names(outs)) {
    cls <- outs[[id]]$codelists %||% list()
    for (var in names(cls)) {
      codes <- .inplace_codes(cls[[var]])
      def <- if (length(codes)) .define_codes_of(meta, var)
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
