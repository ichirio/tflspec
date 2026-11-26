# ============================================================================
#  tfl_check_spec()'s reading of a report's `!r` values: each is read as R
#  once (parse(), not run); one that does not read is an `error` row, with
#  its place in the SPEC (`content$plan_filter$args$where`) and the
#  parser's message.  The schema takes any `!r` for a slot of R
#  (.schema_ok()) and the code writer pastes the text as it is, so such a
#  value otherwise stops only the program that runs.
#
#  The report's `ard:` is left to the ARD check (tfl_ard_code()'s
#  problems): its fields have their own forms (`args` the arguments of a
#  call, `derive` and `post` pieces between `|`).
# ============================================================================

.check_r_parse_rows <- function(o, id) {
  x <- unclass(o)
  tx <- .check_texts(x[setdiff(names(x), "ard")])
  tx <- if (!is.null(tx)) tx[tx$r, , drop = FALSE]
  rows <- list(.check_rows())
  for (i in seq_len(NROW(tx))) {
    m <- .r_parse_problem(tx$text[i])
    if (is.null(m)) next
    shown <- .ai_one_line(tx$text[i])
    if (nchar(shown) > 60L) shown <- paste0(substr(shown, 1L, 57L), "...")
    rows[[length(rows) + 1L]] <- .check_rows(
      output_id = id, part = sub("[$].*$", "", tx$at[i]), item = tx$at[i], level = "error",
      message = sprintf("`!r %s` does not read as R (%s)", shown, m))
  }
  do.call(rbind, rows)
}

# NULL when `text` reads as R, else the parser's message (with its line
# when the text has more than one)
.r_parse_problem <- function(text) {
  e <- tryCatch({
    parse(text = text, keep.source = FALSE)
    NULL
  }, error = function(err) conditionMessage(err))
  if (is.null(e)) return(NULL)
  first <- strsplit(e, "\n", fixed = TRUE)[[1L]][1L]
  line <- sub("^<text>:([0-9]+):.*$", "\\1", first)
  what <- trimws(sub("^<text>:[0-9]+:[0-9]+: ", "", first))
  if (grepl("\n", text, fixed = TRUE) && grepl("^[0-9]+$", line)) paste0("line ", line, ": ", what) else what
}
