# ============================================================================
#  tfl_check_spec()'s look at a table's column header N (#969).  A header
#  text with a population token ({n}, {n1}, {n:Placebo}) and no `values`
#  of its own takes its numbers from the report's BIGN analysis (the
#  header's N is always BIGN's, canon 2026-11-16): the program reads them
#  with tfl_bign_values(), named by the column variable's values, the
#  Total column's the groups' sum.  Without them it has nothing to give:
#  rtfreporter reads what the ARD states and prints N=NA for the rest (the
#  AI baseline drafts, a Total column and every arm of a table whose
#  analyses have missing values).  So the report as written is checked:
# ----------------------------------------------------------------------------
#  - the header asks for N and the report has no BIGN analysis: a warning,
#    with the analysis to add (counted by the table's column variable);
#  - BIGN counts by variables none of which is a column variable of the
#    table: its numbers name no column; a warning naming both.
#  A BIGN with no variable (the analysis set's one N, every column's) and a
#  header with its own `values` are the author's: no row.
# ============================================================================

# The variables a content's table_plan() spreads into columns, every depth
.header_n_cols <- function(content) {
  out <- character()
  for (it in content) {
    if (!identical(.schema_fn_name(it$fn %||% ""), "table_plan")) next
    v <- as.character(unlist(it$args$cols, use.names = FALSE))
    out <- c(out, trimws(unlist(strsplit(v, "|", fixed = TRUE))))
  }
  unique(out[nzchar(out)])
}

# The variables an analysis counts by: its `variables` and its `by`
.header_n_groups <- function(a) {
  v <- as.character(unlist(c(a$variables, a$by), use.names = FALSE))
  v <- trimws(unlist(strsplit(v, "|", fixed = TRUE)))
  unique(v[nzchar(v) & !is.na(v)])
}

# Does the report's header show BIGN's N?  A table of its ARD
# (.bign_applies()) with a plan_col_header() whose text says {n} and that
# gives no `values` of its own (.bign_header_items(), the rule
# tfl_content_code() follows): those take BIGN's.  One notion for both
# checks of the header N: this file's rows and the N check's
# (R/check_spec_n.R) look only at an N the header shows
.bign_shown <- function(output) {
  .bign_applies(output) && is.list(output$content) && !inherits(output$content, "tfl_r") &&
    isTRUE(attr(.bign_header_items(output$content, TRUE), "used"))
}

.check_header_n_rows <- function(o, id) {
  if (!.bign_shown(o)) return(NULL)
  rows <- list(.check_rows())
  add <- function(...) {
    rows[[length(rows) + 1L]] <<- .check_rows(output_id = id, part = "content",
                                               item = "plan_col_header(header)", ...)
  }
  an <- o$ard$analyses
  an <- if (is.list(an) && !is.data.frame(an)) an else list()
  bign <- Filter(function(a) is.list(a) && identical(as.character(a$id %||% "")[1L], .bign_id), an)
  var <- .bign_var(o$content)
  total <- .bign_total(o$content)
  if (!length(bign)) {
    add(level = "warning", message = sprintf(paste0(
      "the column header's N (`{n}`) is the report's BIGN analysis, and the report has none: ",
      "the program then reads each column's N from the ARD and prints N=NA where the ARD does ",
      "not state it (%s). Add the analysis `id: %s, method: categorical, data: <the subjects' ",
      "data>%s` (the subjects of each column%s)"),
      if (is.null(total)) "an arm whose analyses have missing values"
      else sprintf("the %s column, an arm whose analyses have missing values", total),
      .bign_id, if (is.null(var)) "" else sprintf(", variables: [%s]", var),
      if (is.null(total)) "" else sprintf("; the %s column's N is their sum", total)))
    return(do.call(rbind, rows))
  }
  cols <- .header_n_cols(o$content)
  for (a in bign) {
    g <- .header_n_groups(a)
    if (!length(g) || !length(cols) || any(g %in% cols)) next
    add(level = "warning", message = sprintf(paste0(
      "the column header's N (`{n}`) is the report's %s analysis, which counts by %s, ",
      "and the table's columns are %s: its numbers name no column, so each column prints N=NA. ",
      "Count %s by the column variable (`variables: [%s]`) or spread the table by %s"),
      .bign_id, paste(g, collapse = ", "), paste(cols, collapse = ", "),
      .bign_id, var %||% cols[1L], paste(g, collapse = ", ")))
  }
  do.call(rbind, rows)
}
