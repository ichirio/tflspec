# ============================================================================
#  A shift table's corner with a denominator of each parameter's own
# ----------------------------------------------------------------------------
#  The corner call of an item with analyses per a column (tflspec_shift's
#  `analyses: {per: [PARAMCD]}`) counts that column as its variable, by the
#  groups only (`by: [TRT01A], variables: [PARAMCD]`).  With a denominator of
#  the analysis set's subjects that is right: each parameter's subjects over
#  the group's N.  With a denominator of the records' own subjects -- one row
#  a subject and parameter, the subjects tested (canon 11-28, CANON-6) -- cards
#  divides by every row of the group, all parameters pooled (N the subject x
#  parameter pairs, p over 100%).  So such a call is made a value of the
#  column at a time, its numerator and its denominator both of that value,
#  and bound back: the same rows (variable_level the value), tagged as the
#  item's other calls are.
# ============================================================================

# Is the call `call` of item `r` its corner, to be made a value at a time?
# The column the item's analyses are per, and the denominator's data row,
# when the call's one variable is that column (not in its by) and the
# denominator is a data row made from the same dataset as the item's data
# (the records': it has the column); else NULL
.item_corner_sliced <- function(x, r, call) {
  per <- .item_per_rule(r)$cols
  if (length(per) != 1L) return(NULL)
  v <- .split_bar(call$variables %||% NA)
  b <- .split_bar(call$by %||% NA)
  if (!identical(v, per) || per %in% b) return(NULL)
  den <- call$denominator %||% NA
  d <- call$data %||% NA
  ad <- x$analysis_data
  if (!is.character(den) || length(den) != 1L || is.na(den) || is.na(d) ||
      !is.data.frame(ad) || !nrow(ad)) return(NULL)
  ad <- .adata_of(ad, r$output_id)
  if (!all(c(den, d) %in% ad$data_id)) return(NULL)
  src <- .adata_dataset(ad, den)
  if (is.na(src) || !identical(toupper(src), toupper(.adata_dataset(ad, d)))) return(NULL)
  list(col = per, den = den)
}

# The corner's lines: `lines`, the call written on `.d` under the name `.a`
# (its denominator the data row s$den), run once a value of s$col, on that
# value's records and that value's denominator rows, its rows of the value
# kept; bound under `name`
.item_corner_lines <- function(lines, s, data, name, id) {
  rx <- sprintf("(denominator = )%s(?![A-Za-z0-9._])", gsub(".", "\\.", s$den, fixed = TRUE))
  lines <- gsub(rx, "\\1.den", lines, perl = TRUE)
  lines <- gsub("\n", "\n  ", paste0("  ", lines), fixed = TRUE)
  c(sprintf("# %s: the corner a value of %s at a time, the records and the denominator (%s) both of it",
            id, s$col, s$den),
    sprintf("%s <- bind_items(lapply(sort(unique(as.character(%s$%s))), function(.value) {",
            name, data, s$col),
    sprintf("  .d <- dplyr::filter(%s, %s == .value)", data, s$col),
    sprintf("  .den <- dplyr::filter(%s, %s == .value)", s$den, s$col),
    lines,
    paste0("  dplyr::filter(.a, vapply(variable_level, function(v) ",
           "identical(as.character(unlist(v)), .value), NA))"),
    "}))")
}
