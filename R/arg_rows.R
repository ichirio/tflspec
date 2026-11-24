# An argument whose value a person fills as rows (the basic field type Rows)
# while the SPEC keeps it as text: the definition's `serializer:` names the
# text form (canon 464: few basic types, made from the arg type; the field
# type map 2026-11-24, section 3 row 6).  One serializer so far:
#
#   sort_keys   plan_sort()'s keys: one row a key -- by, direction, level,
#               column -- written as rtfreporter's key text, "-n@group1[Total]"
#
# tfl_rows_read() turns the SPEC's value into the rows, tfl_rows_write() the
# rows back into the value; the two are a round trip for every value the
# serializer reads.

# the closed list of serializers (a definition's `serializer:`)
.arg_serializers <- c("sort_keys")

#' An argument's value as rows, and back
#'
#' Some arguments keep a short text form in the SPEC that a form shows as
#' rows: [rtfreporter::plan_sort()]'s keys are written `"-n@group1[Total]"`
#' in the SPEC and filled as one row a key (what to sort by, the direction,
#' the level whose count is read, the column it is read in).  The argument's
#' definition names its text form ([tfl_arg_defs()]'s `serializer`); these
#' two functions turn the value into its rows and the rows back into the
#' value, a round trip.
#'
#' The sort keys' rows (`serializer = "sort_keys"`):
#' \describe{
#'   \item{by}{what the key sorts by: a row key or `label` (`group1`), a
#'     statistic (`n`, `max(n)`), `levels(group1)`, `.overall` or `.depth`.}
#'   \item{direction}{`"asc"` or `"desc"` (written as a leading `-`).}
#'   \item{level}{for a statistic, the row key whose group's own count is
#'     carried to its rows (`@group1`); `NA` for the row's own.}
#'   \item{column}{for a statistic, the one column it is read in
#'     (`[Total]`); `NA` for all the columns together.}
#' }
#' `FALSE` (the data's order) is no rows, with the attribute `"false"`
#' `TRUE`.
#'
#' @param value The argument's value as the SPEC has it: for `sort_keys` a
#'   character vector of keys, `FALSE`, or `NULL`.
#' @param rows The rows: a data frame, or a list of rows each a named list
#'   (a form's records; a missing cell `NA`).  A row with no `by` is left
#'   out.
#' @param serializer The text form's name, one of `"sort_keys"`.
#' @param false `TRUE` writes the type's `FALSE` (the data's order) whatever
#'   the rows are.
#' @return `tfl_rows_read()`: a data frame of the rows (`NULL` for `NULL`).
#'   `tfl_rows_write()`: the value (`NULL` when no row is left).
#' @examples
#' r <- tfl_rows_read(c(".overall", "-n@group1", "group1", ".depth", "-n[Total]", "label"),
#'                    "sort_keys")
#' r
#' tfl_rows_write(r, "sort_keys")
#' tfl_rows_write(list(list(by = "n", direction = "desc", column = "Drug X")), "sort_keys")
#' tfl_rows_read(FALSE, "sort_keys")
#' @export
tfl_rows_read <- function(value, serializer) {
  .arg_serializer_check(serializer)
  if (is.null(value)) return(NULL)
  switch(serializer, sort_keys = .sort_keys_rows(value))
}

#' @rdname tfl_rows_read
#' @export
tfl_rows_write <- function(rows, serializer, false = FALSE) {
  .arg_serializer_check(serializer)
  if (isTRUE(false)) return(FALSE)
  switch(serializer, sort_keys = .sort_rows_keys(rows))
}

.arg_serializer_check <- function(serializer) {
  if (!is.character(serializer) || length(serializer) != 1L || !serializer %in% .arg_serializers) {
    stop(sprintf("serializer is one of %s.", paste(.arg_serializers, collapse = ", ")), call. = FALSE)
  }
}

# the sort keys' columns
.sort_rows_cols <- c("by", "direction", "level", "column")

# keys -> rows: "-n@group1[Total]" is by n, desc, level group1, column
# Total (rtfreporter's .ard_sort_scoped(): the statistic, then @key, then
# [col]); a key of another form is all `by`
.sort_keys_rows <- function(keys) {
  empty <- data.frame(by = character(), direction = character(), level = character(),
                      column = character(), stringsAsFactors = FALSE)
  if (isFALSE(keys)) return(structure(empty, false = TRUE))
  if (!is.character(keys) || anyNA(keys) || !all(nzchar(trimws(keys)))) {
    stop("plan_sort()'s keys are text (\"-n@group1\"), or false for the data's order.", call. = FALSE)
  }
  if (!length(keys)) return(empty)
  keys <- trimws(keys)
  desc <- startsWith(keys, "-")
  body <- sub("^-", "", keys)
  m <- regmatches(body, regexec("^([^@[]+)(@([^@[]+))?([[]([^]]+)[]])?$", body))
  by <- level <- column <- rep(NA_character_, length(keys))
  for (i in seq_along(keys)) {
    x <- m[[i]]
    if (!length(x)) {
      by[i] <- body[i]
      next
    }
    by[i] <- x[2L]
    if (nzchar(x[4L])) level[i] <- x[4L]
    if (nzchar(x[6L])) column[i] <- x[6L]
  }
  data.frame(by = by, direction = ifelse(desc, "desc", "asc"), level = level, column = column,
             stringsAsFactors = FALSE)
}

# rows -> keys: the direction's minus, by, @level, [column]
.sort_rows_keys <- function(rows) {
  if (is.null(rows)) return(NULL)
  if (is.data.frame(rows)) {
    rows <- lapply(seq_len(nrow(rows)), function(i) as.list(rows[i, , drop = FALSE]))
  }
  if (!is.list(rows)) stop("The sort rows are a data frame or a list of rows.", call. = FALSE)
  cell <- function(r, k) {
    v <- r[[k]]
    v <- if (is.null(v) || !length(v)) NA_character_ else as.character(unlist(v))[1L]
    if (is.na(v) || !nzchar(trimws(v))) NA_character_ else trimws(v)
  }
  keys <- character()
  for (r in rows) {
    by <- cell(r, "by")
    if (is.na(by)) next
    dir <- cell(r, "direction")
    if (!is.na(dir) && !dir %in% c("asc", "desc")) {
      stop(sprintf("A sort row's direction is asc or desc, not \"%s\".", dir), call. = FALSE)
    }
    level <- cell(r, "level")
    column <- cell(r, "column")
    if ((!is.na(level) || !is.na(column)) && grepl("[@[]", by)) {
      stop(sprintf("A sort row's by (\"%s\") has @ or [ in it: give the level and the column in their own cells.",
                   by), call. = FALSE)
    }
    if (!is.na(level) && grepl("[@[]", level)) {
      stop(sprintf("A sort row's level (\"%s\") is one row key, with no @ or [.", level), call. = FALSE)
    }
    if (!is.na(column) && grepl("]", column, fixed = TRUE)) {
      stop(sprintf("A sort row's column (\"%s\") has no ].", column), call. = FALSE)
    }
    keys <- c(keys, paste0(if (identical(dir, "desc")) "-", by,
                           if (!is.na(level)) paste0("@", level),
                           if (!is.na(column)) paste0("[", column, "]")))
  }
  if (length(keys)) keys
}
