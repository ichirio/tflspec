# One content item at a time: its code, the arguments its code writes, and
# what a report's own layer holds of it -- what a builder's card shows and
# saves (tflplanner B3).

#' One content item: its code, its written arguments, its own part
#'
#' * `tfl_item_code()`: the item's one call, as [tfl_content_code()] writes
#'   it (no `plan <-`, no pipe).
#' * `tfl_item_written()`: the names of the arguments its code writes -- the
#'   arguments given, an `!unset` or `NULL` one left out; a verb's
#'   positional `...` (`plan_titles("A", "B")`) is `"..."`.
#' * `tfl_item_own()`: what the report's own layer holds of the item, given
#'   the item `below` it (the layers' without the report's own,
#'   [tfl_resolve_output()]'s `provenance = TRUE`).  A verb whose `...` is
#'   named (`plan_cells()` ...) keeps only the keys whose value differs from
#'   below's, and a key below has that the item has not is `!unset`; any
#'   other verb is the item whole, or `NULL` when it is the same as below's
#'   (the own item can go).  Values are compared as their R text, so `30`
#'   and `30L` are the same.
#'
#' @param item A [tfl_item()].
#' @param pipe Unused: the call alone (kept for the code's form to come).
#' @param below The item below it, or `NULL` (no layer has the verb: the
#'   item is all the report's own).
#' @return `tfl_item_code()`: the code, one element a line.
#'   `tfl_item_written()`: the argument names.  `tfl_item_own()`: a
#'   [tfl_item()] or `NULL`.
#' @examples
#' it <- tfl_item("plan_cells", list(continuous = list(n = "{N}"), default = "{n} ({p}%)"))
#' tfl_item_code(it)
#' tfl_item_written(it)
#' below <- tfl_item("plan_cells", list(default = "{n} ({p}%)"), from = "defaults")
#' tfl_item_own(it, below)$args
#' @export
tfl_item_code <- function(item, pipe = NULL) {
  .item_check(item, "tfl_item_code()")
  if (.fig_is_figure_fn(item$fn)) {
    return(sprintf("%s(%s)", .schema_fn_name(item$fn), tfl_args_to_r(item$fn, item$args)))
  }
  cl <- .content_calls(list(item))[[1L]]
  unlist(strsplit(.spec_call_code(.content_print(cl), 2L, 0L), "\n", fixed = TRUE))
}

#' @rdname tfl_item_code
#' @export
tfl_item_written <- function(item) {
  .item_check(item, "tfl_item_written()")
  a <- item$args %||% list()
  keep <- !vapply(a, function(v) is.null(v) || inherits(v, "tfl_unset"), NA)
  nm <- names(a)[keep]
  if (is.null(nm)) return(character())
  sc <- tryCatch(tfl_schema(item$fn), error = function(e) NULL)
  key <- sc$dots$key
  if (identical(sc$dots$style, "positional") && !is.null(key)) nm[nm == key] <- "..."
  nm[!nzchar(nm)] <- "..."
  unique(nm)
}

#' @rdname tfl_item_code
#' @export
tfl_item_own <- function(item, below = NULL) {
  .item_check(item, "tfl_item_own()")
  if (is.null(below)) return(.item_bare(item, "own"))
  if (!identical(below$fn, item$fn)) {
    .ard_stop(sprintf("tfl_item_own(): `below` is a %s item; the item is %s.", below$fn, item$fn))
  }
  txt <- function(args) tfl_args_to_r(item$fn, args)
  if (.dots_named(item$fn)) {
    a <- item$args %||% list()
    b <- below$args %||% list()
    keys <- names(a)[vapply(names(a), function(k)
      is.null(b[[k]]) || !identical(txt(a[k]), txt(b[k])), NA)]
    own <- a[keys]
    for (k in setdiff(names(b), names(a))) own[[k]] <- tfl_unset()
    if (!length(own)) return(NULL)
    return(tfl_item(item$fn, own, from = "own"))
  }
  if (identical(txt(item$args), txt(below$args))) return(NULL)
  .item_bare(item, "own")
}

.item_check <- function(item, fn) {
  if (!inherits(item, "tfl_item")) .ard_stop(sprintf("%s: `item` is a tfl_item().", fn))
  invisible(item)
}

# the item as a report writes it: fn, args, from (no provenance fields)
.item_bare <- function(item, from) tfl_item(item$fn, item$args %||% list(), from = from)
