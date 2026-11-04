# ============================================================================
#  A listing written as code, back to a listing spec
# ----------------------------------------------------------------------------
#  The inverse of tfl_listing() / tfl_listing_code(), as tfl_as_table_spec()
#  is the table's: what the listing's columns and its options say goes to
#  the two sheets; what the sheets cannot say is named (not_converted), not
#  dropped.  From a plan (plan_listing()) the data is at hand, so the spec
#  is run back through tfl_listing() and the RTF compared.
# ============================================================================

#' A listing written in code, as a listing spec
#'
#' Writes an rtfreporter listing -- a `listing_spec()` object, or a
#' `table_plan()` with `plan_listing()` -- as a [tfl_listing_spec()]: one
#' `listings` row and its `listing_cols`.  What the sheets cannot carry is
#' listed in `attr(, "not_converted")`: a column's `name`, `rel_width` or
#' `layout`, the listing's spacer, `layout`, `blank_row_first`, an unnamed
#' `wrap` function, `record = FALSE`, and from a plan any layer besides the
#' listing and its rows a page.  The listing's own default `sep` / `align`
#' go to each column that does not set its own.
#'
#' The data, its subset and its order are the program's: `dataset` names
#' the dataset of the data catalog the listing reads, for the `listings`
#' row (`where` and `sort` stay blank).
#'
#' @param x A `listing_spec()` object, or a `table_plan()` with
#'   `plan_listing()`.
#' @param output_id The listing's id.
#' @param dataset The dataset it lists (of the data catalog), or `NA`.
#' @param compare For a plan: `TRUE` (default) makes the listing's pages from
#'   the spec ([tfl_listing()] on the plan's data) and compares their RTF,
#'   byte by byte, with the plan's.
#' @return A [tfl_listing_spec()] (not checked: `dataset` may be blank), with
#'   attributes `"not_converted"` and `"same_pages"` (`TRUE` / `FALSE`: the
#'   same RTF; `NA` when not compared).
#' @seealso [tfl_as_table_spec()], the table's.
#' @export
tfl_as_listing_spec <- function(x, output_id = "L", dataset = NA_character_,
                                compare = TRUE) {
  lost <- character()
  miss <- function(...) lost <<- c(lost, sprintf(...))
  data <- NULL
  pages <- NULL
  given <- list()
  max_rows <- NA_character_
  if (inherits(x, "table_plan")) {
    L <- suppressMessages(rtfreporter::plan_layers(x))
    ly <- L$layers
    lst <- ly[["listing"]]
    if (is.null(lst)) {
      .ard_stop("tfl_as_listing_spec(): the plan has no plan_listing().")
    }
    data <- L$data
    given <- lst[setdiff(names(lst), "cols")]
    cols <- lst$cols
    pg <- ly[["pages"]]
    if (!is.null(pg$max_rows)) max_rows <- as.character(pg$max_rows)
    for (k in setdiff(names(pg), "max_rows")) {
      miss("plan_paginate_rows(%s = ) stays in code", k)
    }
    for (k in setdiff(names(ly), c("listing", "pages"))) {
      if (length(ly[[k]])) miss("the plan's `%s` layer stays in code", k)
    }
    if (isTRUE(compare)) {
      pages <- tryCatch(suppressMessages(rtfreporter::plan_apply(x, "pages")),
                        error = function(e) NULL)
    }
  } else if (inherits(x, "rtf_listing_spec")) {
    cols <- x$cols
    # what the type gives anyway is not the listing's own
    dflt <- rtfreporter::listing_spec(cols, type = x$type)
    for (k in c("sep", "spacer", "spacer_rel_width", "blank_row",
                "blank_row_first", "align", "layout")) {
      if (!identical(x[[k]], dflt[[k]])) given[[k]] <- x[[k]]
    }
    given$type <- x$type
    if (isTRUE(x$wrap_custom)) given$wrap <- x$wrap
    if (!identical(x$record_col, dflt$record_col)) given$record <- FALSE
  } else {
    .ard_stop(sprintf(paste0(
      "tfl_as_listing_spec(): `x` must be a listing_spec() or a table_plan() ",
      "with plan_listing(); got %s."), .what(x)))
  }

  lab <- function(v) if (is.null(v)) NA_character_ else
    gsub("\n", "\\n", paste(v, collapse = "\n"), fixed = TRUE)
  one <- function(v) if (is.null(v)) NA_character_ else as.character(v)
  # leading or trailing spaces survive the sheet quoted, as in a table spec
  quoted <- function(v) if (!is.na(v) && grepl("^\\s|\\s$", v))
    paste0("\"", v, "\"") else v
  rows <- lapply(seq_along(cols), function(i) {
    cl <- cols[[i]]
    at <- sprintf("column %d (%s)", i, paste(cl$vars, collapse = " | "))
    if (!is.null(cl$name) && !identical(cl$name, cl$vars[1L])) {
      miss("%s: listing_col(name = ) stays in code", at)
    }
    if (!is.null(cl$rel_width)) miss("%s: listing_col(rel_width = ) stays in code", at)
    if (!is.null(cl$layout)) miss("%s: listing_col(layout = ) stays in code", at)
    data.frame(
      output_id = output_id, vars = paste(cl$vars, collapse = " | "),
      label = lab(cl$label), width = one(cl$width),
      sep = quoted(lab(cl$sep %||% given$sep)),
      align = one(cl$align %||% given$align),
      collapse_repeats = if (isTRUE(cl$collapse_repeats)) "TRUE" else NA,
      stringsAsFactors = FALSE)
  })
  for (k in c("spacer", "spacer_rel_width", "blank_row_first", "layout")) {
    if (!is.null(given[[k]])) miss("listing_spec(%s = ) stays in code", k)
  }
  if (!is.null(given$wrap)) {
    miss("listing_spec(wrap = <function>) stays in code: name the function in `wrap`")
  }
  if (identical(given$record, FALSE)) miss("listing_spec(record = FALSE) stays in code")
  listings <- data.frame(
    output_id = output_id, type = one(given$type), dataset = dataset,
    where = NA_character_, sort = NA_character_, max_rows = max_rows,
    blank_row = if (is.null(given$blank_row)) NA_character_ else
      as.character(given$blank_row),
    wrap = NA_character_, stringsAsFactors = FALSE)
  sp <- tfl_listing_spec(listings, do.call(rbind, rows), check = FALSE)

  same <- NA
  if (isTRUE(compare) && !is.null(data)) {
    back <- tryCatch(suppressMessages(tfl_listing(data, sp, output_id)),
                     error = function(e) e)
    if (inherits(back, "error")) {
      miss("the spec does not run: %s", conditionMessage(back))
      same <- FALSE
    } else {
      same <- !is.null(pages) && identical(.spec_rtf(back), .spec_rtf(pages))
    }
  }
  if (length(lost) || isFALSE(same)) {
    message(sprintf(
      "tfl_as_listing_spec() [%s]: %s\n%s", output_id,
      if (isTRUE(same)) "the spec gives the same RTF as the plan"
      else if (isFALSE(same)) "the spec does NOT give the same RTF"
      else "not compared",
      if (length(lost)) paste0("  not converted:\n",
                               paste0("    - ", unique(lost), collapse = "\n"))
      else ""))
  }
  attr(sp, "not_converted") <- unique(lost)
  attr(sp, "same_pages") <- same
  sp
}
