# ============================================================================
#  A listing: its definition (two sheets) -> the R code, or the pages
# ----------------------------------------------------------------------------
#  A listing is defined like a table, in rows: which data (a dataset of the
#  study's data catalog and a condition), in which order, over how many rows
#  a page, and its columns (rtfreporter's listing_col(): the variables
#  stacked in a column, the header, the width, whether a repeated value is
#  printed once).  The definition is a workbook of two sheets, `listings`
#  (one row a listing) and `listing_cols` (one row a column), keyed by
#  output_id -- the sheets tflplanner's listing_figure_spec.xlsx has.
#
#  From it, tfl_listing_code() writes the program (read the data, subset,
#  rework, sort, lay out with rtfreporter's listing_spec() / as_rtftables())
#  and tfl_listing() makes the same pages from data in hand.  How a listing
#  is laid out on the page stays rtfreporter's; this only calls it.
# ============================================================================

#' The code that reads one dataset of a data catalog
#'
#' Writes `adsl <- haven::read_xpt("data/adam/adsl.xpt")` (the reader follows
#' the file's extension: `.rds`, `.xpt`, `.sas7bdat`, `.csv`, `.parquet`, and `.rda` /
#' `.RData` holding one dataset)
#' into an object named after the dataset, and its derived columns.
#'
#' @param datasets The data catalog: a data frame with `dataset`, `path`
#'   and `derive` (`NAME = R expression`, `|` between them), as the
#'   `datasets` sheet of an [tfl_ard_spec()].
#' @param dataset The dataset to read.
#' @return The code, one element per line.  A dataset the catalog does not
#'   have gives a `stop()` line, so the program says so when it runs.
#' @export
tfl_read_data_code <- function(datasets, dataset) {
  r <- datasets[!is.na(datasets$dataset) & datasets$dataset == dataset, ,
                drop = FALSE]
  if (!nrow(r) || is.na(r$path[1L])) {
    return(sprintf("stop(\"tflspec: dataset %s is not in the data catalog.\")",
                   dataset))
  }
  obj <- .r_name(dataset)
  .drop_attached_ns(c(sprintf("%s <- %s", obj, .reader(r$path[1L])),
                      .derive_code(obj, r$derive[1L])))
}

.r_sort <- function(sort) {
  s <- .split_bar(sort)
  if (!length(s)) return(NULL)
  keys <- vapply(s, function(k) {
    if (startsWith(k, "-")) sprintf("dplyr::desc(%s)", substring(k, 2L)) else k
  }, "")
  sprintf("dplyr::arrange(%s)", paste(keys, collapse = ", "))
}

# a label as the sheet writes it: `\n` (two characters) is a line break
.listing_label <- function(x) gsub("\\n", "\n", x, fixed = TRUE)

.r_label <- function(x) encodeString(.listing_label(x), quote = "\"")

# ---- the definition --------------------------------------------------------

.listing_sheets <- list(
  listings     = c("output_id", "type", "dataset", "where", "sort",
                   "max_rows", "blank_row", "wrap"),
  listing_cols = c("output_id", "vars", "label", "width", "sep", "align",
                   "collapse_repeats"))

# a sheet as the definition keeps it: its columns, in order, as text; blank
# cells NA; rows with nothing but an output_id dropped
.listing_sheet <- function(d, sheet) {
  cols <- .listing_sheets[[sheet]]
  d <- if (is.null(d)) data.frame() else as.data.frame(d, stringsAsFactors = FALSE)
  n <- nrow(d)
  out <- lapply(cols, function(c) {
    v <- if (c %in% names(d)) trimws(as.character(d[[c]])) else
      rep(NA_character_, n)
    v[!is.na(v) & !nzchar(v)] <- NA
    v
  })
  out <- as.data.frame(stats::setNames(out, cols), stringsAsFactors = FALSE)
  out <- out[rowSums(!is.na(out[setdiff(cols, "output_id")])) > 0, ,
             drop = FALSE]
  rownames(out) <- NULL
  out
}

#' A listing definition: read, write and check it
#'
#' A listing is defined in two sheets, keyed by `output_id`:
#'
#' * `listings`, one row a listing: `output_id`; `type` (an rtfreporter
#'   listing type, blank for the default); `dataset` (of the data catalog);
#'   `where` (an R condition on its columns); `sort` (variables, `|` between
#'   them, `-` in front for descending); `max_rows` (rows a page);
#'   `blank_row` (`TRUE` / `FALSE`: a blank row after each record; blank:
#'   the type's); `wrap` (the name of an R function that breaks a cell into
#'   lines, `listing_spec(wrap = )`; blank: the type's own rule).
#' * `listing_cols`, one row a printed column, in order: `output_id`; `vars`
#'   (`|` between variables stacked in the column); `label` (the header, `\n`
#'   for a line break); `width` (characters); `sep` (what separates the
#'   stacked variables -- quote it, `" / "`, to keep its spaces; blank: the
#'   type's, `/`); `align` (`left` /
#'   `center` / `right`; blank: the type's); `collapse_repeats` (`TRUE`
#'   prints a value once until it changes).
#'
#' `tfl_listing_spec()` makes the definition from those two data frames (or a
#' list holding them, as a GUI keeps them; other elements are ignored) and
#' checks it.  `tfl_read_listing_spec()` reads it from a workbook -- other
#' sheets, such as tflplanner's `figures`, are ignored -- and
#' `tfl_write_listing_spec()` writes it; `tfl_write_listing_spec(tfl_listing_spec(),
#' path)` gives an empty workbook to fill in.
#'
#' @param listings The `listings` sheet (a data frame), or a list with
#'   `listings` and `listing_cols`, or a `tfl_listing_spec`.
#' @param listing_cols The `listing_cols` sheet.
#' @param check `FALSE` keeps a definition still being written (a listing
#'   with no dataset yet, a column with no variable) without refusing it.
#' @return A `tfl_listing_spec`: a list of the two sheets, all text.
#' @seealso [tfl_listing_code()] (the program), [tfl_listing()] (the pages).
#' @examples
#' spec <- tfl_listing_spec(
#'   listings = data.frame(output_id = "L-16-2-7", dataset = "ADAE",
#'                         where = "AESEV == 'SEVERE'",
#'                         sort = "USUBJID | -ASTDY", max_rows = "20"),
#'   listing_cols = data.frame(
#'     output_id = "L-16-2-7",
#'     vars  = c("USUBJID", "AEDECOD | AESEV", "ASTDY"),
#'     label = c("Subject", "Preferred term / Severity", "Study day"),
#'     width = c("12", "30", "8"),
#'     collapse_repeats = c("TRUE", NA, NA)))
#' spec
#' @export
tfl_listing_spec <- function(listings = NULL, listing_cols = NULL,
                             check = TRUE) {
  if (inherits(listings, "tfl_listing_spec")) {
    x <- unclass(listings)
  } else if (is.list(listings) && !is.data.frame(listings)) {
    x <- listings[intersect(names(listings), names(.listing_sheets))]
  } else {
    x <- list(listings = listings, listing_cols = listing_cols)
  }
  out <- lapply(names(.listing_sheets), function(s) .listing_sheet(x[[s]], s))
  names(out) <- names(.listing_sheets)
  out <- structure(out, class = "tfl_listing_spec")
  if (check) .listing_check(out)
  out
}

.listing_check <- function(x) {
  l <- x$listings
  cl <- x$listing_cols
  err <- character()
  who <- function(id) if (is.na(id)) "a listing with no output_id" else
    paste("listing", id)
  if (any(is.na(l$output_id))) {
    err <- c(err, "`listings`: a row has no output_id")
  }
  dup <- unique(l$output_id[!is.na(l$output_id) & duplicated(l$output_id)])
  if (length(dup)) {
    err <- c(err, sprintf("`listings`: output_id repeated: %s",
                          paste(dup, collapse = ", ")))
  }
  for (i in seq_len(nrow(l))) {
    r <- l[i, ]
    if (is.na(r$dataset)) err <- c(err, sprintf("%s: no `dataset`", who(r$output_id)))
    if (!is.na(r$where) &&
        inherits(try(str2lang(r$where), silent = TRUE), "try-error")) {
      err <- c(err, sprintf("%s: `where` is not R code: %s", who(r$output_id),
                            r$where))
    }
    bad <- .split_bar(r$sort)
    bad <- bad[!grepl("^-?[.A-Za-z][.A-Za-z0-9_]*$", bad)]
    if (length(bad)) {
      err <- c(err, sprintf("%s: `sort` takes variable names (- for descending): %s",
                            who(r$output_id), paste(bad, collapse = ", ")))
    }
    if (!is.na(r$max_rows) && !grepl("^[1-9][0-9]*$", r$max_rows)) {
      err <- c(err, sprintf("%s: `max_rows` is not a whole number: %s",
                            who(r$output_id), r$max_rows))
    }
    if (!is.na(r$blank_row) && !toupper(r$blank_row) %in% c("TRUE", "FALSE")) {
      err <- c(err, sprintf("%s: `blank_row` is TRUE or FALSE, not %s",
                            who(r$output_id), r$blank_row))
    }
    if (!is.na(r$wrap) && !grepl("^([A-Za-z.][A-Za-z0-9.]*::)?[A-Za-z.][A-Za-z0-9._]*$",
                                 r$wrap)) {
      err <- c(err, sprintf("%s: `wrap` is the name of an R function, not %s",
                            who(r$output_id), r$wrap))
    }
    if (!is.na(r$output_id) && !r$output_id %in% cl$output_id) {
      err <- c(err, sprintf("%s: no columns in `listing_cols`", who(r$output_id)))
    }
  }
  orphan <- setdiff(stats::na.omit(cl$output_id), l$output_id)
  if (length(orphan)) {
    err <- c(err, sprintf("`listing_cols`: output_id not in `listings`: %s",
                          paste(orphan, collapse = ", ")))
  }
  if (any(is.na(cl$output_id))) {
    err <- c(err, "`listing_cols`: a row has no output_id")
  }
  for (i in seq_len(nrow(cl))) {
    r <- cl[i, ]
    at <- sprintf("%s, column %d", who(r$output_id),
                  sum(cl$output_id[seq_len(i)] %in% r$output_id))
    if (!length(.split_bar(r$vars))) err <- c(err, sprintf("%s: no `vars`", at))
    if (!is.na(r$width) &&
        (is.na(suppressWarnings(as.numeric(r$width))) ||
         as.numeric(r$width) <= 0)) {
      err <- c(err, sprintf("%s: `width` is not a positive number: %s", at,
                            r$width))
    }
    if (!is.na(r$align) && !r$align %in% c("left", "center", "right")) {
      err <- c(err, sprintf("%s: `align` is left, center or right, not %s",
                            at, r$align))
    }
    if (!is.na(r$collapse_repeats) &&
        !toupper(r$collapse_repeats) %in% c("TRUE", "FALSE")) {
      err <- c(err, sprintf("%s: `collapse_repeats` is TRUE or FALSE, not %s",
                            at, r$collapse_repeats))
    }
  }
  if (length(err)) {
    .ard_stop(paste(c("The listing definition is not valid:",
                      paste("  *", err)), collapse = "\n"))
  }
  invisible(x)
}

#' @rdname tfl_listing_spec
#' @param path The workbook (`.xlsx`).
#' @param output_id Only these listings; `NULL` for all.
#' @export
tfl_read_listing_spec <- function(path, output_id = NULL, check = TRUE) {
  .ard_need("readxl", "tfl_read_listing_spec()")
  if (!file.exists(path)) .stop_no_file(path, "tfl_read_listing_spec")
  sheets <- readxl::excel_sheets(path)
  if (!"listings" %in% sheets) {
    .ard_stop(sprintf("%s has no `listings` sheet.", path))
  }
  x <- lapply(names(.listing_sheets), function(s) {
    if (!s %in% sheets) return(NULL)
    d <- .xlsx_text(path, s)
    bad <- setdiff(names(d), c(.listing_sheets[[s]], "note"))
    if (length(bad)) {
      .ard_stop("Sheet `", s, "` has columns it does not read: ",
                paste(bad, collapse = ", "))
    }
    d
  })
  names(x) <- names(.listing_sheets)
  spec <- tfl_listing_spec(x, check = FALSE)
  if (!is.null(output_id)) {
    miss <- setdiff(output_id, spec$listings$output_id)
    if (length(miss)) {
      .ard_stop(sprintf("No listing %s in %s.", paste(miss, collapse = ", "),
                        path))
    }
    spec$listings <- spec$listings[spec$listings$output_id %in% output_id, ,
                                   drop = FALSE]
    spec$listing_cols <- spec$listing_cols[
      spec$listing_cols$output_id %in% output_id, , drop = FALSE]
  }
  if (check) .listing_check(spec)
  spec
}

#' @rdname tfl_listing_spec
#' @param spec A `tfl_listing_spec`.
#' @return `tfl_write_listing_spec()`: `path`, invisibly.
#' @export
tfl_write_listing_spec <- function(spec, path) {
  spec <- tfl_listing_spec(spec, check = FALSE)
  .write_spec_book(unclass(spec)[names(.listing_sheets)], path)
}

#' @export
print.tfl_listing_spec <- function(x, ...) {
  l <- x$listings
  cat("<tfl_listing_spec> ", nrow(l), " listing", if (nrow(l) != 1L) "s",
      "\n", sep = "")
  for (i in seq_len(nrow(l))) {
    n <- sum(x$listing_cols$output_id %in% l$output_id[i])
    cat(sprintf("  %-12s %s%s, %d column%s%s\n", l$output_id[i],
                if (is.na(l$dataset[i])) "(no dataset)" else l$dataset[i],
                if (is.na(l$where[i])) "" else paste0(" where ", l$where[i]),
                n, if (n != 1L) "s" else "",
                if (is.na(l$max_rows[i])) "" else
                  paste0(", ", l$max_rows[i], " rows a page")))
  }
  invisible(x)
}

# one listing of a definition (or of the workbook at `spec`)
.listing_one <- function(spec, output_id, fn = "tfl_listing") {
  spec <- .as_spec(spec, "listing", fn, output_id = output_id)
  ids <- spec$listings$output_id
  if (is.null(output_id)) {
    if (length(ids) != 1L) {
      .ard_stop(sprintf("The definition has %d listings; say which with `output_id` (%s).",
                        length(ids), paste(ids, collapse = ", ")))
    }
    output_id <- ids
  }
  if (length(output_id) != 1L || !output_id %in% ids) {
    .ard_stop(sprintf("No listing %s in the definition (it has %s).",
                      paste(output_id, collapse = ", "),
                      if (length(ids)) paste(ids, collapse = ", ") else "none"))
  }
  list(listing = spec$listings[match(output_id, ids), , drop = FALSE],
       cols = spec$listing_cols[spec$listing_cols$output_id %in% output_id, ,
                                drop = FALSE])
}

# ---- the program -----------------------------------------------------------

#' The code that makes a listing from its definition
#'
#' Writes the part of a listing program that reads the data, subsets,
#' reworks and sorts it, and lays it out:
#' `content <- as_rtftables(data, listing = lst)`.  The program's setup
#' (`library(rtfreporter)`) and its report are the caller's.
#'
#' @param spec A [tfl_listing_spec()], or the path of its workbook.
#' @param output_id The listing; may be left out when the definition has one.
#' @param datasets The data catalog (see [tfl_read_data_code()]).
#' @param rework Code run on `data` before it is sorted, or `NULL`.
#' @param type The listing type when the listing's `type` is blank (one of
#'   rtfreporter's `listing_spec()` types).
#' @param codelists The reports' code lists (a table definition's
#'   `codelists` sheet, or a data frame with `output_id`, `variable`,
#'   `value`, `label`, `order`): the listing's rows of the columns it shows
#'   or sorts by are put on its data after its condition (`cl_<variable>`
#'   and `set_levels()`, see [tfl_helpers_code()]) -- each such column a
#'   factor in its list's order (it sorts so), its values as the list has
#'   them.
#' @return The code, one element per line; `NULL` when the listing names no
#'   dataset yet.
#' @seealso [tfl_listing()] for the pages themselves.
#' @export
tfl_listing_code <- function(spec, output_id = NULL, datasets,
                             rework = NULL, type = "multiline",
                             codelists = NULL) {
  x <- .listing_one(spec, output_id, "tfl_listing_code")
  l <- x$listing
  cols <- x$cols
  if (is.na(l$dataset)) return(NULL)
  obj <- .r_name(l$dataset)
  col_code <- vapply(seq_len(nrow(cols)), function(i) {
    c <- cols[i, ]
    v <- .split_bar(c$vars)
    vv <- if (length(v) == 1L) encodeString(v, quote = "\"") else
      sprintf("c(%s)", paste(encodeString(v, quote = "\""), collapse = ", "))
    a <- c(vv,
           if (!is.na(c$width)) paste("width =", c$width),
           if (!is.na(c$label)) paste("label =", .r_label(c$label)),
           if (!is.na(c$sep)) paste("sep =", .r_label(.ard_spec_unquote(c$sep))),
           if (!is.na(c$align)) paste("align =", encodeString(c$align, quote = "\"")),
           if (identical(toupper(c$collapse_repeats), "TRUE"))
             "collapse_repeats = TRUE")
    sprintf("  listing_col(%s)", paste(a, collapse = ", "))
  }, "")
  type <- if (is.na(l$type)) type else l$type
  # the code lists of the columns it shows or sorts by
  shown <- unique(c(unlist(lapply(cols$vars, .split_bar)),
                    sub("^-", "", .split_bar(l$sort))))
  lv <- .codelist_levels(codelists, l$output_id, shown)
  # the records its condition keeps, its code lists, its order -- in one
  # statement; a rework (code of its own) before the order
  sort <- .r_sort(l$sort)
  steps <- c(if (!is.na(l$where)) sprintf("dplyr::filter(%s)", l$where),
             if (length(lv)) .levels_call(lv),
             if (is.null(rework)) sort)
  c(if (length(lv)) .codelists_head(lv),
    paste0("# the data: ", l$dataset, " (data catalog)"),
    tfl_read_data_code(datasets, l$dataset),
    if (!length(steps)) sprintf("data <- %s", obj) else
      if (length(steps) == 1L && !length(lv)) {
        sprintf("data <- %s(%s, %s)", sub("\\(.*$", "", steps),
                obj, sub("^[^(]*\\((.*)\\)$", "\\1", steps))
      } else paste0("data <- ", obj, " |>\n  ", paste(steps, collapse = " |>\n  ")),
    if (!is.null(rework)) c("", "# rework", rework, "",
                            if (!is.null(sort)) sprintf("data <- %s", sub("\\(", "(data, ", sort))),
    "# dates as they print",
    "data[] <- lapply(data, function(v) if (inherits(v, c(\"Date\", \"POSIXt\"))) format(v) else v)",
    "",
    sprintf("lst <- listing_spec(list(\n%s),\n  type = %s%s%s)",
            paste(col_code, collapse = ",\n"),
            encodeString(type, quote = "\""),
            if (!is.na(l$blank_row)) paste0(", blank_row = ", toupper(l$blank_row)) else "",
            if (!is.na(l$wrap)) paste0(", wrap = ", l$wrap) else ""),
    sprintf("content <- as_rtftables(data, listing = lst%s)",
            if (!is.na(l$max_rows)) paste0(", max_rows = ", l$max_rows) else
              "")) |>
    .drop_attached_ns()
}

# ---- the pages -------------------------------------------------------------

#' A listing's pages from its definition
#'
#' What the program [tfl_listing_code()] writes does, done on data in hand:
#' keep the rows `where` says, sort them, print dates as text, and lay them
#' out with rtfreporter's `listing_spec()` / `as_rtftables()`.  The pages go
#' to [tfl_report()] or `rtf_tables()` like a table's.
#'
#' The data comes first, as in [tfl_table_plan()], so a listing can be
#' piped from its data.  (Before 0.0.24.9015 the spec came first; a call in
#' that order stops, saying so.)
#'
#' @param data The listing's dataset (already read, and reworked if it
#'   needs to be).
#' @inheritParams tfl_listing_code
#' @return A list of `rtftable` pages.
#' @examples
#' spec <- tfl_listing_spec(
#'   listings = data.frame(output_id = "L-1", dataset = "ADSL",
#'                         sort = "-AGE", max_rows = "10"),
#'   listing_cols = data.frame(output_id = "L-1",
#'                             vars = c("USUBJID", "AGE | SEX"),
#'                             label = c("Subject", "Age / Sex")))
#' adsl <- data.frame(USUBJID = sprintf("S-%02d", 1:12),
#'                    AGE = 40 + 1:12, SEX = rep(c("F", "M"), 6))
#' pages <- tfl_listing(adsl, spec)
#' length(pages)
#' @export
tfl_listing <- function(data, spec, output_id = NULL, type = "multiline") {
  # the order before 0.0.24.9015 was (spec, data): say so, not a puzzle
  if (inherits(data, "tfl_listing_spec") ||
      (is.data.frame(spec) && !is.data.frame(data))) {
    .ard_stop(paste0(
      "tfl_listing(): the order of the arguments changed: ",
      "tfl_listing(data, spec).\n  The data comes first, as in ",
      "tfl_table_plan(data, spec)."))
  }
  .spec_need_rtfreporter()
  x <- .listing_one(spec, output_id, "tfl_listing")
  l <- x$listing
  cols <- x$cols
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  if (!is.na(l$where)) {
    keep <- eval(str2lang(l$where), data, parent.frame())
    data <- data[!is.na(keep) & keep, , drop = FALSE]
  }
  s <- .split_bar(l$sort)
  if (length(s)) {
    keys <- lapply(s, function(k) {
      desc <- startsWith(k, "-")
      v <- sub("^-", "", k)
      if (!v %in% names(data)) {
        .ard_stop(sprintf("Listing %s sorts by %s, which the data has not.",
                          l$output_id, v))
      }
      if (desc) -xtfrm(data[[v]]) else data[[v]]
    })
    data <- data[do.call(order, unname(keys)), , drop = FALSE]
  }
  data[] <- lapply(data, function(v)
    if (inherits(v, c("Date", "POSIXt"))) format(v) else v)
  lcols <- lapply(seq_len(nrow(cols)), function(i) {
    c <- cols[i, ]
    a <- list(.split_bar(c$vars))
    if (!is.na(c$width)) a$width <- as.numeric(c$width)
    if (!is.na(c$label)) a$label <- .listing_label(c$label)
    if (!is.na(c$sep)) a$sep <- .listing_label(.ard_spec_unquote(c$sep))
    if (!is.na(c$align)) a$align <- c$align
    if (identical(toupper(c$collapse_repeats), "TRUE")) a$collapse_repeats <- TRUE
    do.call(rtfreporter::listing_col, a)
  })
  la <- list(lcols, type = if (is.na(l$type)) type else l$type)
  if (!is.na(l$blank_row)) la$blank_row <- toupper(l$blank_row) == "TRUE"
  if (!is.na(l$wrap)) {
    la$wrap <- eval(str2lang(l$wrap), parent.frame())
  }
  lst <- do.call(rtfreporter::listing_spec, la)
  args <- list(data, listing = lst)
  if (!is.na(l$max_rows)) args$max_rows <- as.numeric(l$max_rows)
  do.call(rtfreporter::as_rtftables, args)
}
