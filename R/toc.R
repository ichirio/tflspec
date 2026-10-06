# ============================================================================
#  A company's table of contents (TOC) as report specs
# ----------------------------------------------------------------------------
#  A study's list of outputs -- number, kind, titles, population, footnotes,
#  program -- is often kept in a workbook of the company's own layout.
#  tfl_read_toc() reads one through a map of its columns and gives the
#  report half of a table spec (report, titles, footnotes): the reports
#  exist, titled, before any table is made.  The spec is not changed.
# ============================================================================

# What a TOC column may be mapped to.
.toc_fields <- c("output_id", "type", "title", "population", "footnote",
                 "program", "file", "note", "section", "label")

#' Read a table of contents (TOC) as report specs
#'
#' Reads a study's list of outputs from a workbook or a `.csv` in the
#' company's own layout, through `map` -- which of its columns is what --
#' and gives the report sheets of a table spec: `report` (`output_id`,
#' `type`, `program`, `file`, `note`), `titles` and `footnotes`.  The
#' result goes to [tfl_write_specs()] / [tfl_write_report_spec()] or into
#' tflplanner.  Nothing else (the table, the ARD) is made.
#'
#' * `title` and `footnote` may each be one column or several (in order).
#'   A cell holding several lines -- a line break, or `" | "` between them
#'   -- gives one line each.
#' * `population` (e.g. "Safety Population") becomes the last title line.
#' * The kind: `type` read loosely (`Table`, `tbl`, `T`, `Figure`, `Fig`,
#'   `Listing`, `Lst` ...); else, with `type_from_id`, from the start of the
#'   output id (`T-14-1-1`, `F14.2`, `L-16-2-7`, `Table 14.1.1`) or of the
#'   first title ("Figure 14.2.1 ..."); else `table`.  Each report whose
#'   kind was guessed is listed in `attr(, "guessed")`.
#' * A row with no output id that says nothing else, or one thing only (a
#'   section heading such as "14.1 Demographics"), is passed over and
#'   listed in `attr(, "skipped")`; a row with no output id that has more
#'   is an error, as is an output id given twice.
#' * For a report list's tokens: `attr(, "labels")` (the map's `label`, the
#'   report's ID as printed, "Table 14.1.1"), `attr(, "first_titles")` and
#'   `attr(, "populations")`, each named by output id, `NA` for none.
#' * Each report's section is in `attr(, "sections")` (named by output id;
#'   `NA` for none): its `section` column when the map names one, else the
#'   heading row above it (the text of the last row passed over as a
#'   heading).  It is not part of the spec: what keeps a report list (an
#'   app) may keep it.
#'
#' @param path An `.xlsx` or `.csv` file.
#' @param map Which column is what: a named character vector or list, the
#'   names among `output_id` (required), `type`, `title`, `population`,
#'   `footnote`, `program`, `file`, `note`, `section`, `label`, each the TOC's column name (or
#'   names, for `title` and `footnote`).  Names are matched ignoring case
#'   and surrounding blanks.
#' @param sheet The sheet of a workbook (name or number); `NULL` for the
#'   first.
#' @param skip Rows above the header row.
#' @param type_from_id Guess a missing or unreadable kind from the output
#'   id or the first title.
#' @return A table spec ([tfl_table_spec()]) holding the report sheets, with
#'   attributes `guessed` and `skipped`.
#' @examples
#' toc <- tempfile(fileext = ".csv")
#' writeLines(c("No.,Kind,Title,Population,Footnotes",
#'              "T-14-1-1,Table,Demographics,Safety Population,N: subjects",
#'              "F-14-2-1,,Mean SBP by visit | Mean (SE),Safety Population,"),
#'            toc)
#' sp <- tfl_read_toc(toc, map = c(output_id = "No.", type = "Kind",
#'                                 title = "Title", population = "Population",
#'                                 footnote = "Footnotes"))
#' sp$report[c("output_id", "type")]
#' sp$titles
#' @export
tfl_read_toc <- function(path, map, sheet = NULL, skip = 0L,
                         type_from_id = TRUE) {
  d <- .toc_read(path, sheet, skip)
  map <- .toc_map(map, names(d))
  get <- function(field) {
    cols <- map[[field]]
    if (!length(cols)) return(NULL)
    lapply(cols, function(cn) d[[cn]])
  }
  id <- get("output_id")[[1L]]
  # what each row says besides its id: a heading says one thing at most
  others <- setdiff(unlist(map), map$output_id)
  said <- if (length(others)) rowSums(!is.na(d[others])) else rep(0L, nrow(d))
  no_id <- is.na(id)
  heading <- no_id & said <= 1L
  bad <- which(no_id & !heading)
  if (length(bad)) {
    .ard_stop(sprintf(paste0(
      "tfl_read_toc(): row(s) %s have no output id (column %s) but other ",
      "cells filled.\n  Give them an id, or empty them if they are ",
      "headings."),
      paste(bad + skip + 1L, collapse = ", "), sQuote(map$output_id)))
  }
  skipped <- d[heading & said > 0L, others, drop = FALSE]
  # each row's section: the text of the last heading row above it (a row
  # with no id and one cell at most, of any column), or its own `section`
  head_text <- apply(d, 1L, function(r) {
    v <- r[!is.na(r)]
    if (length(v)) v[[1L]] else NA_character_
  })
  sec <- rep(NA_character_, nrow(d))
  cur <- NA_character_
  for (i in seq_len(nrow(d))) {
    if (heading[i] && !is.na(head_text[i])) cur <- unname(head_text[i])
    sec[i] <- cur
  }
  if (length(map$section)) {
    own <- d[[map$section[[1L]]]]
    sec[!is.na(own)] <- own[!is.na(own)]
  }
  keep <- !no_id
  dup <- unique(id[keep][duplicated(id[keep])])
  if (length(dup)) {
    .ard_stop(sprintf("tfl_read_toc(): output id(s) given twice: %s.",
                      paste(sQuote(dup), collapse = ", ")))
  }
  d <- d[keep, , drop = FALSE]
  sec <- sec[keep]
  id <- id[keep]
  n <- length(id)
  col1 <- function(field) {
    v <- get(field)
    if (is.null(v)) rep(NA_character_, n) else v[[1L]]
  }
  lines_of <- function(field) {
    v <- get(field)
    if (is.null(v)) return(replicate(n, character(), simplify = FALSE))
    lapply(seq_len(n), function(i) {
      unlist(lapply(v, function(col) .toc_split(col[i])))
    })
  }
  titles <- lines_of("title")
  pop <- col1("population")
  titles <- lapply(seq_len(n), function(i)
    c(titles[[i]], if (!is.na(pop[i])) pop[i]))
  notes <- lines_of("footnote")

  # the kind
  tp <- .toc_type(col1("type"))
  guessed <- character()
  if (isTRUE(type_from_id)) {
    miss <- is.na(tp)
    first_title <- vapply(titles, function(x) if (length(x)) x[1L] else NA_character_, "")
    g <- .toc_type_of_id(id[miss])
    g2 <- .toc_type_of_id(first_title[miss])
    g[is.na(g)] <- g2[is.na(g)]
    tp[miss] <- g
  }
  guessed <- id[is.na(col1("type")) | is.na(.toc_type(col1("type")))]
  tp[is.na(tp)] <- "table"

  report <- data.frame(output_id = id, type = tp, file = col1("file"),
                       program = col1("program"), note = col1("note"),
                       stringsAsFactors = FALSE)
  line_rows <- function(ls, where) {
    rows <- lapply(seq_len(n), function(i) {
      if (!length(ls[[i]])) return(NULL)
      r <- data.frame(output_id = id[i], line = as.character(seq_along(ls[[i]])),
                      left = NA_character_, center = NA_character_,
                      right = NA_character_, stringsAsFactors = FALSE)
      r[[where]] <- ls[[i]]
      r
    })
    rows <- rows[!vapply(rows, is.null, NA)]
    if (length(rows)) do.call(rbind, rows) else NULL
  }
  sheets <- list(report = report)
  tl <- line_rows(titles, "center")
  fn <- line_rows(notes, "left")
  if (!is.null(tl)) sheets$titles <- tl
  if (!is.null(fn)) sheets$footnotes <- fn
  sp <- tfl_table_spec(sheets)
  attr(sp, "guessed") <- guessed
  attr(sp, "skipped") <- skipped
  attr(sp, "sections") <- stats::setNames(sec, id)
  # what a report list keeps for the report's own tokens ({OUTPUT_LABEL},
  # {OUTPUT_TITLE}, {OUTPUT_POPULATION}): the label as printed (the map's
  # `label`), the first title line, the analysis set; NA for none
  attr(sp, "labels") <- stats::setNames(col1("label"), id)
  attr(sp, "first_titles") <- stats::setNames(
    vapply(lines_of("title"), function(x) if (length(x)) x[1L] else NA_character_, ""), id)
  attr(sp, "populations") <- stats::setNames(pop, id)
  sp
}

# The TOC as text: every column character, blanks NA.
.toc_read <- function(path, sheet, skip) {
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) {
    .ard_stop(sprintf("tfl_read_toc(): no file '%s'.", path))
  }
  ext <- tolower(tools::file_ext(path))
  d <- switch(ext,
    xlsx = , xlsm = , xls = as.data.frame(readxl::read_excel(
      path, sheet = sheet %||% 1L, skip = skip, col_types = "text",
      .name_repair = "minimal")),
    csv = utils::read.csv(path, colClasses = "character", skip = skip,
                          check.names = FALSE, fileEncoding = "UTF-8-BOM",
                          na.strings = character()),
    .ard_stop(sprintf("tfl_read_toc(): a TOC is an .xlsx or a .csv; got .%s.", ext)))
  d[] <- lapply(d, function(v) {
    v <- trimws(as.character(v))
    v[!is.na(v) & !nzchar(v)] <- NA_character_
    v
  })
  d[rowSums(!is.na(d)) > 0L, , drop = FALSE]
}

# `map` checked against the TOC's columns: field -> the columns, as named
# in the file.  A column that is not there names the closest ones.
.toc_map <- function(map, cols) {
  if (is.null(map) || !length(map) || is.null(names(map)) ||
      any(!nzchar(names(map)))) {
    .ard_stop("tfl_read_toc(): `map` names each field: c(output_id = \"No.\", title = \"Title\", ...).")
  }
  map <- as.list(map)
  unknown <- setdiff(names(map), .toc_fields)
  if (length(unknown)) {
    .ard_stop(sprintf("tfl_read_toc(): `map` has no field %s; the fields are %s.",
                      paste(sQuote(unknown), collapse = ", "),
                      paste(.toc_fields, collapse = ", ")))
  }
  if (is.null(map$output_id)) {
    .ard_stop("tfl_read_toc(): `map` must say which column is the `output_id`.")
  }
  for (f in c("output_id", "type", "population", "program", "file", "note")) {
    if (length(map[[f]]) > 1L) {
      .ard_stop(sprintf("tfl_read_toc(): `%s` is one column; got %d.", f,
                        length(map[[f]])))
    }
  }
  norm <- function(x) tolower(trimws(x))
  lapply(map, function(want) {
    hit <- match(norm(want), norm(cols))
    if (anyNA(hit)) {
      w <- want[is.na(hit)][1L]
      near <- cols[order(utils::adist(norm(w), norm(cols)))][seq_len(min(3L, length(cols)))]
      .ard_stop(sprintf(
        "tfl_read_toc(): no column %s in the TOC.\n  Closest: %s.\n  Its columns: %s.",
        sQuote(w), paste(sQuote(near), collapse = ", "),
        paste(sQuote(cols), collapse = ", ")))
    }
    cols[hit]
  })
}

# A cell's lines: a line break or " | " between them.
.toc_split <- function(x) {
  if (is.na(x)) return(character())
  v <- trimws(unlist(strsplit(x, "\r?\n|\\s+\\|\\s+")))
  v[nzchar(v)]
}

# The kind, read loosely: Table / tbl / T, Figure / fig / F, Listing / lst /
# L (any case); NA when it is none of them.
.toc_type <- function(x) {
  x <- tolower(trimws(x))
  out <- rep(NA_character_, length(x))
  out[grepl("^(t|tab|tbl|table|tables)$", x)] <- "table"
  out[grepl("^(f|fig|figure|figures|graph|plot)$", x)] <- "figure"
  out[grepl("^(l|lst|lis|list|listing|listings)$", x)] <- "listing"
  out
}

# The kind an id or a title starts with: "T-14-1-1", "Table 14.1.1",
# "F14.2", "Figure 14.2.1 ...", "L-16-2-7"; NA when none.
.toc_type_of_id <- function(x) {
  x <- trimws(x)
  out <- rep(NA_character_, length(x))
  out[grepl("^(table|tab|t)[ ._-]?[0-9]", x, ignore.case = TRUE)] <- "table"
  out[grepl("^(figure|fig|f)[ ._-]?[0-9]", x, ignore.case = TRUE)] <- "figure"
  out[grepl("^(listing|list|l)[ ._-]?[0-9]", x, ignore.case = TRUE)] <- "listing"
  out
}
