# The flat view of the YAML spec: the parts that are rows (the report list,
# the data catalog, the analyses, code lists, tokens, the lines of a header
# or footer, a listing's columns) written to a workbook to be read in Excel,
# one row a thing, keyed as today's sheets are (output_id blank: the
# defaults; @<name>: a pattern; else the report).  What a sheet cannot
# hold -- the layers of `content:`, `!r` values, nested calls, a figure's
# plot -- is listed on `_not_in_xlsx`.  Design: spec-format-redesign 5.

#' Write the flat view of a study's YAML spec as a workbook
#'
#' Reads the study's YAML spec (`tfl_read_study_spec()`,
#' `tfl_read_outputs()`; the files as written, not layered) and writes the
#' parts that are rows to `path`: `toc`, `datasets`, `populations`,
#' `analysis_data`, `analyses`, `codelists`, `tokens`, `header`, `footer`,
#' `titles`, `footnotes`, `page`, `report`, `listing_cols`, and
#' `_not_in_xlsx` (what the workbook does not hold: `content:`, `!r`
#' values, nested calls, a figure's plot).  Lists are written `a | b`;
#' R text as it is (the columns that hold R are known).  The workbook is a
#' view for Excel; an import back changes only these rows, by their keys.
#'
#' @param dir The study folder (or its `spec/`).
#' @param path The workbook to write.
#' @return Invisibly, a list: `path`, `sheets` (the names written),
#'   `not_in_xlsx` (a data frame: `output`, `part`, `reason`).
#' @export
tfl_write_spec_xlsx <- function(dir, path) {
  .ard_need("openxlsx", "tfl_write_spec_xlsx()")
  s <- tfl_read_study_spec(dir)
  outs <- tfl_read_outputs(dir)
  nx <- list()
  note <- function(output, part, reason) {
    nx[[length(nx) + 1L]] <<- data.frame(output = output, part = part, reason = reason,
                                         stringsAsFactors = FALSE)
  }
  sheets <- list()
  add_rows <- function(sheet, rows) {
    if (!length(rows)) return()
    sheets[[sheet]] <<- c(sheets[[sheet]], rows)
  }
  # toc
  add_rows("toc", lapply(s$toc$outputs %||% list(), function(e) .flat_map(e, "toc", NA, note)))
  # the data catalog
  add_rows("datasets", lapply(s$data$datasets %||% list(), function(e) .flat_map(e, "datasets", NA, note)))
  add_rows("populations", lapply(s$data$populations %||% list(), function(e) .flat_map(e, "populations", NA, note)))
  # the defaults and patterns: the page parts, the code list templates
  scopes <- c(list(list(id = NA_character_, layer = s$defaults)),
              lapply(names(s$patterns), function(n) list(id = paste0("@", n), layer = s$patterns[[n]])))
  for (sc in scopes) {
    .flat_page(sc$layer$page, sc$id, add_rows, note)
    if (length(sc$layer$content)) note(sc$id, "content", "the layers (content) are not rows")
  }
  for (v in names(s$codelists)) {
    add_rows("codelists", lapply(s$codelists[[v]], function(e) c(list(output_id = NA_character_, variable = v), e)))
  }
  # each report
  for (id in names(outs)) {
    o <- outs[[id]]
    if (length(o$content)) note(id, "content", "the layers (content) are not rows")
    if (!is.null(o$data)) {
      for (e in o$data$analysis_data %||% list()) add_rows("analysis_data", list(.flat_map(c(list(output_id = id), e), "analysis_data", id, note)))
      for (e in o$data$analyses %||% list()) add_rows("analyses", list(.flat_map(c(list(output_id = id), e), "analyses", id, note)))
      for (k in setdiff(names(o$data), c("source", "analysis_data", "analyses", "dataset", "where", "sort"))) {
        note(id, paste0("data.", k), "not rows (code, steps, objects)")
      }
    }
    for (v in names(o$codelists)) {
      add_rows("codelists", lapply(o$codelists[[v]], function(e) c(list(output_id = id, variable = v), e)))
    }
    .flat_page(o$page, id, add_rows, note)
    if (o$kind == "listing") {
      pl <- Filter(function(it) identical(it$fn, "plan_listing"), o$content)
      cols <- if (length(pl)) Filter(function(cl) inherits(cl, "tfl_call"), pl[[1L]]$args$cols %||% list()) else list()
      add_rows("listing_cols", lapply(cols, function(cl) .flat_map(c(list(output_id = id), cl$args), "listing_cols", id, note)))
      nx <- Filter(function(r) !(identical(r$output, id) && identical(r$part, "content")), nx)
      if (length(o$content) > 1L || (length(pl) && length(setdiff(names(pl[[1L]]$args), "cols")))) note(id, "content", "the listing's plan (type, blanks, pages) is not rows")
    }
    for (k in setdiff(names(o), c(.output_fields))) note(id, k, "a figure's own part (plot, plots, compose, template)")
  }
  # the workbook
  wb <- openxlsx::createWorkbook()
  written <- character()
  order <- c("toc", "datasets", "populations", "analysis_data", "analyses", "codelists",
             "tokens", "header", "footer", "titles", "footnotes", "page", "report", "listing_cols")
  for (sh in intersect(order, names(sheets))) {
    d <- .rows_frame(sheets[[sh]])
    openxlsx::addWorksheet(wb, sh)
    openxlsx::writeData(wb, sh, d, keepNA = FALSE)
    openxlsx::freezePane(wb, sh, firstRow = TRUE)
    written <- c(written, sh)
  }
  nxd <- if (length(nx)) do.call(rbind, nx) else
    data.frame(output = character(), part = character(), reason = character(), stringsAsFactors = FALSE)
  openxlsx::addWorksheet(wb, "_not_in_xlsx")
  openxlsx::writeData(wb, "_not_in_xlsx", nxd, keepNA = FALSE)
  written <- c(written, "_not_in_xlsx")
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(list(path = path, sheets = written, not_in_xlsx = nxd))
}

# a map as one row of text cells: scalars as they are, vectors `a | b`, an
# !r as its text, an !unset as "(none)", a map or a call noted
.flat_map <- function(m, sheet, output, note) {
  out <- list()
  for (k in names(m)) {
    v <- m[[k]]
    if (is.null(v) || (is.atomic(v) && length(v) == 1L && is.na(v))) next
    if (inherits(v, "tfl_unset")) { out[[k]] <- "(none)"; next }
    if (inherits(v, "tfl_r")) { out[[k]] <- as.character(v); next }
    if (inherits(v, "tfl_call")) { note(output, paste0(sheet, ".", k), "a nested call"); next }
    if (is.list(v)) {
      if (!is.null(names(v))) { note(output, paste0(sheet, ".", k), "a map (not one cell)"); next }
      v <- unlist(v)
    }
    if (is.logical(v)) v <- ifelse(v, "TRUE", "FALSE")
    out[[k]] <- paste(as.character(v), collapse = " | ")
  }
  out
}

# the page parts of one scope as rows of the band, tokens, page and report sheets
.flat_page <- function(page, id, add_rows, note) {
  if (is.null(page) || !length(page)) return()
  for (s in c("header", "footer", "titles", "footnotes")) {
    m <- page[[s]]
    if (is.null(m) || !length(m) || inherits(m, "tfl_unset")) next
    rows <- lapply(names(m), function(line) {
      cells <- m[[line]]
      r <- list(output_id = id, line = line)
      if (inherits(cells, "tfl_unset")) { r$left <- "(none)"; return(r) }
      for (cn in c("left", "center", "right")) {
        v <- cells[[cn]]
        if (!is.null(v)) r[[cn]] <- if (inherits(v, "tfl_unset")) "(none)" else as.character(v)
      }
      r
    })
    add_rows(s, rows)
  }
  if (length(page$tokens)) {
    add_rows("tokens", lapply(names(page$tokens), function(n) {
      v <- page$tokens[[n]]
      list(output_id = id, name = n, value = if (inherits(v, "tfl_unset")) "(none)" else as.character(v))
    }))
  }
  pr <- .page_columns(page)
  for (s in c("page", "report")) {
    if (length(pr[[s]])) add_rows(s, list(c(list(output_id = id), pr[[s]])))
  }
  for (k in setdiff(names(page), .page_known)) {
    note(id, paste0("page.", k), "no sheet for it")
  }
}

# rows (lists of scalars) as one data frame of text, the columns in the
# order they first appear
.rows_frame <- function(rows) {
  cols <- unique(unlist(lapply(rows, names)))
  d <- as.data.frame(lapply(cols, function(cn) vapply(rows, function(r) {
    v <- r[[cn]]
    if (is.null(v)) NA_character_ else as.character(v)[1L]
  }, "")), stringsAsFactors = FALSE)
  names(d) <- cols
  d
}
