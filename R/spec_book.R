# ============================================================================
#  Spec workbooks: each spec with the sheets it needs
# ============================================================================
#
#  A spec workbook holds the sheets of ONE kind of spec -- a table spec its
#  table sheets, a report spec its report sheets, an ARD spec its four --
#  with the `study` keys that kind reads and an `about` sheet stating the
#  spec_version.  Fewer sheets are easier to read.  What a column means is a
#  comment on its header cell (tfl_spec_columns()), not a `_README` sheet.
#
#  Several specs may still share one workbook (tfl_write_specs()): each
#  reader takes its own sheets and passes over the others' sheets and the
#  others' `study` keys.
# ============================================================================

# The sheets of each kind of spec, in the order they are written.
.spec_kind_sheets <- function() {
  s <- names(.ard_spec_schema())
  list(ard     = setdiff(names(.ard_spec_sheets), "study"),
       table   = s[seq_len(which(s == "col_header"))],
       report  = s[seq(which(s == "report"), length(s))],
       listing = names(.listing_sheets))
}

# The `study` keys each kind reads.
.spec_kind_keys <- list(ard = c("id", "output", "source"),
                        table = "rounding",
                        report = c("output_path", "program_dir"),
                        listing = character())

# The kind a sheet belongs to (NA when none does).
.spec_kind_of <- function(sheet) {
  k <- .spec_kind_sheets()
  hit <- vapply(k, function(s) tolower(sheet) %in% s, NA)
  if (any(hit)) names(k)[hit][1L] else NA_character_
}

#' What each column of a spec workbook means
#'
#' The help the spec workbooks carry as comments on their header cells, as
#' one table: every column of every sheet -- ARD, table, report and listing
#' -- with the form of its value, its unit and what a blank cell means, and
#' an example.  A row with no `column` describes the sheet; rows whose
#' `sheet` is in parentheses are about the whole workbook (`output_id`,
#' `note`, the tokens).
#'
#' @param sheet Sheet names to keep the rows of, or `NULL` for every row.
#'   A row naming several sheets (`"titles / footnotes"`) is kept for each.
#' @return A data frame: `sheet`, `column`, `description`, `example`.
#' @examples
#' head(tfl_spec_columns("tables"))
#' tfl_spec_columns("analyses")
#' @export
tfl_spec_columns <- function(sheet = NULL) {
  f <- system.file("spec", "columns.csv", package = "tflspec")
  d <- utils::read.csv(f, stringsAsFactors = FALSE, fileEncoding = "UTF-8",
                       colClasses = "character", na.strings = "")
  d <- d[c("sheet", "column", "description", "example")]
  rownames(d) <- NULL
  if (is.null(sheet)) return(d)
  keep <- vapply(strsplit(d$sheet, " / ", fixed = TRUE),
                 function(s) any(trimws(s) %in% sheet), NA)
  d <- d[keep, , drop = FALSE]
  rownames(d) <- NULL
  d
}

# A sheet's header comments: column -> text (the description, then the
# example).  A row naming several columns ("key / value") comments each.
.spec_column_help <- function(sheet) {
  # the sheet's own rows, then the workbook's (output_id, note)
  d <- tfl_spec_columns(c(sheet, "(workbook)"))
  d <- d[!is.na(d$column) & nzchar(d$column), , drop = FALSE]
  out <- character()
  for (i in seq_len(nrow(d))) {
    txt <- d$description[i]
    if (!is.na(d$example[i]) && nzchar(d$example[i])) {
      txt <- paste0(txt, "\nExample: ", d$example[i])
    }
    for (cn in trimws(strsplit(d$column[i], " / ", fixed = TRUE)[[1L]])) {
      if (!cn %in% names(out)) out[[cn]] <- txt
    }
  }
  out
}

# Write the sheets: the header row frozen, each header cell with the
# column's help as a comment.  Blank cells stay blank.
.write_spec_book <- function(sheets, path) {
  wb <- openxlsx::createWorkbook()
  for (nm in names(sheets)) {
    d <- as.data.frame(sheets[[nm]], stringsAsFactors = FALSE,
                       check.names = FALSE)
    rownames(d) <- NULL
    openxlsx::addWorksheet(wb, nm)
    openxlsx::writeData(wb, nm, d, keepNA = FALSE)
    openxlsx::freezePane(wb, nm, firstRow = TRUE)
    help <- if (startsWith(nm, "_") || nm == "about") character() else
      .spec_column_help(nm)
    for (j in seq_along(names(d))) {
      txt <- help[names(d)[j]]
      if (length(txt) && !is.na(txt)) {
        openxlsx::writeComment(wb, nm, col = j, row = 1L,
                               comment = openxlsx::createComment(
                                 txt, author = "tflspec", visible = FALSE,
                                 width = 4, height = 6))
      }
    }
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

# The `study` sheet for some kinds: their keys always shown (blank or not,
# so each fact has its place), any other key the spec states kept.
.spec_study_sheet <- function(study, kinds) {
  st <- if (is.null(study) || !nrow(study)) {
    data.frame(key = character(), value = character(),
               stringsAsFactors = FALSE)
  } else {
    study[intersect(c("key", "value", "note"), names(study))]
  }
  want <- unlist(.spec_kind_keys[kinds], use.names = FALSE)
  others <- unlist(.spec_kind_keys[setdiff(names(.spec_kind_keys), kinds)],
                   use.names = FALSE)
  # another kind's key goes with that kind, unless it states something
  st <- st[!(st$key %in% others & is.na(st$value)), , drop = FALSE]
  for (k in setdiff(want, st$key)) {
    st[nrow(st) + 1L, c("key", "value")] <- list(k, NA_character_)
  }
  st[order(match(st$key, want)), , drop = FALSE]
}

.spec_about <- function() {
  data.frame(key = "spec_version", value = as.character(.ard_spec_version),
             stringsAsFactors = FALSE)
}

# A table / report spec's sheets for some kinds: that kind's sheets always
# (empty ones too, their columns there to fill in); the other half's only
# when they hold rows, so nothing a spec states is dropped.
.spec_half_sheets <- function(sp, kind) {
  mine <- .spec_kind_sheets()[[kind]]
  other <- .spec_kind_sheets()[[setdiff(c("table", "report"), kind)]]
  # its own sheets first, then any of the other half's that hold rows
  keep <- c(mine, other[vapply(other, function(s) NROW(sp[[s]]) > 0L, NA)])
  stats::setNames(lapply(keep, function(s) sp[[s]]), keep)
}

#' @rdname tfl_write_table_spec
#' @export
tfl_write_report_spec <- function(spec, path) {
  .ard_spec_xlsx_path(path, "tfl_write_report_spec")
  sp <- tfl_table_spec(spec)
  sheets <- c(list(study = .spec_study_sheet(sp$study, "report")),
              .spec_half_sheets(sp, "report"),
              list(about = .spec_about()))
  .write_spec_book(sheets, path)
}

#' Write several specs to one workbook
#'
#' The specs are written each with its sheets, as their own writers would,
#' into one workbook: one `study` sheet with every key the specs read, then
#' the ARD sheets, the table sheets, the report sheets and the listing
#' sheets, and `about`.  Each reader ([tfl_read_ard_spec()],
#' [tfl_read_table_spec()], [tfl_read_report_spec()],
#' [tfl_read_listing_spec()]) takes its own sheets back from it.  Writing
#' each spec to its own workbook (the default of every writer) is easier
#' to read; one workbook is for handing a study's specs over as one file.
#'
#' @param path Destination `.xlsx`.
#' @param ard An ARD spec ([tfl_ard_spec()]), or `NULL`.
#' @param table A table spec ([tfl_table_spec()]): its table sheets, or
#'   `NULL`.
#' @param report A report spec (a [tfl_table_spec()], as
#'   [tfl_read_report_spec()] reads it): its report sheets, or `NULL`.  The
#'   same object as `table` may be given for both.
#' @param listing A listing spec ([tfl_listing_spec()]), or `NULL`.
#' @return `path`, invisibly.
#' @seealso [tfl_write_table_spec()], [tfl_write_ard_spec()],
#'   [tfl_write_listing_spec()]
#' @export
tfl_write_specs <- function(path, ard = NULL, table = NULL, report = NULL,
                            listing = NULL) {
  .ard_spec_xlsx_path(path, "tfl_write_specs")
  kinds <- c(ard = !is.null(ard), table = !is.null(table),
             report = !is.null(report), listing = !is.null(listing))
  if (!any(kinds)) .ard_stop("Give at least one spec to write.")
  ks <- .spec_kind_sheets()
  sheets <- list()
  study <- NULL
  add_study <- function(st) {
    if (is.null(st) || !nrow(st)) return()
    st <- st[intersect(c("key", "value", "note"), names(st))]
    study <<- if (is.null(study)) st else
      rbind(study[intersect(names(study), names(st))],
            st[!st$key %in% study$key, intersect(names(study), names(st))])
  }
  if (!is.null(ard)) {
    a <- .ard_spec_normalized(ard)
    add_study(a$study)
    sheets[ks$ard] <- a[ks$ard]
  }
  if (!is.null(table)) {
    sp <- tfl_table_spec(table)
    add_study(sp$study)
    sheets[ks$table] <- sp[ks$table]
  }
  if (!is.null(report)) {
    sp <- tfl_table_spec(report)
    add_study(sp$study)
    sheets[ks$report] <- sp[ks$report]
  }
  if (!is.null(listing)) {
    l <- tfl_listing_spec(listing, check = FALSE)
    sheets[ks$listing] <- unclass(l)[ks$listing]
  }
  study_kinds <- names(kinds)[kinds & names(kinds) != "listing"]
  out <- c(if (length(study_kinds))
             list(study = .spec_study_sheet(study, study_kinds)),
           sheets, list(about = .spec_about()))
  .write_spec_book(out, path)
}
