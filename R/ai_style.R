# ============================================================================
#  A company's past TFLs, surveyed for their look (10/26, the user)
# ----------------------------------------------------------------------------
#  Before a company standard is written, see what the company's reports
#  already do: where the tables draw their lines (above the table, under
#  the column header, under each spanner of a header of several rows, at
#  the end), how wide the stub is to the value columns, the fonts and
#  sizes, how titles and footnotes are aligned.  R reads the numbers out of
#  the RTF files (tfl_rtf_style_scan(): each table row's cell widths,
#  borders, size and alignment -- never a cell's text); an AI agent, given
#  the prompt and that table, counts the forms and writes the most common
#  ones to a YAML file; tfl_ai_style_read() shows it.  What becomes of it
#  (a company standard) is the person's to decide once they have seen it.
# ============================================================================

.ai_style_default_out <- function() {
  file.path(tools::R_user_dir("tflspec", "data"), "standards", "ai_style.yml")
}

#' The look of a folder of RTF reports, as numbers
#'
#' Reads every `.rtf` file of a folder (and its subfolders) and returns, one
#' row a table row, what its look is made of -- never its text: the cells'
#' widths (`\cellx`), each cell's top and bottom border (`\clbrdrt`,
#' `\clbrdrb`), the font size (`\fs`), the cells' alignment (`\ql`, `\qc`,
#' `\qr`), and the file's fonts (its font table).  Rows in a run, with
#' nothing but row definitions between them, are one table.
#'
#' @param rtf_dir The folder of RTF files.
#' @return A data frame: `file` (its name), `table` (its place in the file),
#'   `row` (in the table), `n_cells`, `width_twips` (the row's width),
#'   `rel_widths` (each cell's width as parts of the narrowest, ` | `
#'   between them), `border_top`, `border_bottom` (one character a cell:
#'   `1` a border, `0` none), `font_size_pt`, `align` (one character a
#'   cell: `l`, `c`, `r`), `fonts` (the file's font table, ` | ` between).
#' @seealso [tfl_ai_style_prompt()]
#' @export
tfl_rtf_style_scan <- function(rtf_dir) {
  fs <- list.files(rtf_dir, pattern = "[.]rtf$", full.names = TRUE, recursive = TRUE,
                   ignore.case = TRUE)
  empty <- data.frame(file = character(), table = integer(), row = integer(),
                      n_cells = integer(), width_twips = integer(), rel_widths = character(),
                      border_top = character(), border_bottom = character(),
                      font_size_pt = numeric(), align = character(), fonts = character(),
                      stringsAsFactors = FALSE)
  rows <- lapply(fs, function(f) {
    x <- paste(readLines(f, warn = FALSE, encoding = "latin1"), collapse = "\n")
    .rtf_style_rows(x, basename(f))
  })
  out <- do.call(rbind, c(list(empty), rows))
  rownames(out) <- NULL
  out
}

# One file's table rows (see tfl_rtf_style_scan())
.rtf_style_rows <- function(x, name) {
  ft <- regmatches(x, regexpr("\\{\\\\fonttbl.*?\\}\\}", x, perl = TRUE))
  # (each entry's name: what is left of it without its control words)
  fonts <- if (length(ft)) {
    ent <- strsplit(ft, "{\\f", fixed = TRUE)[[1L]]
    ent <- ent[grepl("^[0-9]", ent)]
    ent <- gsub("\\\\[a-z]+-?[0-9]* ?", "", sub(";.*$", "", ent), perl = TRUE)
    trimws(gsub("^[0-9]+", "", ent))
  } else character()
  # the document's size, for a row that says none of its own
  first_row <- regexpr("\\\\trowd", x, perl = TRUE)
  pre <- if (first_row > 0L) substring(x, 1L, first_row) else x
  doc_fs <- regmatches(pre, regexpr("\\\\fs[0-9]+", pre))
  doc_pt <- if (length(doc_fs)) as.numeric(substring(doc_fs, 4L)) / 2 else NA_real_
  # each row: from its \trowd to its \row; what lies between two rows says
  # whether they are one table (row definitions only) or two
  at <- gregexpr("\\\\trowd", x, perl = TRUE)[[1L]]
  if (at[1L] < 0L) return(NULL)
  ends <- gregexpr("\\\\row(?![a-z])", x, perl = TRUE)[[1L]]
  out <- list()
  tbl <- 0L
  prev_end <- -1L
  k <- 0L
  for (s in at) {
    e <- ends[ends > s][1L]
    if (is.na(e)) break
    gap <- if (prev_end > 0L) substring(x, prev_end, s - 1L) else ""
    if (prev_end < 0L || grepl("\\\\par(?![a-z])", gap, perl = TRUE)) {
      tbl <- tbl + 1L
      k <- 0L
    }
    k <- k + 1L
    r <- substring(x, s, e)
    # the row definition: each cell's settings, up to its \cellx
    defs <- regmatches(r, gregexpr("(?s).*?\\\\cellx-?[0-9]+", r, perl = TRUE))[[1L]]
    cx <- as.integer(sub("(?s)^.*\\\\cellx(-?[0-9]+)$", "\\1", defs, perl = TRUE))
    if (!length(cx)) next
    w <- diff(c(0L, cx))
    w[w <= 0L] <- NA_integer_
    rel <- if (all(is.na(w))) NA_character_ else
      paste(round(w / min(w, na.rm = TRUE), 1L), collapse = " | ")
    bt <- paste(ifelse(grepl("\\\\clbrdrt", defs), "1", "0"), collapse = "")
    bb <- paste(ifelse(grepl("\\\\clbrdrb", defs), "1", "0"), collapse = "")
    body <- sub("(?s)^.*\\\\cellx-?[0-9]+", "", r, perl = TRUE)
    fsz <- regmatches(r, regexpr("\\\\fs[0-9]+", r))
    al <- regmatches(body, gregexpr("\\\\q[lcrj](?![a-z])", body, perl = TRUE))[[1L]]
    al <- paste(substring(al, 3L, 3L), collapse = "")
    out[[length(out) + 1L]] <- data.frame(
      file = name, table = tbl, row = k, n_cells = length(cx), width_twips = max(cx),
      rel_widths = rel, border_top = bt, border_bottom = bb,
      font_size_pt = if (length(fsz)) as.numeric(substring(fsz, 4L)) / 2 else doc_pt,
      align = al, fonts = paste(fonts, collapse = " | "), stringsAsFactors = FALSE)
    prev_end <- e + 4L
  }
  do.call(rbind, out)
}

#' A prompt for an AI agent to survey a company's past TFLs for their look
#'
#' Writes what an AI agent needs to tell how a company's past reports look,
#' so that a company standard can be written from it: the prompt
#' (`ai_style_prompt.md`) and the numbers R read out of the RTF files
#' (`ai_style_scan.csv`, [tfl_rtf_style_scan()]: cell widths, borders,
#' sizes, alignment -- no text of the reports).  The agent opens the RTF
#' files where it needs to, counts the forms -- where the lines are drawn,
#' the stub's width to the value columns', the fonts, how titles and
#' footnotes are aligned -- and writes the most common ones, with their
#' counts, to `out`, which [tfl_ai_style_read()] reads.  Nothing is sent
#' anywhere; nothing of the reports' text is written.
#'
#' @param rtf_dir The folder of the past RTF reports.
#' @param out The file the agent writes its survey to (YAML); its folder
#'   gets the prompt and the numbers too.  `NULL`: `ai_style.yml` under
#'   `standards/` of tflspec's data folder ([tools::R_user_dir()]).
#' @param dir Instead of `out`: the folder (`out` is `ai_style.yml` in it).
#' @return The prompt (a `tfl_ai_text`), with the files' paths in its
#'   attribute `files` (`prompt`, `scan`, `result`).
#' @seealso [tfl_ai_style_read()]
#' @export
tfl_ai_style_prompt <- function(rtf_dir, out = NULL, dir = NULL) {
  if (!is.character(rtf_dir) || length(rtf_dir) != 1L || !dir.exists(rtf_dir)) {
    .ard_stop("tfl_ai_style_prompt(): `rtf_dir` is a folder of RTF files.")
  }
  out <- out %||% if (!is.null(dir)) file.path(dir, "ai_style.yml") else .ai_style_default_out()
  where <- dirname(out)
  dir.create(where, recursive = TRUE, showWarnings = FALSE)
  scan <- tfl_rtf_style_scan(rtf_dir)
  f_scan <- file.path(where, "ai_style_scan.csv")
  con <- file(f_scan, open = "w", encoding = "UTF-8")
  utils::write.csv(scan, con, row.names = FALSE)
  close(con)
  n_files <- length(unique(scan$file))
  n_tables <- nrow(unique(scan[c("file", "table")]))
  norm <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
  text <- c(
    "# Survey the look of a company's past TFLs",
    "",
    sprintf("The folder `%s` holds a company's past tables, figures and listings as RTF files (%d files, %d tables found).",
            norm(rtf_dir), n_files, n_tables),
    "Count how they look, so that a company standard can be written from what they already do. Report forms and counts only: never copy a report's text (titles, labels, numbers) anywhere.",
    "",
    "## What R read out for you",
    "",
    sprintf("`%s`: one row a table row, the numbers of its look (no text):", norm(f_scan)),
    "",
    "- `file`, `table` (its place in the file; rows in a run with nothing but row definitions between them), `row` (in the table).",
    "- `n_cells`, `width_twips`, `rel_widths`: the cells' widths as parts of the narrowest (`3 | 1 | 1 | 1`: the stub three times a value column).",
    "- `border_top`, `border_bottom`: one character a cell, `1` a border on that side (`\\clbrdrt` / `\\clbrdrb`), `0` none -- `0111` is a line over the last three cells (a spanner's underline).",
    "- `font_size_pt` (`\\fs` / 2), `align` (one letter a cell: `l`, `c`, `r`), `fonts` (the file's font table).",
    "",
    "Work from this table; open an RTF file only to settle what it leaves unclear (which rows are the column header, a table's kind). In RTF a row is `\\trowd ... \\row`; each cell's settings end with its `\\cellx<right edge, twips>`; `\\clbrdrt` / `\\clbrdrb` before it are its top / bottom border; a title or footnote may be a one-cell row or a paragraph (`\\pard ... \\par`).",
    "",
    "## What to count",
    "",
    "1. Lines: where the tables draw them -- above the table, above and below the column header, under each spanner of a header of several rows (over which columns: the spanned ones only, or the whole width), between row groups, at the end of the table, above the footnotes. The common patterns and how many tables have each.",
    "2. Column widths: the stub (the first, row-label column) to a value column, by the kind of table (demographics, adverse events, laboratory, efficacy, listings ...) where you can tell it; the common ratios and their counts.",
    "3. Fonts and sizes: the body's, the titles', the footnotes'.",
    "4. Alignment: titles, footnotes, the column header, the stub, the values.",
    "",
    "## What to write",
    "",
    sprintf("Write the survey to `%s` (UTF-8), in this form -- the forms you found, most common first, each with `count` (tables, or files for fonts):", norm(out)),
    "",
    "```yaml",
    sprintf("tfl_style_survey: {version: 1, files: %d, tables: %d}", n_files, n_tables),
    "borders:",
    "  - {form: \"above the table, under the column header, at the end\", count: 34}",
    "  - {form: \"above the table, under the column header\", count: 6}",
    "column_header:",
    "  - {form: \"two rows: a line under each spanner, over its columns only\", count: 12}",
    "  - {form: \"one row\", count: 20}",
    "widths:",
    "  - {kind: demographics, form: \"stub 3 : values 1\", count: 10}",
    "  - {kind: adverse events, form: \"stub 4 : values 1\", count: 8}",
    "fonts:",
    "  - {form: \"Courier New 9 pt\", count: 40}",
    "titles:",
    "  - {form: \"centred\", count: 38}",
    "footnotes:",
    "  - {form: \"left\", count: 40}",
    "notes: \"anything a person should know: exceptions, files that could not be read\"",
    "```",
    "",
    "Then tell the person the file is written; they read it with `tflspec::tfl_ai_style_read()`.")
  f_prompt <- file.path(where, "ai_style_prompt.md")
  .ai_write_text(text, f_prompt)
  structure(paste(text, collapse = "\n"), class = "tfl_ai_text",
            files = c(prompt = f_prompt, scan = f_scan, result = out))
}

#' Read an AI agent's survey of a company's past TFLs
#'
#' The survey [tfl_ai_style_prompt()] asks the agent to write: the forms of
#' the reports' look (lines, column header, widths, fonts, alignment) and
#' how many tables have each.
#'
#' @param file The survey (YAML); `NULL`: [tfl_ai_style_prompt()]'s default.
#' @return A data frame, one row a form: `section` (`borders`,
#'   `column_header`, `widths`, `fonts`, `titles`, `footnotes`), `kind` (a
#'   table kind, for widths), `form`, `count`; in the order written.  Its
#'   attribute `text` is a short summary, a line a section (the commonest
#'   form and its count), which `print()` shows; `files` / `tables` the
#'   survey's counts and `notes` its notes.
#' @export
tfl_ai_style_read <- function(file = NULL) {
  file <- file %||% .ai_style_default_out()
  if (!file.exists(file)) .ard_stop(sprintf("tfl_ai_style_read(): no survey at %s yet.", file))
  y <- tryCatch(yaml::read_yaml(file), error = function(e) {
    .ard_stop(sprintf("tfl_ai_style_read(): %s is not YAML: %s", file, conditionMessage(e)))
  })
  secs <- c("borders", "column_header", "widths", "fonts", "titles", "footnotes")
  rows <- list()
  for (s in secs) {
    for (e in y[[s]] %||% list()) {
      if (!is.list(e)) e <- list(form = as.character(e))
      cnt <- suppressWarnings(as.integer(e$count %||% NA))
      rows[[length(rows) + 1L]] <- data.frame(
        section = s, kind = as.character(e$kind %||% NA_character_),
        form = as.character(e$form %||% NA_character_), count = cnt, stringsAsFactors = FALSE)
    }
  }
  out <- do.call(rbind, c(list(data.frame(section = character(), kind = character(),
                                          form = character(), count = integer(),
                                          stringsAsFactors = FALSE)), rows))
  hdr <- y$tfl_style_survey %||% list()
  text <- c(sprintf("Survey of %s file(s), %s table(s)", hdr$files %||% "?", hdr$tables %||% "?"),
            unlist(lapply(secs, function(s) {
              d <- out[out$section == s, , drop = FALSE]
              if (!nrow(d)) return(NULL)
              d <- d[order(-ifelse(is.na(d$count), -1L, d$count)), , drop = FALSE]
              sprintf("%s: %s%s (%s)", s, if (!is.na(d$kind[1L])) paste0(d$kind[1L], ", ") else "",
                      d$form[1L], if (is.na(d$count[1L])) "?" else d$count[1L])
            })))
  structure(out, class = c("tfl_ai_style", "data.frame"), text = text,
            files = hdr$files, tables = hdr$tables, notes = y$notes)
}

#' @export
print.tfl_ai_style <- function(x, ...) {
  cat(attr(x, "text"), sep = "\n")
  n <- attr(x, "notes")
  if (length(n) && nzchar(n)) cat("notes:", n, "\n")
  invisible(x)
}
