# ============================================================================
#  The TOC / SHELL workbook as text for an AI chat
# ----------------------------------------------------------------------------
#  A company's TOC and shells come in its own layout: a TOC sheet, a sheet a
#  shell (or several shells in one sheet), titles in merged cells, the stub
#  indented by the cell's alignment.  Nothing here knows that layout.  The
#  workbook is read as what it is -- an .xlsx is a zip of XML -- and every
#  non-empty cell is written in reading order with its place (`B3`, a merged
#  range `A1:E1`), the sheet's name kept, so the assistant sees the shell as
#  a person does.  Only base R (utils::unzip, regular expressions): the
#  XML of an .xlsx is machine-written and regular, and openxlsx / readxl
#  neither keep a cell's place nor its indent.
#
#    tfl_ai_read_shell()   the workbook as text, all of it or some outputs
#    tfl_ai_batches()      its tables in batches of one family each
# ============================================================================

# The characters the text holds at most, by default (about 10k tokens).
.ai_shell_max_chars <- 40000L

# The longest cell text written (the rest is cut, with an ellipsis).
.ai_shell_max_cell <- 300L

#' A TOC / SHELL workbook as text for an AI chat
#'
#' Reads a company's TOC and shells from one Excel workbook without knowing
#' its layout and writes them as compact text an AI chat reads: every sheet
#' (its name kept), its non-empty cells in reading order -- row by row, left
#' to right -- each with its column letter, a merged cell once with its range
#' (`A1:E1=...`), a cell's indent (the alignment's indent level) as two
#' spaces a level, a line break inside a cell as `\n`.  Nothing is sent
#' anywhere: paste the text into the chat, or attach the workbook itself
#' when the chat reads Excel.
#'
#' The report numbers are found as they are written -- `T-14-1-1`,
#' `Table 14.1.1`, `14.1.1`, `F14.2.1` -- and matched whatever the
#' punctuation (`T-14-1-1` is `Table 14.1.1`; full-width digits too).  The
#' TOC is the sheet named like one (TOC, contents, index, list, the Japanese
#' words for them ...) that lists report numbers, or else the sheet listing
#' the most report numbers; a header cell saying title (Title, Output
#' Title, or the Japanese words for it) above the first report names the
#' title column.  A shell is a sheet named by its report's number, or
#' the rows of a sheet from the row that names its number to the row before
#' the next number.
#'
#' * `outputs`: only these reports -- the TOC's header rows and the reports'
#'   rows, and each report's shell.
#' * `sheets`: only these sheets, whole.
#'
#' @param path The workbook (`.xlsx` or `.xlsm`).
#' @param outputs Report numbers to keep (as the workbook or your TOC writes
#'   them); `NULL` for all.
#' @param sheets Sheet names (or positions) to keep whole; `NULL` for all.
#'   With `outputs` too, the sheets are added to the reports' parts.
#' @param toc_sheet The TOC sheet's name, when the guess is wrong; `NA` for
#'   none.
#' @param max_chars The text's length at most; the rest is cut and the text
#'   says so (pick fewer `outputs`).
#' @param file A file to write the text to (UTF-8), or `NULL`.
#' @param shells Workbooks of shells apart from the TOC's (`path`): their
#'   sheets join the TOC workbook's -- a sheet named as one already there
#'   becomes `"<sheet> [<workbook>]"` -- and each report's shell is looked
#'   for in all of them.  The TOC is `path`'s.  The text has a part a
#'   workbook.
#' @return A `tfl_ai_shell`: a list with `text` (one string), `toc` (a data
#'   frame, one row a report the TOC lists: `output_id` as written, `key`,
#'   `type`, `title`, `text` -- the row's text -- `sheet`, `row`, `shell` --
#'   the sheet of its shell, `NA` for none found), `sheets` (`sheet`,
#'   `role` -- `toc`, `shell` or `other` -- `rows`, `cells`, `hidden`),
#'   `cells` (every non-empty cell: `sheet`, `row`, `col`, `ref`, `text`),
#'   `outputs` (the reports kept), `truncated`, `path` and `args` (the
#'   `toc_sheet` and `sheets` given: [tfl_ai_context()] reads each request's
#'   reports with them again).  `format()` gives the text; `print()` a
#'   summary, and what to pass when no TOC or a report's shell was found.
#' @seealso [tfl_ai_batches()] to split the TOC into requests,
#'   [tfl_ai_adam_summary()], [tfl_ai_context()] (`shell =`).
#' @examples
#' f <- system.file("ai", "example", "onco_toc_shell.xlsx", package = "tflspec")
#' x <- tfl_ai_read_shell(f)
#' x
#' x$toc[c("output_id", "title", "shell")]
#' cat(substr(format(tfl_ai_read_shell(f, outputs = "T-14-1-1")), 1, 900))
#' @export
tfl_ai_read_shell <- function(path, outputs = NULL, sheets = NULL,
                              toc_sheet = NULL, max_chars = .ai_shell_max_chars,
                              file = NULL, shells = NULL) {
  fn <- "tfl_ai_read_shell()"
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) {
    .ard_stop(sprintf(paste0("%s: no file %s in %s; give the workbook's path from that ",
                             "folder, or its full path."), fn, sQuote(path, FALSE),
                      sQuote(getwd(), FALSE)))
  }
  wb <- .xl_read(path, fn)
  cells <- wb$cells
  info <- wb$sheets
  info$book <- basename(path)
  info$book_path <- normalizePath(path, winslash = "/")
  info$name <- info$sheet
  # the shells in workbooks of their own: their sheets join the TOC's (a
  # sheet's name another has already: "<sheet> [<book>]")
  for (f in shells %||% character()) {
    if (!is.character(f) || length(f) != 1L || !file.exists(f)) {
      .ard_stop(sprintf("%s: `shells`: no file %s.", fn, sQuote(f, FALSE)))
    }
    w <- .xl_read(f, fn)
    s <- w$sheets
    s$name <- s$sheet
    new <- ifelse(s$sheet %in% info$sheet, sprintf("%s [%s]", s$sheet, basename(f)), s$sheet)
    w$cells$sheet <- new[match(w$cells$sheet, s$sheet)]
    s$sheet <- new
    s$book <- basename(f)
    s$book_path <- normalizePath(f, winslash = "/")
    info <- rbind(info, s)
    cells <- rbind(cells, w$cells)
  }
  # the TOC is the TOC workbook's
  toc_name <- .ai_shell_toc_sheet(info[info$book_path == info$book_path[1L], , drop = FALSE],
                                  cells, toc_sheet, fn)
  toc <- .ai_shell_toc(cells, toc_name)
  toc$shell <- .ai_shell_find(toc, info, cells, toc_name)
  info$role <- ifelse(info$sheet %in% toc_name, "toc",
                      ifelse(info$sheet %in% toc$shell, "shell", "other"))
  # the workbook each report's shell is in (its TOC's when none was found)
  toc$book <- info$book[match(toc$shell, info$sheet)]
  toc$book[is.na(toc$book)] <- info$book[1L]
  keep <- .ai_shell_keep(cells, info, toc, toc_name, outputs, sheets, fn)
  # one part a workbook
  books <- unique(info$book_path)
  text <- paste(unlist(lapply(books, function(b) {
    bi <- info[info$book_path == b, , drop = FALSE]
    bc <- keep$cells[keep$cells$sheet %in% bi$sheet, , drop = FALSE]
    if (!nrow(bc) && !identical(b, books[1L])) return(NULL)
    .ai_shell_text(bi$book[1L], bi, bc, keep$outputs)
  })), collapse = "\n\n")
  truncated <- FALSE
  if (is.numeric(max_chars) && length(max_chars) == 1L && nchar(text) > max_chars) {
    cut <- max(which(cumsum(nchar(strsplit(text, "\n", fixed = TRUE)[[1L]]) + 1L) <= max_chars),
               1L)
    lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
    text <- paste(c(lines[seq_len(cut)], "", sprintf(paste0(
      "[... cut at %d characters of %d: pick fewer reports ",
      "(tfl_ai_read_shell(outputs = ...)) or sheets]"), max_chars, nchar(text))),
      collapse = "\n")
    truncated <- TRUE
  }
  out <- structure(list(
    text = text, toc = toc,
    sheets = info[c("sheet", "role", "rows", "cells", "hidden", "book", "name")],
    cells = cells, outputs = keep$outputs, truncated = truncated,
    path = normalizePath(path, winslash = "/"),
    books = data.frame(book = unique(info$book), path = books,
                       role = c("toc", rep("shells", length(books) - 1L)),
                       stringsAsFactors = FALSE),
    # what a request's own read of the workbook passes again
    args = list(toc_sheet = toc_sheet, sheets = sheets, shells = shells)
  ), class = "tfl_ai_shell")
  if (!is.null(file)) .ai_write_text(text, file)
  out
}

#' @export
format.tfl_ai_shell <- function(x, ...) x$text

#' @export
print.tfl_ai_shell <- function(x, ...) {
  cat(sprintf("<tfl_ai_shell: %s>\n", basename(x$path)))
  s <- x$sheets
  if (length(x$books$book) > 1L) {
    cat(sprintf("%d workbooks: %s (the TOC), %s\n", nrow(x$books), x$books$book[1L],
                paste(x$books$book[-1L], collapse = ", ")))
  }
  cat(sprintf("%d sheet(s): %s\n", nrow(s),
              paste(sprintf("%s (%s)", s$sheet, s$role), collapse = ", ")))
  cat(sprintf("%d report(s) in the TOC, %d with a shell found\n",
              nrow(x$toc), sum(!is.na(x$toc$shell))))
  if (length(x$outputs)) {
    cat(sprintf("kept: %s\n", paste(x$outputs, collapse = ", ")))
  }
  cat(sprintf("text: %d characters%s; format(x) gives it\n", nchar(x$text),
              if (isTRUE(x$truncated)) " (cut)" else ""))
  hint <- .ai_shell_hint(x)
  if (length(hint)) cat(paste0("Next: ", hint, "\n"), sep = "")
  invisible(x)
}

# What to pass when the guess missed: no TOC, or reports with no shell.
.ai_shell_hint <- function(x) {
  file <- basename(x$path)
  others <- x$sheets$sheet[x$sheets$role != "toc"]
  q <- function(v) paste0("\"", v, "\"")
  if (!nrow(x$toc)) {
    return(sprintf(paste0("no TOC found: name the TOC sheet, ",
                          "tfl_ai_read_shell(\"%s\", toc_sheet = %s) (the sheets: %s)."),
                   file, q(x$sheets$sheet[1L]), paste(q(x$sheets$sheet), collapse = ", ")))
  }
  miss <- x$toc$output_id[is.na(x$toc$shell)]
  if (!length(miss)) return(character())
  cand <- setdiff(others, x$toc$shell)
  sprintf(paste0("no shell found for %d report(s) (%s): add the sheets that hold them, ",
                 "tfl_ai_read_shell(\"%s\", sheets = c(%s)); their requests then ",
                 "carry those sheets whole."),
          length(miss), paste(utils::head(miss, 8L), collapse = ", "), file,
          paste(q(utils::head(if (length(cand)) cand else others, 5L)), collapse = ", "))
}

# ---- the workbook's XML ------------------------------------------------------

# A member of the zip as one UTF-8 string ("" when it is not there).
.xl_member <- function(path, member, members) {
  if (!member %in% members) return("")
  con <- unz(path, member, open = "rb")
  on.exit(close(con), add = TRUE)
  raw <- readBin(con, "raw", n = 1e9)
  x <- rawToChar(raw)
  Encoding(x) <- "UTF-8"
  x
}

# The attribute `name` of each tag in `tags` (NA when absent).
.xl_attr <- function(tags, name) {
  re <- sprintf("\\s%s=\"([^\"]*)\"", name)
  m <- regmatches(tags, regexec(re, tags))
  vapply(m, function(z) if (length(z) == 2L) .xl_unescape(z[2L]) else NA_character_, "")
}

.xl_unescape <- function(x) {
  if (!length(x)) return(x)
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&apos;", "'", x, fixed = TRUE)
  num <- function(x, re, base) {
    while (any(grepl(re, x, perl = TRUE))) {
      m <- regmatches(x, regexpr(re, x, perl = TRUE))
      for (e in unique(m)) {
        code <- strtoi(sub(re, "\\1", e, perl = TRUE), base)
        x <- gsub(e, if (is.na(code)) "" else intToUtf8(code), x, fixed = TRUE)
      }
    }
    x
  }
  x <- num(x, "&#([0-9]+);", 10L)
  x <- num(x, "&#[xX]([0-9a-fA-F]+);", 16L)
  x <- gsub("_x000[dD]_", "", x)
  gsub("&amp;", "&", x, fixed = TRUE)
}

# The text of a string item (<si> or <is>): its <t> runs, without the
# phonetic runs (<rPh>) Japanese Excel keeps beside the text.
.xl_si_text <- function(x) {
  x <- gsub("<rPh\\b.*?</rPh>", "", x, perl = TRUE)
  t <- regmatches(x, gregexpr("<t(\\s[^>]*)?>.*?</t>", x, perl = TRUE))
  vapply(t, function(z) {
    .xl_unescape(paste(sub("^<t(\\s[^>]*)?>(.*)</t>$", "\\2", z, perl = TRUE),
                       collapse = ""))
  }, "")
}

# The column number of a letter (A = 1, AA = 27).
.xl_col_num <- function(letters) {
  vapply(toupper(letters), function(l) {
    d <- utf8ToInt(l) - 64L
    as.integer(sum(d * 26L^rev(seq_along(d) - 1L)))
  }, 1L, USE.NAMES = FALSE)
}

.xl_col_letter <- function(n) {
  vapply(n, function(k) {
    s <- ""
    while (k > 0L) {
      r <- (k - 1L) %% 26L
      s <- paste0(LETTERS[r + 1L], s)
      k <- (k - 1L) %/% 26L
    }
    s
  }, "")
}

# The workbook: its sheets (sheet, hidden, member) and every non-empty cell
# (sheet, row, col, ref, text: a merged cell once, with its range).
.xl_read <- function(path, fn) {
  members <- tryCatch(utils::unzip(path, list = TRUE)$Name, error = function(e) NULL)
  if (is.null(members) || !"xl/workbook.xml" %in% members) {
    .ard_stop(sprintf(paste0(
      "%s: %s is not an .xlsx workbook (an old .xls: save it as .xlsx in ",
      "Excel first)."), fn, sQuote(basename(path), FALSE)))
  }
  wbx <- .xl_member(path, "xl/workbook.xml", members)
  st <- regmatches(wbx, gregexpr("<sheet\\s[^>]*>", wbx, perl = TRUE))[[1L]]
  rels <- .xl_member(path, "xl/_rels/workbook.xml.rels", members)
  rl <- regmatches(rels, gregexpr("<Relationship\\s[^>]*>", rels, perl = TRUE))[[1L]]
  target <- stats::setNames(.xl_attr(rl, "Target"), .xl_attr(rl, "Id"))
  rid <- .xl_attr(st, "r:id")
  member <- unname(target[rid])
  member <- ifelse(startsWith(member, "/"), substring(member, 2L), paste0("xl/", member))
  info <- data.frame(sheet = .xl_attr(st, "name"),
                     hidden = !is.na(.xl_attr(st, "state")) & .xl_attr(st, "state") != "visible",
                     member = member, stringsAsFactors = FALSE)
  info <- info[grepl("worksheets/", info$member, fixed = TRUE) & info$member %in% members, ,
               drop = FALSE]
  sst <- .xl_member(path, "xl/sharedStrings.xml", members)
  shared <- .xl_si_text(regmatches(sst, gregexpr("<si>.*?</si>|<si/>", sst, perl = TRUE))[[1L]])
  indent <- .xl_indents(.xl_member(path, "xl/styles.xml", members))
  cells <- lapply(seq_len(nrow(info)), function(i) {
    .xl_sheet_cells(.xl_member(path, info$member[i], members), info$sheet[i], shared, indent)
  })
  cells <- do.call(rbind, c(list(.xl_no_cells()), cells))
  info$rows <- vapply(info$sheet, function(s) length(unique(cells$row[cells$sheet == s])), 1L)
  info$cells <- vapply(info$sheet, function(s) sum(cells$sheet == s), 1L)
  rownames(info) <- NULL
  list(sheets = info, cells = cells)
}

.xl_no_cells <- function() {
  data.frame(sheet = character(), row = integer(), col = integer(), ref = character(),
             text = character(), stringsAsFactors = FALSE)
}

# The indent level of each cell style (cellXfs, by position from 0).
.xl_indents <- function(styles) {
  xfs <- regmatches(styles, regexpr("<cellXfs\\b.*?</cellXfs>", styles, perl = TRUE))
  if (!length(xfs)) return(integer())
  xf <- regmatches(xfs, gregexpr("<xf\\b[^>]*/>|<xf\\b.*?</xf>", xfs, perl = TRUE))[[1L]]
  vapply(seq_along(xf), function(i) {
    a <- regmatches(xf[i], regexpr("<alignment\\b[^>]*>", xf[i], perl = TRUE))
    if (!length(a)) return(0L)
    v <- suppressWarnings(as.integer(.xl_attr(a, "indent")))
    if (is.na(v)) 0L else v
  }, 1L)
}

# One sheet's non-empty cells, a merged range once (its top-left cell's text
# with the range as the ref).
.xl_sheet_cells <- function(x, sheet, shared, indent) {
  if (!nzchar(x)) return(.xl_no_cells())
  rows <- regmatches(x, gregexpr("<row\\b[^>]*?(/>|>.*?</row>)", x, perl = TRUE))[[1L]]
  out <- vector("list", length(rows))
  last_row <- 0L
  for (i in seq_along(rows)) {
    rtag <- regmatches(rows[i], regexpr("<row\\b[^>]*>", rows[i], perl = TRUE))
    r <- suppressWarnings(as.integer(.xl_attr(rtag, "r")))
    if (is.na(r)) r <- last_row + 1L
    last_row <- r
    cs <- regmatches(rows[i], gregexpr("<c\\b[^>]*?(/>|>.*?</c>)", rows[i], perl = TRUE))[[1L]]
    if (!length(cs)) next
    ctag <- regmatches(cs, regexpr("<c\\b[^>]*>", cs, perl = TRUE))
    ref <- .xl_attr(ctag, "r")
    col <- integer(length(cs))
    prev <- 0L
    for (j in seq_along(cs)) {
      col[j] <- if (is.na(ref[j])) prev + 1L else .xl_col_num(sub("[0-9]+$", "", ref[j]))
      prev <- col[j]
    }
    type <- .xl_attr(ctag, "t")
    style <- suppressWarnings(as.integer(.xl_attr(ctag, "s")))
    v <- regmatches(cs, regexec("<v>(.*?)</v>", cs))
    v <- vapply(v, function(z) if (length(z) == 2L) .xl_unescape(z[2L]) else NA_character_, "")
    txt <- character(length(cs))
    for (j in seq_along(cs)) {
      tp <- type[j]
      txt[j] <- if (identical(tp, "s")) {
        k <- suppressWarnings(as.integer(v[j]))
        if (!is.na(k) && k + 1L <= length(shared)) shared[k + 1L] else ""
      } else if (identical(tp, "inlineStr")) {
        .xl_si_text(cs[j])
      } else if (identical(tp, "b")) {
        if (identical(v[j], "1")) "TRUE" else if (is.na(v[j])) "" else "FALSE"
      } else if (is.na(v[j])) {
        ""
      } else if (identical(tp, "str") || identical(tp, "e")) {
        v[j]
      } else {
        num <- suppressWarnings(as.numeric(v[j]))
        if (is.na(num)) v[j] else format(num, digits = 15L, trim = TRUE)
      }
      lev <- if (!is.na(style[j]) && style[j] + 1L <= length(indent)) indent[style[j] + 1L] else 0L
      if (lev > 0L && nzchar(trimws(txt[j]))) {
        txt[j] <- paste0(strrep("  ", lev), txt[j])
      }
    }
    keep <- nzchar(trimws(txt))
    if (!any(keep)) next
    out[[i]] <- data.frame(sheet = sheet, row = r, col = col[keep],
                           ref = paste0(.xl_col_letter(col[keep]), r),
                           text = txt[keep], stringsAsFactors = FALSE)
  }
  d <- do.call(rbind, c(list(.xl_no_cells()), out))
  mc <- regmatches(x, gregexpr("<mergeCell\\s[^>]*>", x, perl = TRUE))[[1L]]
  for (rg in .xl_attr(mc, "ref")) {
    if (is.na(rg) || !grepl(":", rg, fixed = TRUE)) next
    tl <- strsplit(rg, ":", fixed = TRUE)[[1L]][1L]
    d$ref[d$ref == tl] <- rg
  }
  d <- d[order(d$row, d$col), , drop = FALSE]
  rownames(d) <- NULL
  d
}

# ---- report numbers ---------------------------------------------------------

# A report number inside a text: an optional kind (Table, Figure, Listing,
# T, F, L, or the Japanese words), then two numbers or more.
.ai_id_regex <- paste0(
  "(?i)(?<![A-Za-z0-9])(?:(table|figure|listing|tab|fig|lst|t|f|l|",
  "\u8868|\u56f3|\u4e00\u89a7)[ ._-]?)?",
  "([0-9]{1,3}(?:[._-][0-9]{1,3}){1,5}[a-z]?)(?![0-9])")

# Full-width letters, digits and punctuation (\uff01-\uff5e, as Japanese
# workbooks may write a number: \uff14.1.1) as their ASCII ones; the
# ideographic space as a space.
.ai_half_width <- function(x) {
  if (!length(x) || !any(grepl("[\uff01-\uff5e\u3000]", x, perl = TRUE))) return(x)
  vapply(x, function(z) {
    if (is.na(z)) return(z)
    u <- utf8ToInt(z)
    w <- u >= 0xff01L & u <= 0xff5eL
    u[w] <- u[w] - 0xfee0L
    u[u == 0x3000L] <- 32L
    intToUtf8(u)
  }, "", USE.NAMES = FALSE)
}

# The report numbers a text names: the text as written, their key and kind.
.ai_ids_in <- function(text) {
  text <- .ai_half_width(text)
  m <- gregexpr(.ai_id_regex, text, perl = TRUE)
  hits <- regmatches(text, m)
  lapply(hits, function(h) {
    if (!length(h)) return(data.frame(id = character(), key = character(), type = character()))
    data.frame(id = trimws(h), key = .ai_id_key(h), type = .ai_id_type(h),
               stringsAsFactors = FALSE)
  })
}

# A report number's key: the kind's letter (T, F, L; none when not said),
# then its numbers joined with dots.
.ai_id_key <- function(x) {
  x <- trimws(.ai_half_width(x))
  out <- rep(NA_character_, length(x))
  ok <- grepl(.ai_id_regex, x, perl = TRUE)
  num <- vapply(x, function(z) {
    if (!grepl(.ai_id_regex, z, perl = TRUE)) return(NA_character_)
    n <- sub(paste0("^.*?", .ai_id_regex, ".*$"), "\\2", z, perl = TRUE)
    toupper(gsub("[._-]", ".", n))
  }, "", USE.NAMES = FALSE)
  tp <- .ai_id_type(x)
  letter <- c(table = "T", figure = "F", listing = "L")[tp]
  out[ok] <- paste0(ifelse(is.na(letter[ok]), "", letter[ok]), num[ok])
  out
}

.ai_id_type <- function(x) {
  k <- tolower(vapply(x, function(z) {
    if (!grepl(.ai_id_regex, z, perl = TRUE)) return("")
    sub(paste0("^.*?", .ai_id_regex, ".*$"), "\\1", z, perl = TRUE)
  }, "", USE.NAMES = FALSE))
  out <- rep(NA_character_, length(k))
  out[k %in% c("table", "tab", "t", "\u8868")] <- "table"
  out[k %in% c("figure", "fig", "f", "\u56f3")] <- "figure"
  out[k %in% c("listing", "lst", "l", "\u4e00\u89a7")] <- "listing"
  out
}

# Do two keys name one report?  The same key; or the same numbers when
# one of them does not say its kind.
.ai_id_same <- function(a, b) {
  if (is.na(a) || is.na(b)) return(FALSE)
  if (identical(a, b)) return(TRUE)
  na <- sub("^[TFL]", "", a)
  nb <- sub("^[TFL]", "", b)
  identical(na, nb) && (!grepl("^[TFL]", a) || !grepl("^[TFL]", b))
}

.ai_id_match <- function(key, keys) {
  which(vapply(keys, function(k) .ai_id_same(key, k), NA))
}

# ---- the TOC ----------------------------------------------------------------

.ai_shell_toc_sheet <- function(info, cells, toc_sheet, fn) {
  if (!is.null(toc_sheet)) {
    if (length(toc_sheet) == 1L && is.na(toc_sheet)) return(character())
    if (!toc_sheet %in% info$sheet) {
      .ard_stop(sprintf("%s: no sheet %s; the sheets are %s.", fn, sQuote(toc_sheet, FALSE),
                        paste(sQuote(info$sheet, FALSE), collapse = ", ")))
    }
    return(toc_sheet)
  }
  # the rows of each sheet naming a report number
  n <- vapply(info$sheet, function(s) {
    d <- cells[cells$sheet == s, , drop = FALSE]
    hit <- vapply(.ai_ids_in(d$text), nrow, 1L) > 0L
    length(unique(d$row[hit]))
  }, 1L)
  # a sheet named like a TOC (not one named by a report number: "L-16-2-1
  # \u4e00\u89a7" is a shell) that lists report numbers; the one listing most
  named <- grepl(paste0(
    "(?i)^(toc|contents?|index|list|outputs?|tfl|tlf)\\b|table of contents|",
    "\u76ee\u6b21|\u4e00\u89a7|\u5e33\u7968|\u30ea\u30b9\u30c8"), info$sheet, perl = TRUE) &
    vapply(.ai_ids_in(info$sheet), nrow, 1L) == 0L & n > 0L
  if (any(named)) return(info$sheet[named][which.max(n[named])])
  # else: the sheet with the most rows naming a report number (three at least)
  if (!length(n) || max(n) < 3L) return(character())
  info$sheet[which.max(n)]
}

# The TOC's reports: one row a row of the TOC sheet naming a report number
# (its first); else one a sheet named by a number.
.ai_shell_toc <- function(cells, toc_name) {
  empty <- data.frame(output_id = character(), key = character(), type = character(),
                      title = character(), text = character(), sheet = character(),
                      row = integer(), stringsAsFactors = FALSE)
  if (!length(toc_name)) {
    sh <- unique(cells$sheet)
    ids <- .ai_ids_in(sh)
    rows <- lapply(seq_along(sh), function(i) {
      if (!nrow(ids[[i]])) return(NULL)
      d <- cells[cells$sheet == sh[i], , drop = FALSE]
      txt <- d$text[d$row %in% utils::head(unique(d$row), 3L)]
      data.frame(output_id = ids[[i]]$id[1L], key = ids[[i]]$key[1L], type = ids[[i]]$type[1L],
                 title = .ai_title_of(txt, ids[[i]]$id[1L]), text = paste(txt, collapse = " | "),
                 sheet = sh[i], row = NA_integer_, stringsAsFactors = FALSE)
    })
    return(do.call(rbind, c(list(empty), rows)))
  }
  d <- cells[cells$sheet == toc_name, , drop = FALSE]
  # a cell that is a number (with a word of kind at most), not a text that
  # holds one ("draft 0.2", "14.1 Demographic Data"); the TOC's numbers are
  # in the column holding most of them
  ids <- .ai_ids_in(d$text)
  is_id <- vapply(seq_along(ids), function(i) {
    nrow(ids[[i]]) > 0L && nchar(trimws(d$text[i])) <= nchar(ids[[i]]$id[1L]) + 10L
  }, NA)
  if (!any(is_id)) return(empty)
  id_col <- as.integer(names(which.max(table(d$col[is_id]))))
  title_col <- .ai_toc_title_col(d, min(d$row[is_id & d$col == id_col]))
  out <- list()
  for (r in unique(d$row[is_id & d$col == id_col])) {
    x <- d[d$row == r, , drop = FALSE]
    k <- which(x$col == id_col)[1L]
    id <- .ai_ids_in(x$text[k])[[1L]][1L, ]
    shown <- trimws(.ai_half_width(x$text[k]))
    others <- x$text[-k]
    tp <- id$type
    if (is.na(tp)) tp <- .ai_type_word(paste(x$text, collapse = " "))
    ttl <- if (!is.na(title_col) && any(x$col == title_col)) {
      trimws(gsub("\\s+", " ", x$text[x$col == title_col][1L]))
    } else {
      .ai_title_of(others, id$id)
    }
    out[[length(out) + 1L]] <- data.frame(
      output_id = shown, key = id$key, type = tp,
      title = ttl, text = paste(trimws(x$text), collapse = " | "),
      sheet = toc_name, row = r, stringsAsFactors = FALSE)
  }
  toc <- do.call(rbind, c(list(empty), out))
  toc[!duplicated(toc$key), , drop = FALSE]
}

# The TOC's title column: the cell of a header row above the first report
# (the last such row) that says title -- Title, Output Title, \u30bf\u30a4\u30c8\u30eb,
# \u8868\u984c, \u5e33\u7968\u540d ...; NA when no header says so.
.ai_toc_title_col <- function(d, first_row) {
  h <- d[d$row < first_row, , drop = FALSE]
  re <- paste0("(?i)^\\s*((output|table|report)\\s+)?(title|name)s?\\s*$|",
               "^\\s*(\u30bf\u30a4\u30c8\u30eb|\u8868\u984c|\u6a19\u984c|",
               "\u5e33\u7968\u540d|\u5e33\u7968\u30bf\u30a4\u30c8\u30eb|",
               "\u540d\u79f0)\\s*$")
  hit <- h[grepl(re, h$text, perl = TRUE), , drop = FALSE]
  if (!nrow(hit)) return(NA_integer_)
  hit$col[hit$row == max(hit$row)][1L]
}

# The title of a TOC row: its longest text that is not a number, a kind or a
# short code.
.ai_title_of <- function(txt, id) {
  txt <- trimws(gsub("\\s+", " ", gsub("\n", " ", txt, fixed = TRUE)))
  txt <- txt[nzchar(txt) & txt != id]
  txt <- txt[!grepl("^(table|figure|listing|t|f|l)$", txt, ignore.case = TRUE)]
  txt <- txt[nchar(txt) > 3L]
  if (!length(txt)) return(NA_character_)
  txt[which.max(nchar(txt))]
}

.ai_type_word <- function(x) {
  if (grepl("(?i)\\btable\\b|\u8868", x, perl = TRUE)) return("table")
  if (grepl("(?i)\\bfigure\\b|\\bplot\\b|\u56f3", x, perl = TRUE)) return("figure")
  if (grepl("(?i)\\blisting\\b|\u4e00\u89a7", x, perl = TRUE)) return("listing")
  NA_character_
}

# Each report's shell sheet: the sheet named by its number; else a sheet
# (not the TOC) one of whose cells names it.
.ai_shell_find <- function(toc, info, cells, toc_name) {
  others <- setdiff(info$sheet, toc_name)
  skeys <- vapply(.ai_ids_in(others), function(d) if (nrow(d)) d$key[1L] else NA_character_, "")
  # the numbers named in each sheet's cells
  named <- lapply(others, function(s) {
    d <- cells[cells$sheet == s, , drop = FALSE]
    unique(unlist(lapply(.ai_ids_in(d$text), `[[`, "key")))
  })
  vapply(toc$key, function(k) {
    i <- .ai_id_match(k, skeys)
    if (length(i)) return(others[i[1L]])
    j <- which(vapply(named, function(n) any(vapply(n, .ai_id_same, NA, b = k)), NA))
    if (length(j)) others[j[1L]] else NA_character_
  }, "", USE.NAMES = FALSE)
}

# ---- what is kept ------------------------------------------------------------

.ai_shell_keep <- function(cells, info, toc, toc_name, outputs, sheets, fn) {
  if (!is.null(sheets)) {
    if (is.numeric(sheets)) sheets <- info$sheet[sheets[sheets <= nrow(info)]]
    bad <- setdiff(sheets, info$sheet)
    if (length(bad)) {
      .ard_stop(sprintf("%s: no sheet %s; the sheets are %s.", fn,
                        paste(sQuote(bad, FALSE), collapse = ", "),
                        paste(sQuote(info$sheet, FALSE), collapse = ", ")))
    }
  }
  if (is.null(outputs)) {
    keep <- if (is.null(sheets)) cells else cells[cells$sheet %in% sheets, , drop = FALSE]
    return(list(cells = keep, outputs = character()))
  }
  keys <- .ai_id_key(outputs)
  if (anyNA(keys)) {
    .ard_stop(sprintf("%s: %s is not a report number (T-14-1-1, Table 14.1.1, 14.1.1).",
                      fn, paste(sQuote(outputs[is.na(keys)], FALSE), collapse = ", ")))
  }
  at <- lapply(keys, function(k) .ai_id_match(k, toc$key))
  miss <- outputs[!lengths(at)]
  if (length(miss)) {
    .ard_stop(sprintf("%s: the TOC has no report %s; it has %s.", fn,
                      paste(sQuote(miss, FALSE), collapse = ", "),
                      if (nrow(toc)) paste(utils::head(toc$output_id, 30L), collapse = ", ")
                      else "none (give `toc_sheet`)"))
  }
  rows <- toc[unique(unlist(lapply(at, `[`, 1L))), , drop = FALSE]
  parts <- list()
  if (length(toc_name)) {
    d <- cells[cells$sheet == toc_name, , drop = FALSE]
    first <- if (nrow(toc)) min(toc$row, na.rm = TRUE) else Inf
    parts[[1L]] <- d[d$row < first | d$row %in% rows$row, , drop = FALSE]
  }
  for (i in seq_len(nrow(rows))) {
    s <- rows$shell[i]
    if (is.na(s)) next
    parts[[length(parts) + 1L]] <- .ai_shell_block(cells[cells$sheet == s, , drop = FALSE],
                                                   rows$key[i], toc$key)
  }
  if (!is.null(sheets)) parts[[length(parts) + 1L]] <- cells[cells$sheet %in% sheets, , drop = FALSE]
  keep <- do.call(rbind, c(list(.xl_no_cells()), parts))
  keep <- keep[!duplicated(keep[c("sheet", "ref")]), , drop = FALSE]
  ord <- match(keep$sheet, info$sheet)
  keep <- keep[order(ord, keep$row, keep$col), , drop = FALSE]
  list(cells = keep, outputs = rows$output_id)
}

# A report's rows of a sheet: the whole sheet when no other report is named
# in it; else from the row naming it to the row before the next number.
.ai_shell_block <- function(d, key, all_keys) {
  ids <- .ai_ids_in(d$text)
  row_keys <- lapply(split(ids, d$row), function(z) unique(unlist(lapply(z, `[[`, "key"))))
  rows <- as.integer(names(row_keys))
  is_me <- vapply(row_keys, function(k) any(vapply(k, .ai_id_same, NA, b = key)), NA)
  is_other <- vapply(row_keys, function(k) {
    any(vapply(k, function(z) !.ai_id_same(z, key) && length(.ai_id_match(z, all_keys)) > 0L, NA))
  }, NA)
  if (!any(is_other & !is_me)) return(d)
  start <- rows[is_me][1L]
  if (is.na(start)) return(d[0L, , drop = FALSE])
  nxt <- rows[is_other & !is_me & rows > start]
  end <- if (length(nxt)) min(nxt) - 1L else max(d$row)
  d[d$row >= start & d$row <= end, , drop = FALSE]
}

# ---- the text -----------------------------------------------------------------

.ai_shell_text <- function(file, info, cells, outputs) {
  sh <- info$sheet[info$sheet %in% cells$sheet]
  head <- c(
    sprintf("# Workbook %s: %d sheet(s)%s", file, length(sh),
            if (length(outputs)) sprintf(", the parts of %s", paste(outputs, collapse = ", "))
            else ""),
    "",
    paste("Each line is one row of a sheet: `<row>: <column>=<text> | ...`, the",
          "non-empty cells left to right. A merged cell is written once with its",
          "range (`A1:E1=`). Leading spaces are the cell's indent; `\\n` is a line",
          "break inside a cell."),
    "")
  body <- unlist(lapply(sh, function(s) {
    d <- cells[cells$sheet == s, , drop = FALSE]
    i <- info[info$sheet == s, , drop = FALSE]
    rows <- unique(d$row)
    lines <- vapply(rows, function(r) {
      x <- d[d$row == r, , drop = FALSE]
      ref <- ifelse(grepl(":", x$ref, fixed = TRUE), x$ref, sub("[0-9]+$", "", x$ref))
      paste0(r, ": ", paste0(ref, "=", .ai_shell_cell(x$text), collapse = " | "))
    }, "")
    c(sprintf("## Sheet \"%s\"%s%s", s,
              if (identical(i$role, "toc")) " (the TOC)" else "",
              if (isTRUE(i$hidden)) " (hidden)" else ""),
      lines, "")
  }), use.names = FALSE)
  paste(c(head, body), collapse = "\n")
}

.ai_shell_cell <- function(x) {
  x <- gsub("\r\n?", "\n", x)
  x <- sub("\\s+$", "", x)
  x <- gsub("\n", "\\n", x, fixed = TRUE)
  long <- nchar(x) > .ai_shell_max_cell
  x[long] <- paste0(substr(x[long], 1L, .ai_shell_max_cell), "...")
  x
}

# Text (one string, or lines) to a file as UTF-8 bytes whatever the
# session's locale (a Japanese Windows R in CP932 too), "\n" line ends.
.ai_write_text <- function(text, file) {
  text <- paste(enc2utf8(as.character(text)), collapse = "\n")
  # its folder made if need be (file = "ai/toc_shell.md" in a new study)
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  con <- file(file, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(enc2utf8(paste0(text, "\n"))), con)
  invisible(file)
}

# ---- batches -------------------------------------------------------------------

# The families a table's title says, in the order they are tried.
.ai_families <- c(
  disposition = paste0("(?i)dispos|discontinu|withdr[ae]w|enrol|screen|analysis sets?\\b|",
                       "populations?\\b|protocol deviation|\u4e2d\u6b62|\u5185\u8a33|",
                       "\u89e3\u6790\u5bfe\u8c61"),
  demographics = paste0("(?i)demograph|baseline characteristic|characteristics|medical history|",
                        "prior|concomitant|\u80cc\u666f|\u4f75\u7528"),
  exposure = "(?i)exposure|dose|dosing|treatment duration|compliance|\u66dd\u9732|\u6295\u4e0e",
  ae = paste0("(?i)adverse|teae|\\bt?ae\\b|\\bsaes?\\b|death|\u6709\u5bb3\u4e8b\u8c61|",
              "\u526f\u4f5c\u7528|\u6b7b\u4ea1"),
  labs = paste0("(?i)laborator|\\blab\\b|chemistry|ha?ematology|urinalysis|shift|",
                "\u81e8\u5e8a\u691c\u67fb"),
  vitals = paste0("(?i)vital|ecg|electrocardio|blood pressure|pulse|weight|temperature|",
                  "\u30d0\u30a4\u30bf\u30eb|\u5fc3\u96fb\u56f3"),
  efficacy = paste0("(?i)response|\\borr\\b|\\bdcr\\b|survival|progression|\\bpfs\\b|\\bos\\b|",
                    "efficacy|tumou?r|duration of|endpoint|time to|\u6709\u52b9\u6027|",
                    "\u594f\u52b9|\u751f\u5b58|\u5fdc\u7b54"),
  pk = "(?i)pharmacokinetic|\\bpk\\b|concentration|\u85ac\u7269\u52d5\u614b"
)

#' The TOC's tables in batches, one family each
#'
#' A chat drafts a few reports well and many badly: the `table` prompt
#' ([tfl_ai_prompt()]) is made for 5 to 10 reports a request.  This splits
#' the TOC's tables into batches of one family each -- disposition,
#' demographics, exposure, adverse events, laboratory, vital signs,
#' efficacy, PK, other, by the words of the title (English or Japanese) --
#' so that one request holds tables that look alike, and a reviewed table of
#' a family can be the example for the rest of it ([tfl_ai_context()]'s
#' `examples`).
#'
#' @param x A [tfl_ai_read_shell()] result (its `toc`), or a data frame with
#'   `output_id` and `title` (and optionally `type`, `family`).
#' @param size The reports a batch holds at most.
#' @param types The kinds of report kept (`type`; a report whose kind is not
#'   known is kept); `NULL` for all.
#' @param families Your own families: a named character vector of regular
#'   expressions (Perl) tried on the title before the built-in ones.
#' @return A data frame: `output_id`, `title`, `family`, `batch` (`ae-1`,
#'   `ae-2` ...), in the TOC's order within a batch; `split(x$output_id,
#'   x$batch)` gives the requests.
#' @examples
#' f <- system.file("ai", "example", "onco_toc_shell.xlsx", package = "tflspec")
#' b <- tfl_ai_batches(tfl_ai_read_shell(f), size = 2)
#' b
#' split(b$output_id, b$batch)
#' @export
tfl_ai_batches <- function(x, size = 8L, types = "table", families = NULL) {
  fn <- "tfl_ai_batches()"
  toc <- if (inherits(x, "tfl_ai_shell")) x$toc else x
  if (!is.data.frame(toc) || !all(c("output_id", "title") %in% names(toc))) {
    .ard_stop(sprintf(paste0("%s: `x` is a tfl_ai_read_shell() result, or a data frame with ",
                             "`output_id` and `title`."), fn))
  }
  if (!is.numeric(size) || length(size) != 1L || is.na(size) || size < 1) {
    .ard_stop(sprintf("%s: `size` is one number, 1 or more.", fn))
  }
  if (!is.null(families) && (!is.character(families) || is.null(names(families)))) {
    .ard_stop(sprintf("%s: `families` is a named character vector of regular expressions.", fn))
  }
  if (!nrow(toc)) {
    return(data.frame(output_id = character(), title = character(), family = character(),
                      batch = character(), stringsAsFactors = FALSE))
  }
  if (!"type" %in% names(toc)) toc$type <- NA_character_
  if (!is.null(types)) toc <- toc[is.na(toc$type) | toc$type %in% types, , drop = FALSE]
  fam <- c(families, .ai_families)
  text <- if ("text" %in% names(toc)) ifelse(is.na(toc$title), toc$text, toc$title) else toc$title
  text[is.na(text)] <- ""
  family <- if ("family" %in% names(toc)) as.character(toc$family) else rep(NA_character_, nrow(toc))
  for (i in which(is.na(family))) {
    hit <- names(fam)[vapply(fam, function(re) grepl(re, text[i], perl = TRUE), NA)]
    family[i] <- if (length(hit)) hit[1L] else "other"
  }
  out <- data.frame(output_id = toc$output_id, title = toc$title, family = family,
                    batch = NA_character_, stringsAsFactors = FALSE)
  for (f in unique(family)) {
    i <- which(family == f)
    out$batch[i] <- paste0(f, "-", (seq_along(i) - 1L) %/% as.integer(size) + 1L)
  }
  ord <- order(match(out$family, unique(c(names(fam), "other"))), seq_len(nrow(out)))
  out <- out[ord, , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Where each report's shell is in the workbook -- its sheet and cells (a
# sheet holding several reports: the report's rows, as .ai_shell_block()
# cuts them) and its TOC row -- what a request says when the workbook is
# attached (tfl_ai_write_requests(workbook = )).  One line a report.
.ai_shell_where <- function(x, ids) {
  toc <- x$toc
  vapply(ids, function(id) {
    i <- .ai_id_match(.ai_id_key(id), toc$key)
    if (!length(i)) return(sprintf("- %s: not in the TOC; look for it in the workbook.", id))
    r <- toc[i[1L], , drop = FALSE]
    several <- !is.null(x$sheets$book) && length(unique(x$sheets$book)) > 1L
    at_toc <- if (!is.na(r$sheet) && !is.na(r$row)) {
      sprintf(" (its TOC row: %ssheet '%s', row %d)",
              if (several) sprintf("workbook `%s`, ", x$sheets$book[1L]) else "",
              r$sheet, as.integer(r$row))
    } else ""
    if (is.na(r$shell)) {
      return(sprintf("- %s: no shell was found by its number; look for it in the workbook%s.",
                     id, at_toc))
    }
    d <- x$cells[x$cells$sheet == r$shell, , drop = FALSE]
    b <- .ai_shell_block(d, r$key, toc$key)
    where <- if (!nrow(b) || nrow(b) == nrow(d)) "the whole sheet" else
      sprintf("cells %s%d:%s%d", .xl_col_letter(min(b$col)), as.integer(min(b$row)),
              .xl_col_letter(max(b$col)), as.integer(max(b$row)))
    sh <- x$sheets
    if (several) {
      # several workbooks: which, and the sheet's own name in it
      k <- match(r$shell, sh$sheet)
      return(sprintf("- %s: workbook `%s`, sheet '%s', %s%s.", id, sh$book[k], sh$name[k],
                     where, at_toc))
    }
    sprintf("- %s: sheet '%s', %s%s.", id, r$shell, where, at_toc)
  }, "", USE.NAMES = FALSE)
}
