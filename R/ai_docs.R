# ============================================================================
#  A document as text the agent can read: a Word file (.docx), a PDF, a
#  workbook (.xlsx) as Markdown -- the headings kept, the tables as Markdown
#  tables, a PDF's pages marked.  tfl_ai_prepare(docs = ) writes it beside
#  the document in ai/docs/, and AGENTS.md names the text.
# ============================================================================

#' A document as Markdown text for the agent
#'
#' An agent in the editor reads text; a SAP in Word or PDF, or a workbook,
#' reaches it as nothing.  This turns one into Markdown: a `.docx`'s
#' paragraphs with its heading styles as `#` headings, its list paragraphs
#' as `-` items and its tables as Markdown tables (needs the officer
#' package); a `.pdf`'s text page by page, each page marked `<!-- page n
#' -->` (needs pdftools); a `.xlsx` / `.xls` sheet by sheet, each sheet a
#' heading and a Markdown table of its cells (readxl).  A `.md` or `.txt`
#' is its own text.  [tfl_ai_prepare()] calls it for `docs = ` and for the
#' TOC / SHELL workbook the agent may open, and names the text files in
#' `AGENTS.md`.
#'
#' @param path The document.
#' @param file A file to write the text to (UTF-8), or `NULL`.
#' @return The text, one string (Markdown), invisibly when written to
#'   `file`.
#' @examples
#' if (requireNamespace("openxlsx", quietly = TRUE)) {
#'   xl <- tempfile(fileext = ".xlsx")
#'   openxlsx::write.xlsx(list(Sets = data.frame(Set = c("SAF", "FAS"),
#'                                               Definition = c("treated", "randomized"))), xl)
#'   cat(tfl_ai_doc_text(xl))
#' }
#' @export
tfl_ai_doc_text <- function(path, file = NULL) {
  fn <- "tfl_ai_doc_text()"
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) {
    .ard_stop(sprintf("%s: no file '%s'.", fn, path))
  }
  ext <- tolower(tools::file_ext(path))
  text <- switch(ext,
    docx = .ai_doc_docx(path, fn),
    pdf = .ai_doc_pdf(path, fn),
    xlsx = , xlsm = , xls = .ai_doc_xlsx(path, fn),
    md = , markdown = , txt = paste(readLines(path, warn = FALSE, encoding = "UTF-8"),
                                    collapse = "\n"),
    .ard_stop(sprintf(paste0("%s: '%s' is not a document this reads (.docx, .pdf, .xlsx, ",
                             ".md, .txt)."), fn, basename(path))))
  if (!is.null(file)) {
    .ai_write_text(text, file)
    return(invisible(text))
  }
  text
}

# the documents tfl_ai_doc_text() turns into text (a .md / .txt is text already)
.ai_doc_exts <- c("docx", "pdf", "xlsx", "xlsm", "xls")

.ai_doc_need <- function(pkg, ext, fn) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    .ard_stop(sprintf("%s: reading .%s files needs the %s package: install.packages(\"%s\").",
                      fn, ext, pkg, pkg))
  }
}

# a cell's text in a Markdown table: one line, | escaped
.ai_doc_cell <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("\\s*\n\\s*", " ", x)
  gsub("|", "\\|", trimws(x), fixed = TRUE)
}

# rows (a list of character vectors, the first the header) as a Markdown table
.ai_doc_table <- function(rows) {
  if (!length(rows)) return(character())
  n <- max(lengths(rows))
  rows <- lapply(rows, function(r) c(.ai_doc_cell(r), rep("", n - length(r))))
  line <- function(r) paste0("| ", paste(r, collapse = " | "), " |")
  c(line(rows[[1L]]), paste0("|", paste(rep(" --- |", n), collapse = "")),
    vapply(rows[-1L], line, ""), "")
}

# A Word file: its paragraphs and tables in order (officer::docx_summary()),
# a heading style as a Markdown heading, a list paragraph as an item
.ai_doc_docx <- function(path, fn) {
  .ai_doc_need("officer", "docx", fn)
  s <- officer::docx_summary(officer::read_docx(path))
  s <- s[order(s$doc_index), , drop = FALSE]
  out <- character()
  i <- 1L
  n <- nrow(s)
  while (i <= n) {
    if (identical(s$content_type[i], "table cell")) {
      # a table: its cells until the next paragraph (a new table starts
      # when the row numbering starts again)
      j <- i
      while (j + 1L <= n && identical(s$content_type[j + 1L], "table cell") &&
             !(s$row_id[j + 1L] == 1L && s$cell_id[j + 1L] == 1L && s$row_id[j] >= 1L &&
               j > i)) {
        j <- j + 1L
      }
      cells <- s[i:j, , drop = FALSE]
      rows <- lapply(split(cells, cells$row_id), function(r) r$text[order(r$cell_id)])
      out <- c(out, .ai_doc_table(unname(rows)))
      i <- j + 1L
      next
    }
    txt <- trimws(s$text[i] %||% "")
    style <- tolower(s$style_name[i] %||% "")
    if (is.na(style)) style <- ""
    lvl <- regmatches(style, regexpr("[0-9]+$", style))
    if (grepl("^(heading|title|\u898b\u51fa\u3057)", style)) {
      k <- if (grepl("^title", style)) 1L else if (length(lvl)) as.integer(lvl) else 1L
      if (nzchar(txt)) out <- c(out, "", paste(strrep("#", min(k, 6L)), txt), "")
    } else if (nzchar(txt)) {
      is_list <- grepl("list", style, fixed = TRUE) || !is.na(s$num_id[i] %||% NA)
      out <- c(out, if (is_list) paste("-", txt) else c(txt, ""))
    }
    i <- i + 1L
  }
  .ai_doc_tidy(out)
}

# A PDF: its text page by page (pdftools::pdf_text()), each page marked
.ai_doc_pdf <- function(path, fn) {
  .ai_doc_need("pdftools", "pdf", fn)
  pages <- pdftools::pdf_text(path)
  out <- unlist(lapply(seq_along(pages), function(k) {
    lines <- strsplit(pages[k], "\n", fixed = TRUE)[[1L]]
    lines <- sub("\\s+$", "", lines)
    c(sprintf("<!-- page %d -->", k), "", lines, "")
  }), use.names = FALSE)
  .ai_doc_tidy(out)
}

# A workbook: each sheet a heading and a table of its cells, as text
.ai_doc_xlsx <- function(path, fn) {
  .ai_doc_need("readxl", tools::file_ext(path), fn)
  out <- character()
  for (sh in readxl::excel_sheets(path)) {
    d <- tryCatch(suppressMessages(readxl::read_excel(path, sh, col_names = FALSE,
                                                      col_types = "text",
                                                      .name_repair = "minimal")),
                  error = function(e) NULL)
    out <- c(out, "", paste("##", sh), "")
    if (is.null(d) || !nrow(d)) {
      out <- c(out, "(empty)", "")
      next
    }
    d <- as.data.frame(d, stringsAsFactors = FALSE)
    keep <- rowSums(!is.na(d) & nzchar(trimws(as.matrix(d)))) > 0L
    d <- d[keep, , drop = FALSE]
    rows <- lapply(seq_len(nrow(d)), function(i) unlist(d[i, ], use.names = FALSE))
    out <- c(out, .ai_doc_table(rows))
  }
  .ai_doc_tidy(out)
}

# one blank line at most between blocks, none at the ends
.ai_doc_tidy <- function(lines) {
  lines <- sub("\\s+$", "", lines)
  blank <- !nzchar(lines)
  lines <- lines[!(blank & c(TRUE, blank[-length(blank)]))]
  while (length(lines) && !nzchar(lines[length(lines)])) lines <- lines[-length(lines)]
  paste(lines, collapse = "\n")
}

# file.copy() that leaves a file alone when it is copied onto itself: on
# Windows, copying a file onto its own path truncates it to nothing (a
# document the person had put into ai/docs/ already and then named).  `to`
# is a file path each, or one folder.
.file_copy_safe <- function(from, to, overwrite = TRUE) {
  if (!length(from)) return(invisible(logical()))
  dest <- if (length(to) == 1L && dir.exists(to)) file.path(to, basename(from)) else to
  norm <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
  same <- file.exists(dest) & tolower(norm(from)) == tolower(norm(dest))
  ok <- same
  if (any(!same)) ok[!same] <- file.copy(from[!same], dest[!same], overwrite = overwrite)
  invisible(ok)
}

# The documents the person gave.  A study's documents live in the study
# folder (its docs/): one inside it is read where it is, never copied -- a
# .md / .txt at its own path, a .docx / .pdf / workbook as the text written
# to ai/docs/<name>.md.  One outside the study is copied into ai/docs/, and
# the text written beside it for those kinds.  The paths the agent reads
# (from the study folder): the text files, the documents themselves for the
# rest; `from`: the document a text file stands for.
.ai_docs_place <- function(dir, docs) {
  if (!length(docs)) return(list(refs = character(), files = character(), from = character()))
  rd_ <- file.path(dir, "ai", "docs")
  dir.create(rd_, recursive = TRUE, showWarnings = FALSE)
  norm <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
  root <- paste0(norm(dir), "/")
  inside <- startsWith(tolower(norm(docs)), tolower(root))
  refs <- character(length(docs))
  from <- rep(NA_character_, length(docs))
  files <- character()
  for (i in seq_along(docs)) {
    text_kind <- tolower(tools::file_ext(docs[i])) %in% .ai_doc_exts
    if (inside[i]) {
      own <- substring(norm(docs[i]), nchar(root) + 1L)
      if (!text_kind) {
        refs[i] <- own
        next
      }
      md <- file.path(rd_, paste0(tools::file_path_sans_ext(basename(docs[i])), ".md"))
      tfl_ai_doc_text(docs[i], file = md)
      from[i] <- own
      refs[i] <- file.path("ai", "docs", basename(md))
      files <- c(files, md)
      next
    }
    to <- file.path(rd_, basename(docs[i]))
    .file_copy_safe(docs[i], to)
    refs[i] <- file.path("ai", "docs", basename(docs[i]))
    files <- c(files, to)
    if (!text_kind) next
    md <- file.path(rd_, paste0(tools::file_path_sans_ext(basename(docs[i])), ".md"))
    tfl_ai_doc_text(docs[i], file = md)
    from[i] <- refs[i]
    refs[i] <- file.path("ai", "docs", basename(md))
    files <- c(files, md)
  }
  list(refs = gsub("\\\\", "/", refs), files = files, from = gsub("\\\\", "/", from))
}
