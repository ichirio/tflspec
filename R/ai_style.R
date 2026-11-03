# ============================================================================
#  A company's past TFLs, surveyed for their look (10/26, the user)
# ----------------------------------------------------------------------------
#  Before a company standard is written, see what the company's reports
#  already do: where the tables draw their lines (above the table, under
#  the column header, under each spanner of a header of several rows, at
#  the end), how wide the stub is to the value columns, the fonts and
#  sizes, how titles and footnotes are aligned.  R reads the numbers out of
#  the RTF files (tfl_rtf_style_scan(): each table row's cell widths,
#  borders, size and alignment -- never a cell's text; tfl_rtf_style_tables():
#  a table's); an AI agent, given the prompt and those tables, opens a few
#  reports of each kind and writes their conventions to a YAML file;
#  tfl_ai_style_read() shows it, tfl_ai_style_diff() sets it against the
#  company standard.  What is adopted is the person's to decide.
# ============================================================================

.ai_style_default_out <- function() {
  file.path(tools::R_user_dir("tflspec", "data"), "ai", "style", "learned.yml")
}

#' The look of a folder of RTF reports, as numbers
#'
#' Reads every `.rtf` file of a folder (and its subfolders) and returns, one
#' row a table row, what its look is made of -- never its text: the cells'
#' widths (`\cellx`), each cell's top and bottom border (`\clbrdrt`,
#' `\clbrdrb`), the font size (`\fs`), the cells' alignment (`\ql`, `\qc`,
#' `\qr`), and the file's fonts (its font table).  Rows in a run, with
#' nothing but row definitions between them, are one table.  A table that
#' goes on over pages, as SAS's ODS RTF writes it (a page a section, the
#' column header rows, `\trhdr`, again at the top of each), is one table:
#' its later pages' header rows are dropped, so a header counts once.
#'
#' @param rtf_dir The folder of RTF files.
#' @return A data frame: `file` (its path under `rtf_dir`), `table` (its
#'   place in the file), `row` (in the table), `page` (the file's page it
#'   is on), `header` (a column header row, `\trhdr`), `n_cells`,
#'   `width_twips` (the row's width), `rel_widths` (each cell's width as parts of the narrowest, ` | `
#'   between them), `border_top`, `border_bottom` (one character a cell:
#'   `1` a border, `0` none), `font_size_pt`, `align` (one character a
#'   cell: `l`, `c`, `r`), `fonts` (the file's font table, ` | ` between).
#' @seealso [tfl_ai_style_prompt()]
#' @export
tfl_rtf_style_scan <- function(rtf_dir) {
  fs <- list.files(rtf_dir, pattern = "[.]rtf$", full.names = TRUE, recursive = TRUE,
                   ignore.case = TRUE)
  empty <- data.frame(file = character(), table = integer(), row = integer(),
                      page = integer(), header = logical(),
                      n_cells = integer(), width_twips = integer(), rel_widths = character(),
                      border_top = character(), border_bottom = character(),
                      font_size_pt = numeric(), align = character(), fonts = character(),
                      stringsAsFactors = FALSE)
  rows <- lapply(fs, function(f) {
    x <- paste(readLines(f, warn = FALSE, encoding = "latin1"), collapse = "\n")
    # (its path under the folder: a study's subfolder and the file)
    .rtf_style_rows(x, substring(normalizePath(f, winslash = "/"),
                                 nchar(normalizePath(rtf_dir, winslash = "/")) + 2L))
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
  # the page breaks (SAS ODS RTF: a \sect a page; Word: \page)
  pb <- gregexpr("\\\\(sect|page)(?![a-z])", x, perl = TRUE)[[1L]]
  pb <- pb[pb > 0L]
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
      file = name, table = tbl, row = k, page = 1L + sum(pb < s),
      header = grepl("\\\\trhdr(?![a-z])", defs[1L], perl = TRUE),
      n_cells = length(cx), width_twips = max(cx),
      rel_widths = rel, border_top = bt, border_bottom = bb,
      font_size_pt = if (length(fsz)) as.numeric(substring(fsz, 4L)) / 2 else doc_pt,
      align = al, fonts = paste(fonts, collapse = " | "), .cx = paste(cx, collapse = ","),
      stringsAsFactors = FALSE)
    prev_end <- e + 4L
  }
  if (!length(out)) return(NULL)
  .rtf_join_pages(do.call(rbind, out))
}

# A table that goes on over pages is one table: SAS's ODS RTF writes each
# page's rows as a table of their own, its column header rows (\trhdr)
# again at the top of each.  A table that starts on a later page than the
# one before it, with the same header rows (the same cells' right edges),
# is that table going on: its rows join it, its repeated header rows go.
.rtf_join_pages <- function(d) {
  sig <- function(t) {
    r <- d[d$table == t, , drop = FALSE]
    h <- cumprod(r$header) == 1L
    paste(r$.cx[h], collapse = "/")
  }
  d$.drop <- FALSE
  ids <- unique(d$table)
  into <- stats::setNames(ids, ids)
  for (i in seq_along(ids)[-1L]) {
    t <- ids[i]
    p <- into[[as.character(ids[i - 1L])]]
    s <- sig(t)
    if (nzchar(s) && identical(s, sig(p)) &&
        min(d$page[d$table == t]) > max(d$page[d$table %in% ids[into == p]])) {
      into[[as.character(t)]] <- p
      r <- which(d$table == t)
      d$.drop[r] <- cumprod(d$header[r]) == 1L
    }
  }
  d$table <- unname(into[as.character(d$table)])
  d <- d[!d$.drop, , drop = FALSE]
  d$table <- match(d$table, unique(d$table))
  d$row <- stats::ave(seq_len(nrow(d)), d$table, FUN = seq_along)
  d$.drop <- NULL
  d$.cx <- NULL
  rownames(d) <- NULL
  d
}

#' A prompt for an AI agent to learn a company's conventions from its past TFLs
#'
#' Writes what an AI agent needs to learn how a company's past reports are
#' made, by the kind of report, so that the company standard can take it in
#' (tflspec#547): R's numbers of the reports' look (`scan.csv`,
#' [tfl_rtf_style_scan()]; `tables.csv`, one row a table,
#' [tfl_rtf_style_tables()]) and the prompt (`prompt.md`).  The agent
#' opens a few files of each kind for what R cannot read -- the statistics
#' and their labels, the n (%) and header N forms, the decimals, the sort,
#' the footnotes' wording -- and writes the conventions it found, a kind at
#' a time, with how many of the files it read show each, to `out`
#' (`learned.yml`), which [tfl_ai_style_read()] reads and
#' [tfl_ai_style_diff()] sets against the company standard.  Nothing is
#' sent anywhere; the files and what is learned stay in the folder.
#'
#' @param rtf_dir The folder of the past RTF reports (a subfolder a study).
#' @param out The file the agent writes what it learned to (YAML); its
#'   folder gets the prompt and the numbers too.  `NULL`: `learned.yml`
#'   under `ai/style/` of tflspec's data folder ([tools::R_user_dir()]).
#' @param dir Instead of `out`: the folder (`out` is `learned.yml` in it).
#' @return The prompt (a `tfl_ai_text`), with the files' paths in its
#'   attribute `files` (`prompt`, `scan`, `tables`, `result`).
#' @seealso [tfl_ai_style_read()], [tfl_ai_style_diff()]
#' @export
tfl_ai_style_prompt <- function(rtf_dir, out = NULL, dir = NULL) {
  if (!is.character(rtf_dir) || length(rtf_dir) != 1L || !dir.exists(rtf_dir)) {
    .ard_stop("tfl_ai_style_prompt(): `rtf_dir` is a folder of RTF files.")
  }
  out <- out %||% if (!is.null(dir)) file.path(dir, "learned.yml") else .ai_style_default_out()
  where <- dirname(out)
  dir.create(where, recursive = TRUE, showWarnings = FALSE)
  write_csv <- function(d, f) {
    con <- file(f, open = "w", encoding = "UTF-8")
    on.exit(close(con))
    utils::write.csv(d, con, row.names = FALSE)
  }
  scan <- tfl_rtf_style_scan(rtf_dir)
  f_scan <- file.path(where, "scan.csv")
  write_csv(scan, f_scan)
  tabs <- tfl_rtf_style_tables(rtf_dir)
  f_tabs <- file.path(where, "tables.csv")
  write_csv(tabs, f_tabs)
  n_files <- length(unique(tabs$file))
  norm <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
  # the files to open: three a kind at most, the newest first
  mt <- file.mtime(file.path(rtf_dir, tabs$file))
  by_kind <- split(tabs$file[order(mt, decreasing = TRUE)], tabs$kind[order(mt, decreasing = TRUE)])
  kinds <- intersect(.ai_style_kinds, names(by_kind))
  kinds <- c(kinds, setdiff(names(by_kind), kinds))
  open_lines <- vapply(kinds, function(k) {
    f <- unique(by_kind[[k]])
    sprintf("- `%s` (%d table(s)): %s", k, sum(tabs$kind == k),
            paste0("`", utils::head(f, 3L), "`", collapse = ", "))
  }, "")
  keys <- .ai_style_keys
  key_lines <- sprintf("- `%s`: %s%s", keys$key, keys$what,
                       ifelse(keys$from == "numbers", " (from `tables.csv`; open a file only to settle it)", ""))
  text <- c(
    "# Learn a company's conventions from its past TFLs",
    "",
    sprintf("The folder `%s` holds a company's past tables, figures and listings as RTF files, a subfolder a study (%d files, %d tables found).",
            norm(rtf_dir), n_files, nrow(tabs)),
    "Learn how this company makes each kind of report, so that its company standard can take it in and every study's AI drafts follow it.",
    "Write forms, not content: never copy a study's numbers, treatment names or study ids. Where a form holds them, write a placeholder (`xx`, `<treatment>`, `<population>`).",
    "",
    "## What R read for you",
    "",
    sprintf("- `%s`: one row a table -- `kind` (R's guess from the title: %s), `title`, `header_rows`, `n_cols`, `stub_ratio` (the first column's width to a value column's), `rules` (where lines are drawn: `above_header`, `under_spanner`, `under_header`, `body`, `end`), `font`, `font_size_pt`, `stub_align`, `value_align` (`l`, `c`, `r`), `orientation`, `margins_in`, `title_place` / `footnote_place` (`header` / `footer` of the page, `before` / `after` the table), `title_align`, `footnote_align`, `footnotes` (their number).",
            norm(f_tabs), paste0("`", .ai_style_kinds, "`", collapse = ", ")),
    sprintf("- `%s`: the same, one row a table row (cell widths, borders, alignment), when a table needs a closer look.", norm(f_scan)),
    "",
    "A guessed kind can be wrong: when a table's title says otherwise, use the right kind and say so in `notes`.",
    "",
    "## The files to open",
    "",
    "Open these (the newest first) for what R cannot read; open more of a kind only when they disagree:",
    "",
    open_lines,
    "",
    "In RTF a cell's text ends with `\\cell` and a row with `\\row`; titles and footnotes are in the page's `{\\header ...}` / `{\\footer ...}` or in paragraphs (`\\par`) before and after the table.",
    "",
    "## What to learn, by kind",
    "",
    "For each kind, as many of these as the files show:",
    "",
    key_lines,
    "",
    "Write a convention every kind shares (the font, the page, the titles' place) once, under `all`.",
    "",
    "## What to write",
    "",
    sprintf("Write `%s` (UTF-8) in this form. Each convention: `value` (the form, in words or with placeholders), `seen` (how many of the files you read for it show it) and `of` (how many you read). Where the files differ, write the commonest form and name the others in `notes`.",
            norm(out)),
    "",
    "```yaml",
    sprintf("tfl_style_learned: {version: 2, files: %d, tables: %d}", n_files, nrow(tabs)),
    "kinds:",
    "  all:",
    "    font: {value: \"Courier New 9 pt\", seen: 12, of: 12}",
    "    titles: {value: \"page header, centred: Table <number> / <title> / (<population>)\", seen: 12, of: 12}",
    "  demog:",
    "    evidence: [STUDY-B/t_dm.rtf, STUDY-A/t14_1_1.rtf]",
    "    n_form: {value: \"(N=xx)\", seen: 2, of: 2}",
    "    categorical: {value: \"xx (xx.x%)\", seen: 2, of: 2}",
    "    continuous: {value: \"n | Mean (SD) | Median | Min, Max\", seen: 2, of: 2}",
    "    digits: {value: \"mean and median one more than the data, SD two more\", seen: 1, of: 2}",
    "    stub_ratio: {value: \"3\", seen: 2, of: 2}",
    "  ae_soc_pt:",
    "    first_row: {value: \"Any TEAE\", seen: 3, of: 3}",
    "    sort: {value: \"SOC and PT by descending frequency in the total column\", seen: 3, of: 3}",
    "notes: \"anything the person should know: exceptions, kinds guessed wrong, files that could not be read\"",
    "```",
    "",
    "Then tell the person the file is written: they read it with `tflspec::tfl_ai_style_read()` and set it against the company standard with `tflspec::tfl_ai_style_diff()`.")
  f_prompt <- file.path(where, "prompt.md")
  .ai_write_text(text, f_prompt)
  structure(paste(text, collapse = "\n"), class = "tfl_ai_text",
            files = c(prompt = f_prompt, scan = f_scan, tables = f_tabs, result = out))
}

#' Read what an AI agent learned from a company's past TFLs
#'
#' The conventions [tfl_ai_style_prompt()] asks the agent to write
#' (`learned.yml`): by kind of report, each convention's form and how many
#' of the files the agent read show it.  A form fewer than half of them
#' show is kept, but to be adopted by hand only (`adopt`), as is a
#' convention whose name is none of those the prompt asks for.
#'
#' @param file The agent's file (YAML); `NULL`: [tfl_ai_style_prompt()]'s
#'   default.
#' @return A data frame, one row a convention, in the order written:
#'   `kind` (`all` for every kind), `key`, `value`, `seen`, `of`,
#'   `evidence` (the kind's files the agent read, ` | ` between), `adopt`
#'   (`"proposed"`, or `"by hand"`: fewer than half the files, no count,
#'   or a name the prompt does not know).  Its attribute `text` is a short
#'   summary (a line a convention), which `print()` shows; `files` /
#'   `tables` the counts the file gives and `notes` its notes.
#' @seealso [tfl_ai_style_diff()]
#' @export
tfl_ai_style_read <- function(file = NULL) {
  file <- file %||% .ai_style_default_out()
  if (!file.exists(file)) .ard_stop(sprintf("tfl_ai_style_read(): nothing learned at %s yet.", file))
  # (n, N, y as text: a statistic is called n)
  y <- tryCatch(yaml::read_yaml(file, handlers = .yaml_handlers()), error = function(e) {
    .ard_stop(sprintf("tfl_ai_style_read(): %s is not YAML: %s", file, conditionMessage(e)))
  })
  if (!is.list(y) || !is.list(y$kinds)) {
    .ard_stop(sprintf("tfl_ai_style_read(): %s has no `kinds:` (see tfl_ai_style_prompt()'s prompt).", file))
  }
  one <- function(v) if (length(v)) paste(as.character(unlist(v)), collapse = " | ") else NA_character_
  count <- function(v) suppressWarnings(as.integer(one(v)))
  rows <- list()
  for (k in names(y$kinds)) {
    e <- y$kinds[[k]]
    if (!is.list(e)) next
    ev <- one(e$evidence)
    for (key in setdiff(names(e), "evidence")) {
      v <- e[[key]]
      if (!is.list(v) || is.null(names(v))) v <- list(value = v)
      rows[[length(rows) + 1L]] <- data.frame(
        kind = k, key = key, value = one(v$value), seen = count(v$seen), of = count(v$of),
        evidence = ev, stringsAsFactors = FALSE)
    }
  }
  out <- do.call(rbind, c(list(data.frame(kind = character(), key = character(), value = character(),
                                          seen = integer(), of = integer(), evidence = character(),
                                          stringsAsFactors = FALSE)), rows))
  share <- out$seen / out$of
  out$adopt <- ifelse(!is.na(share) & out$of > 0L & share >= 0.5 & out$key %in% .ai_style_keys$key,
                      "proposed", "by hand")
  rownames(out) <- NULL
  hdr <- y$tfl_style_learned %||% list()
  text <- c(sprintf("Learned from %s file(s), %s table(s)", hdr$files %||% "?", hdr$tables %||% "?"),
            if (nrow(out)) sprintf("%s: %s = %s (%s of %s)%s", out$kind, out$key, out$value,
                                   ifelse(is.na(out$seen), "?", out$seen),
                                   ifelse(is.na(out$of), "?", out$of),
                                   ifelse(out$adopt == "by hand", " -- by hand", "")))
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
