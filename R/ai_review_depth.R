# ============================================================================
#  How deep the agent checks what it built (study.yml ai: review)
# ----------------------------------------------------------------------------
#  The agent builds each request itself (run.R) and checks what R made
#  against the shell, as deep as the report's kind says: 0 the check alone
#  (ai/status.md, the repairs), 1 and the report's program (<id>.R), 2 and
#  what it came out as, in words (<id>-facts.txt), 3 and the output (a
#  figure's PNG; a table's pages, from the same facts).  tfl_ai_run()
#  writes the program and the facts beside each report's RTF; the agent
#  reads them, never the RTF.
# ============================================================================

.ai_review_kinds <- c("figure", "table", "listing")

# The levels, one a kind (figure, table, listing): 0 to 3, or NA where the
# agent does not build that kind ("none": the person's check).  One value
# is every kind's; a kind not said keeps the default.
.ai_review_levels <- function(x) {
  d <- .ai_spec_defaults$review
  if (!is.list(x)) x <- stats::setNames(rep(list(x), length(.ai_review_kinds)), .ai_review_kinds)
  bad <- setdiff(names(x), .ai_review_kinds)
  if (length(bad)) {
    .ard_stop(sprintf("study.yml ai: review: the kinds are %s; not %s.",
                      paste(.ai_review_kinds, collapse = ", "), paste(bad, collapse = ", ")))
  }
  vapply(.ai_review_kinds, function(k) {
    vs <- unlist(if (is.null(x[[k]])) d[[k]] else x[[k]])
    if (length(vs) > 1L) {
      .ard_stop(sprintf("study.yml ai: review: %s is one value, 0 to 3 or none; not a list (%s).",
                        k, paste(format(vs), collapse = ", ")))
    }
    v <- vs[1L]
    # (YAML reads `yes` as TRUE: no level, and `no` as FALSE: none)
    if (isTRUE(v)) {
      .ard_stop(sprintf("study.yml ai: review: %s is 0 to 3, or none; not yes / true.", k))
    }
    if (isFALSE(v) || (is.character(v) && tolower(v) %in% c("none", "off", "no"))) return(NA_integer_)
    n <- suppressWarnings(as.integer(v))
    if (is.na(n) || !n %in% 0:3) {
      .ard_stop(sprintf("study.yml ai: review: %s is 0 to 3, or none; not %s.", k, format(v)))
    }
    n
  }, 1L)
}

# ---- the program (level 1) -------------------------------------------------------

# A report's program, for the agent to read: the steps that make its data
# and its ARD (the study ARD program's part for the report, without the
# functions every program calls), the records it reads, then its content
.ai_report_program <- function(o, st, id) {
  ard <- tryCatch({
    x <- tfl_ard_code(st, output_id = id, save = FALSE)
    i <- grep("^# ---- data ----", x)
    cl <- grep("^# ---- code lists", x)
    if (length(i) && any(grepl("^# ---- analyses ----", x))) {
      # (the code lists the program uses come first, then the data on;
      # never the functions every program calls)
      keep <- x[i[1L]:length(x)]
      if (length(cl) && cl[1L] < i[1L]) {
        heads <- grep("^# ---- ", x)
        end <- min(heads[heads > cl[1L]], length(x) + 1L) - 1L
        keep <- c(x[cl[1L]:end], keep)
      }
      keep
    }
  }, error = function(e) NULL)
  data <- tryCatch(unlist(tfl_data_code(o, st, ard = FALSE), use.names = FALSE),
                   error = function(e) NULL)
  content <- tryCatch(tfl_content_code(o), error = function(e) NULL)
  c(sprintf("# %s: the program that makes it, as tflspec writes it from the draft.", id),
    "# Read it against the shell; it is not run from here.", "",
    ard, if (length(data)) c("", "# ---- the records ----", data),
    if (length(content)) c("", "# ---- content ----", content))
}

# ---- the facts (levels 2 and 3) --------------------------------------------------

# A table's or a listing's facts: its header, its rows and its empty
# cells (level 2), then its pages as R lays them out (level 3).  `doc`:
# the report as built (tfl_report()).
.ai_table_facts <- function(doc, id) {
  lay <- suppressWarnings(rtfreporter::rtf_layout(doc))
  geo <- .ai_page_geometry(doc)
  segs <- list()
  for (k in seq_along(lay$pages)) {
    for (s in lay$pages[[k]]$segments) if (inherits(s$content, "rtftable")) {
      s$page <- k
      s$section <- lay$pages[[k]]$section
      segs[[length(segs) + 1L]] <- s
    }
  }
  if (!length(segs)) return(sprintf("# %s: no table was laid out.", id))
  first <- segs[[1L]]$content
  pages <- .ai_pages_facts(segs, geo, length(lay$pages), doc$sections)
  over <- attr(pages, "over")
  c(sprintf("# %s: what R made, in words (read this, not the RTF)", id),
    if (length(lay$pages) > 1L || over) {
      sprintf("More than one page (%d laid out%s): read *The pages* too.", length(lay$pages),
              if (over) ", one over full" else "")
    } else "One page.",
    "", "## The table", "", .ai_rtftable_facts(first),
    if (length(segs) > 1L) c("", sprintf("%d tables (a table a page or a group of pages):", length(segs)),
                             vapply(seq_along(segs), function(i) {
                               .ai_rows_line(segs[[i]]$content, sprintf("- %d (page %d)", i, segs[[i]]$page))
                             }, "")),
    "", "## The pages", "", as.character(pages))
}

# A table's header rows: its own, else its data's names
.ai_header_rows <- function(tb) {
  if (length(tb$col_header)) tb$col_header else list(names(tb$data))
}

# A header row's cells: their text and the columns each spans (a spanner
# is a list of from, to, label)
.ai_header_cells <- function(h) {
  if (!is.list(h)) return(list(text = as.character(h), from = seq_along(h), to = seq_along(h)))
  sp <- function(x, k) if (is.list(x)) as.integer(x[[k]]) else NA_integer_
  text <- vapply(h, function(x) if (is.list(x)) as.character(x$label %||% "") else paste(x, collapse = " "), "")
  from <- vapply(seq_along(h), function(i) if (is.list(h[[i]])) sp(h[[i]], "from") else i, 1L)
  to <- vapply(seq_along(h), function(i) if (is.list(h[[i]])) sp(h[[i]], "to") else i, 1L)
  text <- ifelse(to > from, sprintf("%s [columns %d-%d]", text, from, to), text)
  list(text = text, from = from, to = to)
}

# A row's label without its indent
.ai_label <- function(x) trimws(x, whitespace = "[\\h\\v]")

# A table's cells as text: the data's columns, each a character vector
.ai_cells <- function(tb) {
  d <- tb$data
  as.data.frame(lapply(d, function(v) {
    v <- as.character(v)
    v[is.na(v)] <- ""
    v
  }), stringsAsFactors = FALSE, optional = TRUE)
}

.ai_rows_line <- function(tb, lead) {
  d <- .ai_cells(tb)
  if (!nrow(d)) return(sprintf("%s: no rows", lead))
  sprintf("%s: %d rows, the first \"%s\", the last \"%s\"", lead, nrow(d),
          .ai_label(d[[1L]][1L]), .ai_label(d[[1L]][nrow(d)]))
}

.ai_rtftable_facts <- function(tb) {
  d <- .ai_cells(tb)
  n_blank <- length(tb$blank_rows)
  row_text <- function(i) {
    x <- paste(unlist(d[i, ], use.names = FALSE), collapse = " | ")
    if (nchar(x) > 160L) paste0(substr(x, 1L, 157L), "...") else x
  }
  rows <- if (nrow(d) <= 40L) vapply(seq_len(nrow(d)), row_text, "") else {
    c(vapply(1:20, row_text, ""), sprintf("... %d rows ...", nrow(d) - 40L),
      vapply((nrow(d) - 19L):nrow(d), row_text, ""))
  }
  w <- tb$col_rel_width
  widths <- if (length(w) == ncol(d)) paste(sprintf("%.0f%%", 100 * w / sum(w)), collapse = " | ")
  # the value cells: empty where a row has others (a heading row has none),
  # and a missing value printed
  vals <- d[, -1L, drop = FALSE]
  empty <- if (ncol(vals)) {
    e <- as.matrix(vals) == "" | is.na(as.matrix(vals))
    which(rowSums(e) > 0L & rowSums(e) < ncol(vals))
  } else integer()
  na <- if (ncol(vals)) which(apply(as.matrix(vals), 1L, function(r) any(grepl("\\bNA\\b|NaN|Inf", r))))
  where <- function(i) {
    if (!length(i)) return("none.")
    lab <- .ai_label(d[[1L]][i])
    sprintf("%d, in the rows %s%s.", length(i), paste(sprintf("\"%s\"", utils::head(lab, 8L)), collapse = ", "),
            if (length(i) > 8L) " ..." else "")
  }
  c(sprintf("%d columns, %d rows (and %d blank rows between them).", ncol(d), nrow(d), n_blank),
    "Header:",
    vapply(seq_along(.ai_header_rows(tb)), function(k) {
      sprintf("  %d: %s", k, paste(.ai_header_cells(.ai_header_rows(tb)[[k]])$text, collapse = " | "))
    }, ""),
    if (!is.null(widths)) sprintf("Widths: %s", widths),
    if (nrow(d) <= 40L) "Rows:" else "Rows (the first 20 and the last 20):",
    paste0("  ", rows),
    sprintf("Empty cells (in a row with others): %s", where(empty)),
    sprintf("Missing values printed (NA, NaN, Inf): %s", where(na)))
}

# The page's size and the lines a page holds, about (the font's line
# height on the page less its margins); the column widths in inches
.ai_page_geometry <- function(doc) {
  pg <- doc$document$page
  sizes <- list(letter = c(8.5, 11), a4 = c(8.27, 11.69), legal = c(8.5, 14))
  wh <- if (!is.null(pg$width_in) && !is.null(pg$height_in)) c(pg$width_in, pg$height_in) else {
    d <- sizes[[tolower(pg$paper_size %||% "letter")]] %||% sizes$letter
    if (identical(pg$orientation %||% "landscape", "landscape")) rev(d) else d
  }
  m <- function(k) pg[[paste0("margin_", k, "_in")]] %||% 0.75
  hp <- doc$document$style$font_size_half_points %||%
    getOption("rtfreporter.font_size_half_points", 18)
  list(width = wh[1L] - m("left") - m("right"), hp = hp,
       lines = floor((wh[2L] - m("top") - m("bottom")) * 72 / (hp / 2 * 1.2)),
       desc = sprintf("%s, %s pt", if (!is.null(pg$width_in)) sprintf("%s x %s in", wh[1L], wh[2L]) else
         paste(pg$paper_size %||% "letter", pg$orientation %||% "landscape"), format(hp / 2)))
}

# The lines a text takes in a width, wrapped (at least 1)
.ai_text_lines <- function(text, width_in, hp) {
  if (!length(text)) return(integer())
  w <- rtfreporter::text_width_in(as.character(text), font_size_half_points = hp)
  pmax(1L, as.integer(ceiling(w / pmax(width_in - 0.1, 0.2))))
}

# A page band's (rtf_header() / rtf_footer()) lines as stored: a row's
# cells apart by " | " (empty ones left out), the rows in order
.ai_band_lines <- function(b) {
  x <- vapply(b$rows %||% list(), function(r) {
    paste(Filter(nzchar, as.character(unlist(r, use.names = FALSE))), collapse = " | ")
  }, "")
  x[nzchar(x)]
}

# A title's or a footnote's row as its text: a row is the text (named by its
# alignment, c / l / r) or a list of the text, its alignment and flags
.ai_row_text <- function(x) {
  if (is.list(x) && !is.null(x$text)) return(paste(as.character(unlist(x$text)), collapse = " "))
  if (is.list(x)) {
    k <- names(x) %in% c("c", "l", "r", "")
    if (any(k)) x <- x[k]
  }
  paste(as.character(unlist(x, use.names = FALSE)), collapse = " ")
}

.ai_pages_facts <- function(segs, geo, n_pages, sections = NULL) {
  over <- FALSE
  one <- function(s) {
    tb <- s$content
    d <- .ai_cells(tb)
    w <- tb$col_rel_width
    cw <- if (length(w) == ncol(d)) geo$width * w / sum(w) else rep(geo$width / max(ncol(d), 1L), ncol(d))
    row_lines <- if (nrow(d)) {
      do.call(pmax, lapply(seq_len(ncol(d)), function(j) .ai_text_lines(d[[j]], cw[j], geo$hp)))
    } else integer()
    head_lines <- sum(vapply(.ai_header_rows(tb), function(h) {
      h <- .ai_header_cells(h)
      w <- vapply(seq_along(h$text), function(i) sum(cw[h$from[i]:h$to[i]], na.rm = TRUE), 1)
      max(.ai_text_lines(h$text, w, geo$hp))
    }, 1L))
    tl <- vapply(s$title %||% list(), .ai_row_text, "")
    fl <- vapply(s$footnote %||% list(), .ai_row_text, "")
    title_lines <- sum(.ai_text_lines(tl, geo$width, geo$hp))
    foot_lines <- sum(.ai_text_lines(fl, geo$width, geo$hp))
    sec <- if (length(sections)) sections[[min(s$section %||% 1L, length(sections))]]
    ph <- .ai_band_lines(sec$header)
    pf <- .ai_band_lines(sec$footer)
    band_lines <- sum(.ai_text_lines(pf, geo$width, geo$hp))
    used <- title_lines + 1L + head_lines + sum(row_lines) + length(tb$blank_rows) +
      (if (foot_lines) 1L + foot_lines else 0L) + band_lines
    wrapped <- sum(row_lines > 1L)
    fit <- if (used <= geo$lines) "fits" else {
      sprintf("about %d lines more than a page holds: Word carries them on to another page", used - geo$lines)
    }
    over <<- over || used > geo$lines
    c(sprintf("Page %d: %d title lines, %d header lines, %d rows (%d blank), %d footnote lines: about %d lines; %s.",
              s$page, title_lines, head_lines, nrow(d), length(tb$blank_rows), foot_lines, used, fit),
      if (wrapped) sprintf("  %d rows wrap onto more lines (a cell wider than its column).", wrapped),
      if (nrow(d)) sprintf("  The first row \"%s\", the last \"%s\".", .ai_label(d[[1L]][1L]), .ai_label(d[[1L]][nrow(d)])),
      if (length(tl)) paste0("  Title: ", paste(tl, collapse = " / ")),
      if (length(fl)) paste0("  Footnotes: ", paste(fl, collapse = " / ")),
      if (length(ph)) paste0("  Page header: ", paste(ph, collapse = " / ")),
      if (length(pf)) paste0("  Page footer: ", paste(pf, collapse = " / ")))
  }
  lines <- unlist(lapply(segs, one))
  structure(c(sprintf("%d page(s) (%s; about %d lines a page; nothing is measured: Word may differ a little).",
                      n_pages, geo$desc, geo$lines), lines),
            over = over)
}

# The program and the facts beside a report's RTF (`f`): `<id>.R`, and
# for a table or a listing `<id>-facts.txt` (a figure's are its own,
# beside its PNG).  A failure here is the facts', not the build's.
.ai_review_write <- function(o, st, id, f, doc = NULL) {
  base <- sub("\\.rtf$", "", f)
  tryCatch(writeLines(.ai_report_program(o, st, id), paste0(base, ".R"), useBytes = TRUE),
           error = function(e) NULL)
  if (!is.null(doc)) {
    facts <- tryCatch(.ai_table_facts(doc, id), error = function(e) {
      sprintf("# %s: the facts could not be read: %s", id, .ai_one_line(conditionMessage(e)))
    })
    writeLines(facts, paste0(base, "-facts.txt"), useBytes = TRUE)
  }
  invisible(NULL)
}
