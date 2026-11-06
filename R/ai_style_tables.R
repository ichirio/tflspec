# ============================================================================
#  Past TFLs, one row a table (tflspec#547, step 1)
# ----------------------------------------------------------------------------
#  tfl_rtf_style_scan() reads the rows; tfl_rtf_style_tables() sums them up
#  a table: its column header (rows, the rules under it), the stub's width
#  to a value column's, the rules, fonts and alignment, the page, where its
#  titles and footnotes are and how they are aligned, and a guess of its
#  kind from its title.  The titles are read for that guess only; they stay
#  on the person's PC with the files.
# ============================================================================

#' The tables of a folder of past RTF reports, one row each
#'
#' Sums up [tfl_rtf_style_scan()]'s rows a table, for a person (or an AI
#' agent) who writes a company standard from what past reports do.  SAS's
#' ODS RTF is read as it is written: the titles and footnotes in each
#' page's `\header` / `\footer` (or as paragraphs before and after the
#' table), a page a section, the column header rows (`\trhdr`) repeated on
#' each page and counted once.
#'
#' @param rtf_dir The folder of RTF files (a subfolder a study).
#' @return A data frame, one row a table: `file`, `table`, `pages`, `kind`
#'   (a guess from the title: `demog`, `ae_soc_pt`, `ae`, `param_change`,
#'   `shift`, `listing`, `figure`, `other`), `title` (its title lines,
#'   ` / ` between them), `header_rows`, `body_rows`, `n_cols`,
#'   `rel_widths` (the last header row's cells, as parts of the narrowest),
#'   `stub_ratio` (the first column's width to the median of the others'),
#'   `rules` (where lines are drawn: `above_header`, `under_spanner`,
#'   `under_header`, `body`, `end`, ` | ` between), `font`,
#'   `font_size_pt` (the body's), `stub_align`, `value_align`,
#'   `orientation`, `margins_in` (left, right, top, bottom), `title_place`
#'   and `footnote_place` (`header` / `footer`, the page's; `before` /
#'   `after`, paragraphs in the body; `none`), `title_align`,
#'   `footnote_align`, `footnotes` (their number).
#' @seealso [tfl_rtf_style_scan()]
#' @examples
#' d <- tempfile()
#' dir.create(d)
#' writeLines(c("{\\rtf1\\ansi{\\fonttbl{\\f0 Courier New;}}",
#'   "{\\header\\pard\\qc Table 14.1.1\\par\\pard\\qc Demographic Characteristics\\par}",
#'   paste0("\\trowd\\trhdr\\clbrdrt\\brdrs\\clbrdrb\\brdrs\\cellx3000",
#'          "\\clbrdrt\\brdrs\\clbrdrb\\brdrs\\cellx4500"),
#'   "\\pard\\intbl\\ql \\cell\\pard\\intbl\\qc Drug X\\cell\\row",
#'   "\\trowd\\clbrdrb\\brdrs\\cellx3000\\clbrdrb\\brdrs\\cellx4500",
#'   "\\pard\\intbl\\ql Age\\cell\\pard\\intbl\\qc 54\\cell\\row}"),
#'   file.path(d, "t.rtf"))
#' tfl_rtf_style_tables(d)[, c("kind", "header_rows", "stub_ratio", "rules")]
#' @export
tfl_rtf_style_tables <- function(rtf_dir) {
  if (!is.character(rtf_dir) || length(rtf_dir) != 1L || !dir.exists(rtf_dir)) {
    .ard_stop("tfl_rtf_style_tables(): `rtf_dir` is a folder of RTF files.")
  }
  rows <- tfl_rtf_style_scan(rtf_dir)
  out <- list()
  for (f in unique(rows$file)) {
    x <- paste(readLines(file.path(rtf_dir, f), warn = FALSE, encoding = "latin1"), collapse = "\n")
    doc <- .rtf_doc_facts(x)
    d <- rows[rows$file == f, , drop = FALSE]
    for (t in unique(d$table)) {
      out[[length(out) + 1L]] <- .rtf_table_facts(d[d$table == t, , drop = FALSE], doc)
    }
  }
  empty <- data.frame(file = character(), table = integer(), pages = integer(), kind = character(),
                      title = character(), header_rows = integer(), body_rows = integer(),
                      n_cols = integer(), rel_widths = character(), stub_ratio = numeric(),
                      rules = character(), font = character(), font_size_pt = numeric(),
                      stub_align = character(), value_align = character(),
                      orientation = character(), margins_in = character(),
                      title_place = character(), title_align = character(),
                      footnote_place = character(), footnote_align = character(),
                      footnotes = integer(), stringsAsFactors = FALSE)
  out <- do.call(rbind, c(list(empty), out))
  rownames(out) <- NULL
  out
}

# ---- a table's facts ---------------------------------------------------------

.rtf_table_facts <- function(d, doc) {
  # the column header: the rows marked \trhdr, else the first row
  h <- cumprod(d$header) == 1L
  if (!any(h)) h[1L] <- TRUE
  hd <- d[h, , drop = FALSE]
  bd <- d[!h, , drop = FALSE]
  last <- hd[nrow(hd), ]
  w <- as.numeric(strsplit(last$rel_widths, " | ", fixed = TRUE)[[1L]])
  ratio <- if (length(w) > 1L && !anyNA(w)) round(w[1L] / stats::median(w[-1L]), 2) else NA_real_
  bits <- function(s) strsplit(s, "", fixed = TRUE)[[1L]] == "1"
  all_on <- function(s) { b <- bits(s); length(b) && mean(b) >= 0.5 }
  rules <- c(
    above_header = any(bits(hd$border_top[1L])),
    under_spanner = nrow(hd) > 1L && any(vapply(hd$border_bottom[-nrow(hd)], function(s) {
      b <- bits(s); any(b) && !all(b)
    }, NA)),
    under_header = all_on(last$border_bottom) ||
      (nrow(bd) > 0L && all_on(bd$border_top[1L])),
    body = nrow(bd) > 1L && any(vapply(seq_len(nrow(bd) - 1L), function(i) {
      any(bits(bd$border_bottom[i])) || (i > 1L && any(bits(bd$border_top[i])))
    }, NA)),
    end = nrow(bd) > 0L && any(bits(bd$border_bottom[nrow(bd)])))
  al <- if (nrow(bd)) bd$align else hd$align
  mode <- function(v) { v <- v[!is.na(v) & nzchar(v)]; if (length(v)) names(sort(table(v), decreasing = TRUE))[1L] else NA_character_ }
  stub_al <- mode(substring(al, 1L, 1L))
  val_al <- mode(unlist(lapply(al, function(a) if (nchar(a) > 1L) strsplit(substring(a, 2L), "")[[1L]])))
  pg <- min(d$page)
  tt <- doc$titles[[min(pg, length(doc$titles))]] %||% list(place = "none", align = NA, text = character())
  fn <- doc$footnotes[[min(pg, length(doc$footnotes))]] %||% list(place = "none", align = NA, text = character())
  # the font the table's cells use, the commonest; the file's default font
  # (\deff, else the font table's first) when the rows name none
  uses <- unlist(strsplit(d$font_uses[nzchar(d$font_uses)], " | ", fixed = TRUE))
  font_used <- if (length(uses)) mode(uses) else doc$font_default %||% NA_character_
  data.frame(
    file = d$file[1L], table = d$table[1L], pages = length(unique(d$page)),
    kind = .rtf_kind_guess(tt$text), title = paste(tt$text, collapse = " / "),
    header_rows = nrow(hd), body_rows = nrow(bd), n_cols = last$n_cells,
    rel_widths = last$rel_widths, stub_ratio = ratio,
    rules = paste(names(rules)[rules], collapse = " | "),
    font = font_used,
    font_size_pt = suppressWarnings(as.numeric(mode(as.character(
      if (nrow(bd)) bd$font_size_pt else hd$font_size_pt)))),
    stub_align = stub_al, value_align = val_al,
    orientation = doc$orientation, margins_in = doc$margins,
    title_place = tt$place, title_align = tt$align %||% NA_character_,
    footnote_place = fn$place, footnote_align = fn$align %||% NA_character_,
    footnotes = length(fn$text), stringsAsFactors = FALSE)
}

# A table's kind from its title lines (lower case): a guess, `other` when
# none fits
.rtf_kind_guess <- function(title) {
  t <- tolower(paste(title, collapse = " "))
  has <- function(p) grepl(p, t, perl = TRUE)
  if (!nzchar(t)) return("other")
  if (has("^\\s*listing|\\blisting\\b")) return("listing")
  if (has("^\\s*figure|kaplan|\\bplot\\b|forest|waterfall")) return("figure")
  if (has("demograph|baseline characteristic")) return("demog")
  if (has("adverse event") && has("system organ class|preferred term")) return("ae_soc_pt")
  if (has("adverse event")) return("ae")
  if (has("\\bshift\\b")) return("shift")
  if (has("laborator|vital sign|ecg|chemistry|haematology|hematology|urinalysis") &&
      has("change")) return("param_change")
  "other"
}

# ---- a file's facts: its page, and each page's titles and footnotes ----------

.rtf_doc_facts <- function(x) {
  num <- function(w) {
    m <- regmatches(x, regexpr(sprintf("\\\\%s-?[0-9]+", w), x, perl = TRUE))
    if (length(m)) as.numeric(sub(sprintf("\\\\%s", w), "", m)) else NA_real_
  }
  pw <- num("paperw")
  ph <- num("paperh")
  land <- grepl("\\\\landscape(?![a-z])", x, perl = TRUE) || (!is.na(pw) && !is.na(ph) && pw > ph)
  mg <- vapply(c("margl", "margr", "margt", "margb"), num, 0)
  # each page's \header / \footer groups (SAS: one a section)
  heads <- .rtf_groups(x, "header")
  foots <- .rtf_groups(x, "footer")
  # the body: the document without its groups of words (font table,
  # header, footer, info ...), for titles written as paragraphs
  body <- .rtf_drop_groups(x, c("fonttbl", "colortbl", "stylesheet", "info", "header", "headerl",
                                "headerr", "headerf", "footer", "footerl", "footerr", "footerf",
                                "\\*"))
  first <- regexpr("\\\\trowd", body, perl = TRUE)
  ends <- gregexpr("\\\\row(?![a-z])", body, perl = TRUE)[[1L]]
  before <- if (first > 0L) .rtf_paras(substring(body, 1L, first - 1L)) else list()
  after <- if (ends[1L] > 0L) .rtf_paras(substring(body, max(ends) + 4L)) else list()
  side <- function(groups, paras, place_g, place_p) {
    from_g <- lapply(groups, function(g) {
      p <- .rtf_paras(g)
      # (a page header's or footer's running line -- the company, the page
      # number -- is not a title or a footnote: its lines with a page field)
      p <- Filter(function(e) !e$page_no, p)
      if (length(p)) list(place = place_g, align = .rtf_mode(vapply(p, `[[`, "", "align")),
                          text = vapply(p, `[[`, "", "text")) else NULL
    })
    from_g <- Filter(Negate(is.null), from_g)
    if (length(from_g)) return(from_g)
    if (length(paras)) {
      return(list(list(place = place_p, align = .rtf_mode(vapply(paras, `[[`, "", "align")),
                       text = vapply(paras, `[[`, "", "text"))))
    }
    list(list(place = "none", align = NA_character_, text = character()))
  }
  list(orientation = if (land) "landscape" else "portrait",
       margins = if (anyNA(mg)) NA_character_ else paste(round(mg / 1440, 2), collapse = " | "),
       font_default = {
         ft <- .rtf_font_table(x)
         deff <- regmatches(x, regexpr("\\\\deff[0-9]+", x, perl = TRUE))
         dn <- if (length(deff)) substring(deff, 6L) else NA_character_
         if (!is.na(dn) && dn %in% names(ft)) unname(ft[dn]) else if (length(ft)) unname(ft[1L]) else NA_character_
       },
       titles = side(heads, before, "header", "before"),
       footnotes = side(foots, after, "footer", "after"))
}

.rtf_mode <- function(v) {
  v <- v[!is.na(v)]
  if (!length(v)) return(NA_character_)
  names(sort(table(v), decreasing = TRUE))[1L]
}

# The end of the group that opens at `start` (a "{"), braces escaped as
# \{ \} not counted
.rtf_group_end <- function(x, start) {
  ch <- strsplit(substring(x, start), "")[[1L]]
  depth <- 0L
  i <- 1L
  while (i <= length(ch)) {
    c1 <- ch[i]
    if (c1 == "\\") {
      i <- i + 2L
      next
    }
    if (c1 == "{") depth <- depth + 1L
    if (c1 == "}") {
      depth <- depth - 1L
      if (depth == 0L) return(start + i - 1L)
    }
    i <- i + 1L
  }
  nchar(x)
}

# The groups that start with a word ({\header ...}), in order, as text
.rtf_groups <- function(x, word) {
  at <- gregexpr(sprintf("\\{\\\\%s(?![a-z])", word), x, perl = TRUE)[[1L]]
  if (at[1L] < 0L) return(list())
  lapply(at, function(s) substring(x, s, .rtf_group_end(x, s)))
}

# The document without the groups that start with these words
.rtf_drop_groups <- function(x, words) {
  for (w in words) {
    repeat {
      s <- regexpr(sprintf("\\{\\\\%s(?![a-z])", w), x, perl = TRUE)
      if (s < 0L) break
      x <- paste0(substring(x, 1L, s - 1L), substring(x, .rtf_group_end(x, s) + 1L))
    }
  }
  x
}

# A text's paragraphs (\par), each its alignment and its plain text; the
# empty ones dropped
.rtf_paras <- function(x) {
  ps <- strsplit(x, "\\\\(par|cell|row)(?![a-z])", perl = TRUE)[[1L]]
  out <- lapply(ps, function(p) {
    al <- regmatches(p, gregexpr("\\\\q[lcrj](?![a-z])", p, perl = TRUE))[[1L]]
    txt <- .rtf_plain(p)
    if (!nzchar(txt)) return(NULL)
    list(align = if (length(al)) substring(al[length(al)], 3L, 3L) else "l", text = txt,
         page_no = grepl("fldinst\\s*(PAGE|NUMPAGES)|\\\\chpgn", p, perl = TRUE))
  })
  Filter(Negate(is.null), out)
}

# RTF to plain text: the words of a paragraph, without control words,
# fields' instructions or destinations
.rtf_plain <- function(x) {
  x <- gsub("\\{\\\\\\*[^{}]*\\}", "", x, perl = TRUE)
  x <- gsub("\\\\(line|tab|~)(?![a-z])", " ", x, perl = TRUE)
  x <- gsub("\\\\~", " ", x, perl = TRUE)
  hex <- gregexpr("\\\\'[0-9a-fA-F]{2}", x, perl = TRUE)
  regmatches(x, hex) <- lapply(regmatches(x, hex), function(m) {
    vapply(m, function(h) iconv(rawToChar(as.raw(strtoi(substring(h, 3L), 16L))), "latin1", "UTF-8"), "")
  })
  uni <- gregexpr("\\\\u-?[0-9]+\\??", x, perl = TRUE)
  regmatches(x, uni) <- lapply(regmatches(x, uni), function(m) {
    vapply(m, function(u) {
      n <- as.integer(sub("\\?$", "", substring(u, 3L)))
      if (n < 0L) n <- n + 65536L
      intToUtf8(n)
    }, "")
  })
  x <- gsub("\\\\[a-zA-Z]+-?[0-9]* ?", "", x, perl = TRUE)
  x <- gsub("\\\\([{}\\\\])", "\\1", x, perl = TRUE)
  x <- gsub("[{}]", "", x, perl = TRUE)
  trimws(gsub("\\s+", " ", x, perl = TRUE))
}
