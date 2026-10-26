# ============================================================================
#  The shell sheets first, the TOC as candidates
# ----------------------------------------------------------------------------
#  A request is a shell sheet's: what the sheet itself writes (numbers,
#  titles, populations, conditions, layout, variables) comes first, then its
#  name, then the TOC.  The TOC (every TOC sheet, an item once) is read as
#  candidates for each sheet, each with the clues that chose it -- a TOC and
#  a sheet name can hold mistakes, so nothing is dropped for not matching
#  exactly: a TOC item no sheet chose is a TOC item with no shell found.
#  (Design: _archive/ai-sheet-first-design-2026-10-23.md.)
# ============================================================================

# The words of a TOC sheet's name (any sheet whose name says one is a TOC:
# Topline_TOC too)
.ai_toc_name_re <- paste0(
  "(?i)(^|[^a-z])(toc|contents?|index)([^a-z]|$)|table of contents|^lists?([^a-z]|$)|",
  "\u76ee\u6b21|\u4e00\u89a7\u8868|\u5e33\u7968\u4e00\u89a7|\u5e33\u7968\u30ea\u30b9\u30c8")

# Each sheet's role: `toc`, `shell` or `other` -- a sheet named like a TOC
# (none so named: the sheet listing the most report numbers, three at
# least), a sheet of content (5 cells or more, not hidden) a shell, the
# rest other; `sheet_roles` (a named vector, sheet = role) over all.
# `toc_sheet` (one name; NA: none) as before.  A named vector by sheet.
.ai_sheet_roles <- function(info, cells, toc_sheet = NULL, sheet_roles = NULL, fn) {
  roles <- stats::setNames(rep("other", nrow(info)), info$sheet)
  n_cells <- vapply(info$sheet, function(s) sum(cells$sheet == s), 1L)
  hidden <- if (!is.null(info$hidden)) as.logical(info$hidden) else rep(FALSE, nrow(info))
  hidden[is.na(hidden)] <- FALSE
  # (a sheet named by a report number, or writing one, is a shell however
  # small)
  numbered <- vapply(.ai_ids_in(info$sheet), nrow, 1L) > 0L |
    vapply(info$sheet, function(s) {
      h <- .ai_ids_in(cells$text[cells$sheet == s])
      any(vapply(h, function(z) nrow(z) > 0L && any(!is.na(z$type)), NA))
    }, NA)
  roles[(n_cells >= 5L | (numbered & n_cells > 0L)) & !hidden] <- "shell"
  first <- info$book_path == info$book_path[1L]
  if (!is.null(toc_sheet)) {
    if (!(length(toc_sheet) == 1L && is.na(toc_sheet))) {
      if (!all(toc_sheet %in% info$sheet)) {
        .ard_stop(sprintf("%s: no sheet %s; the sheets are %s.", fn,
                          paste(sQuote(setdiff(toc_sheet, info$sheet), FALSE), collapse = ", "),
                          paste(sQuote(info$sheet, FALSE), collapse = ", ")))
      }
      roles[toc_sheet] <- "toc"
    }
  } else {
    named <- first & grepl(.ai_toc_name_re, info$sheet, perl = TRUE) &
      vapply(.ai_ids_in(info$sheet), nrow, 1L) == 0L
    if (any(named)) {
      roles[info$sheet[named]] <- "toc"
    } else {
      g <- .ai_shell_toc_sheet(info[first, , drop = FALSE], cells, NULL, fn)
      if (length(g)) roles[g] <- "toc"
    }
  }
  if (!is.null(sheet_roles)) {
    if (!is.character(sheet_roles) || is.null(names(sheet_roles)) ||
        !all(sheet_roles %in% c("toc", "shell", "other"))) {
      .ard_stop(sprintf(paste0("%s: `sheet_roles` is a named character vector, a sheet's role ",
                               "each: c(\"Topline TOC\" = \"other\"); the roles are toc, shell, other."),
                        fn))
    }
    bad <- setdiff(names(sheet_roles), info$sheet)
    if (length(bad)) {
      .ard_stop(sprintf("%s: `sheet_roles`: no sheet %s; the sheets are %s.", fn,
                        paste(sQuote(bad, FALSE), collapse = ", "),
                        paste(sQuote(info$sheet, FALSE), collapse = ", ")))
    }
    roles[names(sheet_roles)] <- unname(sheet_roles)
  }
  roles
}

# The TOC items of every TOC sheet, an item once (by its number): the
# sheets that list it (`toc_sheets`, " | "), the first row kept -- of the
# TOC listing the most items first.
.ai_toc_items <- function(cells, toc_names, toc_cols, sheet_keys) {
  if (!length(toc_names)) return(.ai_shell_toc(cells, character(), toc_cols, sheet_keys))
  each <- lapply(toc_names, function(s) {
    t <- suppressWarnings(.ai_shell_toc(cells, s, toc_cols, sheet_keys))
    attr(t, "same_number") <- NULL
    t
  })
  n <- vapply(each, nrow, 1L)
  # each TOC sheet's first item row: its header rows are above it
  first <- stats::setNames(vapply(each, function(t) if (nrow(t)) min(t$row, na.rm = TRUE) else Inf, 1),
                           toc_names)
  ord <- order(-n)
  all <- do.call(rbind, each[ord])
  if (!nrow(all)) {
    all$toc_sheets <- character()
    return(all)
  }
  k <- ifelse(is.na(all$key), paste0("\r", seq_len(nrow(all))), all$key)
  lists <- tapply(all$sheet, factor(k, levels = unique(k)), function(v) paste(unique(v), collapse = " | "))
  keep <- !duplicated(k)
  toc <- all[keep, , drop = FALSE]
  toc$toc_sheets <- unname(lists[k[keep]])
  # (an item one TOC alone lists, when there are several: its other
  # information says so)
  if (length(toc_names) > 1L) {
    only <- !grepl(" | ", toc$toc_sheets, fixed = TRUE)
    note <- sprintf("listed only in TOC sheet '%s'", toc$toc_sheets)
    toc$info[only] <- ifelse(is.na(toc$info[only]), note[only], paste0(toc$info[only], "; ", note[only]))
  }
  toc$same_number <- FALSE
  rownames(toc) <- NULL
  attr(toc, "first_rows") <- first
  toc
}

# A key's number levels ("T15.1.4.2.2.1" -> 15 1 4 2 2 1), letters dropped
.ai_levels <- function(key) {
  if (is.na(key)) return(character())
  k <- sub("^[A-Z]+", "", key)
  k <- sub("[A-Z]$", "", k)
  strsplit(k, ".", fixed = TRUE)[[1L]]
}

# The words of a text worth comparing (3 letters or more, lower case)
.ai_words <- function(x) {
  w <- tolower(unlist(strsplit(paste(x, collapse = " "), "[^[:alnum:]]+")))
  unique(w[nchar(w) >= 3L & !grepl("^[0-9]+$", w)])
}

# What a shell sheet says of itself: the numbers written in its first rows
# (keys and as written) and its title-like line (the longest text there
# that is not only a number)
.ai_sheet_self <- function(d, rows = 12L) {
  top <- d[d$row %in% utils::head(sort(unique(d$row)), rows), , drop = FALSE]
  # (the numbers of the whole sheet: two reports one under the other)
  ids <- .ai_ids_in(d$text)
  h <- do.call(rbind, c(list(data.frame(id = character(), key = character(),
                                          stringsAsFactors = FALSE)),
                        lapply(ids, function(z) if (nrow(z)) z[c("id", "key")])))
  h <- h[!duplicated(h$key), , drop = FALSE]
  # (a number with no word of kind is one of three levels or more: not a
  # version 1.0, a date)
  typed <- vapply(seq_len(nrow(h)), function(i) {
    !is.na(.ai_id_type(h$id[i])) || length(.ai_levels(h$key[i])) >= 3L
  }, NA)
  h <- h[typed, , drop = FALSE]
  txt <- trimws(gsub("\\s+", " ", top$text))
  txt <- txt[nchar(txt) > 3L]
  rest <- vapply(txt, function(t) {
    for (i in h$id) t <- gsub(i, "", t, fixed = TRUE)
    trimws(t)
  }, "", USE.NAMES = FALSE)
  ok <- nchar(rest) > 5L
  title <- if (any(ok)) txt[ok][which.max(nchar(rest[ok]))] else NA_character_
  list(ids = h$id, keys = h$key, title = title, text = txt)
}

# What the candidates are scored on, a TOC item each, made once a
# workbook: its keys (as numbers without the kind's letter too), the
# levels of its number and its shell's, its title's words
.ai_toc_pre <- function(toc) {
  k <- toc$key
  sk <- toc$shell_key %||% k
  bare <- function(v) sub("^[TFL]", "", v)
  list(k = k, sk = sk, kb = bare(k), skb = bare(sk),
       kt = grepl("^[TFL]", k), skt = grepl("^[TFL]", sk),
       lv = lapply(k, .ai_levels), slv = lapply(sk, .ai_levels),
       words = lapply(toc$title, .ai_words))
}

# Is the key `a` each item's number (or its shell's), as .ai_id_same()
# says it, a vector over the items
.ai_same_v <- function(a, keys, bare, typed) {
  if (is.na(a)) return(rep(FALSE, length(keys)))
  ab <- sub("^[TFL]", "", a)
  at <- grepl("^[TFL]", a)
  !is.na(keys) & (keys == a | (bare == ab & (!at | !typed)))
}

# The levels two numbers share from the first, as a share of the longer's
# (two at least; else 0)
.ai_close <- function(la, lb) {
  if (length(la) < 2L || length(lb) < 2L) return(0)
  m <- min(length(la), length(lb))
  eq <- la[seq_len(m)] == lb[seq_len(m)]
  n <- if (all(eq)) m else which(!eq)[1L] - 1L
  if (n < 2L) 0 else n / max(length(la), length(lb))
}

# A sheet's TOC candidates: every item scored on the clues -- a number in
# the sheet that is the item's (its number or its shell's): 3; the sheet
# name's number that is the item's: 2; a close number (the first levels
# the same): the share of levels in common; the title's words: their
# overlap -- the top `top` with a score, each with its clues.
.ai_sheet_candidates <- function(sheet, self, toc, top = 3L, pre = .ai_toc_pre(toc)) {
  none <- data.frame(i = integer(), score = numeric(), clues = character(),
                     stringsAsFactors = FALSE)
  n <- nrow(toc)
  if (!n) return(none)
  name_key <- {
    z <- .ai_ids_in(sheet)[[1L]]
    if (nrow(z)) z$key[1L] else NA_character_
  }
  hit <- function(a) .ai_same_v(a, pre$k, pre$kb, pre$kt) | .ai_same_v(a, pre$sk, pre$skb, pre$skt)
  written <- rep(FALSE, n)
  for (k in self$keys) written <- written | hit(k)
  named <- hit(name_key)
  score <- 3 * written + 2 * named
  # a close number, where the number is not the sheet's own
  own <- c(self$keys, name_key)
  own <- own[!is.na(own)]
  close <- rep(0, n)
  if (length(own)) {
    olv <- lapply(own, .ai_levels)
    # (a close number of another kind is none: a figure's 14.2.1 is not
    # near a table's 14.2.1)
    ot <- unique(substr(own[grepl("^[TFL]", own)], 1L, 1L))
    it <- ifelse(grepl("^[TFL]", pre$k), substr(pre$k, 1L, 1L), NA_character_)
    for (i in which(score < 2 & (is.na(it) | !length(ot) | it %in% ot))) {
      close[i] <- max(0, vapply(olv, function(l) max(.ai_close(l, pre$lv[[i]]),
                                                     .ai_close(l, pre$slv[[i]])), 0))
    }
  }
  sw <- .ai_words(c(self$title, self$text))
  ov <- if (length(sw)) vapply(pre$words, function(w) {
    if (length(w)) length(intersect(w, sw)) / length(w) else 0
  }, 0) else rep(0, n)
  ov[ov < 0.5] <- 0
  score <- score + close + ov
  keep <- which(score >= 0.8)
  if (!length(keep)) return(none)
  keep <- keep[order(-score[keep], keep)][seq_len(min(top, length(keep)))]
  clues <- vapply(keep, function(i) paste(c(
    if (written[i]) "its number is written in the sheet",
    if (named[i]) "the sheet name has its number",
    if (close[i] > 0) sprintf("a close number (%d%% of its levels)", round(100 * close[i])),
    if (ov[i] > 0) sprintf("its title's words are in the sheet (%d%%)", round(100 * ov[i]))),
    collapse = "; "), "")
  data.frame(i = keep, score = score[keep], clues = clues, stringsAsFactors = FALSE)
}

# The shell sheets: a row a sheet -- its name, what it says of itself, its
# TOC candidates (`candidates`: "toc_id (clues)" a line " | ", `cand` the
# items' rows), `guessed` (no number of its own and no candidate: a shell
# by its content alone).  And each TOC item's sheets (`sheets`, the shells
# it is a candidate of, its best first; NA: no shell found).
.ai_shell_sheets <- function(info, cells, roles, toc) {
  shells <- names(roles)[roles == "shell"]
  pre <- .ai_toc_pre(toc)
  rows <- lapply(shells, function(s) {
    d <- cells[cells$sheet == s, , drop = FALSE]
    self <- .ai_sheet_self(d)
    cand <- .ai_sheet_candidates(s, self, toc, pre = pre)
    list(sheet = s, self = self, cand = cand)
  })
  sh <- data.frame(
    sheet = shells,
    book = info$book[match(shells, info$sheet)],
    numbers_in_sheet = vapply(rows, function(r) paste(r$self$ids, collapse = " | "), ""),
    title_in_sheet = vapply(rows, function(r) r$self$title %||% NA_character_, ""),
    candidates = vapply(rows, function(r) {
      if (!nrow(r$cand)) return(NA_character_)
      paste(sprintf("%s (%s)", toc$output_id[r$cand$i], r$cand$clues), collapse = " | ")
    }, ""),
    stringsAsFactors = FALSE)
  sh$numbers_in_sheet[!nzchar(sh$numbers_in_sheet)] <- NA_character_
  sh$guessed <- is.na(sh$numbers_in_sheet) & is.na(sh$candidates)
  cand <- lapply(rows, `[[`, "cand")
  names(cand) <- shells
  # each item: the sheets it is a candidate of, the best score first
  best <- do.call(rbind, c(list(data.frame(i = integer(), score = numeric(), sheet = character(),
                                           stringsAsFactors = FALSE)),
                           lapply(shells, function(s) {
                             cc <- cand[[s]]
                             if (!nrow(cc)) return(NULL)
                             data.frame(i = cc$i, score = cc$score, sheet = s,
                                        stringsAsFactors = FALSE)
                           })))
  best <- best[order(best$i, -best$score), , drop = FALSE]
  toc$sheets <- vapply(seq_len(nrow(toc)), function(i) {
    s <- best$sheet[best$i == i]
    if (length(s)) paste(s, collapse = " | ") else NA_character_
  }, "")
  list(shells = sh, cand = cand, toc = toc)
}

# A shell sheet's request text: the TOC's header rows and its candidates'
# rows, then the sheet whole -- the workbook's text form
.ai_sheet_text <- function(x, sheet) {
  info <- x$info %||% x$sheets
  cand <- x$cand[[sheet]]
  toc <- x$toc
  parts <- list()
  for (ts in names(x$roles)[x$roles == "toc"]) {
    d <- x$cells[x$cells$sheet == ts, , drop = FALSE]
    # (and the TOC items with no shell its numbers were taken for)
    sn <- x$sheet_numbers
    guess <- if (is.null(sn)) integer() else
      match(sn$toc_id[sn$sheet == sheet & sn$source == "guess"], toc$output_id)
    ii <- unique(c(cand$i, guess[!is.na(guess)]))
    rows_ts <- toc$row[ii][toc$sheet[ii] == ts]
    first <- x$first_rows[[ts]] %||% Inf
    parts[[length(parts) + 1L]] <- d[d$row < first | d$row %in% rows_ts, , drop = FALSE]
  }
  parts[[length(parts) + 1L]] <- x$cells[x$cells$sheet == sheet, , drop = FALSE]
  keep <- do.call(rbind, c(list(.xl_no_cells()), parts))
  ord <- match(keep$sheet, info$sheet)
  keep <- keep[order(ord, keep$row, keep$col), , drop = FALSE]
  .ai_shell_join(x$info, list(cells = keep, outputs = toc$output_id[cand$i]))
}

# A shell sheet's request lines: what it is and how to read it, its TOC
# candidates (their clues and rows), the rules of the sheet first
.ai_sheet_lines <- function(x, sheet) {
  sh <- x$shells[x$shells$sheet == sheet, , drop = FALSE]
  cand <- x$cand[[sheet]]
  toc <- x$toc
  c(sprintf("This request is the shell sheet '%s' (its sheet name, not a report's number).", sheet),
    if (!is.na(sh$numbers_in_sheet)) sprintf("Numbers written in the sheet: %s.", sh$numbers_in_sheet),
    if (isTRUE(sh$guessed)) paste(
      "The sheet writes no report number and no TOC item fits it: it was taken as a shell by",
      "its content alone. If it is not a shell (a cover, notes, a change history), write no",
      "report file (`outputs: []`) and say so as an assumption."),
    "",
    if (nrow(cand)) c(
      "The TOC items that may be this sheet's (candidates; the TOC can hold mistakes):",
      vapply(seq_len(nrow(cand)), function(j) {
        r <- toc[cand$i[j], , drop = FALSE]
        # (a close candidate another sheet fits better: that sheet's)
        first <- strsplit(.na_or(r$sheets %||% NA, ""), " | ", fixed = TRUE)[[1L]][1L]
        said <- grepl("its number is written in the sheet|the sheet name has its number",
                      cand$clues[j])
        else_where <- if (!said && !is.na(first) && !identical(first, sheet)) {
          sprintf("; sheet '%s' fits it better: write it only if this sheet is its shell", first)
        }
        paste0("- ", r$output_id, " (", cand$clues[j], ")", else_where,
               if (!is.na(r$title)) paste0("; title: ", r$title),
               if (!is.na(r$population %||% NA)) paste0("; population: ", r$population),
               if (!is.na(r$info %||% NA)) paste0("; ", r$info))
      }, ""), "")
    else c("No TOC item was found for this sheet: read the reports from the sheet alone.", ""),
    .ai_sheet_numbers_lines(x, sheet),
    paste(
      "Read the reports from the sheet first: the numbers, titles, populations, conditions,",
      "layout and variables written in it; then its sheet name; then the TOC candidates.",
      "The TOC and the sheet names can hold mistakes: choose the candidate that fits the sheet,",
      "or decide none does, and write why -- and any disagreement between the sheet, its name",
      "and the TOC -- as an assumption with its question."),
    paste(
      "The sheet can make several reports (parts, phases, subgroups, a parameter or a visit",
      "each), and one TOC item can be made by several sheets (a part a sheet): a report file",
      "for each report. Its id: the number the sheet writes for it; else the chosen TOC item's",
      "number; else that number with `.a`, `.b` ... in order (the last resort)."),
    "")
}

# The request of the TOC items no sheet chose: their rows, and what to do
.ai_noshell_lines <- function(x, ids) {
  toc <- x$toc
  r <- toc[match(ids, toc$output_id), , drop = FALSE]
  c("These TOC items were found in no shell sheet (no sheet's number, name or title fits them).",
    "Write each from its TOC row (and the shells of reports like it) when you can, or write no",
    "report file for it and say why as an assumption with its question.",
    "",
    vapply(seq_len(nrow(r)), function(k) paste0(
      "- ", r$output_id[k],
      if (!is.na(r$title[k])) paste0("; title: ", r$title[k]),
      if (!is.na(r$population[k] %||% NA)) paste0("; population: ", r$population[k]),
      if (!is.na(r$info[k] %||% NA)) paste0("; ", r$info[k])), ""),
    "")
}

# The TOC items' rows (no shell sheet chose them): the TOC's header rows
# and theirs, the workbook's text form
.ai_noshell_text <- function(x, ids) {
  toc <- x$toc
  i <- match(ids, toc$output_id)
  parts <- list()
  for (ts in names(x$roles)[x$roles == "toc"]) {
    d <- x$cells[x$cells$sheet == ts, , drop = FALSE]
    first <- x$first_rows[[ts]] %||% Inf
    parts[[length(parts) + 1L]] <- d[d$row < first | d$row %in% toc$row[i][toc$sheet[i] == ts], , drop = FALSE]
  }
  keep <- do.call(rbind, c(list(.xl_no_cells()), parts))
  .ai_shell_join(x$info, list(cells = keep, outputs = ids))
}

# The plan of a workbook's requests, shell sheets first: the tables'
# batches (`ub`, named by request: a sheet a request with `size` 1, named
# after it; else `size` sheets of a family, "<family>-<k>"), the figure
# sheets (`figs`: a request each), the TOC items no sheet chose
# (`noshell`), the names already taken kept clear of
.ai_sheet_plan <- function(x, size, types, families = NULL, taken = character()) {
  toc <- x$toc
  fam_re <- c(families, .ai_families)
  units <- list()
  for (j in seq_len(nrow(x$shells))) {
    s <- x$shells$sheet[j]
    cc <- x$cand[[s]]
    kind <- if (nrow(cc)) toc$type[cc$i[1L]] else NA_character_
    if (is.na(kind)) {
      z <- .ai_ids_in(.na_or(x$shells$numbers_in_sheet[j], ""))[[1L]]
      kind <- if (nrow(z)) z$type[1L] else NA_character_
    }
    if (!is.na(kind) && !kind %in% types) next
    txt <- paste(c(x$shells$title_in_sheet[j], toc$title[cc$i]), collapse = " ")
    hit <- names(fam_re)[vapply(fam_re, function(re) grepl(re, txt, perl = TRUE), NA)]
    # the reports it is asked for: the TOC items whose number it writes or
    # its name has (a part a sheet too); the other candidates are for the
    # AI's reference only, in the text; none: the AI decides
    said <- grepl("its number is written in the sheet|the sheet name has its number", cc$clues)
    own <- cc$i[said]
    # (the person's correction of the sheet's numbers, when given or saved:
    # .ai_sheet_numbers_apply())
    ids <- if (!is.null(x$sheet_ids[[s]])) x$sheet_ids[[s]] else toc$output_id[own]
    units[[length(units) + 1L]] <- list(sheet = s, ids = ids,
                                        kind = .na_or(kind, "table"),
                                        family = if (identical(kind, "listing")) "listing" else
                                          if (length(hit)) hit[1L] else "other")
  }
  tab <- Filter(function(u) !identical(u$kind, "figure"), units)
  figs <- Filter(function(u) identical(u$kind, "figure"), units)
  next_name <- function(f, used) {
    k <- 1L
    while (paste0(f, "-", k) %in% used) k <- k + 1L
    paste0(f, "-", k)
  }
  ub <- list()
  if (isTRUE(as.integer(size) == 1L)) {
    nm <- make.unique(c(taken, vapply(tab, function(u) .ai_file_id(u$sheet), "")), sep = "-")
    nm <- nm[length(taken) + seq_along(tab)]
    for (k in seq_along(tab)) ub[[nm[k]]] <- list(tab[[k]])
  } else {
    fams <- vapply(tab, `[[`, "", "family")
    for (f in unique(fams)) {
      us <- tab[fams == f]
      g <- (seq_along(us) - 1L) %/% as.integer(size)
      for (k in unique(g)) ub[[next_name(f, c(taken, names(ub)))]] <- us[g == k]
    }
  }
  # (a figure a request: named after its sheet with `size` 1, else figure-<k>)
  fig_names <- if (isTRUE(as.integer(size) == 1L)) {
    make.unique(c(taken, names(ub), vapply(figs, function(u) .ai_file_id(u$sheet), "")),
                sep = "-")[length(taken) + length(ub) + seq_along(figs)]
  } else {
    fn <- character()
    for (k in seq_along(figs)) fn[k] <- next_name("figure", c(taken, names(ub), fn))
    fn
  }
  names(figs) <- fig_names
  noshell <- toc$output_id[is.na(toc$sheets) & (is.na(toc$type) | toc$type %in% types)]
  # (a TOC item the person made a sheet's report is that sheet's)
  noshell <- setdiff(noshell, c(unlist(x$sheet_ids, use.names = FALSE), x$sheet_toc_ids))
  noshell_name <- if (length(noshell)) {
    make.unique(c(taken, names(ub), fig_names, "no-shell"), sep = "-")[
      length(taken) + length(ub) + length(fig_names) + 1L]
  }
  list(ub = ub, figs = figs, noshell = noshell, noshell_name = noshell_name)
}

# The plan as the dry run's requests table
.ai_sheet_plan_table <- function(plan) {
  row <- function(request, type, family, outputs, sheets) {
    data.frame(request = request, type = type, family = family,
               outputs = paste(outputs, collapse = ", "), n = length(outputs),
               chars = NA_integer_, file = NA_character_,
               sheets = paste(sheets, collapse = " | "), stringsAsFactors = FALSE)
  }
  rows <- c(
    lapply(names(plan$ub), function(nm) {
      us <- plan$ub[[nm]]
      row(nm, "table", us[[1L]]$family, unique(unlist(lapply(us, `[[`, "ids"))),
          vapply(us, `[[`, "", "sheet"))
    }),
    lapply(names(plan$figs), function(nm) {
      u <- plan$figs[[nm]]
      row(nm, "figure", "figure", u$ids[seq_len(min(1L, length(u$ids)))], u$sheet)
    }),
    if (length(plan$noshell)) list(row(plan$noshell_name, "table", "other", plan$noshell, character())))
  empty <- data.frame(request = character(), type = character(), family = character(),
                      outputs = character(), n = integer(), chars = integer(),
                      file = character(), sheets = character(), stringsAsFactors = FALSE)
  do.call(rbind, c(list(empty), rows))
}
