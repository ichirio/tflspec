# The report numbers each shell sheet writes (or its name has), and which
# of them the sheet's request makes: the rule's choice, the person's
# corrections over it (given, or saved in the study as
# ai/sheet_numbers.csv), applied to the workbook read before the requests
# are planned.

# The rule's numbers: a row a report number a shell sheet writes or its
# name has -- `sheet`, `number` (as written), `title`, `toc_id` (the TOC
# item it is, NA none), `make` (the rule makes it the sheet's report: its
# number written in the sheet or in its name, and a TOC item's), `source`
# ("rule"; "guess": a TOC item with no shell -- no sheet writes its number
# or has it in its name -- whose number, a part letter taken off the
# sheet's, sheets write is made the best sheet's, the others' a candidate:
# .ai_sheet_numbers_guess()), `reason` (a guess's: why that sheet, NA the
# rule's).  Its attribute "guess": the guesses' sheets ranked, an item at a
# time.
.ai_sheet_numbers_rule <- function(x) {
  empty <- data.frame(sheet = character(), number = character(), title = character(),
                      toc_id = character(), make = logical(), source = character(),
                      reason = character(), stringsAsFactors = FALSE)
  sh <- x$shells
  if (is.null(sh) || !nrow(sh)) return(empty)
  toc <- x$toc
  pre <- .ai_toc_pre(toc)
  rows <- lapply(seq_len(nrow(sh)), function(j) {
    s <- sh$sheet[j]
    written <- strsplit(.na_or(sh$numbers_in_sheet[j], ""), " | ", fixed = TRUE)[[1L]]
    written <- written[nzchar(written)]
    nm <- .ai_ids_in(s)[[1L]]
    num <- c(written, if (nrow(nm)) nm$id[1L])
    if (!length(num)) return(NULL)
    key <- .ai_id_key(num)
    # (one number once: the name's number when the sheet writes it too)
    dup <- duplicated(ifelse(is.na(key), num, key))
    num <- num[!dup]
    key <- key[!dup]
    cc <- x$cand[[s]]
    said <- grepl("its number is written in the sheet|the sheet name has its number", cc$clues)
    own <- cc$i[said]
    i <- vapply(key, function(k) .ai_sheet_number_toc(k, pre, prefer = own), 0L)
    # (a part of a TOC item: 15.2.4.2.b, the item 15.2.4.2)
    part <- is.na(i) & !is.na(key) & grepl("[A-Z]$", key)
    i[part] <- vapply(sub("[A-Z]$", "", key[part]), .ai_sheet_number_toc, 0L, pre = pre)
    # where the sheet has each number: its title rows, its name, else the
    # body only
    top_keys <- .ai_sheet_top_keys(x$cells[x$cells$sheet == s, , drop = FALSE])
    in_name <- if (nrow(nm)) vapply(key, function(k) .ai_id_same(k, nm$key[1L]), NA) else
      rep(FALSE, length(key))
    in_top <- vapply(key, function(k) any(vapply(top_keys, function(t) .ai_id_same(k, t), NA)), NA)
    where <- ifelse(in_top, "top", ifelse(in_name, "name", "body"))
    # the title: the TOC item's; else the sheet's, when it writes that
    # number alone
    title <- ifelse(is.na(i), NA_character_, toc$title[i])
    if (length(written) == 1L) {
      w <- is.na(i) & num == written & !is.na(sh$title_in_sheet[j])
      # (its number taken off)
      own_title <- trimws(sub("^[[:space:]:.-]+", "",
                              sub(written, "", sh$title_in_sheet[j], fixed = TRUE)))
      title[w] <- if (nzchar(own_title)) own_title else NA_character_
    }
    data.frame(sheet = s, number = num, title = title, toc_id = toc$output_id[i],
               make = !is.na(i) & i %in% own, source = "rule", reason = NA_character_,
               part = part, where = where, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, c(list(data.frame(empty, part = logical(), where = character(),
                                          stringsAsFactors = FALSE)), rows))
  rownames(out) <- NULL
  # a TOC item with no shell: the best sheet writing its number makes it;
  # the others, a candidate each
  said_any <- unlist(lapply(x$cand, function(cc) {
    cc$i[grepl("its number is written in the sheet|the sheet name has its number", cc$clues)]
  }), use.names = FALSE)
  no_shell <- toc$output_id[setdiff(seq_len(nrow(toc)), said_any)]
  g <- .ai_sheet_numbers_guess(out, intersect(no_shell, out$toc_id), sh$sheet)
  out <- g$tab
  out$part <- NULL
  out$where <- NULL
  attr(out, "guess") <- g$rank
  out
}

# 1st, 2nd, 3rd, 4th ...
.ai_ordinal <- function(k) {
  ifelse(k == 1L, "1st", ifelse(k == 2L, "2nd", ifelse(k == 3L, "3rd", paste0(k, "th"))))
}

# The report numbers in a sheet's title rows (keys): the rows before its
# table's header (the first row of two cells or more), of its first 12; a
# sheet with no such row, its first 3 rows
.ai_sheet_top_keys <- function(d, rows = 12L) {
  if (!nrow(d)) return(character())
  r <- sort(unique(d$row))
  n <- tabulate(match(d$row, r), length(r))
  head_at <- r[n >= 2L]
  top <- if (length(head_at)) r[r < head_at[1L]] else utils::head(r, 3L)
  top <- utils::head(top, rows)
  ids <- .ai_ids_in(d$text[d$row %in% top])
  unique(unlist(lapply(ids, `[[`, "key"), use.names = FALSE))
}

# The TOC items with no shell (`ids`) given to the sheets writing their
# number, by priority: an exact number before one a part letter differs;
# in the sheet's title rows or name before the body only; then the sheet
# whose own numbers are nearest the item's (the most levels in common
# from the first: the same section first); then the workbook's order.
# Ranked a number at a time (as written, its part letter with it: each
# part of the item its own report, 15.2.4.2.a and 15.2.4.2.b): its best
# sheet makes it, the others keep it as a candidate; each row's `reason`
# says why.  `tab` the rows (with `part` and `where`), `rank` (`toc_id`,
# `key` -- the number's, its kind's letter off --, `sheet`, `rank`, `why`
# -- how it matches --, `exact`).
.ai_sheet_numbers_guess <- function(tab, ids, order_of) {
  rank <- data.frame(toc_id = character(), key = character(), sheet = character(),
                     rank = integer(), why = character(), exact = logical(),
                     stringsAsFactors = FALSE)
  for (id in ids) {
    all_at <- which(tab$toc_id %in% id)
    tab$source[all_at] <- "guess"
    lv <- .ai_levels(.ai_id_key(id))
    keys <- sub("^[TFL]", "", .ai_id_key(tab$number[all_at]))
    for (pk in unique(keys)) {
      at <- all_at[keys %in% pk]
      # a row's tiers (lower better) and the reason it matches
      tier_m <- ifelse(tab$part[at], 2L, 1L)
      tier_w <- ifelse(tab$where[at] == "body", 2L, 1L)
      why <- paste0(ifelse(tab$part[at],
                           sprintf("part letter differs (%s for %s)", tab$number[at], id),
                           "exact number"), "; ",
                    c(top = "in the sheet's title rows", name = "in the sheet name",
                      body = "in the body only")[tab$where[at]])
      exact <- tier_m == 1L & tier_w == 1L
      why[!exact] <- paste0(why[!exact], "; not an exact match")
      # a sheet's best row
      sheets <- unique(tab$sheet[at])
      best <- vapply(sheets, function(s) {
        w <- which(tab$sheet[at] == s)
        w[order(tier_m[w], tier_w[w])][1L]
      }, 0L)
      # the sheet's own numbers: those the rule made it, its name's, else
      # those in its title rows -- not this item's
      near <- vapply(sheets, function(s) {
        mine <- tab$sheet == s & !(tab$toc_id %in% id)
        own <- tab$number[mine & tab$make & tab$source == "rule"]
        own <- c(own, tab$number[mine & tab$where == "name"])
        if (!length(own)) own <- tab$number[mine & tab$where == "top"]
        if (!length(own)) return(0L)
        max(vapply(own, function(n) {
          l <- .ai_levels(.ai_id_key(n))
          m <- min(length(l), length(lv))
          eq <- l[seq_len(m)] == lv[seq_len(m)]
          as.integer(if (all(eq)) m else which(!eq)[1L] - 1L)
        }, 0L))
      }, 0L)
      o <- order(tier_m[best], tier_w[best], -near, match(sheets, order_of))
      sheets <- sheets[o]
      best <- best[o]
      near <- near[o]
      chosen <- sheets[1L]
      for (k in seq_along(sheets)) {
        w <- which(tab$sheet[at] == sheets[k])
        tab$make[at[w]] <- k == 1L
        tab$reason[at[w]] <- if (k == 1L) {
          tie <- if (length(sheets) > 1L) {
            b2 <- best[2L]
            same_tier <- tier_m[best[1L]] == tier_m[b2] && tier_w[best[1L]] == tier_w[b2]
            paste0("; 1st choice of ", length(sheets), " sheets",
                   if (same_tier && near[1L] > near[2L]) "; its own number is nearest" else
                     if (same_tier) "; first in the workbook of those alike")
          }
          paste0(why[w], tie %||% "")
        } else {
          sprintf("%s choice; %s; chosen: sheet '%s'", .ai_ordinal(k), why[w], chosen)
        }
        rank <- rbind(rank, data.frame(toc_id = id, key = pk, sheet = sheets[k], rank = k,
                                       why = why[best[k]], exact = exact[best[k]],
                                       stringsAsFactors = FALSE))
      }
    }
  }
  list(tab = tab, rank = rank)
}

# A row's report id: the TOC item's number, or the number as the sheet
# writes it (no TOC item, or a part of one: 15.2.4.2.b)
.ai_sheet_number_id <- function(d) {
  num <- trimws(.ai_half_width(d$number))
  same <- !is.na(d$toc_id) & mapply(function(a, b) .ai_id_same(a, b),
                                    .ai_id_key(num), .ai_id_key(d$toc_id))
  ifelse(same, d$toc_id, num)
}

# The TOC item a number's key is (its number or its shell's), the sheet's
# own items first; NA none
.ai_sheet_number_toc <- function(k, pre, prefer = integer()) {
  if (is.na(k)) return(NA_integer_)
  hit <- which(.ai_same_v(k, pre$k, pre$kb, pre$kt) | .ai_same_v(k, pre$sk, pre$skb, pre$skt))
  if (!length(hit)) return(NA_integer_)
  p <- hit[hit %in% prefer]
  as.integer(if (length(p)) p[1L] else hit[1L])
}

# The person's mapping as rows `sheet`, `number`, `make`: a data frame of
# them (make NA: back to the rule), or a named list (a sheet: the numbers
# it makes; its other numbers reference only)
.ai_sheet_numbers_input <- function(numbers, rule, shells, fn) {
  if (is.list(numbers) && !is.data.frame(numbers)) {
    if (length(numbers) && (is.null(names(numbers)) || any(!nzchar(names(numbers))))) {
      .ard_stop(sprintf("%s: `numbers` as a list is named by sheet: list(<sheet> = c(<numbers to make>)).",
                        fn))
    }
    rows <- lapply(names(numbers), function(s) {
      mk <- as.character(unlist(numbers[[s]], use.names = FALSE))
      mk <- trimws(mk[!is.na(mk) & nzchar(trimws(mk))])
      have <- rule$number[rule$sheet == s]
      made <- vapply(have, function(n) any(.ai_sheet_number_same(n, mk)), NA)
      extra <- mk[!vapply(mk, function(n) any(.ai_sheet_number_same(n, have)), NA)]
      data.frame(sheet = rep(s, length(have) + length(extra)), number = c(have, extra),
                 make = c(made, rep(TRUE, length(extra))), stringsAsFactors = FALSE)
    })
    numbers <- do.call(rbind, c(list(data.frame(sheet = character(), number = character(),
                                                make = logical(), stringsAsFactors = FALSE)),
                                rows))
  }
  if (!is.data.frame(numbers)) {
    .ard_stop(sprintf("%s: `numbers` is a data frame (sheet, number, make) or a list named by sheet.",
                      fn))
  }
  miss <- setdiff(c("sheet", "number", "make"), names(numbers))
  if (length(miss)) {
    .ard_stop(sprintf("%s: `numbers` has no column %s; it needs sheet, number and make.", fn,
                      paste(miss, collapse = ", ")))
  }
  out <- data.frame(sheet = as.character(numbers[["sheet"]]),
                    number = trimws(as.character(numbers[["number"]])),
                    make = as.logical(numbers[["make"]]), stringsAsFactors = FALSE)
  bad <- setdiff(unique(out$sheet), shells)
  if (length(bad)) {
    .ard_stop(sprintf("%s: `numbers` names no shell sheet %s; the shell sheets are %s.", fn,
                      paste(sQuote(bad, FALSE), collapse = ", "),
                      paste(sQuote(shells, FALSE), collapse = ", ")))
  }
  if (anyNA(out$number) || any(!nzchar(out$number))) {
    .ard_stop(sprintf("%s: `numbers` has a row with no number.", fn))
  }
  out
}

# Is the number `a` one of `b`: as written, or the same report's key
.ai_sheet_number_same <- function(a, b) {
  if (!length(b)) return(logical())
  ka <- .ai_id_key(a)
  b == a | vapply(.ai_id_key(b), function(k) .ai_id_same(ka, k), NA)
}

# The saved mapping (ai/sheet_numbers.csv), NULL none; its rows of sheets
# this workbook has not left out (another workbook's)
.ai_sheet_numbers_read <- function(dir, shells) {
  f <- file.path(dir, "ai", "sheet_numbers.csv")
  if (!file.exists(f)) return(NULL)
  d <- utils::read.csv(f, colClasses = "character", fileEncoding = "UTF-8",
                       check.names = FALSE, na.strings = character())
  if (!all(c("sheet", "number", "make") %in% names(d))) return(NULL)
  d <- d[d$sheet %in% shells, , drop = FALSE]
  if (!nrow(d)) return(NULL)
  data.frame(sheet = d$sheet, number = d$number, make = as.logical(toupper(d$make)),
             stringsAsFactors = FALSE)
}

# The person's mapping in effect: the saved rows, the given over them (a
# row of the same sheet and number replaced; make NA, taken out: back to
# the rule), each with its `source`; NULL none
.ai_sheet_numbers_merge <- function(saved, given) {
  if (!is.null(saved)) saved$source <- rep("saved", nrow(saved))
  if (is.null(given)) return(saved)
  given$source <- rep("given", nrow(given))
  if (!is.null(saved) && nrow(saved)) {
    gone <- vapply(seq_len(nrow(saved)), function(r) {
      g <- given$number[given$sheet == saved$sheet[r]]
      any(.ai_sheet_number_same(saved$number[r], g))
    }, NA)
    given <- rbind(saved[!gone, , drop = FALSE], given)
  }
  rownames(given) <- NULL
  given
}

.ai_sheet_numbers_write <- function(dir, given) {
  f <- file.path(dir, "ai", "sheet_numbers.csv")
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  keep <- given[!is.na(given$make), c("sheet", "number", "make"), drop = FALSE]
  utils::write.csv(keep, f, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  f
}

# The workbook read with the person's mapping over the rule: its
# `sheet_numbers` (the table) and `sheet_ids` (a sheet corrected: the
# reports its request makes, in the rule's order then the added), the
# warning of a number made that no TOC item has
.ai_sheet_numbers_apply <- function(x, over, fn) {
  rule <- .ai_sheet_numbers_rule(x)
  tab <- rule
  attr(tab, "guess") <- NULL
  if (!is.null(over) && nrow(over)) {
    toc <- x$toc
    pre <- .ai_toc_pre(toc)
    # (a TOC item made a sheet's: no longer guessed into another)
    given_to <- function(k) {
      if (!isTRUE(tab$make[k]) || is.na(tab$toc_id[k])) return(invisible())
      g <- tab$source == "guess" & tab$toc_id %in% tab$toc_id[k] & tab$sheet != tab$sheet[k]
      tab$make[g] <<- FALSE
      tab$reason[g] <<- sprintf("the person made it in sheet '%s'", tab$sheet[k])
      invisible()
    }
    for (r in seq_len(nrow(over))) {
      s <- over$sheet[r]
      n <- over$number[r]
      source <- over$source[r]
      at <- which(tab$sheet == s)
      hit <- at[.ai_sheet_number_same(n, tab$number[at]) |
                  (!is.na(tab$toc_id[at]) & tab$toc_id[at] == n)]
      if (is.na(over$make[r])) next
      if (length(hit)) {
        tab$make[hit[1L]] <- over$make[r]
        tab$source[hit[1L]] <- source
        tab$reason[hit[1L]] <- "set by the person"
        given_to(hit[1L])
        next
      }
      # a number the rule did not read from the sheet (a TOC item that was
      # a candidate, a number the reading missed)
      i <- .ai_sheet_number_toc(.ai_id_key(n), pre)
      tab <- rbind(tab, data.frame(sheet = s, number = n,
                                   title = if (is.na(i)) NA_character_ else toc$title[i],
                                   toc_id = if (is.na(i)) NA_character_ else toc$output_id[i],
                                   make = over$make[r], source = source,
                                   reason = "set by the person", stringsAsFactors = FALSE))
      given_to(nrow(tab))
    }
    # (the rows in the sheets' order)
    tab <- tab[order(match(tab$sheet, x$shells$sheet), seq_len(nrow(tab))), , drop = FALSE]
    rownames(tab) <- NULL
  }
  # a number made that no TOC item has: a report of its sheet all the same
  odd <- tab[tab$make & is.na(tab$toc_id), , drop = FALSE]
  if (nrow(odd)) {
    warning(sprintf(paste0("%s: no TOC item has %s; made a report of its sheet's request as ",
                           "`numbers` says."), fn,
                    paste(sprintf("%s (sheet '%s')", odd$number, odd$sheet), collapse = ", ")),
            call. = FALSE)
  }
  ids <- list()
  for (s in unique(tab$sheet[tab$source != "rule"])) {
    d <- tab[tab$sheet == s, , drop = FALSE]
    cc <- x$cand[[s]]
    said <- grepl("its number is written in the sheet|the sheet name has its number", cc$clues)
    rule_ids <- x$toc$output_id[cc$i[said]]
    id <- .ai_sheet_number_id(d)
    off <- id[d$make %in% FALSE]
    on <- id[d$make %in% TRUE]
    ids[[s]] <- unique(c(setdiff(rule_ids, off), on))
  }
  x$sheet_numbers <- tab
  x$sheet_ids <- ids
  x$sheet_guess <- attr(rule, "guess")
  # the TOC items a sheet makes (out of the request of those with no shell)
  x$sheet_toc_ids <- unique(tab$toc_id[tab$make %in% TRUE & !is.na(tab$toc_id)])
  x$toc <- .ai_sheet_numbers_toc(x, tab, rule)
  x
}

# The TOC items' shells as the numbers say them, where not the rule's
# alone: an item made a report (given, or a guess) has the sheets making it
# first, its parts' order then the workbook's, the first its shell; an
# item the rule made
# and the person made in no sheet has no shell (the no-shell request's); a
# sheet the person said only refers to it is not one of its sheets
.ai_sheet_numbers_toc <- function(x, tab, rule) {
  toc <- x$toc
  if (is.null(toc$sheets) || !nrow(tab)) return(toc)
  info <- x$info %||% x$sheets
  order_of <- x$shells$sheet
  ids <- unique(tab$toc_id[tab$source != "rule" & !is.na(tab$toc_id)])
  made_by_rule <- unique(rule$toc_id[rule$make %in% TRUE & !is.na(rule$toc_id)])
  for (id in ids) {
    i <- match(id, toc$output_id)
    if (is.na(i)) next
    d <- tab[tab$toc_id %in% id, , drop = FALSE]
    # (its parts in their order, 15.2.4.2.a first; then the workbook's)
    m <- d[d$make %in% TRUE, , drop = FALSE]
    on <- unique(m$sheet[order(.ai_id_key(m$number), match(m$sheet, order_of))])
    off <- d$sheet[d$make %in% FALSE & d$source %in% c("given", "saved")]
    had <- strsplit(.na_or(toc$sheets[i], ""), " | ", fixed = TRUE)[[1L]]
    had <- setdiff(had[nzchar(had)], off)
    sheets <- if (!length(on) && id %in% made_by_rule) character() else unique(c(on, had))
    toc$sheets[i] <- if (length(sheets)) paste(sheets, collapse = " | ") else NA_character_
    toc$shell[i] <- if (length(sheets)) sheets[1L] else NA_character_
    if (!is.null(toc$book)) {
      bk <- info$book[match(toc$shell[i], info$sheet)]
      toc$book[i] <- if (is.na(bk)) info$book[1L] else bk
    }
  }
  toc
}

# A sheet's request lines of its numbers when not the rule's alone: the
# person's correction (the numbers it makes, those it only refers to) and
# the TOC items with no shell its numbers were taken for
.ai_sheet_numbers_lines <- function(x, sheet) {
  all <- x$sheet_numbers
  if (is.null(all)) return(NULL)
  d <- all[all$sheet == sheet & all$source != "rule", , drop = FALSE]
  if (!nrow(d)) return(NULL)
  ids <- x$sheet_ids[[sheet]] %||% character()
  p <- d[d$source %in% c("given", "saved"), , drop = FALSE]
  g <- d[d$source == "guess", , drop = FALSE]
  ref <- p$number[p$make %in% FALSE]
  rank <- x$sheet_guess
  guess_line <- function(k) {
    id <- g$toc_id[k]
    pk <- sub("^[TFL]", "", .ai_id_key(g$number[k]))
    rk <- if (is.null(rank)) NULL else rank[rank$toc_id %in% id & rank$key %in% pk, , drop = FALSE]
    rk <- rk[order(rk$rank), , drop = FALSE]
    others <- rk[rk$sheet != sheet, , drop = FALSE]
    if (isTRUE(g$make[k])) {
      exact <- !grepl("not an exact match", .na_or(g$reason[k], ""), fixed = TRUE)
      return(c(
        sprintf(paste0("- %s: the TOC item %s has no shell of its own; this sheet was chosen ",
                       "for it by priority (%s): a report of this sheet, its id %s."),
                g$number[k], id, .na_or(g$reason[k], "its number is written here"),
                .ai_sheet_number_id(g[k, , drop = FALSE])),
        if (!exact) paste0("  It is NOT an exact match: check that this sheet is its shell, ",
                           "and say so as an assumption."),
        if (nrow(others)) sprintf("  Other candidate sheets (references only): %s.",
                                  paste(sprintf("'%s' (%s choice: %s)", others$sheet,
                                                .ai_ordinal(others$rank), others$why),
                                        collapse = "; "))))
    }
    made <- all$toc_id %in% id & all$make %in% TRUE
    # (the sheet making this part, else any part of the item)
    same <- made & sub("^[TFL]", "", .ai_id_key(all$number)) %in% pk
    maker <- unique(all$sheet[if (any(same)) same else made])
    here <- rk[rk$sheet == sheet, , drop = FALSE]
    if (length(maker) && nrow(here) && maker[1L] %in% rk$sheet[rk$rank == 1L]) {
      return(sprintf(paste0("- %s: the TOC item %s: its shell is more likely sheet '%s' ",
                            "(1st choice: %s); this sheet is its %s choice (%s): a reference ",
                            "here, no report file for it from this sheet."),
                     g$number[k], id, maker[1L], rk$why[rk$rank == 1L][1L],
                     .ai_ordinal(here$rank[1L]), here$why[1L]))
    }
    if (length(maker)) {
      return(sprintf("- %s: the TOC item %s is made by sheet '%s': a reference here.",
                     g$number[k], id, maker[1L]))
    }
    others <- setdiff(unique(all$sheet[all$toc_id %in% id]), sheet)
    sprintf(paste0("- %s: the TOC item %s has no shell of its own; sheet(s) %s write its number ",
                   "too: write it from this sheet only if this sheet is its shell."),
            g$number[k], id, paste(sQuote(others, FALSE), collapse = ", "))
  }
  c(if (nrow(p)) c(
      "The person checked this sheet's report numbers:",
      if (length(ids)) sprintf("- its reports: %s (write a report file for each);",
                               paste(ids, collapse = ", "))
      else "- it makes none of the numbers it writes;",
      if (length(ref)) sprintf("- reference only, not this sheet's reports: %s (write no report file for them from this sheet).",
                               paste(ref, collapse = ", ")),
      if (any(p$make & is.na(p$toc_id))) sprintf(
        "- no TOC item has %s: a report all the same, its id the number as the sheet writes it.",
        paste(p$number[p$make & is.na(p$toc_id)], collapse = ", "))),
    if (nrow(g)) c("TOC items with no shell found that this sheet's numbers name:",
                   unlist(lapply(seq_len(nrow(g)), guess_line))),
    "")
}
