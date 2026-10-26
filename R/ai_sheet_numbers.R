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
# sheet's, one sheet writes is made that sheet's; two sheets, a candidate
# of each)
.ai_sheet_numbers_rule <- function(x) {
  empty <- data.frame(sheet = character(), number = character(), title = character(),
                      toc_id = character(), make = logical(), source = character(),
                      stringsAsFactors = FALSE)
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
               make = !is.na(i) & i %in% own, source = "rule", stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, c(list(empty), rows))
  rownames(out) <- NULL
  # a TOC item with no shell: the sheet writing its number (the only one)
  # makes it; several sheets, a candidate of each
  said_any <- unlist(lapply(x$cand, function(cc) {
    cc$i[grepl("its number is written in the sheet|the sheet name has its number", cc$clues)]
  }), use.names = FALSE)
  no_shell <- toc$output_id[setdiff(seq_len(nrow(toc)), said_any)]
  for (id in intersect(no_shell, out$toc_id)) {
    at <- which(out$toc_id %in% id)
    out$make[at] <- length(unique(out$sheet[at])) == 1L
    out$source[at] <- "guess"
  }
  out
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
.ai_sheet_numbers_apply <- function(x, over, source, fn) {
  rule <- .ai_sheet_numbers_rule(x)
  tab <- rule
  if (!is.null(over) && nrow(over)) {
    toc <- x$toc
    pre <- .ai_toc_pre(toc)
    # (a TOC item made a sheet's: no longer guessed into another)
    given_to <- function(k) {
      if (!isTRUE(tab$make[k]) || is.na(tab$toc_id[k])) return(invisible())
      g <- tab$source == "guess" & tab$toc_id %in% tab$toc_id[k] & tab$sheet != tab$sheet[k]
      tab$make[g] <<- FALSE
      invisible()
    }
    for (r in seq_len(nrow(over))) {
      s <- over$sheet[r]
      n <- over$number[r]
      at <- which(tab$sheet == s)
      hit <- at[.ai_sheet_number_same(n, tab$number[at]) |
                  (!is.na(tab$toc_id[at]) & tab$toc_id[at] == n)]
      if (is.na(over$make[r])) next
      if (length(hit)) {
        tab$make[hit[1L]] <- over$make[r]
        tab$source[hit[1L]] <- source
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
                                   stringsAsFactors = FALSE))
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
  # the TOC items a sheet makes (out of the request of those with no shell)
  x$sheet_toc_ids <- unique(tab$toc_id[tab$make %in% TRUE & !is.na(tab$toc_id)])
  x$toc <- .ai_sheet_numbers_toc(x, tab, rule)
  x
}

# The TOC items' shells as the numbers say them, where not the rule's
# alone: an item made a report (given, or a guess) has the sheets making it
# first, in the workbook's order, the first its shell; an item the rule made
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
    on <- order_of[order_of %in% d$sheet[d$make %in% TRUE]]
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
  guess_line <- function(k) {
    id <- g$toc_id[k]
    if (isTRUE(g$make[k])) {
      return(sprintf(paste0("- %s: the TOC item %s has no shell of its own and this sheet alone ",
                            "writes its number: a report of this sheet, its id %s."),
                     g$number[k], id, .ai_sheet_number_id(g[k, , drop = FALSE])))
    }
    maker <- unique(all$sheet[all$toc_id %in% id & all$make %in% TRUE])
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
                   vapply(seq_len(nrow(g)), guess_line, "")),
    "")
}
