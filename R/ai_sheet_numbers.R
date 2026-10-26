# The report numbers each shell sheet writes (or its name has), and which
# of them the sheet's request makes: the rule's choice, the person's
# corrections over it (given, or saved in the study as
# ai/sheet_numbers.csv), applied to the workbook read before the requests
# are planned.

# The rule's numbers: a row a report number a shell sheet writes or its
# name has -- `sheet`, `number` (as written), `title`, `toc_id` (the TOC
# item it is, NA none), `make` (the rule makes it the sheet's report: its
# number written in the sheet or in its name, and a TOC item's), `source`
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
  out
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
    id <- ifelse(is.na(d$toc_id), trimws(.ai_half_width(d$number)), d$toc_id)
    off <- id[d$make %in% FALSE]
    on <- id[d$make %in% TRUE]
    ids[[s]] <- unique(c(setdiff(rule_ids, off), on))
  }
  x$sheet_numbers <- tab
  x$sheet_ids <- ids
  x
}

# A sheet's request lines of the person's correction: the numbers it makes
# and those it only refers to (none when the rule stands)
.ai_sheet_numbers_lines <- function(x, sheet) {
  d <- x$sheet_numbers
  if (is.null(d)) return(NULL)
  d <- d[d$sheet == sheet & d$source != "rule", , drop = FALSE]
  if (!nrow(d)) return(NULL)
  ids <- x$sheet_ids[[sheet]] %||% character()
  ref <- d$number[d$make %in% FALSE]
  c("The person checked this sheet's report numbers:",
    if (length(ids)) sprintf("- its reports: %s (write a report file for each);",
                             paste(ids, collapse = ", "))
    else "- it makes none of the numbers it writes;",
    if (length(ref)) sprintf("- reference only, not this sheet's reports: %s (write no report file for them from this sheet).",
                             paste(ref, collapse = ", ")),
    if (any(d$make & is.na(d$toc_id))) sprintf(
      "- no TOC item has %s: a report all the same, its id the number as the sheet writes it.",
      paste(d$number[d$make & is.na(d$toc_id)], collapse = ", ")),
    "")
}
