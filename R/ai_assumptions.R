# ============================================================================
#  The AI's assumptions, their ids and the person's answers to them (the
#  review loop): ai/assumption_ids.csv, ai/assumption_answers.csv
# ----------------------------------------------------------------------------
#  Each assumption of spec/_assumptions.yml is given an id, `A<n>`, kept
#  across collections: its key is its report, its `where` and its `item`
#  (the item compared without case, spaces or punctuation).  Each is also
#  given a kind, `K<n>`: the assumptions that ask the same question (as
#  ai/questions.md groups them), or, with no question, that say the same
#  item and assumption.  ai/assumption_ids.csv keeps the key of each id.
#
#  The person answers an assumption -- `ok`, or `wrong` with the right
#  content -- for its report (its `A<n>`) or for every report of its kind
#  (its `K<n>`), in ai/assumption_answers.csv (a screen writes it, or
#  tfl_ai_assumption_answers_write()).  tfl_ai_corrections() hands the
#  wrong ones to the agent (ai/corrections.md); the agent applies them and
#  marks the assumption `status: fixed` in spec/_assumptions.yml.
#
#    tfl_ai_assumptions()                 one row an assumption, its answer
#    tfl_ai_assumption_answers_read()     the person's answers
#    tfl_ai_assumption_answers_write()    an answer added, changed, removed
# ============================================================================

.ai_asm_ids_file <- function(d) file.path(d, "ai", "assumption_ids.csv")
.ai_asm_answers_file <- function(d) file.path(d, "ai", "assumption_answers.csv")
.ai_asm_answer_cols <- c("id", "scope", "output_id", "verdict", "correct")

# Text compared without case, spaces or punctuation (as ai/questions.md
# groups its questions); NA as ""
.ai_asm_norm <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  tolower(trimws(gsub("[[:space:][:punct:]]+", " ", x)))
}

.ai_asm_trim <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}

# An assumption's key: report | where | item; a second one of the same key
# in the file `#2`
.ai_asm_keys <- function(a) {
  k <- paste(.ai_asm_trim(a$output_id), .ai_asm_trim(a$where), .ai_asm_norm(a$item), sep = "|")
  n <- stats::ave(seq_along(k), k, FUN = seq_along)
  ifelse(n > 1L, paste0(k, "#", n), k)
}

# An assumption's kind's key: its question, else its item and assumption
.ai_asm_kind_keys <- function(a) {
  q <- .ai_asm_norm(a$question)
  ifelse(nzchar(q), paste0("q|", q),
         paste0("a|", .ai_asm_norm(a$item), "|", .ai_asm_norm(a$assumption)))
}

.ai_asm_ids_read <- function(d) {
  none <- data.frame(type = character(), key = character(), id = character(),
                     stringsAsFactors = FALSE)
  f <- .ai_asm_ids_file(d)
  if (!file.exists(f)) return(none)
  r <- tryCatch(utils::read.csv(f, colClasses = "character", na.strings = character(),
                                fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE),
                error = function(e) NULL)
  if (is.null(r) || !all(names(none) %in% names(r))) return(none)
  r[names(none)]
}

# The assumptions given their id (by their key) and kind (by their kind's
# key; a row the agent fixed keeps the kind it had, its text being the
# person's now); the keys new to ai/assumption_ids.csv added to it
.ai_asm_number <- function(a, d) {
  if (!nrow(a)) {
    a$id <- character()
    a$kind <- character()
    return(a)
  }
  reg <- .ai_asm_ids_read(d)
  n0 <- nrow(reg)
  give <- function(type, keys) {
    have <- reg$key[reg$type == type]
    new <- setdiff(unique(keys), have)
    if (length(new)) {
      num <- suppressWarnings(as.integer(sub("^[A-Z]", "", reg$id[reg$type == type])))
      top <- max(c(0L, num), na.rm = TRUE)
      reg <<- rbind(reg, data.frame(type = type, key = new, id = paste0(type, top + seq_along(new)),
                                    stringsAsFactors = FALSE))
    }
    at <- reg$type == type
    reg$id[at][match(keys, reg$key[at])]
  }
  a$id <- give("A", .ai_asm_keys(a))
  had <- if (is.null(a$kind)) rep(NA_character_, nrow(a)) else as.character(a$kind)
  fixed <- if (is.null(a$status)) rep(FALSE, nrow(a)) else a$status %in% "fixed"
  keep <- fixed & !is.na(had) & grepl("^K[0-9]+$", had)
  kind <- had
  if (any(!keep)) kind[!keep] <- give("K", .ai_asm_kind_keys(a[!keep, , drop = FALSE]))
  a$kind <- kind
  if (nrow(reg) > n0) {
    dir.create(dirname(.ai_asm_ids_file(d)), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(reg, .ai_asm_ids_file(d), row.names = FALSE, fileEncoding = "UTF-8")
  }
  a
}

# spec/_assumptions.yml written from its rows: each a mapping, its id and
# kind first, an empty field left out; the path
.ai_asm_write <- function(d, rows) {
  f <- file.path(d, "spec", "_assumptions.yml")
  ord <- c("id", "kind", "output_id", "where", "item", "assumption", "value", "question",
           "status", "note")
  ord <- c(intersect(ord, names(rows)), setdiff(names(rows), ord))
  lst <- lapply(seq_len(nrow(rows)), function(i) {
    r <- as.list(rows[i, ord, drop = FALSE])
    r[!vapply(r, function(z) is.null(z) || is.na(z[1L]), NA)]
  })
  .ai_write_text(yaml::as.yaml(lst, indent.mapping.sequence = TRUE), f)
  f
}

# A report's assumptions from its answer (`new`), collected again: one the
# agent fixed (`old`, `status: fixed`, the same key) stays as the agent
# left it -- the answer may still say the old text -- with a note when the
# answer's text differs.  The rows, with the columns of the file's frame.
.ai_asm_keep_fixed <- function(old, new) {
  cols <- names(old)
  for (k in setdiff(cols, names(new))) new[[k]] <- NA_character_
  new <- new[cols]
  fx <- old[old$status %in% "fixed", , drop = FALSE]
  if (!nrow(fx) || !nrow(new)) return(new)
  at <- match(.ai_asm_keys(new), .ai_asm_keys(old))
  for (i in which(!is.na(at) & old$status[at] %in% "fixed")) {
    r <- old[at[i], , drop = FALSE]
    r$note <- if (.ai_asm_norm(new$assumption[i]) == .ai_asm_norm(r$assumption)) NA_character_ else
      sprintf("collected again: the answer still says \"%s\"; kept as the agent fixed it",
              .ai_asm_trim(new$assumption[i]))
    new[i, ] <- r
  }
  new
}

# spec/_assumptions.yml with every row's id and kind (written back when one
# was new or changed): the frame with `where`, `id`, `kind`, `status`, `note`
.ai_asm_stamp <- function(d) {
  a <- .ai_folder_assumptions(d, all = TRUE)
  names(a)[names(a) == "sheet"] <- "where"
  if (!nrow(a)) return(a)
  b <- .ai_asm_number(a, d)
  if (!identical(b$id, a$id) || !identical(b$kind, a$kind)) .ai_asm_write(d, b)
  b
}

# ---- the person's answers: ai/assumption_answers.csv ------------------------------

# The answers file's rows, cleaned: each column there, trimmed, "" as NA,
# the verdict in lower case (a `correct` with no verdict: `wrong`)
.ai_asm_answers_clean <- function(x, fn) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  if (!"id" %in% names(x)) .ard_stop(sprintf("%s: the answers have a column `id` (A<n> or K<n>).", fn))
  for (k in .ai_asm_answer_cols) {
    v <- if (k %in% names(x)) as.character(x[[k]]) else rep(NA_character_, nrow(x))
    v <- trimws(gsub("[\r\n]+", " ", v))
    v[!is.na(v) & !nzchar(v)] <- NA_character_
    x[[k]] <- v
  }
  x <- x[.ai_asm_answer_cols]
  x$id <- toupper(x$id)
  x$scope <- tolower(x$scope)
  x$verdict <- tolower(x$verdict)
  x$verdict[is.na(x$verdict) & !is.na(x$correct)] <- "wrong"
  if (anyNA(x$id) || !all(grepl("^[AK][0-9]+$", x$id))) {
    bad <- x$id[is.na(x$id) | !grepl("^[AK][0-9]+$", x$id)]
    .ard_stop(sprintf("%s: an id is A<n> (one report's assumption) or K<n> (every report of a kind), not %s.",
                      fn, paste(sQuote(ifelse(is.na(bad), "(empty)", bad), FALSE), collapse = ", ")))
  }
  bad <- !is.na(x$verdict) & !x$verdict %in% c("ok", "wrong")
  if (any(bad)) {
    .ard_stop(sprintf("%s: the verdict of %s is `ok` or `wrong`, not %s.", fn,
                      paste(x$id[bad], collapse = ", "),
                      paste(sQuote(unique(x$verdict[bad]), FALSE), collapse = ", ")))
  }
  bad <- x$verdict %in% "wrong" & is.na(x$correct)
  if (any(bad)) {
    .ard_stop(sprintf("%s: %s %s `wrong` with no `correct`: write the right content.", fn,
                      paste(x$id[bad], collapse = ", "), if (sum(bad) > 1L) "are" else "is"))
  }
  bad <- !is.na(x$scope) & !x$scope %in% c("report", "all")
  if (any(bad)) {
    .ard_stop(sprintf("%s: the scope of %s is `report` (an A<n>) or `all` (a K<n>).", fn,
                      paste(x$id[bad], collapse = ", ")))
  }
  bad <- (startsWith(x$id, "A") & x$scope %in% "all") | (startsWith(x$id, "K") & x$scope %in% "report")
  if (any(bad)) {
    .ard_stop(sprintf(paste0("%s: %s: an A<n> is answered for its report (scope `report`), a K<n> for ",
                             "every report of its kind (scope `all`; tfl_ai_assumptions()$kind)."),
                      fn, paste(x$id[bad], collapse = ", ")))
  }
  x$scope <- ifelse(startsWith(x$id, "A"), "report", "all")
  x$output_id[x$scope == "all"] <- NA_character_
  rownames(x) <- NULL
  x
}

#' The person's answers to the AI's assumptions
#'
#' Each assumption the AI wrote (`spec/_assumptions.yml`) has an id,
#' `A<n>`, and a kind, `K<n>` ([tfl_ai_assumptions()]).  The person says
#' an assumption is right (`ok`) or `wrong` and what is right instead
#' (`correct`), for its report (`id` `A<n>`, `scope` `report`) or for
#' every report of its kind (`id` `K<n>`, `scope` `all`): an answer to an
#' `A<n>` wins over its kind's.  The answers are kept in
#' `ai/assumption_answers.csv` (UTF-8), which a screen writes or
#' `tfl_ai_assumption_answers_write()` does; [tfl_ai_corrections()] then
#' hands the wrong ones to the agent.
#'
#' The file: one row an answer, the columns `id`, `scope` (`report` /
#' `all`), `output_id` (the report, for scope `report`), `verdict` (`ok` /
#' `wrong`) and `correct` (the right content, for `wrong`).  A row with
#' `correct` and no verdict is `wrong`.
#'
#' `tfl_ai_assumption_answers_write()` adds the rows of `x` to the file: an
#' id answered there already is answered anew (`overwrite = TRUE`), and a
#' row with neither `verdict` nor `correct` removes that id's answer (the
#' assumption is open again).  An unknown id, a verdict not `ok` / `wrong`
#' or `wrong` with no `correct` is an error, and nothing is written.  An
#' answer completes a question of `ai/questions.md` when every assumption
#' of it is answered: its `- Answer:` is written from them (`A3, A7: <the
#' right content>`, `A4: confirmed`), unless the person has written one
#' there.
#'
#' @param study_dir The study folder.
#' @param x The answers: a data frame with `id` and any of `scope`,
#'   `output_id`, `verdict`, `correct` (a missing one is empty; `scope`
#'   and `output_id` are filled from the id).
#' @param overwrite `TRUE` (default) to answer anew an id answered already;
#'   `FALSE` makes that an error.
#' @return `tfl_ai_assumption_answers_read()`: a data frame of the columns
#'   above, one row an answer (none: no row), or `NULL` when the study has
#'   no `ai/assumption_answers.csv`.  `tfl_ai_assumption_answers_write()`:
#'   the file's path, invisibly.
#' @seealso [tfl_ai_assumptions()], [tfl_ai_corrections()]
#' @export
tfl_ai_assumption_answers_read <- function(study_dir) {
  fn <- "tfl_ai_assumption_answers_read()"
  f <- .ai_asm_answers_file(study_dir)
  if (!file.exists(f)) return(NULL)
  x <- tryCatch(utils::read.csv(f, colClasses = "character", na.strings = "",
                                fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE),
                error = function(e) .ard_stop(sprintf("%s: %s could not be read: %s", fn,
                                                      "ai/assumption_answers.csv",
                                                      conditionMessage(e))))
  x <- .ai_asm_answers_clean(x, fn)
  # (a row with neither verdict nor correct: no answer; the last row of an id wins)
  x <- x[!is.na(x$verdict), , drop = FALSE]
  x <- x[!duplicated(x$id, fromLast = TRUE), , drop = FALSE]
  rownames(x) <- NULL
  x
}

#' @rdname tfl_ai_assumption_answers_read
#' @export
tfl_ai_assumption_answers_write <- function(study_dir, x, overwrite = TRUE) { # nolint
  fn <- "tfl_ai_assumption_answers_write()"
  if (!is.data.frame(x)) .ard_stop(sprintf("%s: `x` is a data frame (id, scope, output_id, verdict, correct).", fn))
  x <- .ai_asm_answers_clean(x, fn)
  if (any(duplicated(x$id))) {
    .ard_stop(sprintf("%s: %s answered twice in `x`.", fn, paste(unique(x$id[duplicated(x$id)]),
                                                                 collapse = ", ")))
  }
  a <- .ai_asm_stamp(study_dir)
  known <- unique(c(a$id, a$kind))
  bad <- setdiff(x$id, known)
  if (length(bad)) {
    .ard_stop(sprintf("%s: no assumption %s in spec/_assumptions.yml (see tfl_ai_assumptions()).",
                      fn, paste(bad, collapse = ", ")))
  }
  own <- x$scope == "report"
  rep_of <- a$output_id[match(x$id, a$id)]
  bad <- own & !is.na(x$output_id) & !is.na(rep_of) & x$output_id != rep_of
  if (any(bad)) {
    .ard_stop(sprintf("%s: %s is an assumption of %s, not %s.", fn, x$id[bad][1L], rep_of[bad][1L],
                      x$output_id[bad][1L]))
  }
  x$output_id[own] <- rep_of[own]
  have <- tfl_ai_assumption_answers_read(study_dir)
  if (is.null(have)) have <- x[0L, , drop = FALSE]
  again <- intersect(x$id[!is.na(x$verdict)], have$id)
  if (length(again) && !isTRUE(overwrite)) {
    .ard_stop(sprintf("%s: %s answered already; pass `overwrite = TRUE` to answer anew.", fn,
                      paste(again, collapse = ", ")))
  }
  out <- rbind(have[!have$id %in% x$id, , drop = FALSE], x[!is.na(x$verdict), , drop = FALSE])
  f <- .ai_asm_answers_file(study_dir)
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(out, f, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  .ai_asm_questions_sync(study_dir)
  invisible(normalizePath(f, winslash = "/"))
}

# ---- the assumptions with their answers --------------------------------------------

# One row an assumption, its answer (its own, else its kind's: `via`) and
# status; the sections of ai/corrections.md for the status `done`
.ai_asm_table <- function(study_dir) {
  a <- .ai_asm_stamp(study_dir)
  ans <- tfl_ai_assumption_answers_read(study_dir)
  if (is.null(ans)) ans <- data.frame(id = character(), scope = character(), output_id = character(),
                                      verdict = character(), correct = character(),
                                      stringsAsFactors = FALSE)
  cf <- .ai_corrections_file(study_dir)
  sec <- .ai_corrections_read(if (file.exists(cf)) readLines(cf, warn = FALSE, encoding = "UTF-8")
                              else character())
  done <- paste0(sec$kind, sec$n)[sec$status %in% "done"]
  own <- match(a$id, ans$id)
  kin <- match(a$kind, ans$id)
  at <- ifelse(is.na(own), kin, own)
  a$via <- ans$id[at]
  a$verdict <- ans$verdict[at]
  a$correct <- ans$correct[at]
  a$n_reports <- vapply(a$kind, function(k) {
    length(unique(.ai_asm_trim(a$output_id[a$kind %in% k])))
  }, 1L, USE.NAMES = FALSE)
  a$status_answer <- ifelse(is.na(a$verdict),
                            ifelse(a$status %in% "fixed", "done", "open"),
                            ifelse(a$verdict == "ok", "confirmed",
                                   ifelse(a$status %in% "fixed" | a$via %in% done,
                                          "done", "answered")))
  a
}

#' The AI's assumptions, their ids and the person's answers
#'
#' What the AI took on a guess, from `spec/_assumptions.yml` (written as
#' its answers are collected, [tfl_ai_collect_answers()]), one row an
#' assumption, for a screen to show and the person to answer
#' ([tfl_ai_assumption_answers_write()]).
#'
#' Each assumption has an id, `A<n>`, kept across collections: the same
#' report, `where` and `item` (the item compared without case, spaces or
#' punctuation) keep their id, and a new one takes the next number
#' (`ai/assumption_ids.csv` keeps them: columns `type`, `key`, `id`).  Each
#' also has a kind, `K<n>`: the assumptions asking the same question (as
#' `ai/questions.md` groups them), or, with no question, saying the same
#' item and assumption -- an answer to the kind is an answer for every
#' report of it, unless a report's assumption has its own.
#'
#' @param study_dir The study folder.
#' @return A data frame, one row an assumption: `id`, `kind`, `n_reports`
#'   (the reports of its kind), `output_id`, `where`, `item`,
#'   `assumption` (one sentence: what the AI decided and how), `value`
#'   (the value it used, as text; `NA` when the item has none), `question`,
#'   `verdict` and `correct` (its answer, its own
#'   or its kind's; `NA` when none), `status`: `open` (no answer),
#'   `confirmed` (`ok`), `answered` (`wrong`, for the agent to apply) or
#'   `done` (the agent applied it: its section of `ai/corrections.md` is
#'   done, or the assumption is `status: fixed` in `spec/_assumptions.yml`).
#' @seealso [tfl_ai_assumption_answers_read()], [tfl_ai_corrections()],
#'   [tfl_ai_questions()]
#' @export
tfl_ai_assumptions <- function(study_dir) {
  a <- .ai_asm_table(study_dir)
  out <- data.frame(id = a$id, kind = a$kind, n_reports = as.integer(a$n_reports),
                    output_id = a$output_id, where = a$where, item = a$item,
                    assumption = a$assumption, value = a$value, question = a$question,
                    verdict = a$verdict, correct = a$correct, status = a$status_answer,
                    stringsAsFactors = FALSE)
  rownames(out) <- NULL
  out
}

# ---- ai/corrections.md: the wrong answers ------------------------------------------

# The sections the wrong answers ask for: an A<n> its report's, a K<n>
# every report of its kind (but those answered on their own); each its
# heading's id and key, its correct and its lines
.ai_asm_correction_items <- function(a, ans) {
  out <- list()
  w <- ans[ans$verdict %in% "wrong", , drop = FALSE]
  w <- w[order(substr(w$id, 1L, 1L), as.integer(substring(w$id, 2L))), , drop = FALSE]
  na_as <- function(x, y) if (is.na(x) || !nzchar(trimws(x))) y else x
  for (i in seq_len(nrow(w))) {
    id <- w$id[i]
    if (startsWith(id, "A")) {
      r <- a[a$id %in% id, , drop = FALSE]
      if (!nrow(r)) next
      key <- sprintf("assumption of %s: %s", na_as(r$output_id[1L], "(no report)"),
                     na_as(r$item[1L], "(no item)"))
      ln <- c(paste("- Was:", na_as(r$assumption[1L], "")), paste("- Correct:", w$correct[i]))
    } else {
      r <- a[a$kind %in% id & a$via %in% id, , drop = FALSE]
      if (!nrow(r)) next
      key <- sprintf("assumption of every report: %s", na_as(r$item[1L], "(no item)"))
      reps <- unique(stats::na.omit(r$output_id))
      ln <- c(paste("- Reports:", if (length(reps)) paste(reps, collapse = ", ") else "(none named)"),
              paste("- Was:", na_as(r$assumption[1L], "")), paste("- Correct:", w$correct[i]))
    }
    out[[id]] <- list(id = id, key = key, correct = w$correct[i], lines = ln)
  }
  out
}

# corrections.md's lines with the wrong answers: a section there and still
# open (or withdrawn) given the answer's correct content (open again); one
# there and open whose answer is gone or no longer wrong `withdrawn`; the
# new sections returned (their headings and lines) for the caller to add
.ai_asm_corrections <- function(study_dir, lines) {
  a <- .ai_asm_table(study_dir)
  ans <- tfl_ai_assumption_answers_read(study_dir)
  if (is.null(ans)) ans <- data.frame(id = character(), verdict = character(),
                                      correct = character(), stringsAsFactors = FALSE)
  items <- if (nrow(a)) .ai_asm_correction_items(a, ans) else list()
  have <- .ai_corrections_read(lines)
  have <- have[have$kind %in% c("A", "K"), , drop = FALSE]
  sid <- paste0(have$kind, have$n)
  set <- function(k, field, value) {
    b <- have$start[k]:have$end[k]
    j <- b[grep(sprintf("^- %s:", field), lines[b])]
    if (length(j)) lines[j[1L]] <<- sprintf("- %s: %s", field, value)
  }
  for (k in seq_len(nrow(have))) {
    it <- items[[sid[k]]]
    st <- have$status[k]
    if (is.null(it)) {
      if (st %in% "open") set(k, "Status", "withdrawn")
    } else if (st %in% c("open", "withdrawn")) {
      set(k, "Correct", it$correct)
      if (st %in% "withdrawn") set(k, "Status", "open")
    }
  }
  new <- character()
  add <- character()
  for (it in items[setdiff(names(items), sid)]) {
    h <- sprintf("## %s. %s", it$id, it$key)
    new <- c(new, h)
    add <- c(add, h, it$lines, "- Status: open", "")
  }
  list(lines = lines, new = new, add = add)
}

# ---- ai/questions.md: a question answered through its assumptions -------------------

# An answer written from the assumptions' answers (each line `A3, A7: ...`)
.ai_asm_auto_answer <- function(txt) {
  ln <- trimws(strsplit(txt, "\n", fixed = TRUE)[[1L]])
  ln <- ln[nzchar(ln)]
  length(ln) > 0L && all(grepl("^[AK][0-9]+(, ?[AK][0-9]+)*: ", ln))
}

# Each open question whose assumptions are all answered (ok, or wrong with
# the right content): its `- Answer:` written from those answers, one line
# a distinct answer -- unless the person wrote one there; one written so
# whose assumptions are no longer all answered emptied again
.ai_asm_questions_sync <- function(study_dir) {
  qf <- file.path(study_dir, "ai", "questions.md")
  if (!file.exists(qf) || !file.exists(.ai_asm_answers_file(study_dir))) return(invisible(NULL))
  a <- .ai_asm_table(study_dir)
  if (!nrow(a)) return(invisible(NULL))
  q <- .ai_questions_read(readLines(qf, warn = FALSE, encoding = "UTF-8"))
  put <- character()
  for (i in seq_len(nrow(q))) {
    if (!is.na(q$status[i]) && !q$status[i] %in% "open") next
    ids <- if (!is.na(q$assumptions[i])) trimws(strsplit(q$assumptions[i], ",", fixed = TRUE)[[1L]]) else
      a$id[.ai_asm_norm(a$question) == .ai_asm_norm(q$question[i])]
    ids <- ids[ids %in% a$id]
    if (!length(ids)) next
    ours <- is.na(q$answer[i]) || .ai_asm_auto_answer(q$answer[i])
    if (!ours) next
    r <- a[match(ids, a$id), , drop = FALSE]
    txt <- if (all(r$verdict %in% c("ok", "wrong"))) {
      lab <- ifelse(r$verdict == "ok", "confirmed", r$correct)
      g <- split(ids, factor(lab, levels = unique(lab)))
      paste(sprintf("%s: %s", vapply(g, paste, "", collapse = ", "), names(g)), collapse = "\n")
    } else ""
    now <- if (is.na(q$answer[i])) "" else
      paste(trimws(strsplit(q$answer[i], "\n", fixed = TRUE)[[1L]]), collapse = "\n")
    if (!identical(now, txt)) put[paste0("Q", q$n[i])] <- txt
  }
  if (length(put)) tfl_ai_answer_questions(study_dir, put)
  invisible(names(put))
}
