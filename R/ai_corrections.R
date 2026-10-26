# ============================================================================
#  The person's corrections for the agent, in one file (the review loop,
#  10/24): ai/corrections.md
# ----------------------------------------------------------------------------
#  What the person hands the agent for its next round, one section an item,
#  each with its `- Status:` as ai/questions.md has: the reports to add,
#  drop or change from their list (ai/expected_outputs.csv), their notes on
#  what a report should show, and the questions they have answered.  The
#  person then says only "the answers and corrections are written"; the
#  agent works through the open sections (AGENTS.md, round 2) and sets
#  each one done.  Written again, a section already there keeps its number
#  and its status; the same correction is never written twice.
# ============================================================================

.ai_corrections_file <- function(study_dir) file.path(study_dir, "ai", "corrections.md")

# The sections of the file: `## C<n>. <what>` or `## Q<n> (answered). ...`,
# each to the next heading -- its kind, number, key (its heading's text
# after the number), status and lines
.ai_corrections_read <- function(lines) {
  at <- grep("^## [CQAK][0-9]+", lines)
  none <- data.frame(kind = character(), n = integer(), key = character(),
                     status = character(), start = integer(), end = integer(),
                     stringsAsFactors = FALSE)
  if (!length(at)) return(none)
  end <- c(at[-1L] - 1L, length(lines))
  rows <- lapply(seq_along(at), function(i) {
    h <- lines[at[i]]
    b <- lines[at[i]:end[i]]
    j <- grep("^- Status:", b)
    data.frame(kind = substr(h, 4L, 4L),
               n = as.integer(sub("^## [CQAK]([0-9]+).*$", "\\1", h)),
               key = trimws(sub("^## [CQAK][0-9]+[.]?", "", h)),
               status = if (length(j)) tolower(trimws(sub("^- Status:", "", b[j[1L]]))) else NA_character_,
               start = at[i], end = end[i], stringsAsFactors = FALSE)
  })
  do.call(rbind, c(list(none), rows))
}

# The corrections the person's list says, one section each: its key (the
# heading's text) and its lines below the heading
.ai_corrections_items <- function(e) {
  out <- list()
  v <- function(x) length(x) == 1L && !is.na(x) && nzchar(trimws(x))
  item <- function(key, ...) {
    ln <- c(...)
    out[[length(out) + 1L]] <<- list(key = key, lines = ln[!is.na(ln)])
  }
  for (i in seq_len(nrow(e))) {
    r <- e[i, ]
    where <- paste(c(if (v(r$sheet)) paste("sheet", r$sheet),
                     if (v(r$toc_id)) paste("TOC", r$toc_id)), collapse = ", ")
    where <- if (nzchar(where)) sprintf(" (%s)", where) else ""
    id <- if (v(r$output_id)) r$output_id else "a new report"
    act <- r$action %|% ""
    if (identical(act, "add")) {
      item(sprintf("add %s%s", id, where),
           if (v(r$title)) paste("- Title:", r$title),
           if (v(r$population)) paste("- Population:", r$population),
           if (v(r$split)) paste("- Part:", r$split),
           if (v(r$note)) paste("- Note:", r$note))
      next
    }
    if (identical(act, "drop")) {
      item(sprintf("drop %s%s", id, where),
           if (v(r$reason)) paste("- Reason:", r$reason))
      next
    }
    if (identical(act, "change")) {
      # what changed: each field the person's `was` names, from it to now
      was <- .ai_was_fields(r$was)
      now <- list(output_id = r$output_id, title = r$title, population = r$population,
                  split = r$split)
      ch <- vapply(names(was), function(k) {
        sprintf("%s %s -> %s", k, was[[k]], now[[k]] %|% "(none)")
      }, "")
      item(sprintf("change %s%s", if (v(was$output_id)) was$output_id else id,
                   if (length(ch)) paste0(": ", paste(ch, collapse = "; ")) else ""),
           if (v(r$reason)) paste("- Reason:", r$reason))
    }
    if (v(r$note)) item(sprintf("note %s", id), paste("-", r$note))
  }
  out
}

# `title: Demographics | population: SAF` as a named list (the fields of a
# report the list has)
.ai_was_fields <- function(was) {
  if (is.na(was %|% NA) || !nzchar(trimws(was))) return(list())
  parts <- trimws(strsplit(was, "|", fixed = TRUE)[[1L]])
  parts <- parts[grepl(":", parts, fixed = TRUE)]
  k <- tolower(trimws(sub(":.*$", "", parts)))
  k[k == "id"] <- "output_id"
  vals <- trimws(sub("^[^:]*:", "", parts))
  keep <- k %in% c("output_id", "title", "population", "split")
  stats::setNames(as.list(vals[keep]), k[keep])
}

#' Write the person's corrections for the agent's next round
#'
#' Gathers what the person wants changed into one file, `ai/corrections.md`,
#' which the agent works through in its next round (round 2 of its
#' instructions, `AGENTS.md`): from the person's list of reports
#' ([tfl_ai_expected_write()]) each report to `add`, `drop` or `change`
#' and each `note` on what a report should show, and each question of
#' `ai/questions.md` the person has answered (still open).  One section an
#' item -- `## C<n>. add ...`, `## Q<n> (answered). ...` -- with `- Status:
#' open`, which the agent sets to `done` once it has done it.
#'
#' Each assumption of the AI the person answered `wrong`
#' ([tfl_ai_assumption_answers_write()]) is a section too: one report's
#' (`## A12. assumption of T-14-1-1: <item>`, with `- Was:` and
#' `- Correct:`) or every report of its kind's (`## K3. assumption of every
#' report: <item>`, with `- Reports:` too).  An answer `ok` needs no
#' section.  An answer changed while its section is open changes its
#' `- Correct:`; an answer removed (or no longer `wrong`) while its section
#' is open sets it `- Status: withdrawn`.  A question of `ai/questions.md`
#' whose assumptions are all answered is answered first from them
#' (unless the person has written its answer).
#'
#' Written again (after more corrections), the sections there are kept as
#' they are -- their number, their status, what the agent wrote -- and only
#' new ones are added: the same correction is never written twice.  The
#' person then tells the agent "the answers and corrections are written".
#'
#' @param study_dir The study folder.
#' @return The sections written this time (their headings), invisibly; the
#'   file is `ai/corrections.md`.
#' @seealso [tfl_ai_outputs()], [tfl_ai_questions()]
#' @export
tfl_ai_corrections <- function(study_dir = ".") {
  f <- .ai_corrections_file(study_dir)
  lines <- if (file.exists(f)) readLines(f, warn = FALSE, encoding = "UTF-8") else character()
  # a question answered through its assumptions' answers: its answer written first
  .ai_asm_questions_sync(study_dir)
  have <- .ai_corrections_read(lines)
  e <- tfl_ai_expected_read(study_dir)
  items <- if (is.null(e)) list() else .ai_corrections_items(e)
  # the questions answered, not yet decided
  qf <- file.path(study_dir, "ai", "questions.md")
  q <- if (file.exists(qf)) .ai_questions_read(readLines(qf, warn = FALSE, encoding = "UTF-8")) else
    .ai_questions_read(character())
  q <- q[q$status %in% "open" & !is.na(q$answer), , drop = FALSE]
  new <- character()
  add <- character()
  next_c <- max(c(0L, have$n[have$kind == "C"])) + 1L
  for (it in items) {
    if (it$key %in% have$key[have$kind == "C"] || it$key %in% sub("^## C[0-9]+[.] ", "", new)) next
    h <- sprintf("## C%d. %s", next_c, it$key)
    next_c <- next_c + 1L
    new <- c(new, h)
    add <- c(add, h, it$lines, "- Status: open", "")
  }
  for (i in seq_len(nrow(q))) {
    if (q$n[i] %in% have$n[have$kind == "Q"]) next
    h <- sprintf("## Q%d (answered). Reports: %s", q$n[i], q$reports[i] %|% "(none named)")
    new <- c(new, h)
    add <- c(add, h, paste("- Question:", q$question[i]),
             paste("- Answer:", gsub("\n", "\n  ", q$answer[i], fixed = TRUE)),
             "- Status: open", "")
  }
  # the assumptions the person answered wrong (ai/assumption_answers.csv)
  asm <- .ai_asm_corrections(study_dir, lines)
  lines <- asm$lines
  new <- c(new, asm$new)
  add <- c(add, asm$add)
  if (!length(lines)) {
    lines <- c("# Corrections for the agent",
               "",
               "What the person wants changed, one section each, from the list of reports",
               "(ai/expected_outputs.csv), the questions answered (ai/questions.md) and the",
               "assumptions answered wrong (ai/assumption_answers.csv).",
               "Tell the agent \"the answers and corrections are written\": it does each",
               "section with `- Status: open` and sets it to `- Status: done`.",
               "")
  }
  if (length(add)) {
    lines <- c(lines, if (length(lines) && nzchar(lines[length(lines)])) "",
               sprintf("<!-- written %s -->", format(Sys.time(), "%Y-%m-%d %H:%M")), "", add)
  }
  .ai_write_text(lines, f)
  invisible(new)
}
