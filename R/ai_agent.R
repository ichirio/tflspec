# ============================================================================
#  tfl_ai_prepare(mode = "agent"): the study folder as a coding agent's work
# ----------------------------------------------------------------------------
#  A coding agent (GitHub Copilot's agent in VS Code, and the like) works
#  through every request of the study without stopping: it answers a
#  request into ai/answers/, runs run.R (tfl_ai_run(): collect, ARD, RTF,
#  ai/status.md) and repairs what the status says.  What it cannot settle
#  it takes a guess on and asks as an assumption's `question`; tfl_ai_run()
#  gathers the questions into ai/questions.md -- one question asked by
#  several reports once, the reports listed -- for the person to answer.
#  A second round reads the answers into ai/decisions.md and redoes the
#  reports they touch.  The instructions are AGENTS.md and
#  .github/copilot-instructions.md (the same text).
#
#    .ai_agent_files()   the instructions, run.R, questions / decisions
#    tfl_ai_run()        collect, build, status, questions
# ============================================================================

# The agent's files of a study folder: the instructions, run.R, and the
# questions / decisions files (never written over: the person's answers
# and decisions are in them).  The paths written.
.ai_agent_files <- function(dir, overwrite = FALSE) {
  src <- system.file("ai", "agent", package = "tflspec")
  rd <- function(f) paste(readLines(file.path(src, f), warn = FALSE, encoding = "UTF-8"),
                          collapse = "\n")
  out <- character()
  put <- function(to, text, again = FALSE) {
    f <- file.path(dir, to)
    if (file.exists(f) && !(again && isTRUE(overwrite))) return(invisible())
    .ai_write_text(text, f)
    out <<- c(out, f)
  }
  agents <- rd("AGENTS.md")
  put("AGENTS.md", agents, again = TRUE)
  put(file.path(".github", "copilot-instructions.md"), agents, again = TRUE)
  put("run.R", rd("run.R"), again = TRUE)
  put(file.path("ai", "questions.md"), .ai_questions_head())
  put(file.path("ai", "decisions.md"), rd("decisions.md"))
  out
}

# The lines a request starts with in agent mode: the steps, so a request
# read alone says them.
.ai_agent_request_head <- function(request) {
  c("<!-- tflspec: agent mode -->",
    paste0("> **For the agent** (AGENTS.md): answer this request with its one `yaml` block and ",
           "save the whole answer as `ai/answers/", request, ".md`; then run `Rscript run.R` and ",
           "read `ai/status.md`. Read `ai/decisions.md` first and follow it. Do not stop to ask: ",
           "take the likeliest choice and write it as an assumption with its `question`. Work from ",
           "`ai/adam_summary.md`; read the ADaM with R only to settle a question it does not."),
    "", "---", "")
}

#' Run the study's AI answers through: collect, build, status, questions
#'
#' What a coding agent runs after each answer it saves (`run.R` of
#' [tfl_ai_prepare()]`(mode = "agent")` calls it): [tfl_ai_collect_answers()]
#' (read, check, write the report files, a repair prompt for an answer with
#' errors), then each report written (`ok`, `warning`) built -- its ARD
#' ([tfl_build_ard()]) and its RTF (rtfreporter) into the study's
#' `output_path` -- one report's failure not stopping the others.  It writes
#' `ai/status.md` (each report's status, its RTF or why none, each
#' request's next step, the questions open) and gathers the answers'
#' questions (an assumption's `question`) into `ai/questions.md`: one
#' question once, with every report that asks it; a question there already
#' is kept as it is -- its answer, its status -- with the new reports added
#' to its list.  Nothing is sent anywhere.
#'
#' @param study_dir The study folder.
#' @param rtf `FALSE` to build the ARDs only (no rtfreporter).
#' @return A `tfl_ai_collected` (as [tfl_ai_collect_answers()]) whose
#'   `reports` also have `rtf` (the file made, else `NA`) and `rtf_problem`,
#'   and a `questions` data frame (`n`, `question`, `reports`, `status`,
#'   `answer`), invisibly.
#' @seealso [tfl_ai_prepare()] (`mode = "agent"`).
#' @export
tfl_ai_run <- function(study_dir = ".", rtf = TRUE) {
  fn <- "tfl_ai_run()"
  if (isTRUE(rtf) && !requireNamespace("rtfreporter", quietly = TRUE)) {
    .ard_stop(sprintf("%s: the RTF needs the rtfreporter package; or rtf = FALSE.", fn))
  }
  res <- tfl_ai_collect_answers(study_dir)
  r <- res$reports
  r$rtf <- NA_character_
  r$rtf_problem <- NA_character_
  todo <- which(r$status %in% c("ok", "warning") & !is.na(r$file))
  if (length(todo)) {
    old <- setwd(study_dir)
    on.exit(setwd(old), add = TRUE)
    st <- tryCatch(tfl_read_study_spec("."), error = function(e) e)
    outs <- if (!inherits(st, "error")) tryCatch(tfl_read_outputs("."), error = function(e) e)
    if (inherits(st, "error") || inherits(outs, "error")) {
      e <- if (inherits(st, "error")) st else outs
      r$rtf_problem[todo] <- paste("the study could not be read:", .ai_one_line(conditionMessage(e)))
    } else {
      out_dir <- st$study$settings$output_path %||% "output"
      for (i in todo) {
        id <- r$output_id[i]
        made <- tryCatch({
          o <- tfl_resolve_output(outs[[id]], st)
          ard <- suppressMessages(tfl_build_ard(st, dir = ".", output_id = id, save = FALSE))
          if (!is.null(ard$output_id)) ard <- ard[ard$output_id %in% id, , drop = FALSE]
          if (isTRUE(rtf)) {
            plan <- suppressMessages(tfl_content_run(o, ard))
            dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
            f <- file.path(out_dir, paste0(id, ".rtf"))
            suppressMessages(rtfreporter::generate_rtfreport(
              tfl_report(o, content = plan, study = st), f, overwrite = TRUE))
            gsub("\\\\", "/", f)
          } else NA_character_
        }, error = function(e) structure(.ai_one_line(conditionMessage(e)), class = "failed"))
        if (inherits(made, "failed")) {
          r$rtf_problem[i] <- unclass(made)
        } else {
          r$rtf[i] <- made
        }
      }
    }
  }
  res$reports <- r
  res$questions <- .ai_questions_update(study_dir, res$assumptions)
  .ai_write_text(.ai_status_text(res), file.path(study_dir, "ai", "status.md"))
  made <- sum(!is.na(r$rtf))
  open <- sum(res$questions$status %in% "open")
  message(sprintf("%d of %d reports have an RTF; %d question(s) open in ai/questions.md; see ai/status.md.",
                  made, nrow(r), open))
  invisible(res)
}

# ---- ai/status.md -----------------------------------------------------------------

.ai_status_text <- function(res) {
  r <- res$reports
  cell <- function(x) ifelse(is.na(x), "", gsub("|", "/", x, fixed = TRUE))
  q <- res$questions
  bad <- r[!is.na(r$rtf_problem), , drop = FALSE]
  nxt <- res$next_steps
  for (i in seq_len(nrow(bad))) {
    nxt[bad$request[i]] <- sprintf(paste0("the answer passed its checks but %s could not be built (%s): ",
                                          "fix the answer and run again"),
                                   bad$output_id[i], bad$rtf_problem[i])
  }
  c(sprintf("# Status (written by run.R, %s)", format(Sys.time(), "%Y-%m-%d %H:%M")),
    "",
    sprintf("%d of %d reports have an RTF. Questions: %d open, %d decided (ai/questions.md).",
            sum(!is.na(r$rtf)), nrow(r), sum(q$status %in% "open"), sum(q$status %in% "decided")),
    "",
    "| request | report | answer | RTF | the first problem |",
    "|---|---|---|---|---|",
    sprintf("| %s | %s | %s | %s | %s |", r$request, r$output_id, r$status,
            ifelse(is.na(r$rtf), "none", r$rtf), cell(ifelse(is.na(r$rtf_problem), r$first,
                                                              r$rtf_problem))),
    "",
    "## Next, each request",
    "",
    if (length(nxt)) sprintf("- %s: %s", names(nxt), nxt) else "- (no request)",
    if (length(res$stray)) c("", sprintf(paste0("Not read (no request of that name): %s -- ",
                                                "rename each to <request>.md."),
                                         paste(res$stray, collapse = ", "))))
}

# ---- ai/questions.md ----------------------------------------------------------------

.ai_questions_head <- function() {
  c("# Questions for the person",
    "",
    "The agent's open questions, one each, with the reports they touch.",
    "Write your answer after `- Answer:` (one line or more), then tell the agent",
    "\"the questions are answered\". `- Status: decided` is set by the agent once",
    "it has moved the answer to `ai/decisions.md` and redone the reports.",
    "")
}

# The questions of the file: one row a `## Q<n>. <question>` block.
.ai_questions_read <- function(lines) {
  at <- grep("^## Q[0-9]+[.] ", lines)
  none <- data.frame(n = integer(), question = character(), reports = character(),
                     status = character(), answer = character(), start = integer(),
                     end = integer(), stringsAsFactors = FALSE)
  if (!length(at)) return(none)
  end <- c(at[-1L] - 1L, length(lines))
  rows <- lapply(seq_along(at), function(i) {
    b <- lines[at[i]:end[i]]
    field <- function(k) {
      j <- grep(sprintf("^- %s:", k), b)
      if (!length(j)) return(NA_character_)
      trimws(sub(sprintf("^- %s:", k), "", b[j[1L]]))
    }
    ans <- grep("^- Answer:", b)
    answer <- if (length(ans)) {
      a <- c(sub("^- Answer:", "", b[ans[1L]]), b[-seq_len(ans[1L])])
      a <- trimws(paste(a, collapse = "\n"))
      if (nzchar(a)) a else NA_character_
    } else NA_character_
    data.frame(n = as.integer(sub("^## Q([0-9]+)[.] .*$", "\\1", b[1L])),
               question = trimws(sub("^## Q[0-9]+[.] ", "", b[1L])),
               reports = field("Reports"), status = tolower(field("Status") %||% NA_character_),
               answer = answer, start = at[i], end = end[i], stringsAsFactors = FALSE)
  })
  do.call(rbind, c(list(none), rows))
}

# The answers' questions into ai/questions.md: a new question added, one
# there already given the reports that ask it too; the rest as it is.
.ai_questions_update <- function(study_dir, assumptions) {
  f <- file.path(study_dir, "ai", "questions.md")
  lines <- if (file.exists(f)) readLines(f, warn = FALSE, encoding = "UTF-8") else .ai_questions_head()
  a <- assumptions
  a <- a[!is.na(a$question) & nzchar(trimws(a$question)), , drop = FALSE]
  key <- function(x) tolower(trimws(gsub("[[:space:][:punct:]]+", " ", x)))
  q <- .ai_questions_read(lines)
  changed <- FALSE
  if (nrow(a)) {
    groups <- split(seq_len(nrow(a)), key(a$question))
    groups <- groups[order(vapply(groups, min, 1L))]
    for (g in groups) {
      ids <- unique(stats::na.omit(a$output_id[g]))
      k <- match(key(a$question[g[1L]]), key(q$question))
      if (!is.na(k)) {
        have <- if (is.na(q$reports[k])) character() else
          trimws(strsplit(q$reports[k], ",", fixed = TRUE)[[1L]])
        have <- have[nzchar(have)]
        add <- setdiff(ids, have)
        if (!length(add)) next
        b <- q$start[k]:q$end[k]
        j <- b[grep("^- Reports:", lines[b])]
        line <- sprintf("- Reports: %s", paste(c(have, add), collapse = ", "))
        if (length(j)) lines[j[1L]] <- line else lines <- append(lines, line, after = q$start[k])
      } else {
        n <- if (nrow(q)) max(q$n) + 1L else 1L
        while (length(lines) && !nzchar(trimws(lines[length(lines)]))) lines <- lines[-length(lines)]
        lines <- c(lines, "",
                   sprintf("## Q%d. %s", n, trimws(a$question[g[1L]])),
                   "",
                   sprintf("- Reports: %s", paste(ids, collapse = ", ")),
                   sprintf("- The agent took, for now: %s", trimws(a$assumption[g[1L]] %||% "")),
                   "- Status: open",
                   "- Answer:")
      }
      changed <- TRUE
      q <- .ai_questions_read(lines)
    }
  }
  if (changed || !file.exists(f)) .ai_write_text(lines, f)
  q <- .ai_questions_read(lines)
  q$status[is.na(q$status)] <- "open"
  rownames(q) <- NULL
  q[c("n", "question", "reports", "status", "answer")]
}

# ---- spec/toc.yml -------------------------------------------------------------------

# The TOC read from the workbook as spec/toc.yml, in tflplanner's form: the
# reports in the TOC's order, each with its kind, its title, its section
# (the number's first parts: 14.1 of 14.1.1) and its analysis set when the
# TOC row names one of spec/data.yml's (its id or its label) and no other.
# Written when the study has none (or `overwrite`); the path, else nothing.
.ai_toc_yml <- function(dir, shell, overwrite = FALSE) {
  f <- file.path(dir, "spec", "toc.yml")
  t <- shell$toc
  if (!nrow(t) || (file.exists(f) && !isTRUE(overwrite))) return(character())
  dy <- file.path(dir, "spec", "data.yml")
  pops <- if (file.exists(dy)) tryCatch(yaml::read_yaml(dy)$populations, error = function(e) NULL)
  pid <- vapply(pops %||% list(), function(p) p$id %||% NA_character_, "")
  plab <- vapply(pops %||% list(), function(p) p$label %||% NA_character_, "")
  pop_of <- function(txt) {
    if (!length(pid) || is.na(txt)) return(NA_character_)
    low <- tolower(txt)
    hit <- vapply(seq_along(pid), function(i) {
      (!is.na(plab[i]) && grepl(tolower(plab[i]), low, fixed = TRUE)) ||
        (!is.na(pid[i]) && grepl(sprintf("(^|[^a-z0-9])%s([^a-z0-9]|$)", tolower(pid[i])), low))
    }, NA)
    if (sum(hit) == 1L) pid[hit] else NA_character_
  }
  outs <- lapply(seq_len(nrow(t)), function(i) {
    num <- sub("^[A-Z]*", "", t$key[i] %||% "")
    parts <- strsplit(num, ".", fixed = TRUE)[[1L]]
    e <- list(id = t$output_id[i], kind = if (is.na(t$type[i])) "table" else t$type[i])
    if (!is.na(t$title[i]) && nzchar(t$title[i])) e$label <- t$title[i]
    if (length(parts) >= 3L) e$section <- paste(parts[seq_len(length(parts) - 1L)], collapse = ".")
    p <- pop_of(t$text[i])
    if (!is.na(p)) e$population <- p
    e
  })
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  .ai_write_text(yaml::as.yaml(list(outputs = outs), indent.mapping.sequence = TRUE), f)
  f
}
