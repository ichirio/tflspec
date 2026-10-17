# A study's table requests as files, and their answers read back: the
# drafting of many reports with a chat that takes one pasted text a time.

#' Write a study's table requests, one file a request
#'
#' Splits the TOC's tables into requests ([tfl_ai_batches()]) and writes the
#' prompt of each ([tfl_ai_prompt()]) to its own file, to paste into a chat
#' as **one message**: no attachment is needed.  A request longer than
#' `max_chars` is split again, until it fits or holds one report.  The
#' requests' contexts are kept beside them (`_contexts.rds`), so
#' [tfl_ai_collect_answers()] checks each answer against its own request.
#'
#' Start small (`size = 2` or `3`), check the answers, then raise `size`
#' while the chat keeps up.
#'
#' A request holds the instructions and schema (about 23,000 characters,
#' the same in every request) and its own reports (the TOC rows, shells and
#' ADaM summary).  For a chat that cannot take that in one message,
#' `instructions = "once"` writes the instructions to `00-instructions.md`
#' and each request without them: paste `00-instructions.md` first in a
#' new chat, then the request.
#'
#' @param study_dir The study folder ([tfl_ai_study_init()]).
#' @param shell The TOC and shells ([tfl_ai_read_shell()]).
#' @param adam The ADaM summary ([tfl_ai_adam_summary()]).
#' @param size The reports a request holds at most (1 or more).
#' @param dir The folder the requests go to, under `study_dir`.
#' @param max_chars A request's length at most, in characters; `NULL` for
#'   no limit.
#' @param families,types As [tfl_ai_batches()].
#' @param language As [tfl_ai_context()]: the language of the AI's
#'   assumptions.
#' @param examples Reviewed report files for the AI to follow
#'   ([tfl_ai_context()]'s `examples`).  Named by family
#'   (`c(ae = "spec/outputs/T-14-3-1.yml")`), each goes only to its
#'   family's requests; unnamed, to every request.
#' @param instructions `"each"` (default): every request holds the
#'   instructions, one paste a request.  `"once"`: the instructions in
#'   `00-instructions.md`, the requests without them.
#' @param overwrite `TRUE` to write over the requests already there.
#' @return A data frame, one row a request: `request` (the file's name,
#'   `ae-1`), `family`, `outputs` (the report ids, comma-separated), `n`,
#'   `chars`, `file`.  The same is written to `index.md` in `dir`.
#' @seealso [tfl_ai_collect_answers()] for the answers.
#' @examples
#' f <- system.file("ai", "example", "onco_toc_shell.xlsx", package = "tflspec")
#' d <- file.path(tempdir(), "req-study")
#' dir.create(file.path(d, "adam"), recursive = TRUE, showWarnings = FALSE)
#' if (requireNamespace("haven", quietly = TRUE)) {
#'   haven::write_xpt(as.data.frame(lapply(adsl, function(x)
#'     if (is.factor(x)) as.character(x) else x)), file.path(d, "adam", "adsl.xpt"))
#'   tfl_ai_study_init(d, overwrite = TRUE)
#'   r <- tfl_ai_write_requests(d, tfl_ai_read_shell(f),
#'                              tfl_ai_adam_summary(file.path(d, "adam")),
#'                              size = 2, overwrite = TRUE)
#'   r[c("request", "outputs", "chars")]
#' }
#' @export
tfl_ai_write_requests <- function(study_dir, shell, adam, size = 8L,
                                  dir = file.path("ai", "requests"),
                                  max_chars = NULL, families = NULL,
                                  types = "table", language = NULL,
                                  examples = NULL,
                                  instructions = c("each", "once"),
                                  overwrite = FALSE) {
  fn <- "tfl_ai_write_requests()"
  instructions <- match.arg(instructions)
  if (!inherits(shell, "tfl_ai_shell")) {
    .ard_stop(sprintf("%s: `shell` is a tfl_ai_read_shell() result.", fn))
  }
  if (!is.null(max_chars) && (!is.numeric(max_chars) || length(max_chars) != 1L ||
                              is.na(max_chars) || max_chars < 1000)) {
    .ard_stop(sprintf("%s: `max_chars` is one number, 1000 or more, or NULL.", fn))
  }
  b <- tfl_ai_batches(shell, size = size, types = types, families = families)
  if (!nrow(b)) .ard_stop(sprintf("%s: the TOC has no table to request.", fn))
  out_dir <- file.path(study_dir, dir)
  if (dir.exists(out_dir) && length(list.files(out_dir, "[.]md$")) && !isTRUE(overwrite)) {
    .ard_stop(sprintf("%s: %s already holds requests; pass `overwrite = TRUE`.", fn,
                      sQuote(dir, FALSE)))
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  unlink(list.files(out_dir, "[.]md$", full.names = TRUE))

  prompt_of <- function(ids, family) {
    ex <- if (is.null(names(examples))) examples
          else unname(examples[names(examples) == family])
    if (!length(ex)) ex <- NULL
    # a path of the study's own (spec/outputs/<id>.yml), from anywhere
    if (!is.null(ex)) ex <- ifelse(file.exists(ex), ex, file.path(study_dir, ex))
    ctx <- tfl_ai_context("table", outputs = ids, shell = shell, datasets = adam,
                          study_dir = study_dir, language = language,
                          examples = ex)
    p <- tfl_ai_prompt("table", ctx)
    text <- if (identical(instructions, "each")) format(p) else
      paste(c("(Follow the instructions of my first message in this chat.)", "",
              p$user), collapse = "\n")
    list(context = ctx, text = text, system = p$system)
  }
  # a request too long is halved until it fits, or holds one report
  fit <- function(name, ids, family) {
    p <- prompt_of(ids, family)
    if (is.null(max_chars) || nchar(p$text) <= max_chars || length(ids) == 1L) {
      if (!is.null(max_chars) && nchar(p$text) > max_chars) {
        warning(sprintf(paste0("%s: %s is %d characters with one report (%s), over ",
                               "`max_chars` (%d)."), fn, name, nchar(p$text), ids, max_chars),
                call. = FALSE)
      }
      return(stats::setNames(list(c(p, list(ids = ids))), name))
    }
    h <- ceiling(length(ids) / 2)
    c(fit(paste0(name, "a"), ids[seq_len(h)], family),
      fit(paste0(name, "b"), ids[-seq_len(h)], family))
  }
  req <- list()
  fam <- character()
  for (bt in unique(b$batch)) {
    r <- fit(bt, b$output_id[b$batch == bt], b$family[match(bt, b$batch)])
    req <- c(req, r)
    fam <- c(fam, rep(b$family[match(bt, b$batch)], length(r)))
  }
  for (nm in names(req)) .ai_write_text(req[[nm]]$text, file.path(out_dir, paste0(nm, ".md")))
  if (identical(instructions, "once")) {
    .ai_write_text(req[[1L]]$system, file.path(out_dir, "00-instructions.md"))
  }
  saveRDS(lapply(req, function(r) list(ids = r$ids, context = r$context)),
          file.path(out_dir, "_contexts.rds"))
  idx <- data.frame(
    request = names(req), family = fam,
    outputs = vapply(req, function(r) paste(r$ids, collapse = ", "), ""),
    n = vapply(req, function(r) length(r$ids), 0L),
    chars = vapply(req, function(r) nchar(r$text), 0L),
    file = file.path(dir, paste0(names(req), ".md")),
    stringsAsFactors = FALSE)
  rownames(idx) <- NULL
  .ai_write_text(paste(c(
    "# Requests",
    "",
    if (identical(instructions, "once"))
      c("Paste `00-instructions.md` first in a new chat, then one request.", ""),
    "Paste each file into a new chat as one message; save the whole answer as",
    sprintf("`ai/answers/<request>.md`, then run `tfl_ai_collect_answers(\"%s\")`.",
            "."),
    "",
    "| request | reports | characters | outputs |",
    "|---|---|---|---|",
    sprintf("| %s | %d | %d | %s |", idx$request, idx$n, idx$chars, idx$outputs)),
    collapse = "\n"), file.path(out_dir, "index.md"))
  message(sprintf("%d requests (%d reports) in %s; the longest %d characters.",
                  nrow(idx), sum(idx$n), dir, max(idx$chars)))
  idx
}

#' Read the answers of a study's requests, check them, write the report files
#'
#' Reads each `<request>.md` in `answers` (the AI's whole answer to the
#' request of the same name, [tfl_ai_write_requests()]), checks it against
#' its request ([tfl_ai_parse()], [tfl_ai_check()]) and, when it has no
#' error, writes its report files ([tfl_ai_unpack()]).  An answer with
#' errors writes no file: its repair prompt ([tfl_ai_repair_prompt()]) goes
#' to `repairs`, to paste into the same chat; save the new answer over the
#' old and collect again.
#'
#' @param study_dir The study folder.
#' @param requests,answers,repairs The folders, under `study_dir`.
#' @param overwrite `TRUE` (default) to write over report files already
#'   there: an answer collected again replaces its reports.
#' @return A data frame, one row a report: `request`, `output_id`,
#'   `status` (`ok`, `warning`, `error`, `no answer`), `problems` (how
#'   many), `first` (the first problem).  Its print is a summary.
#' @seealso [tfl_ai_write_requests()]
#' @export
tfl_ai_collect_answers <- function(study_dir, requests = file.path("ai", "requests"),
                                   answers = file.path("ai", "answers"),
                                   repairs = file.path("ai", "repairs"),
                                   overwrite = TRUE) {
  fn <- "tfl_ai_collect_answers()"
  cf <- file.path(study_dir, requests, "_contexts.rds")
  if (!file.exists(cf)) {
    .ard_stop(sprintf("%s: no requests in %s; write them with tfl_ai_write_requests().",
                      fn, sQuote(requests, FALSE)))
  }
  ctxs <- readRDS(cf)
  rows <- list()
  row <- function(req, ids, status, n = 0L, first = NA_character_) {
    data.frame(request = req, output_id = ids, status = status, problems = n,
               first = first, stringsAsFactors = FALSE)
  }
  for (nm in names(ctxs)) {
    ids <- ctxs[[nm]]$ids
    f <- file.path(study_dir, answers, paste0(nm, ".md"))
    if (!file.exists(f)) {
      rows[[nm]] <- row(nm, ids, "no answer")
      next
    }
    a <- tryCatch(tfl_ai_parse(readLines(f, warn = FALSE, encoding = "UTF-8"), "table"),
                  error = function(e) e)
    if (inherits(a, "error")) {
      rows[[nm]] <- row(nm, ids, "error", 1L, conditionMessage(a))
      next
    }
    pr <- tfl_ai_check(a, ctxs[[nm]]$context)
    err <- pr[pr$severity == "error", , drop = FALSE]
    if (nrow(err)) {
      dir.create(file.path(study_dir, repairs), recursive = TRUE, showWarnings = FALSE)
      .ai_write_text(format(tfl_ai_repair_prompt(a, pr)),
                     file.path(study_dir, repairs, paste0(nm, ".md")))
    } else {
      # only the reports this request asked for (another is warned of)
      keep <- a
      keep$outputs <- a$outputs[names(a$outputs) %in% ids]
      if (!is.null(keep$assumption_rows)) {
        r <- keep$assumption_rows
        keep$assumption_rows <- r[is.na(r$output_id) | r$output_id %in% ids, , drop = FALSE]
      }
      if (length(keep$outputs)) tfl_ai_unpack(keep, study_dir, overwrite = overwrite)
      unlink(file.path(study_dir, repairs, paste0(nm, ".md")))
    }
    # each report's own problems; one naming no report is the request's
    per <- lapply(ids, function(id) {
      hit <- pr[grepl(id, pr$where, fixed = TRUE), , drop = FALSE]
      own <- pr[!vapply(pr$where, function(w) any(vapply(ids, grepl, NA, x = w,
                                                         fixed = TRUE)), NA), , drop = FALSE]
      p <- rbind(hit, own)
      missing <- !id %in% names(a$outputs)
      st <- if (missing || any(p$severity == "error") || nrow(err)) "error"
            else if (nrow(p)) "warning" else "ok"
      first <- if (missing) "not in the answer"
               else if (nrow(p)) paste0(p$where[1L], ": ", p$problem[1L]) else NA_character_
      row(nm, id, st, nrow(p) + missing, first)
    })
    rows[[nm]] <- do.call(rbind, per)
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  class(out) <- c("tfl_ai_collected", class(out))
  out
}

#' @export
print.tfl_ai_collected <- function(x, ...) {
  tab <- table(factor(x$status, c("ok", "warning", "error", "no answer")))
  cat(sprintf("%d reports: %s\n", nrow(x),
              paste(sprintf("%s %d", names(tab), tab), collapse = ", ")))
  todo <- x[x$status %in% c("error", "warning"), , drop = FALSE]
  if (nrow(todo)) {
    cat("\n")
    print(as.data.frame(todo[c("request", "output_id", "status", "first")]),
          row.names = FALSE, right = FALSE)
  }
  invisible(x)
}
