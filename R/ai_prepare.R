# The AI table guide's step 3 in one call: the study folder, the ADaM
# summary, the TOC / SHELL workbook read, the requests.

#' Prepare a study's AI table requests in one call
#'
#' The steps before the first request, one after the other:
#' [tfl_ai_study_init()] (only when the study has no `study.yml` yet -- one
#' that is there is kept, so the call can be run again),
#' [tfl_ai_adam_summary()] (to `ai/adam_summary.md`),
#' [tfl_ai_read_shell()] (to `ai/toc_shell.md`; its summary is printed,
#' with what to pass when no TOC or a report's shell was found) and
#' [tfl_ai_write_requests()].  Nothing is sent anywhere.
#'
#' @param dir The study folder.
#' @param toc The TOC / SHELL workbook (`.xlsx` or `.xlsm`).
#' @param adam The ADaM folder, from `dir` (relative) or absolute.
#' @param study_id,title The study's id and title, for a new study folder.
#' @param toc_sheet,sheets As [tfl_ai_read_shell()]: the TOC sheet when the
#'   guess is wrong, the sheets to keep.
#' @param size,language,workbook,instructions,max_chars As
#'   [tfl_ai_write_requests()]: the reports a request holds, the language of
#'   the assumptions, how the workbook reaches the chat (`"text"`,
#'   `"attach"`, `"both"`), the instructions in each request or once, a
#'   request's length at most.
#' @param overwrite `TRUE` to start the study folder again
#'   ([tfl_ai_study_init()] over its `study.yml` and `spec/`).  The requests
#'   are written again on every call (the answers are kept).
#' @param dry_run `TRUE` to read the workbook and the ADaM and write
#'   nothing: the same result, to look at before the requests are written
#'   (its `requests` are the TOC's batches, without `chars` and `file`;
#'   `files` is empty).
#' @param adam_summary A [tfl_ai_adam_summary()] result to use instead of
#'   reading the ADaM again (a previous call's `$adam`); read again when
#'   `datasets` names others than it has.
#' @param datasets,max_levels As [tfl_ai_adam_summary()]: the ADaM datasets
#'   the requests describe (`NULL`: all), the values listed of a variable.
#'   Fewer datasets, shorter requests.
#' @param quiet `TRUE` to say nothing (the steps' messages): a screen shows
#'   the result's `hint` and `next_steps` itself.
#' @return A `tfl_ai_prepared`: a list with `requests` (one row a request:
#'   `request`, `family`, `outputs`, `n`, `chars`, `file` -- as
#'   [tfl_ai_write_requests()] gives them), `toc` (one row a report of the
#'   TOC: `output_id`, `title`, `type`, `shell` -- the sheet of its shell --,
#'   `shell_found`, `request` -- the request that asks for it, `NA` for none),
#'   `sheets` (the workbook's sheets and their roles), `adam` (the
#'   [tfl_ai_adam_summary()]: `datasets`, `variables`, `text`), `files` (the
#'   files written, relative to `dir`), `hint` (what the workbook's reading
#'   could not find -- no TOC, a report with no shell -- and what to pass)
#'   and `next_steps` (what to do next).
#'   Its print is the workbook's summary and the next step.  The files are
#'   UTF-8.
#' @seealso The AI table guide: `system.file("ai", "tflspec-ai-table-guide.md",
#'   package = "tflspec")`.
#' @examples
#' ex <- system.file("ai", "example", package = "tflspec")
#' d <- file.path(tempdir(), "prepare-study")
#' dir.create(file.path(d, "adam"), recursive = TRUE, showWarnings = FALSE)
#' if (requireNamespace("haven", quietly = TRUE)) {
#'   for (x in c("adsl", "adae", "adrs", "adlb")) {
#'     df <- getExportedValue("tflspec", x)
#'     for (v in names(df)) if (is.factor(df[[v]])) df[[v]] <- as.character(df[[v]])
#'     haven::write_xpt(df, file.path(d, "adam", paste0(x, ".xpt")))
#'   }
#'   r <- tfl_ai_prepare(d, file.path(ex, "onco_toc_shell.xlsx"),
#'                       study_id = "XYZ-ONC-01", overwrite = TRUE)
#'   r$requests[c("request", "outputs", "chars")]
#' }
#' @export
tfl_ai_prepare <- function(dir, toc, adam = "adam", study_id = "STUDY-01", title = NULL,
                           toc_sheet = NULL, sheets = NULL, size = 3L, language = NULL,
                           workbook = c("text", "attach", "both"),
                           instructions = c("each", "once"), max_chars = NULL,
                           overwrite = FALSE, dry_run = FALSE, adam_summary = NULL,
                           datasets = NULL, max_levels = 15L, quiet = FALSE) {
  if (isTRUE(quiet)) {
    cl <- match.call()
    cl$quiet <- FALSE
    return(suppressMessages(eval(cl, parent.frame())))
  }
  fn <- "tfl_ai_prepare()"
  workbook <- match.arg(workbook)
  instructions <- match.arg(instructions)
  if (!is.character(toc) || length(toc) != 1L || !file.exists(toc)) {
    .ard_stop(sprintf("%s: `toc` is the TOC / SHELL workbook's path (.xlsx).", fn))
  }
  if (!is.null(adam_summary) && !inherits(adam_summary, "tfl_ai_adam")) {
    .ard_stop(sprintf("%s: `adam_summary` is a tfl_ai_adam_summary() result (a call's `$adam`).",
                      fn))
  }
  dry <- isTRUE(dry_run)
  ai <- file.path(dir, "ai")
  files <- character()
  if (!dry) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    if (!file.exists(file.path(dir, "study.yml")) || isTRUE(overwrite)) {
      files <- tfl_ai_study_init(dir, adam = adam, study_id = study_id, title = title,
                                 overwrite = overwrite)
    }
    dir.create(ai, recursive = TRUE, showWarnings = FALSE)
  }
  adam_dir <- if (grepl("^(/|[A-Za-z]:)", adam)) adam else file.path(dir, adam)
  ad <- adam_summary
  if (is.null(ad) || (!is.null(datasets) &&
                      !setequal(toupper(datasets), toupper(ad$datasets$dataset)))) {
    ad <- tfl_ai_adam_summary(adam_dir, datasets = datasets, max_levels = max_levels)
  }
  shell <- do.call(tfl_ai_read_shell, c(list(toc), Filter(Negate(is.null), list(
    toc_sheet = toc_sheet, sheets = sheets))))
  hint <- .ai_shell_hint(shell)
  if (dry) {
    b <- if (nrow(shell$toc)) tfl_ai_batches(shell, size = size) else
      data.frame(output_id = character(), family = character(), batch = character())
    bt <- unique(b$batch)
    req <- data.frame(
      request = bt, family = b$family[match(bt, b$batch)],
      outputs = vapply(bt, function(x) paste(b$output_id[b$batch == x], collapse = ", "), ""),
      n = vapply(bt, function(x) sum(b$batch == x), 0L),
      chars = rep(NA_integer_, length(bt)), file = rep(NA_character_, length(bt)),
      stringsAsFactors = FALSE)
    rownames(req) <- NULL
    nxt <- "look at the TOC and the requests; then run again with dry_run = FALSE to write them"
  } else {
    .ai_write_text(format(ad), file.path(ai, "adam_summary.md"))
    .ai_write_text(format(shell), file.path(ai, "toc_shell.md"))
    req <- tfl_ai_write_requests(dir, shell, ad, size = size, language = language,
                                 instructions = instructions, max_chars = max_chars,
                                 workbook = workbook, overwrite = TRUE)
    rq <- file.path(dir, "ai", "requests")
    files <- c(files, file.path(ai, c("adam_summary.md", "toc_shell.md")),
               list.files(rq, "[.](md|rds)$", full.names = TRUE))
    nxt <- c(sprintf(paste0("paste %s into a new chat; save the whole answer as ",
                                  "ai/answers/%s.md (tfl_ai_save_answer()), then ",
                                  "tfl_ai_collect_answers()"), req$file[1L], req$request[1L]))
  }
  # each report of the TOC, its shell and the request asking for it
  t <- shell$toc
  asked <- strsplit(req$outputs, ", ", fixed = TRUE)
  who <- rep(req$request, lengths(asked))
  tc <- data.frame(output_id = t$output_id, title = t$title, type = t$type,
                   shell = t$shell, shell_found = !is.na(t$shell),
                   request = who[match(t$output_id, unlist(asked))],
                   stringsAsFactors = FALSE)
  # the files written, relative to the study folder
  root <- paste0(normalizePath(dir, winslash = "/"), "/")
  files <- normalizePath(files, winslash = "/")
  inside <- startsWith(files, root)
  files[inside] <- substring(files[inside], nchar(root) + 1L)
  structure(list(requests = req, toc = tc, sheets = shell$sheets, adam = ad,
                 files = unique(files), hint = hint, next_steps = nxt, dry_run = dry,
                 workbook = basename(toc)),
            class = "tfl_ai_prepared")
}

#' @export
print.tfl_ai_prepared <- function(x, ...) {
  cat(sprintf("<tfl_ai_prepared%s: %s>\n", if (isTRUE(x$dry_run)) ", dry run" else "",
              x$workbook))
  s <- x$sheets
  cat(sprintf("%d sheet(s): %s\n", nrow(s),
              paste(sprintf("%s (%s)", s$sheet, s$role), collapse = ", ")))
  cat(sprintf("%d report(s) in the TOC, %d with a shell found; %d ADaM dataset(s)\n",
              nrow(x$toc), sum(x$toc$shell_found), nrow(x$adam$datasets)))
  r <- x$requests
  cat(sprintf("%d request(s)%s: %s\n", nrow(r), if (isTRUE(x$dry_run)) " to write" else "",
              paste(sprintf("%s (%d)", r$request, r$n), collapse = ", ")))
  if (length(x$hint)) cat(paste0("Note: ", x$hint, "\n"), sep = "")
  if (length(x$next_steps)) cat(paste0("Next: ", x$next_steps, "\n"), sep = "")
  invisible(x)
}
