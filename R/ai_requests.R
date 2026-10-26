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
#' @param size The reports a request holds at most (1 or more); `NULL`
#'   (the default): 1 with `mode = "agent"` (each answer checked and built
#'   before the next), 8 in a chat.
#' @param dir The folder the requests go to, under `study_dir`.
#' @param max_chars A request's length at most, in characters; `NULL` for
#'   no limit.  A request over it is halved; with `templates`, it first
#'   lists the templates' arguments by name only (no type, default or
#'   description).
#' @param families,types As [tfl_ai_batches()]: the tables and listings
#'   in batches (a listing's family `listing`; its answer a report of `kind:
#'   listing`, in the table task).  A
#'   figure (`"figure"` in `types`, the default) is a request of its own,
#'   `figure-1`, `figure-2` ... (family `figure`): its shell, the ADaM's
#'   summary, the figure templates; its answer a figure design.
#' @param language As [tfl_ai_context()]: the language of the AI's
#'   assumptions.
#' @param examples Reviewed report files for the AI to follow
#'   ([tfl_ai_context()]'s `examples`).  Named by family
#'   (`c(ae = "spec/outputs/T-14-3-1.yml")`), each goes only to its
#'   family's requests; unnamed, to every request.
#' @param instructions In a chat: `"once"` (default), the instructions in
#'   `00-instructions.md` (the chat's first message), the requests without
#'   them; `"each"`, every request holds them, one paste a request.  With
#'   `mode = "agent"` either is the same: the instructions are written once,
#'   `ai/instructions-table.md` / `ai/instructions-figure.md`, and no
#'   request repeats them.
#' @param workbook How the TOC / SHELL workbook reaches the chat:
#'   `"text"` (default) the shells as text in each request
#'   ([tfl_ai_read_shell()]); `"attach"` the workbook attached to each chat,
#'   the request saying where each report's shell is in it (its sheet,
#'   cells and TOC row) instead of the text; `"both"` the places and the
#'   text, the attachment right where the two differ -- for a workbook whose
#'   text is not read right.  A chat that takes no Excel file needs
#'   `"text"`.  `"file"`: for an agent that reads files
#'   ([tfl_ai_prepare()]`(mode = "agent")`) -- the text, and the workbook's
#'   path with each report's place in it, to read that place alone when the
#'   text looks cut, never another sheet.
#' @param overwrite `TRUE` to write over the requests already there; with
#'   `add`, to ask again for a report asked for already (its request is
#'   written anew).
#' @param add `TRUE` to keep the requests already there and add these: the
#'   new ones named after them (`ae-2` when `ae-1` is there, `figure-3`
#'   ...), the index listing all of them, each request's `book` the
#'   workbook its reports' shells are in.
#' @param templates `TRUE` (default): each request asks the AI to answer a
#'   report with a template (`template: {name, args}`) where one fits --
#'   the templates registered when the requests are written listed in it,
#'   its first line `<!-- templates: TRUE -->` -- else as a code SPEC.
#'   `FALSE`: the code SPEC only, the requests as they were before templates.
#' @param progress `NULL`, or a function(step, i, n) called as the requests
#'   are made: `step` `"request"`, `i` of `n` written (0 first) -- for a
#'   progress bar (shiny's `setProgress()`).  Each request is written as
#'   it is made; `index.md` and the contexts at the end.
#' @param unit A request's unit: `"sheet"` (default; `shell`'s `shells`) a
#'   shell sheet, its TOC items as candidates, and one request of the TOC
#'   items no sheet chose; `"toc"` the TOC's reports in batches
#'   ([tfl_ai_batches()]), as before.  A sheet's request lets the AI name
#'   its reports: their ids are not held to a list (one that is no TOC
#'   item's is a warning), and a sheet that is no shell is answered with no
#'   report (a warning).
#' @param mode `"chat"` (default): each request whole, to paste.
#'   `"agent"` ([tfl_ai_prepare()]`(mode = "agent")`): what every request
#'   would repeat in a file of the study's `ai/` folder, written once
#'   (`instructions-table.md`, `instructions-figure.md`, `templates.md`),
#'   the ADaM summary and `spec/` files named, not copied; each request
#'   names them, its answer's header and, with `templates`, the templates
#'   likely for its reports (from their titles).
#' The folder the answers go to (`ai/answers`, beside `dir`) is made too.
#'
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
tfl_ai_write_requests <- function(study_dir, shell, adam, size = NULL,
                                  dir = file.path("ai", "requests"),
                                  max_chars = NULL, families = NULL,
                                  types = c("table", "listing", "figure"), language = NULL,
                                  examples = NULL,
                                  instructions = c("once", "each"),
                                  workbook = c("text", "attach", "both", "file"),
                                  overwrite = FALSE, add = FALSE, templates = TRUE,
                                  mode = c("chat", "agent"), progress = NULL,
                                  unit = c("sheet", "toc")) {
  fn <- "tfl_ai_write_requests()"
  if (!is.null(progress) && !is.function(progress)) {
    .ard_stop(sprintf("%s: `progress` is a function(step, i, n), or NULL.", fn))
  }
  templates <- .ai_templates_flag(templates, fn)
  instructions <- match.arg(instructions)
  mode <- match.arg(mode)
  unit <- match.arg(unit)
  agent <- identical(mode, "agent")
  # (an agent's request: one report; a chat's: up to 8)
  size <- size %||% if (agent) 1L else 8L
  workbook <- match.arg(workbook)
  book <- basename(shell$path %||% "the workbook")
  # the workbook's path for an agent: from the study folder when inside it
  root <- paste0(normalizePath(study_dir, winslash = "/", mustWork = FALSE), "/")
  # (a path in the study folder, however it was given -- relative, with
  # backslashes, another case on Windows -- from the study folder; one
  # outside it, whole)
  from_study <- function(p) {
    q <- normalizePath(p, winslash = "/", mustWork = FALSE)
    same <- if (.Platform$OS.type == "windows") startsWith(tolower(q), tolower(root)) else
      startsWith(q, root)
    ifelse(same, substring(q, nchar(root) + 1L), q)
  }
  book_path <- if (is.null(shell$path)) book else from_study(shell$path)
  # the workbooks of shells apart from the TOC's
  more_books <- if (!is.null(shell$books) && nrow(shell$books) > 1L) {
    from_study(shell$books$path[-1L])
  } else character()
  if (!inherits(shell, "tfl_ai_shell")) {
    .ard_stop(sprintf("%s: `shell` is a tfl_ai_read_shell() result.", fn))
  }
  if (!is.null(max_chars) && (!is.numeric(max_chars) || length(max_chars) != 1L ||
                              is.na(max_chars) || max_chars < 1000)) {
    .ard_stop(sprintf("%s: `max_chars` is one number, 1000 or more, or NULL.", fn))
  }
  if (!nrow(shell$toc)) {
    .ard_stop(sprintf(paste0("%s: no TOC was found in %s; read it again naming its TOC sheet: ",
                             "tfl_ai_read_shell(\"%s\", toc_sheet = \"<sheet>\") ",
                             "(the sheets: %s)."), fn, basename(shell$path), basename(shell$path),
                      paste(shell$sheets$sheet, collapse = ", ")))
  }
  # the tables in batches; each figure a request of its own (figure-1 ...)
  tab_types <- setdiff(types, "figure")
  b <- if (length(tab_types)) {
    tfl_ai_batches(shell, size = size, types = tab_types, families = families)
  } else data.frame(output_id = character(), family = character(), batch = character())
  figs <- if ("figure" %in% types) shell$toc$output_id[shell$toc$type %in% "figure"] else character()
  # the shell sheets first (tfl_ai_read_shell()$shells): a request a sheet
  # (a chat's: `size` sheets), the TOC its candidates; a figure sheet's
  # request a figure's; the TOC items no sheet chose, one request
  sheet_mode <- identical(unit, "sheet") && !is.null(shell$shells)
  if (sheet_mode) {
    b <- data.frame(output_id = character(), family = character(), batch = character())
    figs <- character()
  }
  if (!nrow(b) && !length(figs) && !(sheet_mode && nrow(shell$shells) + nrow(shell$toc) > 0L)) {
    .ard_stop(sprintf(paste0("%s: none of the TOC's %d reports is a %s (shell$toc$type); ",
                             "pass `types` or `families` that pick them."), fn, nrow(shell$toc),
                      paste(types, collapse = " or ")))
  }
  out_dir <- file.path(study_dir, dir)
  cf <- file.path(out_dir, "_contexts.rds")
  # add = TRUE: the requests there are kept, these added beside them
  old <- if (isTRUE(add) && file.exists(cf)) readRDS(cf) else list()
  if (!isTRUE(add) && dir.exists(out_dir) && length(list.files(out_dir, "[.]md$")) &&
      !isTRUE(overwrite)) {
    .ard_stop(sprintf("%s: %s already holds requests; pass `overwrite = TRUE`, or `add = TRUE` to add these.",
                      fn, sQuote(dir, FALSE)))
  }
  if (length(old)) {
    # a report asked for already: stopped, or (overwrite) its request asked anew
    had <- unlist(lapply(old, `[[`, "ids"))
    # (the shell sheets' requests: the TOC items they name, a sheet with no
    # candidate by its name -- as their contexts keep them)
    asking <- if (sheet_mode) {
      # (a sheet asked for no TOC item stands under its name)
      unique(c(shell$toc$output_id, vapply(shell$shells$sheet, .ai_file_id, "", USE.NAMES = FALSE)))
    } else c(b$output_id, figs)
    again <- intersect(asking, had)
    if (length(again) && !isTRUE(overwrite)) {
      in_req <- names(old)[vapply(old, function(o) any(o$ids %in% again), NA)]
      .ard_stop(sprintf(paste0("%s: %s already asked for (%s); pass `overwrite = TRUE` to ",
                               "ask for them again."), fn, paste(again, collapse = ", "),
                        paste(in_req, collapse = ", ")))
    }
    if (length(again)) {
      drop <- names(old)[vapply(old, function(o) any(o$ids %in% again), NA)]
      left <- setdiff(unlist(lapply(old[drop], `[[`, "ids")), again)
      if (length(left)) {
        message(sprintf("%s: %s no longer asked for (their request %s is written anew).", fn,
                        paste(left, collapse = ", "), paste(drop, collapse = ", ")))
      }
      unlink(file.path(out_dir, paste0(drop, ".md")))
      old <- old[setdiff(names(old), drop)]
    }
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  if (!isTRUE(add)) unlink(list.files(out_dir, "[.]md$", full.names = TRUE))
  # the new requests named after those kept: ae-2 when ae-1 is there
  next_name <- function(family, taken) {
    k <- 1L
    while (paste0(family, "-", k) %in% taken) k <- k + 1L
    paste0(family, "-", k)
  }
  taken <- names(old)
  if (length(old) && nrow(b)) {
    to <- character()
    for (bt in unique(b$batch)) {
      to[bt] <- next_name(b$family[match(bt, b$batch)], c(taken, to))
    }
    b$batch <- unname(to[b$batch])
  }
  fig_names <- character()
  for (k in seq_along(figs)) fig_names[k] <- next_name("figure", c(taken, fig_names))
  # one report a request: the request named after its report's number
  # (t.15.1.5.1.md: the sheet it reads), letters a file name cannot hold
  # replaced
  if (isTRUE(as.integer(size) == 1L) && nrow(b)) {
    # (a shell's reports: the shell's number)
    one <- !duplicated(b$batch)
    nm <- .ai_file_id((b$shell_id %||% b$output_id)[one])
    nm <- make.unique(c(taken, nm), sep = "-")[length(taken) + seq_along(nm)]
    b$batch <- unname(stats::setNames(nm, b$batch[one])[b$batch])
    if (length(figs)) {
      fn_ <- make.unique(c(taken, nm, .ai_file_id(figs)), sep = "-")
      fig_names <- fn_[length(taken) + length(nm) + seq_along(figs)]
    }
  }

  # the shells' text as the workbook reaches the chat: the text alone; with
  # where each report's shell is in the attached workbook ("attach",
  # "both"); or in the workbook file an agent reads ("file")
  placed <- function(text, ids) {
    # a part on its parent's sheet: which of the sheet's reports is this one
    text <- paste(c(.ai_toc_row_lines(shell, ids), text), collapse = "\n")
    if (identical(workbook, "file")) {
      # an agent that reads files: the text, and where each report's shell
      # is in the workbook file -- to check what the text leaves unclear
      # when the agent can open it; when it cannot, the request alone
      where <- c(if (length(more_books)) {
                   sprintf(paste("The TOC is the workbook `%s`; the shells are in %s.",
                                 "The reports' shells:"), book_path,
                           paste(sprintf("`%s`", more_books), collapse = ", "))
                 } else {
                   sprintf("The TOC / SHELL workbook is the file `%s`. The reports' shells in it:",
                           book_path)
                 }, "",
                 .ai_shell_where(shell, ids), "",
                 paste("Use the text below, read from that workbook. When you can open the",
                       "workbook, use it to check what the text leaves unclear -- above all a",
                       "shell's shape (several parts, a part that writes only what differs,",
                       "merged cells, a text that looks cut): read the place above -- that",
                       "sheet, those cells -- with R:",
                       sprintf("`tflspec::tfl_ai_read_shell(\"%s\", %soutputs = \"<id>\")`", book_path,
                               if (length(more_books)) sprintf("shells = c(%s), ",
                                 paste(sprintf("\"%s\"", more_books), collapse = ", ")) else ""),
                       "or `readxl::read_excel(<file>, sheet = <sheet>, range = <cells>)`. When",
                       "you cannot open it (outside your workspace, no permission), decide from",
                       "this request alone. When the place looks wrong, ask it as a question."))
      return(paste(c(where, "", text), collapse = "\n"))
    }
    if (identical(workbook, "text")) return(text)
    # the workbook attached: where each report's shell is in it, with the
    # text ("both") or instead of it ("attach")
    where <- c(sprintf(paste("The TOC / SHELL workbook `%s` is attached to this message.",
                             "The reports' shells in it:"), book), "",
               .ai_shell_where(shell, ids),
               if (identical(workbook, "both"))
                 c("", paste("The text below is read from that workbook; where the two",
                             "differ, the attached workbook is right.")))
    paste(c(where, if (identical(workbook, "both")) c("", text)), collapse = "\n")
  }
  # a figure: a request of its own, its shell, its analysis set when the
  # TOC row names one of spec/data.yml's
  fig_prompt_of <- function(id, sheet = NULL) {
    i <- match(id, shell$toc$output_id)
    ttl <- shell$toc$title[i]
    ctx <- tfl_ai_context("figure", output_id = id,
                          titles = if (!is.na(ttl)) ttl, datasets = adam,
                          population = .ai_population_of(study_dir, shell$toc$text[i]),
                          shell = shell, language = language)
    ctx$shell <- if (is.null(sheet)) placed(ctx$shell, id) else
      placed_sheets(paste(c(.ai_sheet_lines(shell, sheet), .ai_sheet_text(shell, sheet)),
                          collapse = "\n"), sheet)
    if (!is.null(sheet)) ctx$sheets <- sheet
    ctx$ai_templates <- templates
    if (agent) ctx$ai_files <- TRUE
    p <- tfl_ai_prompt("figure", ctx)
    list(context = ctx, text = if (agent) p$user else format(p), system = p$system,
         task = "figure")
  }
  prompt_of <- function(ids, family, short = FALSE) {
    ex <- if (is.null(names(examples))) examples
          else unname(examples[names(examples) == family])
    if (!length(ex)) ex <- NULL
    # a path of the study's own (spec/outputs/<id>.yml), from anywhere
    if (!is.null(ex)) ex <- ifelse(file.exists(ex), ex, file.path(study_dir, ex))
    ctx <- tfl_ai_context("table", outputs = ids, shell = shell, datasets = adam,
                          study_dir = study_dir, language = language,
                          examples = ex)
    ctx$shell <- placed(ctx$shell, ids)
    ctx$ai_templates <- templates
    if (short) ctx$ai_templates_short <- TRUE
    if (agent) ctx$ai_files <- TRUE
    p <- tfl_ai_prompt("table", ctx)
    text <- if (agent) p$user else if (identical(instructions, "each")) format(p) else
      paste(c("(Follow the instructions of my first message in this chat.)", "",
              p$user), collapse = "\n")
    list(context = ctx, text = text, system = p$system, task = "table")
  }
  # a request of shell sheets (or of the TOC items no sheet chose): each
  # sheet's lines and text; the reports the AI's to name
  sheet_prompt_of <- function(us, family, noshell = character()) {
    ex <- if (is.null(names(examples))) examples
          else unname(examples[names(examples) == family])
    if (!length(ex)) ex <- NULL
    if (!is.null(ex)) ex <- ifelse(file.exists(ex), ex, file.path(study_dir, ex))
    sheets <- vapply(us, `[[`, "", "sheet")
    text <- if (length(noshell)) {
      paste(c(.ai_noshell_lines(shell, noshell), .ai_noshell_text(shell, noshell)), collapse = "\n")
    } else {
      paste(c(unlist(lapply(sheets, function(s) .ai_sheet_lines(shell, s))),
              unlist(lapply(sheets, function(s) .ai_sheet_text(shell, s)))), collapse = "\n")
    }
    ctx <- tfl_ai_context("table", shell = text, datasets = adam, study_dir = study_dir,
                          language = language, examples = ex)
    ctx$shell <- placed_sheets(ctx$shell, sheets)
    ctx$sheet_mode <- TRUE
    ctx$sheets <- sheets
    ids <- if (length(noshell)) noshell else unique(unlist(lapply(us, `[[`, "ids")))
    ctx$candidates <- ids
    ctx$toc_keys <- shell$toc$key
    ctx$ai_templates <- templates
    if (agent) ctx$ai_files <- TRUE
    p <- tfl_ai_prompt("table", ctx)
    text <- if (agent) p$user else if (identical(instructions, "each")) format(p) else
      paste(c("(Follow the instructions of my first message in this chat.)", "",
              p$user), collapse = "\n")
    list(context = ctx, text = text, system = p$system, task = "table",
         ids = if (length(ids)) ids else vapply(sheets, .ai_file_id, ""))
  }
  # where a request's sheets are, as the workbook reaches the AI
  placed_sheets <- function(text, sheets) {
    at <- if (length(sheets)) paste(sprintf("'%s'", sheets), collapse = ", ") else "(none)"
    if (identical(workbook, "file")) {
      return(paste(c(sprintf("The TOC / SHELL workbook is the file `%s`; this request's sheet(s): %s.",
                             book_path, at),
                     paste("Use the text below, read from that workbook. When you can open it,",
                           "use it to check what the text leaves unclear (read the sheet with R:",
                           "`readxl::read_excel(<file>, sheet = <sheet>)`); when you cannot,",
                           "decide from this request alone."), "", text), collapse = "\n"))
    }
    if (identical(workbook, "text")) return(text)
    where <- sprintf("The TOC / SHELL workbook `%s` is attached to this message; this request's sheet(s): %s.",
                     book, at)
    paste(c(where, if (identical(workbook, "both")) c("", paste(
      "The text below is read from that workbook; where the two differ, the attached",
      "workbook is right."), "", text)), collapse = "\n")
  }
  # a request too long is halved until it fits, or holds one report
  # (a chat's with templates: first the templates' arguments by name only)
  fit <- function(name, ids, family, short = FALSE) {
    p <- prompt_of(ids, family, short)
    if (!short && templates && !agent && identical(instructions, "each") &&
        !is.null(max_chars) && nchar(p$text) > max_chars) {
      return(fit(name, ids, family, short = TRUE))
    }
    if (is.null(max_chars) || nchar(p$text) <= max_chars || length(ids) == 1L) {
      if (!is.null(max_chars) && nchar(p$text) > max_chars) {
        warning(sprintf(paste0("%s: %s is %d characters with one report (%s), over ",
                               "`max_chars` (%d)."), fn, name, nchar(p$text), ids, max_chars),
                call. = FALSE)
      }
      return(stats::setNames(list(c(p, list(ids = ids))), name))
    }
    h <- ceiling(length(ids) / 2)
    c(fit(paste0(name, "a"), ids[seq_len(h)], family, short),
      fit(paste0(name, "b"), ids[-seq_len(h)], family, short))
  }
  # (with templates: the setting at the request's head)
  head <- if (templates) paste0("<!-- ", .ai_templates_line(templates), " -->\n")
  # each request written as soon as it is made: a run stopped half way
  # leaves the requests it made (index.md, the contexts: at the end)
  write_req <- function(r) {
    for (nm in names(r)) {
      .ai_write_text(paste0(head, r[[nm]]$text), file.path(out_dir, paste0(nm, ".md")))
    }
  }
  if (sheet_mode) {
    plan <- .ai_sheet_plan(shell, size, types, families, taken)
    ub <- plan$ub
    noshell <- plan$noshell
    fig_names <- names(plan$figs)
    figs <- unname(vapply(plan$figs, function(u) if (length(u$ids)) u$ids[1L] else u$sheet, ""))
    fig_sheets <- unname(vapply(plan$figs, `[[`, "", "sheet"))
    batches <- names(ub)
    n_req <- length(ub) + length(figs) + as.integer(length(noshell) > 0L)
  } else {
    batches <- unique(b$batch)
    n_req <- length(batches) + length(figs)
  }
  tick <- function(i) if (is.function(progress)) progress("request", i, n_req)
  tick(0L)
  req <- list()
  fam <- character()
  for (j in seq_along(batches)) {
    bt <- batches[j]
    if (sheet_mode) {
      us <- ub[[bt]]
      r <- stats::setNames(list(sheet_prompt_of(us, us[[1L]]$family)), bt)
      write_req(r)
      req <- c(req, r)
      fam <- c(fam, us[[1L]]$family)
      tick(j)
      next
    }
    r <- fit(bt, b$output_id[b$batch == bt], b$family[match(bt, b$batch)])
    write_req(r)
    req <- c(req, r)
    fam <- c(fam, rep(b$family[match(bt, b$batch)], length(r)))
    tick(j)
  }
  if (sheet_mode && length(noshell)) {
    r <- stats::setNames(list(sheet_prompt_of(list(), "other", noshell = noshell)),
                         plan$noshell_name)
    write_req(r)
    req <- c(req, r)
    fam <- c(fam, "other")
    tick(length(batches) + 1L)
  }
  for (k in seq_along(figs)) {
    # (a sheet's figure: the sheet whole, its candidates first)
    fp <- if (sheet_mode) fig_prompt_of(figs[k], sheet = fig_sheets[k]) else fig_prompt_of(figs[k])
    r <- stats::setNames(list(c(fp, list(ids = figs[k]))), fig_names[k])
    write_req(r)
    req <- c(req, r)
    fam <- c(fam, "figure")
    tick(length(batches) + k)
  }
  if (agent) {
    # an agent's: what every request would repeat, once (ai/instructions-
    # <task>.md, ai/templates.md)
    for (task in unique(vapply(req, `[[`, "", "task"))) {
      sys <- unique(unlist(lapply(Filter(function(r) identical(r$task, task), req), `[[`, "system")))
      stopifnot(length(sys) == 1L)
      .ai_write_text(sys, file.path(study_dir, .ai_instructions_file(task)))
    }
    if (templates) .ai_write_text(.ai_templates_file_text(), file.path(study_dir, "ai", "templates.md"))
  } else if (identical(instructions, "once")) {
    .ai_write_text(req[[1L]]$system, file.path(out_dir, "00-instructions.md"))
  }
  # the folder the answers are saved in, beside the requests (ai/answers)
  dir.create(file.path(study_dir, dirname(dir), "answers"), recursive = TRUE,
             showWarnings = FALSE)
  # each request's workbook: its reports' shells'
  book_of <- function(ids) {
    bk <- shell$toc$book[match(ids, shell$toc$output_id)]
    bk <- unique(bk[!is.na(bk)])
    if (length(bk)) paste(bk, collapse = ", ") else basename(shell$path %||% NA_character_)
  }
  new <- stats::setNames(lapply(seq_along(req), function(i) {
    ctx <- req[[i]]$context
    # (the study folder: whichever the answers are collected in, not this
    # PC's path -- a copied study checks its answers against its own files)
    if (!is.null(ctx$study_dir)) ctx$study_dir <- "."
    list(ids = req[[i]]$ids, context = ctx, family = fam[i],
         book = book_of(req[[i]]$ids))
  }), names(req))
  saveRDS(c(old, new), cf)
  idx <- tfl_ai_requests(study_dir, requests = dir)
  .ai_write_text(paste(c(
    "# Requests",
    "",
    if (!agent && identical(instructions, "once"))
      c("Paste `00-instructions.md` first in a new chat, then one request.", ""),
    if (!agent && workbook %in% c("attach", "both"))
      c(sprintf("Attach `%s` (the TOC / SHELL workbook) to each chat with its request.", book), ""),
    if (agent) c(
      "For the agent (AGENTS.md; its first message is `ai/agent_prompt.md`): answer each",
      "request in this order, save the whole answer as `ai/answers/<request>.md` and run",
      "`Rscript run.R <request>`; then read `ai/status.md`.")
    else c(
      "Paste each file into a new chat as one message; save the whole answer as",
      sprintf("`ai/answers/<request>.md`, then run `tfl_ai_collect_answers(\"%s\")`.", ".")),
    "",
    "| request | reports | characters | outputs |",
    "|---|---|---|---|",
    sprintf("| %s | %d | %d | %s |", idx$request, idx$n, idx$chars, idx$outputs)),
    collapse = "\n"), file.path(out_dir, "index.md"))
  ans <- gsub("\\\\", "/", file.path(dirname(dir), "answers"))
  message(sprintf(paste0("%d requests (%d reports) in %s; the longest %d characters.\n",
                         "Next: paste %s into a new chat%s%s; save the whole answer as ",
                         "%s/%s.md, then run tfl_ai_collect_answers()."),
                  nrow(idx), sum(idx$n), dir, max(idx$chars), idx$file[1L],
                  if (!agent && identical(instructions, "once")) " after 00-instructions.md" else "",
                  if (workbook %in% c("attach", "both")) sprintf(", attaching %s", book) else "",
                  ans, idx$request[1L]))
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
#' An answer is read as UTF-8, with or without the byte-order mark Windows
#' Notepad writes, and with any line ends; an answer saved in another
#' encoding (Shift-JIS / CP932, "ANSI") is an `error` that says how to save
#' it again.  An answer saved as `<request>.md.txt` or `<request>.txt` (Notepad
#' adds `.txt`) is read too.
#'
#' @param study_dir The study folder.
#' @param requests,answers,repairs The folders, under `study_dir`.
#' @param overwrite `TRUE` (default) to write over report files already
#'   there: an answer collected again replaces its reports.
#' @return A `tfl_ai_collected`: a list with `reports` (a data frame, one
#'   row a report: `request`, `output_id`, `status` -- `ok`, `warning`,
#'   `error`, `no answer` --, `problems` (how many), `first` (the first
#'   problem), `problems_text` (all of them, one a line), `file` (the
#'   report's `spec/outputs/<id>.yml` when written, else `NA`), `repair`
#'   (the request's repair prompt to paste into the same chat, else `NA`)),
#'   `next_steps` (a request's next step, named by the request),
#'   `assumptions` (the study's `spec/_assumptions.yml` for these reports:
#'   `output_id`, `where`, `item`, `assumption`, `question`), `stray` (answer
#'   files named after no request), `requests` (the requests' folder),
#'   `templates` (each request's setting, [tfl_ai_prepare()]'s `templates`:
#'   a logical named by the request), `manifest` and `manifest_file`
#'   (below).
#'
#'   The manifest is every report the answers say they make, for the person
#'   to check the count against the TOC (one TOC row can be several
#'   reports): each answer lists its reports first (`reports:`, one line a
#'   report: `id`, `title`, `split` -- how its TOC row is split for it --,
#'   `toc` -- the TOC row's number).  A data frame, one row a report:
#'   `request`, `toc`, `output_id`, `title`, `split`, `listed` (`FALSE`: a
#'   report the answer writes but does not list; `NA`: a request not
#'   answered or not read, its reports as asked) and `status` (the report's,
#'   as in `reports`, or `"not answered"`: listed, but no report file in the
#'   answer).  It is written to `ai/manifest.csv` (UTF-8) beside the
#'   requests' folder, anew at each collection.
#'   The paths are relative to `study_dir`.  Its print is a summary: each
#'   request's status, its reports and the next step.
#' @seealso [tfl_ai_write_requests()]
#' @export
tfl_ai_collect_answers <- function(study_dir, requests = file.path("ai", "requests"),
                                   answers = file.path("ai", "answers"),
                                   repairs = file.path("ai", "repairs"),
                                   overwrite = TRUE) {
  fn <- "tfl_ai_collect_answers()"
  cf <- file.path(study_dir, requests, "_contexts.rds")
  if (!file.exists(cf)) {
    .ard_stop(sprintf(paste0("%s: no requests in %s; write them first with ",
                             "tfl_ai_write_requests(\"%s\", shell, adam)."),
                      fn, sQuote(file.path(study_dir, requests), FALSE), study_dir))
  }
  ctxs <- readRDS(cf)
  # (a study with the AI's drafts apart: the answers write them there)
  area_opt <- .ai_area_options(study_dir)
  if (!is.null(area_opt)) on.exit(options(area_opt), add = TRUE)
  # each request's study folder is this one, wherever it was written (a
  # copied study; requests written before kept this PC's path)
  here <- normalizePath(study_dir, winslash = "/", mustWork = FALSE)
  ctxs <- lapply(ctxs, function(x) {
    if (!is.null(x$context$study_dir)) x$context$study_dir <- here
    x
  })
  rows <- list()
  row <- function(req, ids, status, n = 0L, first = NA_character_, all = first) {
    data.frame(request = req, output_id = ids, status = status, problems = n,
               first = first, problems_text = all, stringsAsFactors = FALSE)
  }
  rel <- function(...) gsub("\\\\", "/", file.path(...))
  nxt <- character()
  # each answer's list of its reports (`reports`), for the manifest
  listed <- list()
  # a sheet's request: the TOC items it was asked for (the others its
  # candidates only), by request -- a report another sheet was asked for is
  # that request's
  own <- lapply(ctxs, function(x) if (isTRUE(x$context$sheet_mode)) .ai_file_id(x$context$candidates))
  for (nm in names(ctxs)) {
    ids <- .ai_file_id(ctxs[[nm]]$ids)
    f <- .ai_answer_file(file.path(study_dir, answers), nm)
    # the next step as the request was written for: a chat's (paste, ask
    # the chat) or an agent's (answer, save, run.R)
    agent <- isTRUE(ctxs[[nm]]$context$ai_files)
    run <- sprintf(" and run `Rscript run.R %s`", nm)
    if (is.na(f)) {
      rows[[nm]] <- row(nm, ids, "no answer")
      nxt[nm] <- if (agent) {
        sprintf("answer %s; save the whole answer as %s%s",
                rel(requests, paste0(nm, ".md")), rel(answers, paste0(nm, ".md")), run)
      } else sprintf("paste %s into a new chat; save the whole answer as %s",
                     rel(requests, paste0(nm, ".md")), rel(answers, paste0(nm, ".md")))
      next
    }
    task <- ctxs[[nm]]$context$task %||% "table"
    a <- tryCatch(tfl_ai_parse(.ai_read_lines(f, fn), task),
                  error = function(e) e)
    if (inherits(a, "error")) {
      rows[[nm]] <- row(nm, ids, "error", 1L, conditionMessage(a))
      nxt[nm] <- sprintf(paste0("fix %s as the problem below says (copy the whole answer, ",
                                "from the first ``` to the last), then %s"),
                         rel(answers, basename(f)),
                         if (agent) sprintf("run `Rscript run.R %s`", nm) else "collect again")
      next
    }
    # a report the answer gives in parts (t.15.1.5.1.a, .b): its parts are
    # its reports here
    ids <- unlist(lapply(ids, function(id) {
      k <- .ai_id_key(id)
      pk <- .ai_id_key(names(a$outputs))
      part <- !is.na(pk) & !is.na(k) & grepl("[A-Z]$", pk) &
        vapply(pk, function(z) .ai_id_same(sub("[A-Z]$", "", z), k), NA)
      if (any(part) && !id %in% names(a$outputs)) names(a$outputs)[part] else id
    }), use.names = FALSE)
    a <- .ai_answer_as_asked(a, ids)
    sheet <- isTRUE(ctxs[[nm]]$context$sheet_mode)
    if (sheet) ids <- .ai_sheet_answer_ids(ids, a, nm, own)
    listed[[nm]] <- .ai_manifest_rows(nm, a)
    a <- .ai_answer_overall(a)
    pr <- tryCatch(tfl_ai_check(a, ctxs[[nm]]$context), error = function(e) e)
    # a figure's design under its report, as a table answer's reports
    if (identical(task, "figure") && !inherits(pr, "error")) {
      a$outputs <- stats::setNames(list(a$design), a$output_id %||% ids[1L])
    }
    if (inherits(pr, "error")) {
      rows[[nm]] <- row(nm, ids, "error", 1L, conditionMessage(pr))
      nxt[nm] <- if (agent) {
        sprintf("answer the request again; save it over %s%s", rel(answers, basename(f)), run)
      } else sprintf("ask the chat for the answer again; save it over %s",
                     rel(answers, basename(f)))
      next
    }
    err <- pr[pr$severity == "error", , drop = FALSE]
    wrote <- NULL
    if (nrow(err)) {
      dir.create(file.path(study_dir, repairs), recursive = TRUE, showWarnings = FALSE)
      # each answer that needed a repair, once (tfl_ai_compare() counts them)
      .ai_repair_log(file.path(study_dir, repairs), nm, f)
      .ai_write_text(format(tfl_ai_repair_prompt(a, pr)),
                     file.path(study_dir, repairs, paste0(nm, ".md")))
      nxt[nm] <- if (agent) {
        sprintf("fix the answer as %s says; save the whole answer over %s%s",
                rel(repairs, paste0(nm, ".md")), rel(answers, basename(f)), run)
      } else sprintf("paste %s into the SAME chat; save its new answer over %s",
                     rel(repairs, paste0(nm, ".md")), rel(answers, basename(f)))
    } else {
      # only the reports this request asked for (another is warned of)
      keep <- a
      keep$outputs <- a$outputs[names(a$outputs) %in% ids]
      if (!is.null(keep$assumption_rows)) {
        r <- keep$assumption_rows
        keep$assumption_rows <- r[is.na(r$output_id) | r$output_id %in% ids, , drop = FALSE]
      }
      # (the ADaM's variables: a listing's columns given their widths)
      keep$vars <- ctxs[[nm]]$context$datasets
      if (identical(task, "figure")) {
        keep <- .ai_fig_as_files(a, ids, ctxs[[nm]]$context)
        if (!is.null(keep$error)) wrote <- keep$error
      }
      if (is.null(wrote)) {
        wrote <- tryCatch({
          if (length(keep$outputs)) tfl_ai_unpack(keep, study_dir, overwrite = overwrite)
          NULL
        }, error = function(e) conditionMessage(e))
      }
      if (is.null(wrote)) unlink(file.path(study_dir, repairs, paste0(nm, ".md")))
      nxt[nm] <- if (!is.null(wrote)) {
                   if (agent) sprintf("see the problem below, then run `Rscript run.R %s`", nm)
                   else "see the problem below, then collect again"
                 }
                 else if (nrow(pr)) "read the warnings; the report files are written"
                 else "done: the report files are written"
    }
    # each report's own problems; one naming no report is the request's
    per <- lapply(ids, function(id) {
      hit <- pr[grepl(id, pr$where, fixed = TRUE), , drop = FALSE]
      own <- pr[!vapply(pr$where, function(w) any(vapply(ids, grepl, NA, x = w,
                                                         fixed = TRUE)), NA), , drop = FALSE]
      p <- rbind(hit, own)
      missing <- !id %in% names(a$outputs)
      # (a sheet's: the reports are the AI's to decide, one asked for and
      # not answered a warning)
      soft <- missing && sheet
      st <- if ((missing && !soft) || any(p$severity == "error") || nrow(err) || !is.null(wrote)) "error"
            else if (nrow(p) || soft) "warning" else "ok"
      first <- if (missing) sprintf("not in the answer (it has %s)",
                                    if (length(a$outputs)) paste(names(a$outputs), collapse = ", ")
                                    else "no report")
               else if (!is.null(wrote)) wrote
               else if (nrow(p)) paste0(p$where[1L], ": ", p$problem[1L]) else NA_character_
      all <- c(if (missing || !is.null(wrote)) first,
               if (nrow(p)) paste0(p$where, ": ", p$problem))
      row(nm, id, st, nrow(p) + missing, first,
          if (length(all)) paste(all, collapse = "\n") else NA_character_)
    })
    rows[[nm]] <- do.call(rbind, per)
    if (any(names(a$outputs) %in% ids) && !all(ids %in% names(a$outputs)) && !nrow(err) &&
        is.null(wrote)) {
      nxt[nm] <- if (sheet) {
        sprintf(paste0("the report files are written; %s (the sheet's numbers) not in the answer: ",
                       "add them if the sheet makes them, or say why as an assumption"),
                paste(setdiff(ids, names(a$outputs)), collapse = ", "))
      } else sprintf(if (agent) "add %s to the answer; save the whole answer over %s%s"
                         else "ask the same chat for %s too; save the whole answer over %s%s",
                         paste(setdiff(ids, names(a$outputs)), collapse = ", "),
                         rel(answers, basename(f)), if (agent) run else "")
    } else if (!any(names(a$outputs) %in% ids) && !nrow(err)) {
      # none of the reports asked for: nothing was written
      nxt[nm] <- sprintf(paste0("the answer has none of %s (it has %s): %s; save the whole ",
                                "answer over %s%s"),
                         paste(ids, collapse = ", "),
                         if (length(a$outputs)) paste(names(a$outputs), collapse = ", ") else "no report",
                         if (agent) "answer them" else "ask the same chat for them",
                         rel(answers, basename(f)), if (agent) run else "")
    }
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out$type <- vapply(out$request, function(r) ctxs[[r]]$context$task %||% "table", "",
                     USE.NAMES = FALSE)
  out <- out[c("request", "type", setdiff(names(out), c("request", "type")))]
  # answers that match no request (a name typed differently)
  got <- list.files(file.path(study_dir, answers), "[.](md|txt)$", ignore.case = TRUE)
  stray <- got[!sub("([.]md)?([.]txt)?$", "", got, ignore.case = TRUE) %in% names(ctxs)]
  # each report's file (when written) and its request's repair prompt (when one waits)
  yml <- rel(.ai_area(study_dir) %||% file.path("spec", "outputs"), paste0(out$output_id, ".yml"))
  out$file <- ifelse(out$status %in% c("ok", "warning") & file.exists(file.path(study_dir, yml)),
                     yml, NA_character_)
  rp <- rel(repairs, paste0(out$request, ".md"))
  out$repair <- ifelse(file.exists(file.path(study_dir, rp)), rp, NA_character_)
  asm <- .ai_folder_assumptions(study_dir)
  names(asm)[names(asm) == "sheet"] <- "where"
  asm <- asm[is.na(asm$output_id) | asm$output_id %in% out$output_id, , drop = FALSE]
  rownames(asm) <- NULL
  # each request's setting: answered with templates where they fit, or not
  tpl <- vapply(ctxs, function(x) isTRUE(x$context$ai_templates), NA)
  # the TOC items the requests were written from (sheet requests: their
  # keys; NA: not known)
  keys <- unique(unlist(lapply(ctxs, function(x) x$context$toc_keys), use.names = FALSE))
  # the manifest: every report the answers say they make, for the person to
  # check the count against the TOC (ai/manifest.csv)
  man <- .ai_manifest(out, listed)
  mf <- file.path(dirname(file.path(study_dir, requests)), "manifest.csv")
  utils::write.csv(man, mf, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  structure(list(reports = out, next_steps = nxt[unique(out$request)], assumptions = asm,
                 stray = rel(answers, stray), requests = requests, templates = tpl,
                 toc_items = if (length(keys)) length(keys[!is.na(keys)]) else NA_integer_,
                 manifest = man, manifest_file = rel(dirname(requests), "manifest.csv")),
            class = "tfl_ai_collected")
}

# A sheet's request's reports, as its answer names them: those it was asked
# for (none: the sheet's name stands in, dropped once the answer has
# reports), and every other report the answer writes from the sheet but one
# another sheet's request was asked for (that one's; warned of, not written)
.ai_sheet_answer_ids <- function(ids, a, request, own) {
  outs <- names(a$outputs)
  if (!length(outs)) return(ids)
  mine <- own[[request]]
  if (!length(mine)) ids <- character()
  others <- unlist(own[setdiff(names(own), request)], use.names = FALSE)
  ok <- .ai_id_key(others)
  theirs <- vapply(outs, function(o) {
    k <- .ai_id_key(o)
    o %in% others || (!is.na(k) && any(vapply(ok, .ai_id_same, NA, b = k)))
  }, NA)
  unique(c(ids, outs[!outs %in% ids & !theirs]))
}

# An answer's manifest rows: the reports its `reports` lists (the ids as
# its reports are named, the request's ids), then a report it writes but
# does not list
.ai_manifest_rows <- function(request, a) {
  rl <- a$report_list
  if (is.null(rl)) rl <- .ai_report_list_rows(NULL)
  outs <- names(a$outputs)
  same <- function(x, y) {
    identical(x, y) || .ai_id_same(.ai_id_key(x), .ai_id_key(y))
  }
  rl$output_id <- vapply(rl$output_id, function(id) {
    hit <- outs[vapply(outs, same, NA, y = id)]
    if (length(hit)) hit[1L] else id
  }, "", USE.NAMES = FALSE)
  rl$listed <- rep(TRUE, nrow(rl))
  more <- setdiff(outs, rl$output_id)
  if (length(more)) {
    lab <- vapply(more, function(id) .ai_scalar_text(a$outputs[[id]]$label) %||% NA_character_,
                  "", USE.NAMES = FALSE)
    rl <- rbind(rl, data.frame(output_id = more, title = lab, split = NA_character_,
                               toc = NA_character_, listed = FALSE,
                               stringsAsFactors = FALSE))
  }
  if (!nrow(rl)) return(NULL)
  cbind(request = request, rl, stringsAsFactors = FALSE)
}

# The study's manifest: one row a report -- each answer's list, and the
# reports of a request with no answer (or one not read) as asked; with the
# report's status from the collection ("not answered": listed, but no
# report file in the answer)
.ai_manifest <- function(out, listed) {
  rows <- do.call(rbind, c(list(data.frame(
    request = character(), output_id = character(), title = character(),
    split = character(), toc = character(), listed = logical(),
    stringsAsFactors = FALSE)), unname(listed)))
  rest <- out[!out$request %in% rows$request, , drop = FALSE]
  if (nrow(rest)) {
    rows <- rbind(rows, data.frame(request = rest$request, output_id = rest$output_id,
                                   title = NA_character_, split = NA_character_,
                                   toc = NA_character_, listed = NA,
                                   stringsAsFactors = FALSE))
  }
  key <- paste(out$request, out$output_id, sep = " | ")
  st <- out$status[match(paste(rows$request, rows$output_id, sep = " | "), key)]
  rows$status <- ifelse(is.na(st), "not answered", st)
  # in the requests' order
  rows <- rows[order(match(rows$request, unique(out$request))), , drop = FALSE]
  rownames(rows) <- NULL
  rows[c("request", "toc", "output_id", "title", "split", "listed", "status")]
}

# An answer's reports under the ids its request asked for: a report named
# another way (T-14-1-1 where the TOC writes Table 14.1.1) is the one whose
# number it is; its assumptions follow it
.ai_answer_as_asked <- function(a, ids) {
  if (!length(a$outputs)) return(a)
  ik <- .ai_id_key(ids)
  nm <- names(a$outputs)
  for (i in seq_along(nm)) {
    if (nm[i] %in% ids) next
    k <- .ai_id_key(nm[i])
    j <- if (!is.na(k)) .ai_id_match(k, ik) else integer()
    if (length(j) != 1L || ids[j] %in% nm) next
    new <- ids[j]
    a$outputs[[i]]$id <- new
    r <- a$assumption_rows
    if (!is.null(r)) r$output_id[r$output_id %in% nm[i]] <- new
    a$assumption_rows <- r
    nm[i] <- new
  }
  names(a$outputs) <- nm
  a
}

# An overall row (normalize_ard(overall = )) is made by a hierarchical
# analysis with over_variables = TRUE: an answer that names the row and not
# that gets it written into its analyses (the report file says it, so the
# ARD and the file agree), and an assumption says so
.ai_answer_overall <- function(a) {
  for (i in seq_along(a$outputs)) {
    o <- a$outputs[[i]]
    nz <- Filter(function(it) identical(it$fn, "normalize_ard"), o$content %||% list())
    if (!length(nz) || is.null(nz[[1L]]$args$overall)) next
    an <- o$ard$analyses %||% list()
    hier <- which(vapply(an, function(x) {
      is.list(x) && isTRUE(.ai_scalar_text(x[["method"]]) %in%
                             c("hierarchical", "cards::ard_stack_hierarchical"))
    }, NA))
    if (!length(hier)) next
    has <- vapply(an[hier], function(x) {
      grepl("over_variables[[:space:]]*=[[:space:]]*TRUE", paste(unlist(x[["args"]]), collapse = " "))
    }, NA)
    if (any(has)) next
    j <- hier[1L]
    old <- an[[j]][["args"]]
    an[[j]][["args"]] <- tfl_r(if (is.null(old) || !length(old)) "over_variables = TRUE" else
      paste0(paste(unlist(old), collapse = ", "), ", over_variables = TRUE"))
    a$outputs[[i]]$ard$analyses <- an
    row <- data.frame(output_id = names(a$outputs)[i], where = "ard.analyses",
                      item = an[[j]][["id"]] %||% "over_variables",
                      assumption = paste("tflspec added over_variables = TRUE: the overall row",
                                         "(normalize_ard overall) needs it"),
                      question = NA_character_, stringsAsFactors = FALSE)
    a$assumption_rows <- rbind(a$assumption_rows %||% row[0L, , drop = FALSE], row)
  }
  a
}

# The answer file of a request: <request>.md; else the names Notepad saves
# it as (<request>.md.txt, <request>.txt).  NA when there is none.
.ai_answer_file <- function(dir, request) {
  f <- file.path(dir, paste0(request, c(".md", ".md.txt", ".txt", ".MD")))
  f <- f[file.exists(f)]
  if (length(f)) f[1L] else NA_character_
}

#' A study's requests, read again
#'
#' The requests [tfl_ai_prepare()] / [tfl_ai_write_requests()] wrote to
#' `ai/requests`, as they returned them -- for a screen opened again,
#' without preparing them anew.
#'
#' @param study_dir The study folder.
#' @param requests The requests' folder, under `study_dir`.
#' @return A data frame, one row a request: `request`, `family`, `outputs`
#'   (the report ids, comma-separated), `n`, `chars`, `file` (relative to
#'   `study_dir`); no row when none were written.
#' @seealso [tfl_ai_collect_answers()] for their answers.
#' @export
tfl_ai_requests <- function(study_dir, requests = file.path("ai", "requests")) {
  cf <- file.path(study_dir, requests, "_contexts.rds")
  rel <- function(...) gsub("\\\\", "/", file.path(...))
  if (!file.exists(cf)) {
    return(data.frame(request = character(), type = character(), family = character(),
                      outputs = character(),
                      n = integer(), chars = integer(), file = character(),
                      book = character(), sheets = character(), stringsAsFactors = FALSE))
  }
  ctxs <- readRDS(cf)
  nm <- names(ctxs)
  file <- rel(requests, paste0(nm, ".md"))
  book <- vapply(ctxs, function(x) x$book %||% NA_character_, "", USE.NAMES = FALSE)
  chars <- vapply(file.path(study_dir, file), function(f) {
    if (!file.exists(f)) return(NA_integer_)
    nchar(paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
  }, 0L, USE.NAMES = FALSE)
  out <- data.frame(
    request = nm,
    type = vapply(ctxs, function(x) x$context$task %||% "table", "", USE.NAMES = FALSE),
    family = vapply(ctxs, function(x) x$family %||% NA_character_, "", USE.NAMES = FALSE),
    outputs = vapply(ctxs, function(x) paste(x$ids, collapse = ", "), "", USE.NAMES = FALSE),
    n = vapply(ctxs, function(x) length(x$ids), 0L, USE.NAMES = FALSE),
    chars = chars, file = file, book = book,
    # (the shell sheets a request is of: tfl_ai_read_shell()$shells)
    sheets = vapply(ctxs, function(x) {
      s <- x$context$sheets
      if (length(s)) paste(s, collapse = " | ") else NA_character_
    }, "", USE.NAMES = FALSE),
    stringsAsFactors = FALSE)
  rownames(out) <- NULL
  out
}

#' Save a chat's answer to a request where tfl_ai_collect_answers() reads it
#'
#' Writes the answer (the chat's whole reply, as copied) to
#' `ai/answers/<request>.md` in UTF-8 -- the name and the encoding
#' [tfl_ai_collect_answers()] reads -- for a program or a screen that takes
#' the pasted text, rather than the person saving a file.
#'
#' @param study_dir The study folder.
#' @param request The request's name (`ae-1`: a row of the requests'
#'   `request`, `ai/requests/index.md`).
#' @param text The answer: one string, or lines.
#' @param overwrite `TRUE` (default) to write over an answer saved before
#'   (a repaired answer replaces the first).
#' @param requests,answers The folders, under `study_dir`.
#' @return The file's path, invisibly.
#' @seealso [tfl_ai_collect_answers()], [tfl_ai_prepare()].
#' @export
tfl_ai_save_answer <- function(study_dir, request, text, overwrite = TRUE,
                               requests = file.path("ai", "requests"),
                               answers = file.path("ai", "answers")) {
  fn <- "tfl_ai_save_answer()"
  cf <- file.path(study_dir, requests, "_contexts.rds")
  if (!file.exists(cf)) {
    .ard_stop(sprintf("%s: no requests in %s; write them first (tfl_ai_prepare()).", fn,
                      sQuote(file.path(study_dir, requests), FALSE)))
  }
  known <- names(readRDS(cf))
  if (!is.character(request) || length(request) != 1L || !request %in% known) {
    .ard_stop(sprintf("%s: `request` is one of the study's requests: %s.", fn,
                      paste(known, collapse = ", ")))
  }
  if (!is.character(text) || !length(text) || anyNA(text) || !any(nzchar(trimws(text)))) {
    .ard_stop(sprintf("%s: `text` is the chat's answer (text, not empty).", fn))
  }
  f <- file.path(study_dir, answers, paste0(request, ".md"))
  if (file.exists(f) && !isTRUE(overwrite)) {
    .ard_stop(sprintf("%s: %s already there; pass `overwrite = TRUE`.", fn,
                      sQuote(file.path(answers, basename(f)), FALSE)))
  }
  # (<request>.md is read before a .md.txt / .txt Notepad may have left)
  .ai_write_text(gsub("\r\n?", "\n", enc2utf8(paste(text, collapse = "\n"))), f)
  invisible(normalizePath(f, winslash = "/"))
}

#' @export
print.tfl_ai_collected <- function(x, ...) {
  obj <- x
  x <- obj$reports
  tab <- table(factor(x$status, c("ok", "warning", "error", "no answer")))
  req <- unique(x$request)
  cat(sprintf("%d reports in %d requests: %s\n", nrow(x), length(req),
              paste(sprintf("%s %d", names(tab), tab), collapse = ", ")))
  # a request's status: its worst report's
  rank <- c(ok = 1L, warning = 2L, `no answer` = 3L, error = 4L)
  st <- vapply(req, function(r) {
    s <- x$status[x$request == r]
    s[which.max(rank[s])]
  }, "")
  nxt <- obj$next_steps
  outs <- vapply(req, function(r) paste(x$output_id[x$request == r], collapse = ", "), "")
  w <- max(nchar(req))
  cat("\n")
  for (i in seq_along(req)) {
    cat(sprintf("%-*s  %-9s  %s\n", w, req[i], st[i], outs[i]))
    step <- if (is.null(nxt)) NA_character_ else unname(nxt[req[i]])
    if (!is.na(step)) cat(sprintf("%-*s  next: %s\n", w, "", step))
  }
  todo <- x[x$status %in% c("error", "warning"), , drop = FALSE]
  if (nrow(todo)) {
    cat("\nProblems (the first of each report):\n")
    print(as.data.frame(todo[c("request", "output_id", "status", "first")]),
          row.names = FALSE, right = FALSE)
  }
  man <- obj$manifest
  if (!is.null(man) && nrow(man)) {
    toc <- unique(man$toc[!is.na(man$toc)])
    cat(sprintf("\nManifest (%s): %d report(s)%s", obj$manifest_file %||% "ai/manifest.csv",
                nrow(man), if (length(toc)) sprintf(" from %d TOC row(s)", length(toc)) else ""))
    odd <- c(if (any(man$status == "not answered"))
               sprintf("%d listed but not answered", sum(man$status == "not answered")),
             if (any(man$listed %in% FALSE))
               sprintf("%d answered but not listed", sum(man$listed %in% FALSE)))
    cat(if (length(odd)) sprintf(" -- %s", paste(odd, collapse = ", ")) else "", "\n", sep = "")
  }
  stray <- obj$stray
  if (length(stray)) {
    cat(sprintf(paste0("\nNot read (no request of that name): %s -- rename each to ",
                       "<request>.md, the names in %s/index.md.\n"),
                paste(stray, collapse = ", "), obj$requests %||% "ai/requests"))
  }
  if (all(x$status %in% c("ok", "warning"))) {
    cat("\nAll answers read: next, build the RTF files (the guide's step 6).\n")
  } else if (any(x$status == "error")) {
    cat("\nAfter each fix, run tfl_ai_collect_answers() again: it reads every answer anew.\n")
  }
  invisible(obj)
}
