# The AI table guide's step 3 in one call: the study folder, the ADaM
# summary, the TOC / SHELL workbook read, the requests.

#' Prepare a study's AI table requests in one call
#'
#' The steps before the first request, one after the other:
#' [tfl_ai_study_init()] (only when the study has no `study.yml` yet -- one
#' that is there is kept, so the call can be run again),
#' [tfl_ai_adam_summary()] (to `ai/adam_summary.md`),
#' [tfl_ai_read_shell()] (to `ai/toc_shell.md`; what it could not find is
#' the result's `hint`), `spec/toc.yml` (the TOC's reports in order, with
#' their section and analysis set, as tflplanner reads it; when the study
#' has none) and [tfl_ai_write_requests()].  Nothing is sent anywhere.
#'
#' @section The agent's work:
#' The requests are for a coding agent (GitHub Copilot's agent in VS Code,
#' and the like) that **asks the person in the chat** what defines each
#' report and is not settled -- the analysis set, a parameter, the
#' treatment variable, a denominator -- waits for the answers, keeps them
#' in `ai/decisions.md` ([tfl_ai_decisions_read()]) and applies them to
#' the reports after, then writes each report's **skeleton** as a draft,
#' `ai/spec/outputs/<id>.yml`: its data, its analyses, the items that make
#' its rows (or a template), its titles and footnotes -- nothing of the
#' look.  What it could not settle it marks under the draft's `ai:
#' confirm` ([tfl_ai_confirm()]).  The person checks the drafts
#' (tflplanner's "Check", [tfl_ai_collect_answers()]), finishes them in the
#' builder and adopts them ([tfl_ai_adopt()]).
#'
#' Besides the requests (each starting with the agent's steps and the
#' decisions that apply to its reports), and what every request would
#' repeat written once for the agent to read -- `ai/instructions-table.md`
#' and `ai/instructions-figure.md` (the instructions, the schema, what a
#' draft holds), `ai/templates.md` (with `templates = TRUE`: every
#' template, each request naming the ones likely for its reports),
#' `ai/adam_summary.md` and `spec/`, each request naming them -- the study
#' folder gets `ai/AGENTS.md` (the instructions: what the agent reads, how
#' it asks, what a draft holds), `AGENTS.md` and
#' `.github/copilot-instructions.md` (short: read `ai/AGENTS.md`), `run.R`
#' ([tfl_ai_run()]: the check from the command line, with the ARD and RTF)
#' and `ai/decisions.md`, which is never written over.  One message
#' (`ai/agent_prompt.md`) starts the agent on every request of
#' `ai/requests/index.md` in order; stopped, it goes on at "continue" from
#' the first request with a draft missing.
#'
#' `study.yml`'s `ai:` keys set the agent's rules (the study standard's
#' `spec/defaults.yml` may say them under its own `ai:` key first):
#' `language` (the chat's: `"en"`, `"ja"` ...), `ask_when` (`"request"`,
#' the default: the request's reports' questions together; `"report"`:
#' report by report), `carry` (which decision scopes apply to a later
#' report without asking: `all`, `dataset`, `section`), `max_rounds` (how
#' many times one item is asked again, 3), `footnotes_at` (`"footer"`, the
#' default, or `"table"`: where the shell's footnotes go).
#'
#' @param dir The study folder.
#' @param toc The TOC / SHELL workbook (`.xlsx` or `.xlsm`).
#' @param adam The ADaM folder, from `dir` (relative) or absolute.
#' @param study_id,title The study's id and title, for a new study folder.
#' @param toc_sheet,sheets As [tfl_ai_read_shell()]: the TOC sheet when the
#'   guess is wrong, the sheets to keep.
#' @param size,language,workbook,max_chars As [tfl_ai_write_requests()]:
#'   the reports a request holds (`NULL`, the default: 1), the language of
#'   the assumptions -- and of the agent's questions in the chat: it is
#'   written to `study.yml` as `ai: language` --, how the workbook reaches
#'   the agent (`"text"`,
#'   `"file"`; with `"workbook"` in `agent_reads`, `"text"` is `"file"`:
#'   the text and each shell's place in the workbook file), a request's
#'   length at most.
#' @param overwrite `TRUE` to start the study folder again
#'   ([tfl_ai_study_init()] over its `study.yml` and `spec/`).  The requests
#'   are written again on every call (the drafts are kept).
#' @param dry_run `TRUE` to read the workbook and the ADaM and write
#'   nothing: the same result, to look at before the requests are written
#'   (its `requests` are the TOC's batches, without `chars` and `file`;
#'   `files` is empty).
#' @param define,adam_spec The study's `define.xml`, or its ADaM spec
#'   workbook (Pinnacle 21 layout), for the ADaM summary
#'   ([tfl_ai_adam_summary()]): its code lists in their order with their
#'   decodes, and the digits and display formats, value level too (a
#'   parameter's AVAL), go into `ai/adam_summary.md` as the study's.
#'   `NULL` (default): the data alone.
#' @param adam_summary A [tfl_ai_adam_summary()] result to use instead of
#'   reading the ADaM again (a previous call's `$adam`); read again when
#'   `datasets` names others than it has.
#' @param datasets,max_levels As [tfl_ai_adam_summary()]: the ADaM datasets
#'   the requests describe (`NULL`: all), the values listed of a variable.
#'   Fewer datasets, shorter requests.
#' @param quiet `TRUE` to say nothing (the steps' messages): a screen shows
#'   the result's `hint` and `next_steps` itself.
#' @param agent_reads What the agent may open besides the requests and
#'   the files they name: `"workbook"` (the TOC / SHELL workbook: each
#'   request then says where its shells are in it) and `"adam"` (the
#'   ADaM), read only to settle what a request leaves unclear, with
#'   whatever tool the PC has (PowerShell, Python, R).  `character()`
#'   (default): neither, and `AGENTS.md` says not to open them.
#' @param docs Documents the agent may read for what a request leaves
#'   unclear (a SAP, a data guide): their paths.  They are copied into
#'   `ai/docs/` (the agent works in the study folder); a `.docx`, `.pdf`
#'   or workbook, which an agent in the editor cannot read, is also written
#'   as text beside it (`<name>.md`, [tfl_ai_doc_text()]: the headings and
#'   tables kept) and `AGENTS.md` names the text; the agent reads no other
#'   document.  `NULL` (default): none.
#' @param agent_max_reads How many times for a request the agent may open
#'   a file or run a command beyond the request and the files it names;
#'   then it asks, or marks what is unclear to confirm, and goes on.
#'   Default 3.
#' @param toc_cols The TOC's columns, when its headers do not say them (as
#'   [tfl_ai_read_shell()]): `c(output_id = "Output No", shell = "Shell",
#'   title = c("Title 1", "Title 2"))`, a header's text or a column's letter.
#' @param shells Workbooks holding the shells when `toc` holds the TOC
#'   alone (as [tfl_ai_read_shell()]): each report's shell is looked for in
#'   them; the requests and the `toc` say each report's `book`.
#' @param add `TRUE` to add this workbook's reports to the study's requests
#'   and drafts already there (a TOC of listings, a book of PK figures,
#'   one at a time): the new requests are named after those there
#'   (`ae-2`), a report asked for already stops the call (`overwrite =
#'   TRUE` asks for it again), `spec/toc.yml` gets the new reports at its
#'   end, and the workbook's text is `ai/toc_shell-<workbook>.md`.  With
#'   `add`, `overwrite` is about the requests alone: the study's
#'   `study.yml`, `spec/data.yml`, `spec/defaults.yml` and the agent's
#'   files are kept as they are.
#' @param progress `NULL`, or a function(step, i, n) called as the work goes:
#'   `step` `"adam"` (the ADaM's summary), `"workbook"` (the TOC / SHELL
#'   read), then `"request"`, `i` of `n` requests written -- for a progress
#'   bar (shiny's `setProgress()`); see [tfl_ai_write_requests()].
#' @param templates `TRUE` (default): the agent writes a report with one of
#'   the study's templates where it fits (`template: {name, args}`, and the
#'   report's own content items for what it does not cover), else as a code
#'   SPEC; the requests (and `AGENTS.md`) list the templates registered now
#'   -- tflspec's, a company's, the study's variants.  `FALSE`: the code
#'   SPEC only, the requests as they were before templates.  The setting is
#'   each request's first line (`templates: TRUE`) and `ai/status.md`'s.
#' @param sheet_roles As [tfl_ai_read_shell()]: sheets' roles over the guess.
#' @param unit A request's unit: `"sheet"` (default) a shell sheet -- what
#'   the sheet writes first, then its name, then its TOC candidates (the
#'   TOC and the sheet names can hold mistakes), the reports and their ids
#'   the AI's to read from it -- and one request of the TOC items no sheet
#'   chose; `"toc"` the TOC's reports, as before (a report's shell found
#'   by its number).
#' @param area Where the agent's drafts go: `TRUE` (the default), their
#'   own folder `ai/spec/outputs/` (made here), built into `ai/output/`,
#'   which the person adopts into the study's `spec/outputs/`
#'   ([tfl_ai_adopt()]); `FALSE`, the folder is not made here (a study of
#'   the form before is moved with [tfl_ai_migrate()]).
#' @param numbers The person's check of the report numbers each shell sheet
#'   writes (see the result's `numbers`), over the rule (a number written
#'   in the sheet or its name, and a TOC item's, is a report of the sheet's
#'   request): a data frame with `sheet`, `number` and `make` -- `FALSE`,
#'   the number is a reference only (the request still shows it, not as
#'   one of its reports); `TRUE`, a report of that sheet's request (a TOC
#'   item the rule left a candidate, or a number no TOC item has, which is
#'   warned of and keeps its number as the sheet writes it as its id);
#'   `NA`, back to the rule -- or a list named by sheet, each the numbers
#'   that sheet makes (its other numbers references only).  The numbers and
#'   sheets not given keep the rule.  The result's `toc` follows: a TOC
#'   item made in a sheet has that sheet as its shell (made in several,
#'   the first in the workbook's order; the others listed in `sheets`), one
#'   the rule made and no sheet makes now has none (the no-shell request's,
#'   counted in `counts`).  Saved as `ai/sheet_numbers.csv`
#'   (`sheet`, `number`, `make`, UTF-8) when the requests are written (not
#'   by a dry run), and read by a later call; given again, it goes over the
#'   saved rows (a row of the same sheet and number replaced, `NA` taking
#'   it out) and the file keeps the result.  `NULL` (default): the saved file, if any.
#'   With `unit = "sheet"` only.
#' @return A `tfl_ai_prepared`: a list with `requests` (one row a request:
#'   `request`, `family`, `outputs` -- the TOC items it names (a sheet's:
#'   its candidates) --, `n`, `chars`, `file`, `sheets` -- as
#'   [tfl_ai_write_requests()] gives them), `toc` (one row a TOC item:
#'   `toc_id`, `title`, `type`, `toc_sheets`, `sheets` -- the shell sheets
#'   it is a candidate of --, `shell`, `shell_found`, `request` -- the
#'   requests naming it, `NA` for none --, `book`, `shell_id`,
#'   `population`, `program`, `file`, `info` as [tfl_ai_read_shell()]'s
#'   `toc` says them), `shells` (a row a shell sheet, as
#'   [tfl_ai_read_shell()]'s, with its `request`), `numbers` (a row a
#'   report number a shell sheet writes or its name has: `sheet`, `number`
#'   as the sheet writes it, `title` -- the TOC item's, else the sheet's
#'   own when it writes that number alone, else `NA` --, `toc_id` -- the
#'   TOC item it is, `NA` none --, `make` -- a report of that sheet's
#'   request --, `source` -- `"rule"`, `"guess"` (a TOC item no sheet
#'   writes or names, whose number -- a part letter taken off, `15.2.4.2.b`
#'   -- sheets write, each number (a part with its letter) on its own:
#'   made the best sheet's, its id the number the sheet
#'   writes, the other sheets' a candidate each, not made; the best: an
#'   exact number before one a part letter differs, in the sheet's title
#'   rows or name before its body only, then the sheet whose own number is
#'   nearest, then the first in the workbook), `"saved"`
#'   (`ai/sheet_numbers.csv`) or `"given"` (`numbers`) --, `reason` -- a
#'   guess's: how it matches, "not an exact match" when not, its choice
#'   among the sheets; `"set by the person"` for `"saved"` and `"given"`;
#'   `NA` the rule's --; a dry run's and the written requests' alike),
#'   `counts` (`shells`,
#'   `toc_items`, `no_shell`),
#'   `sheets` (the workbook's sheets and their roles), `adam` (the
#'   [tfl_ai_adam_summary()]: `datasets`, `variables`, `text`), `files` (the
#'   files written, relative to `dir`), `hint` (what the workbook's reading
#'   could not find -- no TOC, a report with no shell -- and what to pass),
#'   `next_steps` (what to do next), `agent_prompt` (the first message to
#'   the agent, its paths from the study folder (a copy of the study says
#'   the same), also written to `ai/agent_prompt.md`) and `agent_files`
#'   (what the agent may open besides the requests and the files they
#'   name, from the study folder: the workbook and the ADaM as
#'   `agent_reads` allows, and the documents; `character()` else).
#'   Its print is the workbook's summary and the next step.  The files are
#'   UTF-8.
#' @seealso The AI table guide: `system.file("ai", "tflspec-ai-table-guide.md",
#'   package = "tflspec")`.
#' @examples
#' \donttest{
#' # (over 5 seconds: the workbook read, five ADaM written, every request)
#' ex <- system.file("ai", "example", package = "tflspec")
#' d <- file.path(tempdir(), "prepare-study")
#' dir.create(file.path(d, "adam"), recursive = TRUE, showWarnings = FALSE)
#' if (requireNamespace("haven", quietly = TRUE)) {
#'   for (x in c("adsl", "adae", "adrs", "adlb", "adtte")) {
#'     df <- getExportedValue("tflspec", x)
#'     for (v in names(df)) if (is.factor(df[[v]])) df[[v]] <- as.character(df[[v]])
#'     haven::write_xpt(df, file.path(d, "adam", paste0(x, ".xpt")))
#'   }
#'   r <- tfl_ai_prepare(d, file.path(ex, "onco_toc_shell.xlsx"),
#'                       study_id = "XYZ-ONC-01", overwrite = TRUE)
#'   r$requests[c("request", "outputs", "chars")]
#' }
#' }
#' @export
tfl_ai_prepare <- function(dir, toc, adam = "adam", study_id = "STUDY-01", title = NULL,
                           toc_sheet = NULL, sheets = NULL, size = NULL, language = NULL,
                           workbook = c("text", "file"), max_chars = NULL,
                           overwrite = FALSE, dry_run = FALSE, adam_summary = NULL,
                           datasets = NULL, max_levels = 15L, quiet = FALSE,
                           shells = NULL, add = FALSE,
                           templates = TRUE, progress = NULL, toc_cols = NULL,
                           sheet_roles = NULL, unit = c("sheet", "toc"), area = TRUE,
                           numbers = NULL, agent_reads = character(),
                           docs = NULL, agent_max_reads = 3L,
                           define = NULL, adam_spec = NULL) {
  unit <- match.arg(unit)
  if (isTRUE(quiet)) {
    cl <- match.call()
    cl$quiet <- FALSE
    return(suppressMessages(eval(cl, parent.frame())))
  }
  fn <- "tfl_ai_prepare()"
  templates <- .ai_templates_flag(templates, fn)
  workbook <- match.arg(workbook)
  if (!is.character(toc) || length(toc) != 1L || !file.exists(toc)) {
    .ard_stop(sprintf("%s: `toc` is the TOC / SHELL workbook's path (.xlsx).", fn))
  }
  if (!is.null(adam_summary) && !inherits(adam_summary, "tfl_ai_adam")) {
    .ard_stop(sprintf("%s: `adam_summary` is a tfl_ai_adam_summary() result (a call's `$adam`).",
                      fn))
  }
  # the reports a request holds: one, its draft checked before the next
  size <- size %||% 1L
  # an agent allowed to open the workbook reads each shell's place in the
  # workbook file too; else the text alone
  agent_reads <- as.character(agent_reads %||% character())
  bad <- setdiff(agent_reads, c("workbook", "adam"))
  if (length(bad)) {
    .ard_stop(sprintf("tfl_ai_prepare(agent_reads = ): %s; it takes \"workbook\" and \"adam\".",
                      paste(sprintf("\"%s\"", bad), collapse = ", ")))
  }
  if (!is.numeric(agent_max_reads) || length(agent_max_reads) != 1L || is.na(agent_max_reads) ||
      agent_max_reads < 0) {
    .ard_stop("tfl_ai_prepare(agent_max_reads = ): one number, 0 or more.")
  }
  if (length(docs) && !all(file.exists(docs))) {
    .ard_stop(sprintf("tfl_ai_prepare(docs = ): no file %s.",
                      paste(sprintf("'%s'", docs[!file.exists(docs)]), collapse = ", ")))
  }
  if (identical(workbook, "text") && "workbook" %in% agent_reads) workbook <- "file"
  dry <- isTRUE(dry_run)
  ai <- file.path(dir, "ai")
  files <- character()
  agent_files <- character()
  if (!dry) {
    # the agent's drafts in their own folder
    if (isTRUE(area)) dir.create(file.path(ai, "spec", "outputs"), recursive = TRUE, showWarnings = FALSE)
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    # (added to a study: its study.yml and spec/ as they are, whatever
    # `overwrite`, which then says only the requests)
    if (!file.exists(file.path(dir, "study.yml")) || (isTRUE(overwrite) && !isTRUE(add))) {
      files <- tfl_ai_study_init(dir, adam = adam, study_id = study_id, title = title,
                                 overwrite = overwrite)
    }
    dir.create(ai, recursive = TRUE, showWarnings = FALSE)
    # the language given here is the chat's too: study.yml's ai: language
    if (!is.null(language)) .ai_study_yml_set(dir, "language", language)
  }
  adam_dir <- if (grepl("^(/|[A-Za-z]:)", adam)) adam else file.path(dir, adam)
  # (where it is: the ADaM's summary, the workbook, then a request at a time)
  step <- function(s, i = 0L, n = 1L) if (is.function(progress)) progress(s, i, n)
  step("adam")
  ad <- adam_summary
  if (is.null(ad) || (!is.null(datasets) &&
                      !setequal(toupper(datasets), toupper(ad$datasets$dataset)))) {
    ad <- tfl_ai_adam_summary(adam_dir, datasets = datasets, max_levels = max_levels,
                              define = define, adam_spec = adam_spec)
  }
  step("workbook")
  shell <- do.call(tfl_ai_read_shell, c(list(toc), Filter(Negate(is.null), list(
    toc_sheet = toc_sheet, sheets = sheets, shells = shells, toc_cols = toc_cols,
    sheet_roles = sheet_roles))))
  # the report numbers each shell sheet writes, and which its request makes:
  # the person's over the rule's: the saved in the study, the given over
  # them (NA: back to the rule) -- one mapping for the numbers, the TOC
  # view, the requests and the counts
  if (!is.null(shell$shells)) {
    shells_at <- shell$shells$sheet
    rule <- .ai_sheet_numbers_rule(shell)
    given <- if (!is.null(numbers)) .ai_sheet_numbers_input(numbers, rule, shells_at, fn)
    over <- .ai_sheet_numbers_merge(.ai_sheet_numbers_read(dir, shells_at), given)
    if (identical(unit, "sheet")) {
      shell <- .ai_sheet_numbers_apply(shell, over, fn)
    }
    nums <- shell$sheet_numbers %||% rule
    # (given: saved for the next call when the requests are written)
    if (!dry && !is.null(numbers)) {
      files <- c(files, .ai_sheet_numbers_write(dir, over))
    }
  } else nums <- .ai_sheet_numbers_rule(shell)
  attr(nums, "guess") <- NULL
  # (what the workbook's reading could not find, as the numbers left it)
  hint <- .ai_shell_hint(shell)
  if (dry && identical(unit, "sheet") && !is.null(shell$shells)) {
    # the shell sheets' requests (as written)
    sz <- size %||% 1L
    req <- .ai_sheet_plan_table(.ai_sheet_plan(shell, sz, c("table", "listing", "figure")))
    rownames(req) <- NULL
    nxt <- "look at the TOC and the requests; then run again with dry_run = FALSE to write them"
  } else if (dry) {
    # the tables and listings in batches, each figure a request of its own
    # (as written)
    b <- if (nrow(shell$toc)) tfl_ai_batches(shell, size = size, types = c("table", "listing")) else
      data.frame(output_id = character(), family = character(), batch = character())
    figs <- shell$toc$output_id[shell$toc$type %in% "figure"]
    if (length(figs)) {
      b <- b[!b$output_id %in% figs, , drop = FALSE]
      b <- rbind(b[c("output_id", "family", "batch")],
                 data.frame(output_id = figs, family = "figure",
                            batch = paste0("figure-", seq_along(figs)), stringsAsFactors = FALSE))
    }
    # (one report a request: named after it, as the written ones)
    if (isTRUE(as.integer(size) == 1L) && nrow(b)) {
      fg <- b$family == "figure"
      one <- !fg & !duplicated(b$batch)
      nm <- .ai_file_id((b$shell_id %||% b$output_id)[one])
      b$batch[!fg] <- unname(stats::setNames(nm, b$batch[one])[b$batch[!fg]])
      b$batch[fg] <- .ai_file_id(b$output_id[fg])
      b$batch <- make.unique(b$batch, sep = "-")[match(b$batch, b$batch)]
    }
    bt <- unique(b$batch)
    req <- data.frame(
      request = bt, type = ifelse(b$family[match(bt, b$batch)] == "figure", "figure", "table"),
      family = b$family[match(bt, b$batch)],
      outputs = vapply(bt, function(x) paste(b$output_id[b$batch == x], collapse = ", "), ""),
      n = vapply(bt, function(x) sum(b$batch == x), 0L),
      chars = rep(NA_integer_, length(bt)), file = rep(NA_character_, length(bt)),
      stringsAsFactors = FALSE)
    rownames(req) <- NULL
    nxt <- "look at the TOC and the requests; then run again with dry_run = FALSE to write them"
  } else {
    .ai_write_text(format(ad), file.path(ai, "adam_summary.md"))
    # a workbook added: its text beside the first's
    shell_md <- if (isTRUE(add)) {
      paste0("toc_shell-", tools::file_path_sans_ext(basename(toc)), ".md")
    } else "toc_shell.md"
    .ai_write_text(format(shell), file.path(ai, shell_md))
    req <- tfl_ai_write_requests(dir, shell, ad, size = size, language = language,
                                 max_chars = max_chars, workbook = workbook,
                                 overwrite = if (isTRUE(add)) overwrite else TRUE, add = add,
                                 templates = templates, progress = progress, unit = unit)
    rq <- file.path(dir, "ai", "requests")
    # what a draft holds (the skeleton, the footnotes' place), after the
    # instructions every request shares
    .ai_instructions_spec(dir)
    files <- c(files, file.path(ai, c("adam_summary.md", shell_md)),
               .ai_toc_yml(dir, shell, overwrite = overwrite, add = add))
    {
      # where the ADaM and the workbook are, from the study folder
      adam_at <- .ai_from_study(dir, adam_dir)
      book_at <- .ai_from_study(dir, toc)
      # each request says the agent's steps at its head, then the decisions
      # that apply to its reports
      for (i in seq_len(nrow(req))) {
        f <- file.path(dir, req$file[i])
        ids <- strsplit(req$outputs[i], ", ", fixed = TRUE)[[1L]]
        dec <- .ai_decisions_block(dir, ids, section = .ai_request_section(shell, req$outputs[i]))
        txt <- c(.ai_agent_request_head(req$request[i], ids = ids), dec,
                 readLines(f, warn = FALSE, encoding = "UTF-8"))
        .ai_write_text(txt, f)
        req$chars[i] <- nchar(paste(txt, collapse = "\n"))
      }
      # the documents the person gave (a SAP, ...), copied into the study
      # folder: the agent's workspace
      refs <- character()
      docs_from <- character()
      if (length(docs)) {
        placed <- .ai_docs_place(dir, docs)
        refs <- placed$refs
        docs_from <- placed$from
        files <- c(files, placed$files)
      }
      files <- c(files, .ai_agent_files(dir, overwrite = overwrite && !isTRUE(add),
                                        templates = templates, adam = adam_at,
                                        book = book_at, reads = agent_reads,
                                        docs = refs, docs_from = docs_from,
                                        max_reads = agent_max_reads))
      # what the agent may open besides the requests and the files they name
      agent_files <- c(if ("workbook" %in% agent_reads) book_at,
                       if ("adam" %in% agent_reads) adam_at, refs)
      # what every request would repeat, written once
      files <- c(files, list.files(ai, "^(instructions-.*|templates)[.]md$", full.names = TRUE))
    }
    files <- c(files, list.files(rq, "[.](md|rds)$", full.names = TRUE))
    nxt <- c(
      sprintf("open %s in VS Code (that folder: the agent reads its AGENTS.md)",
              normalizePath(dir, winslash = "/")),
      paste0("in the agent's chat, one message: ai/agent_prompt.md; answer its questions ",
             "there; \"continue\" when it stopped"),
      paste0("check the drafts: tflplanner's \"Check\" (tfl_ai_collect_answers()), ",
             "finish them in the builder, adopt them (tfl_ai_adopt())"))
  }
  # each report of the TOC, its shell and the request asking for it
  t <- shell$toc
  asked <- strsplit(req$outputs, ", ", fixed = TRUE)
  who <- rep(req$request, lengths(asked))
  # a TOC item a row: its number (toc_id), the TOC sheets listing it, the
  # shell sheets it is a candidate of (NA: no shell found), the requests
  # naming it; the row's columns
  na <- rep(NA_character_, nrow(t))
  # (a request asking for a part of it, 15.2.4.2.b, names it too)
  part_keys <- lapply(asked, function(a) {
    k <- .ai_id_key(a)
    sub("[A-Z]$", "", k[!is.na(k) & grepl("[A-Z]$", k)])
  })
  req_of <- vapply(t$output_id, function(id) {
    k <- .ai_id_key(id)
    w <- req$request[vapply(seq_along(asked), function(j) {
      id %in% asked[[j]] || any(vapply(part_keys[[j]], .ai_id_same, NA, b = k))
    }, NA)]
    if (length(w)) paste(w, collapse = " | ") else NA_character_
  }, "", USE.NAMES = FALSE)
  tc <- data.frame(toc_id = t$output_id, title = t$title, type = t$type,
                   toc_sheets = t$toc_sheets %||% na, sheets = t$sheets %||% t$shell,
                   shell = t$shell, shell_found = !is.na(t$shell), request = req_of,
                   book = t$book %||% rep(basename(toc), nrow(t)),
                   stringsAsFactors = FALSE)
  for (k in c("shell_id", "population", "program", "file", "info")) tc[[k]] <- t[[k]] %||% na
  # a shell sheet a row: what it says of itself, its TOC candidates, its
  # request; guessed: no number and no candidate (a shell by its content)
  shl <- shell$shells
  if (!is.null(shl)) {
    shl$request <- vapply(shl$sheet, function(s) {
      w <- req$request[vapply(strsplit(req$sheets %||% rep("", nrow(req)), " | ", fixed = TRUE),
                              function(v) s %in% v, NA)]
      if (length(w)) w[1L] else NA_character_
    }, "", USE.NAMES = FALSE)
  }
  counts <- c(shells = if (is.null(shl)) NA_integer_ else nrow(shl), toc_items = nrow(tc),
              no_shell = sum(!tc$shell_found))
  # the files written, relative to the study folder
  root <- paste0(normalizePath(dir, winslash = "/"), "/")
  files <- normalizePath(files, winslash = "/")
  inside <- startsWith(files, root)
  files[inside] <- substring(files[inside], nchar(root) + 1L)
  # the agent's first message: where its instructions, requests and run.R
  # are -- from the study folder, so a study copied to another PC (or
  # another drive) says the same
  # -- and the first one or two requests only: the person asks for the rest
  agent_prompt <- .ai_agent_prompt(basename(normalizePath(dir, mustWork = FALSE)),
                                   templates = templates,
                                   language = .ai_spec_settings(dir)$language)
  # and as a file, to copy from
  if (!dry) {
    .ai_write_text(agent_prompt, file.path(ai, "agent_prompt.md"))
    files <- c(files, "ai/agent_prompt.md")
  }
  structure(list(requests = req, toc = tc, shells = shl, counts = counts,
                 sheets = shell$sheets, numbers = nums, adam = ad,
                 files = unique(files), hint = hint, next_steps = nxt, dry_run = dry,
                 agent_prompt = agent_prompt, agent_files = agent_files,
                 templates = templates,
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
  if (!is.null(x$shells)) {
    cat(sprintf("%d shell sheet(s) (%d with no number or candidate), %d TOC item(s), %d with no shell found; %d ADaM dataset(s)\n",
                nrow(x$shells), sum(x$shells$guessed), nrow(x$toc), sum(!x$toc$shell_found),
                nrow(x$adam$datasets)))
  } else {
    cat(sprintf("%d report(s) in the TOC, %d with a shell found; %d ADaM dataset(s)\n",
                nrow(x$toc), sum(x$toc$shell_found), nrow(x$adam$datasets)))
  }
  r <- x$requests
  cat(sprintf("%d request(s)%s: %s\n", nrow(r), if (isTRUE(x$dry_run)) " to write" else "",
              paste(sprintf("%s (%d)", r$request, r$n), collapse = ", ")))
  if (length(x$hint)) cat(paste0("Note: ", x$hint, "\n"), sep = "")
  if (length(x$next_steps)) cat(paste0("Next: ", x$next_steps, "\n"), sep = "")
  invisible(x)
}
