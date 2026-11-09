# ============================================================================
#  The agent's work: it asks, the drafts are the answers (2026-10-29, the
#  AI flow made again)
# ----------------------------------------------------------------------------
#  tfl_ai_prepare(): the agent writes each report's skeleton as a draft
#  (ai/spec/outputs/<id>.yml) after asking the person in the chat what
#  defines the report and is not settled; it keeps the answers in
#  ai/decisions.md (one structured line a decision) and marks what it could
#  not settle under the draft's `ai: confirm`.  The person's check
#  (tfl_ai_collect_answers() / run.R) reads the drafts as the answers of
#  their requests -- the same checks, repairs, assumptions, status -- and
#  puts the drafts' `ai:` blocks back.
#
#    .ai_spec_settings()      study.yml's ai: keys over the defaults
#    tfl_ai_decisions_*()     ai/decisions.md read and written
#    .ai_decisions_block()    a request's "Decisions that may apply"
#    .ai_drafts_to_answers()  the drafts as the check's answers
#    tfl_ai_confirm()         what is marked to confirm
# ============================================================================

# ---- the settings ------------------------------------------------------------

# The agent's settings: the package's defaults, under study.yml's
# `ai:` keys (run, requests_per_chat, carry, max_rounds, language ...)
.ai_spec_defaults <- list(
  run = "one",                           # "one": one request, its questions, its build, then a menu; "batch": all the requests, the questions at the end
  requests_per_chat = 0L,               # stop after this many requests in one chat (0: no limit)
  carry = c("all", "dataset", "section"),  # a decision applies without asking when its scope is one of these (and the agent is sure)
  max_rounds = 3L,                      # asking again about one item: this many times, then "to confirm"
  footnotes_at = "footer",              # the shell's footnotes: the page's "footer" band, or the "table"'s footnotes
  language = "en",
  titles_in_header = character(),       # the output tokens the study standard's header prints (none: the report writes its titles)
  # how deep the agent checks each kind of report it built itself (run.R):
  # 0 the check, 1 + the program, 2 + the facts, 3 + the output; "none": the person's check
  review = list(figure = 3L, table = 2L, listing = 1L)
)

# The settings, under the study standard's `ai:` keys (spec/defaults.yml:
# the footnotes' place) and then study.yml's `ai:` keys
.ai_spec_settings <- function(study_dir) {
  s <- .ai_spec_defaults
  for (f in file.path(study_dir, c(file.path("spec", "defaults.yml"), "study.yml"))) {
    y <- if (file.exists(f)) tryCatch(yaml::read_yaml(f), error = function(e) NULL)
    if (is.list(y) && identical(basename(f), "defaults.yml") && is.list(y$page)) {
      s$titles_in_header <- .page_output_tokens_used(y$page)
    }
    ai <- if (is.list(y)) y$ai else NULL
    if (!is.list(ai)) next
    .ai_say_ask_when(ai, f)
    for (k in intersect(names(s), names(ai))) if (!is.null(ai[[k]])) {
      # (review: kind by kind, the ones not said as before)
      s[[k]] <- if (identical(k, "review") && is.list(ai[[k]])) utils::modifyList(s[[k]], ai[[k]]) else ai[[k]]
    }
  }
  s$max_rounds <- as.integer(s$max_rounds)
  s$carry <- as.character(unlist(s$carry))
  s$review <- .ai_review_levels(s$review)
  s$run <- .ai_run(s$run)
  s$requests_per_chat <- .ai_requests_per_chat(s$requests_per_chat)
  s$footnotes_at <- match.arg(as.character(s$footnotes_at)[1L], c("footer", "table"))
  s
}

# `ai: ask_when` was removed (#606; `run` replaces it) and is ignored: say so,
# once a session for each file, as the settings are read many times
.ai_said <- new.env(parent = emptyenv())
.ai_say_ask_when <- function(ai, file) {
  key <- normalizePath(file, winslash = "/", mustWork = FALSE)
  if (!"ask_when" %in% names(ai) || isTRUE(.ai_said[[key]])) return(invisible())
  .ai_said[[key]] <- TRUE
  message(basename(file), " ai: ask_when is ignored; `ai: run` (one or batch) replaces it.")
  invisible()
}

# study.yml ai: run, checked: "one" or "batch"
.ai_run <- function(x) {
  ok <- c("one", "batch")
  v <- as.character(unlist(x))[1L]
  if (is.na(v) || !v %in% ok) {
    .ard_stop(sprintf("study.yml ai: run is %s; not %s.", paste(ok, collapse = " or "),
                      format(if (is.na(v)) "" else v)))
  }
  v
}

# study.yml ai: requests_per_chat, checked: an integer >= 0
.ai_requests_per_chat <- function(x) {
  v <- suppressWarnings(as.numeric(unlist(x))[1L])
  if (is.na(v) || v < 0 || v != round(v)) {
    .ard_stop(sprintf("study.yml ai: requests_per_chat is a whole number, 0 or more; not %s.",
                      format(paste(unlist(x), collapse = " "))))
  }
  as.integer(v)
}

# AGENTS-spec.md filled from the settings.  AGENTS.md always holds both ways
# of running (AGENTS-run-one.md: one request, then the menu; AGENTS-run-batch.md:
# a batch, the questions at the end); `run` only decides how the agent starts
# (agent_prompt.md).  `runs` holds the two texts (named `one`, `batch`), else
# they are read from the package
.ai_spec_agents <- function(text, s, runs = NULL) {
  lang <- .ai_language_name(s$language)
  if (is.null(runs)) {
    src <- system.file("ai", "agent", package = "tflspec")
    rd <- function(f) paste(readLines(file.path(src, f), warn = FALSE, encoding = "UTF-8"),
                            collapse = "\n")
    runs <- list(one = rd("AGENTS-run-one.md"), batch = rd("AGENTS-run-batch.md"))
  }
  # what depends on the agent building and checking itself (review not none
  # everywhere): the texts of the two ways of running
  build <- any(!is.na(s$review))
  n <- s$requests_per_chat
  .ai_fill(text, list(
    run_one = runs$one,
    run_batch = runs$batch,
    run_except = if (build) ", except to run `../run.R` as *Build and check* says" else "",
    one_build = if (build) paste0(", then build\n   and check them at the study's review depth ",
                                  "(*Build and check*)") else "",
    batch_build = if (build) ", then build and check them (*Build and check*)" else "",
    batch_done = if (build) "built and checked" else "drafted",
    batch_defaults = if (build) paste0(
      "then **the defaults you chose yourself**, from the first section of\n",
      "  `ai/defaults.md`, *For the person: the defaults the agent chose* (the\n",
      "  defaults whose source is no document -- your conventions -- merged by\n",
      "  topic across the reports): its groups 1 and 2 in full (*Changes the\n",
      "  numbers*, *Changes the rows or statistics*), then the count of the rest\n",
      "  (group 3 and the defaults a document gave), with \"see ai/defaults.md\".\n",
      "  `run.R` writes that file from the drafts' assumptions, the most important\n",
      "  first; read it before you post, and say in a line that **the defaults can\n",
      "  differ from run to run, so the person should read them too**") else paste0(
      "no list of the defaults you chose yourself: `ai/defaults.md` is written by\n",
      "  `run.R`, which you do not run, so do not read it or cite it; each default you\n",
      "  took is in its entry above"),
    rework_build = if (build) "Build and check only the reworked requests (*Build and check*)." else
      "Do not build or check them: the person runs the check.",
    per_chat = if (isTRUE(n > 0L)) sprintf(paste0(
      "\n- After %d requests in this chat, finish the current one, update ",
      "`ai/progress.md` and stop with \"Open a new chat and give it ai/agent_prompt.md ",
      "(it starts: First read AGENTS.md in this folder and follow it); ",
      "it goes on from ai/progress.md\"."), n) else "",
    language = lang,
    carry_section = if ("section" %in% s$carry) " or its TOC section" else "",
    footnotes_at = .ai_footnotes_text(s$footnotes_at),
    titles_rule = .ai_titles_text(s$titles_in_header)))
}

# The titles: the shell's (the TOC's when the shell has none for the report), as `page: titles` -- unless the study standard's
# header prints the label, the title and the population from the report's
# `label:` and `population:` (its tokens): then none, or they print twice
.ai_titles_text <- function(tokens) {
  if (!length(tokens)) return(paste0("`page: titles`, from the shell (the TOC's title only when none for this ",
                                  "report can be read from the shell; an assumption when it differs from `label`);"))
  sprintf(paste0("**no `page: titles`**: the study's header prints the report's label, title ",
                 "and population itself (%s) from `label:` and `population:` -- write those two ",
                 "keys (the title as the shell shows it, in `label:`), and no titles, or they print twice;"),
          paste0("{", tokens, "}", collapse = ", "))
}

# Where the shell's footnotes go, as the study standard says: the page's
# footer band (`page: footer`, the package's default) or the table's own
# footnotes (`page: footnotes`)
.ai_footnotes_text <- function(at) {
  if (identical(at, "table")) {
    paste0("this study puts them under the table: `page: footnotes: {\"1\": {left: <text>}, ",
           "\"2\": ...}` (the study's standard).")
  } else {
    paste0("this study puts them in the page's footer: `page: footer: {\"1\": {left: <text>}, ",
           "\"2\": ...}` (the study's standard; the study's own footer lines stay above them).")
  }
}

# a language's name, from its code or itself
.ai_language_name <- function(x) {
  x <- as.character(x %||% "en")[1L]
  names_ <- c(en = "English", ja = "Japanese", de = "German", fr = "French", es = "Spanish",
              zh = "Chinese", ko = "Korean", it = "Italian", pt = "Portuguese")
  if (tolower(x) %in% names(names_)) unname(names_[tolower(x)]) else x
}

# ---- decisions: ai/decisions.md -----------------------------------------------

#' The decisions of a study's AI drafts
#'
#' `ai/decisions.md` holds what the reports follow over the agent's own
#' choices, one line a decision: `- D<n> [<scope>] <topic>: <decision>
#' (<source>)`.  `scope` is `all`, a dataset, a TOC section or report ids;
#' `topic` the item decided.  The agent adds the person's chat answers
#' there ([tfl_ai_prepare()]) and applies
#' a decision to a later report when its scope covers it.  A line of the
#' older form, `- Q<n> (<reports>): <decision>`, is read too (its scope the
#' reports, no topic), and a person's own line without a scope is `all`.
#'
#' `tfl_ai_decisions_read()` reads the file; `tfl_ai_decisions_write()` adds
#' decisions (a data frame with `scope`, `topic`, `decision` and `source`;
#' their ids are given) and returns the file's path.
#'
#' @param study_dir The study folder.
#' @param x The decisions to add: a data frame with `scope`, `topic`,
#'   `decision`, `source` (`source` may be left out: `"person"`).
#' @return `tfl_ai_decisions_read()`: a data frame, one row a decision:
#'   `id`, `scope`, `topic`, `decision`, `source`, `line` (the line's number
#'   in the file); no rows when the file has none.
#' @examples
#' d <- file.path(tempfile(), "study")
#' dir.create(file.path(d, "ai"), recursive = TRUE)
#' tfl_ai_decisions_write(d, data.frame(scope = "all", topic = "treatment_var.SAF",
#'                                      decision = "TRT01A", source = "person"))
#' tfl_ai_decisions_read(d)
#' @export
tfl_ai_decisions_read <- function(study_dir) {
  empty <- data.frame(id = character(), scope = character(), topic = character(),
                      decision = character(), source = character(), line = integer(),
                      stringsAsFactors = FALSE)
  f <- file.path(study_dir, "ai", "decisions.md")
  if (!file.exists(f)) return(empty)
  lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
  rows <- list(empty)
  in_code <- FALSE
  for (i in seq_along(lines)) {
    l <- lines[i]
    if (grepl("^```", l)) { in_code <- !in_code; next }
    if (in_code || !grepl("^- ", l)) next
    body <- trimws(sub("^- ", "", l))
    # - D3 [ADAE] teae.where: TRTEMFL == "Y" (chat, T-14-3-1)
    m <- regmatches(body, regexec("^(D[0-9]+)\\s+\\[([^]]*)\\]\\s+([^:]+):\\s*(.*)$", body))[[1L]]
    if (length(m)) {
      dec <- m[5L]
      src <- regmatches(dec, regexec("\\(([^()]*)\\)\\s*$", dec))[[1L]]
      if (length(src)) dec <- trimws(sub("\\([^()]*\\)\\s*$", "", dec))
      rows[[length(rows) + 1L]] <- data.frame(
        id = m[2L], scope = trimws(m[3L]), topic = trimws(m[4L]), decision = dec,
        source = if (length(src)) src[2L] else NA_character_, line = i, stringsAsFactors = FALSE)
      next
    }
    # - Q2 (T-14-3-1, T-14-3-2): the decision   (a question's line, before)
    m <- regmatches(body, regexec("^(Q[0-9]+)\\s+\\(([^)]*)\\):\\s*(.*)$", body))[[1L]]
    if (length(m)) {
      rows[[length(rows) + 1L]] <- data.frame(
        id = m[2L], scope = trimws(m[3L]), topic = NA_character_, decision = trimws(m[4L]),
        source = m[2L], line = i, stringsAsFactors = FALSE)
      next
    }
    # the person's own line: every report
    rows[[length(rows) + 1L]] <- data.frame(
      id = NA_character_, scope = "all", topic = NA_character_, decision = body,
      source = "person", line = i, stringsAsFactors = FALSE)
  }
  do.call(rbind, rows)
}

#' @rdname tfl_ai_decisions_read
#' @export
tfl_ai_decisions_write <- function(study_dir, x) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  for (k in c("scope", "topic", "decision")) {
    if (!k %in% names(x)) .ard_stop(sprintf("tfl_ai_decisions_write(): the decisions have `%s`.", k))
  }
  if (!"source" %in% names(x)) x$source <- "person"
  f <- file.path(study_dir, "ai", "decisions.md")
  if (!file.exists(f)) {
    dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
    .ai_write_text(paste(readLines(system.file("ai", "agent", "decisions.md", package = "tflspec"),
                                   warn = FALSE, encoding = "UTF-8"), collapse = "\n"), f)
  }
  have <- tfl_ai_decisions_read(study_dir)
  n <- suppressWarnings(as.integer(sub("^D", "", have$id[grepl("^D[0-9]+$", have$id %||% "")])))
  next_id <- if (length(n)) max(n) + 1L else 1L
  lines <- character()
  for (i in seq_len(nrow(x))) {
    sc <- trimws(as.character(x$scope[i]))
    if (!nzchar(sc) || is.na(sc)) sc <- "all"
    lines <- c(lines, sprintf("- D%d [%s] %s: %s (%s)", next_id, sc, trimws(x$topic[i]),
                              trimws(x$decision[i]), trimws(x$source[i])))
    next_id <- next_id + 1L
  }
  old <- readLines(f, warn = FALSE, encoding = "UTF-8")
  .ai_write_text(c(old, lines), f)
  invisible(f)
}

# The decisions that apply to a report (`id`), its dataset(s) and its
# section, under the settings' `carry`: the rows, with `how` ("apply":
# applied without asking; "ask": other reports' only, to ask about)
.ai_decisions_for <- function(study_dir, id, datasets = character(), section = NA_character_,
                              settings = .ai_spec_settings(study_dir)) {
  d <- tfl_ai_decisions_read(study_dir)
  if (!nrow(d)) { d$how <- character(); return(d) }
  key <- .ai_id_key(id)
  carry <- settings$carry
  how <- vapply(seq_len(nrow(d)), function(i) {
    parts <- trimws(strsplit(d$scope[i], ",", fixed = TRUE)[[1L]])
    if ("all" %in% tolower(parts)) return(if ("all" %in% carry) "apply" else "ask")
    ids <- parts[!is.na(.ai_id_key(parts))]
    if (any(vapply(ids, function(p) .ai_id_same(.ai_id_key(p), key), NA))) return("apply")
    if ("dataset" %in% carry && length(datasets) && any(toupper(parts) %in% toupper(datasets))) return("apply")
    if ("section" %in% carry && !is.na(section) && any(tolower(parts) == tolower(section))) return("apply")
    "ask"
  }, "")
  d$how <- how
  d
}

# The lines a request carries: the decisions that apply to its reports and
# those to ask about (a request's "Decisions that may apply")
.ai_decisions_block <- function(study_dir, ids, datasets = character(), section = NA_character_) {
  s <- .ai_spec_settings(study_dir)
  rows <- do.call(rbind, lapply(ids, function(id) {
    d <- .ai_decisions_for(study_dir, id, datasets, section, s)
    if (nrow(d)) d$report <- id
    d
  }))
  if (is.null(rows) || !nrow(rows)) return(character())
  line_of <- function(r) sprintf("- %s [%s] %s: %s", r$id %||% "", r$scope, r$topic %||% "", r$decision)
  apply_ <- rows[rows$how == "apply", , drop = FALSE]
  ask <- rows[rows$how == "ask", , drop = FALSE]
  apply_ <- apply_[!duplicated(apply_$line), , drop = FALSE]
  ask <- ask[!duplicated(ask$line), , drop = FALSE]
  c("## Decisions that may apply", "",
    if (nrow(apply_)) c(paste0("Judge each one for these reports: apply it (`from: D<n>`) only when it clearly ",
                               "holds for this report; else ask, as AGENTS.md *Decisions* says:"), "",
                        vapply(seq_len(nrow(apply_)), function(i) line_of(apply_[i, ]), "")),
    if (nrow(ask)) c("", "These were decided for other reports; ask whether they hold here too:", "",
                     vapply(seq_len(nrow(ask)), function(i) line_of(ask[i, ]), "")),
    "")
}

# ---- the drafts as answers: the person's check --------------------------------

# Where the YAML parser stopped, for the agent: the offending line of the
# file (trimmed, cut to 120 characters) and, for a `"` inside a
# double-quoted text, the one fix.  "" when the error names no line or the
# file cannot be read as text.
.ai_yaml_error_line <- function(msg, file) {
  at <- regmatches(msg, gregexpr("line ([0-9]+)", msg))[[1L]]
  if (!length(at) || !file.exists(file)) return("")
  txt <- tryCatch(suppressWarnings(readLines(file, warn = FALSE, encoding = "UTF-8")),
                  error = function(e) character())
  # the line where it stopped; the line where the construct began when that
  # is past the file's end or blank (a flow never closed)
  ns <- rev(as.integer(sub("line ", "", at, fixed = TRUE)))
  ns <- ns[!is.na(ns) & ns >= 1L & ns <= length(txt)]
  ns <- ns[nzchar(trimws(txt[ns]))]
  if (!length(ns)) return("")
  n <- ns[1L]
  ln <- trimws(txt[n])
  # the first `"` opens a value (after `: `, `[`, `, ` or `- `), and a `"` after
  # it, not escaped, is followed by more text
  ln_ <- gsub("\\\\\"", "", ln)
  quoted <- grepl("did not find expected|mapping values are not allowed", msg) &&
    grepl("^[^\"]*(: |\\[ ?|, |(^|[[:space:]])- )\"[^\"]*\"([[:alnum:]_(]|\")", ln_)
  show <- if (nchar(ln) > 120L) paste0(substr(ln, 1L, 117L), "...") else ln
  .ai_one_line(paste0(
    "Line ", n, " reads: ", show,
    if (quoted) paste(" -- a `\"` inside a double-quoted text ends it; write a condition in a text",
                      "with single quotes (`TRTEMFL == 'Y'`).")))
}

# A draft's `ai:` block (assumptions, confirm), by report, from
# ai/spec/outputs/*.yml; kept while the check writes the drafts anew.  A
# file YAML cannot read, with no `id`, or that is no report file (a call's
# `args` given as a text ...), is in `bad` (by the file's name) with why:
# the check refuses it alone, and the agent fixes its form; the other
# drafts are read and built.
.ai_draft_blocks <- function(study_dir) {
  area <- file.path(study_dir, "ai", "spec", "outputs")
  out <- list(blocks = list(), bad = character())
  if (!dir.exists(area)) return(out)
  for (f in list.files(area, "[.]ya?ml$", full.names = TRUE)) {
    id <- sub("[.]ya?ml$", "", basename(f))
    y <- tryCatch(yaml::read_yaml(f, handlers = .yaml_handlers()), error = function(e) e)
    if (inherits(y, "error")) {
      out$bad[id] <- trimws(paste("the YAML cannot be read:", .ai_one_line(conditionMessage(y)),
                                  .ai_yaml_error_line(conditionMessage(y), f)))
    } else if (!is.list(y) || is.null(y$id)) {
      out$bad[id] <- "not a report file: no `id`"
    } else if (!identical(.ai_file_id(y$id), id) &&
               !isTRUE(.ai_id_same(.ai_id_key(as.character(y$id)[1L]), .ai_id_key(id)))) {
      # (the same report number written another way -- "Table 14.1.1" in
      # T-14-1-1.yml -- is the same report)
      out$bad[id] <- sprintf("its `id` is %s, not the file's name: name the file after the id",
                             as.character(y$id)[1L])
    } else {
      # read as the file's id: the same report written another way is no error here
      y_id <- y
      y_id$id <- id
      rd <- tryCatch(.output_from_list(y_id, f), error = function(e) e)
      if (inherits(rd, "error")) {
        why <- .ai_one_line(conditionMessage(rd))
        if (grepl("`args` is a named list", why, fixed = TRUE)) {
          why <- paste0(why, " A call's `args` (plot.add, a `call` layer) is a mapping, one key an ",
                        "argument, `!r` for R: {fn: theme, args: {axis.text.x: !r element_text(angle = 45)}}.")
        }
        out$bad[id] <- paste("the report file cannot be read:", why)
      } else {
        out$blocks[[id]] <- list(file = f, ai = y$ai, y = y)
      }
    }
  }
  out
}

# The drafts the agent wrote, as the answers the check reads: for each
# request, its reports' draft files become one answer (`answers`/<request>.md,
# a folder of the check's own) in the answer's form -- a table's: the
# header, the outputs, the assumptions; a figure's: its header and its
# design (.fig_design_of_output()) -- so tfl_ai_collect_answers() checks,
# repairs, numbers the assumptions and writes the status as it does for an
# answer.  A list: `blocks` (the drafts' `ai:` blocks, to be put back
# after), `bad` (the files refused, with why) and `stray` (the drafts no
# request asks for).
.ai_drafts_to_answers <- function(study_dir, ctxs, answers) {
  .ai_drafts_footer_below(study_dir)
  d <- .ai_draft_blocks(study_dir)
  blocks <- d$blocks
  used <- character()
  keys <- .ai_id_key(names(blocks))
  for (nm in names(ctxs)) {
    ids <- .ai_file_id(ctxs[[nm]]$ids)
    task <- ctxs[[nm]]$context$task %||% "table"
    have <- names(blocks)[vapply(keys, function(k) any(vapply(.ai_id_key(ids), .ai_id_same, NA, b = k)), NA)]
    if (!length(have)) next
    used <- c(used, have)
    asm <- do.call(c, lapply(have, function(id) {
      a <- blocks[[id]]$ai$assumptions %||% list()
      lapply(a, function(e) { e$output_id <- id; e })
    }))
    if (identical(task, "figure")) {
      # (the figure's design back from its file; a template's {name, args}
      # as the answer gives it)
      y <- blocks[[have[1L]]]$y
      y$ai <- NULL
      o <- tfl_read_output(blocks[[have[1L]]]$file)
      fd <- .fig_design_of_output(o)
      if (is.list(y$template)) fd$template <- y$template
      body <- c(list(tflspec_ai = list(task = "figure", version = 1L, output_id = have[1L])),
                fd, list(assumptions = asm %||% list()))
    } else {
      outs <- lapply(have, function(id) {
        y <- blocks[[id]]$y
        y$ai <- NULL
        y
      })
      body <- list(tflspec_ai = list(task = "table", version = 1L, outputs = have),
                   outputs = outs, assumptions = asm %||% list())
    }
    txt <- c(sprintf("The report files of %s, from ai/spec/outputs/.", paste(have, collapse = ", ")),
             "", "```yaml",
             sub("\n$", "", yaml::as.yaml(body, indent.mapping.sequence = TRUE, handlers = .yaml_out_handlers())),
             "```", "")
    .ai_write_text(txt, file.path(answers, paste0(nm, ".md")))
  }
  list(blocks = blocks, bad = d$bad, stray = setdiff(names(blocks) %||% character(), used))
}

# A draft's footer lines (the shell's footnotes, written as `page: footer`)
# numbered below the study's own footer lines (spec/defaults.yml's: the
# program, the date), not over them: a draft whose lines collide with the
# study's is written again with its lines moved down.  Once moved, they
# collide no more, so a later check leaves them.
.ai_drafts_footer_below <- function(study_dir) {
  area <- file.path(study_dir, "ai", "spec", "outputs")
  df <- file.path(study_dir, "spec", "defaults.yml")
  if (!dir.exists(area) || !file.exists(df)) return(invisible(NULL))
  y <- tryCatch(yaml::read_yaml(df, handlers = .yaml_handlers()), error = function(e) NULL)
  own <- suppressWarnings(as.integer(names(y$page$footer %||% list())))
  n <- if (length(own) && !all(is.na(own))) max(own, na.rm = TRUE) else 0L
  if (n < 1L) return(invisible(NULL))
  for (f in list.files(area, "[.]ya?ml$", full.names = TRUE)) {
    o <- tryCatch(tfl_read_output(f), error = function(e) NULL)
    ft <- o$page$footer
    if (is.null(o) || !length(ft) || inherits(ft, "tfl_unset")) next
    k <- suppressWarnings(as.integer(names(ft)))
    if (anyNA(k) || !any(k <= n)) next
    names(ft) <- as.character(n + seq_along(ft))
    o$page$footer <- ft
    tfl_write_output(o, f)
  }
  invisible(NULL)
}

# The drafts' `ai:` blocks put back on the files the check wrote (the check
# writes a draft from the answer, without it); a draft the check did not
# write (an answer refused) keeps the agent's file as it was.  What the
# check assumed on its own -- the column widths it filled (item `widths`,
# spec/_assumptions.yml) -- joins the draft's assumptions, so that the next
# check, finding the widths there, still says they were the default ones.
.ai_draft_blocks_restore <- function(study_dir, blocks) {
  area <- file.path(study_dir, "ai", "spec", "outputs")
  af <- file.path(study_dir, "spec", "_assumptions.yml")
  asm <- if (file.exists(af)) tryCatch(yaml::read_yaml(af), error = function(e) NULL)
  if (!is.list(asm)) asm <- list()
  for (id in names(blocks)) {
    f <- file.path(area, paste0(id, ".yml"))
    if (!file.exists(f)) next
    o <- tryCatch(tfl_read_output(f), error = function(e) NULL)
    if (is.null(o)) next
    ai <- blocks[[id]]$ai
    own <- Filter(function(e) is.list(e) && identical(.ai_file_id(e$output_id %||% ""), id) &&
                    identical(e$item, "widths"), asm)
    had <- any(vapply(ai$assumptions %||% list(), function(e) identical(e$item, "widths"), NA))
    if (length(own) && !had) {
      ai$assumptions <- c(ai$assumptions %||% list(), lapply(own, function(e) {
        e[intersect(names(e), c("where", "item", "assumption", "value", "question"))]
      }))
    }
    if (is.null(ai)) next
    o$ai <- ai
    tfl_write_output(o, f)
  }
  invisible(NULL)
}

# ---- what is marked to confirm ----------------------------------------------------

# A report's `ai: confirm` marks (its file's), as rows: output_id, where,
# item, why
.ai_confirm_rows <- function(o) {
  cf <- o$ai$confirm
  empty <- data.frame(output_id = character(), where = character(), item = character(),
                      why = character(), stringsAsFactors = FALSE)
  if (is.null(cf) || !length(cf)) return(empty)
  cell <- function(e, k) { v <- e[[k]]; if (is.null(v) || !length(v)) NA_character_ else as.character(v[[1L]]) }
  do.call(rbind, c(list(empty), lapply(cf, function(e) {
    if (!is.list(e)) e <- list(why = as.character(e))
    data.frame(output_id = o$id %||% NA_character_, where = cell(e, "where"), item = cell(e, "item"),
               why = cell(e, "why"), stringsAsFactors = FALSE)
  })))
}

#' What the AI marked to confirm in a study's drafts
#'
#' The agent marks, in a report file's `ai: confirm`, what it
#' could not settle and the person decides in the builder: one row a mark,
#' from the drafts (`ai/spec/outputs/`) and the adopted reports
#' (`spec/outputs/`).  [tfl_check_spec()] lists them too (`level`
#' `"check"`), and the AI review as rule K19.
#'
#' @param study_dir The study folder.
#' @param where `"drafts"`, `"adopted"` or `"both"` (default).
#' @return A data frame: `output_id`, `where`, `item`, `why`, `file`.
#' @export
tfl_ai_confirm <- function(study_dir, where = c("both", "drafts", "adopted")) {
  where <- match.arg(where)
  dirs <- c(if (where != "adopted") file.path(study_dir, "ai", "spec", "outputs"),
            if (where != "drafts") file.path(study_dir, "spec", "outputs"))
  rows <- list(data.frame(output_id = character(), where = character(), item = character(),
                          why = character(), file = character(), stringsAsFactors = FALSE))
  for (d in dirs) {
    if (!dir.exists(d)) next
    for (f in list.files(d, "[.]yml$", full.names = TRUE)) {
      o <- tryCatch(tfl_read_output(f), error = function(e) NULL)
      if (is.null(o)) next
      r <- .ai_confirm_rows(o)
      if (nrow(r)) { r$file <- .ai_rel_path(study_dir, f); rows[[length(rows) + 1L]] <- r }
    }
  }
  do.call(rbind, rows)
}

.ai_rel_path <- function(study_dir, f) {
  root <- paste0(normalizePath(study_dir, winslash = "/", mustWork = FALSE), "/")
  q <- normalizePath(f, winslash = "/", mustWork = FALSE)
  if (startsWith(q, root)) substring(q, nchar(root) + 1L) else q
}

# The TOC section of a request's reports (the first's), from the shell as
# tfl_ai_read_shell() read it; NA when the TOC has no section
.ai_request_section <- function(shell, outputs) {
  t <- shell$toc
  if (is.null(t) || is.null(t$section)) return(NA_character_)
  ids <- strsplit(outputs, ", ", fixed = TRUE)[[1L]]
  s <- t$section[match(ids, t$output_id)]
  s <- s[!is.na(s) & nzchar(s)]
  if (length(s)) s[1L] else NA_character_
}

# The skeleton's section, under the shared instructions of each task
# (ai/instructions-table.md, -figure.md), from inst/ai/agent/instructions-spec.md
.ai_instructions_spec <- function(study_dir) {
  txt <- paste(readLines(system.file("ai", "agent", "instructions-spec.md", package = "tflspec"),
                         warn = FALSE, encoding = "UTF-8"), collapse = "
")
  txt <- .ai_fill(txt, list(footnotes_at = .ai_footnotes_text(.ai_spec_settings(study_dir)$footnotes_at)))
  for (f in list.files(file.path(study_dir, "ai"), "^instructions-.*[.]md$", full.names = TRUE)) {
    old <- readLines(f, warn = FALSE, encoding = "UTF-8")
    if (any(grepl("^## The skeleton (what a draft holds)", old, fixed = TRUE))) next
    # (the figure's file ends with what a template or a step cannot do)
    lim <- if (basename(f) == "instructions-figure.md") {
      readLines(system.file("ai", "agent", "instructions-figure-limits.md", package = "tflspec"),
                warn = FALSE, encoding = "UTF-8")
    }
    # (the chat-paste form below is for a chat without files; here the answer is the files)
    at <- which(old == "## Your answer")
    if (length(at)) {
      old <- append(old, c("", paste0(
        "**In the study kit your answer is the files** `ai/spec/outputs/<id>.yml`, one a report ",
        "(see *The skeleton*, below); the form that follows, one fenced block, is for a chat ",
        "without files.")), after = at[1L])
    }
    .ai_write_text(c(old, "", txt, if (length(lim)) c("", lim)), f)
  }
  invisible(NULL)
}

# One of study.yml's ai: keys set (the others kept): tfl_ai_prepare(language =)
# writes the chat's language there
.ai_study_yml_set <- function(study_dir, key, value) {
  f <- file.path(study_dir, "study.yml")
  y <- if (file.exists(f)) yaml::read_yaml(f) else list()
  if (!is.list(y)) y <- list()
  if (!is.list(y$ai)) y$ai <- list()
  if (identical(y$ai[[key]], value)) return(invisible(f))
  y$ai[[key]] <- value
  .ai_write_text(sub("
$", "", yaml::as.yaml(y, indent.mapping.sequence = TRUE, handlers = .yaml_out_handlers())), f)
  invisible(f)
}
