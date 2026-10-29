# ---------------------------------------------------------------------------
#  The agent's "spec" route (2026-10-29, the AI flow made again)
# ---------------------------------------------------------------------------
#  tfl_ai_prepare(mode = "agent", route = "spec"): the agent writes each
#  report's skeleton as its draft file, ai/spec/outputs/<id>.yml, asks the
#  person in the chat what defines a report and is not settled, records the
#  answers in ai/decisions.md (one structured line a decision) and marks
#  what it could not settle as `ai: confirm` in the file.  The person's
#  check (tfl_ai_collect_answers() / run.R) reads the drafts as the answers
#  route reads the answers -- the same checks, repairs, assumptions, status
#  -- and keeps the drafts' `ai:` block.
#
#  What the user's seven decisions may change is in one place,
#  .ai_spec_settings(): when the agent asks, how far a decision carries,
#  the questions' limit, the language.

# ---- the settings ------------------------------------------------------------

# The spec route's settings: the package's defaults, under study.yml's
# `ai:` keys (ask_when, carry, max_questions, language)
.ai_spec_defaults <- list(
  ask_when = "request",                 # "request": once per request, before writing; "report": per report
  carry = c("all", "dataset", "section"),  # a decision applies without asking when its scope is one of these (and the agent is sure)
  max_rounds = 3L,                      # asking again about one item: this many times, then "to confirm"
  footnotes_at = "footer",              # the shell's footnotes: the page's "footer" band, or the "table"'s footnotes
  language = "en"
)

# The settings, under the study standard's `ai:` keys (spec/defaults.yml:
# the footnotes' place) and then study.yml's `ai:` keys
.ai_spec_settings <- function(study_dir) {
  s <- .ai_spec_defaults
  for (f in file.path(study_dir, c(file.path("spec", "defaults.yml"), "study.yml"))) {
    y <- if (file.exists(f)) tryCatch(yaml::read_yaml(f), error = function(e) NULL)
    ai <- if (is.list(y)) y$ai else NULL
    if (!is.list(ai)) next
    for (k in intersect(names(s), names(ai))) if (!is.null(ai[[k]])) s[[k]] <- ai[[k]]
  }
  s$max_rounds <- as.integer(s$max_rounds)
  s$carry <- as.character(unlist(s$carry))
  s$footnotes_at <- match.arg(as.character(s$footnotes_at)[1L], c("footer", "table"))
  s
}

# The route a study was prepared for: "spec" when its AGENTS.md says so,
# else "answers"
.ai_route <- function(study_dir) {
  f <- file.path(study_dir, "AGENTS.md")
  if (!file.exists(f)) return("answers")
  if (any(grepl("<!-- tflspec route: spec -->", readLines(f, warn = FALSE, encoding = "UTF-8"),
                fixed = TRUE))) "spec" else "answers"
}

# AGENTS-spec.md filled from the settings
.ai_spec_agents <- function(text, s) {
  lang <- .ai_language_name(s$language)
.ai_fill(text, list(
    language = lang,
    ask_when = if (identical(s$ask_when, "report")) {
      "Report by report, before you write it:"
    } else "For the request's reports together, before you write any of them:",
    carry_section = if ("section" %in% s$carry) " or its TOC section" else "",
    footnotes_at = .ai_footnotes_text(s$footnotes_at)))
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
#' there (the spec route, [tfl_ai_prepare()]`(route = "spec")`) and applies
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
    # - Q2 (T-14-3-1, T-14-3-2): the decision   (the answers route's)
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
    .ai_write_text(paste(readLines(system.file("ai", "agent", "decisions-spec.md", package = "tflspec"),
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
# those to ask about (the spec route's "Decisions that apply")
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
  c("## Decisions that apply", "",
    if (nrow(apply_)) c("Apply these without asking (write an assumption with `from: D<n>`):", "",
                        vapply(seq_len(nrow(apply_)), function(i) line_of(apply_[i, ]), "")),
    if (nrow(ask)) c("", "These were decided for other reports; ask whether they hold here too:", "",
                     vapply(seq_len(nrow(ask)), function(i) line_of(ask[i, ]), "")),
    "")
}

# ---- the drafts as answers: the person's check --------------------------------

# A draft's `ai:` block (assumptions, confirm), by report, from
# ai/spec/outputs/*.yml; kept while the check writes the drafts anew
.ai_draft_blocks <- function(study_dir) {
  area <- file.path(study_dir, "ai", "spec", "outputs")
  if (!dir.exists(area)) return(list())
  fs <- list.files(area, "[.]yml$", full.names = TRUE)
  out <- list()
  for (f in fs) {
    y <- tryCatch(yaml::read_yaml(f, handlers = .yaml_handlers()), error = function(e) NULL)
    if (!is.list(y) || is.null(y$id)) next
    out[[as.character(y$id)]] <- list(file = f, ai = y$ai, y = y)
  }
  out
}

# The drafts the agent wrote, as the answers the check reads: for each
# request, its reports' draft files become one answer
# (ai/answers/<request>.md) in the table answer's form -- the header, the
# outputs, the assumptions -- so tfl_ai_collect_answers() checks, repairs,
# numbers the assumptions and writes the status as it does for an answer.
# The drafts' `ai:` blocks are returned, to be put back after.
.ai_drafts_to_answers <- function(study_dir, ctxs, answers = file.path("ai", "answers")) {
  blocks <- .ai_draft_blocks(study_dir)
  if (!length(blocks)) return(blocks)
  dir.create(file.path(study_dir, answers), recursive = TRUE, showWarnings = FALSE)
  keys <- .ai_id_key(names(blocks))
  for (nm in names(ctxs)) {
    ids <- .ai_file_id(ctxs[[nm]]$ids)
    task <- ctxs[[nm]]$context$task %||% "table"
    have <- names(blocks)[vapply(keys, function(k) any(vapply(.ai_id_key(ids), .ai_id_same, NA, b = k)), NA)]
    if (!length(have)) next
    outs <- lapply(have, function(id) {
      y <- blocks[[id]]$y
      y$ai <- NULL
      y
    })
    asm <- do.call(c, lapply(have, function(id) {
      a <- blocks[[id]]$ai$assumptions %||% list()
      lapply(a, function(e) { e$output_id <- id; e })
    }))
    body <- list(tflspec_ai = list(task = "table", version = 1L, outputs = have),
                 outputs = outs, assumptions = asm %||% list())
    if (identical(task, "figure")) body$tflspec_ai$task <- "table"
    txt <- c(sprintf("The report files of %s, from ai/spec/outputs/ (the spec route).", paste(have, collapse = ", ")),
             "", "```yaml",
             sub("\n$", "", yaml::as.yaml(body, indent.mapping.sequence = TRUE, handlers = .yaml_out_handlers())),
             "```", "")
    .ai_write_text(txt, file.path(study_dir, answers, paste0(nm, ".md")))
  }
  blocks
}

# The drafts' `ai:` blocks put back on the files the check wrote (the check
# writes a draft from the answer, without it); a draft the check did not
# write (an answer refused) keeps the agent's file as it was
.ai_draft_blocks_restore <- function(study_dir, blocks) {
  area <- file.path(study_dir, "ai", "spec", "outputs")
  for (id in names(blocks)) {
    f <- file.path(area, paste0(id, ".yml"))
    if (!file.exists(f) || is.null(blocks[[id]]$ai)) next
    o <- tryCatch(tfl_read_output(f), error = function(e) NULL)
    if (is.null(o)) next
    o$ai <- blocks[[id]]$ai
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
#' The spec route's agent marks, in a report file's `ai: confirm`, what it
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
    .ai_write_text(c(old, "", txt), f)
  }
  invisible(NULL)
}
