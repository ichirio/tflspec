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
#    .ai_decisions_block()    a request's "Decisions that apply"
#    .ai_drafts_to_answers()  the drafts as the check's answers
#    tfl_ai_confirm()         what is marked to confirm
# ============================================================================

# ---- the settings ------------------------------------------------------------

# The agent's settings: the package's defaults, under study.yml's
# `ai:` keys (ask_when, carry, max_questions, language)
.ai_spec_defaults <- list(
  ask_when = "request",                 # "request": once per request, before writing; "report": per report
  carry = c("all", "dataset", "section"),  # a decision applies without asking when its scope is one of these (and the agent is sure)
  max_rounds = 3L,                      # asking again about one item: this many times, then "to confirm"
  footnotes_at = "footer",              # the shell's footnotes: the page's "footer" band, or the "table"'s footnotes
  language = "en",
  titles_in_header = character()        # the output tokens the study standard's header prints (none: the report writes its titles)
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
    for (k in intersect(names(s), names(ai))) if (!is.null(ai[[k]])) s[[k]] <- ai[[k]]
  }
  s$max_rounds <- as.integer(s$max_rounds)
  s$carry <- as.character(unlist(s$carry))
  s$footnotes_at <- match.arg(as.character(s$footnotes_at)[1L], c("footer", "table"))
  s
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
    footnotes_at = .ai_footnotes_text(s$footnotes_at),
    titles_rule = .ai_titles_text(s$titles_in_header)))
}

# The titles: the shell's, as `page: titles` -- unless the study standard's
# header prints the label, the title and the population from the report's
# `label:` and `population:` (its tokens): then none, or they print twice
.ai_titles_text <- function(tokens) {
  if (!length(tokens)) return("`page: titles`, from the shell;")
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
# those to ask about (a request's "Decisions that apply")
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
# ai/spec/outputs/*.yml; kept while the check writes the drafts anew.  A
# file YAML cannot read, or with no `id`, is in `bad` (by the file's name)
# with why: the check refuses it, and the agent fixes its form.
.ai_draft_blocks <- function(study_dir) {
  area <- file.path(study_dir, "ai", "spec", "outputs")
  out <- list(blocks = list(), bad = character())
  if (!dir.exists(area)) return(out)
  for (f in list.files(area, "[.]ya?ml$", full.names = TRUE)) {
    id <- sub("[.]ya?ml$", "", basename(f))
    y <- tryCatch(yaml::read_yaml(f, handlers = .yaml_handlers()), error = function(e) e)
    if (inherits(y, "error")) {
      out$bad[id] <- paste("the YAML cannot be read:", .ai_one_line(conditionMessage(y)))
    } else if (!is.list(y) || is.null(y$id)) {
      out$bad[id] <- "not a report file: no `id`"
    } else if (!identical(.ai_file_id(y$id), id) &&
               !isTRUE(.ai_id_same(.ai_id_key(as.character(y$id)[1L]), .ai_id_key(id)))) {
      # (the same report number written another way -- "Table 14.1.1" in
      # T-14-1-1.yml -- is the same report)
      out$bad[id] <- sprintf("its `id` is %s, not the file's name: name the file after the id",
                             as.character(y$id)[1L])
    } else {
      out$blocks[[id]] <- list(file = f, ai = y$ai, y = y)
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
    .ai_write_text(c(old, "", txt), f)
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
