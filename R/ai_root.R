# ============================================================================
#  The agent's folder is ai/ alone (#528, the user 10/31)
# ----------------------------------------------------------------------------
#  The agent works in a workspace opened on the study's ai/ folder: it
#  cannot read or change spec/, the programs, the data or another study,
#  and the files it reads stay as prepare wrote them.  So what it used to
#  read from spec/ -- the analysis sets, the datasets, whether the study's
#  header prints the titles, the study's code lists -- is a short file of
#  facts in ai/ (study_facts.md), written by prepare and again by the
#  check when spec/ is newer; and a VS Code workspace file opens the folder
#  with one double click.
# ============================================================================

# The study's facts an agent needs, as markdown: from spec/data.yml and
# spec/defaults.yml as they are now
.ai_study_facts <- function(dir) {
  st <- tryCatch(tfl_read_study_spec(dir), error = function(e) NULL)
  if (is.null(st)) return(NULL)
  pops <- st$data$populations %||% list()
  pop_lines <- vapply(pops, function(p) {
    sprintf("| %s | %s | %s |", p$id %||% "", p$label %||% "",
            if (is.null(p$where)) "every subject" else paste0("`", as.character(p$where), "`"))
  }, "")
  ds <- vapply(st$data$datasets %||% list(), function(d) d$dataset %||% "", "")
  tok <- .page_output_tokens_used(.page_config_flat(list(page = .study_default_page(st))))
  cls <- names(st$defaults$codelists %||% list())
  c("# This study's facts",
    "",
    "Read-only: the person's study as it is now (its analysis sets, datasets and",
    "defaults, outside this folder: do not open them); tflspec writes this file",
    "again when they change.",
    "",
    "## The analysis sets (a report's `common: population` is the id)",
    "",
    "| id | label | the subjects |",
    "|---|---|---|",
    pop_lines,
    "",
    "## The datasets",
    "",
    if (length(ds)) paste0("- ", ds) else "- (none)",
    "",
    "## The study's header",
    "",
    if (length(tok)) {
      sprintf(paste("It prints the report's label, title and population (%s):",
                    "a draft writes no `page: titles`."), paste0("{", tok, "}", collapse = ", "))
    } else {
      "It prints no title of the report's: a draft writes its titles under `page: titles`."
    },
    "",
    "## The study's cells and column header",
    "",
    .ai_facts_cells(st$defaults$content),
    "",
    "## The study's code lists",
    "",
    if (length(cls)) {
      c("These variables have one already; a draft writes its own only when the shell differs:",
        "", paste0("- ", cls))
    } else "None.")
}

# The study's count cell and column header (its defaults' plan_cells and
# plan_col_header), as the lines of study_facts.md: what a draft compares
# the shell's with
.ai_facts_cells <- function(content) {
  items <- content %||% list()
  if (inherits(items, "tfl_r")) items <- list()
  fn <- vapply(items, function(it) it$fn %||% "", "")
  cell <- unlist(lapply(items[fn == "plan_cells"], function(it) it$args$categorical))
  cell <- if (length(cell)) as.character(cell[[length(cell)]])
  hdr <- unlist(lapply(items[fn == "plan_col_header"], function(it) {
    vapply(it$args$header %||% list(), function(h) as.character(h$text %||% ""), "")
  }))
  n_in <- any(grepl("{n}", hdr, fixed = TRUE) | grepl("{N}", hdr, fixed = TRUE))
  c(if (length(cell) && nzchar(cell)) {
      sprintf("- A count's cell: `%s`, printed `%s`.", cell, .ai_cell_shape(cell))
    } else {
      "- A count's cell: none set (the template's, else rtfreporter's)."
    },
    if (n_in) {
      "- The column header: the arm's name with its N."
    } else {
      "- The column header: the arm's name, no N (a template's own header may add it: templates.md)."
    },
    "",
    paste("A shell that shows another cell form (`xx (xx.x%)`) or an N under the arm",
          "(`(N=xx)`): the report writes its own (the instructions say how)."))
}

# A cell template as the shell writes it: "{n} ({p:.1f%})" -> "xx (xx.x)"
.ai_cell_shape <- function(tpl) {
  x <- tpl
  for (d in 1:4) {
    x <- gsub(sprintf("[{][^{}:]+:[.]%df%%?[}]", d), paste0("xx.", strrep("x", d)), x)
  }
  gsub("[{][^{}]*[}]", "xx", x)
}

# Write ai/study_facts.md; with `if_older`, only when spec/ is newer than it
.ai_study_facts_write <- function(dir, if_older = FALSE) {
  f <- file.path(dir, "ai", "study_facts.md")
  if (isTRUE(if_older) && file.exists(f)) {
    src <- file.path(dir, "spec", c("data.yml", "defaults.yml"))
    src <- src[file.exists(src)]
    if (!length(src) || all(file.mtime(src) <= file.mtime(f))) return(invisible(NULL))
  }
  x <- .ai_study_facts(dir)
  if (is.null(x)) return(invisible(NULL))
  .ai_write_text(paste(x, collapse = "\n"), f)
  invisible(f)
}

# ai/<study>.code-workspace: VS Code opens the ai folder alone
.ai_workspace_write <- function(dir) {
  name <- .ai_file_id(basename(normalizePath(dir, winslash = "/", mustWork = FALSE)))
  f <- file.path(dir, "ai", paste0(name, ".code-workspace"))
  .ai_write_text(.ai_json(list(
    folders = list(list(path = ".")),
    settings = list(
      # VS Code reads AGENTS.md and the Copilot instruction files, and
      # asks about every terminal command but `Rscript ../run.R <request>`
      chat.useAgentsMdFile = TRUE,
      github.copilot.chat.codeGeneration.useInstructionFiles = TRUE,
      chat.tools.terminal.enableAutoApprove = TRUE,
      chat.tools.terminal.autoApprove = stats::setNames(
        list(TRUE), "/^Rscript \\.\\./run\\.R [A-Za-z0-9._-]+$/")))), f)
  invisible(f)
}

# A list as pretty JSON text
.ai_json <- function(x) {
  paste(as.character(jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE)), collapse = "\n")
}

# ai/CLAUDE.md: Claude Code reads AGENTS.md only when no CLAUDE.md is in the
# folder or above it; this one makes it read ai/AGENTS.md anyway
.ai_claude_md_marker <- "<!-- tflspec: this file makes Claude Code read AGENTS.md -->"
.ai_claude_md_text <- function() c(.ai_claude_md_marker, "@AGENTS.md")

# ai/.claude/settings.json (Claude Code's project settings): the one command
# the agent may run, the files it may write, no installs, no network, and no
# file outside ai/
.ai_claude_settings <- function() {
  list(permissions = list(
    allow = c("Bash(Rscript ../run.R *)",
              "Edit(spec/outputs/**)", "Write(spec/outputs/**)",
              "Edit(decisions.md)", "Write(decisions.md)",
              "Edit(progress.md)", "Write(progress.md)",
              "Edit(questions-end.md)", "Write(questions-end.md)"),
    deny = c("Bash(pip *)", "Bash(curl *)", "Bash(wget *)", "Bash(Rscript -e *)",
             "Bash(R *)", "WebFetch", "WebSearch", "Read(../**)", "Edit(../**)"),
    blockReadsOutsideWorkingDirectories = TRUE,
    blockWritesOutsideWorkingDirectories = TRUE))
}

# Whether a settings.json is one the kit wrote (now or in an older version):
# the same text, or its two permissions the kit always sets
.ai_claude_settings_is_kit <- function(text) {
  if (identical(text, .ai_json(.ai_claude_settings()))) return(TRUE)
  x <- tryCatch(jsonlite::fromJSON(text), error = function(e) NULL)
  is.list(x) && all(c("Bash(Rscript ../run.R *)") %in% x$permissions$allow) &&
    "Bash(pip *)" %in% x$permissions$deny
}

# Write the two Claude Code files into ai/.  A file there that the kit did not
# write (the person's own CLAUDE.md or settings.json) is left alone, with a
# message; one the kit wrote is written again only with `overwrite`.  The
# paths written.
.ai_claude_files <- function(dir, overwrite = FALSE) {
  one <- function(rel, text, is_kit) {
    f <- file.path(dir, "ai", rel)
    if (file.exists(f)) {
      cur <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
      if (!is_kit(cur)) {
        message(sprintf("tfl_ai_prepare(): ai/%s is not the kit's own, so it is left as it is.", rel))
        return(NULL)
      }
      if (!isTRUE(overwrite)) return(NULL)
    }
    .ai_write_text(text, f)
  }
  c(one("CLAUDE.md", .ai_claude_md_text(),
        function(cur) startsWith(cur, "<!-- tflspec:")),
    one(file.path(".claude", "settings.json"), .ai_json(.ai_claude_settings()),
        .ai_claude_settings_is_kit))
}

# Whether a file in ai/ is one the agent reads (its instructions, requests,
# first message, repairs): their paths are from ai/.  The person's files
# (status.md, questions.md) keep theirs from the study folder.
.ai_for_agent <- function(file) {
  f <- gsub("[\\]", "/", file)
  b <- basename(f)
  in_ai <- grepl("(^|/)ai/", f)
  in_ai && (b %in% c("AGENTS.md", "agent_prompt.md", "templates.md") ||
              grepl("^instructions-.*[.]md$", b) ||
              grepl("(^|/)ai/(requests|repairs)/[^/]+[.]md$", f) ||
              grepl("(^|/)ai/[.]github/", f))
}

# A text's paths from ai/: `ai/requests/x.md` is `requests/x.md`, and what
# the agent read from spec/ is ai/study_facts.md
.ai_paths_from_ai <- function(x) {
  x <- gsub("(?<![A-Za-z0-9_./-])ai/(?=[A-Za-z_.{<*])", "", x, perl = TRUE)
  x <- gsub("`spec/data[.]yml`,\\s+`spec/defaults[.]yml`", "`study_facts.md`", x, perl = TRUE)
  gsub("(?<![A-Za-z0-9_./-])spec/(data|defaults)[.]yml", "study_facts.md", x, perl = TRUE)
}

# A file's path from the study folder (NULL for none)
.ai_check_rel <- function(f, dir) {
  if (is.null(f)) return(NULL)
  .check_rel_path(f, dir)
}
