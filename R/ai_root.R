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
  tok <- .page_output_tokens_used(st$defaults$page %||% list())
  cls <- names(st$defaults$codelists %||% list())
  c("# This study's facts",
    "",
    "Read-only: the person's study as it is now (spec/data.yml, spec/defaults.yml);",
    "tflspec writes this file again when they change.",
    "",
    "## The analysis sets (a report's `population:` is the id)",
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
  .ai_write_text(paste(c("{", "  \"folders\": [{\"path\": \".\"}]", "}"), collapse = "\n"), f)
  invisible(f)
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
  x <- gsub("`spec/data[.]yml`, `spec/defaults[.]yml`", "`study_facts.md`", x)
  gsub("(?<![A-Za-z0-9_./-])spec/(data|defaults)[.]yml", "study_facts.md", x, perl = TRUE)
}

# A file's path from the study folder (NULL for none)
.ai_check_rel <- function(f, dir) {
  if (is.null(f)) return(NULL)
  .check_rel_path(f, dir)
}
