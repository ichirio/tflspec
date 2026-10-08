# ============================================================================
#  AI-assisted drafting: the schema and the prompt
# ----------------------------------------------------------------------------
#  The prompt's fixed English parts are in inst/ai/prompts/*.md, read at
#  call time; the schema is generated from the definitions the checks use
#  (tfl_read_toc()'s fields, tfl_fig_parts(), tfl_fig_templates()), so the
#  prompt cannot describe a column or a piece the package does not read.
# ============================================================================

# Variables listed a dataset at most; the rest are counted.
.ai_max_variables <- 200L

# The TOC's columns as a toc answer fills them: one row a field of
# tfl_read_toc() (.toc_fields; test-ai-prompt.R keeps the two in step).
.ai_toc_columns <- function() {
  data.frame(
    column = c(
      "output_id", "type", "title", "population", "footnote", "program",
      "file", "note", "section", "datasets", "label"
    ),
    description = c(
      paste("The report's number as the documents give it, unique in the TOC",
            "(required)."),
      "The kind of report: table, figure or listing.",
      paste("The title lines in order, without the report number and the",
            "analysis set; ` | ` between lines."),
      paste("The analysis set the report is on, as the documents name it; it",
            "becomes the last title line."),
      "The footnote lines in order; ` | ` between lines.",
      "The name of the program that makes the report.",
      "The name of the report's output file.",
      "A remark for people; never read by tflspec.",
      "The heading the report is listed under in the documents.",
      "The ADaM datasets the report reads; ` | ` between them.",
      "The report's number as printed in its title."
    ),
    example = c(
      "T-14-1-1", "table",
      "Summary of Demographic and Baseline Characteristics",
      "Safety Population",
      "Percentages are based on the number of subjects in the analysis set.",
      "t_14_1_1.R", "t_14_1_1.rtf", NA, "14.1 Demographic and Baseline Data",
      "ADSL", "Table 14.1.1"
    ),
    stringsAsFactors = FALSE
  )
}

# The sections of a figure design, in order, with what each holds.
.ai_fig_sections <- c(
  data = paste("the steps from the ADaM datasets to the figure's data `df`,",
               "in order; each piece starts with `step`"),
  stats = paste("what is computed from `df`, each with the `name` the layers",
                "refer to; each piece starts with `step`"),
  plot = paste("one mapping of the figure-wide settings; `add` is a list of",
               "calls written after them"),
  layers = "what is drawn, in order; each piece starts with `layer`"
)

# What a figure field's kind means.
.ai_fig_kinds <- function() {
  c(
    dataset = "a dataset's name",
    variable = "a variable's name",
    variables = "variable names, ` | ` between them",
    flag = "a flag variable's name (FASFL, SAFFL ...)",
    param = "a PARAMCD value",
    object = "`df` or the `name` of a statistics piece",
    choice = "one of the values listed",
    number = "a number",
    logical = "true or false",
    values = "numbers, ` | ` between them",
    text = "text",
    expr = "an R expression, as text",
    code = "R code, as text",
    named = "`name = value` pairs, ` | ` between them",
    raw = "a YAML mapping or list, as the help says",
    pieces = "a list of calls (`fn`, `args` ...)",
    shape = paste("a point shape, one of",
                  paste(names(pp_shape_names), collapse = " | "))
  )
}

#' The schema text of a drafting task
#'
#' What a drafting prompt ([tfl_ai_prompt()]) tells the model it may write:
#' every column or field, what it means and an example, generated from the
#' definitions tflspec's own readers and checks use -- [tfl_read_toc()]'s
#' fields for `toc`; [tfl_fig_parts()] and [tfl_fig_templates()] for
#' `figure` -- so the prompt cannot name a column or a piece that does not
#' exist.
#'
#' @param task One of [tfl_ai_tasks()].
#' @param sheets The parts to describe: for `toc`, `"toc"`; for `figure`,
#'   sections among `"template"`, `"data"`, `"stats"`, `"plot"`,
#'   `"layers"`, or single pieces by name (`"censor_mark"`, `"join"`; see
#'   [tfl_fig_parts()]).  `NULL` for all.  The repair prompt describes only
#'   the parts with problems.
#' @return The schema as one string (markdown), of class `tfl_ai_text`.
#' @examples
#' cat(tfl_ai_schema("toc"))
#' cat(substr(tfl_ai_schema("figure", sheets = "stats"), 1, 600))
#' @export
tfl_ai_schema <- function(task, sheets = NULL) {
  task <- .ai_task(task, "tfl_ai_schema()")
  .ai_text(.ai_schema(task, sheets))
}

# The schema; `templates` restricts a figure's template list.
.ai_schema <- function(task, sheets = NULL, templates = NULL) {
  known <- .ai_schema_parts(task)
  sheets <- sheets %||% known
  pieces <- if (task == "figure") {
    p <- tfl_fig_parts()
    p <- unique(p[!p$piece %in% c("plot", "plot_add"), c("section", "piece")])
    p[p$piece %in% sheets, , drop = FALSE]
  }
  bad <- setdiff(sheets, c(known, pieces$piece))
  if (length(bad)) {
    .ard_stop(sprintf(
      "tfl_ai_schema(): a %s answer has no part %s; its parts are %s.",
      task, paste(sQuote(bad), collapse = ", "), paste(known, collapse = ", ")
    ))
  }
  switch(task,
    toc = .ai_schema_toc(),
    figure = .ai_schema_figure(
      known[known %in% c(sheets, pieces$section)], templates,
      pieces[!pieces$section %in% sheets, , drop = FALSE]
    )
  )
}

.ai_schema_parts <- function(task) {
  switch(task,
    toc = "toc",
    figure = c("template", names(.ai_fig_sections))
  )
}

.ai_schema_toc <- function() {
  d <- .ai_toc_columns()
  lines <- vapply(seq_len(nrow(d)), function(i) {
    ex <- if (is.na(d$example[i])) "" else
      sprintf(" Example: `%s`", d$example[i])
    sprintf("- `%s`: %s%s", d$column[i], d$description[i], ex)
  }, "")
  paste(c(
    "`toc` is a list of rows, one a report. Its columns:", "", lines
  ), collapse = "\n")
}

# `pieces` (section, piece): of those sections, only these pieces.
.ai_schema_figure <- function(sections, templates = NULL, pieces = NULL) {
  parts <- tfl_fig_parts()
  out <- character()
  if ("template" %in% sections) {
    tp <- tfl_fig_templates()
    tp <- tp[tp$parts, , drop = FALSE]
    if (!is.null(templates)) {
      tp <- tp[tp$template %in% templates, , drop = FALSE]
    }
    out <- c(
      out, "### template", "",
      paste("The template the design starts from (a note: the pieces are",
            "what count). One of:"),
      "",
      sprintf("- `%s`: %s (reads %s)", tp$template, tp$label, tp$data),
      ""
    )
  }
  used <- character()
  for (s in setdiff(sections, "template")) {
    p <- parts[parts$section == s, , drop = FALSE]
    if (s == "plot") p <- p[p$piece == "plot", , drop = FALSE]
    if (s %in% pieces$section) p <- p[p$piece %in% pieces$piece, , drop = FALSE]
    out <- c(out, sprintf("### %s", s), "", .ai_cap(.ai_fig_sections[[s]]), "")
    for (pc in unique(p$piece)) {
      f <- p[p$piece == pc, , drop = FALSE]
      used <- c(used, f$kind)
      head <- if (s == "plot") "- Fields:" else
        sprintf("- `%s`: %s.%s Fields:", pc, f$piece_label[1L],
                .ai_help(f$piece_help[1L]))
      out <- c(out, head, sprintf("  - %s", .ai_fig_field_text(f)))
    }
    if (s == "plot") {
      add <- parts[parts$piece == "plot_add", , drop = FALSE]
      used <- c(used, add$kind)
      out <- c(
        out, "- Each call of `add`: Fields:",
        sprintf("  - %s", .ai_fig_field_text(add))
      )
    }
    out <- c(out, "")
  }
  kinds <- .ai_fig_kinds()
  used <- intersect(names(kinds), used)
  if (length(used)) {
    out <- c(
      out, "### The kinds of value", "",
      sprintf("- %s: %s", used, kinds[used]), ""
    )
  }
  paste(c(
    "A field marked * is required. Leave out a field to take its default.",
    "", out
  ), collapse = "\n")
}

# A piece's fields, one line each: `name`* (kind; one of ...; default x): help
.ai_fig_field_text <- function(f) {
  vapply(seq_len(nrow(f)), function(i) {
    bits <- f$kind[i]
    if (!is.na(f$choices[i]) && nzchar(f$choices[i])) {
      bits <- c(bits, paste("one of", f$choices[i]))
    }
    if (!is.na(f$default[i]) && nzchar(f$default[i])) {
      bits <- c(bits, sprintf("default `%s`", f$default[i]))
    }
    sprintf("`%s`%s (%s)%s", f$field[i], if (isTRUE(f$required[i])) "*" else "",
            paste(bits, collapse = "; "), .ai_help(f$help[i], ": "))
  }, "")
}

.ai_help <- function(x, lead = " ") {
  if (is.na(x) || !nzchar(trimws(x))) return("")
  x <- trimws(x)
  paste0(lead, x, if (!grepl("[.?!)]$", x)) ".")
}

.ai_cap <- function(x) paste0(toupper(substr(x, 1L, 1L)), substring(x, 2L), ".")

# ---- the prompt -------------------------------------------------------------

#' A drafting prompt for an AI chat
#'
#' The prompt that asks a chat model to draft part of a specification from
#' the study's documents: the study's table of contents (`toc`) or one
#' figure's design (`figure`).  tflspec sends nothing: the prompt is text,
#' and so is the answer, which [tfl_ai_parse()] reads.
#'
#' The `system` part holds the ground rules (only the names given, nothing
#' invented, a draft that a person reviews), the task's instructions, the
#' answer's shape -- one fenced block that starts with the header
#' `tflspec_ai: {task: ..., version: 1, ...}` and ends with the
#' `assumptions` -- the schema ([tfl_ai_schema()]) and an example answer
#' from the sample study.  The `user` part holds the context
#' ([tfl_ai_context()]) and what to look for in the documents.  The prompt
#' is English; the model is asked to write its assumptions in the user's
#' language (`context$language`, or the language of the user's message).
#'
#' * Mode `"chat"`: the user pastes `format(prompt)` into a chat and
#'   attaches the documents themselves; `attach` lists them.
#' * Mode `"api"`: the documents' text, extracted by the caller, is in the
#'   `user` part between `<document>` fences; `messages` is what a client
#'   sends.  tflspec has no HTTP client.
#'
#' The fixed wording is in the package's `ai/prompts` folder
#' (`system.file("ai", "prompts", package = "tflspec")`).
#'
#' @param task One of [tfl_ai_tasks()].
#' @param context A [tfl_ai_context()] of the same task; `NULL` for an empty
#'   one (a figure needs one: its `output_id`).
#' @param mode `"chat"` (the user attaches the documents) or `"api"` (their
#'   text is in the prompt).
#' @param format The answer's format: `"yaml"` (the default) or `"json"`.
#'   [tfl_ai_parse()] reads both.
#' @param example Include an example answer.
#' @param documents Mode `"api"`: the documents' text, a list of lists each
#'   with `name` and `text` and optionally `kind` and `section`.
#' @return A `tfl_ai_prompt`: a list with `system`, `user` (strings),
#'   `attach` (what the user attaches, mode `"chat"`), `messages` (the
#'   system and user messages as a list of `role` / `content`), `task`,
#'   `mode`, `format` and `nchar`.  `format()` and `print()` give it as
#'   one text to copy.
#' @seealso [tfl_ai_parse()], [tfl_ai_check()], [tfl_ai_repair_prompt()],
#'   [tfl_ai_diff()].
#' @examples
#' ctx <- tfl_ai_context("toc", study = list(study_id = "CDISCPILOT01"),
#'                       reports = "T-14-1-1")
#' p <- tfl_ai_prompt("toc", ctx)
#' p$attach
#' cat(substr(format(p), 1, 800))
#'
#' # a figure, the documents' text in the prompt
#' ctx <- tfl_ai_context("figure", output_id = "F-14-2-1",
#'                       titles = "Kaplan-Meier Plot of Time to Death")
#' p <- tfl_ai_prompt("figure", ctx, mode = "api", documents = list(
#'   list(name = "SAP", section = "9.4", text = "Overall survival ...")))
#' p$nchar
#' @export
tfl_ai_prompt <- function(task, context = NULL, mode = c("chat", "api"),
                          format = c("yaml", "json"), example = TRUE,
                          documents = NULL) {
  fn <- "tfl_ai_prompt()"
  task <- .ai_task(task, fn)
  mode <- match.arg(mode)
  format <- match.arg(format)
  context <- .ai_prompt_context(task, context, fn)
  docs <- .ai_documents(documents, mode, fn)
  parts <- .ai_prompt_parts(task)
  fill <- list(
    task = parts$task,
    values = parts$values,
    format = format,
    header = .ai_header_text(task, context$output_id, format),
    content = parts$content,
    whole = parts$whole,
    language = .ai_language_line(context$language),
    format_rules = .ai_format_rules(format),
    schema = .ai_schema(task, templates = context$templates),
    example = if (isTRUE(example)) .ai_example_text(task, format) else ""
  )
  system <- .ai_fill(.ai_prompt_file("system"), fill)
  user <- paste(c(
    .ai_context_text(context, format), "", parts[[mode]],
    if (mode == "api") c("", .ai_documents_text(docs))
  ), collapse = "\n")
  attach <- if (mode == "chat") .ai_attach(task, context) else character()
  structure(list(
    system = system, user = user, attach = attach,
    messages = list(
      list(role = "system", content = system),
      list(role = "user", content = user)
    ),
    task = task, mode = mode, format = format,
    nchar = c(system = nchar(system), user = nchar(user))
  ), class = "tfl_ai_prompt")
}

#' @export
format.tfl_ai_prompt <- function(x, ...) {
  paste(c(
    x$system, "", "---", "", x$user,
    if (length(x$attach)) {
      c("", paste0("Attached: ", paste(x$attach, collapse = "; "), "."))
    }
  ), collapse = "\n")
}

#' @export
print.tfl_ai_prompt <- function(x, ...) {
  cat(format(x), "\n", sep = "")
  invisible(x)
}

# A string printed as itself.
.ai_text <- function(x) structure(x, class = c("tfl_ai_text", "character"))

#' @export
print.tfl_ai_text <- function(x, ...) {
  cat(x, "\n", sep = "")
  invisible(x)
}

.ai_prompt_context <- function(task, context, fn) {
  if (is.null(context)) {
    if (task == "figure") {
      .ard_stop(sprintf(
        "%s: a figure prompt needs a context with its `output_id` (%s).",
        fn, "tfl_ai_context()"
      ))
    }
    return(tfl_ai_context(task))
  }
  if (!inherits(context, "tfl_ai_context")) {
    .ard_stop(sprintf("%s: `context` is a tfl_ai_context().", fn))
  }
  if (!identical(context$task, task)) {
    .ard_stop(sprintf(
      "%s: the context is for the %s task, not %s.", fn, context$task, task
    ))
  }
  context
}

# The fixed text of a prompt file, as one string.
.ai_prompt_file <- function(name) {
  f <- system.file("ai", "prompts", paste0(name, ".md"), package = "tflspec")
  if (!nzchar(f)) {
    .ard_stop(sprintf("tflspec: the prompt text %s.md is not installed.", name))
  }
  x <- readLines(f, encoding = "UTF-8", warn = FALSE)
  paste(sub("\\s+$", "", x), collapse = "\n")
}

# A task's file as its parts: the text between `<!-- name -->` lines.
.ai_prompt_parts <- function(task) {
  x <- strsplit(.ai_prompt_file(task), "\n", fixed = TRUE)[[1L]]
  at <- grepl("^<!-- [a-z]+ -->$", x)
  name <- sub("^<!-- ([a-z]+) -->$", "\\1", x[at])
  grp <- cumsum(at)
  parts <- lapply(seq_along(name), function(i) {
    paste(x[grp == i & !at], collapse = "\n")
  })
  parts <- stats::setNames(lapply(parts, trimws), name)
  parts
}

# `{{name}}` replaced by its value, each once, in the order given (a value
# is not searched again).
.ai_fill <- function(text, values) {
  for (nm in names(values)) {
    pieces <- strsplit(text, paste0("{{", nm, "}}"), fixed = TRUE)[[1L]]
    if (endsWith(text, paste0("{{", nm, "}}"))) pieces <- c(pieces, "")
    text <- paste(pieces, collapse = values[[nm]])
  }
  # blank lines left by an empty value: at most one
  gsub("\n{3,}", "\n\n", text)
}

.ai_header <- function(task, output_id = NULL) {
  h <- list(task = task, version = .ai_version)
  if (task != "toc") h$output_id <- output_id %||% "<output_id>"
  h
}

.ai_header_text <- function(task, output_id, format) {
  h <- .ai_header(task, output_id)
  if (format == "yaml") {
    paste0("tflspec_ai: ", .ai_yaml_flow(h))
  } else {
    paste0("\"tflspec_ai\": ", jsonlite::toJSON(h, auto_unbox = TRUE))
  }
}

.ai_language_line <- function(language = NULL) {
  if (is.null(language)) {
    "Write the assumptions in the language of the user's message."
  } else {
    sprintf("Write the assumptions in %s.", language)
  }
}

.ai_format_rules <- function(format) .ai_prompt_file(paste0("format-", format))

.ai_attach <- function(task, context) {
  switch(task,
    toc = c(
      "the statistical analysis plan (SAP)",
      "the study's list of planned outputs, if it is a separate document"
    ),
    figure = c(
      "the statistical analysis plan (SAP)",
      sprintf("the shell (mock-up) of %s, if there is one", context$output_id)
    )
  )
}

# ---- the documents (mode "api") ---------------------------------------------

.ai_documents <- function(documents, mode, fn) {
  if (is.null(documents)) return(NULL)
  if (mode != "api") {
    .ard_stop(sprintf(paste0(
      "%s: `documents` are for mode \"api\"; in a chat the user attaches ",
      "the files."
    ), fn))
  }
  if (is.list(documents) && !is.null(documents$text)) {
    documents <- list(documents)
  }
  ok <- is.list(documents) && length(documents) &&
    all(vapply(documents, function(d) {
      is.list(d) && is.character(d$name) && length(d$name) == 1L &&
        is.character(d$text) &&
        !length(setdiff(names(d), c("name", "kind", "text", "section")))
    }, NA))
  if (!ok) {
    .ard_stop(sprintf(paste0(
      "%s: `documents` is a list of documents, each a list with `name` and ",
      "`text` and optionally `kind` and `section`."
    ), fn))
  }
  documents
}

.ai_documents_text <- function(docs) {
  if (!length(docs)) {
    return("No document text is included: work from the context above.")
  }
  attr_of <- function(d, k) {
    v <- d[[k]]
    if (is.null(v) || !length(v) || is.na(v[1L])) return("")
    sprintf(" %s=\"%s\"", k, gsub("\"", "'", v[1L], fixed = TRUE))
  }
  vapply(docs, function(d) {
    txt <- paste(d$text, collapse = "\n")
    txt <- gsub("</document>", "<\\/document>", txt, fixed = TRUE)
    paste0(
      "<document", attr_of(d, "name"), attr_of(d, "kind"),
      attr_of(d, "section"), ">\n", txt, "\n</document>"
    )
  }, "")
}

# ---- the context as text ----------------------------------------------------

# The context as the user's message shows it; an existing design in the
# answer's format.
.ai_context_text <- function(ctx, format = "yaml") {
  switch(ctx$task,
    toc = .ai_context_toc(ctx),
    figure = .ai_context_figure(ctx, format)
  )
}

.ai_context_study <- function(ctx) {
  s <- ctx$study
  if (!length(s)) return(character())
  txt <- paste(c(s$study_id, s$title), collapse = ": ")
  c("## The study", "", txt, "")
}

.ai_context_toc <- function(ctx) {
  rep <- ctx$reports
  c(
    .ai_context_study(ctx),
    "## The TOC's columns", "",
    paste0(
      "Fill these columns (the schema describes them): ",
      paste(sprintf("`%s`", ctx$toc_fields), collapse = ", "), "."
    ),
    "",
    "## Reports already in the TOC", "",
    .ai_reports_text(rep)
  )
}

.ai_reports_text <- function(rep) {
  if (is.null(rep) || !nrow(rep)) return("None: the TOC is empty.")
  cols <- if (all(is.na(rep$title))) "output_id" else c("output_id", "title")
  c(
    paste("Keep their numbers; add the reports the documents plan that are",
          "not here."),
    "", .ai_md_table(rep[cols])
  )
}

.ai_context_figure <- function(ctx, format = "yaml") {
  pop <- ctx$population
  pop_txt <- if (length(pop)) {
    paste0(
      paste(c(pop$id, pop$label), collapse = ", "),
      if (!is.null(pop$where)) sprintf(" (`%s`)", pop$where) else ""
    )
  }
  out <- c(
    .ai_context_study(ctx),
    "## The report", "",
    sprintf("- Report: %s", ctx$output_id),
    if (length(ctx$titles)) {
      sprintf("- Titles: %s", paste(ctx$titles, collapse = " | "))
    },
    if (length(pop_txt)) sprintf("- Analysis set: %s", pop_txt),
    ""
  )
  if (!is.null(ctx$datasets)) {
    out <- c(out, "## The datasets", "",
             "Name only these datasets and variables.", "",
             .ai_datasets_text(ctx$datasets))
  }
  if (!is.null(ctx$templates)) {
    tp <- paste(sprintf("`%s`", ctx$templates), collapse = ", ")
    out <- c(out, "## The templates", "",
             paste0("Start from one of these templates: ", tp, "."), "")
  }
  out <- c(out, "## The current design", "")
  if (is.null(ctx$design)) {
    out <- c(out, "There is none yet: start from the template that fits best.")
  } else {
    out <- c(
      out, "Change this design and return it whole:", "",
      paste0("```", format),
      .ai_write_answer(.fig_design_list(ctx$design), format), "```"
    )
  }
  out
}

.ai_datasets_text <- function(d) {
  out <- character()
  for (ds in unique(d$dataset)) {
    v <- d[d$dataset == ds, , drop = FALSE]
    lab <- v$dataset_label[!is.na(v$dataset_label)]
    out <- c(out, sprintf("### %s%s", ds,
                          if (length(lab)) paste0(": ", lab[1L]) else ""), "")
    more <- nrow(v) - .ai_max_variables
    v <- v[seq_len(min(nrow(v), .ai_max_variables)), , drop = FALSE]
    cols <- c("variable", "label", "kind", "levels")
    cols <- cols[vapply(cols, function(cn) !all(is.na(v[[cn]])), NA)]
    t <- v[cols]
    names(t)[names(t) == "levels"] <- "values"
    out <- c(out, .ai_md_table(t))
    if (more > 0L) {
      out <- c(out, "", sprintf("... and %d more variables.", more))
    }
    out <- c(out, "")
  }
  out
}

# A data frame as a markdown table; NA blank, `|` escaped.
.ai_md_table <- function(d) {
  cell <- function(v) {
    v <- as.character(v)
    v[is.na(v)] <- ""
    gsub("\n", " ", gsub("|", "\\|", v, fixed = TRUE), fixed = TRUE)
  }
  body <- if (nrow(d)) {
    do.call(paste, c(lapply(d, cell), sep = " | "))
  }
  c(
    paste0("| ", paste(names(d), collapse = " | "), " |"),
    paste0("|", paste(rep("---", ncol(d)), collapse = "|"), "|"),
    if (length(body)) paste0("| ", body, " |")
  )
}

# ---- the example answer ----------------------------------------------------

# An answer from the sample study, as the answer's format writes it.
.ai_example_text <- function(task, format) {
  x <- .ai_example(task)
  c(
    "\n## Example\n\n",
    "An answer from a sample study, not yours: it shows the shape, not the ",
    "content.\n\n",
    "```", format, "\n", .ai_write_answer(x, format), "\n```"
  ) |> paste(collapse = "")
}

.ai_example <- function(task) {
  switch(task,
    toc = list(
      tflspec_ai = .ai_header("toc"),
      toc = data.frame(
        output_id = c("T-14-1-1", "T-14-3-1", "F-14-2-1"),
        type = c("table", "table", "figure"),
        title = c(
          "Summary of Demographic and Baseline Characteristics",
          "Overview of Treatment-Emergent Adverse Events",
          "Kaplan-Meier Plot of Overall Survival"
        ),
        population = c("Safety Population", "Safety Population",
                       "Full Analysis Set"),
        footnote = c(
          "Percentages are based on the number of subjects in each group.",
          paste("TEAE: treatment-emergent adverse event. | A subject is",
                "counted once per row."),
          NA
        ),
        section = c("14.1 Demographic and Baseline Data",
                    "14.3 Safety Data", "14.2 Efficacy Data"),
        datasets = c("ADSL", "ADSL | ADAE", "ADTTE"),
        label = c("Table 14.1.1", "Table 14.3.1", "Figure 14.2.1"),
        stringsAsFactors = FALSE
      ),
      assumptions = c(
        "The SAP names no footnotes for Figure 14.2.1; left blank.",
        paste("Table 14.3.1 is listed in SAP section 10.2 but not in the TOC",
              "appendix; kept.")
      )
    ),
    figure = c(
      list(tflspec_ai = .ai_header("figure", "F-14-2-3")),
      .fig_design_list(tfl_fig_template(
        "km_risk_table", data = "ADTTE", param = "OS", pop = "FASFL",
        group = "TRT01P", time_unit = "months",
        title = "Kaplan-Meier Plot of Overall Survival"
      )),
      list(assumptions = c(
        paste("The SAP gives the time axis in months; the shell's ticks every",
              "6 months are not set (the default breaks are used)."),
        "The parameter is OS as in the SAP's section 9.4."
      ))
    )
  )
}

# ---- writing an answer ------------------------------------------------------

# An answer (a list: tflspec_ai, the parts, assumptions) as YAML or JSON.
.ai_write_answer <- function(x, format) {
  if (format == "json") return(.ai_write_json(x))
  out <- character()
  for (nm in names(x)) {
    v <- x[[nm]]
    if (is.null(v) || !length(v)) next
    if (is.data.frame(v)) {
      rows <- .ai_row_list(v)
      out <- c(out, paste0(nm, ":"),
               paste0("- ", vapply(rows, .ai_yaml_flow, "")))
    } else if (nm %in% c("tflspec_ai", "template", "ggplot2_version")) {
      out <- c(out, paste0(nm, ": ", .ai_yaml_flow(v)))
    } else if (nm == "plot") {
      out <- c(out, "plot:", paste0("  ", names(v), ": ",
                                    vapply(v, .ai_yaml_flow, "")))
    } else if (is.list(v) && is.null(names(v)) || nm == "assumptions") {
      out <- c(out, paste0(nm, ":"),
               paste0("- ", vapply(as.list(v), .ai_yaml_flow, "")))
    } else {
      y <- sub("\n$", "", yaml::as.yaml(stats::setNames(list(v), nm)))
      out <- c(out, y)
    }
  }
  paste(out, collapse = "\n")
}

.ai_write_json <- function(x) {
  x <- lapply(x, function(v) {
    if (is.data.frame(v)) {
      .ai_row_list(v)
    } else {
      .ai_json_ready(v)
    }
  })
  x <- x[!vapply(x, function(v) is.null(v) || !length(v), NA)]
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE, pretty = 2L,
                                null = "null", na = "null", digits = NA))
}

# A data frame's rows as lists, the NA cells left out.
.ai_row_list <- function(d) {
  lapply(seq_len(nrow(d)), function(i) {
    r <- as.list(d[i, , drop = FALSE])
    r[!vapply(r, function(z) is.na(z[1L]), NA)]
  })
}

# Raw R (!r) as the string "!r <code>" for JSON; vectors kept as arrays.
.ai_json_ready <- function(v) {
  if (.is_fig_r(v)) return(paste("!r", unclass(v)))
  if (is.list(v)) return(lapply(v, .ai_json_ready))
  if (is.atomic(v) && length(v) > 1L) return(I(v))
  v
}

# A value as YAML flow: a mapping {k: v}, a list [a, b], a scalar quoted
# as the prompt asks (a character of `.ai_yaml_special`, a blank at either
# end) or where YAML would not read it back as the same text.
.ai_yaml_flow <- function(v) {
  if (.is_fig_r(v)) return(paste("!r", .ai_yaml_quote(unclass(v))))
  if (is.list(v)) {
    items <- vapply(v, .ai_yaml_flow, "")
    if (!is.null(names(v)) && all(nzchar(names(v)))) {
      return(paste0("{", paste0(names(v), ": ", items, collapse = ", "), "}"))
    }
    return(paste0("[", paste(items, collapse = ", "), "]"))
  }
  if (length(v) != 1L) {
    return(paste0("[", paste(vapply(as.list(v), .ai_yaml_flow, ""),
                             collapse = ", "), "]"))
  }
  if (is.na(v)) return("null")
  if (is.logical(v)) return(if (v) "true" else "false")
  if (is.numeric(v)) return(format(v, scientific = FALSE, trim = TRUE))
  v <- as.character(v)
  if (grepl(.ai_yaml_special, v) || grepl("^\\s|\\s$", v)) {
    return(.ai_yaml_quote(v))
  }
  back <- tryCatch(yaml::yaml.load(paste0("[", v, "]")),
                   error = function(e) NULL)
  if (identical(back, list(v)) || identical(back, v)) v else .ai_yaml_quote(v)
}

.ai_yaml_special <- "[][{}:,#&*!|>'\"%]"

.ai_yaml_quote <- function(v) {
  v <- gsub("\\", "\\\\", v, fixed = TRUE)
  v <- gsub("\"", "\\\"", v, fixed = TRUE)
  v <- gsub("\n", "\\n", v, fixed = TRUE)
  paste0("\"", v, "\"")
}
