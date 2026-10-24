# ============================================================================
#  The AI flow with templates: `tfl_ai_prepare(templates = TRUE)`
# ----------------------------------------------------------------------------
#  A request then asks the AI to answer a report with `template: {name,
#  args}` when one of the study's templates fits (and the report's own
#  content items for what the template does not cover), else as a code
#  SPEC.  The templates are listed from the registry as it is when the
#  requests are written (tflspec's, a company's, a study's variants), never
#  by hand.  An answer's template is checked (its name, its arguments' names
#  and, cheaply, their types) and then expanded, so the code SPEC's checks
#  run on what it stands for.  `templates = FALSE` writes the requests as
#  before, byte for byte.
# ============================================================================

# A single TRUE / FALSE, or a stop
.ai_templates_flag <- function(templates, fn) {
  if (!is.logical(templates) || length(templates) != 1L || is.na(templates)) {
    .ard_stop(sprintf("%s: `templates` is TRUE (answer with templates where they fit) or FALSE (code SPEC only).",
                      fn))
  }
  templates
}

# The line a request starts with, and status.md's
.ai_templates_line <- function(templates) sprintf("templates: %s", if (isTRUE(templates)) "TRUE" else "FALSE")

# A template's description: its tpl_*() help page's title (before the
# colon), else its label, else its layout
.ai_tpl_description <- function(t) {
  title <- .ai_tpl_rd_title(paste0("tpl_", t$name))
  d <- if (!is.null(t$label) && nzchar(t$label)) t$label else title %||% sprintf("on the %s layout", t$layout)
  sub(":.*$", "", d)
}

.ai_tpl_rd_titles <- new.env(parent = emptyenv())
.ai_tpl_rd_title <- function(call) {
  if (is.null(.ai_tpl_rd_titles$db)) {
    db <- tryCatch(tools::Rd_db("tflspec"), error = function(e) list())
    if (!length(db)) {
      # (a source tree, devtools::load_all(): its man/ files)
      man <- system.file("man", package = "tflspec")
      f <- if (nzchar(man)) list.files(man, "[.]Rd$", full.names = TRUE) else character()
      db <- stats::setNames(lapply(f, function(x) tryCatch(tools::parse_Rd(x), error = function(e) NULL)),
                            basename(f))
    }
    titles <- character()
    for (rd in db) {
      if (is.null(rd)) next
      tags <- vapply(rd, function(x) attr(x, "Rd_tag") %||% "", "")
      al <- unlist(lapply(rd[tags == "\\alias"], function(x) paste(unlist(x), collapse = "")))
      tt <- rd[tags == "\\title"]
      if (!length(tt)) next
      ttl <- trimws(gsub("\\s+", " ", paste(unlist(tt[[1L]]), collapse = "")))
      for (a in al) titles[[a]] <- ttl
    }
    .ai_tpl_rd_titles$db <- titles
  }
  t <- .ai_tpl_rd_titles$db[call]
  if (is.na(t)) NULL else unname(t)
}

# A parameter's value as one short line of YAML
.ai_tpl_value <- function(v) {
  if (is.null(v)) return("~")
  # (a named vector as the mapping a report writes)
  if (is.atomic(v) && !is.null(names(v))) v <- as.list(v)
  s <- .ai_yaml_flow(v)
  if (nchar(s) > 70L) s <- paste0(substr(s, 1L, 67L), "...")
  s
}

# The templates a request may answer with: one block each -- name, what it
# is, its kind and layout (a variant: of which), then a line a parameter
# (the target ones marked: population, subset, where).  `kinds`: the kinds
# listed (a table request lists tables, a figure request figures).
# (`short`: each template's arguments on one line, their names only -- a
# chat's request over `max_chars` with the whole list)
.ai_templates_list <- function(kinds = c("table", "listing", "figure"), short = FALSE) {
  tt <- tfl_templates()
  tt <- tt[tt$kind %in% kinds, , drop = FALSE]
  target <- c("population", "subset", "where")
  out <- character()
  for (i in seq_len(nrow(tt))) {
    t <- .tpl_get(tt$name[i], "tfl_ai_prepare()")
    p <- .tpl_shown(tfl_template_params(tt$name[i]))
    from <- if (!is.na(tt$from[i])) sprintf("; variant of %s", tt$from[i]) else ""
    pkg <- if (!identical(tt$package[i], "tflspec")) sprintf("; %s's", tt$package[i]) else ""
    out <- c(out, sprintf("- `%s`: %s (%s, layout %s%s%s)", tt$name[i], .ai_tpl_description(t),
                          tt$kind[i], tt$layout[i], from, pkg))
    if (short) {
      if (length(p)) {
        out <- c(out, paste0("  - ", paste(vapply(p, function(x) {
          tgt <- if (identical(x$group, "target") || (is.null(x$group) && x$name %in% target)) " [target]" else ""
          sprintf("`%s`%s%s", x$name, tgt, if (isTRUE(x$required)) "*" else "")
        }, ""), collapse = ", ")))
      }
      next
    }
    for (x in p) {
      what <- x$type %||% "text"
      if (isTRUE(x$multiple)) what <- paste0(what, ", several")
      if (!is.null(x$of)) what <- paste0(what, " of ", x$of)
      if (length(x$choices)) what <- paste0(what, ": ", paste(x$choices, collapse = " | "))
      if (length(x$fields)) what <- paste0(what, " {", paste(x$fields, collapse = ", "), "}")
      tgt <- if (identical(x$group, "target") || (is.null(x$group) && x$name %in% target)) " [target]" else ""
      out <- c(out, sprintf("  - `%s`%s%s (%s) = %s%s", x$name, tgt,
                            if (isTRUE(x$required)) "*" else "", what, .ai_tpl_value(x$default),
                            if (!is.null(x$doc) && nzchar(x$doc)) paste0(": ", x$doc) else ""))
    }
  }
  out
}

# The instructions, the list and the example a request with templates
# holds (its system part, after the schema)
.ai_templates_text <- function(task, short = FALSE) {
  kinds <- if (identical(task, "figure")) "figure" else c("table", "listing")
  ex <- .ai_prompt_file(if (identical(task, "figure")) "templates-figure" else "templates-table")
  paste(c(ex, "", "### The templates", "",
          if (short) {
            paste("Each: its name, what it is, its kind and layout; then its parameters'",
                  "names -- [target] (the report's analysis set, subjects and records),",
                  "* required. A parameter left out takes its default or the study",
                  "standard's value.")
          } else {
            paste("Each: its name, what it is, its kind and layout; then its parameters --",
                  "`name` [target] (the report's analysis set, subjects and records),",
                  "* required, (its type), = its default, and what it is. A parameter left",
                  "out takes its default or the study standard's value.")
          }, "",
          .ai_templates_list(kinds, short = short)), collapse = "\n")
}

# ---- an agent's files -------------------------------------------------------
#  tfl_ai_prepare(mode = "agent"): what every request would repeat is a file
#  of the study's ai/ folder, written once -- the instructions and schema of
#  a task (ai/instructions-<task>.md), the templates (ai/templates.md), the
#  ADaM (ai/adam_summary.md), the study's spec/ files -- and a request names
#  them, with the templates likely for its reports.

# The file of a task's instructions
.ai_instructions_file <- function(task) file.path("ai", paste0("instructions-", task, ".md"))

# ai/templates.md: how to answer with a template, for a table and a figure,
# and every template registered, with its arguments
.ai_templates_file_text <- function() {
  how <- function(task) {
    sub("as the schema above says", sprintf("as the schema of `%s` says", .ai_instructions_file(task)),
        .ai_prompt_file(paste0("templates-", task)), fixed = TRUE)
  }
  kinds <- list(table = c("table", "listing"), figure = "figure")
  paste(c("# The study's templates", "",
          paste("Read when a request has `<!-- templates: TRUE -->` at its head. The",
                "templates are written from the registry (tflspec's, the company's, the",
                "study's variants) when the requests were; name no other."), "",
          unlist(lapply(names(kinds), function(task) {
            c(sub("^## ", sprintf("## %s: ", if (task == "table") "Tables and listings" else "Figures"),
                  how(task)), "",
              "### The templates", "",
              paste("Each: its name, what it is, its kind and layout; then its parameters --",
                    "`name` [target] (the report's analysis set, subjects and records),",
                    "* required, (its type), = its default, and what it is. A parameter left",
                    "out takes its default or the study standard's value."), "",
              .ai_templates_list(kinds[[task]]), "")
          }))), collapse = "\n")
}

# The words a title and a template are matched by: lower case, the usual
# names of a report kind as one word
.ai_tpl_words <- function(x) {
  x <- tolower(paste(x, collapse = " "))
  same <- c("adverse events?" = "ae", "system organ class(es)?" = "soc", "preferred terms?" = "pt",
            "kaplan[- ]meier" = "km", "demograph[a-z]*" = "demog", "laborator[a-z]*" = "lab",
            "vital signs?" = "vs", "hazard ratios?" = "hr", "\\bexposure[- ]adjusted\\b" = "eair",
            "incidence rates?" = "eair", "(objective )?response( rate)?" = "orr",
            "severity|toxicity|grades?" = "grade", "listings?" = "listing", "subgroups?" = "forest",
            "number at risk" = "risk", "confidence interval|\\bci\\b" = "ci",
            "standard error|\\bse\\b" = "se", "change from baseline|changes?" = "change",
            "abnormal[a-z]*" = "abnormal", "shifts?" = "shift", "overview|summary of ae" = "overview")
  for (p in names(same)) x <- gsub(p, paste0(" ", same[[p]], " "), x, perl = TRUE)
  w <- unique(strsplit(x, "[^a-z0-9]+")[[1L]])
  setdiff(w[nzchar(w)], c("a", "an", "and", "the", "of", "by", "at", "to", "in", "for", "on",
                          "first", "each", "most", "its", "table", "tables", "figure",
                          "plot", "time", "subjects", "patients", "number", "summary"))
}

# The templates likely for reports of these titles (at most `n`, the best
# first): a template's name's words count twice its description's
.ai_templates_likely <- function(titles, task, n = 3L) {
  tt <- tfl_templates()
  w <- .ai_tpl_words(titles)
  kinds <- if (identical(task, "figure")) "figure" else if ("listing" %in% w) "listing" else "table"
  tt <- tt[tt$kind %in% kinds, , drop = FALSE]
  if (!nrow(tt) || !length(w)) return(character())
  score <- vapply(seq_len(nrow(tt)), function(i) {
    nm <- strsplit(tt$name[i], "_", fixed = TRUE)[[1L]]
    d <- .ai_tpl_words(.ai_tpl_description(.tpl_get(tt$name[i], "tfl_ai_prepare()")))
    2 * sum(nm %in% w) + sum(setdiff(d, nm) %in% w)
  }, 0)
  o <- order(-score, tt$name)
  o <- o[score[o] > 0]
  tt$name[utils::head(o, n)]
}

# The lines an agent's request starts its message with: its instructions'
# file, its header, the templates' file and the likely ones
.ai_files_lines <- function(task, header, ctx) {
  # each report's title (a figure: its own)
  t <- if (is.data.frame(ctx$titles)) ctx$titles else
    data.frame(output_id = ctx$output_id %||% ctx$outputs %||% NA_character_,
               title = paste(ctx$titles, collapse = " "), stringsAsFactors = FALSE)
  likely <- if (isTRUE(ctx$ai_templates)) vapply(seq_len(nrow(t)), function(i) {
    x <- .ai_templates_likely(if (is.na(t$title[i])) "" else t$title[i], task)
    if (length(x)) paste(sprintf("`%s`", x), collapse = ", ") else "none from its title"
  }, "")
  c(sprintf(paste("Follow `%s`: the instructions and the schema, the same for every %s",
                  "request (read it once)."), .ai_instructions_file(task), task),
    sprintf("Your answer's header: `%s`.", header),
    if (isTRUE(ctx$ai_templates)) {
      c("", paste("Templates: `ai/templates.md` (how to answer with one, and every template",
                  "with its arguments). Likely, from the titles (another may fit better):"),
        "", sprintf("- %s: %s", t$output_id, likely))
    }, "")
}

# ---- an answer's template ---------------------------------------------------

# A report's template as an answer gives it: list(name, args), or NULL when
# the report has none (a figure design's `template:` is its preset's name,
# a string, not this)
.ai_answer_template <- function(o) {
  tp <- o$template
  if (is.null(tp) || !is.list(tp) || is.null(tp$name)) return(NULL)
  tp
}

# The problems of a report's template: an unknown template, an unknown
# argument, a required one missing, a value of the wrong type
.ai_check_template <- function(tp, at, kinds = NULL) {
  p <- .ai_problems()
  add <- function(where, what, severity = "error") {
    p <<- .ai_problem(p, paste(at, where), what, severity)
  }
  nm <- .ai_scalar_text(tp$name)
  have <- tfl_templates()
  if (!is.null(kinds)) have <- have[have$kind %in% kinds, , drop = FALSE]
  if (is.null(nm) || !nm %in% have$name) {
    add("template name", sprintf("%s is not a template; the templates are %s",
                                 sQuote(nm %||% "", FALSE), paste(have$name, collapse = ", ")))
    return(p)
  }
  params <- tfl_template_params(nm)
  args <- tp$args %||% list()
  if (!is.list(args) || (length(args) && is.null(names(args)))) {
    add("template args", "is a mapping of the template's parameters by name")
    return(p)
  }
  for (a in setdiff(names(args), names(params))) {
    add(paste("template args", a), sprintf("%s is not an argument of template %s; its parameters are %s",
                                           a, nm, paste(names(params), collapse = ", ")))
  }
  for (x in params) {
    v <- args[[x$name]]
    if (is.null(v)) next
    ok <- switch(x$type %||% "text",
                 flag = is.logical(v) && length(v) == 1L,
                 number = is.numeric(v),
                 vars = , levels = , text = , choice = is.character(v) || inherits(v, "tfl_r") ||
                   (is.list(v) && all(vapply(v, is.character, NA))),
                 TRUE)
    if (!isTRUE(ok)) {
      add(paste("template args", x$name), sprintf("of template %s is %s%s", nm, x$type,
                                                  if (isTRUE(x$multiple)) " (several)" else ""))
    } else if (identical(x$type, "choice") && length(x$choices) && !all(unlist(v) %in% x$choices)) {
      add(paste("template args", x$name), sprintf("of template %s is one of %s", nm,
                                                  paste(x$choices, collapse = ", ")))
    }
  }
  p
}

# A template report as the items it stands for (its own after them): as
# tfl_template_to_spec() writes it with the study, else from the template
# alone (the study's defaults of its parameters then not taken)
.ai_template_items <- function(o, study = NULL) {
  if (!is.null(study)) return(tfl_template_to_spec(o, study))
  tp <- o$template
  args <- tp$args %||% list()
  if (is.null(args$population) && "population" %in% names(tfl_template_params(tp$name)) &&
      !is.na(o$population %||% NA)) {
    args$population <- o$population
  }
  x <- tfl_template_expand(tp$name, args)
  o$ard <- x$ard
  if (!is.null(x$data)) o$data <- x$data
  o$content <- c(x$content, o$content %||% list())
  o$template <- NULL
  o
}

# ---- a figure answered with a template --------------------------------------

# The answer's template, its data stage (`data`: a design's data steps,
# before the template's call: read, the analysis set, param, filter) and
# its own layers (each a mapping, a design's layer); a design's other
# parts beside it are not read
.ai_read_tpl_figure <- function(x, p) {
  layers <- x$layers %||% list()
  if (!is.list(layers) || !is.null(names(layers)) ||
      !all(vapply(layers, function(z) is.list(z) && !is.null(z$layer), NA))) {
    p <- .ai_problem(p, "layers", "is a list of layers, each a mapping that starts with its `layer`")
    layers <- list()
  }
  data <- x$data
  if (!is.null(data) && (!is.list(data) || !is.null(names(data)) ||
                         !all(vapply(data, function(z) is.list(z) && !is.null(z$step), NA)))) {
    p <- .ai_problem(p, "data", "is a list of data steps, each a mapping that starts with its `step`")
    data <- NULL
  }
  for (k in intersect(names(x), c("stats", "plot", "plots", "compose"))) {
    p <- .ai_problem(p, k, "is a design's part: with a template it is not read (the template makes it)",
                     "warning")
  }
  list(design = structure(list(template = x$template, data = data, layers = layers),
                         class = "tfl_ai_tpl_figure"),
       problems = p)
}

# The figure's report file: its template, its own layers as content items
.ai_tpl_fig_output <- function(design, id, ctx) {
  ttl <- ctx$titles %||% character()
  pop <- ctx$population$id
  tp <- design$template
  content <- lapply(design$layers %||% list(), function(l) {
    tfl_item(l$layer, l[setdiff(names(l), "layer")])
  })
  tfl_output(id, kind = "figure", label = if (length(ttl)) ttl[1L] else NA_character_,
             population = if (length(pop)) pop else NA_character_,
             template = list(name = tp$name, args = tp$args %||% list()), content = content,
             data = if (length(design$data)) list(steps = design$data) else NULL,
             page = if (length(ttl)) list(titles = as.list(ttl)) else list())
}

# Its checks: the request's figure, the template's name and arguments, and
# the report resolved in the study and its program written
.ai_check_tpl_figure <- function(answer, context) {
  p <- .ai_problems()
  want <- context$output_id
  if (!is.null(want) && !is.null(answer$output_id) && !identical(answer$output_id, want)) {
    p <- .ai_problem(p, "tflspec_ai", sprintf("the answer is for %s; the prompt asked for %s",
                                              answer$output_id, want))
  }
  tp <- answer$design$template
  tpp <- .ai_check_template(tp, "", kinds = "figure")
  tpp$where <- trimws(tpp$where)
  p <- rbind(p, tpp)
  if (any(tpp$severity == "error") || is.null(context$study_dir)) return(p)
  st <- tryCatch(tfl_read_study_spec(context$study_dir), error = function(e) e)
  if (inherits(st, "error")) return(p)
  o <- .ai_tpl_fig_output(answer$design, want %||% answer$output_id %||% "F-1", context)
  r <- tryCatch({
    ro <- tfl_resolve_output(o, st)
    suppressMessages(tfl_content_code(ro, template = "items"))
    NULL
  }, error = function(e) .ai_one_line(conditionMessage(e)))
  if (!is.null(r)) p <- .ai_problem(p, "template", paste("could not be written:", r))
  p
}

# A figure report with a template, drawn in the study folder: the datasets
# its steps read (and its ARD when it has analyses), the template's design
# run, the PNG under the study's output path
.ai_tpl_fig_draw <- function(st, o, out_dir) {
  ds <- st$data$datasets %||% list()
  have <- toupper(vapply(ds, function(z) z$dataset %||% NA_character_, ""))
  steps <- o$data$steps %||% list()
  reads <- unique(toupper(unlist(lapply(steps, function(s) if (is.list(s)) s$dataset))))
  data <- list()
  for (r in reads) {
    i <- match(r, have)
    if (is.na(i)) .ard_stop(sprintf("%s reads %s, which spec/data.yml does not list.", o$id, r))
    data[[tolower(r)]] <- .ai_read_dataset(ds[[i]]$path)
  }
  if (length(o$ard$analyses %||% list())) {
    data$ard <- suppressMessages(tfl_build_ard(st, dir = ".", output_id = o$id, save = FALSE))
  }
  fig <- suppressMessages(suppressWarnings(tfl_content_run(o, data)))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  png <- file.path(out_dir, paste0(o$id, ".png"))
  ggplot2::ggsave(png, fig, width = 9, height = 5, dpi = 72)
  list(plot = fig, png = png)
}

# status.md's line of the setting: `templates: TRUE`; the requests of each
# when they differ (some added with the other)
.ai_templates_status <- function(tpl) {
  if (!length(tpl)) return(.ai_templates_line(FALSE))
  v <- unique(tpl)
  if (length(v) == 1L) return(.ai_templates_line(v))
  paste(vapply(c(TRUE, FALSE), function(x) sprintf("%s (%s)", .ai_templates_line(x),
                                                   paste(names(tpl)[tpl == x], collapse = ", ")), ""),
        collapse = "; ")
}

# ---- the comparison: with templates or the code SPEC only -------------------

# An answer that needed a repair, logged once by its content: request, md5
.ai_repair_log <- function(dir, request, file) {
  f <- file.path(dir, "_log.csv")
  md5 <- unname(tools::md5sum(file))
  old <- if (file.exists(f)) utils::read.csv(f, stringsAsFactors = FALSE) else
    data.frame(request = character(), md5 = character(), stringsAsFactors = FALSE)
  if (any(old$request == request & old$md5 == md5)) return(invisible())
  utils::write.csv(rbind(old, data.frame(request = request, md5 = md5, stringsAsFactors = FALSE)),
                   f, row.names = FALSE)
  invisible()
}

# The values a report file holds: its YAML's leaves
.ai_leaves <- function(x) {
  if (is.list(x)) return(sum(vapply(x, .ai_leaves, 0L)))
  length(x)
}

#' Compare a study's AI answers: with templates or the code SPEC only
#'
#' One row a report of the study's requests, for setting the two forms
#' side by side ([tfl_ai_prepare()]'s `templates`): the setting its request
#' was written with, what the answer used (a template, or a code SPEC), how
#' many values its report file holds (the leaves of its YAML: what a person
#' reviews), its checks' result and how many answers needed a repair.  Run
#' after [tfl_ai_run()] (or [tfl_ai_collect_answers()]): the answers are
#' read and checked again, and the table written to `ai/compare.csv`.
#'
#' @param study_dir The study folder.
#' @param file Where the table goes, under `study_dir`; `NULL` writes none.
#' @return A data frame, one row a report: `output_id`, `request`,
#'   `templates` (`TRUE`, `FALSE`), `answered_with` (`template:<name>` or
#'   `code`; `NA` with no report file), `values`, `check` (`ok`, `warning`,
#'   `error`, `no answer`) and `repairs`.
#' @seealso [tfl_ai_prepare()], [tfl_ai_run()].
#' @export
tfl_ai_compare <- function(study_dir, file = file.path("ai", "compare.csv")) {
  res <- suppressMessages(tfl_ai_collect_answers(study_dir))
  r <- res$reports
  log <- file.path(study_dir, "ai", "repairs", "_log.csv")
  log <- if (file.exists(log)) utils::read.csv(log, stringsAsFactors = FALSE) else
    data.frame(request = character(), md5 = character())
  yml <- file.path(study_dir, "spec", "outputs", paste0(r$output_id, ".yml"))
  read <- lapply(yml, function(f) if (file.exists(f)) {
    tryCatch(yaml::read_yaml(f, handlers = list(r = function(x) x)), error = function(e) NULL)
  })
  with <- vapply(read, function(y) {
    if (is.null(y)) return(NA_character_)
    if (is.list(y$template) && !is.null(y$template$name)) paste0("template:", y$template$name) else "code"
  }, "")
  out <- data.frame(
    output_id = r$output_id, request = r$request,
    templates = unname(res$templates[r$request]) %in% TRUE,
    answered_with = with,
    values = vapply(read, function(y) if (is.null(y)) NA_integer_ else .ai_leaves(y), 0L),
    check = r$status,
    repairs = vapply(r$request, function(q) sum(log$request == q), 0L, USE.NAMES = FALSE),
    stringsAsFactors = FALSE)
  if (!is.null(file)) {
    dir.create(dirname(file.path(study_dir, file)), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(out, file.path(study_dir, file), row.names = FALSE)
  }
  out
}
