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

# A line a template's block adds: a fact the agent found only by building
.ai_tpl_notes <- list(
  param_change = paste0(
    "`baseline: true` adds a Baseline column (rows, with `layout: rows`) from the same ",
    "records `where` keeps: with `where: ONTRTFL == \"Y\"` the baseline is empty; for a ",
    "baseline, `where: ONTRTFL == \"Y\" | ABLFL == \"Y\"`."),
  shift = paste0(
    "`levels` are the data's values, in order (`LOW`, `NORMAL`, `HIGH`); to print other ",
    "text (`Low`), give the report's `codelists` for both the post and the baseline ",
    "column (value, label), and `levels` may then be the codes or the labels; ",
    "`Missing` and `Total` are the template's rows: not in `levels`, and they need not ",
    "be in the code list; the rows are `levels`, then `Missing`, then `Total` (with a ",
    "code list for the baseline they always print, ",
    "empty when nobody is in them; without one, `Missing` prints only when a subject ",
    "has no baseline value)."),
  lab_shift = paste0(
    "`levels` are the data's values, in order (`LOW`, `NORMAL`, `HIGH`); to print other ",
    "text (`Low`), give the report's `codelists` for both the post and the baseline ",
    "column (value, label), and `levels` may then be the codes or the labels; ",
    "`Missing` and `Total` are the template's rows: not in `levels`, and they need not ",
    "be in the code list; the rows are `levels`, then `Missing`, then `Total` (with a ",
    "code list for the baseline they always print, ",
    "empty when nobody is in them; without one, `Missing` prints only when a subject ",
    "has no baseline value)."),
  km_survival = paste0(
    "the log-rank p-value is shown by default: set `pvalue: false` when the shell shows ",
    "none."),
  forest_hr = paste0(
    "the subgroup headings read the common ADaM variables' labels (SEX Sex, AGEGR1 Age ",
    "group, REGION1 Region ...); `subgroup_labels: {ECOGBL: ECOG}` gives the shell's own."))

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
    # (a template whose data stage is the report's: the rule beside it)
    own <- any(vapply(t$params %||% list(), function(p) isTRUE(p$hidden) && identical(p$stage, "data"), NA))
    out <- c(out, sprintf("- `%s`: %s (%s, layout %s%s%s)%s", tt$name[i], .ai_tpl_description(t),
                          tt$kind[i], tt$layout[i], from, pkg,
                          if (own) paste0(" -- own data: the report writes `data` steps `read`, the analysis ",
                                          "set, `param`; not the template's own steps (`survfit`, `time_unit`)",
                                          if (startsWith(tt$name[i], "km_")) paste0(
                                            "; the arms' order: a `levels` data step on the arm ",
                                            "variable, not `codelists`")) else ""))
    # (what a trial found only by building: a line under the template)
    if (!short && !is.null(.ai_tpl_notes[[tt$name[i]]])) {
      out <- c(out, paste0("  - note: ", .ai_tpl_notes[[tt$name[i]]]))
    }
    # (its catalog aliases: the same template, by another collection's name)
    al <- .tpl_aliases_of(tt$name[i])
    if (nrow(al)) {
      out <- c(out, paste0("  - aliases (the same template): ",
                           paste(sprintf("`%s` (%s %s)", al$alias, al$source, al$item),
                                 collapse = ", ")))
    }
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
      if (.tpl_is_name_text(x)) {
        what <- "mapping, name: text"
      } else if (length(x$fields)) {
        what <- paste0(what, " {", paste(x$fields, collapse = ", "), "}")
      }
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
  # (the example a draft file, as the agent writes it, not a chat's answer)
  how <- function(task) {
    x <- sub("as the schema above says", sprintf("as the schema of `%s` says", .ai_instructions_file(task)),
             .ai_prompt_file(paste0("templates-", task)), fixed = TRUE)
    x <- sub("(?s)\nAn example:.*$", "", x, perl = TRUE)
    x <- sub("- After the header: `template:", "- `template:", x, fixed = TRUE)
    x <- sub("- Then `assumptions`, as always.", "- Then `ai:`, as AGENTS.md says.", x, fixed = TRUE)
    paste(x, .ai_prompt_file(paste0("templates-", task, "-draft")), sep = "\n")
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
  # each report's title (a figure: its own; a request of shell sheets: its
  # reports' TOC titles)
  t <- if (is.data.frame(ctx$likely) && nrow(ctx$likely)) ctx$likely else
    if (is.data.frame(ctx$titles)) ctx$titles else
    data.frame(output_id = ctx$output_id %||% ctx$outputs %||% NA_character_,
               title = paste(ctx$titles, collapse = " "), stringsAsFactors = FALSE)
  likely <- if (isTRUE(ctx$ai_templates)) vapply(seq_len(nrow(t)), function(i) {
    x <- .ai_templates_likely(if (is.na(t$title[i])) "" else t$title[i], task)
    if (length(x)) paste(sprintf("`%s`", x), collapse = ", ") else "none from its title"
  }, "")
  c(sprintf(paste("Follow `%s`: the instructions and the schema, the same for every %s",
                  "request (read it once)."), .ai_instructions_file(task), task),
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
  # (an alias of the catalog's is its template, as itself)
  real <- if (!is.null(nm) && !nm %in% have$name) {
    tryCatch(.tpl_alias_target(nm, "tfl_ai_check()"), error = function(e) NULL)
  } else nm
  if (is.null(nm) || is.null(real) || !real %in% have$name) {
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
    # (a map written as a list of {name, text} is read as the mapping; one
    # that cannot be read says so here, not at the expanded item)
    if (.tpl_is_name_text(x)) {
      chk <- tryCatch(.tpl_name_text_map(v, nm, x$name, "tfl_ai_check()"), error = function(e) e)
      if (inherits(chk, "error")) {
        add(paste("template args", x$name), sub("^[^:]*: ", "", conditionMessage(chk)))
        next
      }
    }
    ok <- switch(x$type %||% "text",
                 flag = is.logical(v) && length(v) == 1L,
                 # (YAML reads [0.25, 1, 2] -- decimals and whole numbers -- as a list)
                 number = is.numeric(v) || (is.list(v) && length(v) &&
                                              all(vapply(v, function(e) is.numeric(e) && length(e) == 1L, NA))),
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

# A shift template's `levels` and `worst` against the data's values (the
# kit's `vars`: a variable's `levels`, ` | ` between the values, NA when the
# data was not read, the variable has too many values or is not coded).
# `levels` are the data's values; the report's `codelists` (value, label) for
# the post and the baseline column give other printed text, so a code list's
# values and labels pass too.  `worst` is matched against the raw data before
# any code list (.tpl_shift_code()): the data's values only.  No entry
# matches: an error; some do not: a warning (a category the data lacks has
# no row or column).  Nothing is said when the values are not known.
.ai_check_shift_levels <- function(tp, codelists, vars, at) {
  p <- .ai_problems()
  nm <- .ai_scalar_text(tp$name)
  if (is.null(nm) || is.null(vars) || !all(c("dataset", "variable", "levels") %in% names(vars))) {
    return(p)
  }
  real <- if (nm %in% tfl_templates()$name) nm else
    tryCatch(.tpl_alias_target(nm, "tfl_ai_check()"), error = function(e) NULL)
  if (is.null(real) || !real %in% c("shift", "lab_shift")) return(p)
  args <- tp$args %||% list()
  pp <- tfl_template_params(real)
  # (lab_shift has no data / baseline / post parameter: the shift defaults)
  arg <- function(k) args[[k]] %||% (pp[[k]] %||% .tpl_shift_params[[k]])$default
  texts <- function(v) {
    if (is.null(v) || inherits(v, "tfl_r")) return(NULL)
    v <- unlist(v, use.names = FALSE)
    if (is.character(v) && length(v) && !anyNA(v)) v else NULL
  }
  ds <- texts(arg("data"))
  post <- texts(arg("post"))
  base <- texts(arg("baseline"))
  if (length(ds) != 1L || length(post) != 1L || length(base) != 1L) return(p)
  r <- which(vars$dataset == toupper(ds) & vars$variable == post)
  have <- if (length(r)) vars$levels[r[1L]] else NA_character_
  if (is.na(have) || !nzchar(have)) return(p)
  vals <- strsplit(have, " | ", fixed = TRUE)[[1L]]
  # (a report's code list for a column: its values and its labels)
  cl_text <- function(v) {
    tryCatch(unlist(lapply(.tpl_cl_rows(codelists[[v]]), function(r) {
      c(as.character(r$value), as.character(r$label))
    })), error = function(e) character())
  }
  where <- sprintf("%s (%s)", sprintf("%s.%s", toupper(ds), post), paste(vals, collapse = ", "))
  one <- function(k, ok, allowed_in, fix, wont = "they will not print",
                  entries = texts(args[[k]])) {
    if (is.null(entries)) return()
    good <- entries %in% ok
    if (all(good)) return()
    bad <- paste(entries[!good], collapse = ", ")
    p <<- if (any(good)) {
      .ai_problem(p, paste(at, "template args", k), sprintf(
        "%s of template %s: %s match no value of %s%s; %s",
        k, real, bad, where, allowed_in, wont), "warning")
    } else {
      .ai_problem(p, paste(at, "template args", k), sprintf(
        "%s of template %s: %s match no value of %s%s; %s",
        k, real, bad, where, allowed_in, fix))
    }
  }
  # (the Missing and Total rows are the template's: an entry of them in
  # `levels` would be a second one -- the build stops on the duplicate)
  lv <- texts(args$levels)
  own <- c(texts(arg("missing_label")), texts(arg("total_label")))
  for (e in unique(lv[lv %in% own])) {
    p <- .ai_problem(p, paste(at, "template args levels"), sprintf(
      "levels of template %s: `%s` is a row the template adds: leave it out of `levels`", real, e))
  }
  # (a code list's text counts when both columns have it: one column's alone
  # leaves the other unmatched)
  one("levels", c(vals, intersect(cl_text(post), cl_text(base))),
      sprintf(" or a value or label the report's `codelists` give for both %s and %s", post, base),
      sprintf(paste0("`levels` are the data's values; for other printed text give ",
                     "`codelists` with value and label for %s and %s"), post, base),
      entries = lv[!lv %in% own])
  one("worst", vals, " (a code list's label does not count)",
      "`worst` is matched against the data's values, before any code list: write the values",
      wont = "they rank nothing, so check the order")
  p
}

# A draft report's code lists as its build has them: its own over the ones
# of the layers it extends (per variable, as tfl_report_codelists() gives
# them).  NULL when the draft extends something and the study cannot be
# read (or cannot give the layers): they are not known, so a check on
# them is left out.
.ai_draft_codelists <- function(o, study) {
  own <- o$codelists %||% list()
  if (!length(o$extends) || identical(o$extends, "none")) return(.codelists_norm(own))
  if (is.null(study)) return(NULL)
  below <- tryCatch(.layers_below(o, study)$acc$codelists, error = function(e) NULL)
  if (is.null(below)) return(NULL)
  .codelists_norm(.merge_map(below, own) %||% list())
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
# its own layers (each a mapping, a design's layer) and its settings
# (`plot`, over the template's, as a report written by hand); a design's
# other parts beside it are not read
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
  plot <- x$plot
  if (!is.null(plot) && (!is.list(plot) || (length(plot) && is.null(names(plot))))) {
    p <- .ai_problem(p, "plot", "is a mapping of the figure's settings (x_label, y_by, legend ...)")
    plot <- NULL
  }
  for (k in intersect(names(x), c("stats", "plots", "compose"))) {
    p <- .ai_problem(p, k, "is a design's part: with a template it is not read (the template makes it)",
                     "warning")
  }
  list(design = structure(list(template = x$template, data = data, layers = layers, plot = plot),
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
  o <- tfl_output(id, kind = "figure", label = if (length(ttl)) ttl[1L] else NA_character_,
                  population = if (length(pop)) pop else NA_character_,
                  template = list(name = tp$name, args = tp$args %||% list()), content = content,
                  data = if (length(design$data)) list(steps = design$data) else NULL,
                  page = if (length(ttl)) list(titles = as.list(ttl)) else list())
  if (length(design$plot)) o$plot <- design$plot
  o
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
  # (its data steps, when the report says them: the analysis set kept --
  # the parameter the template's own check says)
  if (length(answer$design$data)) {
    fr <- .ai_fig_required(answer$design$data, answer$design["layers"], context)
    p <- rbind(p, fr[!grepl("keeps no parameter", fr$problem), , drop = FALSE])
  }
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
    .data_file_must_exist(r, ds[[i]]$path, ".")
    data[[tolower(r)]] <- .ai_read_dataset(ds[[i]]$path)
  }
  if (length(o$ard$analyses %||% list())) {
    data$ard <- suppressMessages(tfl_build_ard(st, dir = ".", output_id = o$id, save = FALSE))
  }
  fig <- suppressMessages(suppressWarnings(tfl_content_run(o, data)))
  .ai_fig_nothing_drawn(fig, o$id)
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
  # (the drafts' folder in a study with the AI's drafts apart)
  yml <- file.path(study_dir, .ai_area(study_dir) %||% file.path("spec", "outputs"),
                   paste0(r$output_id, ".yml"))
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
