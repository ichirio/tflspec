# ============================================================================
#  AI-assisted drafting, the `table` task: report files of the YAML spec
# ----------------------------------------------------------------------------
#  A batch of 5 to 10 tables of one family, drafted from the TOC / SHELL
#  workbook (R/ai_shell.R) and the ADaM summary (R/ai_adam.R), over the
#  study's spec/data.yml and spec/defaults.yml (tfl_ai_study_init()).  The
#  answer is one YAML block, the header then `outputs:` (one report file
#  each, as tfl_read_output() reads a file of several reports) and the
#  `assumptions`.  The checks are the spec's own: the reader, the schema
#  (tfl_schema_check()), the ARD program and the content code written
#  from a study folder holding the answer -- and the ADaM summary for the
#  datasets and variables named.
# ============================================================================

# The report file's keys an answer may write, in the order written.
.ai_table_keys <- c("id", "kind", "label", "population", "extends", "ard", "data",
                    "codelists", "content", "page", "template")

# The functions a table's content may call: the table's stage of the schema
# (rtfreporter's plan verbs and normalize_ard), in their rank.
.ai_table_functions <- function() {
  s <- tfl_schema()
  keep <- vapply(names(s), function(f) {
    e <- s[[f]]
    identical(e$package, "rtfreporter") && is.null(e$figure) &&
      (f %in% c("normalize_ard", "table_plan") || startsWith(f, "plan_")) &&
      !f %in% c("plan_listing", "plan_titles", "plan_footnotes", "plan_after")
  }, NA)
  f <- names(s)[keep]
  rank <- vapply(f, function(x) as.numeric(s[[x]]$rank %||% 999), 1)
  rank[f == "normalize_ard"] <- 0
  f[order(rank)]
}

# The fields of the ARD's two parts, as the spec's columns say them, under
# their YAML names.
.ai_table_ard_fields <- function() {
  cols <- tfl_spec_columns()
  part <- function(sheet, rename) {
    d <- cols[cols$sheet %in% sheet & !is.na(cols$column), , drop = FALSE]
    d <- d[d$column != "output_id", , drop = FALSE]
    nm <- d$column
    for (k in names(rename)) nm[nm == k] <- rename[[k]]
    # R columns are one `!r` (several derivations: ` | ` between them);
    # the others' ` | ` lists are YAML lists
    r <- nm %in% c("where", "derive", "code", "args")
    txt <- d$description
    txt[!r] <- gsub(",? ?\\| between them", " (a list)", txt[!r])
    txt[r] <- paste(txt[r], "Written `!r`.")
    data.frame(field = nm, description = txt, stringsAsFactors = FALSE)
  }
  list(
    analysis_data = part("analysis_data", c(data_id = "id", population_id = "population")),
    analyses = part("analyses", c(analysis_id = "id", population_id = "population"))
  )
}

# ---- the context -------------------------------------------------------------

.ai_ctx_table <- function(out, given, fn) {
  if (!is.null(given$outputs)) {
    if (!is.character(given$outputs) || !length(given$outputs) || anyNA(given$outputs)) {
      .ard_stop(sprintf("%s: `outputs` is the report numbers to draft, a character vector.", fn))
    }
    out$outputs <- unique(trimws(given$outputs))
    if (length(out$outputs) > 12L) {
      message(sprintf(paste0(
        "%s: %d reports in one request; a chat drafts 5 to 10 well ",
        "(tfl_ai_batches())."), fn, length(out$outputs)))
    }
  }
  sh <- given$shell
  if (inherits(sh, "tfl_ai_shell")) {
    if (!is.null(out$outputs) && !length(sh$outputs)) {
      # as the workbook was read: its TOC sheet and sheets as given; never
      # cut (a shell cut in half misleads; a request too long is split by
      # tfl_ai_write_requests(max_chars =), a report at a time)
      # (from what was read: the file is read once, not a request at a time)
      sh <- .ai_shell_for(sh, out$outputs, fn)
    }
    toc <- sh$toc
    if (!is.null(out$outputs)) {
      at <- vapply(.ai_id_key(out$outputs), function(k) {
        i <- .ai_id_match(k, toc$key)
        if (length(i)) i[1L] else NA_integer_
      }, 1L)
      out$titles <- data.frame(output_id = out$outputs, title = toc$title[at],
                               stringsAsFactors = FALSE)
    }
    out$shell <- sh$text
  } else if (!is.null(sh)) {
    if (!is.character(sh)) {
      .ard_stop(sprintf("%s: `shell` is a tfl_ai_read_shell() result or text.", fn))
    }
    out$shell <- paste(sh, collapse = "\n")
  }
  if (!is.null(given$study_dir)) {
    d <- given$study_dir
    if (!is.character(d) || length(d) != 1L || !dir.exists(d)) {
      .ard_stop(sprintf("%s: `study_dir` is the study folder (tfl_ai_study_init()).", fn))
    }
    st <- tfl_read_study_spec(d)
    out$study_dir <- normalizePath(d, winslash = "/")
    if (is.null(out[["study"]]) && !is.null(st$study$study_id)) {
      out[["study"]] <- .ai_ctx_study(Filter(Negate(is.null), list(
        study_id = st$study$study_id, title = st$study$title)), fn)
    }
    rd <- function(f) {
      f <- file.path(.spec_dir(d), f)
      if (file.exists(f)) paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    }
    out$data_yml <- .ai_data_yml_names(rd("data.yml"))
    out$defaults_yml <- rd("defaults.yml")
    out$populations <- vapply(st$data$populations %||% list(), function(p) p$id %||% "", "")
    out$patterns <- names(st$patterns)
  }
  if (!is.null(given$examples)) {
    ex <- given$examples
    if (!is.character(ex) || !all(file.exists(ex))) {
      .ard_stop(sprintf("%s: `examples` are the paths of reviewed report files (spec/outputs/<id>.yml).", fn))
    }
    out$examples <- stats::setNames(
      vapply(ex, function(f) paste(.ai_read_lines(f, fn), collapse = "\n"), ""),
      basename(ex))
  }
  out
}

# ---- the context as text -------------------------------------------------------

.ai_context_table <- function(ctx) {
  out <- .ai_context_study(ctx)
  out <- c(out, "## The reports to write", "")
  if (isTRUE(ctx$sheet_mode)) {
    out <- c(out, paste("The reports the shell sheet(s) below make: one report file a report,",
                        "its id as the lines above the workbook's text say; write the ids you",
                        "chose in the header's `outputs`."), "")
  } else if (!is.null(ctx$titles)) {
    t <- ctx$titles
    t$title[is.na(t$title)] <- ""
    out <- c(out, paste("One report file each, in this order; `id` is the number as",
                        "written here."), "", .ai_md_table(t), "")
  } else if (!is.null(ctx$outputs)) {
    out <- c(out, paste("One report file each, in this order; `id` is the number as",
                        "written here:"), "",
             paste0("- ", ctx$outputs), "")
  } else {
    out <- c(out, "Every table of the TOC and shells.", "")
  }
  # an agent's request: the files it reads, once, not their text
  files <- isTRUE(ctx$ai_files)
  yml <- function(x) if (files) character() else c("", "```yaml", x, "```")
  if (!is.null(ctx$data_yml) || !is.null(ctx$defaults_yml)) {
    out <- c(out, if (files) "## The study's files, already written: read them" else
                    "## The study's files, already written", "")
    if (!is.null(ctx$data_yml)) {
      out <- c(out, paste("`spec/data.yml`: the datasets and the analysis sets. Name an",
                          "analysis set by its `id`", if (length(ctx$populations))
                            sprintf("(%s)", paste(ctx$populations, collapse = ", ")),
                          "in `population`; write no other."),
               yml(ctx$data_yml), "")
    }
    if (!is.null(ctx$defaults_yml)) {
      out <- c(out, paste("`spec/defaults.yml`: what every report starts from",
                          "(`extends: [defaults]`). Do not repeat what it gives; an item",
                          "of the same function in a report replaces it (`plan_cells`,",
                          "`plan_labels`, `plan_levels`, `plan_digits`: merged by key)."),
               yml(ctx$defaults_yml), "")
    }
  }
  if (!is.null(ctx$datasets) && files) {
    out <- c(out, "## The ADaM datasets", "",
             paste("`ai/adam_summary.md`: name only its datasets, variables and values",
                   "(a value as the data write it)."), "")
  } else if (!is.null(ctx$datasets)) {
    out <- c(out, "## The ADaM datasets", "",
             paste("Name only these datasets, variables and values (a value as the",
                   "data write it). No record is shown."), "",
             .ai_datasets_compact(ctx$datasets), "")
  }
  if (length(ctx$examples)) {
    out <- c(out, "## Reviewed reports of this study", "",
             paste("A person has checked these report files. Write the new reports",
                   "the same way: the same analyses, items, cells, header and page",
                   "where the shell is alike; change only what the shell asks."), "")
    for (nm in names(ctx$examples)) {
      out <- c(out, sprintf("`%s`:", nm), "", "```yaml", ctx$examples[[nm]], "```", "")
    }
  }
  if (!is.null(ctx$shell)) {
    out <- c(out, "## The TOC and the shells", "", ctx$shell, "")
  }
  out
}

# The datasets, one line a variable: `NAME (type) Label: values`.
.ai_datasets_compact <- function(d) {
  unlist(lapply(unique(d$dataset), function(ds) {
    v <- d[d$dataset == ds, , drop = FALSE]
    lab <- v$dataset_label[!is.na(v$dataset_label)]
    kind <- unname(.ai_adam_kind[v$kind])
    kind[is.na(kind)] <- ifelse(is.na(v$kind[is.na(kind)]), "", v$kind[is.na(kind)])
    c(sprintf("### %s%s", ds, if (length(lab)) paste0(": ", lab[1L]) else ""),
      paste0(v$variable, ifelse(nzchar(kind), paste0(" (", kind, ")"), ""),
             ifelse(is.na(v$label), "", paste0(" ", v$label)),
             ifelse(is.na(v$levels), "", paste0(": ", v$levels))),
      "")
  }), use.names = FALSE)
}

# ---- the schema --------------------------------------------------------------------

# Parts: "report", "ard", "listing", or a content function's name.
.ai_schema_table <- function(parts = NULL) {
  fns <- .ai_table_functions()
  parts <- parts %||% c("report", "ard", "listing", fns)
  out <- character()
  if ("report" %in% parts) {
    out <- c(out, "### The report file", "",
      "One mapping a report, with these keys (a key marked * is required):", "",
      "- `id`*: the report's number as the user's message writes it (its file's name keeps letters, digits, '.', '_' and '-').",
      "- `kind`*: `table`; `listing` for a listing (its own section below).",
      "- `label`: the report's title, without its number and analysis set.",
      "- `population`: the analysis set's `id` (spec/data.yml).",
      "- `extends`: `[defaults]`: the study's defaults under this report.",
      "- `ard`: the report's analyses, made before any table: `analysis_data` (named data the analyses read) and `analyses` (one cards / cardx call each). Below.",
      "- `data`: `{source: ard}`: the table reads its rows of the study ARD.",
      "- `codelists`: by variable, its values in the order printed, each `{value: <as in the data>, label: <as printed>}`; the ARD then holds the labels, in that order.",
      "- `content`: the table: a list of items, one a function call written `- <function>: {<argument>: <value>, ...}` (the functions below). `table_plan` first.",
      "- `page`: `titles` and `footnotes`, each a mapping by line number (`\"1\": {center: Table 14.1.1}`, `\"1\": {left: <footnote>}`); the line keys are quoted.",
      "")
  }
  if ("ard" %in% parts) {
    f <- .ai_table_ard_fields()
    m <- tfl_ard_methods()
    out <- c(out, "### ard: analysis_data", "",
             "Each a mapping with these fields (all optional but `id` and `from`):", "",
             sprintf("- `%s`: %s", f$analysis_data$field, f$analysis_data$description), "",
             "### ard: analyses", "",
             "Each a mapping with these fields (`id`, `method` and, but for `total_n`, `variables` required):", "",
             sprintf("- `%s`: %s", f$analyses$field, f$analyses$description), "",
             "The methods (`method`):", "",
             sprintf("- `%s`: %s (%s)%s", m$method, m$label, m$call,
                     ifelse(is.na(m$note) | !nzchar(m$note), "", paste0("; ", m$note))),
             "")
  }
  s <- tfl_schema()
  # a function's arguments, as the schema has them
  fn_lines <- function(fn) {
    e <- s[[fn]]
    out <- sprintf("- `%s`: %s", fn, .ai_help(e$description %||% "", "") %||% "")
    if (!is.null(e$dots)) {
      d <- e$dots
      logical_in <- function(v) is.logical(v) || (is.list(v) && any(vapply(v, logical_in, NA))) ||
        any(names(v) %in% c("TRUE", "FALSE"))
      ex <- if (!is.null(d$example) && !logical_in(d$example)) {
        sprintf(" Example: `%s: %s`", fn,
                if (identical(d$style, "positional")) .ai_yaml_flow(stats::setNames(list(d$example), d$key))
                else .ai_yaml_flow(d$example))
      } else ""
      what <- if (identical(d$style, "positional")) sprintf("`%s`", d$key) else "`...`"
      out <- c(out, sprintf("  - %s (%s)%s%s", what, d$type, .ai_help(d$description %||% "", ": "), ex))
    }
    for (a in names(e$args)) {
      x <- e$args[[a]]
      if (!is.null(x$renamed_to)) next
      # an argument the schema lists for a form only (its label, no
      # description): the AI is not told of it (its request stays short)
      if (is.null(x$description) && !is.null(x$label)) next
      out <- c(out, sprintf("  - `%s` (%s)%s", a, x$type %||% "value",
                            .ai_help(x$description %||% "", ": ")))
    }
    out
  }
  if ("listing" %in% parts) {
    out <- c(out, "### A listing", "",
      "A listing (`kind: listing`) prints records, not statistics: it has no `ard: analyses` and no `table_plan`.", "",
      "- `data`*: the records: `{source: dataset, dataset: <ADaM dataset>, population: <analysis set id>, where: !r '<R condition on its columns>', sort: [<variable>, -<variable, descending>]}`; or `{source: dataset, data: <an analysis_data id of this report>}`, that data in `ard: analysis_data` (its `population` there), when the records need more than a condition (ADSL's columns added, derived columns).",
      "- The analysis set is required: `data.population`, or the analysis data's. A dataset of parameters (PARAMCD: ADLB, ADVS ...) is kept to the shell's (`where: !r 'PARAMCD == \"ALT\"'`) or shows PARAM / PARAMCD in a column. When the shell does not say them, ask -- do not guess.",
      "- `content`*: one `plan_listing` item, its `cols` the shell's columns in order, each `{fn: listing_col, args: {vars: [<variables stacked in the column>], label: <header>, width: <characters>, collapse_repeats: true}}` (`collapse_repeats`: a value printed once while it repeats, as a subject's id); `type: multiline` unless the shell is one line a record. Then, if the shell says how many rows a page, `plan_paginate_rows: {max_rows: <n>}`.",
      "- `page`: its titles and footnotes, as a table's.",
      "",
      fn_lines("plan_listing"), fn_lines("listing_col"), "")
  }
  fns <- intersect(fns, parts)
  if (length(fns)) {
    out <- c(out, "### The content's functions", "",
             "Each item's arguments by name; `...` means the item's keys are the entries (`plan_labels: {AGE: Age (years)}`). Leave out an argument to take its default.",
             "")
    for (fn in fns) out <- c(out, fn_lines(fn))
    out <- c(out, "",
      "### The types", "",
      "- `column`: a column of the normalized ARD (a by or analysis variable, `variable`, `.label` ...) or of the table (`row_label`, `.values`, an arm's name).",
      "- `stat`: a statistic's name (`n`, `N`, `p`, `mean`, `sd`, `median`, `min`, `max`, `estimate`, `conf.low`, `conf.high`, `p.value` ...).",
      "- `template`: cell text with statistics in braces and their format: `\"{n} ({p:.1f%})\"`, `\"{mean:.1f} ({sd:.2f})\"`. Always quoted.",
      "- `r`: R code, tagged `!r`: `where: !r 'TRTEMFL == \"Y\"'`.",
      "- `list<T>`: a YAML list; `vec<K, V>` and `map<K, V>`: a YAML mapping; `call<fn>`: `{fn: <fn>, args: {...}}`; `T|U`: either.",
      "")
  }
  paste(out, collapse = "\n")
}

# ---- the example -----------------------------------------------------------------

.ai_example_table_text <- function(format) {
  f <- system.file("ai", "example", "table-answer.md", package = "tflspec")
  x <- readLines(f, warn = FALSE, encoding = "UTF-8")
  blk <- .ai_blocks(paste(x, collapse = "\n"))
  body <- blk[[1L]]$text
  if (format == "json") {
    v <- yaml::yaml.load(body, handlers = .yaml_handlers())
    body <- .ai_write_json(.ai_json_table(v))
  }
  paste(c("\n## Example\n",
          "An answer from a sample study, not yours: it shows the shape, not the content.\n",
          paste0("```", format), body, "```"), collapse = "\n")
}

# !r values as the "!r <code>" strings JSON carries
.ai_json_table <- function(v) {
  if (inherits(v, "tfl_r")) return(paste("!r", unclass(v)))
  if (is.list(v)) return(lapply(v, .ai_json_table))
  if (is.atomic(v) && length(v) > 1L) return(I(v))
  v
}

# ---- reading the answer ------------------------------------------------------------

# outputs: a list of report files, each read as tfl_read_output() reads
# one; the assumptions as rows (output_id, where, item, assumption,
# question) when they are mappings.
.ai_read_table <- function(x) {
  p <- .ai_problems()
  outs <- list()
  v <- x$outputs
  if (is.null(v)) {
    return(list(outputs = outs, problems = .ai_problem(p, "outputs", "the answer has no `outputs`")))
  }
  if (.is_map(v) && all(vapply(v, is.list, NA))) {
    # a mapping by id: each its report
    v <- lapply(names(v), function(id) c(list(id = id), v[[id]][setdiff(names(v[[id]]), "id")]))
  }
  if (!is.list(v) || !is.null(names(v))) {
    return(list(outputs = outs, problems = .ai_problem(p, "outputs", "is a list of report files, one mapping each")))
  }
  for (i in seq_along(v)) {
    o <- v[[i]]
    at <- sprintf("outputs[%d]", i)
    if (!is.list(o) || is.null(names(o))) {
      p <- .ai_problem(p, at, "is not a mapping (a report file)")
      next
    }
    id <- .ai_scalar_text(o$id)
    # the report's number as the TOC writes it ("Table 14.1.1", as the
    # prompt asks): its file's id has letters, digits, . _ - only
    if (!is.null(id) && !grepl("^[A-Za-z0-9._-]+$", id)) o$id <- id <- .ai_file_id(id)
    if (!is.null(id)) at <- paste(at, id)
    for (k in setdiff(names(o), .ai_table_keys)) {
      p <- .ai_problem(p, paste(at, k), sprintf(
        "is not a key of a report file; its keys are %s", paste(.ai_table_keys, collapse = ", ")))
    }
    r <- tryCatch(.output_from_list(o[intersect(names(o), .ai_table_keys)]),
                  error = function(e) e)
    if (inherits(r, "error")) {
      p <- .ai_problem(p, at, paste("could not be read:", .ai_one_line(conditionMessage(r))))
      next
    }
    if (r$id %in% names(outs)) {
      p <- .ai_problem(p, at, sprintf("%s is in the answer twice", r$id))
      next
    }
    outs[[r$id]] <- r
  }
  list(outputs = outs, problems = p)
}

# spec/data.yml for a prompt: each dataset's `path:` as its file's name
# alone.  The ADaM folder may be a server's (\\server\study\adam, C:/...):
# where it is is no business of the chat's, and the file stays as it is.
.ai_data_yml_names <- function(x) {
  if (is.null(x)) return(x)
  lines <- strsplit(x, "\n", fixed = TRUE)[[1L]]
  at <- grepl("^\\s*(-\\s+)?path:\\s*\\S", lines, perl = TRUE)
  if (!any(at)) return(x)
  head <- sub("^(\\s*(-\\s+)?path:\\s*).*$", "\\1", lines[at], perl = TRUE)
  val <- trimws(substring(lines[at], nchar(head) + 1L))
  val <- sub("\\s+#.*$", "", val, perl = TRUE)
  val <- gsub("^[\"']|[\"']$", "", val, perl = TRUE)
  lines[at] <- paste0(head, basename(gsub("\\\\", "/", val)))
  paste(lines, collapse = "\n")
}

.ai_assumption_rows <- function(x) {
  empty <- data.frame(output_id = character(), where = character(), item = character(),
                      assumption = character(), value = character(),
                      question = character(), from = character(), stringsAsFactors = FALSE)
  if (is.null(x) || !length(x)) { empty$from <- NULL; return(empty) }
  cell <- function(e, k) {
    v <- e[[k]]
    if (is.null(v) || !length(v) || is.list(v)) NA_character_ else as.character(v[[1L]])
  }
  rows <- lapply(x, function(e) {
    if (!is.list(e)) e <- list(assumption = as.character(e))
    if (!is.null(e$output_id) && length(e$output_id) == 1L && !is.list(e$output_id)) {
      e$output_id <- .ai_file_id(e$output_id)
    }
    data.frame(output_id = cell(e, "output_id"), where = cell(e, "where"),
               item = cell(e, "item"), assumption = cell(e, "assumption"),
               value = .ai_assumption_value(e$value),
               question = cell(e, "question"), from = cell(e, "from"), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, c(list(empty), rows))
  # (`from`: the decision an assumption was taken from, the spec route's;
  # a column only when one says it)
  if (!nrow(out) || all(is.na(out$from))) out$from <- NULL
  out
}

# An assumption's `value` -- the value the AI used -- as text: a scalar as
# it is, a list or a vector (the arms, the variables) joined with ", "
.ai_assumption_value <- function(v) {
  if (is.null(v) || !length(v)) return(NA_character_)
  v <- unlist(v, use.names = FALSE)
  if (!length(v)) return(NA_character_)
  paste(as.character(v), collapse = ", ")
}

# A table answer's `reports`: the list of the reports its request makes,
# written first (id, title, how its TOC row is split for it, the TOC row),
# for the study's manifest (tfl_ai_collect_answers()); one row a report
.ai_report_list_rows <- function(x) {
  empty <- data.frame(output_id = character(), title = character(), split = character(),
                      toc = character(), stringsAsFactors = FALSE)
  if (is.null(x) || !length(x) || !is.list(x)) return(empty)
  cell <- function(e, k) {
    v <- e[[k]]
    if (is.null(v) || !length(v) || is.list(v)) NA_character_ else as.character(v[[1L]])
  }
  rows <- lapply(x, function(e) {
    if (!is.list(e)) e <- list(id = as.character(e))
    id <- cell(e, "id")
    data.frame(output_id = if (is.na(id)) id else .ai_file_id(id), title = cell(e, "title"),
               split = cell(e, "split"), toc = cell(e, "toc"), stringsAsFactors = FALSE)
  })
  do.call(rbind, c(list(empty), rows))
}

# ---- checking it -----------------------------------------------------------------------

.ai_check_table <- function(answer, context) {
  p <- .ai_problems()
  outs <- answer$outputs
  if (!length(outs)) {
    # (a sheet that is no shell: no report, said as an assumption)
    if (isTRUE(context$sheet_mode)) {
      return(.ai_problem(p, "outputs", "holds no report (the sheet is no shell, the answer says)",
                         "warning"))
    }
    return(.ai_problem(p, "outputs", "holds no report"))
  }
  # a sheet's request: its reports are the AI's to name; one that is no TOC
  # item's number is said
  if (isTRUE(context$sheet_mode) && length(context$toc_keys)) {
    # (a number the person made a report of this sheet: no TOC item's either)
    sn <- context$sheet_numbers
    made <- if (is.null(sn)) character() else .ai_id_key(sn$number[sn$make %in% TRUE])
    known <- c(context$toc_keys, made[!is.na(made)])
    for (id in names(outs)) {
      k <- .ai_id_key(id)
      base <- if (!is.na(k)) sub("[A-Z]$", "", k) else k
      if (is.na(k) || !(length(.ai_id_match(k, known)) ||
                        length(.ai_id_match(base, known)))) {
        p <- .ai_problem(p, paste("outputs", id),
                         "is no TOC item's number: check the number the sheet writes", "warning")
      }
    }
  }
  want <- context$outputs
  if (length(want)) {
    wk <- .ai_id_key(want)
    have_k <- .ai_id_key(names(outs))
    # a report answered in parts (t.15.1.5.1.a, .b): asked for, its parts
    part_of <- function(k) {
      !is.na(k) & grepl("[A-Z]$", k) &
        vapply(k, function(z) length(.ai_id_match(sub("[A-Z]$", "", z), wk)) > 0L, NA)
    }
    in_parts <- vapply(wk, function(w) any(part_of(have_k) &
      vapply(have_k, function(z) .ai_id_same(sub("[A-Z]$", "", z), w), NA)), NA)
    for (i in seq_along(want)) {
      if (in_parts[i]) next
      if (!is.na(wk[i]) && !length(.ai_id_match(wk[i], have_k)) && !want[i] %in% names(outs)) {
        p <- .ai_problem(p, "outputs", sprintf("has no report %s; the prompt asked for it", want[i]))
      }
    }
    for (id in names(outs)) {
      k <- .ai_id_key(id)
      if (!id %in% want && (is.na(k) || !length(.ai_id_match(k, wk))) && !part_of(k)) {
        p <- .ai_problem(p, paste("outputs", id),
                         "was not asked for: not written (its own request drafts it)", "warning")
      }
    }
  }
  vars <- context$datasets
  st <- NULL
  bad <- character()
  for (i in seq_along(outs)) {
    at <- sprintf("outputs[%d] %s", i, names(outs)[i])
    o <- outs[[i]]
    # a template: its name and arguments; then what it stands for (its
    # items, the report's own after them) checked as a code SPEC
    tp <- .ai_answer_template(o)
    if (!is.null(tp)) {
      tpp <- .ai_check_template(tp, at, kinds = c("table", "listing"))
      p <- rbind(p, tpp)
      if (any(tpp$severity == "error")) {
        bad <- c(bad, names(outs)[i])
        next
      }
      if (!is.null(o$ard)) {
        bad <- c(bad, names(outs)[i])
        p <- .ai_problem(p, paste(at, "ard"), paste(
          "a report has `template` or `ard`, not both: the template makes the analyses"))
        next
      }
      if (is.null(st) && !is.null(context$study_dir)) {
        st <- tryCatch(tfl_read_study_spec(context$study_dir), error = function(e) NULL)
      }
      o <- tryCatch(.ai_template_items(o, st), error = function(e) e)
      if (inherits(o, "error")) {
        bad <- c(bad, names(outs)[i])
        p <- .ai_problem(p, paste(at, "template"), paste("could not be expanded:",
                                                         .ai_one_line(conditionMessage(o))))
        next
      }
    }
    # (a listing, drafted with the tables: R/ai_listing.R)
    p <- rbind(p, if (identical(o[["kind"]], "listing")) .ai_check_listing_output(o, at, context, vars)
               else .ai_check_table_output(o, at, context, vars))
  }
  # (a report whose template is wrong: its run would say it again)
  rbind(p, .ai_check_table_run(outs[setdiff(names(outs), bad)], context))
}

.ai_check_table_output <- function(o, at, context, vars) {
  p <- .ai_problems()
  add <- function(where, what, severity = "error") {
    p <<- .ai_problem(p, paste(at, where), what, severity)
  }
  if (!identical(o[["kind"]], "table")) {
    add("kind", sprintf("is %s; this task writes tables and listings", o[["kind"]]))
  }
  for (e in o[["extends"]]) {
    ok <- e %in% c("defaults", "none") ||
      (startsWith(e, "patterns/") && (is.null(context$patterns) ||
                                        substring(e, 10L) %in% context$patterns))
    if (!ok) add("extends", sprintf("%s is not defaults or a pattern of the study", sQuote(e, FALSE)))
  }
  pops <- context$populations
  chk_pop <- function(where, x) {
    if (length(pops) && !is.null(x) && !x %in% pops) {
      add(where, sprintf("%s is not an analysis set of spec/data.yml (%s)", sQuote(x, FALSE),
                         paste(pops, collapse = ", ")))
    }
  }
  if (!is.na(o[["population"]])) chk_pop("population", o[["population"]])
  if (is.na(o[["label"]])) add("label", "the report has no label (its title)", "warning")
  # the ARD
  ad <- o[["ard"]]$analysis_data %||% list()
  an <- o[["ard"]]$analyses %||% list()
  fields <- .ai_table_ard_fields()
  known_ds <- if (!is.null(vars)) unique(vars$dataset)
  ad_ids <- character()
  derived <- list()
  # (what keeps a BDS dataset to its parameters, and who names an analysis
  # set: a report that says neither is no report -- the shell always does)
  par_rx <- "\\bPARAM(CD|N)?\\b"
  says_par <- function(x) any(grepl(par_rx, unlist(lapply(x, as.character))))
  ad_par <- list()
  has_pop <- !is.na(o[["population"]])
  bds <- if (!is.null(vars)) unique(vars$dataset[vars$variable %in% "PARAMCD"]) else character()
  unkept <- character()
  for (j in seq_along(ad)) {
    a <- ad[[j]]
    w <- sprintf("ard.analysis_data[%d]", j)
    if (!is.list(a) || is.null(names(a))) {
      add(w, "is not a mapping")
      next
    }
    for (k in setdiff(names(a), fields$analysis_data$field)) {
      add(paste(w, k), sprintf("is not a field of analysis_data; its fields are %s",
                               paste(fields$analysis_data$field, collapse = ", ")))
    }
    id <- .ai_scalar_text(a[["id"]])
    if (is.null(id)) add(w, "has no id") else ad_ids <- c(ad_ids, id)
    chk_pop(paste(w, "population"), .ai_scalar_text(a[["population"]]))
    has_pop <- has_pop || !is.null(a[["population"]]) || !is.null(a[["subjects"]])
    ad_par[[tolower(id %||% paste0("data", j))]] <- says_par(a[["where"]]) || says_par(a[["code"]]) ||
      isTRUE(ad_par[[tolower(.ai_scalar_text(a[["from"]]) %||% "")]])
    from <- toupper(.ai_scalar_text(a[["from"]]) %||% "")
    from_ds <- if (from %in% toupper(ad_ids)) attr(derived[[tolower(from)]], "dataset") else from
    if (!nzchar(from) && is.null(a[["code"]])) add(paste(w, "from"), "says no dataset")
    if (!is.null(known_ds) && nzchar(from) && !from %in% known_ds && !tolower(from) %in% tolower(ad_ids)) {
      add(paste(w, "from"), sprintf("%s is not among the datasets given (%s)", from,
                                    paste(known_ds, collapse = ", ")))
    }
    new <- .ai_derived_names(a[["derive"]])
    have <- c(new, unlist(a[["add"]]), if (from %in% toupper(ad_ids)) derived[[tolower(from)]])
    derived[[tolower(id %||% paste0("data", j))]] <- structure(have, dataset = from_ds)
    if (!is.null(vars) && nzchar(from_ds %||% "")) {
      p <- rbind(p, .ai_check_vars(.ai_r_names(a[["where"]]), vars, from_ds, have,
                                   paste(at, w, "where"), "warning"))
      p <- rbind(p, .ai_check_vars(unlist(a[["add"]]), vars, "ADSL", character(), paste(at, w, "add")))
    }
  }
  if (!length(an)) add("ard", "has no analyses: the table has nothing to show")
  methods <- tfl_ard_methods()$method
  for (j in seq_along(an)) {
    a <- an[[j]]
    w <- sprintf("ard.analyses[%d]", j)
    if (!is.list(a) || is.null(names(a))) {
      add(w, "is not a mapping")
      next
    }
    if (!is.null(a[["id"]])) w <- paste(w, a[["id"]])
    for (k in setdiff(names(a), fields$analyses$field)) {
      add(paste(w, k), sprintf("is not a field of analyses; its fields are %s",
                               paste(fields$analyses$field, collapse = ", ")))
    }
    if (is.null(a[["id"]])) add(w, "has no id")
    m <- .ai_scalar_text(a[["method"]])
    if (is.null(m)) {
      add(paste(w, "method"), "is missing")
    } else if (!m %in% methods && !grepl("^[A-Za-z][A-Za-z0-9.]*::[A-Za-z_.][A-Za-z0-9_.]*$", m)) {
      add(paste(w, "method"), sprintf("%s is not a method (%s) nor a pkg::function",
                                      sQuote(m, FALSE), paste(methods, collapse = ", ")))
    }
    if (!identical(m, "total_n") && !identical(m, "custom") && is.null(a[["parent"]]) &&
        !length(unlist(a[["variables"]]))) {
      add(paste(w, "variables"), "is missing: the variables analysed")
    }
    chk_pop(paste(w, "population"), .ai_scalar_text(a[["population"]]))
    has_pop <- has_pop || !is.null(a[["population"]])
    data_id <- .ai_scalar_text(a[["data"]])
    ds <- toupper(.ai_scalar_text(a[["dataset"]]) %||% "")
    have <- character()
    if (!is.null(data_id)) {
      if (!data_id %in% ad_ids) {
        add(paste(w, "data"), sprintf("%s is not an analysis_data id of this report%s",
                                      sQuote(data_id, FALSE),
                                      if (length(ad_ids)) sprintf(" (%s)", paste(ad_ids, collapse = ", ")) else ""))
      } else {
        have <- derived[[tolower(data_id)]]
        ds <- toupper(attr(have, "dataset") %||% "")
      }
    } else if (!nzchar(ds) && !is.null(a[["population"]])) {
      ds <- "ADSL"
    }
    if (!is.null(known_ds) && nzchar(ds) && !ds %in% known_ds) {
      add(paste(w, "dataset"), sprintf("%s is not among the datasets given (%s)", ds,
                                       paste(known_ds, collapse = ", ")))
    } else if (!is.null(vars) && nzchar(ds)) {
      for (k in c("variables", "by", "strata")) {
        p <- rbind(p, .ai_check_vars(unlist(a[[k]]), vars, ds, have, paste(at, w, k)))
      }
      p <- rbind(p, .ai_check_vars(.ai_r_names(a[["where"]]), vars, ds, have, paste(at, w, "where"),
                                   "warning"))
    }
    # a BDS dataset's analysis: kept to its parameters (where, its data's
    # where, its named records) or by them (PARAM in by, strata, variables)
    if (ds %in% bds && is.null(a[["parent"]]) && !identical(m, "custom") && is.null(a[["records"]]) &&
        !says_par(a[["where"]]) && !isTRUE(ad_par[[tolower(data_id %||% "")]]) &&
        !says_par(a[c("by", "strata", "variables")])) {
      unkept[w] <- ds
    }
  }
  if (!has_pop) {
    add("population", paste(
      "the report names no analysis set (population: the report's, an analysis's or its data's):",
      "write the one the shell says; if the shell does not say it, ask it in a question --",
      "do not guess"))
  }
  # code lists: variables of the data
  if (!is.null(vars)) {
    for (v in names(o[["codelists"]])) {
      if (!v %in% vars$variable && !v %in% unlist(derived)) {
        add(paste("codelists", v), "is not a variable of the datasets given", "warning")
      }
    }
  }
  # content
  if (inherits(o[["content"]], "tfl_r")) {
    add("content", "is R code; write the items (one function call each)", "warning")
    return(p)
  }
  fns <- vapply(o[["content"]], `[[`, "", "fn")
  # (a table with PARAM in its rows lists every parameter: kept by them)
  rows_par <- any(vapply(o[["content"]][fns %in% "table_plan"], function(it) says_par(it$args), NA))
  for (w in if (!rows_par) names(unkept)) {
    add(paste(w, "where"), sprintf(paste(
      "reads %s, a dataset of parameters (PARAMCD), but keeps no parameter: write the shell's",
      "(where: PARAMCD == \"...\"), or put PARAM in by / the rows for a table of them all;",
      "if the shell does not say which, ask it in a question -- do not guess"),
      unkept[[w]]))
  }
  known <- .ai_table_functions()
  if (!"table_plan" %in% fns) add("content", "has no table_plan item (the roles: cols and rows)")
  for (j in seq_along(o[["content"]])) {
    it <- o[["content"]][[j]]
    w <- sprintf("content[%d] %s", j, it$fn)
    if (!it$fn %in% known) {
      add(w, sprintf("%s is not a function of a table's content; they are %s", it$fn,
                     paste(known, collapse = ", ")))
      next
    }
    chk <- tfl_schema_check(it)
    for (r in seq_len(nrow(chk))) {
      # an argument the function does not have is the AI's invention: an error
      sev <- if (identical(chk$level[r], "error") || grepl("has no argument", chk$message[r], fixed = TRUE))
        "error" else "warning"
      add(paste(w, if (nzchar(chk$arg[r])) paste("field", chk$arg[r]) else ""), chk$message[r], sev)
    }
    for (a in .ai_required_args(it$fn)) {
      if (!a %in% names(it$args)) add(w, sprintf("%s needs `%s`", it$fn, a))
    }
  }
  # the column key: a table has one, one arm too (the arm by the analyses,
  # and as the columns)
  item_of <- function(fn) {
    hit <- Filter(function(it) identical(it$fn, fn), o[["content"]])
    if (length(hit)) hit[[1L]]
  }
  tp <- item_of("table_plan")
  if (!is.null(tp) && is.null(tp$args$cols)) {
    add("content table_plan", paste(
      "has no `cols` (the column key); a table has one, one arm too: give the",
      "analyses `by: [<the arm>]` and table_plan `cols: <the arm>`"))
  }
  # normalize_ard(overall = ): the overall row is made by a hierarchical
  # analysis with over_variables = TRUE; without it, it is not there
  nz <- item_of("normalize_ard")
  if (!is.null(nz) && !is.null(nz$args$overall)) {
    hier <- Filter(function(a) {
      is.list(a) && isTRUE(.ai_scalar_text(a[["method"]]) %in%
                             c("hierarchical", "cards::ard_stack_hierarchical"))
    }, an)
    over <- vapply(hier, function(a) {
      grepl("over_variables\\s*=\\s*TRUE", paste(unlist(a[["args"]]), collapse = " "))
    }, NA)
    if (length(hier) && !any(over)) {
      add("content normalize_ard overall", paste(
        "names an overall row, but no hierarchical analysis makes one: give it",
        "`args: over_variables = TRUE`"), "warning")
    }
  }
  if (!length(o[["page"]]$titles)) add("page", "has no titles", "warning")
  p$where <- trimws(p$where)
  p
}

# The arguments a content function cannot do without: its formals with no
# default (the plan itself aside).
.ai_required_args <- function(fn) {
  f <- .schema_formals(fn, tfl_schema(fn))
  if (is.null(f)) return(character())
  miss <- vapply(f, function(v) is.symbol(v) && identical(as.character(v), ""), NA)
  setdiff(names(f)[miss], c("...", "plan", ".plan", "data", "x", ".data", "ard"))
}

# The names a derive makes (`RESP = AVALC %in% c("CR", "PR")`, several
# with ` | ` between them, or a list).
.ai_derived_names <- function(x) {
  if (is.null(x)) return(character())
  x <- unlist(lapply(x, as.character))
  x <- unlist(strsplit(x, "|", fixed = TRUE))
  nm <- sub("^\\s*([A-Za-z.][A-Za-z0-9._]*)\\s*=.*$", "\\1", x)
  trimws(nm[grepl("^\\s*[A-Za-z.][A-Za-z0-9._]*\\s*=[^=]", x)])
}

# The variable-like names of an R condition (functions and constants left
# out).
.ai_r_names <- function(x) {
  if (is.null(x)) return(character())
  code <- paste(as.character(unlist(x)), collapse = "\n")
  e <- tryCatch(parse(text = code, keep.source = FALSE), error = function(e) NULL)
  if (is.null(e)) return(character())
  v <- all.vars(e)
  v[grepl("^[A-Z][A-Z0-9_]*$", v)]
}

# Variables that are not in the dataset (nor made or added on the way).
.ai_check_vars <- function(v, vars, ds, have, where, severity = "error") {
  p <- .ai_problems()
  v <- unique(as.character(v))
  v <- v[!is.na(v) & nzchar(v)]
  inds <- vars$variable[vars$dataset == toupper(ds)]
  if (!length(inds)) return(p)
  for (x in setdiff(v, c(inds, have))) {
    else_where <- unique(vars$dataset[vars$variable == x])
    p <- .ai_problem(p, where, sprintf(
      "%s is not a variable of %s%s", x, toupper(ds),
      if (length(else_where)) sprintf(" (it is in %s: add it from ADSL with analysis_data `add`, or read that dataset)",
                                      paste(else_where, collapse = ", ")) else
        " nor of any dataset given"),
      severity)
  }
  p
}

# The answer in a study folder: its ARD program and each table's code
# written as tflspec writes them (nothing run), each error a problem.
.ai_check_table_run <- function(outs, context) {
  p <- .ai_problems()
  d <- tempfile("ai-table-")
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  dir.create(file.path(d, "spec", "outputs"), recursive = TRUE)
  src <- context$study_dir
  if (!is.null(src)) {
    for (f in c("study.yml", "spec/data.yml", "spec/defaults.yml")) {
      g <- file.path(src, f)
      if (file.exists(g)) file.copy(g, file.path(d, f))
    }
    pat <- file.path(.spec_dir(src), "patterns")
    if (dir.exists(pat)) file.copy(pat, file.path(d, "spec"), recursive = TRUE)
  }
  for (id in names(outs)) {
    tfl_write_output(outs[[id]], file.path(d, "spec", "outputs", paste0(id, ".yml")))
  }
  st <- tryCatch(tfl_read_study_spec(d), error = function(e) e)
  if (inherits(st, "error")) {
    return(.ai_problem(p, "study", paste("could not be read:", .ai_one_line(conditionMessage(st)))))
  }
  for (i in seq_along(outs)) {
    id <- names(outs)[i]
    at <- sprintf("outputs[%d] %s", i, id)
    o <- tryCatch(tfl_resolve_output(outs[[id]], st), error = function(e) e)
    if (inherits(o, "error")) {
      p <- .ai_problem(p, paste(at, "extends"), .ai_one_line(conditionMessage(o)))
      next
    }
    if (length(outs[[id]]$ard$analyses) && !is.null(src)) {
      r <- tryCatch(suppressMessages(suppressWarnings(tfl_ard_code(st, output_id = id, save = FALSE))),
                    error = function(e) e)
      if (inherits(r, "error")) {
        p <- .ai_problem(p, paste(at, "ard"), paste("the ARD program could not be written:",
                                                    .ai_one_line(conditionMessage(r))))
      }
    }
    if (!inherits(o$content, "tfl_r")) {
      r <- tryCatch(suppressMessages(suppressWarnings(tfl_content_code(o))), error = function(e) e)
      if (inherits(r, "error")) {
        p <- .ai_problem(p, paste(at, "content"), paste("the table's code could not be written:",
                                                        .ai_one_line(conditionMessage(r))))
      }
    }
  }
  p
}

# The schema parts a table answer's problems are in: a content function
# named in `where`, `ard`, or the report file's keys.
.ai_problem_parts_table <- function(where) {
  fns <- .ai_table_functions()
  hit <- regmatches(where, regexpr("content\\[[0-9]+\\] [A-Za-z_.]+", where))
  hit <- unique(sub("^content\\[[0-9]+\\] ", "", hit))
  c(if (any(grepl("ard\\b|analys", where))) "ard",
    if (any(grepl("^outputs|kind|extends|page|label|population", where))) "report",
    if (any(grepl("listing|\\bdata\\b", where))) "listing",
    intersect(fns, hit))
}

# ---- writing it --------------------------------------------------------------------

# A table answer's reports as spec/outputs/<id>.yml; its assumptions into
# spec/_assumptions.yml (those of these reports replaced, the others kept).
.ai_unpack_table <- function(answer, dir, overwrite) {
  errs <- answer$problems[answer$problems$severity == "error", , drop = FALSE]
  if (!length(answer$outputs)) {
    .ard_stop(sprintf("tfl_ai_unpack(): the answer holds no report it could read: %s",
                      paste(errs$problem, collapse = "; ")))
  }
  bad <- grepl("^(block|answer)$", errs$where)
  if (any(bad)) {
    .ard_stop(sprintf("tfl_ai_unpack(): %s", paste(errs$problem[bad], collapse = "; ")))
  }
  ids <- names(answer$outputs)
  paths <- file.path(.outputs_dir(dir), paste0(ids, ".yml"))
  there <- paths[file.exists(paths)]
  if (length(there) && !isTRUE(overwrite)) {
    .ard_stop(sprintf("tfl_ai_unpack(): %s already there; pass `overwrite = TRUE`.",
                      paste(sQuote(file.path("spec", "outputs", basename(there)), FALSE),
                            collapse = ", ")))
  }
  # (the reports whose widths R sets: an assumption says so)
  unset <- .ai_widths_unset(answer$outputs)
  # (a listing's columns their widths, where the answer gave none: R/ai_widths.R)
  answer$outputs <- lapply(answer$outputs, .ai_listing_widths, vars = answer$vars)
  # (a table's widths R gave it on an earlier run)
  known <- .ai_widths_read(dir)
  answer$outputs <- lapply(answer$outputs, .ai_widths_again, known = known)
  for (i in seq_along(ids)) tfl_write_output(answer$outputs[[i]], paths[i])
  # a table with no widths: built here when its data can be read, its
  # widths from its printed columns (else run.R gives them when it builds)
  .ai_widths_on_read(answer$outputs, paths, dir)
  out <- paths
  a <- answer$assumption_rows
  aw <- .ai_widths_assumptions(unset)
  if (!is.null(aw)) {
    if (is.null(a) || !nrow(a)) {
      a <- aw
    } else {
      for (k in setdiff(names(a), names(aw))) aw[[k]] <- NA_character_
      a <- rbind(a, aw[names(a)])
    }
  }
  if (!is.null(a) && nrow(a)) {
    old <- .ai_folder_assumptions(dir, all = TRUE)
    names(old)[names(old) == "sheet"] <- "where"
    mine <- !is.na(old$output_id) & old$output_id %in% ids
    # (an assumption the agent fixed from the person's answer: kept as fixed)
    rows <- rbind(old[!mine, , drop = FALSE], .ai_asm_keep_fixed(old[mine, , drop = FALSE], a))
    out <- c(out, .ai_asm_write(dir, rows))
  }
  invisible(normalizePath(out, winslash = "/"))
}
