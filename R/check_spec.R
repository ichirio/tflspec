#' Check a study's SPEC against the packages installed: one call
#'
#' Everything tflspec can say about a study's reports before their programs
#' run, in one table, each row at one of three levels -- the person's
#' definition of 2026-10-29:
#'
#' * `error`: the file cannot be read as a SPEC (its YAML, or a key or a
#'   value the schema does not have, an analysis the ARD definition
#'   refuses): the report cannot be copied into the study;
#' * `warning`: the SPEC reads, but a program that runs cannot be written
#'   from it (the ARD program or the table's code stops; a content
#'   function the installed rtfreporter does not have yet; with `adam`, a
#'   `!=` condition that drops the rows where its variable is NA; a
#'   table whose header N and counts give a subject different groups; a
#'   report's own code list, for a column the study's `adam_meta` maps,
#'   with a code the define's code list does not have, `part`
#'   `"codelists"` -- codes compared, not labels);
#' * `note`: the program is written all the same -- an argument the
#'   installed cards / cardx does not know, a version to look at.
#'
#' A report with no `error` and no `warning` row is OK: its code is
#' written (whether the report is right is another matter).  What the AI
#' marked to confirm (`ai: confirm`, [tfl_ai_confirm()]) is a row of level
#' `check`, apart from the three: it does not keep a report from being
#' adopted.  [tfl_check_reports()] gives the three levels a report at a
#' time, with its confirms; [tfl_check_fix_text()] writes, for a report at
#' `error` or `warning`, the text to paste into the agent's chat.
#'
#' Each report is read and checked on its own: a file that cannot be read
#' is an `error` of that report (by its file's name) and stops no other.
#' The checks: each report's ARD definition against the installed cards /
#' cardx ([tfl_ard_spec()], written as its program, [tfl_ard_code()]), its
#' content items against the schema and the installed rtfreporter
#' ([tfl_schema_check()], the items of an appended part too; its code
#' written, [tfl_content_code()]), and a figure's design against the
#' installed ggplot2 and the version it is written for
#' ([tfl_check_fig_design()]), and the names its fields give
#' ([tfl_output_objects()]): a field that names an object made below it,
#' a figure's field that names an object nothing above its content makes
#' (`wrap_plots`'s `plots`, `ggsurvfit`'s `x`, a `data` or a `fit`), and a
#' name given twice in a report or that is a study dataset's or a
#' function's are errors (`part` `"names"` for the last two); an object
#' the report names and nothing uses is a note.  The packages' installed versions come with
#' it, as an attribute.  A GUI's check button and the AI's run call it.
#'
#' A table's column header N: a header text with `{n}` (and no `values` of
#' its own) takes each column's N from the report's `BIGN` analysis.  A
#' report with no `BIGN`, or whose `BIGN` counts by no column variable of
#' the table, is a `warning` (`part` `"content"`, item
#' `"plan_col_header(header)"`): its header would print `N=NA`.
#'
#' Every `!r` value of the SPEC is read as R once, by one walker; one that
#' does not read is an `error`, with the parser's message.  It is read, not
#' run, by the kind of slot it fills: an item's argument by the schema's
#' kind (a guard takes `condition ~ text`), the fields of R of the ARD and
#' of data.yml by their own (`where` one condition, `args` a call's
#' arguments, `derive` pieces `name = expression` and `post` steps between
#' `|`, `code` and `then` R code); those fields are R whether or not they
#' are written `!r`.  A report's value: `part` the report's key it is under
#' (`content`, `template`, `ard` ...), `item` its place
#' (`"content$plan_filter$args$where"`, `"ard$analyses$HR$code"`).  The
#' study's own (study.yml `setup`, defaults.yml, data.yml's populations,
#' records and datasets), on a check of every report only: `output_id` NA,
#' `part` `"study"`, `item` the file and the place
#' (`"data.yml: populations$SAF$where"`).  The ARD check's row and the
#' `program` warning for the same value are not given twice.  A report's
#' own program (a file, not a SPEC value) is read by [tfl_check_code()]
#' (rule U01) on the review path.
#'
#' @param study A study read with [tfl_read_study_spec()], or its folder.
#' @param output_id The reports to check; `NULL` for every report.
#' @param build `TRUE`: each report the checks above pass is also built
#'   ([tfl_check_build()]) -- its data read, its ARD made, its table,
#'   listing or figure object made, no RTF -- so that a report with no
#'   row can be made; one that cannot has an `error` row, `part`
#'   `"build"`.  A few seconds a report.  `FALSE` (default): the checks
#'   above only.
#' @param adam Where the data for the data check come from.  `TRUE`
#'   (default): the study's own datasets, from the paths of `spec/data.yml`
#'   (from the study folder), read as the study's program reads them and
#'   only when needed: a study with no `!=` condition reads no data, and a
#'   dataset is read once.  `FALSE` (or `NULL`): no data check.  Or the ADaM
#'   data as [tfl_check_fig_design()]'s `adam` takes it ([tfl_read_adam()]: a
#'   folder or a named list of data frames).  The check: a condition of the
#'   ARD definition (a `where` of the populations, records, analysis data or
#'   analyses) that holds `var != value` is looked at in the dataset it
#'   applies to: where `var` has NA there, the condition drops those rows (a
#'   filter keeps only TRUE), and a `warning` row (`part` `"ard"`) gives the
#'   count and the form that keeps them, `!var %in% value`.  No study
#'   folder, a dataset whose file is not there or is not listed, or a
#'   variable not in the dataset: no row.  The generated code is not changed.
#'   The same data check a table's header N (its `BIGN` analysis, from
#'   whatever data or population it names) against its counts: an analysis
#'   that groups by BIGN's variable and reads another dataset, whose data
#'   does not take the variable from the subjects (`add`), gives a `warning`
#'   row when the two data carry the variable and its value differs for a
#'   subject (the subject counted under one group in the N, another in the
#'   rows).  Only a table whose header shows the N (`{n}` in a column
#'   header that gives no values of its own) is looked at, and only the
#'   datasets of such tables are read.
#' @return A data frame, one row a problem: `output_id` (`NA` for the
#'   study's ARD definition as a whole and the study's own rows), `part`
#'   (`"file"`, `"study"`, `"ard"`, `"content"`, `"program"`, `"figure"`,
#'   `"codelists"`, `"build"`), `item` (the file, the
#'   analysis, the function and argument, the figure's part and
#'   field, or the code list's column), `level` (`"error"`, `"warning"`, `"note"`, `"check"`) and
#'   `message`.  Its attributes: `versions`, a data frame of the key
#'   packages (`package`, `installed`, `NA` when not installed); `ids`, the
#'   reports checked (the OK ones too); `kinds`, each report's kind, named
#'   by id; `outputs_dir`, the folder the report files are in, from the
#'   study folder.
#' @examples
#' if (requireNamespace("haven", quietly = TRUE)) {
#'   # a study of the bundled ADSL, with one report
#'   d <- file.path(tempfile(), "study")
#'   dir.create(file.path(d, "adam"), recursive = TRUE)
#'   chr <- function(x) if (is.factor(x)) as.character(x) else x
#'   haven::write_xpt(as.data.frame(lapply(adsl, chr)), file.path(d, "adam", "adsl.xpt"))
#'   tfl_ai_study_init(d, adam = "adam", study_id = "EX-1")
#'   tfl_write_output(
#'     tfl_output("T-DM", "table", common = list(population = "SAF"),
#'                template = list(name = "demog", args = list(population = "SAF"))),
#'     file.path(d, "spec", "outputs", "T-DM.yml"))
#'   p <- tfl_check_spec(d)
#'   p
#'   tfl_check_reports(p)
#'   attr(p, "versions")
#' }
#' @seealso [tfl_check_reports()], [tfl_check_fix_text()], [tfl_ard_spec()],
#'   [tfl_schema_check()], [tfl_check_fig_design()]
#' @export
tfl_check_spec <- function(study, output_id = NULL, build = FALSE, adam = TRUE) {
  if (is.character(study) && length(study) == 1L) study <- tfl_read_study_spec(study)
  if (!inherits(study, "tfl_study_spec")) {
    .ard_stop("tfl_check_spec(): `study` is a study read with tfl_read_study_spec(), or its folder.")
  }
  rows <- list(.check_rows())
  add <- function(...) rows[[length(rows) + 1L]] <<- .check_rows(...)
  one <- function(x) .ai_one_line(conditionMessage(x))

  # the report files, each on its own: one that cannot be read is an error
  # of that report (its file's name), and stops no other
  outs <- list()
  bad <- character()
  out_dir <- if (!is.null(study$dir)) .outputs_dir(study$dir)
  if (!is.null(out_dir) && dir.exists(out_dir)) {
    for (f in list.files(out_dir, "\\.ya?ml$", recursive = TRUE, full.names = TRUE)) {
      r <- tryCatch(tfl_read_output(f), error = function(e) e)
      if (inherits(r, "error")) {
        id <- sub("\\.ya?ml$", "", basename(f))
        bad <- c(bad, id)
        add(output_id = id, part = "file", item = basename(f), level = "error",
            message = paste("the YAML cannot be read as a report file:", one(r)))
        next
      }
      if (inherits(r, "tfl_output")) r <- list(r)
      for (o in r) outs[[o$id]] <- o
    }
  }
  ids <- output_id %||% c(names(outs), bad)
  miss <- setdiff(ids, c(names(outs), bad))
  if (length(miss)) {
    .ard_stop(sprintf("tfl_check_spec(): no report %s in the study.",
                      paste(sQuote(miss), collapse = ", ")))
  }
  kinds <- vapply(outs, function(o) o$kind %||% "table", "")
  # (the study's datasets: no object of a report takes one's name)
  study_ds <- .obj_study_datasets(study)
  # (the reports whose header shows BIGN's N, .bign_shown())
  n_shown <- character()

  # what the user wrote and a study's R fields hold: a test for a blank text,
  # where study$adam has NA (a template's own rows are not the user's: left out)
  blank_note <- function(output_id, msg) {
    at <- regmatches(msg, regexec("^(.*?) / ([^:]+): (.*)$", msg, perl = TRUE))[[1L]]
    add(output_id = output_id, part = "ard", item = paste(at[2L], "/", at[3L]), level = "note",
        message = at[4L])
  }
  # the study's own `!r` values that do not read (study.yml, defaults.yml,
  # data.yml), on a full check only (R/check_spec_r_parse.R)
  wks <- .check_rows()
  if (is.null(output_id)) {
    rows[[length(rows) + 1L]] <- wks <- .r_walk_study(study)
    dd <- study$data %||% list()
    for (m in c(.blank_entry_notes(dd$populations, c("where", "derive")),
                .blank_entry_notes(dd$records, "where"),
                .blank_entry_notes(dd$datasets, "derive", function(e) as.character(e$dataset %||% NA)[1L]))) {
      blank_note(NA_character_, m)
    }
    # a study code list named like an ADaM column adam_meta does not map:
    # study$adam has no levels of it (the name is not a mapping)
    for (w in .study_list_unmapped(study)) {
      add(output_id = NA_character_, part = "study", item = paste0("codelists: ", w$list),
          level = "warning", message = w$message)
    }
  }
  # a report's own code list with a code the define's list of its column
  # does not have (the column adam_meta maps; codes, not labels)
  rows[[length(rows) + 1L]] <- .check_define_code_rows(study, outs[setdiff(ids, bad)])
  for (id in setdiff(ids, bad)) {
    o <- outs[[id]]
    for (m in c(.blank_entry_notes(o$ard$analysis_data, c("where", "derive", "code", "then")),
                .blank_entry_notes(o$ard$analyses, c("where", "post", "code")))) {
      blank_note(id, m)
    }
    rows[[length(rows) + 1L]] <- wk <- .check_r_parse_rows(o, id)  # an `!r` value that is not R
    # its ARD definition, written as its program: the definition's problems
    # are errors; what the installed packages do not know, notes; a program
    # that cannot be written otherwise, a warning
    if (!is.null(study$dir) && length(o$ard$analyses)) {
      withCallingHandlers(
        tryCatch(tfl_ard_code(study, output_id = id, save = FALSE), error = function(e) {
          p <- e$problems
          if (is.data.frame(p) && nrow(p)) {
            # (a field of R that does not read: the walker's row says it)
            p <- .r_ard_problems_left(p, rbind(wk, wks))
            if (nrow(p)) {
              add(output_id = ifelse(is.na(p$output_id), id, p$output_id), part = "ard",
                  item = ifelse(nzchar(p$row %||% ""), paste0(p$row, "$", p$field), p$field),
                  level = "error", message = p$message)
            }
          } else if (!any(wk$level == "error" & wk$part == "ard")) {
            # (not when an `ard:` value that does not read is the cause: the
            # walker's row says it)
            add(output_id = id, part = "program", item = "ard", level = "warning",
                message = paste("the ARD program cannot be written:", one(e)))
          }
          NULL
        }),
        warning = function(w) {
          for (m in strsplit(conditionMessage(w), "\n  ", fixed = TRUE)[[1L]]) {
            at <- regmatches(m, regexec("^([^ ]+) / ([^:]+): ", m))[[1L]]
            add(output_id = if (length(at)) at[2L] else id, part = "ard",
                item = if (length(at)) at[3L] else "", level = "note",
                message = if (length(at)) substring(m, nchar(at[1L]) + 1L) else m)
          }
          invokeRestart("muffleWarning")
        })
    }
    # (a `plot:` key that does nothing in the template: a warning of the report)
    ro <- tryCatch(withCallingHandlers(tfl_resolve_output(o, study),
      tfl_plot_key_unused = function(w) {
        add(output_id = id, part = "plot", item = w$key, level = "warning", message = w$text)
        invokeRestart("muffleWarning")
      }), error = function(e) e)
    if (inherits(ro, "error")) {
      add(output_id = id, part = "content", item = "", level = "error", message = one(ro))
      next
    }
    # (a header that shows BIGN's N: the N check looks at that N only)
    if (.bign_shown(ro)) n_shown <- c(n_shown, id)
    # its own titles under a study header that prints the label, the title
    # and the population from the report's keys: printed twice (#518)
    if (length(.page_args(o$page$titles))) {
      cfg <- tryCatch(.report_style(o, study)$set, error = function(e) NULL)
      tok <- .page_output_tokens_used(.page_config_flat(cfg))
      if (length(tok)) {
        add(output_id = id, part = "page", item = "titles", level = "warning",
            message = sprintf(paste0("the study's header prints the report's label, title and population ",
                                     "already (%s): `page: titles` prints them twice. Remove `page: titles`; ",
                                     "`label:` and `common: population` fill the header."),
                              paste0("{", tok, "}", collapse = ", ")))
      }
    }
    # what the AI marked to confirm (`ai: confirm`): to look at
    cf <- .ai_confirm_rows(ro)
    if (nrow(cf)) {
      add(output_id = id, part = "content", item = ifelse(is.na(cf$item), cf$where, paste0(cf$where, "$", cf$item)),
          level = "check", message = paste0("to confirm: ", cf$why))
    }
    # the report as written: what stops the build far from its cause
    rows[[length(rows) + 1L]] <- .check_static_rows(o, id, study)
    # its analysis set against the study: data.yml's sets, the TOC's line
    rows[[length(rows) + 1L]] <- .check_set_rows(o, id, study)
    # the names its fields give: a figure's that nothing above makes, a
    # table's named block (R/object_list.R)
    rows[[length(rows) + 1L]] <- .check_object_rows(ro, id, study_ds)
    # a column header's N with no BIGN to take it from (R/check_spec_header_n.R)
    rows[[length(rows) + 1L]] <- .check_header_n_rows(ro, id)
    if (inherits(ro$content, "tfl_r")) next
    # a figure's content key (`figure: {name, args}`): its name is one of the
    # table's (.fig_content_table()), its program is written
    if (identical(ro$kind, "figure") && !is.null(ro$figure)) {
      r <- tryCatch(suppressMessages(suppressWarnings(tfl_content_code(ro))), error = function(e) e)
      if (inherits(r, "error")) {
        add(output_id = id, part = "figure", item = "figure", level = "error", message = one(r))
      }
      next
    }
    if (identical(ro$kind, "figure") && !.fig_new_content(ro$content)) {
      bad_design <- function(e) {
        data.frame(part = "", field = "", problem = conditionMessage(e), stringsAsFactors = FALSE)
      }
      p <- tryCatch(tfl_check_fig_design(.fig_design_of_output(ro)), error = bad_design)
      # the design tflplanner opens (the sheets' one: a template expanded,
      # its plot settings joined) is checked as it checks it, so a draft it
      # would refuse is an error here
      pd <- tryCatch(suppressWarnings(
        tfl_sheets_from_spec(study, stats::setNames(list(o), id), templates = "expand")$figures[[id]]),
        error = function(e) NULL)
      if (is.list(pd) && length(Filter(length, pd))) {
        p <- unique(rbind(p, tryCatch(tfl_check_fig_design(structure(pd, class = "tfl_fig_design")),
                                      error = bad_design)))
      }
      if (nrow(p)) {
        add(output_id = id, part = "figure", item = paste0(p$part, "$", p$field),
            level = "error", message = p$problem)
      }
      next
    }
    form_ok <- TRUE
    # (the cells plan_stat_groups() names -- a column's heading, "n (%)" --
    # are plan_cells() entries of their own, no row's label)
    groups <- as.character(unlist(lapply(
      Filter(function(it) identical(it$fn, "plan_stat_groups"), ro$content),
      function(it) names(it$args))))
    groups <- groups[nzchar(groups) & !startsWith(groups, ".")]
    for (it in ro$content) {
      p <- tfl_schema_check(it)
      if (nrow(p) && identical(it$fn, "plan_cells") && length(groups)) {
        p <- p[!(p$arg %in% groups & grepl("is a row's label", p$message, fixed = TRUE)), , drop = FALSE]
      }
      if (nrow(p)) {
        # (an argument the function does not have stops the program: an
        # error; the schema's other warnings -- a renamed argument -- are
        # notes, the code is written)
        lv <- ifelse(p$level == "error" | grepl("has no argument", p$message, fixed = TRUE),
                     "error", "note")
        add(output_id = id, part = "content",
            item = ifelse(nzchar(p$arg), paste0(p$fn, "(", p$arg, ")"), p$fn),
            level = lv, message = p$message)
        if (any(lv == "error")) form_ok <- FALSE
      }
      # (a function of the schema the installed rtfreporter does not have
      # yet: the program would stop)
      fn <- it$fn %||% ""
      if (!grepl("::", fn, fixed = TRUE) && fn %in% names(tfl_schema()) &&
          !nrow(p[p$level == "error", , drop = FALSE])) {
        m <- .pkg_fn_missing(paste0("rtfreporter::", fn))
        if (!is.null(m)) add(output_id = id, part = "program", item = fn, level = "warning",
                             message = paste0(m, ": update rtfreporter"))
      }
    }
    # the report's code, written: a stop is a warning (the form is right,
    # the code is not); not when an `!r` that does not read is already
    # an error (the walker's row says it, R/check_spec_r_parse.R)
    if (form_ok && !any(wk$level == "error")) {
      r <- tryCatch(suppressMessages(suppressWarnings(tfl_content_code(ro))), error = function(e) e)
      if (inherits(r, "error")) {
        add(output_id = id, part = "program", item = "content", level = "warning",
            message = paste("the report's code cannot be written:", one(r)))
      }
    }
  }
  # (with the study's data: a `!=` that drops the rows where the variable is NA)
  rows[[length(rows) + 1L]] <- .check_ne_na_rows(study, adam, setdiff(ids, bad), is.null(output_id))
  # (and a table's header N and its counts: a column variable of two data that
  # differs for a subject, R/check_spec_n.R)
  rows[[length(rows) + 1L]] <- .check_n_rows(study, adam, intersect(setdiff(ids, bad), n_shown))
  out <- do.call(rbind, rows)
  out <- out[is.na(out$output_id) | out$output_id %in% ids, , drop = FALSE]
  if (isTRUE(build) && !is.null(study$dir)) {
    # the reports the checks pass: built, so that no row means it can be made
    clean <- setdiff(ids, c(bad, out$output_id[out$level == "error"]))
    if (length(clean)) out <- rbind(out, .check_build_rows(outs[clean], study))
  }
  rownames(out) <- NULL
  attr(out, "versions") <- .check_versions()
  attr(out, "ids") <- ids
  attr(out, "kinds") <- kinds[intersect(ids, names(kinds))]
  attr(out, "outputs_dir") <- if (!is.null(study$dir) && !is.null(out_dir)) {
    .check_rel_path(out_dir, study$dir)
  } else NA_character_
  out
}

# A path from the study folder (as written, forward slashes), when under it
.check_rel_path <- function(path, dir) {
  p <- normalizePath(path, winslash = "/", mustWork = FALSE)
  d <- paste0(normalizePath(dir, winslash = "/", mustWork = FALSE), "/")
  if (startsWith(p, d)) substring(p, nchar(d) + 1L) else p
}

#' Build each report, to know that it can be made
#'
#' The step after [tfl_check_spec()]'s reading: each report's data read,
#' its ARD made, its table, listing or figure object made -- all a
#' program does but writing the RTF -- in the study folder.  A report that
#' cannot be made gives an `error` row, `part` `"build"`, with the message
#' the build stopped on; one that can gives no row.  The person's bar for
#' OK (2026-10-30): the report's object can be made.  A few seconds a
#' report; the AI's drafts apart (`options(tflspec.outputs_dir = )`) are
#' read as [tfl_check_spec()] reads them.
#'
#' @inheritParams tfl_check_spec
#' @return A data frame as [tfl_check_spec()]'s (`output_id`, `part`,
#'   `item`, `level`, `message`): the reports that could not be built.
#'   Attributes `ids` (the reports built or tried) and `kinds`.
#' @seealso [tfl_check_spec()] (`build = TRUE` does both), [tfl_run_output()]
#' @export
tfl_check_build <- function(study, output_id = NULL) {
  if (is.character(study) && length(study) == 1L) study <- tfl_read_study_spec(study)
  if (!inherits(study, "tfl_study_spec") || is.null(study$dir)) {
    .ard_stop("tfl_check_build(): `study` is a study read with tfl_read_study_spec(), or its folder.")
  }
  outs <- tryCatch(suppressWarnings(tfl_read_outputs(study$dir, skip_bad = TRUE)),
                   error = function(e) list())
  ids <- output_id %||% names(outs)
  miss <- setdiff(ids, names(outs))
  if (length(miss)) {
    .ard_stop(sprintf("tfl_check_build(): no report %s in the study.",
                      paste(sQuote(miss), collapse = ", ")))
  }
  out <- .check_build_rows(outs[ids], study)
  rownames(out) <- NULL
  attr(out, "ids") <- ids
  attr(out, "kinds") <- vapply(outs[ids], function(o) o$kind %||% "table", "")
  out
}

# One error row a report whose content cannot be made (in the study
# folder, with the study's rounding, as tfl_run_output() makes it)
.check_build_rows <- function(outs, study) {
  old <- setwd(study$dir)
  on.exit(setwd(old), add = TRUE)
  old_r <- .study_rounding_option(study)
  on.exit(options(old_r), add = TRUE)
  rows <- list(.check_rows())
  for (id in names(outs)) {
    e <- tryCatch({
      .output_build_content(outs[[id]], study)
      NULL
    }, error = function(e) e)
    if (!is.null(e)) {
      rows[[length(rows) + 1L]] <- .check_rows(
        output_id = id, part = "build", item = "", level = "error",
        message = paste("the report cannot be made:", .ai_one_line(conditionMessage(e))))
    }
  }
  do.call(rbind, rows)
}

#' The check's three levels, a report at a time
#'
#' [tfl_check_spec()]'s rows summed up by report: its level -- `ERROR`
#' (it cannot be copied into the study), `WARNING` (its program cannot be
#' written), `OK` (its code is written) -- with the counts behind it, and
#' apart from the level, what the AI marked to confirm ([tfl_ai_confirm()]).
#' A problem of the study's ARD definition as a whole (no report) counts
#' for every report.
#'
#' @param problems [tfl_check_spec()]'s result.
#' @return A data frame, one row a report: `output_id`, `level`
#'   (`"ERROR"`, `"WARNING"`, `"OK"`), `errors`, `warnings`, `notes`
#'   (the counts), `confirms` (how many marked to confirm) and `confirm`
#'   (their texts, `; ` between them; `NA` for none).
#' @seealso [tfl_check_spec()], [tfl_check_fix_text()]
#' @export
tfl_check_reports <- function(problems) {
  ids <- attr(problems, "ids") %||% unique(stats::na.omit(problems$output_id))
  if (!length(ids)) {
    return(data.frame(output_id = character(), level = character(), errors = integer(),
                      warnings = integer(), notes = integer(), confirms = integer(),
                      confirm = character(), stringsAsFactors = FALSE))
  }
  n <- function(id, lv) sum(problems$level == lv & (is.na(problems$output_id) | problems$output_id == id))
  errors <- vapply(ids, n, 1L, lv = "error")
  warnings <- vapply(ids, n, 1L, lv = "warning")
  notes <- vapply(ids, n, 1L, lv = "note")
  confirm <- vapply(ids, function(id) {
    m <- problems$message[problems$level == "check" & problems$output_id %in% id]
    if (length(m)) paste(sub("^to confirm: ", "", m), collapse = "; ") else NA_character_
  }, "")
  data.frame(output_id = ids,
             level = ifelse(errors > 0L, "ERROR", ifelse(warnings > 0L, "WARNING", "OK")),
             errors = unname(errors), warnings = unname(warnings), notes = unname(notes),
             confirms = vapply(ids, n, 1L, lv = "check"), confirm = unname(confirm),
             stringsAsFactors = FALSE, row.names = NULL)
}

#' The text that asks the agent to fix a draft
#'
#' For each report [tfl_check_spec()] put at `ERROR` or `WARNING`, the
#' text to paste into the agent's chat: which file, which report, what is
#' wrong where (each `error` and `warning` row), and what not to change --
#' the common rules are in `ai/AGENTS.md` and the instructions, which the
#' text names rather than repeats.  In English, or in Japanese
#' (`language = "ja"`); tflspec's own messages stay as they are.
#'
#' @param problems [tfl_check_spec()]'s result.
#' @param language `"en"` (default) or `"ja"` (`"Japanese"` too).
#' @param output_id The reports to write the text for; `NULL` (default):
#'   every report at `ERROR` or `WARNING`.
#' @return A character vector named by report id, one text a report (its
#'   lines joined with newlines); empty when no report needs a fix.
#' @seealso [tfl_check_spec()], [tfl_check_reports()]
#' @export
tfl_check_fix_text <- function(problems, language = "en", output_id = NULL) {
  ja <- tolower(as.character(language)[1L]) %in% c("ja", "japanese", "\u65e5\u672c\u8a9e")
  rep <- tfl_check_reports(problems)
  rep <- rep[rep$level %in% c("ERROR", "WARNING"), , drop = FALSE]
  if (!is.null(output_id)) rep <- rep[rep$output_id %in% output_id, , drop = FALSE]
  if (!nrow(rep)) return(stats::setNames(character(), character()))
  od <- attr(problems, "outputs_dir") %||% NA_character_
  if (is.na(od)) od <- file.path("spec", "outputs")
  kinds <- attr(problems, "kinds") %||% character()
  out <- character()
  for (i in seq_len(nrow(rep))) {
    id <- rep$output_id[i]
    file <- paste0(od, "/", id, ".yml")
    kind <- if (id %in% names(kinds)) kinds[[id]] else "table"
    ins <- if (identical(kind, "figure")) "ai/instructions-figure.md" else "ai/instructions-table.md"
    p <- problems[problems$level %in% c("error", "warning") &
                    (is.na(problems$output_id) | problems$output_id == id), , drop = FALSE]
    where <- ifelse(nzchar(p$item), paste0(p$part, ", ", p$item), p$part)
    items <- sprintf("- %s (%s): %s", toupper(p$level), where, p$message)
    txt <- if (ja) c(
      sprintf("\u4e0b\u66f8\u304d `%s`\uff08%s\uff09\u3092\u76f4\u3057\u3066\u304f\u3060\u3055\u3044\u3002\u691c\u67fb\u3067\u53d7\u3051\u4ed8\u3051\u3089\u308c\u307e\u305b\u3093\u3067\u3057\u305f\u3002", file, id),
      sprintf("\u5f62\u306f `ai/AGENTS.md` \u3068 `%s` \u3092\u3082\u3046\u4e00\u5ea6\u8aad\u3093\u3067\u304f\u3060\u3055\u3044\u3002", ins),
      "\u4e0b\u306e\u9805\u76ee\u3060\u3051\u3092\u76f4\u3057\u3001\u305d\u308c\u4ee5\u5916\u3068 `ai:` \u306e\u90e8\u5206\u306f\u305d\u306e\u307e\u307e\u6b8b\u3057\u3066\u304f\u3060\u3055\u3044\u3002",
      "", items, "",
      "\u76f4\u3057\u3066\u4fdd\u5b58\u3057\u305f\u3089\u3001\u3053\u3053\u3067\u77e5\u3089\u305b\u3066\u304f\u3060\u3055\u3044\u3002\u4eba\u304c\u3082\u3046\u4e00\u5ea6\u691c\u67fb\u3057\u307e\u3059\u3002"
    ) else c(
      sprintf("Fix the draft `%s` (%s): the check refused it.", file, id),
      sprintf("Read `ai/AGENTS.md` and `%s` again for the form.", ins),
      "Change only what is listed below; keep everything else, and the `ai:` block, as it is.",
      "", items, "",
      "When the file is saved, say so here; the person checks it again."
    )
    out[[id]] <- paste(txt, collapse = "\n")
  }
  out
}

.check_rows <- function(output_id = character(), part = character(), item = character(),
                        level = character(), message = character()) {
  n <- max(length(output_id), length(part), length(item), length(level), length(message))
  r <- function(x) if (length(x) == n) as.character(x) else rep(as.character(x), length.out = n)
  if (n == 0L) return(data.frame(output_id = character(), part = character(), item = character(),
                            level = character(), message = character(), stringsAsFactors = FALSE))
  data.frame(output_id = r(output_id), part = r(part), item = r(item), level = r(level),
             message = r(message), stringsAsFactors = FALSE)
}

# The key packages' installed versions
.check_versions <- function() {
  pk <- c("tflspec", "rtfreporter", "cards", "cardx", "ggplot2")
  v <- vapply(pk, function(p) {
    if (requireNamespace(p, quietly = TRUE)) as.character(utils::packageVersion(p)) else NA_character_
  }, "")
  data.frame(package = pk, installed = unname(v), stringsAsFactors = FALSE)
}
