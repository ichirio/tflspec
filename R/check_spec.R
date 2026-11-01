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
#'   function the installed rtfreporter does not have yet);
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
#' ([tfl_check_fig_design()]).  The packages' installed versions come with
#' it, as an attribute.  A GUI's check button and the AI's run call it.
#'
#' @param study A study read with [tfl_read_study_spec()], or its folder.
#' @param output_id The reports to check; `NULL` for every report.
#' @param build `TRUE`: each report the checks above pass is also built
#'   ([tfl_check_build()]) -- its data read, its ARD made, its table,
#'   listing or figure object made, no RTF -- so that a report with no
#'   row can be made; one that cannot has an `error` row, `part`
#'   `"build"`.  A few seconds a report.  `FALSE` (default): the checks
#'   above only.
#' @return A data frame, one row a problem: `output_id` (`NA` for the
#'   study's ARD definition as a whole), `part` (`"file"`, `"ard"`,
#'   `"content"`, `"program"`, `"figure"`, `"build"`), `item` (the file, the
#'   analysis, the function and argument, or the figure's part and
#'   field), `level` (`"error"`, `"warning"`, `"note"`, `"check"`) and
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
#'     tfl_output("T-DM", "table", population = "SAF",
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
tfl_check_spec <- function(study, output_id = NULL, build = FALSE) {
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

  for (id in setdiff(ids, bad)) {
    o <- outs[[id]]
    # its ARD definition, written as its program: the definition's problems
    # are errors; what the installed packages do not know, notes; a program
    # that cannot be written otherwise, a warning
    if (!is.null(study$dir) && length(o$ard$analyses)) {
      withCallingHandlers(
        tryCatch(tfl_ard_code(study, output_id = id, save = FALSE), error = function(e) {
          p <- e$problems
          if (is.data.frame(p) && nrow(p)) {
            add(output_id = ifelse(is.na(p$output_id), id, p$output_id), part = "ard",
                item = ifelse(nzchar(p$row %||% ""), paste0(p$row, "$", p$field), p$field),
                level = "error", message = p$message)
          } else {
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
    ro <- tryCatch(tfl_resolve_output(o, study), error = function(e) e)
    if (inherits(ro, "error")) {
      add(output_id = id, part = "content", item = "", level = "error", message = one(ro))
      next
    }
    # its own titles under a study header that prints the label, the title
    # and the population from the report's keys: printed twice (#518)
    if (length(.page_args(o$page$titles)) && ("defaults" %in% (o$extends %||% "defaults"))) {
      tok <- .page_output_tokens_used(study$defaults$page %||% list())
      if (length(tok)) {
        add(output_id = id, part = "page", item = "titles", level = "warning",
            message = sprintf(paste0("the study's header prints the report's label, title and population ",
                                     "already (%s): `page: titles` prints them twice. Remove `page: titles`; ",
                                     "`label:` and `population:` fill the header."),
                              paste0("{", tok, "}", collapse = ", ")))
      }
    }
    # what the AI marked to confirm (`ai: confirm`): to look at
    cf <- .ai_confirm_rows(ro)
    if (nrow(cf)) {
      add(output_id = id, part = "content", item = ifelse(is.na(cf$item), cf$where, paste0(cf$where, "$", cf$item)),
          level = "check", message = paste0("to confirm: ", cf$why))
    }
    if (inherits(ro$content, "tfl_r")) next
    if (identical(ro$kind, "figure") && !.fig_new_content(ro$content)) {
      p <- tryCatch(tfl_check_fig_design(.fig_design_of_output(ro)), error = function(e) {
        data.frame(part = "", field = "", problem = conditionMessage(e), stringsAsFactors = FALSE)
      })
      if (nrow(p)) {
        add(output_id = id, part = "figure", item = paste0(p$part, "$", p$field),
            level = "error", message = p$problem)
      }
      next
    }
    form_ok <- TRUE
    for (it in ro$content) {
      p <- tfl_schema_check(it)
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
    # the code is not)
    if (form_ok) {
      r <- tryCatch(suppressMessages(suppressWarnings(tfl_content_code(ro))), error = function(e) e)
      if (inherits(r, "error")) {
        add(output_id = id, part = "program", item = "content", level = "warning",
            message = paste("the report's code cannot be written:", one(r)))
      }
    }
  }
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
