# ============================================================================
#  tfl_ai_prepare(mode = "agent"): the study folder as a coding agent's work
# ----------------------------------------------------------------------------
#  A coding agent (GitHub Copilot's agent in VS Code, and the like) works
#  through every request of the study without stopping: it answers a
#  request into ai/answers/, runs run.R (tfl_ai_run(): collect, ARD, RTF,
#  ai/status.md) and repairs what the status says.  What it cannot settle
#  it takes a guess on and asks as an assumption's `question`; tfl_ai_run()
#  gathers the questions into ai/questions.md -- one question asked by
#  several reports once, the reports listed -- for the person to answer.
#  A second round reads the answers into ai/decisions.md and redoes the
#  reports they touch.  The instructions are AGENTS.md and
#  .github/copilot-instructions.md (the same text).
#
#    .ai_agent_files()   the instructions, run.R, questions / decisions
#    tfl_ai_run()        collect, build, status, questions
# ============================================================================

# The agent's files of a study folder: the instructions, run.R, and the
# questions / decisions files (never written over: the person's answers
# and decisions are in them).  `adam`, `book`: where the ADaM and the TOC /
# SHELL workbook are, from the study folder, as AGENTS.md says them.  The
# paths written.
.ai_agent_files <- function(dir, overwrite = FALSE, templates = FALSE, adam = NULL,
                            book = NULL) {
  src <- system.file("ai", "agent", package = "tflspec")
  rd <- function(f) paste(readLines(file.path(src, f), warn = FALSE, encoding = "UTF-8"),
                          collapse = "\n")
  out <- character()
  put <- function(to, text, again = FALSE) {
    f <- file.path(dir, to)
    if (file.exists(f) && !(again && isTRUE(overwrite))) return(invisible())
    .ai_write_text(text, f)
    out <<- c(out, f)
  }
  agents <- rd("AGENTS.md")
  place <- function(p) if (length(p) && !is.na(p[1L])) sprintf("`%s`", p[1L]) else
    "(where the request says)"
  agents <- .ai_fill(agents, list(workbook = place(book), adam = place(adam)))
  # with templates: answer with one where it fits (each request lists them)
  if (isTRUE(templates)) agents <- paste(agents, rd("AGENTS-templates.md"), sep = "\n\n")
  put("AGENTS.md", agents, again = TRUE)
  put(file.path(".github", "copilot-instructions.md"), agents, again = TRUE)
  put("run.R", rd("run.R"), again = TRUE)
  put(file.path("ai", "questions.md"), .ai_questions_head())
  put(file.path("ai", "decisions.md"), rd("decisions.md"))
  out
}

# The lines a request starts with in agent mode: the steps, so a request
# read alone says them, and where the ADaM and the workbook are (`adam`,
# `book`: from the study folder)
.ai_agent_request_head <- function(request, adam = NULL, book = NULL) {
  at <- function(p) length(p) && !is.na(p[1L])
  places <- c(if (at(book)) sprintf("the TOC / SHELL workbook is `%s`", book[1L]),
              if (at(adam)) sprintf("the ADaM is in `%s`", adam[1L]))
  c("<!-- tflspec: agent mode -->",
    paste0("> **For the agent** (AGENTS.md): answer this request with its one `yaml` block and ",
           "save the whole answer as `ai/answers/", request, ".md`; then run `Rscript run.R ", request, "` and ",
           "read `ai/status.md`. Read `ai/decisions.md` first and follow it. Do not stop to ask: ",
           "take the likeliest choice and write it as an assumption with its `question`. Work from ",
           "`ai/adam_summary.md`; read the ADaM with R only to settle a question it does not."),
    if (length(places)) paste0(
      "> From this study folder, ", paste(places, collapse = " and "), ". When you can open ",
      "them, use them to check what this request leaves unclear -- above all a shell's shape ",
      "(several parts, a part that writes only what differs, merged cells). When you cannot ",
      "(outside your workspace, no permission), decide from this request alone."),
    "", "---", "")
}

# A path as seen from the study folder `dir`: the part below it when it is
# inside (however it was given), else the whole path
.ai_from_study <- function(dir, p) {
  if (is.null(p) || !length(p) || is.na(p[1L])) return(NA_character_)
  root <- paste0(normalizePath(dir, winslash = "/", mustWork = FALSE), "/")
  q <- normalizePath(p, winslash = "/", mustWork = FALSE)
  same <- if (.Platform$OS.type == "windows") startsWith(tolower(q), tolower(root)) else
    startsWith(q, root)
  ifelse(same, substring(q, nchar(root) + 1L), q)
}

#' Run the study's AI answers through: collect, build, status, questions
#'
#' What a coding agent runs after each answer it saves (`run.R` of
#' [tfl_ai_prepare()]`(mode = "agent")` calls it): [tfl_ai_collect_answers()]
#' (read, check, write the report files, a repair prompt for an answer with
#' errors), then each report written (`ok`, `warning`) built -- its ARD
#' ([tfl_build_ard()]) and its RTF (rtfreporter) into the study's
#' `output_path` -- one report's failure not stopping the others.  It writes
#' `ai/status.md` (each report's status, its RTF or why none, each
#' request's next step, the questions open) and gathers the answers'
#' questions (an assumption's `question`) into `ai/questions.md`: one
#' question once, with every report that asks it; a question there already
#' is kept as it is -- its answer, its status -- with the new reports added
#' to its list.  Nothing is sent anywhere.
#'
#' A report is built again only when it changed: its report file, the
#' study's files (`study.yml`, `spec/data.yml`, `spec/defaults.yml`, its
#' templates), the ADaM it reads or the packages' versions -- else its RTF
#' (and PNG) of the last run is kept, as `ai/built.csv` says.  A report
#' that passed its checks but could not be built gets a repair
#' (`ai/repairs/<request>-build.md`) with why, which stays until the
#' request's reports are built; its RTF (and PNG) of an earlier run are
#' removed, and `ai/built.csv` keeps why, so that a run of other requests
#' still says it.  A report not built yet (only other requests run) says
#' so in `ai/status.md`.
#'
#' When the person keeps a list of the reports they expect
#' (`ai/expected_outputs.csv`, [tfl_ai_expected_write()]), the reports made
#' are compared with it, and `ai/status.md` says what differs (a report
#' missing, one dropped still there, a title or analysis set not the
#' list's, one the list does not have): told, never refused.
#'
#' @param study_dir The study folder.
#' @param rtf `FALSE` to build the ARDs only (no rtfreporter).
#' @param requests The requests whose reports are built (`"ae-1"`); `NULL`
#'   (the default): every request's.  Every answer is still read and
#'   checked, and `ai/status.md` lists them all.  `Rscript run.R ae-1`
#'   passes them.
#' @param rebuild `TRUE` to build every report again, changed or not.
#' @return A `tfl_ai_collected` (as [tfl_ai_collect_answers()]) whose
#'   `reports` also have `rtf` (the file made, else `NA`), `rtf_problem`
#'   and `built` (`"built"`, `"kept"` -- unchanged since the last run --
#'   or `NA`), and a `questions` data frame (`n`, `question`, `reports`, `assumptions`,
#'   `status`, `answer`), and `expected` (the comparison with the
#'   person's list: `output_id`, `result` -- `missing`, `still there`,
#'   `differs`, `not expected` --, `detail`; its counts in its attribute
#'   `text`; `NULL` without a list), invisibly.
#' @seealso [tfl_ai_prepare()] (`mode = "agent"`), [tfl_ai_outputs()].
#' @export
tfl_ai_run <- function(study_dir = ".", rtf = TRUE, requests = NULL, rebuild = FALSE) {
  fn <- "tfl_ai_run()"
  # (the study's own path: the run builds from inside it)
  here <- normalizePath(study_dir, winslash = "/", mustWork = FALSE)
  if (isTRUE(rtf) && !requireNamespace("rtfreporter", quietly = TRUE)) {
    .ard_stop(sprintf("%s: the RTF needs the rtfreporter package; or rtf = FALSE.", fn))
  }
  res <- tfl_ai_collect_answers(study_dir)
  r <- res$reports
  r$rtf <- NA_character_
  r$png <- NA_character_
  r$rtf_problem <- NA_character_
  r$built <- NA_character_
  if (!is.null(requests)) {
    bad <- setdiff(requests, r$request)
    if (length(bad)) {
      .ard_stop(sprintf("%s: no request %s; the requests are %s.", fn,
                        paste(sQuote(bad, FALSE), collapse = ", "),
                        paste(unique(r$request), collapse = ", ")))
    }
  }
  todo <- which(r$status %in% c("ok", "warning") & !is.na(r$file))
  # the reports not asked for this time: as the last run left them
  done <- .ai_built_read(study_dir)
  keep_as <- function(i) {
    b <- done[match(r$output_id[i], done$output_id), , drop = FALSE]
    if (!nrow(b) || is.na(b$output_id)) return(FALSE)
    # its last build failed: still so
    if (!is.na(b$problem) && nzchar(b$problem)) {
      r$rtf_problem[i] <<- b$problem
      return(TRUE)
    }
    if (.ai_built_there(study_dir, b)) {
      r$rtf[i] <<- b$rtf
      r$png[i] <<- b$png
      r$built[i] <<- "kept"
      TRUE
    } else FALSE
  }
  if (!is.null(requests)) {
    for (i in setdiff(todo, which(r$request %in% requests))) keep_as(i)
    todo <- intersect(todo, which(r$request %in% requests))
  }
  if (length(todo)) {
    old <- setwd(study_dir)
    on.exit(setwd(old), add = TRUE)
    # (a study with the AI's drafts apart: they are read and built, into ai/output)
    area_opt <- .ai_area_options(".")
    if (!is.null(area_opt)) on.exit(options(area_opt), add = TRUE)
    st <- tryCatch(tfl_read_study_spec("."), error = function(e) e)
    outs <- if (!inherits(st, "error")) tryCatch(tfl_read_outputs("."), error = function(e) e)
    if (inherits(st, "error") || inherits(outs, "error")) {
      e <- if (inherits(st, "error")) st else outs
      r$rtf_problem[todo] <- paste("the study could not be read:", .ai_one_line(conditionMessage(e)))
    } else {
      out_dir <- st$study$settings$output_path %||% "output"
      # (the drafts' RTF under ai/, as the study's under its output path)
      if (!is.null(area_opt)) out_dir <- file.path("ai", out_dir)
      common <- .ai_built_common(st)
      for (i in todo) {
        id <- r$output_id[i]
        # unchanged since the last run, its files there: kept
        key <- .ai_built_key(id, common)
        b <- done[match(id, done$output_id), , drop = FALSE]
        if (!isTRUE(rebuild) && nrow(b) && identical(b$key, key) && .ai_built_there(".", b) &&
            (!isTRUE(rtf) || !is.na(b$rtf))) {
          r$rtf[i] <- b$rtf
          r$png[i] <- b$png
          r$built[i] <- "kept"
          next
        }
        made <- tryCatch({
          o <- tfl_resolve_output(outs[[id]], st)
          dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
          f <- file.path(out_dir, paste0(id, ".rtf"))
          if (identical(o$kind, "figure")) {
            # drawn by its program, as tflplanner draws it: the PNG, then the RTF
            fig <- if (!is.null(.tpl_spec_of(outs[[id]]))) .ai_tpl_fig_draw(st, o, out_dir) else
              .ai_fig_draw(st, id, out_dir)
            if (isTRUE(rtf)) {
              suppressMessages(rtfreporter::generate_rtfreport(
                tfl_report(o, content = fig$plot, study = st), f, overwrite = tfl_report_overwrite(o)))
            }
            list(rtf = if (isTRUE(rtf)) f, png = fig$png)
          } else if (identical(o$kind, "listing") && identical(o$data$source, "dataset")) {
            # a listing: its records (read with rtf = FALSE too), laid out
            recs <- .listing_data_of(o, st, ".")
            if (isTRUE(rtf)) {
              plan <- suppressMessages(tfl_content_run(o, recs))
              suppressMessages(rtfreporter::generate_rtfreport(
                tfl_report(o, content = plan, study = st), f, overwrite = tfl_report_overwrite(o)))
            }
            list(rtf = if (isTRUE(rtf)) f)
          } else {
            ard <- suppressMessages(tfl_build_ard(st, dir = ".", output_id = id, save = FALSE))
            if (!is.null(ard$output_id)) ard <- ard[ard$output_id %in% id, , drop = FALSE]
            # a table whose columns have no widths: the usual layout's
            # (the stub wider), written into its report file -- what a
            # person then changes is a value, not an argument to add
            widened <- FALSE
            if (isTRUE(rtf) || !.ai_has_widths(o)) {
              plan <- suppressMessages(tfl_content_run(o, ard))
              w <- .ai_table_widths(outs[[id]], o, plan)
              if (!is.null(w)) {
                outs[[id]] <- w
                tfl_write_output(w, file.path(.outputs_dir("."), paste0(id, ".yml")))
                pc <- Filter(function(it) identical(it$fn, "plan_columns"), w$content)
                .ai_widths_remember(".", id, pc[[1L]]$args$widths)
                o <- tfl_resolve_output(w, st)
                if (isTRUE(rtf)) plan <- suppressMessages(tfl_content_run(o, ard))
                widened <- TRUE
              }
            }
            if (isTRUE(rtf)) {
              suppressMessages(rtfreporter::generate_rtfreport(
                tfl_report(o, content = plan, study = st), f, overwrite = tfl_report_overwrite(o)))
            }
            list(rtf = if (isTRUE(rtf)) f, widened = widened)
          }
        }, error = function(e) structure(.ai_one_line(conditionMessage(e)), class = "failed"))
        if (inherits(made, "failed")) {
          r$rtf_problem[i] <- unclass(made)
          # the last run's RTF and PNG are not this answer's: gone
          old_files <- c(file.path(out_dir, paste0(id, c(".rtf", ".png"))), b$rtf, b$png)
          old_files <- unique(old_files[!is.na(old_files) & nzchar(old_files)])
          unlink(old_files[file.exists(old_files)])
          # (said again by a run that does not build it)
          done <- rbind(done[!done$output_id %in% id, , drop = FALSE],
                        data.frame(output_id = id, key = key, rtf = NA_character_,
                                   png = NA_character_, problem = r$rtf_problem[i],
                                   stringsAsFactors = FALSE))
        } else {
          # (its report file given widths: the key of the file as it is now)
          if (isTRUE(made$widened)) key <- .ai_built_key(id, common)
          if (!is.null(made$rtf)) r$rtf[i] <- gsub("\\\\", "/", made$rtf)
          if (!is.null(made$png)) r$png[i] <- gsub("\\\\", "/", made$png)
          r$built[i] <- "built"
          done <- rbind(done[!done$output_id %in% id, , drop = FALSE],
                        data.frame(output_id = id, key = key, rtf = r$rtf[i], png = r$png[i],
                                   problem = NA_character_, stringsAsFactors = FALSE))
        }
      }
      .ai_built_write(".", done)
    }
  }
  # a report that passed its checks but could not be built: a repair, as
  # for an answer with errors
  .ai_build_repairs(study_dir, r)
  res$reports <- r
  res$questions <- .ai_questions_update(study_dir, res$assumptions)
  # the reports made against the person's list (ai/expected_outputs.csv):
  # told, not refused
  res$expected <- .ai_expected_compare(tfl_ai_expected_read(here), tfl_ai_outputs(here))
  .ai_write_text(.ai_status_text(res), file.path(study_dir, "ai", "status.md"))
  made <- sum(!is.na(r$rtf))
  open <- sum(res$questions$status %in% "open")
  message(sprintf("%s; %d question(s) open in ai/questions.md; see ai/status.md.",
                  .ai_count_text(res), open))
  invisible(res)
}

# ---- what the last run built: ai/built.csv ---------------------------------------
# A report's key: its report file's md5, the study's files' (study.yml,
# spec/data.yml, spec/defaults.yml, spec/templates/), the ADaM files' size
# and time, the packages' versions.  The same key, its RTF / PNG there:
# nothing to build.

.ai_built_file <- function(dir) file.path(dir, "ai", "built.csv")

.ai_built_read <- function(dir) {
  f <- .ai_built_file(dir)
  empty <- data.frame(output_id = character(), key = character(), rtf = character(),
                      png = character(), problem = character(), stringsAsFactors = FALSE)
  if (!file.exists(f)) return(empty)
  x <- tryCatch(utils::read.csv(f, stringsAsFactors = FALSE, colClasses = "character"),
                error = function(e) empty)
  # (a file of an older tflspec: no problem column, every row built)
  if (nrow(x) && !"problem" %in% names(x)) x$problem <- NA_character_
  if (!all(names(empty) %in% names(x))) return(empty)
  x[names(empty)]
}

.ai_built_write <- function(dir, x) {
  dir.create(file.path(dir, "ai"), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(x[order(x$output_id), , drop = FALSE], .ai_built_file(dir), row.names = FALSE)
}

# its RTF (and PNG) are still there
.ai_built_there <- function(dir, b) {
  ok <- function(p) is.na(p) || !nzchar(p) || file.exists(file.path(dir, p))
  !is.na(b$rtf) && ok(b$rtf) && ok(b$png)
}

# what every report reads: the study's files, the ADaM, the packages
.ai_built_common <- function(st) {
  md5 <- function(f) {
    f <- f[file.exists(f)]
    if (length(f)) paste(basename(f), unname(tools::md5sum(f)), collapse = ";") else ""
  }
  tpl <- sort(list.files(file.path("spec", "templates"), "[.][Rr]$", full.names = TRUE))
  # (the datasets' paths: from the study folder, or whole)
  data <- unlist(lapply(st$data$datasets %||% list(), function(d) d$path), use.names = FALSE)
  fi <- file.info(data[file.exists(data)])
  adam <- if (nrow(fi)) paste(rownames(fi), fi$size, format(fi$mtime, "%Y%m%d%H%M%S"), collapse = ";") else ""
  pk <- function(p) if (requireNamespace(p, quietly = TRUE)) format(utils::packageVersion(p)) else "-"
  paste(md5(c("study.yml", file.path("spec", c("data.yml", "defaults.yml")))), md5(tpl), adam,
        "tflspec", pk("tflspec"), "rtfreporter", pk("rtfreporter"), sep = "|")
}

.ai_built_key <- function(id, common) {
  # (the drafts' folder in a study with the AI's drafts apart)
  f <- file.path(.outputs_dir("."), paste0(id, ".yml"))
  own <- if (file.exists(f)) unname(tools::md5sum(f)) else ""
  rlang::hash(paste(own, common, sep = "|"))
}

# A report that passed its checks but could not be built: its repair --
#   ai/repairs/<request>-build.md: a file of its own, run.R's -- the checks'
#   repair (<request>.md) is tfl_ai_collect_answers()'s, which removes it
#   when the answer passes; this one stays until the request's reports are
#   built (a collect run in between, a screen's reload, leaves it)
.ai_build_repair_file <- function(dir, request) {
  file.path(dir, "ai", "repairs", paste0(request, "-build.md"))
}

# The package a build problem says is missing (R's "there is no package
# called 'x'", tflspec's "needs the x package"), else NA: the PC's to
# install, no fault of the answer
.ai_missing_package <- function(x) {
  rx <- c("there is no package called [^A-Za-z0-9.]*([A-Za-z][A-Za-z0-9.]*)",
          "needs the ([A-Za-z][A-Za-z0-9.]*) package")
  out <- rep(NA_character_, length(x))
  for (re in rx) {
    hit <- is.na(out) & !is.na(x) & grepl(re, x, perl = TRUE)
    out[hit] <- sub(paste0("^.*?", re, ".*$"), "\\1", x[hit], perl = TRUE)
  }
  out
}

.ai_build_repairs <- function(dir, r) {
  bad <- r[!is.na(r$rtf_problem), , drop = FALSE]
  # (a missing package is no repair of the answer: status.md says to install it)
  bad <- bad[is.na(.ai_missing_package(bad$rtf_problem)), , drop = FALSE]
  # a request whose reports are all built (or kept) now: its build repair goes
  ok <- setdiff(unique(r$request[r$built %in% c("built", "kept")]), bad$request)
  for (q in ok) {
    f <- .ai_build_repair_file(dir, q)
    if (file.exists(f)) unlink(f)
  }
  if (!nrow(bad)) return(invisible())
  rd <- file.path(dir, "ai", "repairs")
  dir.create(rd, recursive = TRUE, showWarnings = FALSE)
  for (q in unique(bad$request)) {
    b <- bad[bad$request == q, , drop = FALSE]
    .ai_write_text(c(
      sprintf("# Repair: %s -- could not be built", q), "",
      "The answer passed its checks, but a report could not be built. Fix the answer",
      sprintf("(ai/answers/%s.md, the whole answer again) and run `Rscript run.R %s`.", q, q), "",
      "## Could not be built", "",
      sprintf("- %s: %s", b$output_id, b$rtf_problem)), .ai_build_repair_file(dir, q))
    a <- file.path(dir, "ai", "answers", paste0(q, ".md"))
    if (file.exists(a)) .ai_repair_log(rd, q, a)
  }
  invisible()
}

# ---- ai/status.md -----------------------------------------------------------------

# The count of the reports: those the answers have (a report asked for and
# not in its answer, or not answered yet, is not one) and the TOC items,
# and how many have an RTF
.ai_count_text <- function(res) {
  r <- res$reports
  made <- sum(!is.na(r$rtf))
  m <- res$toc_items %||% NA_integer_
  if (is.na(m)) return(sprintf("%d of %d reports have an RTF", made, nrow(r)))
  have <- r$status != "no answer" & !grepl("^not in the answer", ifelse(is.na(r$first), "", r$first))
  sprintf("Reports (in the answers) %d, TOC items %d: %d have an RTF", sum(have), m, made)
}

.ai_status_text <- function(res) {
  r <- res$reports
  cell <- function(x) ifelse(is.na(x), "", gsub("|", "/", x, fixed = TRUE))
  q <- res$questions
  bad <- r[!is.na(r$rtf_problem), , drop = FALSE]
  nxt <- res$next_steps
  # passed its checks, never built (a run of other requests only)
  wait <- r[r$status %in% c("ok", "warning") & is.na(r$rtf) & is.na(r$rtf_problem) &
              is.na(r$built), , drop = FALSE]
  for (w in unique(wait$request)) {
    nxt[w] <- sprintf("the answer passed its checks; %s not built yet: run `Rscript run.R %s`",
                      paste(wait$output_id[wait$request == w], collapse = ", "), w)
  }
  pkg <- .ai_missing_package(bad$rtf_problem)
  for (i in seq_len(nrow(bad))) {
    nxt[bad$request[i]] <- if (!is.na(pkg[i])) {
      sprintf(paste0("the answer passed its checks but %s could not be built: R has no package %s ",
                     "on this PC -- not the answer's fault, do not change it. Ask the person to ",
                     "install it (install.packages(\"%s\")) in a question, go on with the next ",
                     "request, and run `Rscript run.R %s` again once it is installed"),
              bad$output_id[i], pkg[i], pkg[i], bad$request[i])
    } else {
      sprintf(paste0("the answer passed its checks but %s could not be built (%s): ",
                     "fix the answer (ai/repairs/%s-build.md) and run again"),
              bad$output_id[i], bad$rtf_problem[i], bad$request[i])
    }
  }
  c(sprintf("# Status (written by run.R, %s)", format(Sys.time(), "%Y-%m-%d %H:%M")),
    "",
    sprintf("%s. Questions: %d open, %d decided (ai/questions.md).",
            .ai_count_text(res), sum(q$status %in% "open"), sum(q$status %in% "decided")),
    "",
    .ai_templates_status(res$templates),
    "",
    "| request | report | answer | RTF | the first problem |",
    "|---|---|---|---|---|",
    sprintf("| %s | %s | %s | %s | %s |", r$request, r$output_id, r$status,
            ifelse(is.na(r$rtf), "none", r$rtf), cell(ifelse(is.na(r$rtf_problem), r$first,
                                                              r$rtf_problem))),
    "",
    "## Next, each request",
    "",
    if (length(nxt)) sprintf("- %s: %s", names(nxt), nxt) else "- (no request)",
    .ai_expected_status(res$expected),
    if (length(res$stray)) c("", sprintf(paste0("Not read (no request of that name): %s -- ",
                                                "rename each to <request>.md."),
                                         paste(res$stray, collapse = ", "))))
}

# ---- ai/questions.md ----------------------------------------------------------------

.ai_questions_head <- function() {
  c("# Questions for the person",
    "",
    "The agent's open questions, one each, with the reports they touch.",
    "Write your answer after `- Answer:` (one line or more); with your corrections",
    "(tfl_ai_corrections(): ai/corrections.md) tell the agent \"the answers and",
    "corrections are written\". `- Status: decided` is set by the agent once it has",
    "moved the answer to `ai/decisions.md` and redone the reports.",
    "")
}

# The questions of the file: one row a `## Q<n>. <question>` block.
.ai_questions_read <- function(lines) {
  at <- grep("^## Q[0-9]+[.] ", lines)
  none <- data.frame(n = integer(), question = character(), reports = character(),
                     assumptions = character(), where = character(), provisional = character(),
                     status = character(), answer = character(), start = integer(),
                     end = integer(), stringsAsFactors = FALSE)
  if (!length(at)) return(none)
  end <- c(at[-1L] - 1L, length(lines))
  rows <- lapply(seq_along(at), function(i) {
    b <- lines[at[i]:end[i]]
    field <- function(k) {
      j <- grep(sprintf("^- %s:", k), b)
      if (!length(j)) return(NA_character_)
      v <- trimws(sub(sprintf("^- %s:", k), "", b[j[1L]]))
      if (nzchar(v)) v else NA_character_
    }
    ans <- grep("^- Answer:", b)
    answer <- if (length(ans)) {
      a <- c(sub("^- Answer:", "", b[ans[1L]]), b[-seq_len(ans[1L])])
      a <- trimws(paste(a, collapse = "\n"))
      if (nzchar(a)) a else NA_character_
    } else NA_character_
    data.frame(n = as.integer(sub("^## Q([0-9]+)[.] .*$", "\\1", b[1L])),
               question = trimws(sub("^## Q[0-9]+[.] ", "", b[1L])),
               reports = field("Reports"), assumptions = field("Assumptions"),
               where = field("Where"),
               provisional = field("The agent took, for now"),
               status = tolower(field("Status") %||% NA_character_),
               answer = answer, start = at[i], end = end[i], stringsAsFactors = FALSE)
  })
  do.call(rbind, c(list(none), rows))
}

# The answers' questions into ai/questions.md: a new question added, one
# there already given the reports that ask it too; the rest as it is.
.ai_questions_update <- function(study_dir, assumptions) {
  f <- file.path(study_dir, "ai", "questions.md")
  lines <- if (file.exists(f)) readLines(f, warn = FALSE, encoding = "UTF-8") else .ai_questions_head()
  a <- assumptions
  a <- a[!is.na(a$question) & nzchar(trimws(a$question)), , drop = FALSE]
  key <- function(x) tolower(trimws(gsub("[[:space:][:punct:]]+", " ", x)))
  q <- .ai_questions_read(lines)
  changed <- FALSE
  if (nrow(a)) {
    groups <- split(seq_len(nrow(a)), key(a$question))
    groups <- groups[order(vapply(groups, min, 1L))]
    listed <- function(x) {
      if (is.na(x)) return(character())
      x <- trimws(strsplit(x, ",", fixed = TRUE)[[1L]])
      x[nzchar(x)]
    }
    for (g in groups) {
      ids <- unique(stats::na.omit(a$output_id[g]))
      # the assumptions asking it (their A<n>, R/ai_assumptions.R)
      aids <- if (is.null(a$id)) character() else unique(stats::na.omit(a$id[g]))
      k <- match(key(a$question[g[1L]]), key(q$question))
      if (!is.na(k)) {
        have <- listed(q$reports[k])
        add <- setdiff(ids, have)
        have_a <- listed(q$assumptions[k])
        add_a <- setdiff(aids, have_a)
        if (!length(add) && !length(add_a)) next
        # (each field's line replaced, or put after the heading / the reports)
        put_line <- function(field, line) {
          q <- .ai_questions_read(lines)
          b <- q$start[k]:q$end[k]
          j <- b[grep(sprintf("^- %s:", field), lines[b])]
          r <- b[grep("^- Reports:", lines[b])]
          if (length(j)) lines[j[1L]] <<- line else
            lines <<- append(lines, line, after = if (length(r)) r[1L] else q$start[k])
        }
        if (length(add)) put_line("Reports", sprintf("- Reports: %s", paste(c(have, add), collapse = ", ")))
        if (length(add_a)) {
          put_line("Assumptions", sprintf("- Assumptions: %s", paste(c(have_a, add_a), collapse = ", ")))
        }
      } else {
        n <- if (nrow(q)) max(q$n) + 1L else 1L
        while (length(lines) && !nzchar(trimws(lines[length(lines)]))) lines <- lines[-length(lines)]
        lines <- c(lines, "",
                   sprintf("## Q%d. %s", n, trimws(a$question[g[1L]])),
                   "",
                   sprintf("- Reports: %s", paste(ids, collapse = ", ")),
                   if (length(aids)) sprintf("- Assumptions: %s", paste(aids, collapse = ", ")),
                   sprintf("- Where: %s", paste(unique(stats::na.omit(c(a$where[g], a$item[g]))),
                                                collapse = ", ")),
                   sprintf("- The agent took, for now: %s", trimws(a$assumption[g[1L]] %||% "")),
                   "- Status: open",
                   "- Answer:")
      }
      changed <- TRUE
      q <- .ai_questions_read(lines)
    }
  }
  if (changed || !file.exists(f)) .ai_write_text(lines, f)
  q <- .ai_questions_read(lines)
  q$status[is.na(q$status)] <- "open"
  rownames(q) <- NULL
  q[c("n", "question", "reports", "assumptions", "where", "provisional", "status", "answer")]
}

# ---- spec/toc.yml -------------------------------------------------------------------

# The TOC read from the workbook as spec/toc.yml, in tflplanner's form: the
# reports in the TOC's order, each with its kind, its title, its section
# (the number's first parts: 14.1 of 14.1.1) and its analysis set when the
# TOC row names one of spec/data.yml's (its id or its label) and no other.
# Written when the study has none (or `overwrite`); the path, else nothing.
.ai_toc_yml <- function(dir, shell, overwrite = FALSE, add = FALSE) {
  f <- file.path(dir, "spec", "toc.yml")
  t <- shell$toc
  # added to a study (add): the TOC there, these reports at its end
  had <- if (isTRUE(add) && file.exists(f)) {
    tryCatch(yaml::read_yaml(f)$outputs, error = function(e) NULL)
  }
  if (length(had)) {
    have <- vapply(had, function(e) e$id %||% NA_character_, "")
    t <- t[!t$output_id %in% have, , drop = FALSE]
    if (!nrow(t)) return(character())
  } else if (!nrow(t) || (file.exists(f) && !isTRUE(overwrite))) return(character())
  pop_of <- function(txt) .ai_population_of(dir, txt)$id %||% NA_character_
  # the id written at the top of each item's shell (its best sheet): the
  # file name's {shell_code}; the study's own form (settings: shell_code)
  sy <- file.path(dir, "spec", "study.yml")
  code_rx <- if (file.exists(sy)) tryCatch(yaml::read_yaml(sy)$settings$shell_code,
                                           error = function(e) NULL)
  code_of <- function(i) {
    s <- if (!is.null(t$sheets)) strsplit(.na_or(t$sheets[i], ""), " | ", fixed = TRUE)[[1L]][1L]
    if (is.null(s) || is.na(s) || is.null(shell$cells)) return(NA_character_)
    .shell_code_of(shell$cells[shell$cells$sheet == s, , drop = FALSE], code_rx)
  }
  # a report written in parts (tfl_ai_read_shell()$toc$parts): a TOC entry
  # a part, in the report's place
  if (!is.null(t$parts) && any(!is.na(t$parts))) {
    t <- do.call(rbind, lapply(seq_len(nrow(t)), function(i) {
      if (is.na(t$parts[i])) return(t[i, , drop = FALSE])
      p <- strsplit(t$parts[i], " | ", fixed = TRUE)[[1L]]
      r <- t[rep(i, length(p)), , drop = FALSE]
      r$output_id <- p
      r$key <- .ai_id_key(p)
      r
    }))
  }
  outs <- lapply(seq_len(nrow(t)), function(i) {
    num <- sub("^[A-Z]*", "", t$key[i] %||% "")
    parts <- strsplit(num, ".", fixed = TRUE)[[1L]]
    e <- list(id = t$output_id[i], kind = if (is.na(t$type[i])) "table" else t$type[i])
    if (!is.na(t$title[i]) && nzchar(t$title[i])) e$label <- t$title[i]
    if (length(parts) >= 3L) e$section <- paste(parts[seq_len(length(parts) - 1L)], collapse = ".")
    p <- pop_of(t$text[i])
    if (!is.na(p)) e$population <- p
    sc <- code_of(i)
    if (!is.na(sc)) e$shell_code <- sc
    e
  })
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  outs <- c(had %||% list(), outs)
  .ai_write_text(yaml::as.yaml(list(outputs = outs), indent.mapping.sequence = TRUE), f)
  f
}

# The analysis set a TOC row names: the one of spec/data.yml whose label or
# id the row's text holds, and no other; NULL when none or several.  As
# tfl_ai_context(population = ): id, label and where.
.ai_population_of <- function(dir, txt) {
  dy <- file.path(dir, "spec", "data.yml")
  if (is.null(txt) || is.na(txt) || !file.exists(dy)) return(NULL)
  pops <- tryCatch(yaml::read_yaml(dy, handlers = list(r = function(x) x))$populations,
                   error = function(e) NULL)
  if (!length(pops)) return(NULL)
  low <- tolower(txt)
  hit <- vapply(pops, function(p) {
    lab <- p$label %||% NA_character_
    id <- p$id %||% NA_character_
    (!is.na(lab) && grepl(tolower(lab), low, fixed = TRUE)) ||
      (!is.na(id) && grepl(sprintf("(^|[^a-z0-9])%s([^a-z0-9]|$)", tolower(id)), low))
  }, NA)
  if (sum(hit) != 1L) return(NULL)
  p <- pops[[which(hit)]]
  out <- list(id = p$id, label = p$label, where = if (!is.null(p$where)) as.character(p$where))
  out[!vapply(out, is.null, NA)]
}

#' The questions of a study's AI drafts, and the person's answers to them
#'
#' `ai/questions.md` holds what the AI could not settle -- an assumption's
#' `question`, gathered by [tfl_ai_run()]: one question asked by several
#' reports once, the reports listed.  `tfl_ai_questions()` reads it; a
#' screen shows the questions and takes the answers, and
#' `tfl_ai_answer_questions()` writes them back -- each into its question's
#' `- Answer:`, everything else as it is.  The agent (or the person) then
#' moves an answered question to `ai/decisions.md`, sets its status to
#' `decided`, and redoes the reports it touches.
#'
#' The file: each question a block from its heading `## Q<n>. <question>`
#' to the next heading, holding `- Reports:`, `- Assumptions:` (the ids of
#' the assumptions asking it, [tfl_ai_assumptions()]), `- Where:`, `- The
#' agent took, for now:`, `- Status: open` (or `decided`) and `- Answer:`
#' (one line or more, to the block's end).  A question is answered through
#' its assumptions too ([tfl_ai_assumption_answers_write()]).
#'
#' @param study_dir The study folder.
#' @param answers The answers: a data frame with `id` (`"Q1"`) and
#'   `answer`, or a character vector named by the ids.  `NA` or `""`
#'   empties an answer.
#' @return `tfl_ai_questions()`: a data frame, one row a question: `id`,
#'   `output_id` (the reports it touches, comma-separated), `where`,
#'   `question`, `provisional` (what the AI took for now), `answer` (`NA`
#'   when none), `status` (`open`, `decided`); no row when there is no
#'   question.  `tfl_ai_answer_questions()`: the file's path, invisibly.
#' @seealso [tfl_ai_run()], [tfl_ai_prepare()] (`mode = "agent"`).
#' @export
tfl_ai_questions <- function(study_dir) {
  f <- file.path(study_dir, "ai", "questions.md")
  lines <- if (file.exists(f)) .ai_read_lines(f, "tfl_ai_questions()") else character()
  q <- .ai_questions_read(lines)
  status <- q$status
  status[is.na(status)] <- "open"
  out <- data.frame(id = if (nrow(q)) paste0("Q", q$n) else character(),
                    output_id = q$reports, where = q$where, question = q$question,
                    provisional = q$provisional, answer = q$answer, status = status,
                    stringsAsFactors = FALSE)
  rownames(out) <- NULL
  out
}

#' @rdname tfl_ai_questions
#' @export
tfl_ai_answer_questions <- function(study_dir, answers) {
  fn <- "tfl_ai_answer_questions()"
  f <- file.path(study_dir, "ai", "questions.md")
  if (!file.exists(f)) .ard_stop(sprintf("%s: %s has no ai/questions.md.", fn, study_dir))
  if (is.data.frame(answers)) {
    if (!all(c("id", "answer") %in% names(answers))) {
      .ard_stop(sprintf("%s: `answers` as a data frame has the columns `id` and `answer`.", fn))
    }
    answers <- stats::setNames(as.character(answers$answer), as.character(answers$id))
  }
  if (!is.character(answers) || is.null(names(answers)) || any(!nzchar(names(answers)))) {
    .ard_stop(sprintf("%s: `answers` is a data frame (id, answer) or text named by the ids.", fn))
  }
  lines <- .ai_read_lines(f, fn)
  q <- .ai_questions_read(lines)
  ids <- paste0("Q", q$n)
  bad <- setdiff(names(answers), ids)
  if (length(bad)) {
    .ard_stop(sprintf("%s: no question %s in ai/questions.md (it has %s).", fn,
                      paste(bad, collapse = ", "),
                      if (length(ids)) paste(ids, collapse = ", ") else "none"))
  }
  # the blocks from the last, so the line numbers of the others hold
  for (k in rev(match(names(answers), ids))) {
    b <- q$start[k]:q$end[k]
    j <- b[grep("^- Answer:", lines[b])]
    txt <- answers[[ids[k]]]
    new <- if (is.na(txt) || !nzchar(trimws(txt))) character() else
      strsplit(gsub("\r\n?", "\n", txt), "\n", fixed = TRUE)[[1L]]
    new <- c(paste(c("- Answer:", new[1L][length(new) > 0L]), collapse = " "), new[-1L])
    if (!length(j)) {
      # no answer line: one after the block's last line of text
      last <- max(b[nzchar(trimws(lines[b]))])
      lines <- append(lines, new, after = last)
      next
    }
    j <- j[1L]
    # the old answer: from its line to the block's last line of text (the
    # blank lines before the next question stay)
    text_at <- b[b >= j & nzchar(trimws(lines[b]))]
    to <- max(text_at)
    lines <- c(lines[seq_len(j - 1L)], new, lines[-seq_len(to)])
  }
  .ai_write_text(lines, f)
  invisible(normalizePath(f, winslash = "/"))
}
