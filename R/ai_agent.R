# ============================================================================
#  tfl_ai_prepare(): the study folder as a coding agent's work
# ----------------------------------------------------------------------------
#  A coding agent (GitHub Copilot's agent in VS Code, and the like) asks the
#  person in the chat what defines each report and is not settled, keeps
#  the answers in ai/decisions.md, writes each report's skeleton as a draft
#  (ai/spec/outputs/<id>.yml, with an `ai:` block: assumptions, confirm)
#  and stops; the person checks the drafts (tflplanner's "Check",
#  tfl_ai_collect_answers(), or run.R -> tfl_ai_run(): collect, ARD, RTF,
#  ai/status.md), finishes them in the builder and adopts them.  The
#  instructions are AGENTS.md and .github/copilot-instructions.md (the same
#  text; R/ai_route_spec.R fills its settings).
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
                            book = NULL, reads = character(), docs = character(),
                            docs_from = character(), max_reads = 3L) {
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
  # the instructions, with the study's settings (study.yml's ai: keys)
  agents <- .ai_spec_agents(rd("AGENTS.md"), .ai_spec_settings(dir))
  place <- function(p) if (length(p) && !is.na(p[1L])) sprintf("`%s`", p[1L]) else
    "(where the request says)"
  # what the agent may open besides the requests: the workbook and the ADaM
  # when the person allows them, the documents the person gave; read with
  # whatever the PC has, not R alone
  more <- c(
    if ("workbook" %in% reads) paste0(
      "\n- the TOC / SHELL workbook {{workbook}}, to check what a shell's text leaves ",
      "unclear (its shape, merged cells, a text that looks cut): read only the place ",
      "the request names (the sheet, the cells);"),
    if ("adam" %in% reads) paste0(
      "\n- the ADaM in {{adam}}, only to settle what `ai/adam_summary.md` does not (a ",
      "value as the data write it, a variable): never in the editor, and print only ",
      "what settles it (a variable's values, counts), not subjects' records;"),
    if (length(docs)) paste0(
      "\n- the documents the person gave, for what a request leaves unclear: ",
      paste(sprintf("`%s`%s", docs, ifelse(
        is.na(docs_from %||% NA) | !nzchar(docs_from %||% ""), "",
        sprintf(" (the text of %s)", basename(docs_from %||% "")))), collapse = ", "),
      "."))
  tools <- if (any(c("workbook", "adam") %in% reads)) paste0(
    "\n\nRead the workbook or the data with a tool this PC has -- PowerShell, ",
    "Python, R -- whichever works; install nothing. In PowerShell, put a text with ",
    "`$` in single quotes ('...'), or `$` is read as a variable. Do not open a ",
    "data file (`.xpt`, `.sas7bdat`) in the editor.")
  agents <- .ai_fill(agents, list(
    reads = paste(c(more, tools), collapse = ""),
    max_reads = as.character(max_reads),
    run_r = if (any(c("workbook", "adam") %in% reads)) {
      "Run a command only to read the files listed under \"What you read\"; nothing else."
    } else "Do not run R or any other program."))
  agents <- .ai_fill(agents, list(workbook = place(book), adam = place(adam)))
  # with templates: a template where it fits (each request lists them)
  if (isTRUE(templates)) agents <- paste(agents, rd("AGENTS-templates.md"), sep = "\n\n")
  # the common instructions in ai/; the study folder's own AGENTS.md (what
  # an agent reads on its own) and the Copilot copy point to them
  put(file.path("ai", "AGENTS.md"), agents, again = TRUE)
  put("AGENTS.md", rd("AGENTS-root.md"), again = TRUE)
  put(file.path(".github", "copilot-instructions.md"), rd("AGENTS-root.md"), again = TRUE)
  put("run.R", rd("run.R"), again = TRUE)
  put(file.path("ai", "decisions.md"), rd("decisions.md"))
  out
}

# The lines a request starts with: the agent's steps, so a request read
# alone says them.  `ids`: the request's reports, whose drafts it writes.
.ai_agent_request_head <- function(request, ids = character()) {
  files <- paste(sprintf("`ai/spec/outputs/%s.yml`", ids), collapse = ", ")
  c("<!-- tflspec: agent -->",
    paste0("> **For the agent** (ai/AGENTS.md): read `ai/decisions.md` and the decisions below, ",
           "then ask in the chat what defines these reports and is not settled -- and wait for ",
           "the answers. Then write each report's skeleton as its file: ", files, ". ",
           "Record the answers in `ai/decisions.md`; mark what you could not settle under `ai: confirm`. ",
           "Read only this request and the files it names; do not look for others, install nothing, ",
           "run nothing. Write nothing of the look."),
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

#' Run the study's AI drafts through: collect, build, status
#'
#' The person's check from the command line (`run.R` of [tfl_ai_prepare()]
#' calls it): [tfl_ai_collect_answers()] (read the agent's drafts, check
#' them, write the report files, a repair prompt for a draft with errors), then each report written (`ok`, `warning`) built -- its ARD
#' ([tfl_build_ard()]) and its RTF (rtfreporter) into the study's
#' `output_path` -- one report's failure not stopping the others.  It writes
#' `ai/status.md` (each report's status, its RTF or why none, each
#' request's next step).  Nothing is sent anywhere.
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
#'   or `NA`), invisibly.
#' @seealso [tfl_ai_prepare()], [tfl_ai_outputs()].
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
    outs <- if (!inherits(st, "error")) {
      tryCatch(suppressWarnings(tfl_read_outputs(".", skip_bad = TRUE)), error = function(e) e)
    }
    if (inherits(st, "error") || inherits(outs, "error")) {
      e <- if (inherits(st, "error")) st else outs
      r$rtf_problem[todo] <- paste("the study could not be read:", .ai_one_line(conditionMessage(e)))
    } else {
      # (the study's rounding, as the programs' setup sets it)
      old_r <- .study_rounding_option(st)
      on.exit(options(old_r), add = TRUE)
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
  res <- .ai_run_next(res)
  .ai_write_text(.ai_status_text(res), file.path(study_dir, "ai", "status.md"))
  message(sprintf("%s; see ai/status.md.", .ai_count_text(res)))
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
  data <- data %||% character()
  fi <- file.info(data[file.exists(data)])
  adam <- if (nrow(fi)) paste(rownames(fi), fi$size, format(fi$mtime, "%Y%m%d%H%M%S"), collapse = ";") else ""
  pk <- function(p) if (requireNamespace(p, quietly = TRUE)) format(utils::packageVersion(p)) else "-"
  # (the packages whose version changes what a program makes: a new one
  # builds every report again)
  paste(md5(c("study.yml", file.path("spec", c("data.yml", "defaults.yml")))), md5(tpl), adam,
        "tflspec", pk("tflspec"), "rtfreporter", pk("rtfreporter"), "cards", pk("cards"),
        "cardx", pk("cardx"), "ggplot2", pk("ggplot2"), sep = "|")
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
      "The drafts passed their checks, but a report could not be built. Fix its draft",
      sprintf("(ai/spec/outputs/<id>.yml) and run `Rscript run.R %s`.", q), "",
      "## Could not be built", "",
      sprintf("- %s: %s", b$output_id, b$rtf_problem)), .ai_build_repair_file(dir, q))
    # (counted as a repair of the request's first draft, for tfl_ai_compare())
    a <- file.path(dir, "ai", "spec", "outputs", paste0(b$output_id[1L], ".yml"))
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

# The run's own next steps, over the collection's: a report that passed its
# checks and was not built in this run, or could not be built (a package
# missing, or a build error).  The text, and its kind and values (as
# tfl_ai_collect_answers()'s `next_kind` / `next_args`).  `res` with them.
.ai_run_next <- function(res) {
  r <- res$reports
  nxt <- res$next_steps
  nk <- res$next_kind %||% stats::setNames(rep(NA_character_, length(nxt)), names(nxt))
  na <- res$next_args %||% stats::setNames(vector("list", length(nxt)), names(nxt))
  bad <- r[!is.na(r$rtf_problem), , drop = FALSE]
  # passed its checks, never built (a run of other requests only)
  wait <- r[r$status %in% c("ok", "warning") & is.na(r$rtf) & is.na(r$rtf_problem) &
              is.na(r$built), , drop = FALSE]
  for (w in unique(wait$request)) {
    ids <- wait$output_id[wait$request == w]
    nxt[w] <- sprintf("the answer passed its checks; %s not built yet: run `Rscript run.R %s`",
                      paste(ids, collapse = ", "), w)
    nk[w] <- "not_built"
    na[[w]] <- list(reports = ids)
  }
  pkg <- .ai_missing_package(bad$rtf_problem)
  for (i in seq_len(nrow(bad))) {
    w <- bad$request[i]
    if (!is.na(pkg[i])) {
      nxt[w] <- sprintf(paste0("the answer passed its checks but %s could not be built: R has no ",
                               "package %s on this PC -- not the answer's fault, do not change it. ",
                               "Ask the person to install it (install.packages(\"%s\")) in a ",
                               "question, go on with the next request, and run `Rscript run.R %s` ",
                               "again once it is installed"),
                        bad$output_id[i], pkg[i], pkg[i], w)
      nk[w] <- "no_package"
      na[[w]] <- list(report = bad$output_id[i], package = pkg[i])
    } else {
      nxt[w] <- sprintf(paste0("the answer passed its checks but %s could not be built (%s): ",
                               "fix the answer (ai/repairs/%s-build.md) and run again"),
                        bad$output_id[i], bad$rtf_problem[i], w)
      nk[w] <- "build_failed"
      na[[w]] <- list(report = bad$output_id[i], problem = bad$rtf_problem[i],
                      repair = sprintf("ai/repairs/%s-build.md", w))
    }
  }
  res$next_steps <- nxt
  res$next_kind <- nk
  res$next_args <- na
  res
}

.ai_status_text <- function(res) {
  r <- res$reports
  cell <- function(x) ifelse(is.na(x), "", gsub("|", "/", x, fixed = TRUE))
  nxt <- res$next_steps
  c(sprintf("# Status (written by run.R, %s)", format(Sys.time(), "%Y-%m-%d %H:%M")),
    "",
    paste0(.ai_count_text(res), "."),
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
    if (length(res$stray)) c("", sprintf(paste0("Not read (no request asks for them): %s -- ",
                                                "a draft's name is its report's id."),
                                         paste(res$stray, collapse = ", "))))
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
    # its batches (a TOC_<batch> sheet, a column of marks): `sets`, the
    # key the study's planner sheet reads and writes (" | " between them)
    if (!is.null(t$batches) && !is.na(t$batches[i])) {
      e$sets <- as.list(strsplit(t$batches[i], " | ", fixed = TRUE)[[1L]])
    }
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

# The agent's one message: the study folder's name (its paths are from
# it); `templates`: a template where one fits; `language`: the chat's.
# The requests are not named: the agent does every one of
# ai/requests/index.md in order, and "continue" goes on from the first
# with a draft missing.
.ai_agent_prompt <- function(study, templates = FALSE, language = "en") {
  paste0("The paths are from the study folder ", study,
    " (the one holding ai/; with a folder above it open, put the study folder before each). ",
    "Read ai/AGENTS.md and follow it. Do every request of ai/requests/index.md, top to bottom; ",
    "skip a request whose reports all have their draft (ai/spec/outputs/<id>.yml). For each: ",
    "read ai/decisions.md and the request; ask me here, in ", .ai_language_name(language),
    ", what defines its reports and is not settled (the analysis set, the parameter, the ",
    "treatment variable, a denominator ...), numbered, with the choices and your default -- ",
    "then wait for my answers. Record them in ai/decisions.md. Then write each report's ",
    "skeleton as ai/spec/outputs/<id>.yml",
    if (isTRUE(templates)) " (a template where one fits, ai/AGENTS.md)" else "",
    ", nothing of the look, and mark what you could not settle under ai: confirm; go on to ",
    "the next request. Read only the request and the files it names; run nothing, install ",
    "nothing. When I say \"continue\", go on from the first request with a draft missing. ",
    "When every request is done, stop and tell me the files written, the decisions added ",
    "and what is to confirm.")
}
