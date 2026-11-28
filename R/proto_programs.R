# PROTOTYPE (c/proto-programs, not adopted): a report's full program and
# its ARD program, written from the same lines, and the run that picks one.
#
#   full program  programs/tfl/<program>.R: data + analysis + content + page;
#                 the content reads the `ard` made in the same process,
#                 saved to the report's own file output/ard/report/<id>.rds
#                 (what made it recorded) as soon as it is made: a later
#                 content or page error leaves it saved;
#   ARD program   programs/ard/<id>.R: data + the whole analysis step (the
#                 same lines), saved the same way; no content, no page.
#
# The study ARD is collect_ard() of the reports' files (canon 11-16): no
# report picks its rows out of the study ARD.  A report reading another
# report's ARD (a figure's `ard_source` table:<id>) reads that report's
# file: an explicit dependency, run first.
#
# Today's programs are not changed: these branches act only when the
# context carries `onepass` (tfl_onepass_program()); tfl_report_program()
# and tfl_ard_program() never set it.

# the lines that open and close the analysis step: only `ard` leaves it
.op_begin <- "ard <- local({  # the analysis step: only `ard` leaves it"
.op_end <- "})  # the end of the analysis step"

# the lines outside the analysis steps
.op_outside_analysis <- function(lines) {
  if (!length(lines)) return(lines)
  open <- cumsum(lines == .op_begin) - cumsum(c(0L, utils::head(lines == .op_end, -1L)))
  lines[open == 0L]
}

# does the report have analyses in the ARD definition?
.op_has_analyses <- function(ctx) {
  sp <- ctx$onepass$spec
  !is.null(sp) && ctx$report$id %in% sp$analyses$output_id
}

# the fingerprint of the report's ARD definition ("NA" as R: none)
.op_hash_code <- function(ctx) {
  if (!.op_has_analyses(ctx)) return("NA_character_")
  op <- ctx$onepass
  .rp_r_string(tfl_ard_spec_hash(op$spec, ctx$report$id, op$dir, op$codelists))
}

# The analysis step: the ARD program's lines (tfl_ard_code(part = "body"),
# the data it reads and the analyses), in local(), leaving `ard`, then
# saved
.op_analysis_lines <- function(ctx) {
  if (!.op_has_analyses(ctx)) return(NULL)
  op <- ctx$onepass
  body <- .rp_ard_code(ctx, op$spec, output_id = ctx$report$id, part = "body", save = FALSE,
                       dir = op$dir, codelists = op$codelists)
  c("# ---- analysis: this report's ARD, made here (its ARD program has the same lines)",
    .op_begin,
    .rp_split_lines(as.character(body)),
    "ard",
    .op_end,
    .op_save_line(ctx, "ard"))
}

# A table's ARD code in its full program: its analysis step, then the ARD
# without its keys, as the table reads it; NULL: not the prototype, or no
# analyses (the company's template, as today)
.op_table_ard_code <- function(ctx) {
  if (is.null(ctx$onepass) || !.op_has_analyses(ctx)) return(NULL)
  paste(c(.op_analysis_lines(ctx), "",
          "# the table reads it without its keys",
          .rp_drop_attached("ard <- dplyr::select(ard, -c(output_id, analysis_id, population_id))")),
        collapse = "\n")
}

# A figure's ARD in its full program: its own made here, a table's read from
# that table's file
.op_fig_ard_lines <- function(ctx, src) {
  q <- .rp_r_string(src$id)
  if (identical(src$kind, "own")) {
    if (!.op_has_analyses(ctx)) {
      return(c(sprintf("stop(%s)", .rp_r_string(paste0(src$id, " has no analyses in the ARD definition."))), ""))
    }
    return(c(.op_analysis_lines(ctx),
             "# (the definition it was made from: the report records it, #293)",
             paste("ard_built <-", .op_hash_code(ctx)), ""))
  }
  c(sprintf("# the ARD of %s (its own file, made by its program: run first)", src$id),
    sprintf("ard <- read_report_ard(%s)", q),
    "# (the definition it was made from: the report records it, #293)",
    sprintf("ard_built <- report_ard_definition(%s)", q),
    "")
}

# after a step of the report's own code: the `ard` it leaves joins the
# study ARD (the rule of 11-28: only the object named `ard`)
.op_keep_ard_lines <- function(ctx) {
  c("# (an `ard` your code leaves joins the study ARD)",
    "if (exists(\"ard\", inherits = FALSE) && is.data.frame(ard)) {",
    paste0("  ", .op_save_line(ctx, "ard")),
    "}")
}

# what a prototype program sources after the study's setup (an ARD program:
# the ARD's setup always, the study's setup and data come with it), then
# its old ARD file removed: an ARD the run does not make again is not left
# behind
.op_source_lines <- function(ctx) {
  lay <- ctx$layout
  c(if (.op_has_analyses(ctx) || identical(ctx$onepass$kind, "ard")) c(
      "# the ARD's statistics and formats (what every ARD program starts with)",
      sprintf("source(%s)", .rp_r_string(file.path(lay[["programs_ard"]], .rp_ard_setup_file)))),
    "# the reports' ARD files: save_report_ard(), read_report_ard(), collect_ard()",
    sprintf("source(%s)", .rp_r_string(.op_helpers_path(lay))),
    "# (this report's ARD file, from an earlier run, removed: the run makes it again)",
    sprintf("save_report_ard(NULL, %s)", .rp_r_string(ctx$report$id)))
}

.op_helpers_path <- function(layout) {
  file.path(dirname(layout[["programs_ard"]]), "onepass_helpers.R")
}

# the line that saves `obj` as the report's ARD, into its own file
# output/ard/report/<id>.rds, with what made it (the study ARD is
# collect_ard() of these files): right after the step that makes it, so a
# later step's error leaves it saved
.op_save_line <- function(ctx, obj) {
  sprintf("save_report_ard(%s, report_id, definition = %s, program = %s)",
          obj, .op_hash_code(ctx), .rp_r_string(ctx$onepass$program))
}

# The ARD program: the same data and analysis lines as the full program, no
# content, no page; NULL when the report makes no ARD (no analyses, no step
# of its own code before its content)
.op_ard_program <- function(ctx, date, generator) {
  r <- ctx$report
  lay <- ctx$layout
  own <- .rp_is_code_step(r, "data")
  if (!own && !.op_has_analyses(ctx)) return(NULL)
  setup0 <- ctx$shared$setup
  steps <- if (own) {
    c(.rp_section("data and analysis: your code"),
      .rp_code_inputs(ctx),
      "# your code: the whole step runs (only `ard` joins the study ARD)",
      .rp_code_block(r$steps$data),
      .op_keep_ard_lines(ctx))
  } else .op_analysis_lines(ctx)
  # (a study with no ARD definition: no analyses to name)
  a <- ctx$onepass$spec$analyses
  a <- if (is.data.frame(a)) a[a$output_id %in% r$id, , drop = FALSE] else data.frame()
  labels <- if (nrow(a)) ifelse(is.na(a$label), a$analysis_id,
                                paste0(a$analysis_id, " (", a$label, ")"))
  c(.program_banner(
      paste("Program    :", ctx$onepass$program),
      paste0("Output     : ", r$id, " -> output/ard/report/",
             sub("\\.R$", ".rds", .ard_program_name(r$id))),
      if (length(labels)) strwrap(paste(labels, collapse = ", "), width = 74,
                                  initial = "Analyses   : ", prefix = strrep(" ", 13)),
      .setup_generated(generator, date),
      paste0("The data and analysis steps of ", file.path(lay[["programs_tfl"]], r$program),
             ", the same lines; no content, no page.")),
    "",
    .rp_study_guard,
    .op_source_lines(ctx),
    "",
    .rp_report_id_line(r$id),
    "",
    if (!is.na(setup0)) c(.rp_code_block(setup0), ""),
    steps,
    .rp_run_end)
}

#' PROTOTYPE: a report's full program or its ARD program
#'
#' Not adopted (`c/proto-programs`): a prototype measured against today's
#' report and ARD programs.  `kind = "full"`: the report's whole program --
#' data, analysis, content and page in one process; its content reads the
#' `ard` made in it, and it ends by saving that `ard` to the report's own
#' file `output/ard/report/<id>.rds` as soon as it is made.  `kind = "ard"`: its data and
#' analysis steps only, the same lines, saved the same way.  Both are
#' written by one maker ([tfl_report_program()]'s, with the analysis lines
#' of [tfl_ard_code()]), so the two cannot differ.  The study ARD is
#' `collect_ard()` of the reports' files ([tfl_onepass_run()]).
#'
#' @inheritParams tfl_report_program
#' @param kind `"full"` or `"ard"`.
#' @param spec The study's ARD definition ([tfl_ard_spec()]), as
#'   [tfl_ard_program()] takes it.
#' @param codelists,dir As [tfl_ard_program()] takes them.
#' @return The program, one element per line; `kind = "ard"`: `NULL` when
#'   the report makes no ARD.
#' @keywords internal
#' @export
tfl_onepass_program <- function(report, kind = c("full", "ard"), spec = NULL,
                                codelists = NULL, dir = ".", study = NULL,
                                shared = list(), standards = list(),
                                attached = character(), study_code = TRUE,
                                series_dir = NULL, layout = tfl_study_layout(),
                                date = Sys.Date(),
                                generator = paste("tflspec", utils::packageVersion("tflspec"))) {
  kind <- match.arg(kind)
  force(report)
  force(spec)
  force(codelists)
  force(study)
  force(shared)
  force(standards)
  force(attached)
  force(series_dir)
  force(date)
  force(generator)
  fn <- "tfl_onepass_program()"
  if (!is.null(spec) && !inherits(spec, "tfl_ard_spec")) {
    stop(fn, ": `spec` is the study's ARD definition (tfl_ard_spec()).", call. = FALSE)
  }
  layout <- .rp_check_common(fn, standards, layout,
                             c("programs_ard", "programs_tfl", "spec", "adam", "ard_import"))
  report$steps <- report$steps %||% list(data = NA_character_, content = NA_character_)
  report$process <- report$process %||% NA_character_
  report$ard_object <- report$ard_object %||% "ard"
  report$ard_source <- report$ard_source %||% list(kind = "none")
  shared$setup <- shared$setup %||% NA_character_
  shared$report_setup <- shared$report_setup %||% FALSE
  shared$study_ard <- shared$study_ard %||% "output/ard/ard.rds"
  program <- if (kind == "full") file.path(layout[["programs_tfl"]], report$program) else
    file.path(layout[["programs_ard"]], .ard_program_name(report$id))
  ctx <- list(report = report, study = study, shared = shared, standards = standards,
              layout = layout,
              onepass = list(kind = kind, spec = spec, codelists = codelists, dir = dir,
                             program = program))
  .with_code_context(switch(kind,
    full = .rp_program(ctx, date, generator),
    ard = .op_ard_program(ctx, date, generator)),
    .rp_context(attached, study_code, series_dir, layout))
}

#' PROTOTYPE: run a study's full or ARD programs
#'
#' Not adopted (`c/proto-programs`).  Runs the programs
#' [tfl_onepass_program()] wrote, each in a child R from the study folder,
#' as `run` says, then makes the study ARD with `collect_ard()`:
#' * `"full"`: each report's full program (data, analysis, content, page);
#' * `"ard"`: each report's ARD program (data and analysis);
#' * `"both"`: both kinds of program were written, the full ones run (never
#'   both: the ARD would be computed twice).
#'
#' A report reading another report's ARD runs after it (`needs`); one whose
#' producer failed in the run is not run.  Nothing else orders the run.
#'
#' @param dir The study folder.
#' @param programs A data frame: `output_id`, `full` (the full program,
#'   relative to `dir`) and `ard` (the ARD program, `NA` for none).
#' @param run `"full"`, `"ard"` or `"both"`.
#' @param needs A named list: an output id -> the output ids whose ARD it
#'   reads.
#' @return A data frame, a row a program run: `output_id`, `program`,
#'   `status` (the exit status; `NA`: not run), `seconds`, `peak_kb` (the
#'   child's peak memory where `/usr/bin/time` gives it, else `NA`), and the
#'   attribute `collect`, the collector's row.
#' @keywords internal
#' @export
tfl_onepass_run <- function(dir, programs, run = c("full", "ard", "both"), needs = list()) {
  run <- match.arg(run)
  col <- if (run == "ard") "ard" else "full"
  todo <- programs[!is.na(programs[[col]]), , drop = FALSE]
  ids <- .op_run_order(todo$output_id, needs)
  todo <- todo[match(ids, todo$output_id), , drop = FALSE]
  failed <- character()
  rows <- list()
  for (i in seq_len(nrow(todo))) {
    id <- todo$output_id[i]
    p <- todo[[col]][i]
    if (length(intersect(needs[[id]], failed))) {
      rows[[i]] <- data.frame(output_id = id, program = p, status = NA_integer_,
                              seconds = 0, peak_kb = NA_real_, stringsAsFactors = FALSE)
      failed <- c(failed, id)
      next
    }
    r <- .op_rscript(dir, p)
    if (r$status != 0L) failed <- c(failed, id)
    rows[[i]] <- data.frame(output_id = id, program = p, status = r$status,
                            seconds = r$seconds, peak_kb = r$kb, stringsAsFactors = FALSE)
  }
  out <- do.call(rbind, rows)
  f <- tempfile("collect", fileext = ".R")
  on.exit(unlink(f), add = TRUE)
  writeLines(c(sprintf("source(%s)", .rp_r_string(.op_helpers_path(tfl_study_layout()))),
               "collect_ard()"), f)
  attr(out, "collect") <- .op_rscript(dir, normalizePath(f, "/"))[c("status", "seconds", "kb")]
  out
}

# the outputs in the order they run: each after the outputs it reads (in
# the order given otherwise); a read of an output not in the run is left to
# its program (it stops, saying which program to run)
.op_run_order <- function(ids, needs) {
  done <- character()
  visit <- function(id, path) {
    if (id %in% done) return()
    if (id %in% path) stop("The outputs read each other's ARD: ",
                           paste(c(path, id), collapse = " -> "), call. = FALSE)
    for (n in intersect(needs[[id]], ids)) visit(n, c(path, id))
    done <<- c(done, id)
  }
  for (id in ids) visit(id, character())
  done
}

# Rscript <file> from `dir`: its exit status, seconds and peak memory (KB)
.op_rscript <- function(dir, file) {
  old <- setwd(dir)
  on.exit(setwd(old), add = TRUE)
  rscript <- file.path(R.home("bin"), "Rscript")
  tf <- tempfile()
  on.exit(unlink(tf), add = TRUE)
  timed <- file.exists("/usr/bin/time")
  t0 <- Sys.time()
  out <- suppressWarnings(if (timed) {
    system2("/usr/bin/time", c("-f", "%M", "-o", tf, rscript, file), stdout = TRUE, stderr = TRUE)
  } else system2(rscript, file, stdout = TRUE, stderr = TRUE))
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  st <- attr(out, "status")
  kb <- if (timed && file.exists(tf)) suppressWarnings(as.numeric(utils::tail(readLines(tf), 1L))) else NA_real_
  list(status = if (is.null(st)) 0L else as.integer(st), seconds = secs,
       kb = if (length(kb)) kb else NA_real_, out = out)
}
