# ---- resolve and run a definition; list and compare the runs (G4 slice B1) --

#' What a run definition makes, and how to start it
#'
#' @description
#' * `tfl_resolve_run()` works out a definition from the study's SPEC: `all`
#'   expanded, the ids in the order of the report list, the ARD each report
#'   reads ([tfl_ard_reads()]), the ARD to build, the options with their
#'   defaults filled, and the problems.  It fails closed: every report whose
#'   ARD read cannot be worked out (whatever the reason `tfl_ard_reads()`
#'   gives) is a problem naming the report and the reason; a report that
#'   reads the ARD of one the definition does not select is a problem naming
#'   both ids (select that report too: its RTF is made as well); and every
#'   row of [tfl_check_runs()] for the definition is a problem.
#' * `tfl_run_command()` is the command that starts an official run, from the
#'   study folder: the Rscript, `programs/autoexec_all.R` and `--run=<name>`;
#'   the attribute `"wd"` is the folder to start it in.  A different choice
#'   of data or options is a different saved definition.
#' * `tfl_run_study()` is the one official entry from R.  It first checks the
#'   programs ([tfl_programs_problem()]), then runs the command in the
#'   foreground and returns the batch folder.  A run the runner refuses (a
#'   definition with a problem, an unresolved read, programs older than the
#'   spec) stops with the runner's own text, the same text a hand-run
#'   `Rscript programs/autoexec_all.R --run=<name>` prints.
#' * `tfl_programs_problem()` says whether `programs/batch.R` can start a run:
#'   `""`, or why not (no programs; programs made before run definitions,
#'   which hold `sets` and no stamp: regenerate the programs).  Whether the
#'   programs are older than the spec is the runner's check ([tfl_spec_stamp()]).
#'
#' @param study_dir The study folder (the one with study.yml).
#' @param name The definition's name, exactly as written.
#' @return `tfl_resolve_run()`: a list: `name`, `reports` (the ids, in run
#'   order), `ard` (the ids whose ARD the run builds), `reads` (the rows of
#'   [tfl_ard_reads()] of these reports), `mode`, `copy_data`, and `problems`
#'   (a data frame as [tfl_check_runs()]'s; no row when the run can start).
#'   `tfl_run_command()`: a character vector.  `tfl_run_study()`: the batch
#'   folder's path, invisibly.  `tfl_programs_problem()`: one string.
#' @examples
#' tfl_run_command(tempdir(), "Topline")
#' @name tfl_run_study
NULL

#' @rdname tfl_run_study
#' @export
tfl_resolve_run <- function(study_dir, name) {
  runs <- tfl_read_runs(study_dir)
  defs <- unclass(runs)
  attributes(defs) <- NULL
  nm <- vapply(defs, function(d) {
    if (is.character(d$name) && length(d$name) == 1L) d$name else NA_character_
  }, "")
  i <- match(name, nm)
  if (is.na(i)) {
    stop("tfl_resolve_run(): the study has no run definition named '", name, "'",
         if (any(!is.na(nm))) paste0(" (it has: ", paste(nm[!is.na(nm)], collapse = ", "), ")"),
         ".", call. = FALSE)
  }
  spec <- tfl_read_study_spec(study_dir)
  chk <- tfl_check_runs(runs, spec)
  problems <- chk[chk$definition == name | (chk$definition == "" & chk$level == "error"),
                  , drop = FALSE]
  d <- defs[[i]]
  all_ids <- .runs_report_ids(spec)
  ids <- if (identical(d$reports, "all")) all_ids else as.character(d$reports)
  ids <- unique(ids[!is.na(ids)])
  pos <- match(ids, all_ids)
  ids <- ids[order(is.na(pos), pos, seq_along(ids))]
  add <- function(field, problem) {
    problems <<- rbind(problems, .runs_prob(name, field, problem))
  }
  rd <- tryCatch(tfl_ard_reads(spec), error = function(e) e)
  if (inherits(rd, "error")) {
    add("reports", paste("what each report reads cannot be worked out:",
                         .runs_one_line(conditionMessage(rd))))
    rd <- data.frame(report = character(), reads = character(), kind = character(),
                     reason = character(), stringsAsFactors = FALSE)
  }
  rd <- rd[rd$report %in% ids, , drop = FALSE]
  for (k in seq_len(nrow(rd))) {
    if (is.na(rd$reads[[k]])) {
      add("reports", paste0(rd$report[[k]], ": ", rd$reason[[k]]))
    } else if (!rd$reads[[k]] %in% ids) {
      add("reports", paste0(rd$report[[k]], " reads the ARD of ", rd$reads[[k]],
                            ", which the definition does not select: select ",
                            rd$reads[[k]], " too (its report is made as well)"))
    }
  }
  rownames(problems) <- NULL
  list(name = name, reports = ids,
       ard = ids[ids %in% rd$reads[!is.na(rd$reads)]],
       reads = rd, mode = d$mode %||% "all", copy_data = d$copy_data %||% FALSE,
       problems = problems)
}

#' @rdname tfl_run_study
#' @export
tfl_run_command <- function(study_dir, name) {
  if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name)) {
    stop("tfl_run_command(): `name` is the definition's name, one text.", call. = FALSE)
  }
  structure(c(file.path(R.home("bin"), "Rscript"),
              file.path("programs", "autoexec_all.R"),
              paste0("--run=", name)),
            wd = normalizePath(study_dir, winslash = "/", mustWork = FALSE))
}

#' @rdname tfl_run_study
#' @export
tfl_programs_problem <- function(study_dir) {
  regen <- "save the study (tflplanner) or regenerate the programs"
  if (!file.exists(file.path(study_dir, "programs", "autoexec_all.R"))) {
    return(paste0("programs/autoexec_all.R is missing: ", regen, "."))
  }
  f <- file.path(study_dir, "programs", "batch.R")
  if (!file.exists(f)) return(paste0("programs/batch.R is missing: ", regen, "."))
  code <- readLines(f, warn = FALSE, encoding = "UTF-8")
  # (programs made before the run definitions: `sets`, no stamp)
  if (any(startsWith(code, ".batch_sets <- ")) || !any(startsWith(code, ".batch_stamp <- ")) ||
      !any(startsWith(code, ".batch_runs <- "))) {
    return(paste0("programs/batch.R was made before run definitions (it has `sets` ",
                  "and no stamp): ", regen, "."))
  }
  ""
}

#' @rdname tfl_run_study
#' @export
tfl_run_study <- function(study_dir, name) {
  p <- tfl_programs_problem(study_dir)
  if (nzchar(p)) stop("tfl_run_study(): ", p, call. = FALSE)
  cmd <- tfl_run_command(study_dir, name)
  old <- setwd(study_dir)
  on.exit(setwd(old))
  out <- suppressWarnings(system2(cmd[[1L]], shQuote(cmd[-1L]), stdout = TRUE,
                                  stderr = TRUE))
  status <- attr(out, "status")
  status <- if (is.null(status)) 0L else status
  line <- grep("^Batch folder: ", out, value = TRUE)
  if (!length(line)) {
    stop("tfl_run_study(): the run was not started (status ", status, ").\n",
         paste(out, collapse = "\n"), call. = FALSE)
  }
  cat(out, sep = "\n")
  dir <- trimws(sub("^Batch folder:", "", line[[1L]]))
  dir <- normalizePath(dir, winslash = "/", mustWork = FALSE)
  if (status != 0L) {
    warning("tfl_run_study(): the run ended with status ", status,
            ": a program had an error (see ", dir, ").", call. = FALSE)
  }
  invisible(dir)
}

# the runner's functions, from its file (base R, one copy): in an
# environment of their own
.runs_runner_env <- function() {
  f <- tfl_runner_file()
  if (!nzchar(f)) stop("the runner file is not installed.", call. = FALSE)
  e <- new.env(parent = globalenv())
  eval(parse(text = readLines(f, warn = FALSE, encoding = "UTF-8"), keep.source = FALSE), e)
  e
}

#' The stamp of a study's spec
#'
#' One md5 of what the study's programs were made from: `study.yml` (its
#' `updated:` line, the time of a save, left out), and every
#' file of the spec folder as the runner's copy takes it (the run definitions
#' file `spec/runs.yml` included; no dot names, no editor files).  Bytes are
#' hashed, so Windows and Linux agree.  The runner's `batch.R` holds the stamp
#' of the spec it was made from (the manifest's `spec_md5`), and a run is
#' refused when the spec's stamp is another.  The function is the runner's own
#' (`.batch_spec_md5()` of [tfl_runner_file()]), evaluated from that file: there
#' is no second copy.
#'
#' @param study_dir The study folder.
#' @param spec The spec folder's name in the study (the manifest's `spec`).
#' @return One string; `"none"` when the study has neither file.
#' @examples
#' d <- tempfile()
#' dir.create(file.path(d, "spec"), recursive = TRUE)
#' writeLines("study_id: S-1", file.path(d, "study.yml"))
#' tfl_spec_stamp(d)
#' @export
tfl_spec_stamp <- function(study_dir, spec = "spec") {
  e <- .runs_runner_env()
  assign(".batch_spec", spec, envir = e)
  old <- setwd(study_dir)
  on.exit(setwd(old))
  e$.batch_spec_md5()
}

# ---- the folders of the runs ------------------------------------------------

#' List and compare a study's runs
#'
#' `tfl_list_runs()` lists every folder of the study's `runs/`: the finished
#' ones, the ones that failed, the previews, the ones with no end record, and
#' the folders of older runners.  It reads each through the same reader as
#' [tfl_run_index()].
#'
#' A folder with no `Finished` line is `"running or stopped"`.  A folder made
#' by an older runner has its definition in `Batch set :` and its mode in the
#' word of its name (`20261124_093000_all`); a newer one has `Run :` and
#' `Mode :` in `batch.txt`, written when the run starts.
#'
#' `tfl_compare_runs()` says what changed between two run folders: their
#' inputs -- the data, by the md5 `data_manifest.csv` records, and the
#' programs, by the md5 of the files of `programs/` -- and their results
#' (`output/`).  A result is `"identical"` (the same bytes), `"same but the
#' time"` (a report or a status file the same but the date and time it prints
#' or records), `"same object"` (an `.rds` read the same), `"different"`,
#' `"not made"` (the earlier run made it, this one did not) or `"new"`.  An
#' input is `"added"`, `"removed"` or `"changed"` (one the same is not listed);
#' `"not kept"` when either run did not keep that input.
#'
#' @param study_dir The study folder (the one with study.yml).
#' @param a,b The earlier and the later run folder (paths).
#' @return `tfl_list_runs()`: a data frame, the newest first: `run` (the
#'   folder's name), `definition` (`""` when none), `mode` (`"all"`, `"ard"`,
#'   `"report"`; `NA` when unknown), `kind`, `what` (as `mode`: the word the
#'   older table read), `started` and `finished` (text, `NA` when unknown),
#'   `verified`, `rerun` (logical), `programs` (rows of `run.csv`), `errors`
#'   (programs with an error), `warnings` (warnings in all), `state`
#'   (`"done"`, `"failed"` or `"running or stopped"`), `data` (the data were
#'   kept), `excluded`, `path`.
#'   `tfl_compare_runs()`: a data frame `kind` (`"data"`, `"code"`, `"output"`),
#'   `file`, `result`; with `attr(, "batches")` (`against`, `batch`) and
#'   `attr(, "changed")` (a list: the `inputs` and the `outputs` that changed).
#' @examples
#' d <- tempfile()
#' dir.create(d)
#' tfl_list_runs(d)
#' @export
tfl_list_runs <- function(study_dir) {
  root <- .tfl_runs_dir(study_dir)
  dirs <- if (dir.exists(root)) list.dirs(root, full.names = FALSE, recursive = FALSE) else character()
  rows <- lapply(dirs, function(d) .run_list_one(file.path(root, d)))
  out <- do.call(rbind, c(list(data.frame(
    run = character(), definition = character(), mode = character(), kind = character(),
    what = character(), started = character(), finished = character(),
    verified = logical(), rerun = logical(), programs = integer(),
    errors = integer(), warnings = integer(), state = character(),
    data = logical(), excluded = character(), path = character(),
    stringsAsFactors = FALSE)), rows))
  out <- out[order(is.na(out$started), out$started, out$run, decreasing = c(FALSE, TRUE, TRUE),
                   method = "radix"), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# one run folder's row
.run_list_one <- function(path) {
  fo <- .run_folder(path)
  run <- fo$run
  lines <- fo$lines
  field <- function(key) .run_field(lines, key)
  none <- function(x) if (is.na(x) || identical(x, "none") || !nzchar(x)) "" else x
  # the stamp in a folder's name: 20261124_093000_all, batch_20261124-093000_X
  m <- regmatches(run, regexec("^(?:batch_|preview_)?([0-9]{8})[_-]([0-9]{6})",
                               run, perl = TRUE))[[1L]]
  from_name <- if (length(m)) paste0(
    substring(m[2L], 1L, 4L), "-", substring(m[2L], 5L, 6L), "-", substring(m[2L], 7L, 8L),
    " ", substring(m[3L], 1L, 2L), ":", substring(m[3L], 3L, 4L), ":", substring(m[3L], 5L, 6L)
  ) else NA_character_
  started <- field("Started")
  if (is.na(started)) started <- from_name
  fin <- field("Finished")
  definition <- field("Run")
  if (is.na(definition)) definition <- field("Batch set")
  mode <- field("Mode")
  if (is.na(mode)) {
    w <- regmatches(run, regexec("^[0-9]{8}_[0-9]{6}_(ard|report|all)(?:_|$)", run,
                                 perl = TRUE))[[1L]]
    mode <- if (length(w)) w[[2L]] else NA_character_
  }
  kind <- field("Kind")
  if (is.na(kind)) kind <- if (startsWith(run, "preview_")) "preview" else "run"
  r <- fo$csv
  errs <- if (is.null(r) || !"status" %in% names(r)) NA_integer_ else sum(r$status != "OK")
  warns <- if (is.null(r) || !"warnings" %in% names(r)) NA_integer_ else
    sum(suppressWarnings(as.integer(r$warnings)), na.rm = TRUE)
  ex <- none(field("Excluded"))
  state <- if (is.na(fin) || is.null(r)) "running or stopped" else
    if (!is.na(errs) && errs > 0L) "failed" else "done"
  data.frame(
    run = run, definition = none(definition), mode = mode, kind = kind, what = mode,
    started = started, finished = fin,
    verified = identical(field("Verified"), "yes"),
    rerun = any(grepl("^Re-run\\s*:", lines)),
    programs = if (is.null(r)) NA_integer_ else nrow(r),
    errors = errs, warnings = warns, state = state,
    data = dir.exists(file.path(path, "data")), excluded = ex,
    path = normalizePath(path, winslash = "/", mustWork = FALSE),
    stringsAsFactors = FALSE)
}

#' @rdname tfl_list_runs
#' @export
tfl_compare_runs <- function(a, b) {
  for (p in c(a, b)) {
    if (!dir.exists(p)) stop("tfl_compare_runs(): no run folder ", sQuote(p), ".", call. = FALSE)
  }
  row <- function(kind, file, result) {
    data.frame(kind = rep(kind, length(file)), file = file, result = result,
               stringsAsFactors = FALSE)
  }
  # an input: by md5, where both runs kept it
  inputs <- function(kind, md5_of) {
    x <- md5_of(a)
    y <- md5_of(b)
    if (is.null(x) || is.null(y)) return(row(kind, "", "not kept"))
    f <- sort(union(names(x), names(y)))
    res <- vapply(f, function(k) {
      if (!k %in% names(x)) "added" else if (!k %in% names(y)) "removed" else
        if (identical(x[[k]], y[[k]])) "same" else "changed"
    }, "")
    row(kind, f, unname(res))[res != "same", , drop = FALSE]
  }
  data_md5 <- function(dir) {
    f <- file.path(dir, "data_manifest.csv")
    if (!file.exists(f)) return(NULL)
    m <- utils::read.csv(f, colClasses = "character")
    stats::setNames(m$md5, m$path)
  }
  code_md5 <- function(dir) {
    d <- file.path(dir, "programs")
    if (!dir.exists(d)) return(NULL)
    f <- list.files(d, recursive = TRUE)
    stats::setNames(unname(tools::md5sum(file.path(d, f))), f)
  }
  outs <- sort(union(list.files(file.path(a, "output"), recursive = TRUE),
                     list.files(file.path(b, "output"), recursive = TRUE)))
  out_res <- vapply(outs, function(f) .run_compare_file(file.path(a, "output", f),
                                                       file.path(b, "output", f)), "")
  out <- rbind(inputs("data", data_md5), inputs("code", code_md5),
               row("output", file.path("output", outs), unname(out_res)))
  rownames(out) <- NULL
  attr(out, "batches") <- c(against = basename(a), batch = basename(b))
  attr(out, "changed") <- list(
    inputs = out$file[out$kind != "output" & out$result != "not kept"],
    outputs = out$file[out$kind == "output" &
                         out$result %in% c("different", "not made", "new")])
  out
}

# One result of two runs: the same bytes; else the same but the date and
# time a report prints ({DATETIME}) or a status file records; else (an
# .rds) the same R object; else different
.run_compare_file <- function(x, y) {
  if (!file.exists(y)) return("not made")
  if (!file.exists(x)) return("new")
  if (identical(unname(tools::md5sum(x)), unname(tools::md5sum(y)))) return("identical")
  if (grepl("[.]rds$", x, ignore.case = TRUE)) {
    same <- tryCatch(identical(readRDS(x), readRDS(y)), error = function(e) FALSE)
    return(if (same) "same object" else "different")
  }
  if (grepl("[.](rtf|csv|txt)$", x, ignore.case = TRUE) &&
      identical(.run_untimed(x), .run_untimed(y))) return("same but the time")
  "different"
}

# a text file with its dates and times put aside: 2026-10-12 08:49:26,
# 12Oct2026  08:49 (a report's {DATETIME}), an RTF's \creatim ...
.run_untimed <- function(f) {
  x <- readLines(f, warn = FALSE, encoding = "UTF-8")
  for (p in c("[0-9]{4}-[0-9]{2}-[0-9]{2}[ T][0-9]{2}:[0-9]{2}(:[0-9]{2})?",
              "[0-9]{1,2}[A-Za-z]{3}[0-9]{4} +[0-9]{1,2}:[0-9]{2}(:[0-9]{2})?",
              "\\\\(creatim|revtim|printim)[^}]*")) {
    x <- gsub(p, "<time>", x, perl = TRUE, useBytes = TRUE)
  }
  x
}
