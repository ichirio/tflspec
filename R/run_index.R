#' The official runs of a study, and each report's latest
#'
#' `tfl_run_index()` reads every finished official run of a study (the batch
#' folders [tfl_runner_code()]'s runner makes): one row per program it ran.
#' `tfl_latest_run()` answers per report: the newest run that made it
#' without error.  A run that failed still answers for the reports it made;
#' a run of the ARD only answers for the ARD.
#'
#' A run counts when its `batch.txt` says it finished and names this study
#' (study.yml's `study_id`), has the line `Verified : yes` (the runner writes
#' it last, after `issues.csv`, so the run's end record is whole) and
#' `Kind : run`, and has no `Re-run :` line, and its `run.csv` has the
#' `output_id` column.  So a run still going, a run folder of another study
#' put under `runs/`, the runs of an older runner, a preview, and a batch
#' folder that was run again (even when it is copied back into `runs/`) do not
#' count.
#'
#' @param study_dir The study folder (the one with study.yml).
#' @param report_id Output ids.
#' @param part `"tfl"`: the run that made the report; `"ard"`: the run that
#'   made its ARD.
#' @return `tfl_run_index()`: a data frame, newest run first: `run` (the
#'   batch folder's name), `started`, `finished` (POSIXct), `part`,
#'   `program`, `output_id`, `status`, `md5` (the program's), `path` (the
#'   batch folder).
#'   `tfl_latest_run()`: a data frame with one row per `report_id`, in
#'   order: `output_id`, `run`, `path`, `finished`, `md5`; `NA` where no run
#'   made it.
#' @examples
#' d <- tempfile()
#' dir.create(d)
#' writeLines("study_id: S-1", file.path(d, "study.yml"))
#' tfl_latest_run(d, c("T-1", "T-2"))
#' @export
tfl_run_index <- function(study_dir) {
  root <- .tfl_runs_dir(study_dir)
  empty <- data.frame(
    run = character(), started = .as_time(character()),
    finished = .as_time(character()), part = character(),
    program = character(), output_id = character(), status = character(),
    md5 = character(), path = character(), stringsAsFactors = FALSE)
  sf <- file.path(study_dir, "study.yml")
  id <- if (file.exists(sf)) tryCatch(yaml::read_yaml(sf)$study_id,
                                      error = function(e) NULL)
  if (!length(id) || !dir.exists(root)) return(empty)
  id <- as.character(id)[[1L]]
  rows <- lapply(list.dirs(root, full.names = FALSE, recursive = FALSE),
                 function(d) .run_index_one(file.path(root, d), d, id))
  out <- do.call(rbind, c(list(empty), rows))
  out <- out[order(out$started, out$run, decreasing = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' @rdname tfl_run_index
#' @export
tfl_latest_run <- function(study_dir, report_id, part = c("tfl", "ard")) {
  part <- match.arg(part)
  report_id <- as.character(report_id)
  idx <- tfl_run_index(study_dir)
  idx <- idx[idx$part == part & idx$status == "OK" & nzchar(idx$output_id), ,
             drop = FALSE]
  # (newest first, so the first match is the latest)
  i <- match(report_id, idx$output_id)
  data.frame(output_id = report_id, run = idx$run[i], path = idx$path[i],
             finished = idx$finished[i], md5 = idx$md5[i],
             stringsAsFactors = FALSE)
}

# Where a study's runs are: the one place that says it (the study layout's
# key replaces it when it comes)
.tfl_runs_dir <- function(study_dir) file.path(study_dir, "runs")

.as_time <- function(x) as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S")

# One batch folder, read once: batch.txt's lines and run.csv.  The one
# reader under tfl_run_index() and tfl_list_runs() (no second batch.txt
# parser).
.run_folder <- function(path) {
  bt <- file.path(path, "batch.txt")
  csv <- file.path(path, "run.csv")
  list(path = path, run = basename(path),
       has_batch = file.exists(bt), has_csv = file.exists(csv),
       lines = if (file.exists(bt)) readLines(bt, warn = FALSE, encoding = "UTF-8") else character(),
       csv = if (file.exists(csv)) tryCatch(utils::read.csv(csv, colClasses = "character"),
                                            error = function(e) NULL))
}

# a batch.txt field ("Finished :"): its value, `NA` when absent
.run_field <- function(lines, key) {
  v <- grep(paste0("^", key, "\\s*:"), lines, value = TRUE)
  if (length(v)) trimws(sub("^[^:]*:", "", v[[1L]])) else NA_character_
}

# one batch folder's rows, or NULL when it does not count
.run_index_one <- function(path, run, study_id) {
  fo <- .run_folder(path)
  if (!fo$has_batch || !fo$has_csv) return(NULL)
  lines <- fo$lines
  field <- function(key) .run_field(lines, key)
  fin <- field("Finished")
  if (is.na(fin) || !identical(field("Study"), study_id)) return(NULL)
  # the end record: verified, an official run, never a run again
  if (!identical(field("Verified"), "yes") || any(grepl("^Re-run\\s*:", lines))) {
    return(NULL)
  }
  kind <- field("Kind")
  if (!is.na(kind) && !identical(kind, "run")) return(NULL)
  r <- fo$csv
  need <- c("part", "program", "output_id", "status", "md5")
  if (is.null(r) || !nrow(r) || !all(need %in% names(r))) return(NULL)
  data.frame(run = run, started = .as_time(field("Started")),
             finished = .as_time(fin), r[need],
             path = normalizePath(path, winslash = "/", mustWork = FALSE),
             stringsAsFactors = FALSE)
}
