# ============================================================================
#  The prototype "onepass" programs' helpers (tflspec, c/proto-programs):
#  each report's ARD in a file of its own, output/ard/report/<id>.rds, and
#  the study ARD collected from them.  Base R only: the study's programs
#  run without tflspec.  (Adopted, these would join study_helpers.R.)
# ============================================================================

# the folder of the reports' own ARD files
.report_ard_dir <- function() file.path("output", "ard", "report")

# a report's ARD file
report_ard_file <- function(report_id) {
  file.path(.report_ard_dir(), paste0(gsub("[^A-Za-z0-9._-]", "_", report_id), ".rds"))
}

# A report's `ard` into its own file, with what made it: the definition's
# fingerprint, the program, the run (option tflplanner.run), when, and the
# md5 of the study setup's files (option tflspec.ard_sources).  NULL (no
# `ard`): nothing is written and an older file is removed, so that a report
# that no longer makes an ARD leaves none behind.
save_report_ard <- function(ard, report_id, definition = NA_character_,
                            program = NA_character_,
                            sources = getOption("tflspec.ard_sources")) {
  f <- report_ard_file(report_id)
  if (is.null(ard)) {
    if (file.exists(f)) file.remove(f)
    return(invisible(NULL))
  }
  if (!is.data.frame(ard)) stop("save_report_ard(): `ard` is a data frame.", call. = FALSE)
  if (!"output_id" %in% names(ard)) ard$output_id <- report_id
  made <- list(output_id = report_id, definition = definition, program = program,
               run = getOption("tflplanner.run", NA_character_),
               built = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), rows = nrow(ard),
               sources = vapply(sources, function(s)
                 if (file.exists(s)) unname(tools::md5sum(s)) else "", ""))
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(f, ".tmp")
  saveRDS(structure(ard, made = made), tmp)
  file.rename(tmp, f)
  cat(sprintf("%s: %d rows into %s\n", report_id, nrow(ard), f))
  invisible(ard)
}

# Another report's ARD (its own file), or the program stops and says which
# program makes it: what a report reading another report's ARD calls
#   ard <- read_report_ard("T-14-2-2")
read_report_ard <- function(report_id) {
  f <- report_ard_file(report_id)
  if (!file.exists(f)) {
    stop(sprintf("There is no ARD of %s (%s): run its program first.", report_id, f),
         call. = FALSE)
  }
  ard <- readRDS(f)
  attr(ard, "made") <- NULL
  ard
}

# the fingerprint of the definition a report's ARD file was made from ("":
# none)
report_ard_definition <- function(report_id) {
  f <- report_ard_file(report_id)
  if (!file.exists(f)) return("")
  d <- attr(readRDS(f), "made")$definition
  if (!length(d) || is.na(d)) "" else d
}

# The study ARD: every report's ARD file, bound in the order of `ids`
# (NULL: the files' order), into output/ard/ard.rds, and what was built in
# ard_status.csv next to it (a row a file: the report, its definition, the
# program and run that made it, when, how many rows).  Nothing is computed:
# the study ARD is the reports' files, as they are.
collect_ard <- function(ids = NULL, path = file.path("output", "ard", "ard.rds")) {
  files <- list.files(.report_ard_dir(), "\\.rds$", full.names = TRUE)
  if (!is.null(ids)) {
    want <- vapply(ids, report_ard_file, "")
    files <- want[file.exists(want)]
  }
  parts <- lapply(files, readRDS)
  made <- lapply(parts, function(a) attr(a, "made"))
  parts <- lapply(parts, function(a) {
    attr(a, "made") <- NULL
    a
  })
  ard <- if (length(parts)) dplyr::bind_rows(parts) else data.frame()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(ard, path)
  one <- function(m, k) if (length(m[[k]]) && !is.na(m[[k]][1L])) as.character(m[[k]][1L]) else ""
  st <- data.frame(
    output_id = vapply(made, one, "", k = "output_id"),
    definition = vapply(made, one, "", k = "definition"),
    built = vapply(made, one, "", k = "built"),
    rows = vapply(made, one, "", k = "rows"),
    error = rep("", length(made)),
    program = vapply(made, one, "", k = "program"),
    run = vapply(made, one, "", k = "run"),
    stringsAsFactors = FALSE)
  utils::write.csv(st, file.path(dirname(path), "ard_status.csv"), row.names = FALSE)
  cat(sprintf("the study ARD: %d reports, %d rows into %s\n", nrow(st), nrow(ard), path))
  invisible(ard)
}
