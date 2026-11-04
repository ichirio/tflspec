# ============================================================================
#  CDISC ARS: siera makes the ARD from the reporting event
# ============================================================================
#
#  siera's readARS() writes one R programme per output from an ARS file; the
#  programme reads each ADaM as `<adam_path>/<DATASET>.csv` (its public
#  contract).  tfl_ars_ard() writes the ADaM there, has siera write the
#  programmes and runs them, each in an environment of its own.  It does not
#  touch the programmes' text, so it keeps working while siera keeps its
#  contract.
# ============================================================================

# The ADaM datasets a reporting event reads, and the variables it names.
.ars_used <- function(re) {
  ds <- character()
  vars <- character()
  cond <- function(w) {
    if (is.null(w)) return(invisible())
    if (!is.null(w$condition)) {
      ds <<- c(ds, w$condition$dataset)
      vars <<- c(vars, paste(w$condition$dataset, w$condition$variable))
    }
    for (z in w$compoundExpression$whereClauses) cond(z)
  }
  for (s in re$analysisSets) cond(s)
  for (s in re$dataSubsets) cond(s)
  for (g in re$analysisGroupings) {
    ds <- c(ds, g$groupingDataset)
    vars <- c(vars, paste(g$groupingDataset, g$groupingVariable))
  }
  for (a in re$analyses) {
    ds <- c(ds, a$dataset)
    vars <- c(vars, paste(a$dataset, a$variable))
  }
  list(datasets = unique(stats::na.omit(ds)), vars = unique(vars))
}

#' Make the ARD of a CDISC ARS reporting event with siera
#'
#' Has siera (pharmaverse) make the ARD the reporting event describes:
#' writes the ADaM it reads as CSV files (siera's programmes read
#' `<DATASET>.csv`), the event as ARS JSON, has [siera::readARS()] write
#' one programme per output, and runs them.  With [tfl_build_ard()] on the
#' same spec this is a round trip: the ARS says the same analyses as the
#' spec when the two ARDs hold the same numbers.
#'
#' CSV keeps text and numbers; a date becomes text and a code with leading
#' zeros may become a number.  A variable of the event that is a date, or
#' text of digits with a leading zero, is warned about before it is written.
#'
#' @param ars A [tfl_ars()] written with `profile = "siera"`.
#' @param adam The ADaM: a named list of data frames, or a folder
#'   ([tfl_read_adam()]).
#' @param dir A folder for the CSV files, the JSON and siera's programmes.
#' @param keep Keep `dir` (by default it is removed on exit).
#' @return The ARD (one data frame, all outputs), with siera's `OutputId`
#'   and `AnalysisId` given back as the spec's `output_id` and the ARS
#'   analysis id.  With `keep = TRUE`, attribute `dir`.
#' @seealso [tfl_ars()], [tfl_build_ard()]
#' @export
tfl_ars_ard <- function(ars, adam, dir = tempfile("tfl_ars_"), keep = FALSE) {
  .ard_need("siera", "tfl_ars_ard()")
  .ard_need("dplyr", "tfl_ars_ard()")
  if (!inherits(ars, "tfl_ars") || !identical(attr(ars, "profile"), "siera")) {
    .ard_stop("`ars` must be a tfl_ars(profile = \"siera\").")
  }
  adam <- tfl_read_adam(adam)
  used <- .ars_used(ars)
  miss <- setdiff(used$datasets, names(adam))
  if (length(miss)) {
    .ard_stop(sprintf("`adam` has no %s.", paste(miss, collapse = ", ")))
  }
  # what CSV cannot keep, among the variables the event names
  risky <- character()
  for (dv in used$vars) {
    p <- strsplit(dv, " ", fixed = TRUE)[[1L]]
    v <- adam[[p[1L]]][[p[2L]]]
    if (is.null(v)) next
    if (inherits(v, c("Date", "POSIXt")) ||
        (is.character(v) && any(grepl("^0[0-9]+$", v)))) risky <- c(risky, dv)
  }
  if (length(risky)) {
    warning("CSV may not keep these as they are (a date, or digits with a ",
            "leading zero): ", paste(sub(" ", ".", risky), collapse = ", "),
            call. = FALSE)
  }
  adam_dir <- file.path(dir, "adam")
  prog_dir <- file.path(dir, "programmes")
  dir.create(adam_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(prog_dir, recursive = TRUE, showWarnings = FALSE)
  if (!keep) on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  for (d in used$datasets) {
    utils::write.csv(adam[[d]], file.path(adam_dir, paste0(d, ".csv")),
                     row.names = FALSE, na = "")
  }
  json <- file.path(dir, "ars.json")
  tfl_write_ars_json(ars, json)
  siera::readARS(json, output_path = normalizePath(prog_dir, "/"),
                 adam_path = normalizePath(adam_dir, "/"))
  scripts <- list.files(prog_dir, pattern = "^ARD_.*[.]R$", full.names = TRUE)
  if (!length(scripts)) {
    .ard_stop("siera::readARS() wrote no programme.")
  }
  ards <- lapply(scripts, .ars_run_programme)
  ard <- do.call(dplyr::bind_rows, ards)
  if (requireNamespace("cards", quietly = TRUE)) {
    ard <- tryCatch(cards::tidy_ard_column_order(ard), error = function(e) ard)
  }
  info <- attr(ars, "ids")
  if (!is.null(ard$OutputId)) {
    o <- unique(info[c("siera_output_id", "output_id")])
    k <- match(ard$OutputId, o$siera_output_id)
    ard$OutputId[!is.na(k)] <- o$output_id[k[!is.na(k)]]
  }
  if (!is.null(ard$AnalysisId)) {
    k <- match(ard$AnalysisId, info$siera_id)
    ard$AnalysisId[!is.na(k)] <- info$ars_id[k[!is.na(k)]]
  }
  if (keep) attr(ard, "dir") <- dir
  ard
}

# Run one of siera's programmes in an environment of its own and return its
# ARD.  Its library() lines are left out: its calls name their packages, and
# attaching would change the caller's search path.
.ars_run_programme <- function(path) {
  code <- gsub("^\\s*library\\(", "# library(", readLines(path, warn = FALSE))
  env <- new.env(parent = globalenv())
  eval(parse(text = code, encoding = "UTF-8"), envir = env)
  if (!exists("ARD", envir = env, inherits = FALSE)) {
    .ard_stop(sprintf("siera's programme %s made no `ARD`.", basename(path)))
  }
  get("ARD", envir = env)
}
