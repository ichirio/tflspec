#' The runner of a study's official runs
#'
#' The code of the runner a study's official runs use: base R, so the
#' study's programs run without tflspec.  It makes a batch folder
#' `runs/<date>_<time>_<what>/` with the logs, what the run made and the code
#' it ran.  `tfl_runner_file()` is the path of the runner file shipped with
#' tflspec.  `tfl_runner_code()` returns the code of a study's `batch.R`
#' without its banner and without the call that starts a run (the program
#' that sources the runner adds it, as it differs between the autoexec files):
#' the lists of the study's programs, written as R, then the runner's lines.
#'
#' The manifest is a named list; every element is required.  Character
#' vectors are never `NA`.
#'
#' * `root` -- the folder of the runs (a string).
#' * `ard` -- the study ARD file the ARD programs make (a string).
#' * `study` -- the study's id (study.yml's `study_id`), written into each
#'   run's batch.txt: [tfl_run_index()] counts only the runs of its study.
#' * `spec` -- the study's spec folder (a string), whose md5 batch.txt
#'   records.
#' * `programs` -- a list `list(ard = , tfl = )` of the programs, each a
#'   character vector of paths, in the order they run.
#' * `ard_outputs` -- a named character vector: the ARD program's file name
#'   (a name of `programs$ard`'s files) -> the output it makes.
#' * `report_ids` -- a named character vector: a report program (an element of
#'   `programs$tfl`) -> its output id.
#' * `needs` -- a named list: a report program -> a character vector of the
#'   reports whose ARD it reads.  `list()` for none.
#' * `unresolved` -- a named character vector: a report program -> why the
#'   ARD it reads cannot be worked out (a run that selects it does not
#'   start).  `character()` for none.
#' * `sets` -- a named list: the name of a batch -> a character vector of
#'   its outputs.  `list()` for none.
#' * `outputs` -- a named list: a report program -> the files it makes.
#' * `support` -- a list `list(ard = , tfl = , all = )` of character vectors:
#'   the files kept with the code of a run.
#' * `data` -- a named list: a program -> the data files it reads.  `list()`
#'   for none.
#'
#' @param manifest The named list above.  A missing or wrong element stops
#'   with a message that names it.
#' @return `tfl_runner_code()`: the code, one element per line.
#'   `tfl_runner_file()`: the path of the runner file (`""` if tflspec is not
#'   installed with it).
#' @examples
#' m <- list(
#'   root = "runs", ard = "output/ard/ard.rds", study = "S-1", spec = "spec",
#'   programs = list(ard = "programs/ard/ard_t1.R", tfl = "programs/tfl/t1.R"),
#'   ard_outputs = c(ard_t1.R = "T-1"), report_ids = c(`programs/tfl/t1.R` = "T-1"),
#'   needs = list(), unresolved = character(), sets = list(),
#'   outputs = list(`programs/tfl/t1.R` = "output/tfl/t1.rtf"),
#'   support = list(ard = character(), tfl = character(), all = character()),
#'   data = list())
#' head(tfl_runner_code(m), 8)
#' @export
tfl_runner_code <- function(manifest) {
  .runner_check(manifest)
  m <- manifest
  q <- function(x) encodeString(x, quote = "\"")
  bq <- function(x) encodeString(x, quote = "`")
  vec <- function(v, names = NULL, indent = "  ") {
    if (!length(v)) return("character()")
    s <- q(v)
    if (!is.null(names)) s <- paste(bq(names), "=", s)
    paste0("c(\n", paste0(indent, s, collapse = ",\n"), ")")
  }
  vlist <- function(...) {
    v <- list(...)
    paste0("list(\n", paste0("  ", names(v), " = ",
                             vapply(v, vec, "", indent = "    "),
                             collapse = ",\n"), "\n)")
  }
  # (an empty vector is character(): c() would be NULL)
  inline <- function(v) {
    if (!length(v)) return("character()")
    paste0("c(", paste(q(v), collapse = ", "), ")")
  }
  # a named list of vectors: one element per line, `name` = c(...)
  nlist <- function(l, one) {
    if (!length(l)) return("list()")
    paste0("list(\n", paste0("  ", bq(names(l)), " = ", vapply(l, one, ""),
                             collapse = ",\n"), ")")
  }
  c("# ---- the study's programs, in the order they run ---------------------------",
    paste0(".batch_root <- ", q(m$root)),
    paste0(".batch_ard  <- ", q(m$ard)),
    paste0(".batch_study <- ", q(m$study)),
    paste0(".batch_spec <- ", q(m$spec)),
    paste0(".batch_programs <- ", vlist(ard = m$programs$ard,
                                        tfl = m$programs$tfl)),
    "# the output each ARD program makes",
    paste0(".batch_ard_outputs <- ", vec(unname(m$ard_outputs),
                                         names(m$ard_outputs))),
    "# the output each report program makes",
    paste0(".batch_report_ids <- ", vec(unname(m$report_ids),
                                        names(m$report_ids))),
    "# a report program -> the reports whose ARD it reads (a run that selects",
    "# the report but not these reports does not start)",
    paste0(".batch_needs <- ", nlist(m$needs, function(v) vec(v, indent = "    "))),
    "# a report program -> why the ARD it reads cannot be worked out (a run",
    "# that selects it does not start)",
    paste0(".batch_unresolved <- ", vec(unname(m$unresolved), names(m$unresolved))),
    "# the named batches (the report list's `batches`): name -> outputs",
    paste0(".batch_sets <- ", nlist(m$sets, inline)),
    "# what each report program makes",
    paste0(".batch_outputs <- ", nlist(m$outputs, inline)),
    "# kept with the code of a run",
    paste0(".batch_support <- ", vlist(ard = m$support$ard, tfl = m$support$tfl,
                                       all = m$support$all)),
    "# the data files each program reads (the data catalog's; --data keeps them)",
    paste0(".batch_data <- ", nlist(m$data, inline)),
    "",
    readLines(tfl_runner_file(), warn = FALSE, encoding = "UTF-8"))
}

#' @rdname tfl_runner_code
#' @export
tfl_runner_file <- function() {
  system.file("batch", "runner.R", package = "tflspec")
}

# The manifest tfl_runner_code() takes: every field there, of its type, and the
# names agreeing with each other.  Stops with a message naming the field.
.runner_fields <- c("root", "ard", "study", "spec", "programs", "ard_outputs", "report_ids",
                    "needs", "unresolved", "sets", "outputs", "support", "data")

.runner_check <- function(manifest) {
  bad <- function(field, what) {
    stop("tfl_runner_code(): `manifest$", field, "` ", what, call. = FALSE)
  }
  is_chr <- function(x) is.character(x) && !anyNA(x)
  is_str <- function(x) is_chr(x) && length(x) == 1L && nzchar(x)
  named <- function(x) !is.null(names(x)) && !anyNA(names(x)) &&
    all(nzchar(names(x))) && !anyDuplicated(names(x))
  if (!is.list(manifest) || is.data.frame(manifest) ||
      (length(manifest) && !named(manifest))) {
    stop("tfl_runner_code(): `manifest` must be a named list.", call. = FALSE)
  }
  miss <- setdiff(.runner_fields, names(manifest))
  if (length(miss)) bad(miss[[1L]], "is missing.")
  extra <- setdiff(names(manifest), .runner_fields)
  if (length(extra)) bad(extra[[1L]], "is not a field of the manifest.")
  m <- manifest
  for (f in c("root", "ard", "study", "spec")) {
    if (!is_str(m[[f]])) bad(f, "must be one non-empty string.")
  }
  p <- m$programs
  if (!is.list(p) || !all(c("ard", "tfl") %in% names(p))) {
    bad("programs", "must be a list with `ard` and `tfl`.")
  }
  for (f in c("ard", "tfl")) {
    if (!is_chr(p[[f]])) {
      bad("programs", paste0("`", f, "` must be a character vector."))
    }
  }
  named_chr <- function(f) {
    x <- m[[f]]
    if (!is_chr(x) || (length(x) && !named(x))) {
      bad(f, "must be a named character vector (unique names).")
    }
  }
  named_lst <- function(f) {
    x <- m[[f]]
    if (!is.list(x) || !all(vapply(x, is_chr, NA)) ||
        (length(x) && !named(x))) {
      bad(f, "must be a named list of character vectors (unique names).")
    }
  }
  named_chr("ard_outputs")
  named_chr("report_ids")
  named_chr("unresolved")
  named_lst("needs")
  named_lst("sets")
  named_lst("outputs")
  named_lst("data")
  s <- m$support
  if (!is.list(s) || !all(c("ard", "tfl", "all") %in% names(s)) ||
      !all(vapply(s[c("ard", "tfl", "all")], is_chr, NA))) {
    bad("support", paste("must be a list with character vectors `ard`,",
                         "`tfl` and `all`."))
  }
  # the runner finds an ARD program's output by its file name: one name, one
  # program
  d <- basename(p$ard)[duplicated(basename(p$ard))]
  if (length(d)) {
    bad("programs", paste0("`ard` has two programs named ", d[[1L]], "."))
  }
  # the names agree
  not_in <- function(x, set) setdiff(x, set)
  d <- not_in(names(m$ard_outputs), basename(p$ard))
  if (length(d)) {
    bad("ard_outputs", paste0("names an ARD program that is not in ",
                              "`programs$ard`: ", d[[1L]], "."))
  }
  for (f in c("report_ids", "needs", "unresolved", "outputs")) {
    d <- not_in(names(m[[f]]), p$tfl)
    if (length(d)) {
      bad(f, paste0("names a program that is not in `programs$tfl`: ",
                    d[[1L]], "."))
    }
  }
  d <- not_in(p$tfl, names(m$report_ids))
  if (length(d)) {
    bad("report_ids", paste0("has no output for the program ", d[[1L]], "."))
  }
  d <- not_in(p$tfl, names(m$outputs))
  if (length(d)) {
    bad("outputs", paste0("has no files for the program ", d[[1L]], "."))
  }
  if (!file.exists(tfl_runner_file()) || !nzchar(tfl_runner_file())) {
    stop("tfl_runner_code(): the runner file is not installed.", call. = FALSE)
  }
  invisible(TRUE)
}
