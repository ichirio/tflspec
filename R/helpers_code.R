#' The functions a study's generated programs call
#'
#' The R code of the functions the programs tflspec writes call:
#' `set_levels()` (the code lists on a data: each listed column a factor in
#' the list's order, its values as the list has them in the ARD; a value
#' not listed stops the program), `reorder_by()` (a column in another's
#' order when the data has it: `PARAM` by `PARAMN`), `order_arms()` (the arm
#' variables in their numeric pairs' order: `TRT01A` by `TRT01AN`), `tag_ard()` (the report, analysis and
#' analysis set in front of an analysis's ARD), `fmt_ard()` and
#' `fmt_pvalue()` (formats after a call), `keep_stats()` (only the
#' statistics asked for), `ard_hierarchical_or_none()` (nested counts,
#' and the ARD of no records: the overall row 0), `complete_hierarchy()` (a
#' nested count's code-list values no subject has, counted 0),
#' `relevel_ard()` (an ARD's
#' columns in their code lists' order, after an analysis that is not
#' cards'), `save_ard()` (one report's rows into the
#' study ARD, and what was built in `ard_status.csv`), and what a figure
#' reads an ARD with: `ard_value()` (one statistic, as text),
#' `ard_stats()` (statistics as a data frame) and `ard_fingerprint()` (the
#' fingerprint `ard_status.csv` recorded for a report).  Base R, cards and
#' dplyr only: a study writes them once into a file of its own (tflplanner:
#' `programs/study_helpers.R`, sourced by its setup) and its programs run
#' without tflspec.  [tfl_ard_code()]'s whole program (`part = "all"`)
#' carries them itself.
#'
#' @return The code, one element per line.
#' @examples
#' cat(head(tfl_helpers_code(), 12), sep = "\n")
#' @export
tfl_helpers_code <- function() {
  readLines(system.file("helpers", "study_helpers.R", package = "tflspec"),
            encoding = "UTF-8")
}
