#' The functions a study's generated programs call
#'
#' The R code of the functions the programs tflspec writes call:
#' `blank_as_na()` (a character column's `""`, and a factor's, as `NA`: what
#' `study$adam` holds), `set_levels()` (the code lists on a data: each listed column a factor of
#' the codes in the list's order, as the ARD holds them, no labels; a value
#' not listed stops the program), `adam_codes()` (the codes of one code list
#' of the study's ADaM metadata, `study$adam_meta`, by its OID, in the list's
#' order: what `set_levels(SEX = adam_codes("CL.SEX"))` puts on a dataset;
#' a list the metadata does not have stops the program), `reorder_by()` (a column in another's
#' order when the data has it: `PARAM` by `PARAMN`), `order_levels()` (the arm
#' variables and the categories it names as factors, set in the data step
#' before the ARD: each in its code list's order, else its numeric pair's
#' -- `TRT01A` by `TRT01AN`, `SEX` by `SEXN`, `RACE` by `RACEN` --, else
#' as it is, which the ARD sorts: cards orders text by `order(x, method =
#' "radix")`, the C locale's byte order; a factor already in the data keeps
#' its levels; a blank is missing, not a level; `from = adsl` orders a
#' records data's arm, which has no number of its own, by the matching arm
#' and number of ADSL, the records' values kept and a value ADSL lacks
#' last; a value with two numbers there is an error), `order_arms()` (the arm
#' variables alone: `order_levels()` with no categories), `tte_time()` (a
#' time to event in one unit, days, weeks, months or years: AVAL from its
#' unit AVALU, DAYS when the data has none; two units in the data stop the
#' program; the days in a month etc. the study's `days_per_month` etc. when
#' set, else [tfl_time_unit_days()]), `tag_ard()` (the report, analysis and
#' analysis set in front of an analysis's ARD), `bind_items()` (a report's items into one
#' ARD: no row lost, the columns in order, a statistic twice in one item an error, the
#' same statistic in two items a warning), `fmt_ard()` and
#' `fmt_pvalue()` (formats after a call), `fmt_num()` (a number as text
#' with the decimals asked for, rounded by the study's rule: what a table
#' cell `{x:.2f}` prints), `keep_stats()` (only the
#' statistics asked for), `ard_hierarchical_or_none()` (nested counts,
#' and the ARD of no records: the overall row 0), `complete_hierarchy()` (a
#' nested count's code-list values no subject has, counted 0),
#' `drop_empty_groups()` (the rows of a group no record fell in -- the
#' baseline visit in a change's records, whose level a code list put back
#' -- so its cells stay empty), `keep_min_pct()` (a nested count's terms that
#' a percentage of subjects or more have, in any, every or the Total column;
#' the other terms' rows dropped), `relevel_ard()` (an ARD's
#' columns in their code lists' order, after an analysis that is not
#' cards'), `save_ard()` (one report's rows into the
#' study ARD, and what was built in `ard_status.csv`), `read_ard()` (one
#' report's rows of the study ARD; none, or no study ARD, stops the program,
#' naming the ARD program to run first), and what a figure
#' reads an ARD with: `ard_value()` (one statistic, as text),
#' `ard_stats()` (statistics as a data frame) and `ard_fingerprint()` (the
#' fingerprint `ard_status.csv` recorded for a report).  Base R, cards,
#' dplyr and rtfreporter's `round_num()` (the study's rounding,
#' `options(rtfreporter.rounding)`): a study writes them once into a file
#' of its own (tflplanner:
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

# The functions the study helpers define, by name
.study_helper_fns <- function() {
  h <- grep("^[A-Za-z][A-Za-z0-9_.]* <- function", tfl_helpers_code(), value = TRUE)
  sub(" <- function.*$", "", h)
}

# Whether `code` calls one of the functions `fns` (its name, then "("; not
# a longer name that ends with it).  By default the study helpers: code run
# in the session that calls one (set_levels(), tte_time(), fmt_num() ...)
# gets them first, eval(tfl_helpers_code()), as a study's programs get them
# from their setup
.calls_helper <- function(code, fns = .study_helper_fns()) {
  if (!length(fns) || !length(code)) return(FALSE)
  pat <- paste0("(^|[^A-Za-z0-9._])(", paste(fns, collapse = "|"), ")\\(")
  any(grepl(pat, code, perl = TRUE))
}
