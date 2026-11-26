# The check of the references to the study's code lists (A7 S2): a report's
# `{list: X}`, the codes its `codes:` picks, and the `study$codelists$X` in
# its own R text (tfl_study_refs()), against the study's lists
# (spec/defaults.yml `codelists:`).  A name or a code the study has not got
# is an error row, not dropped in silence.  It takes the study and the
# reports in memory: tfl_check_spec() reads the folder and calls it; a
# writer (tfl_import_spec_xlsx(), the Planner's save of defaults.yml) calls
# it before it writes, on the old and the new text, and stops on a new row
# (.check_rows_new(): new by report, place, list and code, not by message,
# which names the study's lists and changes with them).
# Exported (tfl_check_study_refs()): the Planner calls it.

#' Check the references to the study's code lists, in memory
#'
#' The part of [tfl_check_spec()] that looks up what each report names of
#' the study's code lists ([tfl_study_refs()]: a code list `{list: X}`, a
#' code its `codes:` picks, a `study$codelists$X` in its own R text) in the
#' study's lists (`spec/defaults.yml` `codelists:`), on a study and reports
#' held in memory: what a writer has before it writes.
#' [tfl_check_spec()] reads the study folder and gives the same rows; the
#' xlsx import ([tfl_import_spec_xlsx()]) runs it before it writes, on the
#' files as read and as it would write them, and stops on a row the new
#' text has and the old has not (the same report, `part`, `item`, `list`
#' and `code`; the message is not compared, as it names the study's lists).
#' A template's default reference is not the report's and is not checked.
#'
#' @param study A study read with [tfl_read_study_spec()] (its code lists
#'   as changed in memory, if they are).
#' @param outputs The reports as read ([tfl_read_output()], not resolved): a
#'   list of `tfl_output`, named by id (an unnamed one by its id), or one
#'   `tfl_output`.
#' @return A data frame as [tfl_check_spec()]'s (`output_id`, `part`,
#'   `item`, `level`, `message`) and `list` and `code`, a row a name or a
#'   code the study has not got, or a list of it that is empty: `level`
#'   `"error"`, `part` `"codelists"`, `item` the variable of a code list or
#'   the place of the R text (`content: plan_labels / RACE`), `list` the
#'   study's list named, `code` the codes picked it has not got (`" | "`
#'   between two; `NA` for a list).  Zero rows when every reference is the
#'   study's.  Attribute
#'   `unresolved`: the reports whose own code lists name a list or a code
#'   the study has not got (they cannot be resolved).
#' @seealso [tfl_check_spec()], [tfl_study_refs()]
#' @examples
#' d <- system.file("extdata", "spec-yaml", package = "tflspec")
#' st <- tfl_read_study_spec(d)
#' o <- tfl_output("T-1", "table", codelists = list(SEX = list(list = "GENDER")))
#' tfl_check_study_refs(st, list(o))
#' @export
tfl_check_study_refs <- function(study, outputs) {
  if (!inherits(study, "tfl_study_spec")) {
    stop("tfl_check_study_refs(): `study` is a study read with tfl_read_study_spec().", call. = FALSE)
  }
  if (inherits(outputs, "tfl_output")) outputs <- list(outputs)
  if (!is.list(outputs) || !all(vapply(outputs, inherits, NA, "tfl_output"))) {
    stop("tfl_check_study_refs(): `outputs` is a list of reports (tfl_output), as read.", call. = FALSE)
  }
  ids <- names(outputs) %||% rep("", length(outputs))
  ids[is.na(ids) | !nzchar(ids)] <- vapply(outputs[is.na(ids) | !nzchar(ids)], function(o) o$id, "")
  names(outputs) <- ids
  lists <- study$codelists %||% list()
  # (a list the study has with no code is empty: named, not usable)
  have <- names(lists)[lengths(lists) > 0L]
  row <- function(..., list = character(), code = character()) {
    d <- .check_rows(...)
    d$list <- as.character(list)
    d$code <- as.character(code)
    d
  }
  rows <- list(row())
  unresolved <- character()
  add <- function(id, item, message, list = NA_character_, code = NA_character_) {
    rows[[length(rows) + 1L]] <<- row(output_id = id, part = "codelists", item = item, level = "error",
                                      message = message, list = list, code = code)
  }
  for (id in names(outputs)) {
    refs <- tryCatch(tfl_study_refs(outputs[[id]]), error = function(e) e)
    if (inherits(refs, "error")) {
      add(id, "", .ai_one_line(conditionMessage(refs)))
      unresolved <- c(unresolved, id)
      next
    }
    own <- refs[refs$part == "codelists", , drop = FALSE]
    for (v in unique(own$item)) {
      r <- own[own$item == v, , drop = FALSE]
      nm <- r$list[1L]
      if (!nm %in% have) {
        add(id, v, paste0("the code list of ", v, " refers to ", .cl_ref_no_list(nm, lists),
                          if (nm %in% names(lists)) ". Give the list its codes, or refer to another one." else
                            ". Add the list to the study's lists, or refer to one the study has."), nm)
        unresolved <- c(unresolved, id)
        next
      }
      codes <- .study_list_codes(lists[[nm]])
      miss <- unique(setdiff(stats::na.omit(r$code), codes))
      if (length(miss)) {
        add(id, v, paste0("the code list of ", v, " picks ", .cl_ref_no_codes(nm, miss, codes),
                          ". Add the code to the study's list, or take it out of `codes:`."),
            nm, paste(miss, collapse = " | "))
        unresolved <- c(unresolved, id)
      }
    }
    txt <- refs[refs$part != "codelists" & !refs$list %in% have, , drop = FALSE]
    for (i in seq_len(nrow(txt))) {
      at <- if (nzchar(txt$item[i])) paste0(txt$part[i], ": ", txt$item[i]) else txt$part[i]
      empty <- txt$list[i] %in% names(lists)
      add(id, at, paste0("`study$codelists$", txt$list[i], "` names ", .cl_ref_no_list(txt$list[i], lists),
                         if (empty) "; the program would find no codes there. Give the list its codes, or name another one." else
                           "; the program would find no list there. Add the list to the study's lists, or name one the study has."),
          txt$list[i])
    }
  }
  out <- do.call(rbind, rows)
  attr(out, "unresolved") <- unique(unresolved)
  out
}

# The rows `new` has that `old` has not: what a writer's change would
# break.  A row is its report, place, list and code (tfl_check_study_refs()'s
# `list` and `code`; a row of two codes is each), not its message, which
# names the study's lists and their codes: a list added for one broken
# reference changes the words of another's row, not the row.
.check_rows_new <- function(new, old) {
  keys <- function(d, i) {
    code <- if (is.null(d$code)) NA_character_ else d$code[i]
    code <- if (is.na(code)) "" else strsplit(code, " | ", fixed = TRUE)[[1L]]
    lst <- if (is.null(d$list)) NA_character_ else d$list[i]
    paste(d$output_id[i], d$part[i], d$item[i], lst, code, sep = "\r")
  }
  had <- unlist(lapply(seq_len(nrow(old)), function(i) keys(old, i)))
  is_new <- vapply(seq_len(nrow(new)), function(i) !all(keys(new, i) %in% had), NA)
  new[is_new, , drop = FALSE]
}
