# ============================================================================
#  A study's reports say their column widths (10/26, the user)
# ----------------------------------------------------------------------------
#  A report made on tflplanner's screens, like one of the AI's, should say its
#  column widths, so the person changes numbers rather than adds an argument,
#  and the number of columns is right.  tfl_fill_widths() writes them into a
#  report that has none -- a table's from its built table
#  (rtfreporter::default_col_widths(), one width a printed column, by name),
#  a listing's columns' from their variables and headers -- and says so with
#  an assumption (item `widths`) in spec/_assumptions.yml.  Its program then
#  writes them too: the widths are in the report.
# ============================================================================

#' Write the default column widths into the reports that have none
#'
#' A report says its column widths: a table's `plan_columns(widths = )`, one
#' width a printed column by name, a listing's `listing_col(width = )`.  For
#' each report of the study that does not, this builds it (its ARD, its
#' plan) and writes the widths rtfreporter uses when none are given
#' ([rtfreporter::default_col_widths()]: the row headings by their content,
#' the value columns sharing the rest) into its file, as numbers to change;
#' a listing's columns get a width from their variables and headers.  Each
#' report it writes widths into gets an assumption in
#' `spec/_assumptions.yml` (item `widths`): the widths are the default ones,
#' set for now.  Its program ([tfl_report_code()]) then writes them too.
#'
#' A report that says its widths is left as it is.  A table that cannot be
#' built (its data missing) is left too, and named in a message.
#'
#' @param study_dir The study folder.
#' @param output_id The reports, by id; `NULL` (default): every report in
#'   `spec/outputs/`.
#' @param assumption Add the assumption for each report written (default
#'   `TRUE`).
#' @return The ids of the reports given widths, invisibly.
#' @seealso [tfl_ai_collect_answers()], which does the same for the AI's
#'   reports.
#' @examples
#' \donttest{
#' d <- file.path(tempdir(), "fill-widths")
#' dir.create(file.path(d, "adam"), recursive = TRUE, showWarnings = FALSE)
#' saveRDS(tflspec::adsl, file.path(d, "adam", "adsl.rds"))
#' invisible(tfl_ai_study_init(d, adam = "adam", study_id = "ONCO-01", overwrite = TRUE))
#' o <- tfl_output("T-1", "table", population = "SAF")
#' o$extends <- list("defaults")
#' o$ard <- list(analyses = list(list(id = "AGE", method = "continuous", dataset = "ADSL",
#'                                    population = "SAF", by = list("TRT01A"),
#'                                    variables = list("AGE"))))
#' o$data <- list(source = "ard")
#' o$content <- list(tfl_item("table_plan", list(cols = "TRT01A",
#'                                              rows = list(group = "variable"))),
#'                   tfl_item("plan_cells", list(continuous = list(
#'                     n = "{N:.0f}", "Mean (SD)" = "{mean:.1f} ({sd:.2f})"))))
#' tfl_write_output(o, file.path(d, "spec", "outputs", "T-1.yml"))
#' tfl_fill_widths(d)
#' unlink(d, recursive = TRUE)
#' }
#' @export
# The tables among `outs` whose widths the study's defaults give them: the
# report resolved against the study (tfl_resolve_output()) has
# plan_columns(widths = ), though its own file does not.  Their ids.  A
# study that cannot be read, or a report that cannot be resolved, gives
# none (the caller then tries to build it, and says so when it cannot).
.ai_widths_inherited <- function(outs, dir) {
  tabs <- names(outs)[vapply(outs, function(o) identical(o$kind, "table"), NA)]
  if (!length(tabs)) return(character())
  old <- setwd(dir)
  on.exit(setwd(old), add = TRUE)
  st <- tryCatch(tfl_read_study_spec("."), error = function(e) NULL)
  if (is.null(st)) return(character())
  tabs[vapply(tabs, function(id) {
    ro <- tryCatch(tfl_resolve_output(outs[[id]], st), error = function(e) NULL)
    !is.null(ro) && .ai_has_widths(ro)
  }, NA)]
}

tfl_fill_widths <- function(study_dir = ".", output_id = NULL, assumption = TRUE) {
  outs <- tfl_read_outputs(study_dir)
  if (!is.null(output_id)) {
    bad <- setdiff(output_id, names(outs))
    if (length(bad)) {
      .ard_stop(sprintf("tfl_fill_widths(): no report %s in %s.",
                        paste(sprintf("'%s'", bad), collapse = ", "),
                        file.path(study_dir, "spec", "outputs")))
    }
    outs <- outs[output_id]
  }
  if (!length(outs)) return(invisible(character()))
  paths <- file.path(.outputs_dir(study_dir), paste0(names(outs), ".yml"))
  unset <- .ai_widths_unset(outs)
  # a table whose widths come from the study's defaults (its file says
  # none, the resolved report has them) says its widths already: left
  # alone, quietly
  unset <- setdiff(unset, .ai_widths_inherited(outs[unset], study_dir))
  # the listings: their columns, from their variables and headers
  lst <- intersect(unset, names(outs)[vapply(outs, function(o) identical(o$kind, "listing"), NA)])
  for (id in lst) {
    i <- match(id, names(outs))
    outs[[i]] <- .ai_listing_widths(outs[[i]])
    tfl_write_output(outs[[i]], paths[i])
  }
  # the tables: built, their widths from the printed table
  tab <- .ai_widths_on_read(outs, paths, study_dir, remember = FALSE)
  set <- c(lst, tab)
  missed <- setdiff(intersect(unset, names(outs)), set)
  if (length(missed)) {
    message(sprintf("tfl_fill_widths(): %s could not be built; no widths written.",
                    paste(missed, collapse = ", ")))
  }
  if (isTRUE(assumption) && length(set)) {
    old <- .ai_folder_assumptions(study_dir, all = TRUE)
    names(old)[names(old) == "sheet"] <- "where"
    # (one a report: a report given widths again keeps the one it had)
    have <- old$output_id[old$item %in% "widths"]
    aw <- .ai_widths_assumptions(setdiff(set, have))
    if (!is.null(aw)) {
      for (k in setdiff(names(old), names(aw))) aw[[k]] <- NA_character_
      .ai_asm_write(study_dir, rbind(old, aw[names(old)]))
    }
  }
  invisible(set)
}
