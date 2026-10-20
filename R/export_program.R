# ============================================================================
#  A report's program that runs without tflspec (the export's groundwork)
# ============================================================================
#
#  What a report's program is when tflspec is not there to call: its
#  datasets read, its ARD program (the helpers it calls written in it), its
#  content as the items a template stands for (no tpl_*() call), and its
#  document (rtfreporter).  A figure's runs after the study's figure setup
#  (tfl_fig_setup_code()).  Internal for now: the exported function and the
#  JOB setting that chooses this form come later.
# ============================================================================

# The data catalog of a study, as tfl_read_data_code() reads it
.export_catalog <- function(study) {
  ds <- study$data$datasets %||% list()
  data.frame(dataset = vapply(ds, function(d) d$dataset %||% NA_character_, ""),
             path = vapply(ds, function(d) d$path %||% NA_character_, ""),
             derive = vapply(ds, function(d) if (is.null(d$derive)) NA_character_ else
               paste(as.character(d$derive), collapse = " | "), ""),
             stringsAsFactors = FALSE)
}

# The program of report `output` (as read, tfl_read_output()) of `study`:
# list(code = , kind = , content = ) -- `content` the name of what it makes
# (a table's or a listing's plan, a figure's `content`), its document `doc`
# for a table or a listing.  `figure_setup`: a figure's program starts with
# the study's figure setup (TRUE), or sources it (FALSE: the caller writes
# it once).
.export_program <- function(output, study, figure_setup = TRUE) {
  ro <- tfl_resolve_output(output, study)
  id <- ro$id
  cat_ <- .export_catalog(study)
  has_ard <- length(ro$ard$analyses %||% list()) > 0L
  ard <- if (has_ard) c("# ---- the ARD ----", tfl_ard_code(study, output_id = id, save = FALSE), "")
  if (identical(ro$kind, "figure")) {
    body <- tfl_content_code(ro, template = "items")
    # the datasets its design reads, by their names
    syms <- unique(all.names(parse(text = body, keep.source = FALSE)))
    reads <- cat_$dataset[.r_name(cat_$dataset) %in% syms]
    code <- c(if (isTRUE(figure_setup)) c("# ---- the study's figure setup ----",
                                          tfl_fig_setup_code(), ""),
              "# ---- the data ----",
              unlist(lapply(reads, function(d) tfl_read_data_code(cat_, d))), "",
              ard, "# ---- the figure ----", body)
    return(list(code = code, kind = "figure", content = "content"))
  }
  if (identical(ro$kind, "listing")) {
    sh <- suppressMessages(tfl_sheets_from_spec(study, stats::setNames(list(ro), id)))
    rework <- stats::na.omit(sh$planner$process_code[sh$planner$output_id == id])
    data <- tfl_listing_code(tfl_listing_spec(sh$listing, check = FALSE), id, cat_,
                             rework = if (length(rework)) rework, part = "data")
    content <- tfl_content_code(ro, template = "items", pipe = "|>", data = "data")
  } else {
    data <- ard
    content <- tfl_content_code(ro, template = "items", pipe = "|>")
  }
  code <- c("library(rtfreporter)", "", data, "",
            "# ---- the content ----", content, "",
            "# ---- the document ----", tfl_report_code(output, study = study, content = "plan"))
  list(code = code, kind = ro$kind, content = "plan")
}
