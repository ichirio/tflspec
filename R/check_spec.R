#' Check a study's SPEC against the packages installed: one call
#'
#' Everything tflspec can say about a study's reports before their programs
#' run, in one table: the ARD definition (its analyses' functions and
#' arguments against the installed cards / cardx, with their versions --
#' [tfl_ard_spec()]'s errors and warnings), each report's content items
#' against the schema and the installed rtfreporter ([tfl_schema_check()],
#' the items of an appended part too), and a figure's design against the
#' installed ggplot2 and the version it is written for
#' ([tfl_check_fig_design()]).  The packages' installed versions come with
#' it, as an attribute.  A GUI's check button and the AI's run call it.
#'
#' @param study A study read with [tfl_read_study_spec()], or its folder.
#' @param output_id The reports to check; `NULL` for every report.
#' @return A data frame, one row a problem: `output_id` (`NA` for the
#'   study's ARD definition as a whole), `part` (`"ard"`, `"content"`,
#'   `"figure"`), `item` (the analysis, the function and argument, or the
#'   figure's part and field), `level` (`"error"`: the program would stop or
#'   is not written; `"warning"`: to look at; `"check"`: what the AI marked
#'   to confirm, [tfl_ai_confirm()]) and `message`.  Its attribute
#'   `versions` is a data frame of the key packages (`package`,
#'   `installed`, `NA` when not installed).
#' @examples
#' if (requireNamespace("haven", quietly = TRUE)) {
#'   # a study of the bundled ADSL, with one report
#'   d <- file.path(tempfile(), "study")
#'   dir.create(file.path(d, "adam"), recursive = TRUE)
#'   chr <- function(x) if (is.factor(x)) as.character(x) else x
#'   haven::write_xpt(as.data.frame(lapply(adsl, chr)), file.path(d, "adam", "adsl.xpt"))
#'   tfl_ai_study_init(d, adam = "adam", study_id = "EX-1")
#'   tfl_write_output(
#'     tfl_output("T-DM", "table", population = "SAF",
#'                template = list(name = "demog", args = list(population = "SAF"))),
#'     file.path(d, "spec", "outputs", "T-DM.yml"))
#'   p <- tfl_check_spec(d)
#'   p
#'   attr(p, "versions")
#' }
#' @seealso [tfl_ard_spec()], [tfl_schema_check()], [tfl_check_fig_design()]
#' @export
tfl_check_spec <- function(study, output_id = NULL) {
  if (is.character(study) && length(study) == 1L) study <- tfl_read_study_spec(study)
  if (!inherits(study, "tfl_study_spec")) {
    .ard_stop("tfl_check_spec(): `study` is a study read with tfl_read_study_spec(), or its folder.")
  }
  outs <- if (!is.null(study$dir)) tfl_read_outputs(study$dir) else list()
  ids <- output_id %||% names(outs)
  miss <- setdiff(ids, names(outs))
  if (length(miss)) {
    .ard_stop(sprintf("tfl_check_spec(): no report %s in the study.",
                      paste(sQuote(miss), collapse = ", ")))
  }
  rows <- list(.check_rows())
  add <- function(...) rows[[length(rows) + 1L]] <<- .check_rows(...)

  # the ARD definition: the study's, its problems kept for the reports asked
  if (!is.null(study$dir)) {
    withCallingHandlers(
      tryCatch(.ard_spec_of_study(study), error = function(e) {
        p <- e$problems
        if (is.data.frame(p) && nrow(p)) {
          add(output_id = p$output_id, part = "ard",
              item = ifelse(nzchar(p$row %||% ""), paste0(p$row, "$", p$field), p$field),
              level = "error", message = p$message)
        } else {
          add(output_id = NA_character_, part = "ard", item = "", level = "error",
              message = conditionMessage(e))
        }
        NULL
      }),
      warning = function(w) {
        for (m in strsplit(conditionMessage(w), "\n  ", fixed = TRUE)[[1L]]) {
          at <- regmatches(m, regexec("^([^ ]+) / ([^:]+): ", m))[[1L]]
          add(output_id = if (length(at)) at[2L] else NA_character_, part = "ard",
              item = if (length(at)) at[3L] else "", level = "warning",
              message = if (length(at)) substring(m, nchar(at[1L]) + 1L) else m)
        }
        invokeRestart("muffleWarning")
      })
  }

  # each report: its content, or its figure's design
  for (id in ids) {
    ro <- tryCatch(tfl_resolve_output(outs[[id]], study), error = function(e) e)
    if (inherits(ro, "error")) {
      add(output_id = id, part = "content", item = "", level = "error", message = conditionMessage(ro))
      next
    }
    # what the AI marked to confirm (`ai: confirm`): to look at
    cf <- .ai_confirm_rows(ro)
    if (nrow(cf)) {
      add(output_id = id, part = "content", item = ifelse(is.na(cf$item), cf$where, paste0(cf$where, "$", cf$item)),
          level = "check", message = paste0("to confirm: ", cf$why))
    }
    if (inherits(ro$content, "tfl_r")) next
    if (identical(ro$kind, "figure") && !.fig_new_content(ro$content)) {
      p <- tryCatch(tfl_check_fig_design(.fig_design_of_output(ro)), error = function(e) {
        data.frame(part = "", field = "", problem = conditionMessage(e), stringsAsFactors = FALSE)
      })
      if (nrow(p)) {
        add(output_id = id, part = "figure", item = paste0(p$part, "$", p$field),
            level = "error", message = p$problem)
      }
      next
    }
    for (it in ro$content) {
      p <- tfl_schema_check(it)
      if (nrow(p)) {
        add(output_id = id, part = "content",
            item = ifelse(nzchar(p$arg), paste0(p$fn, "(", p$arg, ")"), p$fn),
            level = p$level, message = p$message)
      }
    }
  }
  out <- do.call(rbind, rows)
  out <- out[is.na(out$output_id) | out$output_id %in% ids, , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "versions") <- .check_versions()
  out
}

.check_rows <- function(output_id = character(), part = character(), item = character(),
                        level = character(), message = character()) {
  n <- max(length(output_id), length(part), length(item), length(level), length(message))
  r <- function(x) if (length(x) == n) as.character(x) else rep(as.character(x), length.out = n)
  if (!n) return(data.frame(output_id = character(), part = character(), item = character(),
                            level = character(), message = character(), stringsAsFactors = FALSE))
  data.frame(output_id = r(output_id), part = r(part), item = r(item), level = r(level),
             message = r(message), stringsAsFactors = FALSE)
}

# the key packages and their installed versions
.check_versions <- function() {
  pk <- c("tflspec", "rtfreporter", "cards", "cardx", "ggplot2")
  v <- vapply(pk, function(p) {
    if (requireNamespace(p, quietly = TRUE)) format(utils::packageVersion(p)) else NA_character_
  }, "")
  data.frame(package = pk, installed = unname(v), stringsAsFactors = FALSE)
}
