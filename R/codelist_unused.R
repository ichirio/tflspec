# ---- a report's code list that no step uses ------------------------------
#
# Canon 11-28 (CANON-3): a report's `codelists:` is written once, at the top
# of the report, and each program it makes writes the lists it uses.  A list
# that none of the report's steps uses (its data, its analyses, its content,
# its figure) is most likely left over: a warning, once, naming the
# variable.  It does not stop.

# The parts of a report that are its steps (what is not one: its id, kind,
# label, page, the lists themselves, the layers it extends ...)
.cl_step_parts <- c("common", "data", "ard", "analysis", "content", "contents",
                    "template", "plot", "plots", "figure", "compose")

# Every name a value says: the names of its lists, the words of its text
# (R, a glue string, a variable's name alike), the names of its R.  A
# name too many is no harm: it only keeps a list from the warning.
.cl_said_names <- function(x) {
  out <- character()
  word <- "[A-Za-z.][A-Za-z0-9._]*"
  walk <- function(e, depth = 0L) {
    if (is.null(e) || depth > 50L || is.function(e) || is.environment(e)) return(invisible())
    if (is.language(e)) {
      out <<- c(out, all.names(e))
      return(invisible())
    }
    at <- attributes(e)
    for (a in setdiff(names(at), c("names", "class", "row.names", "levels",
                                    "study_codelists", "study_rounding"))) {
      walk(at[[a]], depth + 1L)
    }
    if (is.list(e)) {
      out <<- c(out, names(e))
      for (k in unclass(e)) walk(k, depth + 1L)
      return(invisible())
    }
    if (is.factor(e)) e <- as.character(e)
    if (is.character(e)) {
      s <- e[!is.na(e) & nzchar(e)]
      if (length(s)) out <<- c(out, unlist(regmatches(s, gregexpr(word, s))))
    }
    invisible()
  }
  walk(x)
  unique(out[!is.na(out) & nzchar(out)])
}

# A report's listed variables that none of its steps names.  What a report
# does not write itself counts too, so that a list in use is never warned
# of: its template's definition (its items, parameters, defaults) and,
# unless it extends none, the study's layers (defaults, styles, patterns).
.cl_unused <- function(output, study = NULL) {
  vars <- names(output$codelists)
  if (!length(vars)) return(character())
  parts <- unclass(output)[intersect(names(unclass(output)), .cl_step_parts)]
  # (a template's items, when the study has put them on the report)
  said <- c(.cl_said_names(parts), .cl_said_names(attr(output, "template")))
  tp <- output$template
  tp <- if (is.character(tp)) tp[1L] else if (is.list(tp)) tp$name
  if (is.character(tp) && length(tp) == 1L && !is.na(tp)) {
    said <- c(said, .cl_said_names(tryCatch(unclass(.tpl_get(tp, "codelists")),
                                            error = function(e) NULL)))
  }
  if (!is.null(study) && !identical(output$extends, "none")) {
    said <- c(said, .cl_said_names(unclass(study)[c("defaults", "styles", "patterns",
                                                    "figure_style")]))
  }
  vars[!vars %in% said]
}

# The warning, once a session for a report and its unused lists
.cl_unused_warn <- function(outputs, study = NULL) {
  for (o in outputs) {
    if (!inherits(o, "tfl_output")) next
    v <- .cl_unused(o, study)
    if (!length(v)) next
    key <- paste(c(o$id, v), collapse = "\r")
    if (isTRUE(.cl_unused_memo[[key]])) next
    .cl_unused_memo[[key]] <- TRUE
    warning(sprintf(paste0(
      "%s: the code list of %s is used by no step of the report (data, ",
      "analysis, content or figure). Use it, or remove it from `codelists:`."),
      o$id, paste(v, collapse = ", ")), call. = FALSE)
  }
  invisible(NULL)
}
.cl_unused_memo <- new.env(parent = emptyenv())
