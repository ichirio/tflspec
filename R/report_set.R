# ============================================================================
#  A report's analysis set (A2-T, canon 11-15 rule 1): one rule for every
#  kind.  The set is the `population:` of the report's data -- a data map's
#  own key (a figure's, a listing's), else the one set the rows that make
#  its subjects carry (a table's, a figure's of rows), as its template has
#  filled them: a template report's is the template parameter that fills
#  its subjects row.  Nothing new reads `common: population`; until it goes
#  (T4, a stop) it is the last fallback of .data_population(), the one read.
#
#    .report_set(o)               the reader (spec_yaml.R): the sheets, the
#                                 page's set line, the checks, the AI kit
#    tfl_report_set(o, study)     the same, a template report resolved
#    tfl_set_report_set(o, pop)   the writer: where the reader reads
# ============================================================================

# The sets a report's data rows carry: the own `population` of each row
# that makes subjects of a set (a row of another's subjects, `subjects_in`,
# has theirs and names none), each once in the rows' order
.rows_sets <- function(rows) {
  v <- unlist(lapply(rows, function(a) {
    if (is.list(a) && is.null(a[["subjects_in"]])) .na_scalar(unlist(a[["population"]]))
  }))
  unique(v[!is.na(v)])
}

# The set a report's data rows say: the one set they carry; several (a
# denominator of another set beside the report's own): the set of the row
# the BIGN analysis reads (the heading's N is BIGN's, A2-T Q3); NA when
# none, or several and no BIGN (tfl_check_spec() says so)
.rows_carried_set <- function(rows, analyses = list()) {
  v <- .rows_sets(rows)
  if (length(v) <= 1L) return(if (length(v)) v else NA_character_)
  b <- Filter(function(a) is.list(a) && identical(.na_scalar(unlist(a[["id"]])), .bign_id),
              if (is.list(analyses)) analyses else list())
  if (!length(b)) return(NA_character_)
  id <- .na_scalar(unlist(b[[1L]][["data"]]))
  if (!is.na(id)) return(.rows_adata_pop(rows, id))
  .na_scalar(unlist(b[[1L]][["population"]]))
}

# The template parameter that fills a template's subjects row's set (its
# name, NA when it has none): a template file's (`<series>/<name>`) is the
# parameter its `data:` rows write as `population: "{<param>}"` on a row
# of no other's subjects; a registered template's is the shown parameter
# that picks among the study's sets (`tfl_param(choices = "populations")`,
# the arg defs' choice source of the same word).  A value of the SPEC, never
# a parameter's name.  `study_dir`: a study whose copy of the template is
# read first
.tpl_set_param <- function(name, study_dir = NULL) {
  if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name)) return(NA_character_)
  if (!grepl("/", name, fixed = TRUE)) {
    t <- tryCatch(.tpl_get(name, "template"), error = function(e) NULL)
    return(.tpl_set_param_of(t))
  }
  y <- NULL
  if (!is.null(study_dir)) y <- tryCatch(.template_read(name, study_dir), error = function(e) NULL)
  if (is.null(y)) y <- tryCatch(.template_read(name), error = function(e) NULL)
  rows <- if (is.list(y$data) && is.null(names(y$data))) y$data
  hit <- unlist(lapply(rows, function(r) {
    p <- if (is.list(r) && is.null(r[["subjects_in"]])) r[["population"]]
    if (is.character(p) && length(p) == 1L && grepl("^[{][A-Za-z_][A-Za-z0-9_]*[}]$", p)) {
      substr(p, 2L, nchar(p) - 1L)
    }
  }))
  hit <- intersect(unique(hit), names(y$params))
  if (length(hit) == 1L) hit else NA_character_
}
# ... of a registered template (`t`, .tpl_get()'s): its shown parameter of
# choices "populations".  A hidden one (a data stage the report's own
# `data:` says, km_survival) is no argument the report writes; `hidden =
# TRUE` finds it too, for the set the report says beside (.tpl_set_fill())
.tpl_set_param_of <- function(t, hidden = FALSE) {
  ps <- Filter(function(p) is.list(p) && (hidden || !isTRUE(p$hidden)) &&
                 identical(as.character(unlist(p$choices)), "populations"), t$params %||% list())
  nm <- vapply(ps, function(p) as.character(p$name), "", USE.NAMES = FALSE)
  if (length(nm) == 1L) nm else NA_character_
}

# A template report's set as its template's argument gives it (NA when it
# names no template, the template fills no set from an argument, or the
# report does not give it)
.tpl_set_arg <- function(o, study_dir = NULL) {
  ts <- tryCatch(.tpl_spec_of(o), error = function(e) NULL)
  if (is.null(ts)) return(NA_character_)
  sp <- .tpl_set_param(ts$name, study_dir)
  if (is.na(sp)) return(NA_character_)
  .na_scalar(unlist(ts$args[[sp]]))
}

# The set a template report says beside its template's argument: its data
# map's (a figure's `data: {population: SAF}`), else, until `common` goes,
# its `common: population` -- not its own data rows, which add to the
# template's (another set's denominator) unless they replace its subjects
# row (Q6, tfl_check_spec())
.tpl_set_beside <- function(o) {
  d <- o[["data"]]
  .na_scalar(unlist(.data_population(if (is.list(d) && !is.null(names(d))) d, o, rows = list())))
}

# The text of a set written in two places (A2-T Q6): `a` and `b` say where
.set_twice_text <- function(a, b) {
  sprintf("the set is written twice: %s and %s name different sets; write it once", a, b)
}

# A template report's arguments (`args`) with its set parameter (`sp`, NA
# for none) filled in: a report that does not give it has the set it says
# beside it (.tpl_set_beside()); one that gives it and says another there
# stops (the set is written twice, Q6).  `who` heads the stop.  Both the
# registered templates (.tpl_study_args()) and the template files
# (tfl_template_resolve()) fill it here
.tpl_set_fill <- function(args, sp, o, who) {
  if (is.na(sp)) return(args)
  own <- .tpl_set_beside(o)
  if (is.na(own)) return(args)
  given <- .na_scalar(unlist(args[[sp]]))
  if (is.na(given)) {
    args[[sp]] <- own
  } else if (!identical(given, own)) {
    .ard_stop(sprintf("%s: %s.", who, .set_twice_text(
      sprintf("the template argument `%s` (%s)", sp, given), sprintf("the report's data (%s)", own))))
  }
  args
}

# The ids of the rows a template makes its subjects in, with the set its
# argument gives (`sp` its set parameter, `args` the report's): a template
# file's rows that write `population: "{<sp>}"`, a registered template's
# rows of that set as it expands them; none when it cannot tell
.tpl_set_row_ids <- function(name, sp, args) {
  if (grepl("/", name, fixed = TRUE)) {
    y <- tryCatch(.template_read(name), error = function(e) NULL)
    rows <- if (is.list(y$data) && is.null(names(y$data))) y$data
    want <- paste0("{", sp, "}")
  } else {
    x <- tryCatch(tfl_template_expand(name, args), error = function(e) NULL)
    rows <- c(if (is.list(x$data) && is.null(names(x$data))) x$data, x$ard$analysis_data)
    want <- .na_scalar(unlist(args[[sp]]))
  }
  ids <- unlist(lapply(rows, function(r) {
    if (is.list(r) && is.null(r[["subjects_in"]]) && identical(.na_scalar(unlist(r[["population"]])), want)) {
      .na_scalar(unlist(r[["id"]]))
    }
  }))
  unique(ids[!is.na(ids)])
}

# A template report whose own data row replaces the template's subjects
# row (its id) with a set other than the template argument's (A2-T Q6):
# one row of the check's frame each (.check_rows()), an error
.tpl_set_twice_rows <- function(o, id) {
  rows <- list(.check_rows())
  arg <- .tpl_set_arg(o)
  if (!is.na(arg)) {
    ts <- .tpl_spec_of(o)
    sp <- .tpl_set_param(ts$name)
    subj <- .tpl_set_row_ids(ts$name, sp, ts$args)
    for (a in .data_rows(o)) {
      rid <- .na_scalar(unlist(a[["id"]]))
      p <- .rows_sets(list(a))
      if (rid %in% subj && length(p) && !identical(p, arg)) {
        rows[[length(rows) + 1L]] <- .check_rows(
          output_id = id, part = "data", item = paste0("data$", rid), level = "error",
          message = paste0(.set_twice_text(sprintf("the template argument `%s` (%s)", sp, arg),
                                           sprintf("the report's own row %s (%s)", rid, p)), "."))
      }
    }
  }
  do.call(rbind, rows)
}

#' A report's analysis set
#'
#' The set a report is of, as its data says it (canon 11-15: the set lives
#' on the data stage): a data map's own `population` (a figure's, a
#' listing's); else the one set the data rows that make its subjects carry
#' (a table's; several: the set of the row its BIGN analysis reads); for a
#' template report, its template's set parameter (the one that fills the
#' template's subjects row), else the rows the template makes.  One rule
#' for every kind; the sheets' population column, the page's set line and
#' the checks read the same.
#'
#' @param output A report: a [tfl_output()] (or its list, as
#'   [tfl_read_output()] gives it).
#' @param study The study ([tfl_read_study_spec()]), or `NULL`.  With it, a
#'   template report is resolved first, so that a set the template's
#'   default gives is found; without it, a template report's set is its
#'   argument only.
#' @return The set's id (`"SAF"`), `NA` for none.
#' @seealso [tfl_set_report_set()] to write it.
#' @examples
#' o <- tfl_output("T-1", "table", ard = list(
#'   analysis_data = list(list(id = "adsl_saf", from = "ADSL", population = "SAF")),
#'   analyses = list(list(id = "BIGN", method = "categorical", data = "adsl_saf",
#'                        variables = "TRT01A"))))
#' tfl_report_set(o)
#' @export
tfl_report_set <- function(output, study = NULL) {
  if (!is.list(output)) stop("tfl_report_set(): `output` is a report (tfl_output()).", call. = FALSE)
  if (!is.null(study) && !inherits(study, "tfl_study_spec")) {
    stop("tfl_report_set(): `study` is a tfl_study_spec (tfl_read_study_spec()).", call. = FALSE)
  }
  if (!is.null(study) && inherits(output, "tfl_output") && !is.null(.tpl_spec_of(output))) {
    r <- tfl_resolve_output(output, study)
    # (the rows the template made, with the report's own: no argument left to read)
    r$template <- NULL
    return(.report_set(r, study$dir))
  }
  .report_set(output, study$dir)
}

#' Write a report's analysis set
#'
#' Writes the set where [tfl_report_set()] reads it, in one place:
#' \enumerate{
#'   \item a report whose data is a map that reads subjects' records (a
#'     figure's, a listing's): the map's `population`, unless the map names
#'     none and its subjects rows carry the set (then those rows, 3);
#'   \item a template report: its template's set parameter (the one that
#'     fills the template's subjects row, found from the template, not by a
#'     name); a template with none: a report data row of the template's
#'     subjects row's id, over the template's (`study` needed);
#'   \item a report of its own rows (a code SPEC's table): every row that
#'     makes the report's subjects -- of ADSL or of the set's dataset, of no
#'     other's subjects and no code -- that names no set or the report's
#'     current one.  A row of another set (a denominator) keeps its own.
#' }
#' It never writes `common:`; a `common: population` the report has is
#' taken away (the set is then written once).  A second call with the same
#' set changes nothing.
#'
#' @inheritParams tfl_report_set
#' @param population The set's id (`"SAF"`); `NA` or `NULL` takes the set
#'   away.
#' @param study The study ([tfl_read_study_spec()]), or `NULL`: its sets'
#'   datasets say which rows make subjects (a set defined on ADPC), and a
#'   template with no set parameter is resolved with it.
#' @return The report, its set written.
#' @seealso [tfl_report_set()]
#' @examples
#' o <- tfl_output("T-1", "table", ard = list(
#'   analysis_data = list(list(id = "adsl_saf", from = "ADSL", population = "SAF"))))
#' o <- tfl_set_report_set(o, "FAS")
#' o$ard$analysis_data[[1]]$population
#' @export
tfl_set_report_set <- function(output, population, study = NULL) {
  fn <- "tfl_set_report_set()"
  if (!is.list(output)) stop(fn, ": `output` is a report (tfl_output()).", call. = FALSE)
  pop <- .na_scalar(unlist(population))
  if (length(unlist(population)) > 1L) stop(fn, ": `population` is one set's id.", call. = FALSE)
  pops <- if (!is.null(study)) study$data$populations
  o <- output
  # (never `common:`: an old one is taken away, the set is written once)
  if (is.list(o$common) && !is.null(o$common[["population"]])) {
    o$common <- o$common[setdiff(names(o$common), "population")]
  }
  val <- if (is.na(pop)) NULL else pop
  ts <- tryCatch(.tpl_spec_of(o), error = function(e) NULL)
  if (!is.null(ts)) {
    sp <- .tpl_set_param(ts$name, study$dir)
    if (!is.na(sp)) {
      # (the part of `template:` that gives it, else `args:`)
      part <- "args"
      for (k in c("ard", "content")) if (sp %in% names(o$template[[k]])) part <- k
      o$template[[part]][[sp]] <- val
      if (!length(o$template[[part]])) o$template[[part]] <- NULL
      return(o)
    }
    # (a figure's data written as a map: its key, as any report's map; a
    # template whose set is a hidden data stage, km_survival, reads it there)
    hid <- if (!grepl("/", ts$name, fixed = TRUE)) {
      .tpl_set_param_of(tryCatch(.tpl_get(ts$name, fn), error = function(e) NULL), hidden = TRUE)
    } else {
      NA_character_
    }
    if ((is.list(o$data) && !is.null(names(o$data))) || (is.null(o$data) && !is.na(hid))) {
      if (is.null(o$data) && is.null(val)) return(o)
      o$data[["population"]] <- val
      return(o)
    }
    if (is.null(study)) {
      stop(fn, ": template ", ts$name, " fills its subjects row from no argument: give the study, ",
           "so that the row's id is read from the template.", call. = FALSE)
    }
    r <- tfl_resolve_output(o, study)
    old <- .report_set(r, study$dir)
    from <- unique(c(.pop_from(pops, pop), .pop_from(pops, old)))
    ids <- unlist(lapply(.data_rows(r), function(a) {
      if (.set_row_takes(a, old, from)) .na_scalar(unlist(a[["id"]]))
    }))
    own <- if (is.list(o$data) && is.null(names(o$data))) o$data else list()
    for (i in stats::na.omit(ids)) {
      k <- match(i, vapply(own, function(a) .na_scalar(unlist(a[["id"]])), ""))
      if (is.na(k)) own <- c(own, list(list(id = i)))
      if (is.na(k)) k <- length(own)
      own[[k]][["population"]] <- val
    }
    o$data <- own
    return(o)
  }
  d <- o[["data"]]
  old <- .report_set(o)
  from <- unique(c(.pop_from(pops, pop), .pop_from(pops, old)))
  takes <- function(a) .set_row_takes(a, old, from)
  rows_carry <- any(vapply(.data_rows(o), function(a) takes(a) && !is.null(a[["population"]]), NA))
  if (is.list(d) && !is.null(names(d)) && .set_on_data(o$kind, d) &&
      (!is.null(d[["population"]]) || !rows_carry)) {
    o$data[["population"]] <- val
    return(o)
  }
  set_rows <- function(rows) lapply(rows, function(a) {
    if (takes(a)) a[["population"]] <- val
    a
  })
  hit <- any(vapply(.data_rows(o), takes, NA))
  if (is.list(d) && length(d) && is.null(names(d))) o$data <- set_rows(d)
  if (is.list(o$ard$analysis_data)) o$ard$analysis_data <- set_rows(o$ard$analysis_data)
  if (!hit && !is.na(pop)) {
    warning(sprintf(paste0("%s: %s has no data row that makes its subjects (a row of ADSL or of the ",
                           "set's dataset): the set %s is on no data; add the row"),
                    fn, o$id %||% "the report", pop), call. = FALSE)
  }
  o
}

# Does a data row take the report's set (`old`, the set the report has;
# `from`, the datasets a subjects row reads, .pop_from())?  A row that makes
# subjects (.entry_is_subjects()) and names no set or `old`
.set_row_takes <- function(a, old, from) {
  if (!.entry_is_subjects(a, from)) return(FALSE)
  p <- .na_scalar(unlist(a[["population"]]))
  is.na(p) || (!is.na(old) && identical(p, old))
}
