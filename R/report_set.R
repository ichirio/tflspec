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
#    tfl_report_set(o, study)     the same, exported
#    tfl_set_report_set(o, pop)   the writer: where the reader reads
#    .set_row_carries(a)          the rows that carry the set: the reader's
#                                 and the writer's one rule
# ============================================================================

# Does a data row carry a set (`a`)?  A row that names its own
# `population`, of no other's subjects (`subjects_in`: it has theirs) --
# whatever it reads (ADSL, the set's dataset, another row, its code).  The
# reader (.rows_sets()) and the writer (.set_rows_take()) alike
.set_row_carries <- function(a) {
  is.list(a) && is.null(a[["subjects_in"]]) && !is.na(.na_scalar(unlist(a[["population"]])))
}

# The sets a report's data rows carry (.set_row_carries()), each once in
# the rows' order
.rows_sets <- function(rows) {
  v <- unlist(lapply(Filter(.set_row_carries, rows), function(a) .na_scalar(unlist(a[["population"]]))))
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

# The ids of the rows a template fills its set parameter into (`sp`;
# `args` the report's): a template file's rows that write `population:
# "{<sp>}"`; a registered template's rows that carry the set as it expands
# them, the parameter given a mark.  None, with the reason as its attribute
# `reason`, when the template cannot be read or expanded (a required
# argument the report has not given).  `study_dir`: a study whose copy of
# the template is read first
.tpl_set_row_ids <- function(name, sp, args, study_dir = NULL) {
  why <- NULL
  if (grepl("/", name, fixed = TRUE)) {
    y <- NULL
    if (!is.null(study_dir)) y <- tryCatch(.template_read(name, study_dir), error = function(e) NULL)
    if (is.null(y)) y <- tryCatch(.template_read(name), error = function(e) { why <<- conditionMessage(e); NULL })
    rows <- if (is.list(y$data) && is.null(names(y$data))) y$data
    want <- paste0("{", sp, "}")
  } else {
    want <- "TFLSPEC_SET_PARAM"
    args <- if (is.list(args)) args else list()
    args[[sp]] <- want
    x <- tryCatch(tfl_template_expand(name, args), error = function(e) { why <<- conditionMessage(e); NULL })
    rows <- c(if (is.list(x$data) && is.null(names(x$data))) x$data, x$ard$analysis_data)
  }
  ids <- unlist(lapply(rows, function(r) {
    if (.set_row_carries(r) && identical(.na_scalar(unlist(r[["population"]])), want)) .na_scalar(unlist(r[["id"]]))
  }))
  # (character(0) for none: a template that cannot be expanded has no rows,
  # and its reason is still said)
  out <- as.character(unique(ids[!is.na(ids)]))
  if (!is.null(why)) attr(out, "reason") <- why
  out
}

# A template's own default of its set parameter (`sp`; NA when it has
# none, canon 11-15 (1): the template holds it, DM's SAF): a registered
# template's parameter default, a template file's as its series gives it
# (`!series population_safety`).  `study_dir`: the study's copy first
.tpl_set_default <- function(name, sp, study_dir = NULL) {
  if (is.na(sp)) return(NA_character_)
  if (!grepl("/", name, fixed = TRUE)) {
    t <- tryCatch(.tpl_get(name, ".tpl_set_default()"), error = function(e) NULL)
    p <- Filter(function(q) is.list(q) && identical(q$name, sp), t$params %||% list())
    d <- if (length(p)) p[[1L]]$default
    return(if (is.character(d) && length(d) == 1L) .na_scalar(d) else NA_character_)
  }
  tryCatch({
    tf <- .template_file(name, study_dir)
    series <- .tr_series(name, tf, study_dir, ".tpl_set_default()")
    y <- .series_resolve(.series_read_yaml(tf), series, NULL, name)
    d <- y$params[[sp]]$default
    if (is.character(d) && length(d) == 1L) .na_scalar(d) else NA_character_
  }, error = function(e) NA_character_)
}

# The set a template report says (NA when it names no template with a set
# parameter, or says none: the template's default is then its set): its
# argument (.tpl_set_arg()), else what its data says beside it
# (.tpl_set_beside()), else its own row over the template's subjects row
# (.tpl_set_rows_set()).  What the sheets write in the templates sheet's
# population column -- not the default, which the template has without it
.tpl_set_given <- function(o, study_dir = NULL) {
  ts <- tryCatch(.tpl_spec_of(o), error = function(e) NULL)
  sp <- if (!is.null(ts)) .tpl_set_param(ts$name, study_dir) else NA_character_
  if (is.na(sp)) return(NA_character_)
  a <- .na_scalar(unlist(ts$args[[sp]]))
  if (!is.na(a)) return(a)
  b <- .tpl_set_beside(o)
  if (!is.na(b)) return(b)
  .tpl_set_rows_set(o, ts, sp, study_dir)
}

# The set a template report's own data rows give its template's subjects
# row (a row of that id over the template's, A2-T Q6; in a resolved report
# the row as the template filled it): NA when none, or several
.tpl_set_rows_set <- function(o, ts, sp, study_dir = NULL) {
  ids <- .tpl_set_row_ids(ts$name, sp, ts$args, study_dir)
  rows <- Filter(function(a) .na_scalar(unlist(a[["id"]])) %in% ids, .data_rows(o))
  v <- .rows_sets(rows)
  if (length(v) == 1L) v else NA_character_
}

# A template report whose own data row replaces the template's subjects
# row (its id) with a set other than the template argument's (A2-T Q6):
# one row of the check's frame each (.check_rows()), an error; a template
# whose rows cannot be told (.tpl_set_row_ids()'s reason): a note that
# says the set was not checked
.tpl_set_twice_rows <- function(o, id, study_dir = NULL) {
  rows <- list(.check_rows())
  arg <- .tpl_set_arg(o, study_dir)
  if (!is.na(arg)) {
    ts <- .tpl_spec_of(o)
    sp <- .tpl_set_param(ts$name, study_dir)
    subj <- .tpl_set_row_ids(ts$name, sp, ts$args, study_dir)
    if (!is.null(attr(subj, "reason")) && length(.data_rows(o))) {
      rows[[length(rows) + 1L]] <- .check_rows(
        output_id = id, part = "data", item = "data$population", level = "note",
        message = paste0("the set of the report's own rows against its template's argument is not ",
                         "checked: ", attr(subj, "reason")))
    }
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
#' template's subjects row), else its own data row over the template's
#' subjects row, else the template's own default (canon 11-15 (1): demog's
#' SAF, orr's ITT).  One rule
#' for every kind; the sheets' population column, the page's set line and
#' the checks read the same.
#'
#' @param output A report: a [tfl_output()] (or its list, as
#'   [tfl_read_output()] gives it).
#' @param study The study ([tfl_read_study_spec()]), or `NULL`.  With it,
#'   the study's copies of the templates are read (a template file's
#'   default as the study's series gives it).
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
#'     name), where `template:` gives it (`args:`, a part, a step section
#'     such as `data:`), else where a new argument goes -- never a second
#'     copy; a template with none: a report data row of the template's
#'     subjects row's id, over the template's (`study` needed);
#'   \item a report of its own rows (a code SPEC's table): the rows that
#'     carry the report's current set (a row of no other's subjects that
#'     names it); none: every row that makes its subjects -- of ADSL or of
#'     the set's dataset, of no other's subjects and no code -- and names
#'     no set.  A row of another set (a denominator) keeps its own.  Rows
#'     of several sets and no BIGN analysis to say the report's stop.
#' }
#' A template report's own data row over the template's subjects row
#' gives no set of its own once the argument is written (written once).
#' It never writes `common:`; a `common: population` the report has is
#' taken away (the set is then written once).  A second call with the same
#' set changes nothing.
#'
#' @inheritParams tfl_report_set
#' @param population The set's id (`"SAF"`); `NA` or `NULL` takes the set
#'   away.
#' @param study The study ([tfl_read_study_spec()]), or `NULL`: a set it
#'   does not register stops (canon 11-15 (2)); its sets' datasets say
#'   which rows make subjects (a set defined on ADPC), and a template with
#'   no set parameter is resolved with it.
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
  # (a set is one the study registers, canon 11-15 (2))
  if (!is.null(study) && !is.na(pop)) {
    m <- .pop_unknown_text(pop, .study_set_ids(study$data))
    if (!is.null(m)) stop(fn, ": ", m, ".", call. = FALSE)
  }
  pops <- if (!is.null(study)) study$data$populations
  o <- output
  # (the set the report has, its old `common:` read: the rows that carry it change)
  old <- .report_set(o, study$dir)
  # (never `common:`: an old one is taken away, the set is written once)
  if (is.list(o$common) && !is.null(o$common[["population"]])) {
    o$common <- o$common[setdiff(names(o$common), "population")]
  }
  val <- if (is.na(pop)) NULL else pop
  ts <- tryCatch(.tpl_spec_of(o), error = function(e) NULL)
  if (!is.null(ts)) {
    sp <- .tpl_set_param(ts$name, study$dir)
    if (!is.na(sp)) {
      # (where `template:` gives it -- `args:`, a part, a step section --,
      # else where a new argument goes: written once, .tpl_arg_put())
      o$template <- .tpl_arg_put(o$template, sp, val, study$dir)
      # (a report row over the template's subjects row names no set of its
      # own: the argument is the one place, A2-T Q6)
      ids <- .tpl_set_row_ids(ts$name, sp, ts$args, study$dir)
      own_off <- function(rows) lapply(rows, function(a) {
        if (is.list(a) && .na_scalar(unlist(a[["id"]])) %in% ids) a[["population"]] <- NULL
        a
      })
      if (is.list(o$data) && length(o$data) && is.null(names(o$data))) o$data <- own_off(o$data)
      if (is.list(o$ard$analysis_data)) o$ard$analysis_data <- own_off(o$ard$analysis_data)
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
    rold <- .report_set(r, study$dir)
    rr <- .data_rows(r)
    takes <- .set_row_taker(rr, rold, unique(c(.pop_from(pops, pop), .pop_from(pops, rold))), fn, o$id)
    ids <- unlist(lapply(rr, function(a) if (takes(a)) .na_scalar(unlist(a[["id"]]))))
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
  rows <- .data_rows(o)
  takes <- .set_row_taker(rows, old, unique(c(.pop_from(pops, pop), .pop_from(pops, old))), fn, o$id)
  rows_carry <- any(vapply(rows, function(a) takes(a) && .set_row_carries(a), NA))
  if (is.list(d) && !is.null(names(d)) && .set_on_data(o$kind, d) &&
      (!is.null(d[["population"]]) || !rows_carry)) {
    o$data[["population"]] <- val
    return(o)
  }
  set_rows <- function(rows) lapply(rows, function(a) {
    if (takes(a)) a[["population"]] <- val
    a
  })
  hit <- any(vapply(rows, takes, NA))
  if (is.list(d) && length(d) && is.null(names(d))) o$data <- set_rows(d)
  if (is.list(o$ard$analysis_data)) o$ard$analysis_data <- set_rows(o$ard$analysis_data)
  if (!hit && !is.na(pop)) {
    warning(sprintf(paste0("%s: %s has no data row that makes its subjects (a row of ADSL or of the ",
                           "set's dataset): the set %s is on no data; add the row"),
                    fn, o$id %||% "the report", pop), call. = FALSE)
  }
  o
}

# Which of a report's data rows (`rows`) take its set, as a function of a
# row: the rows that carry the set it has (`old`, .set_row_carries()) --
# the rows the reader reads it from; none: the rows that make its subjects
# (.entry_is_subjects(); `from`, the datasets one reads, .pop_from()) and
# name no set.  Rows of several sets and no set the report has (no BIGN
# analysis to say which) stop: the writer cannot tell which rows are the
# report's.  `fn`, `id` head the stop
.set_row_taker <- function(rows, old, from, fn = "tfl_set_report_set()", id = NULL) {
  own <- function(a) .na_scalar(unlist(a[["population"]]))
  carries <- function(a) .set_row_carries(a) && !is.na(old) && identical(own(a), old)
  if (any(vapply(rows, carries, NA))) return(carries)
  sets <- .rows_sets(rows)
  if (is.na(old) && length(sets) > 1L) {
    stop(sprintf(paste0("%s: %s's data rows carry several analysis sets (%s) and no BIGN analysis says ",
                        "which is the report's: add the BIGN analysis of the report's set first"),
                 fn, id %||% "the report", paste(sets, collapse = ", ")), call. = FALSE)
  }
  function(a) is.na(own(a)) && .entry_is_subjects(a, from)
}
