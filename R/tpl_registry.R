# Template SPEC: a report kind's items from a few parameters.  One
# definition (an R function of declared parameters that builds code-spec
# items) gives both uses: (A) the object, tpl_<name>(), and (B) the items,
# tfl_template_expand(), saved into a report's YAML.  Prototype (10/18):
# the registry and `demog`; see _archive/template-preset-spec-design.

.tpl_registry <- new.env(parent = emptyenv())

#' A template parameter
#'
#' @param name The parameter's name.
#' @param default Its default value; `NULL` for none.
#' @param required `TRUE` when a report must give it.
#' @param doc One line saying what it is.
#' @return A `tfl_param`.
#' @keywords internal
#' @noRd
tfl_param <- function(name, default = NULL, required = FALSE, doc = NULL) {
  structure(list(name = name, default = default, required = isTRUE(required),
                 doc = doc), class = "tfl_param")
}

# Register a template: its kind, layout, parameters and body.  The body is
# function(args, context) returning list(ard = , data = , content = ): the
# report's items, `content` a list of tfl_item().
.tfl_template <- function(name, kind, layout, params, body) {
  names(params) <- vapply(params, `[[`, "", "name")
  assign(name, list(name = name, kind = kind, layout = layout,
                    params = params, body = body), envir = .tpl_registry)
  invisible(name)
}

.tpl_get <- function(name, fn) {
  t <- get0(name, envir = .tpl_registry, inherits = FALSE)
  if (is.null(t)) {
    .ard_stop(sprintf("%s: no template %s; the templates: %s.", fn, sQuote(name, FALSE),
                      paste(sort(ls(.tpl_registry)), collapse = ", ")))
  }
  t
}

# The arguments as the body sees them: given > the template's default.
.tpl_args <- function(t, args, fn) {
  unknown <- setdiff(names(args), names(t$params))
  if (length(unknown)) {
    .ard_stop(sprintf("%s: template %s has no parameter %s.", fn, t$name,
                      paste(sQuote(unknown, FALSE), collapse = ", ")))
  }
  out <- lapply(t$params, function(p) args[[p$name]] %||% p$default)
  miss <- vapply(t$params, function(p) p$required && is.null(out[[p$name]]), NA)
  if (any(miss)) {
    .ard_stop(sprintf("%s: template %s needs %s.", fn, t$name,
                      paste(sQuote(names(miss)[miss], FALSE), collapse = ", ")))
  }
  out
}

#' Expand a template into a report's items
#'
#' The B use of a template SPEC: the items its parameters stand for, to
#' save in a report's YAML (each item marked `template:<name>`) or to run.
#'
#' @param name The template's name (`"demog"`).
#' @param args Its parameters, a named list.
#' @param codelists The report's code lists (as [tfl_read_output()] reads
#'   them), for the order and labels of values.
#' @param population The study's analysis sets (`data.yml`'s populations),
#'   for their labels; `NULL` for none.
#' @return A list: `ard`, `data`, `content` (a list of [tfl_item()]).
#' @seealso [tfl_templates()]; [tpl_demog()] and [tpl_summary()] for the
#'   objects (the A use).
#' @examples
#' x <- tfl_template_expand("demog", list(population = "SAF"))
#' vapply(x$content, function(it) it$fn, "")
#' @export
tfl_template_expand <- function(name, args = list(), codelists = NULL,
                                population = NULL) {
  fn <- "tfl_template_expand()"
  t <- .tpl_get(name, fn)
  a <- .tpl_args(t, args, fn)
  out <- t$body(a, list(codelists = codelists, population = population))
  out$content <- lapply(out$content, function(it) {
    it$from <- paste0("template:", name)
    it
  })
  out
}

# ---- demog: demographics, on the summary layout --------------------------------

.tpl_set_label <- function(pop, population) {
  p <- Filter(function(x) identical(x$id, pop), population %||% list())
  lab <- if (length(p)) p[[1L]]$label %||% pop else pop
  sub(" Analysis Set$", " set", lab)
}

.tpl_levels <- function(vars, codelists) {
  out <- list()
  for (v in intersect(names(codelists), vars)) {
    out[[v]] <- vapply(codelists[[v]], function(r) r$label %||% r$value, "")
  }
  out
}

.tfl_template("demog", kind = "table", layout = "summary",
  params = list(
    tfl_param("population", required = TRUE, doc = "the analysis set's id (SAF)"),
    tfl_param("data", "ADSL", doc = "the dataset"),
    tfl_param("by", "TRT01A", doc = "the column variable"),
    tfl_param("continuous", "AGE", doc = "continuous variables"),
    tfl_param("categorical", c("AGEGR1", "SEX", "RACE", "ETHNIC"),
              doc = "categorical variables"),
    tfl_param("labels", c(AGE = "Age (years)", AGEGR1 = "Age group, n (%)",
                          SEX = "Sex, n (%)", RACE = "Race, n (%)",
                          ETHNIC = "Ethnicity, n (%)"),
              doc = "each variable's heading"),
    tfl_param("cells", list(n = "{N:.0f}", `Mean (SD)` = "{mean:.1f} ({sd:.2f})",
                            Median = "{median:.1f}", `Min, Max` = "{min:.0f}, {max:.0f}"),
              doc = "the continuous variables' rows")
 ,
    tfl_param("stack", FALSE, doc = "TRUE: the ARD as one cards::ard_stack() call")
  ),
  body = function(a, ctx) {
    pop <- a$population
    set <- paste0(tolower(a$data), "_", tolower(pop))
    vars <- c(a$continuous, a$categorical)
    ard <- list(
      analysis_data = list(list(id = set, label = .tpl_set_label(pop, ctx$population),
                                from = a$data, population = pop)),
      analyses = if (isTRUE(a$stack)) list(
        # one cards::ard_stack() call: its children name it as their parent
        list(id = "DEMOG", label = "Demographics, run together",
             method = "cards::ard_stack", data = set, by = a$by,
             purpose = "PRIMARY OUTCOME MEASURE"),
        list(id = "CONT", parent = "DEMOG", label = "Continuous", method = "continuous",
             variables = a$continuous,
             statistics = c("N", "mean", "sd", "median", "min", "max"),
             purpose = "PRIMARY OUTCOME MEASURE"),
        list(id = "CAT", parent = "DEMOG", label = "Categorical", method = "categorical",
             variables = a$categorical, statistics = c("n", "p"),
             purpose = "PRIMARY OUTCOME MEASURE"))
      else list(
        list(id = "GROUPN", label = "Subjects per group", method = "categorical",
             data = set, variables = a$by, purpose = "PRIMARY OUTCOME MEASURE"),
        list(id = "CONT", label = "Continuous", method = "continuous", data = set,
             by = a$by, variables = a$continuous,
             statistics = c("N", "mean", "sd", "median", "min", "max"),
             purpose = "PRIMARY OUTCOME MEASURE"),
        list(id = "CAT", label = "Categorical", method = "categorical", data = set,
             by = a$by, variables = a$categorical, statistics = c("n", "p"),
             purpose = "PRIMARY OUTCOME MEASURE")))
    lv <- c(list(variable = vars), .tpl_levels(c(a$by, a$categorical), ctx$codelists))
    content <- list(
      tfl_item("table_plan", list(cols = a$by, rows = list(group = "variable"))),
      tfl_item("plan_cells", list(continuous = as.list(a$cells))),
      tfl_item("plan_labels", as.list(a$labels[vars])),
      tfl_item("plan_levels", lv),
      tfl_item("plan_paginate_rows", list(max_rows = 30L, split = "group_safe")))
    list(ard = ard, data = list(source = "ard", object = "ard"), content = content)
  })

# The A use: the template's object.  Its items run on the ARD as a report's
# content does; a study's defaults are not in it -- the program writes them
# after the call (`# (study default)`), as plan_*() lines.
.tpl_object <- function(name, ard, args, codelists = NULL, population = NULL) {
  x <- tfl_template_expand(name, args, codelists = codelists, population = population)
  t <- .tpl_get(name, "tpl")
  # (the ARD is normalized first, as a report's content with no step of its
  # own: normalize_ard() with its defaults)
  content <- x$content
  if (!any(vapply(content, function(it) identical(it$fn, "normalize_ard"), NA))) {
    content <- c(list(tfl_item("normalize_ard", list())), content)
  }
  o <- tfl_output(paste0("tpl-", name), kind = t$kind, extends = "none",
                  data = x$data, content = content)
  tfl_content_run(o, ard)
}

#' Demographics: a table plan from the template `demog`
#'
#' The A use of a template SPEC: one call for the report's table plan.  The
#' template `demog` is a display template on the `summary` layout:
#' continuous variables as rows of statistics, categorical ones as n (%).
#'
#' @param ard The normalized ARD's source: the study ARD's rows of this
#'   report.
#' @param population The analysis set's id (`"SAF"`).
#' @param ... The template's other parameters: `data`, `by`, `continuous`,
#'   `categorical`, `labels`, `cells`.
#' @param codelists The report's code lists, for the order and labels of
#'   values; `NULL` takes the ARD's.
#' @return A [rtfreporter::table_plan()]: the template's items run on the
#'   ARD.  A study's defaults are not in it; add them after the call.
#' @seealso [tfl_template_expand()] for the items (the B use).
#' @examples
#' if (requireNamespace("cards", quietly = TRUE)) {
#'   saf <- adsl[adsl$SAFFL == "Y", ]
#'   ard <- cards::bind_ard(
#'     cards::ard_tabulate(saf, variables = TRT01A),
#'     cards::ard_summary(saf, by = TRT01A, variables = AGE),
#'     cards::ard_tabulate(saf, by = TRT01A, variables = c(AGEGR1, SEX, RACE, ETHNIC)))
#'   plan <- tpl_demog(ard, population = "SAF")
#'   head(rtfreporter::plan_apply(plan, "table"))
#' }
#' @export
tpl_demog <- function(ard, population, ..., codelists = NULL) {
  .tpl_object("demog", ard, c(list(population = population), list(...)),
              codelists = codelists)
}

# ---- summary: continuous variables by a column key, the layout ------------------

.tfl_template("summary", kind = "table", layout = "summary",
  params = list(
    tfl_param("population", required = TRUE, doc = "the analysis set's id (SAF)"),
    tfl_param("variables", required = TRUE, doc = "the continuous variables, in order"),
    tfl_param("by", "TRT01A", doc = "the column variable"),
    tfl_param("data", "ADSL", doc = "the dataset"),
    tfl_param("where", NULL, doc = "the records kept (R), for a dataset other than ADSL"),
    tfl_param("data_id", NULL, doc = "the analysis data's id; NULL: <data>_<population>"),
    tfl_param("data_label", NULL, doc = "the analysis data's label"),
    tfl_param("analysis_id", "CONT", doc = "the analysis's id"),
    tfl_param("analysis_label", "Continuous", doc = "the analysis's label"),
    tfl_param("statistics", c("N", "mean", "sd", "median", "min", "max"),
              doc = "the statistics the ARD has"),
    tfl_param("cells", list(n = "{N:.0f}", `Mean (SD)` = "{mean:.1f} ({sd:.2f})",
                            Median = "{median:.1f}", `Min, Max` = "{min:.0f}, {max:.0f}"),
              doc = "the rows of each variable"),
    tfl_param("labels", NULL, doc = "each variable's heading")
  ),
  body = function(a, ctx) {
    pop <- a$population
    subj <- paste0("adsl_", tolower(pop))
    sets <- list(list(id = subj, label = .tpl_set_label(pop, ctx$population),
                      from = "ADSL", population = pop))
    set <- subj
    if (!identical(toupper(a$data), "ADSL")) {
      set <- a$data_id %||% paste0(tolower(a$data), "_", tolower(pop))
      d <- list(id = set, label = a$data_label, from = a$data, subjects = subj,
                where = if (!is.null(a$where)) tfl_r(a$where))
      sets <- c(sets, list(Filter(Negate(is.null), d)))
    }
    ard <- list(
      analysis_data = sets,
      analyses = list(
        list(id = "GROUPN", method = "categorical", data = subj, variables = a$by,
             purpose = "PRIMARY OUTCOME MEASURE"),
        list(id = a$analysis_id, label = a$analysis_label, method = "continuous",
             data = set, by = a$by, variables = a$variables, statistics = a$statistics,
             purpose = "PRIMARY OUTCOME MEASURE")))
    content <- c(
      list(tfl_item("table_plan", list(cols = a$by, rows = list(group = "variable"))),
           tfl_item("plan_cells", list(continuous = as.list(a$cells)))),
      if (length(a$labels)) list(tfl_item("plan_labels", as.list(a$labels[a$variables]))),
      list(tfl_item("plan_levels", c(list(variable = a$variables),
                                     .tpl_levels(a$by, ctx$codelists))),
           tfl_item("plan_paginate_rows", list(max_rows = 30L, split = "group_safe"))))
    list(ard = ard, data = list(source = "ard", object = "ard"), content = content)
  })

#' Continuous variables by a column key: a table plan from the layout `summary`
#'
#' @inheritParams tpl_demog
#' @param variables The continuous variables, in order.
#' @param ... The template's other parameters: `by`, `data`, `where`,
#'   `data_id`, `data_label`, `analysis_id`, `analysis_label`,
#'   `statistics`, `cells`, `labels`.
#' @return A [rtfreporter::table_plan()].
#' @examples
#' if (requireNamespace("cards", quietly = TRUE)) {
#'   saf <- adsl[adsl$SAFFL == "Y", ]
#'   ard <- cards::bind_ard(
#'     cards::ard_tabulate(saf, variables = TRT01A),
#'     cards::ard_summary(saf, by = TRT01A, variables = c(AGE, TRTDURD)))
#'   plan <- tpl_summary(ard, population = "SAF", variables = c("AGE", "TRTDURD"))
#'   head(rtfreporter::plan_apply(plan, "table"))
#' }
#' @export
tpl_summary <- function(ard, population, variables, ..., codelists = NULL) {
  .tpl_object("summary", ard, c(list(population = population, variables = variables),
                                list(...)), codelists = codelists)
}

#' The templates
#'
#' @return A data frame, one row a template: `name`, `kind`, `layout`,
#'   `params` (its parameters, comma-separated).
#' @examples
#' tfl_templates()
#' @export
tfl_templates <- function() {
  nm <- sort(ls(.tpl_registry))
  t <- lapply(nm, get, envir = .tpl_registry)
  data.frame(name = nm, kind = vapply(t, `[[`, "", "kind"),
             layout = vapply(t, `[[`, "", "layout"),
             params = vapply(t, function(x) paste(names(x$params), collapse = ", "), ""),
             stringsAsFactors = FALSE)
}
