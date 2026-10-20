# Template SPEC: a report kind's items from a few parameters.  One
# definition (an R function of declared parameters that builds code-spec
# items) gives both uses: (A) the object, tpl_<name>(), and (B) the items,
# tfl_template_expand(), saved into a report's YAML.  Prototype (10/18):
# the registry and `demog`; see _archive/template-preset-spec-design.

.tpl_registry <- new.env(parent = emptyenv())

#' A template parameter
#'
#' One parameter of a template ([tfl_template()]): its name, default and
#' what it is.  A template has few, and none of them is the look (a font,
#' a width, the rows a page): the study's styles and defaults say that.
#'
#' @param name The parameter's name.
#' @param default Its default value; `NULL` for none.
#' @param required `TRUE` when a report must give it.
#' @param doc One line saying what it is.
#' @param study Where the study standard's value of it is, taken when the
#'   report's template SPEC does not give it: `"output.population"` (the
#'   report's `population:`), `"settings.treatment"` (`study.yml`'s
#'   `settings: treatment:`), or `"<verb>.<argument>"`
#'   (`"plan_cells.continuous"`: that argument of the study's defaults (and
#'   pattern) item).  `NULL`: tflspec's for its name (`population`, `by`,
#'   `group`), else the template's own default only.
#' @param type What its value is, for an input form: `"vars"` (variable
#'   names), `"levels"` (values of a variable), `"flag"` (`TRUE` /
#'   `FALSE`), `"number"`, `"text"` (also an R condition, an id, a label),
#'   `"choice"` (one of `choices`) or `"list"` (a named vector or a list
#'   of records, their `fields`).  `NULL`: tflspec's type for the name
#'   (`by` is `"vars"`, `where` `"text"` ...), else one read from the
#'   default.
#' @param multiple `TRUE` when it takes several values (`hierarchy`),
#'   `FALSE` one (`by`); `NULL`: as `type`.
#' @param of For `type = "levels"`: whose values they are.  A name of the
#'   template's parameters first (the variable it names: `by_levels`, `of =
#'   "by"`), else a variable's name (`params`, `of = "PARAMCD"`).
#' @param choices For `type = "choice"`: the values it may take.
#' @param fields For `type = "list"`: a record's fields, `c("vars",
#'   "label", "width")`; a named vector's are `c("name", "text")`.
#' @param group The input form's group it is shown in: `"target"` (shown
#'   first) for what the report is of -- `population`, `subset` (which
#'   subjects: a condition on the analysis set's ADSL, narrowing the
#'   denominators, the header N and, through them, the records) and `where`
#'   (which records of the records data); `NULL`: tflspec's for its name,
#'   else none.
#' @return A `tfl_param`.
#' @examples
#' tfl_param("population", "SAF", doc = "the analysis set's id")
#' tfl_param("layout", "cols", type = "choice", choices = c("cols", "rows"))
#' @export
tfl_param <- function(name, default = NULL, required = FALSE, doc = NULL, study = NULL,
                      type = NULL, multiple = NULL, of = NULL, choices = NULL,
                      fields = NULL, group = NULL) {
  if (!is.null(type)) type <- match.arg(type, .tpl_param_types)
  structure(list(name = name, default = default, required = isTRUE(required),
                 doc = doc, study = study, type = type, multiple = multiple, of = of,
                 choices = choices, fields = fields, group = group), class = "tfl_param")
}

.tpl_param_types <- c("vars", "levels", "flag", "number", "text", "choice", "list")

# tflspec's parameters' types by name (a template's tfl_param(type = )
# says it where a name means two things: `param`, `sort`)
.tpl_param_kinds <- local({
  k <- function(names, type, multiple = FALSE, ...) {
    stats::setNames(rep(list(list(type = type, multiple = multiple, ...)), length(names)),
                    names)
  }
  nv <- c("name", "text")
  c(k(c("by", "baseline", "post", "term", "grade", "category", "censor", "group", "time",
        "pop", "param_order", "visit_order", "exposure", "response", "value", "visit_var"), "vars"),
    k(c("categorical", "continuous", "variables", "hierarchy", "covariates", "subgroups"),
      "vars", TRUE),
    k("comparison", "levels", of = "group"),
    k("by_levels", "levels", TRUE, of = "by"),
    k("levels", "levels", TRUE, of = "post"),
    k("grade_levels", "levels", TRUE, of = "grade"),
    k("responders", "levels", TRUE, of = "category"),
    k("params", "levels", TRUE, of = "PARAMCD"),
    k("visits", "levels", TRUE, of = "AVISIT"),
    k("visit", "levels", of = "AVISIT"),
    k("baseline_value", "levels", of = "baseline"),
    k(c("sort_by", "ref"), "levels", of = "by"),
    k(c("stack", "drop_time_0", "cumulative", "baseline_normal", "by_response"), "flag"),
    k(c("min_pct", "per"), "number"),
    k("layout", "choice", choices = c("cols", "rows")),
    k("grade_in", "choice", choices = c("rows", "cols")),
    k("min_pct_in", "choice", choices = c("any", "all", "total")),
    # what the report is of: the analysis set, which subjects, which records
    k(c("population", "subset", "where"), "text", group = "target"),
    k(c("data", "data_id", "data_label", "analysis_id",
        "analysis_label", "overall", "any_grade", "cat_id", "cat_label", "ci_id", "ci_label",
        "ci_method", "groupn_label", "median_label", "time_prefix", "time_unit", "unit",
        "when", "visit_label", "param_label", "baseline_label", "value_label", "change_label", "cell", "start"),
      "text"),
    k(c("heading", "statistics", "cat_statistics"), "text", TRUE),
    # a variable's heading each: given by name, merged with the default's
    k(c("labels", "param_labels"), "list", fields = nv, merge = TRUE),
    # the rows: given, they are the rows (no merge)
    k("cells", "list", fields = c("label", "template")),
    k("flags", "list", TRUE, fields = c("id", "label", "where")),
    k("columns", "list", TRUE, fields = c("vars", "label", "width", "collapse_repeats")))
})

# A figure's data step keeping an analysis set's subjects: a semi join of
# its dataset's records (data.yml's definition: dataset, where), so any
# where works, whatever the figure's data has
.tpl_population_step <- function(id, populations) {
  # as a table's analysis data: an analysis set the study does not
  # define stops (no figure of the wrong subjects)
  def <- Filter(function(p) identical(p$id, id), populations %||% list())
  if (!length(def)) {
    .ard_stop(sprintf("template: population %s is not in `populations` (data.yml: %s).",
                      id, paste(vapply(populations %||% list(), `[[`, "", "id"),
                                collapse = ", ")))
  }
  def <- def[[1L]]
  w <- def$where
  list(Filter(Negate(is.null), list(
    step = "join", dataset = def$dataset %||% "ADSL",
    where = if (!is.null(w)) as.character(w), by = "USUBJID", how = "semi")))
}

# The study's own settings a template parameter may take its value from
# (tfl_param(study = )), besides "<verb>.<argument>" of the study's
# defaults: the one list of their names.  Each is function(output, study).
.tpl_study_sources <- list(
  # the report's analysis set (its `population:`)
  # (none when the report names none: NA is tfl_output()'s "not given")
  output.population = function(output, study) {
    p <- output$population
    if (length(p) == 1L && !is.na(p)) p
  },
  # the study's treatment variable (study.yml `settings: treatment:`)
  settings.treatment = function(output, study) study$study$settings$treatment
)

# tflspec's parameters' study sources by name: the analysis set and the
# treatment are the study's (a parameter's own `study =` wins)
.tpl_param_study <- c(population = "output.population", by = "settings.treatment",
                      group = "settings.treatment")

# A parameter's type filled in: as declared, by its name, else from its
# default (a company's template that says none: `guessed`, which
# tfl_test_templates() lists)
.tpl_param_typed <- function(p) {
  if (is.null(p$study) && p$name %in% names(.tpl_param_study)) {
    p$study <- .tpl_param_study[[p$name]]
  }
  by_name <- .tpl_param_kinds[[p$name]]
  if (is.null(p$type) && !is.null(by_name)) {
    for (f in names(by_name)) if (is.null(p[[f]])) p[f] <- list(by_name[[f]])
  }
  # its form's group: tflspec's for its name, whatever its type says
  if (is.null(p$group) && !is.null(by_name$group)) p$group <- by_name$group
  if (is.null(p$type)) {
    p$guessed <- TRUE
    d <- p$default
    p$type <- if (is.logical(d)) "flag" else if (is.numeric(d)) "number" else
      if (is.list(d)) "list" else "text"
  }
  if (is.null(p$multiple)) p$multiple <- identical(p$type, "list") || length(p$default) > 1L
  p
}

# Register a template: its kind, layout, parameters and body.  The body is
# function(args, context) returning list(ard = , data = , content = ): the
# report's items, `content` a list of tfl_item().
.tfl_template <- function(name, kind, layout, params, body, label = NULL, call = NULL,
                          package = "tflspec", example = NULL) {
  params <- lapply(params, .tpl_param_typed)
  names(params) <- vapply(params, `[[`, "", "name")
  assign(name, list(name = name, kind = kind, layout = layout, params = params, body = body,
                    label = label, call = call %||% paste0("tpl_", name), package = package,
                    example = example),
         envir = .tpl_registry)
  invisible(name)
}

.tpl_get <- function(name, fn) {
  t <- get0(name, envir = .tpl_registry, inherits = FALSE)
  if (is.null(t)) {
    .ard_stop(sprintf("%s: no template %s; the templates: %s.", fn, sQuote(name, FALSE),
                      paste(sort(ls(.tpl_registry)), collapse = ", ")))
  }
  if (!is.null(t$from)) t <- .tpl_variant(t)
  t
}

# A named default and the names given: the default's order, the new names
# after it, a NULL dropping its name; as a character vector when the
# default is one
.tpl_merge_named <- function(default, given) {
  m <- utils::modifyList(as.list(default), as.list(given))
  if (is.character(default)) unlist(m) else m
}

# The arguments as the body sees them: given (NULL too) > the template's default.
.tpl_args <- function(t, args, fn) {
  unknown <- setdiff(names(args), names(t$params))
  if (length(unknown)) {
    .ard_stop(sprintf("%s: template %s has no parameter %s.", fn, t$name,
                      paste(sQuote(unknown, FALSE), collapse = ", ")))
  }
  # a parameter given, NULL too ("none"), else the template's default; a
  # map of headings given by name changes those names only (`RACE: ~`, a
  # NULL, drops one)
  out <- lapply(t$params, function(p) {
    if (!p$name %in% names(args)) return(p$default)
    v <- args[[p$name]]
    if (isTRUE(p$merge) && length(p$default) && length(v) && !is.null(names(v))) {
      v <- .tpl_merge_named(p$default, v)
    }
    v
  })
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
  out <- t$body(a, list(codelists = codelists, population = population, given = names(args)))
  # a figure's: how many of the data's last steps its call (tpl_<name>())
  # makes -- the statistics; the program writes the steps before them
  if (identical(t$kind, "figure")) {
    k <- out$call_steps
    if (!is.numeric(k) || length(k) != 1L || is.na(k) || k < 0 ||
        k > length(out$data$steps %||% list())) {
      .ard_stop(sprintf(paste0("%s: template %s's body returns `call_steps`, how many of its ",
                               "data's last steps its call makes."), fn, name))
    }
    out$call_steps <- as.integer(k)
  }
  # which subjects: the analysis set's ADSL narrowed, whatever the template
  if ("subset" %in% names(t$params)) out <- .tpl_subset(out, a$subset, population)
  out$content <- lapply(out$content, function(it) {
    it$from <- paste0("template:", name)
    it
  })
  out
}

# The template parameters every table and figure takes: which subjects
# (`subset`) and which records (`where`)
.tpl_subset_param <- function() {
  tfl_param("subset", NULL, doc = "which subjects (R, on the analysis set's ADSL): PART == \"Part 1\"")
}
.tpl_where_param <- function(default = NULL, doc = "which records (R)") {
  tfl_param("where", default, doc = doc)
}

# Conditions together (`&`), each in brackets when it is an "or"; NULL
# when none
.tpl_and <- function(...) {
  x <- vapply(Filter(function(v) length(v) && !is.na(v[1L]) && nzchar(v[1L]), list(...)),
              function(v) as.character(v)[1L], "")
  if (length(x) < 2L) return(if (length(x)) x)
  x <- vapply(x, function(v) {
    e <- tryCatch(str2lang(v), error = function(err) NULL)
    if (is.call(e) && as.character(e[[1L]]) %in% c("|", "||")) sprintf("(%s)", v) else v
  }, "")
  paste(x, collapse = " & ")
}

# A template's items with the report's subjects (`subset`): the analysis
# set's ADSL data keeps them (its `where`), and a records data of the set
# takes its subjects from it (`subjects`: the three stages); a figure's
# semi join of the set's ADSL; a listing's records of the ADSL subjects it keeps (no
# analysis set: all of ADSL narrowed)
.tpl_subset <- function(out, subset, population = NULL) {
  s <- .tpl_and(subset)
  if (is.null(s)) return(out)
  ad <- out$ard$analysis_data
  if (length(ad)) {
    ids <- vapply(ad, function(e) e$id %||% "", "")
    res <- list()
    for (e in ad) {
      p <- e$population
      if (!is.null(p) && is.null(e$code)) {
        if (identical(toupper(e$from %||% ""), "ADSL")) {
          e$where <- tfl_r(.tpl_and(e$where, s))
        } else {
          sid <- paste0("adsl_", tolower(p))
          if (!sid %in% ids) {
            res <- c(res, list(list(id = sid, label = .tpl_set_label(p, population),
                                    from = "ADSL", population = p, where = tfl_r(s))))
            ids <- c(ids, sid)
          }
          e$population <- NULL
          e$subjects <- sid
        }
      }
      res <- c(res, list(e))
    }
    out$ard$analysis_data <- res
  }
  st <- out$data$steps
  if (is.list(st) && length(st) && !inherits(st, "tfl_r") &&
      all(vapply(st, is.list, NA))) {
    kinds <- vapply(st, function(x) x$step %||% "", "")
    j <- which(kinds == "join" & vapply(st, function(x) identical(x$how, "semi") &&
      identical(toupper(x$dataset %||% ""), "ADSL"), NA))
    if (length(j)) {
      st[[j[1L]]]$where <- .tpl_and(st[[j[1L]]]$where, s)
    } else {
      # (after the read: a figure whose data is its ARD has none)
      at <- match("read", kinds, nomatch = 0L)
      if (at > 0L) st <- append(st, list(list(step = "join", dataset = "ADSL", where = s,
                                              by = "USUBJID", how = "semi")), at)
    }
    out$data$steps <- st
  }
  # a listing has no analysis set: the records of ADSL's subjects the
  # subset keeps, a semi join after its own condition (its data's rework)
  if (identical(out$data$source, "dataset")) {
    e <- tryCatch(str2lang(s), error = function(err) NULL)
    w <- if (!is.null(e)) paste(vapply(.and_parts(e), .expr_text, ""), collapse = ", ") else s
    out$data$steps <- c(out$data$steps, list(tfl_r(paste0(
      "# the subjects of the subset (ADSL)\n",
      "data <- data |>\n",
      "  dplyr::semi_join(\n",
      "    adsl |> dplyr::filter(", w, ") |> dplyr::select(USUBJID),\n",
      "    by = \"USUBJID\"\n",
      "  )"))))
  }
  out
}

# ---- demog: demographics, on the summary layout --------------------------------

.tpl_set_label <- function(pop, population) {
  p <- Filter(function(x) identical(x$id, pop), population %||% list())
  lab <- if (length(p)) p[[1L]]$label %||% pop else pop
  sub(" Analysis Set$", " set", lab)
}

# A code list's values in its order: from a report's code lists as the
# YAML holds them (rows of value / label), or from a list of the values
# themselves (`list(TRT01A = c("Drug X", "Control"))`, what a program
# passes to tpl_*())
.tpl_levels <- function(vars, codelists) {
  out <- list()
  for (v in intersect(names(codelists), vars)) {
    cl <- codelists[[v]]
    out[[v]] <- if (is.character(cl)) unname(cl) else
      vapply(cl, function(r) r$label %||% r$value, "")
  }
  out
}

.tfl_template("demog", kind = "table", layout = "summary",
  params = list(
    tfl_param("population", required = TRUE, doc = "the analysis set's id (SAF)"),
    .tpl_subset_param(),
    tfl_param("data", "ADSL", doc = "the dataset"),
    tfl_param("by", "TRT01A", doc = "the column variable"),
    tfl_param("continuous", "AGE", doc = "continuous variables"),
    tfl_param("categorical", c("AGEGR1", "SEX", "RACE", "ETHNIC"),
              doc = "categorical variables"),
    tfl_param("labels", c(AGE = "Age (years)", AGEGR1 = "Age group, n (%)",
                          SEX = "Sex, n (%)", RACE = "Race, n (%)",
                          ETHNIC = "Ethnicity, n (%)"),
              doc = "each variable's heading; given by name, merged (RACE: ~ drops one)"),
    tfl_param("cells", list(n = "{N:.0f}", `Mean (SD)` = "{mean:.1f} ({sd:.2f})",
                            Median = "{median:.1f}", `Min, Max` = "{min:.0f}, {max:.0f}"),
              doc = "the continuous variables' rows", study = "plan_cells.continuous")
 ,
    tfl_param("stack", FALSE, doc = "TRUE: the ARD as one cards::ard_stack() call")
  ),
  body = function(a, ctx) {
    pop <- a$population
    set <- paste0(tolower(a$data), "_", tolower(pop))
    vars <- c(a$continuous, a$categorical)
    # the template's own variables, not the report's: those the data has
    # (any_of() in the ARD program)
    opt <- function(p) if (!p %in% ctx$given) TRUE
    ard <- list(
      analysis_data = list(list(id = set, label = .tpl_set_label(pop, ctx$population),
                                from = a$data, population = pop)),
      analyses = if (isTRUE(a$stack)) list(
        # one cards::ard_stack() call: its children name it as their parent
        list(id = "DEMOG", label = "Demographics, run together",
             method = "cards::ard_stack", data = set, by = a$by,
             purpose = "PRIMARY OUTCOME MEASURE"),
        list(id = "CONT", parent = "DEMOG", label = "Continuous", method = "continuous",
             variables = a$continuous, optional = opt("continuous"),
             statistics = c("N", "mean", "sd", "median", "min", "max"),
             purpose = "PRIMARY OUTCOME MEASURE"),
        list(id = "CAT", parent = "DEMOG", label = "Categorical", method = "categorical",
             variables = a$categorical, optional = opt("categorical"), statistics = c("n", "p"),
             purpose = "PRIMARY OUTCOME MEASURE"))
      else list(
        list(id = "GROUPN", label = "Subjects per group", method = "categorical",
             data = set, variables = a$by, purpose = "PRIMARY OUTCOME MEASURE"),
        list(id = "CONT", label = "Continuous", method = "continuous", data = set,
             by = a$by, variables = a$continuous, optional = opt("continuous"),
             statistics = c("N", "mean", "sd", "median", "min", "max"),
             purpose = "PRIMARY OUTCOME MEASURE"),
        list(id = "CAT", label = "Categorical", method = "categorical", data = set,
             by = a$by, variables = a$categorical, optional = opt("categorical"),
             statistics = c("n", "p"),
             purpose = "PRIMARY OUTCOME MEASURE")))
    ard$analyses <- lapply(ard$analyses, function(x) Filter(Negate(is.null), x))
    # (no plan_levels(): the ARD says the order -- the code lists applied
    # when it is made, the variables in its analyses' order)
    content <- list(
      tfl_item("table_plan", list(cols = a$by, rows = list(group = "variable"))),
      tfl_item("plan_cells", list(continuous = as.list(a$cells))),
      tfl_item("plan_labels", list(variable = as.list(a$labels[vars]))),
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
#'   `categorical`, `labels`, `cells`.  `labels` given by name change those
#'   names only (`labels:` with `RACE: "Race, n (%) [a]"`); `RACE: ~` drops one.
#'   The default `continuous` and `categorical` are the variables the data
#'   has of them (`any_of()`); the report's own are as written.  Every
#'   table and figure template also takes `subset` (which subjects) and,
#'   with records data, `where` (which records): see [tfl_param()].
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

.tpl_summary_params <- list(
  tfl_param("population", required = TRUE, doc = "the analysis set's id (SAF)"),
  .tpl_subset_param(),
  tfl_param("variables", NULL, doc = "the continuous variables, in order"),
  tfl_param("categorical", NULL, doc = "the categorical variables, in order"),
  tfl_param("by", "TRT01A", doc = "the column variable"),
  tfl_param("data", "ADSL", doc = "the dataset"),
  tfl_param("where", NULL, doc = "the records kept (R), for a dataset other than ADSL"),
  tfl_param("data_id", NULL, doc = "the analysis data's id; NULL: <data>_<population>"),
  tfl_param("data_label", NULL, doc = "the analysis data's label"),
  tfl_param("groupn_label", NULL, doc = "the subjects-per-group analysis' label"),
  tfl_param("analysis_id", "CONT", doc = "the continuous analysis' id"),
  tfl_param("analysis_label", "Continuous", doc = "the continuous analysis' label"),
  tfl_param("statistics", c("N", "mean", "sd", "median", "min", "max"),
            doc = "the continuous statistics the ARD has"),
  tfl_param("ci_id", "CI", doc = "the confidence-interval analysis' id"),
  tfl_param("ci_label", NULL,
            doc = "the continuous variables' mean (CI) analysis' label; NULL: none"),
  tfl_param("ci_method", "mean_ci", doc = "its method (mean_ci: a one-sample t-test)"),
  tfl_param("cat_id", "CAT", doc = "the categorical analysis' id"),
  tfl_param("cat_label", "Categorical", doc = "the categorical analysis' label"),
  tfl_param("cat_statistics", c("n", "p"),
            doc = "the categorical statistics the ARD has; NULL: cards' own"),
  tfl_param("cells", list(n = "{N:.0f}", `Mean (SD)` = "{mean:.1f} ({sd:.2f})",
                          Median = "{median:.1f}", `Min, Max` = "{min:.0f}, {max:.0f}"),
            doc = "the rows of each continuous variable", study = "plan_cells.continuous"),
  tfl_param("labels", NULL, doc = "each variable's heading; given by name, merged (RACE: ~ drops one)")
)
names(.tpl_summary_params) <- vapply(.tpl_summary_params, `[[`, "", "name")

.tpl_summary_body <- function(a, ctx) {
  if (!length(a$variables) && !length(a$categorical)) {
    .ard_stop("template summary: give `variables` (continuous) or `categorical`, or both.")
  }
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
  if (!is.null(a$data_label) && identical(set, subj)) sets[[1L]]$label <- a$data_label
  nn <- function(x) Filter(Negate(is.null), x)
  analyses <- c(
    list(nn(list(id = "GROUPN", label = a$groupn_label, method = "categorical", data = subj,
                 variables = a$by, purpose = "PRIMARY OUTCOME MEASURE"))),
    if (length(a$variables)) list(list(
      id = a$analysis_id, label = a$analysis_label, method = "continuous", data = set,
      by = a$by, variables = a$variables, statistics = a$statistics,
      purpose = "PRIMARY OUTCOME MEASURE")),
    if (length(a$variables) && !is.null(a$ci_label)) list(list(
      id = a$ci_id, label = a$ci_label, method = a$ci_method, data = set, by = a$by,
      variables = a$variables, purpose = "SECONDARY OUTCOME MEASURE")),
    if (length(a$categorical)) list(nn(list(
      id = a$cat_id, label = a$cat_label, method = "categorical", data = set,
      by = a$by, variables = a$categorical,
      optional = if (!"categorical" %in% ctx$given) TRUE, statistics = a$cat_statistics,
      purpose = "PRIMARY OUTCOME MEASURE"))))
  vars <- c(a$variables, a$categorical)
  content <- c(
    list(tfl_item("table_plan", list(cols = a$by, rows = list(group = "variable")))),
    if (length(a$variables)) list(tfl_item("plan_cells", list(continuous = as.list(a$cells)))),
    # (no plan_levels(): the ARD says the order, as for demog)
    if (length(a$labels)) list(tfl_item("plan_labels", list(variable = as.list(a$labels[vars])))),
    # the look is the template's, not a parameter: a report changes it
    # with an item of its own (content:) or the study's style
    list(tfl_item("plan_paginate_rows", list(max_rows = 30L, split = "group_safe"))))
  list(ard = list(analysis_data = sets, analyses = analyses),
       data = list(source = "ard", object = "ard"), content = content)
}

.tfl_template("summary", kind = "table", layout = "summary",
              params = .tpl_summary_params, body = .tpl_summary_body)

# disposition: the subjects' status at the end of the study, on summary
.tfl_template("disposition", kind = "table", layout = "summary",
  params = c(.tpl_summary_params[c("population", "subset")],
             lapply(list(
               list("categorical", "EOSSTT"),
               list("cat_id", "DISP"),
               list("cat_label", "Status at end of study"),
               list("labels", c(EOSSTT = "Status at end of study, n (%)"))),
               function(x) tfl_param(x[[1L]], x[[2L]])),
             .tpl_summary_params[c("by", "cat_statistics", "groupn_label")]),
  body = function(a, ctx) {
    b <- lapply(.tpl_summary_params, `[[`, "default")
    b[names(a)] <- a
    # the template's own defaults are the summary's NULLs where it says so
    for (k in names(a)) if (is.null(a[[k]])) b[k] <- list(NULL)
    out <- .tpl_summary_body(b, ctx)
    # its own look: 24 rows a page, rtfreporter's page break
    k <- which(vapply(out$content, `[[`, "", "fn") == "plan_paginate_rows")
    out$content[[k]]$args <- list(max_rows = 24L)
    out
  })


#' Continuous variables by a column key: a table plan from the layout `summary`
#'
#' @inheritParams tpl_demog
#' @param variables The continuous variables, in order; `NULL` for none.
#' @param ... The template's other parameters: `categorical` (the
#'   categorical variables), `by`, `data`, `where`, `data_id`, `data_label`,
#'   `groupn_label`, `analysis_id`, `analysis_label`, `statistics`, `cat_id`,
#'   `cat_label`, `cat_statistics`, `cells`, `labels`.  The look (the rows a
#'   page ...) is the template's; a report changes it with an item of its own.
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
tpl_summary <- function(ard, population, variables = NULL, ..., codelists = NULL) {
  .tpl_object("summary", ard, c(list(population = population, variables = variables),
                                list(...)), codelists = codelists)
}

#' The templates
#'
#' Every table and figure template takes the same pair of filters, its
#' input form's first group with `population`: `subset` (which subjects: R
#' on the analysis set's ADSL) and, with records data, `where` (which
#' records).  Both are `NULL` by default; see [tfl_param()].
#'
#' @return A data frame, one row a template: `name`, `kind`, `layout`,
#'   `params` (its parameters, comma-separated), `package` (the package
#'   that registered it: tflspec, or a company's, [tfl_template()]) and
#'   `call` (the function its program calls, the A use).
#' @examples
#' tfl_templates()
#' @export
tfl_templates <- function() {
  nm <- sort(ls(.tpl_registry))
  t <- lapply(nm, .tpl_get, fn = "tfl_templates()")
  data.frame(name = nm, kind = vapply(t, `[[`, "", "kind"),
             layout = vapply(t, `[[`, "", "layout"),
             params = vapply(t, function(x) paste(names(x$params), collapse = ", "), ""),
             package = vapply(t, function(x) x$package %||% "tflspec", ""),
             call = vapply(t, function(x) x$call %||% paste0("tpl_", x$name), ""),
             from = vapply(t, function(x) x$from %||% NA_character_, ""),
             stringsAsFactors = FALSE)
}

# ---- hierarchical: a hierarchy's counts (SOC / PT), the layout ------------------

.tpl_hier_params <- list(
  tfl_param("population", required = TRUE, doc = "the analysis set's id (SAF)"),
  .tpl_subset_param(),
  tfl_param("hierarchy", required = TRUE, doc = "the levels, outer first"),
  tfl_param("by", "TRT01A", doc = "the column variable"),
  tfl_param("data", "ADAE", doc = "the dataset of records"),
  tfl_param("where", NULL, doc = "the records kept (R)"),
  tfl_param("data_id", NULL, doc = "the analysis data's id; NULL: <data>_<population>"),
  tfl_param("data_label", NULL, doc = "the analysis data's label"),
  tfl_param("analysis_id", "AE", doc = "the analysis's id"),
  tfl_param("analysis_label", NULL, doc = "the analysis's label"),
  tfl_param("overall", NULL, doc = "the any-record row's label; NULL: none"),
  tfl_param("sort", c(".overall", "group1", ".depth", "-n", "label"),
            type = "text", multiple = TRUE, doc = "the rows' order (plan_sort())"),
  tfl_param("heading", c("System Organ Class", "   Preferred Term"),
            doc = "the label column's heading, a line a level")
)
names(.tpl_hier_params) <- vapply(.tpl_hier_params, `[[`, "", "name")

.tpl_hier_body <- function(a, ctx) {
  pop <- a$population
  subj <- paste0("adsl_", tolower(pop))
  set <- a$data_id %||% paste0(tolower(a$data), "_", tolower(pop))
  d <- list(id = set, label = a$data_label, from = a$data, subjects = subj,
            where = if (!is.null(a$where)) tfl_r(a$where))
  ard <- list(
    analysis_data = list(
      list(id = subj, label = .tpl_set_label(pop, ctx$population), from = "ADSL",
           population = pop),
      Filter(Negate(is.null), d)),
    analyses = list(Filter(Negate(is.null), list(
      id = a$analysis_id, label = a$analysis_label, method = "hierarchical", data = set,
      by = a$by, variables = a$hierarchy, denominator = subj,
      args = if (!is.null(a$overall)) tfl_r("over_variables = TRUE"),
      purpose = "PRIMARY OUTCOME MEASURE"))))
  n <- length(a$hierarchy)
  content <- list(
    tfl_item("normalize_ard", Filter(Negate(is.null),
                                     list(hierarchy = a$hierarchy, overall = a$overall))),
    tfl_item("table_plan", list(
      cols = a$by,
      rows = stats::setNames(as.list(a$hierarchy[-n]), paste0("group", seq_len(n - 1L))),
      label = list(label = a$hierarchy[n]))),
    # (the columns' order is the ARD's: the code list applied when it is made)
    tfl_item("plan_sort", list(keys = a$sort)),
    tfl_item("plan_col_header", list(header = list(
      list(line = "1", cols = "row_label"),
      list(line = "1", cols = ".values", span = "each", text = "{col}\n(N={n})\nn (%)"),
      list(line = "2", cols = "row_label", text = paste(a$heading, collapse = "\n")),
      list(line = "2", cols = ".values")))),
    tfl_item("plan_paginate_rows", list(max_rows = 22L, split = "group_force")))
  list(ard = ard, data = list(source = "ard", object = "ard"), content = content)
}

.tfl_template("hierarchical", kind = "table", layout = "hierarchical",
              params = .tpl_hier_params, body = .tpl_hier_body)

# ae_soc_pt: treatment-emergent AEs by SOC and PT, on the hierarchical layout
.tfl_template("ae_soc_pt", kind = "table", layout = "hierarchical",
  params = c(list(tfl_param("population", required = TRUE,
                            doc = "the analysis set's id (SAF)"),
                  .tpl_subset_param()),
             lapply(list(
               list("hierarchy", c("AEBODSYS", "AEDECOD")),
               list("where", "TRTEMFL == \"Y\""),
               list("data_label", "Treatment-emergent AEs of the safety set"),
               list("analysis_id", "TEAE"),
               list("analysis_label", "TEAE by SOC / PT"),
               list("overall", "Any TEAE")),
               function(x) tfl_param(x[[1L]], x[[2L]])),
             .tpl_hier_params[c("by", "data", "data_id", "sort", "heading")]),
  body = .tpl_hier_body)

#' Adverse events by SOC and PT: a table plan from the template `ae_soc_pt`
#'
#' A display template on the `hierarchical` layout: treatment-emergent
#' events (`TRTEMFL == "Y"`), the subjects with any event first.
#'
#' @inheritParams tpl_demog
#' @param ... The template's other parameters: `hierarchy`, `where`,
#'   `overall`, `by`, `sort`, `heading` ...
#' @return A [rtfreporter::table_plan()].
#' @export
tpl_ae_soc_pt <- function(ard, population, ..., codelists = NULL) {
  .tpl_object("ae_soc_pt", ard, c(list(population = population), list(...)),
              codelists = codelists)
}

#' A hierarchy's counts: a table plan from the layout `hierarchical`
#'
#' @inheritParams tpl_demog
#' @param hierarchy The levels, outer first (`c("AEBODSYS", "AEDECOD")`).
#' @param ... The template's other parameters: `by`, `data`, `where`,
#'   `overall`, `sort`, `heading` ...
#' @return A [rtfreporter::table_plan()].
#' @export
tpl_hierarchical <- function(ard, population, hierarchy, ..., codelists = NULL) {
  .tpl_object("hierarchical", ard, c(list(population = population, hierarchy = hierarchy),
                                     list(...)), codelists = codelists)
}

# ---- ae_pt_freq: PTs alone, by decreasing frequency (>= x%) -----------------

.tpl_pt_params <- list(
  tfl_param("population", required = TRUE, doc = "the analysis set's id (SAF)"),
  .tpl_subset_param(),
  tfl_param("term", "AEDECOD", doc = "the term variable (the PT)"),
  tfl_param("by", "TRT01A", doc = "the column variable"),
  tfl_param("data", "ADAE", doc = "the dataset of records"),
  tfl_param("where", "TRTEMFL == \"Y\"", doc = "the records kept (R)"),
  tfl_param("data_id", NULL, doc = "the analysis data's id; NULL: <data>_<population>"),
  tfl_param("data_label", "Treatment-emergent AEs of the safety set",
            doc = "the analysis data's label"),
  tfl_param("analysis_id", "TEAE_PT", doc = "the analysis's id"),
  tfl_param("analysis_label", "TEAE by PT", doc = "the analysis's label"),
  tfl_param("overall", "Any TEAE", doc = "the any-record row's label; NULL: none"),
  tfl_param("min_pct", NULL, doc = "the PTs kept: this % of subjects or more; NULL: all"),
  tfl_param("min_pct_in", "any",
            doc = "where min_pct is met: \"any\" column, \"all\" of them, or the \"total\" column"),
  tfl_param("sort_by", NULL,
            doc = "the column (a value of `by`) whose counts order the PTs; NULL: the Total column with min_pct_in = \"total\", else the code list's first value of `by`, else all the columns"),
  tfl_param("heading", "Preferred Term", doc = "the label column's heading")
)
names(.tpl_pt_params) <- vapply(.tpl_pt_params, `[[`, "", "name")

.tpl_pt_body <- function(a, ctx) {
  in_ <- match.arg(a$min_pct_in, c("any", "all", "total"))
  pop <- a$population
  subj <- paste0("adsl_", tolower(pop))
  set <- a$data_id %||% paste0(tolower(a$data), "_", tolower(pop))
  d <- list(id = set, label = a$data_label, from = a$data, subjects = subj,
            where = if (!is.null(a$where)) tfl_r(a$where))
  ard <- list(
    analysis_data = list(
      list(id = subj, label = .tpl_set_label(pop, ctx$population), from = "ADSL",
           population = pop),
      Filter(Negate(is.null), d)),
    analyses = list(Filter(Negate(is.null), list(
      id = a$analysis_id, label = a$analysis_label, method = "hierarchical", data = set,
      by = a$by, variables = a$term, denominator = subj,
      args = if (!is.null(a$overall)) tfl_r("over_variables = TRUE"),
      purpose = "PRIMARY OUTCOME MEASURE"))))
  # the order: a column's subjects, most first (the PT's name when equal)
  col <- a$sort_by %||%
    (if (identical(in_, "total")) "Total" else .tpl_levels(a$by, ctx$codelists)[[a$by]][1L])
  key <- if (length(col) && !is.na(col)) sprintf("-n[%s]", col) else "-n"
  keep <- if (!is.null(a$min_pct)) {
    cond <- sprintf("p >= %s", format(a$min_pct / 100))
    if (identical(in_, "total")) cond <- sprintf("%s & %s == \"Total\"", cond, a$by)
    list(tfl_item("plan_filter", c(list(where = tfl_r(cond)),
                                   if (identical(in_, "all")) list(any = FALSE))))
  }
  content <- c(list(
    tfl_item("normalize_ard", Filter(Negate(is.null),
                                     list(hierarchy = a$term, overall = a$overall))),
    tfl_item("table_plan", list(cols = a$by, label = list(label = a$term))),
    tfl_item("plan_sort", list(keys = c(".overall", key, "label")))),
    keep,
    list(
      tfl_item("plan_col_header", list(header = list(
        list(line = "1", cols = "row_label"),
        list(line = "1", cols = ".values", span = "each", text = "{col}\n(N={n})\nn (%)"),
        list(line = "2", cols = "row_label", text = a$heading),
        list(line = "2", cols = ".values")))),
      tfl_item("plan_paginate_rows", list(max_rows = 22L))))
  list(ard = ard, data = list(source = "ard", object = "ard"), content = content)
}

.tfl_template("ae_pt_freq", kind = "table", layout = "hierarchical",
              params = .tpl_pt_params, body = .tpl_pt_body)

#' Adverse events by preferred term, most frequent first: the template `ae_pt_freq`
#'
#' A display template on the `hierarchical` layout with one level: the
#' treatment-emergent events' preferred terms (`AEDECOD`), the subjects
#' with any event first, then the terms by decreasing frequency -- in the
#' first group of the code list of `by`, or the column `sort_by` names --
#' and, with `min_pct`, only the terms of that percentage of subjects or
#' more: in any column (`min_pct_in = "any"`, as FDA's tables of common
#' adverse events), in every column (`"all"`) or in the Total column
#' (`"total"`).
#'
#' @inheritParams tpl_demog
#' @param ... The template's other parameters: `term`, `by`, `where`,
#'   `overall`, `min_pct`, `min_pct_in`, `sort_by`, `heading` ...
#' @return A [rtfreporter::table_plan()].
#' @examples
#' tfl_template_expand("ae_pt_freq", list(population = "SAF", min_pct = 5))$content
#' @export
tpl_ae_pt_freq <- function(ard, population, ..., codelists = NULL) {
  .tpl_object("ae_pt_freq", ard, c(list(population = population), list(...)),
              codelists = codelists)
}

# ---- ae_max_grade: SOC / PT by the maximum severity or grade -----------------

.tpl_mg_params <- list(
  tfl_param("population", required = TRUE, doc = "the analysis set's id (SAF)"),
  .tpl_subset_param(),
  tfl_param("grade", "AESEV", doc = "the severity or grade variable (AESEV, AETOXGR)"),
  tfl_param("grade_levels", NULL,
            doc = "its values, lowest first; NULL: the code list of `grade`"),
  tfl_param("grade_in", "rows", doc = "the grades as \"rows\" under each term, or as \"cols\" in each group"),
  tfl_param("hierarchy", c("AEBODSYS", "AEDECOD"), doc = "the levels, outer first"),
  tfl_param("by", "TRT01A", doc = "the column variable"),
  tfl_param("data", "ADAE", doc = "the dataset of records"),
  tfl_param("where", "TRTEMFL == \"Y\"", doc = "the records kept (R)"),
  tfl_param("data_id", NULL, doc = "the analysis data's id; NULL: <data>_<population>"),
  tfl_param("overall", "Any TEAE", doc = "the any-record row's label"),
  tfl_param("any_grade", "Any grade", doc = "the label of a term's count at any grade"),
  tfl_param("sort", "n", type = "choice", choices = c("n", "name"),
            doc = "the SOCs and PTs by their count, most first (\"n\"), or by \"name\""),
  tfl_param("heading", c("System Organ Class", "   Preferred Term"),
            doc = "the label column's heading, a line a level")
)
names(.tpl_mg_params) <- vapply(.tpl_mg_params, `[[`, "", "name")

.tpl_mg_body <- function(a, ctx) {
  rows_in <- identical(match.arg(a$grade_in, c("rows", "cols")), "rows")
  pop <- a$population
  subj <- paste0("adsl_", tolower(pop))
  set <- a$data_id %||% paste0(tolower(a$data), "_", tolower(pop))
  g <- a$grade
  h <- a$hierarchy
  lv <- a$grade_levels %||% .tpl_levels(g, ctx$codelists)[[g]]
  # a record's rank: its grade among the levels (lowest first)
  rank <- if (length(lv)) {
    sprintf("match(%s, c(%s))", g, paste0("\"", lv, "\"", collapse = ", "))
  } else sprintf("xtfrm(%s)", g)
  # each subject's record of the highest grade: overall, in each SOC, in each PT
  top <- function(suffix, within) {
    list(id = paste0(set, "_max", suffix),
         code = sprintf("dplyr::slice_max(%s, %s, by = c(%s), n = 1, with_ties = FALSE)",
                        set, rank, paste(c("USUBJID", within), collapse = ", ")))
  }
  maxes <- list(top("", character()), top("_soc", h[1L]), top("_pt", h))
  # the overall maximum counted as one more SOC, the overall row's own
  # value: its grades join that row, as rows or as columns
  maxes[[1L]]$code <- sprintf("dplyr::mutate(%s, %s = \"%s\")", maxes[[1L]]$code, h[1L],
                              a$overall)
  d <- list(id = set, from = a$data, subjects = subj,
            where = if (!is.null(a$where)) tfl_r(a$where))
  an <- function(id, data, vars, over = FALSE) Filter(Negate(is.null), list(
    id = id, method = "hierarchical", data = data, by = a$by, variables = vars,
    denominator = subj, args = if (over) tfl_r("over_variables = TRUE"),
    purpose = "PRIMARY OUTCOME MEASURE"))
  ard <- list(
    analysis_data = c(list(
      list(id = subj, label = .tpl_set_label(pop, ctx$population), from = "ADSL",
           population = pop),
      Filter(Negate(is.null), d)), lapply(maxes, function(m) Filter(Negate(is.null), m))),
    analyses = list(
      an("AE_MAX", maxes[[1L]]$id, c(h[1L], g)),
      an("AE_MAX_SOC", maxes[[2L]]$id, c(h[1L], g)),
      an("AE_MAX_PT", maxes[[3L]]$id, c(h, g))))
  n <- length(h)
  by_n <- identical(match.arg(a$sort, c("n", "name")), "n")
  # the terms' row keys (as rows: every level; as columns: all but the PT,
  # the label); each term after its own count (most first), or by name
  grp <- paste0("group", seq_len(if (rows_in) n else n - 1L))
  term_keys <- unlist(lapply(grp, function(k) c(if (by_n) sprintf("-n@%s", k), k)))
  keys <- c("levels(group1)", term_keys, ".depth",
            if (!rows_in) c(if (by_n) "-n", "label"))
  plan <- if (rows_in) {
    list(tfl_item("table_plan", list(
      cols = a$by, rows = stats::setNames(as.list(h), grp), label = list(label = g))))
  } else {
    list(tfl_item("table_plan", list(
           cols = c(a$by, g), rows = stats::setNames(as.list(h[-n]), grp),
           label = list(label = h[n]))),
         # each group's count at any grade: its records with no grade
         tfl_item("plan_total", list(label = a$any_grade, position = "first")))
  }
  content <- c(
    list(tfl_item("normalize_ard", list(hierarchy = c(h, g))),
         # the overall block first, whatever the order of the others
         tfl_item("plan_levels", stats::setNames(list(a$overall), h[1L]))),
    plan,
    list(tfl_item("plan_sort", list(keys = keys))),
    list(tfl_item("plan_col_header", list(header = list(
           list(line = "1", cols = "row_label"),
           list(line = "1", cols = ".values", span = "each", text = "{col}\n(N={n})\nn (%)"),
           list(line = "2", cols = "row_label", text = paste(a$heading, collapse = "\n")),
           list(line = "2", cols = ".values")))),
         tfl_item("plan_paginate_rows", list(max_rows = 22L, split = "group_force"))))
  list(ard = ard, data = list(source = "ard", object = "ard"), content = content)
}

.tfl_template("ae_max_grade", kind = "table", layout = "hierarchical",
              params = .tpl_mg_params, body = .tpl_mg_body)

#' Adverse events by SOC and PT at the maximum severity or grade: the template `ae_max_grade`
#'
#' A display template on the `hierarchical` layout: each subject counted
#' once at the highest severity (`AESEV`) or grade (`AETOXGR`) of their
#' treatment-emergent events -- overall, in each system organ class and in
#' each preferred term.  The grades are rows under each term
#' (`grade_in = "rows"`, as FDA's tables and chevron's AET03), or columns in
#' each group (`"cols"`); a term's count at any grade is its own row or
#' column (`any_grade`).  The grades' order is `grade_levels`, or the code
#' list of `grade`.
#'
#' @inheritParams tpl_demog
#' @param ... The template's other parameters: `grade`, `grade_levels`,
#'   `grade_in`, `hierarchy`, `by`, `where`, `overall`, `any_grade`,
#'   `heading` ...
#' @return A [rtfreporter::table_plan()].
#' @examples
#' x <- tfl_template_expand("ae_max_grade", list(population = "SAF",
#'                          grade_levels = c("MILD", "MODERATE", "SEVERE")))
#' vapply(x$ard$analysis_data, `[[`, "", "id")
#' @export
tpl_ae_max_grade <- function(ard, population, ..., codelists = NULL) {
  .tpl_object("ae_max_grade", ard, c(list(population = population), list(...)),
              codelists = codelists)
}

#' Disposition: a table plan from the template `disposition`
#'
#' A display template on the `summary` layout: the subjects' status at the
#' end of the study (`EOSSTT`), n (%).
#'
#' @inheritParams tpl_demog
#' @param ... The template's other parameters: `categorical`, `by`,
#'   `cat_statistics` ...
#' @return A [rtfreporter::table_plan()].
#' @export
tpl_disposition <- function(ard, population, ..., codelists = NULL) {
  .tpl_object("disposition", ard, c(list(population = population), list(...)),
              codelists = codelists)
}

# ---- shift: a category at baseline by the same after it, the layout -------------

.tpl_shift_params <- list(
  tfl_param("population", required = TRUE, doc = "the analysis set's id (SAF)"),
  .tpl_subset_param(),
  tfl_param("by", "TRT01A", doc = "the column variable"),
  tfl_param("data", "ADLB", doc = "the dataset of records"),
  tfl_param("where", NULL, doc = "the records kept (R): the visit, the parameters"),
  tfl_param("data_id", NULL, doc = "the analysis data's id; NULL: <data>_<population>"),
  tfl_param("analysis_id", "SHIFT", doc = "the analysis's id"),
  tfl_param("param", "PARAMCD", type = "vars",
            doc = "the parameter's variable (a block of rows each)"),
  tfl_param("baseline", "BNRIND", doc = "the category at baseline (the rows)"),
  tfl_param("post", "ANRIND", doc = "the category after it (the columns under each arm)"),
  tfl_param("levels", c("LOW", "NORMAL", "HIGH"), doc = "the categories, in order"),
  tfl_param("by_levels", NULL,
            doc = "the column variable's values in order; NULL: the ARD's (its code list)"),
  tfl_param("param_labels", NULL,
            doc = "each parameter's heading (a named vector); given by name, merged"),
  tfl_param("cell", "{n}", doc = "a cell's template"),
  tfl_param("heading", "Parameter / Baseline", doc = "the label column's heading")
)
names(.tpl_shift_params) <- vapply(.tpl_shift_params, `[[`, "", "name")

.tpl_shift_body <- function(a, ctx) {
  set <- a$data_id %||% paste0(tolower(a$data), "_", tolower(a$population))
  ard <- list(
    analysis_data = list(Filter(Negate(is.null), list(
      id = set, from = a$data, population = a$population,
      where = if (!is.null(a$where)) tfl_r(a$where)))),
    analyses = list(list(id = a$analysis_id, method = "categorical", data = set,
                         by = c(a$by, a$param, a$baseline), variables = a$post)))
  # the columns: the ARD's order (the code list applied when it is made),
  # or `by_levels` when the report says them
  by_lv <- a$by_levels
  lv <- c(if (length(by_lv)) stats::setNames(list(by_lv), a$by),
          stats::setNames(list(a$levels, a$levels), c(a$baseline, ".label")))
  content <- c(
    list(tfl_item("table_plan", list(cols = c(a$by, ".label"),
                                     rows = list(group = a$param), label = a$baseline)),
         tfl_item("plan_cells", list(categorical = a$cell)),
         tfl_item("plan_levels", lv)),
    if (length(a$param_labels)) list(tfl_item("plan_labels", stats::setNames(
      list(as.list(a$param_labels)), a$param))),
    list(tfl_item("plan_stub", list(vars = c("group", a$baseline), name = "row_label",
                                    before = TRUE)),
         tfl_item("plan_col_header", list(header = list(
           list(line = "1", cols = ".values", span = a$by, text = "{col1}"),
           list(line = "2", cols = "row_label", text = a$heading),
           list(line = "2", cols = ".values", span = "each", text = "{col}"))))))
  list(ard = ard, data = list(source = "ard"), content = content)
}

.tfl_template("shift", kind = "table", layout = "shift",
              params = .tpl_shift_params, body = .tpl_shift_body)

# lab_shift: the normal range indicator at baseline by a visit's, on shift
.tfl_template("lab_shift", kind = "table", layout = "shift",
  params = c(.tpl_shift_params[c("population", "subset")],
             list(tfl_param("visit", required = TRUE, doc = "the visit (AVISIT)"),
                  tfl_param("params", required = TRUE, doc = "the PARAMCD values"),
                  .tpl_where_param(doc = "the records kept besides the visit and parameters (R)")),
             .tpl_shift_params[c("by", "data_id", "by_levels", "param_labels", "levels")]),
  body = function(a, ctx) {
    b <- lapply(.tpl_shift_params, `[[`, "default")
    for (k in intersect(names(a), names(b))) b[k] <- list(a[[k]])
    b$where <- .tpl_and(sprintf("AVISIT == \"%s\" & PARAMCD %%in%% c(%s)", a$visit,
                                paste(sprintf("\"%s\"", a$params), collapse = ", ")),
                        a$where)
    .tpl_shift_body(b, ctx)
  })

#' Shift tables: table plans from the templates `shift` and `lab_shift`
#'
#' `shift` is the layout: a category at baseline (the rows, a block a
#' parameter) by the same category after it (the columns, under each arm).
#' `lab_shift` is the display template on it: the laboratory normal range
#' indicator (`BNRIND` by `ANRIND`) at one visit, for some parameters.
#'
#' @inheritParams tpl_demog
#' @param ... The template's other parameters (`tpl_shift()`: `by`, `data`,
#'   `where`, `param`, `baseline`, `post`, `levels`, `param_labels`,
#'   `heading` ...; `tpl_lab_shift()`: `visit`, `params`, `by`,
#'   `param_labels` ...).
#' @return A [rtfreporter::table_plan()].
#' @export
tpl_shift <- function(ard, population, ..., codelists = NULL) {
  .tpl_object("shift", ard, c(list(population = population), list(...)),
              codelists = codelists)
}

#' @rdname tpl_shift
#' @export
tpl_lab_shift <- function(ard, population, ..., codelists = NULL) {
  .tpl_object("lab_shift", ard, c(list(population = population), list(...)),
              codelists = codelists)
}

# ---- listing: one row a record, the layout ---------------------------------------

.tfl_template("listing", kind = "listing", layout = "listing",
  params = list(
    tfl_param("data", required = TRUE, doc = "the dataset (ADAE)"),
    tfl_param("columns", required = TRUE,
              doc = paste("the columns, in order: each a list of vars (one or more),",
                          "label, width and collapse_repeats")),
    tfl_param("where", NULL, doc = "the records kept (R)"),
    tfl_param("subset", NULL, doc = paste(
      "which subjects (R, on ADSL): a listing has no analysis set, so ADSL's",
      "subjects it keeps (a semi join)")),
    tfl_param("sort", NULL, type = "vars", multiple = TRUE, doc = "the records' order (variables)")
  ),
  body = function(a, ctx) {
    # a column's width is part of its definition (each column its own), the
    # one look a listing template takes; the rest of the look is the
    # template's (multiline, 22 rows a page) or the study's style
    cols <- lapply(a$columns, function(cl) {
      args <- Filter(Negate(is.null), list(
        vars = as.character(unlist(cl$vars)), label = cl$label,
        width = if (!is.null(cl$width)) as.integer(cl$width),
        collapse_repeats = if (isTRUE(cl$collapse_repeats)) TRUE))
      tfl_call("listing_col", args)
    })
    data <- Filter(Negate(is.null), list(
      source = "dataset", dataset = a$data,
      where = if (!is.null(a$where)) tfl_r(a$where), sort = a$sort))
    content <- list(
      tfl_item("plan_listing", list(cols = cols, type = "multiline")),
      tfl_item("plan_paginate_rows", list(max_rows = 22L)))
    list(data = data, content = content)
  })

#' A listing: a listing plan from the layout `listing`
#'
#' One row a record of a dataset: the records kept (`where`), their order
#' (`sort`) and the columns, each its variables (one or more, stacked in the
#' cell), its heading, its width and whether a repeated value is printed
#' once.  The look is the template's: one record over lines as needed
#' (`multiline`) and 22 rows a page, which a study's style or a report's own
#' item changes.
#'
#' @param x The dataset (a data frame), the listing's source.
#' @param ... The template's parameters: `columns` (a list of columns, each
#'   `list(vars = , label = , width = , collapse_repeats = )`), `where`,
#'   `sort`; `data` names the dataset in the spec (`"ADAE"`).
#' @return A [rtfreporter::table_plan()] of a listing.
#' @export
tpl_listing <- function(x, ...) {
  a <- list(...)
  it <- tfl_template_expand("listing", a)
  # `x` is the data as the program made it (its subjects kept already)
  it$data$steps <- NULL
  o <- tfl_output("tpl-listing", kind = "listing", extends = "none",
                  data = it$data, content = it$content)
  tfl_content_run(o, x)
}

# ---- km_table: Kaplan-Meier estimates (median, rates by time), from the ARD -----
# The ARD's survival analyses are the report's own (`ard:`, cardx's
# ard_survival_survfit()): the template does ARD -> table only.

.tfl_template("km_table", kind = "table", layout = "km_table",
  params = list(
    tfl_param("by", "TRT01A", doc = "the column variable"),
    tfl_param("by_levels", NULL, doc = "its values in order; NULL: its code list's"),
    tfl_param("median_label", "Median (95% CI)", doc = "the median's row"),
    tfl_param("time_prefix", "Day", doc = "a rate's row: <prefix> <time>"),
    tfl_param("time_unit", "days", doc = "the times' unit, in the median block's heading"),
    tfl_param("drop_time_0", TRUE, doc = "leave out the rate at time 0")
  ),
  body = function(a, ctx) {
    step <- paste(c(
      "data <- normalize_ard(ard)",
      sprintf("# row labels: the median, and the event-free probability by %s%s",
              tolower(a$time_prefix), if (isTRUE(a$drop_time_0)) " (day 0 left out)" else ""),
      "is_time <- data$variable == \"time\"",
      sprintf("data$.label[data$variable == \"prob\"] <- \"%s\"", a$median_label),
      sprintf("data$.label[is_time] <- paste(\"%s\", as.character(data$variable_level[is_time]))",
              a$time_prefix),
      if (isTRUE(a$drop_time_0))
        "data <- data[!(is_time & as.character(data$variable_level) == \"0\"), ]"),
      collapse = "\n")
    # (the arms' order: an order given, else the code list's, which the ARD
    # program puts on the survfit / CI analysis too, relevel_ard())
    by_lv <- a$by_levels
    content <- list(
      tfl_item("table_plan", list(cols = a$by, rows = list(group = "variable"))),
      tfl_item("plan_cells", list(
        prob = list(tfl_r("is.na(estimate) ~ \"NE\""),
                    tfl_r("is.na(conf.high) ~ \"{estimate:.0f} ({conf.low:.0f}, NE)\""),
                    "{estimate:.0f} ({conf.low:.0f}, {conf.high:.0f})"),
        time = "{estimate:.1f%} ({conf.low:.1f%}, {conf.high:.1f%})")),
      tfl_item("plan_levels", c(list(variable = c("prob", "time")),
                                if (length(by_lv)) stats::setNames(list(by_lv), a$by))),
      tfl_item("plan_labels", list(variable = list(
        prob = sprintf("Time to first event (%s)", a$time_unit),
        time = "Event-free probability (95% CI)"))),
      tfl_item("plan_col_header", list(header = list(
        list(line = "1", cols = "row_label"),
        list(line = "1", cols = ".values", span = "each", text = "{col}"),
        list(line = "2", cols = "row_label"),
        list(line = "2", cols = ".values", span = "each", text = "(N={n})")))))
    list(data = list(source = "ard", object = "ard", steps = list(tfl_r(step))),
         content = content)
  })

#' Kaplan-Meier estimates: a table plan from the template `km_table`
#'
#' The median time to the event (95% CI) and the event-free probability
#' (95% CI) at each time, by arm, from an ARD of cardx's
#' `ard_survival_survfit()` (`probs = 0.5` and `times = `): the analyses are
#' the report's own, the template is the ARD to the table.
#'
#' @param ard The study ARD's rows of this report (not normalized: the
#'   template's data step normalizes it).
#' @param ... The template's parameters: `by`, `by_levels`, `median_label`,
#'   `time_prefix`, `time_unit`, `drop_time_0`.
#' @param codelists The report's code lists; `NULL` takes the ARD's.
#' @return A [rtfreporter::table_plan()].
#' @export
tpl_km_table <- function(ard, ..., codelists = NULL) {
  .tpl_object("km_table", ard, list(...), codelists = codelists)
}

# vs_change: a vital sign's baseline, value and change at one visit, on summary
.tfl_template("vs_change", kind = "table", layout = "summary",
  params = list(
    tfl_param("population", required = TRUE, doc = "the analysis set's id (SAF)"),
    .tpl_subset_param(),
    tfl_param("param", required = TRUE, type = "levels", of = "PARAMCD",
              doc = "the PARAMCD (SYSBP)"),
    tfl_param("visit", required = TRUE, doc = "the AVISIT (CYCLE 9 DAY 1)"),
    tfl_param("param_label", required = TRUE, doc = "the parameter in words"),
    tfl_param("when", required = TRUE, doc = "the visit in words, short (week 24)"),
    tfl_param("visit_label", required = TRUE, doc = "the value's row heading's visit"),
    tfl_param("unit", required = TRUE, doc = "the unit (mmHg)"),
    tfl_param("by", "TRTA", doc = "the column variable"),
    .tpl_where_param(doc = "the records kept besides the parameter and visit (R)")
  ),
  body = function(a, ctx) {
    b <- lapply(.tpl_summary_params, `[[`, "default")
    set_lab <- tolower(.tpl_set_label(a$population, ctx$population))
    b[c("population", "by", "data", "where", "data_id", "data_label", "analysis_id",
        "analysis_label", "variables", "statistics", "cells", "labels")] <- list(
      a$population, a$by, "ADVS",
      .tpl_and(sprintf("PARAMCD == \"%s\" & AVISIT == \"%s\"", a$param, a$visit), a$where),
      paste0("advs_", tolower(a$population), "_", tolower(a$param)),
      sprintf("%s at %s, %s", a$param_label, a$when, set_lab),
      a$param, sprintf("%s at %s", a$param, a$when),
      c("BASE", "AVAL", "CHG"),
      c("N", "mean", "sd", "se", "mean_lcl", "mean_ucl", "median", "min", "max"),
      list(n = "{N:.0f}", `Mean (SD)` = "{mean:.1f} ({sd:.2f})", SE = "{se:.2f}",
           `95% CI of the mean` = "{mean_lcl:.1f}, {mean_ucl:.1f}", Median = "{median:.1f}",
           `Min, Max` = "{min:.0f}, {max:.0f}"),
      c(BASE = sprintf("Baseline (%s)", a$unit),
        AVAL = sprintf("%s (%s)", a$visit_label, a$unit),
        CHG = sprintf("Change from baseline (%s)", a$unit)))
    .tpl_summary_body(b, ctx)
  })

#' A vital sign's change: a table plan from the template `vs_change`
#'
#' A display template on the `summary` layout: a parameter's baseline,
#' value and change from baseline at one visit (n, mean (SD), SE, 95% CI of
#' the mean, median, min, max).
#'
#' @inheritParams tpl_demog
#' @param ... The template's parameters: `param`, `visit`, `param_label`,
#'   `when`, `visit_label`, `unit`, `by`.
#' @return A [rtfreporter::table_plan()].
#' @export
tpl_vs_change <- function(ard, population, ..., codelists = NULL) {
  .tpl_object("vs_change", ard, c(list(population = population), list(...)),
              codelists = codelists)
}

# ---- param_change: parameters' value and change from baseline at each visit -------

.tpl_pc_params <- list(
  tfl_param("population", required = TRUE, doc = "the analysis set's id (SAF)"),
  .tpl_subset_param(),
  tfl_param("data", required = TRUE, doc = "the BDS dataset (ADLB, ADVS, ADEG)"),
  tfl_param("params", NULL, doc = "the PARAMCD values, in the data's order; NULL: all"),
  tfl_param("visits", NULL, doc = "the AVISIT values; NULL: all"),
  tfl_param("where", NULL, doc = "the records kept (R), e.g. ANL01FL == \"Y\""),
  tfl_param("by", "TRT01A", doc = "the column variable"),
  tfl_param("layout", "cols", doc = paste(
    "cols: each arm's value and change side by side;",
    "rows: the value's rows, then the change's, under each visit")),
  tfl_param("param_order", "PARAMN", doc = "the parameters' order variable; NULL: by name"),
  tfl_param("visit_order", "AVISITN", doc = "the visits' order variable; NULL: by name"),
  tfl_param("baseline", FALSE, type = "flag",
            doc = "TRUE: the baseline's rows (BASE) before the value's"),
  tfl_param("baseline_label", "Baseline", doc = "the baseline's heading"),
  tfl_param("value_label", "Value", doc = "the value's heading"),
  tfl_param("change_label", "Change from baseline", doc = "the change's heading"),
  tfl_param("data_id", NULL, doc = "the analysis data's id; NULL: <data>_<population>"),
  tfl_param("data_label", NULL, doc = "the analysis data's label"),
  tfl_param("statistics", c("N", "mean", "sd", "median", "min", "max"),
            doc = "the statistics the ARD has"),
  tfl_param("cells", list(n = "{N:.0f}", `Mean (SD)` = "{mean:.1f} ({sd:.2f})",
                          Median = "{median:.1f}", `Min, Max` = "{min:.1f}, {max:.1f}"),
            doc = "the rows of a value or a change", study = "plan_cells.continuous")
)
names(.tpl_pc_params) <- vapply(.tpl_pc_params, `[[`, "", "name")

.tpl_pc_body <- function(a, ctx) {
  rows_in <- identical(match.arg(a$layout, c("cols", "rows")), "rows")
  pop <- a$population
  subj <- paste0("adsl_", tolower(pop))
  set <- a$data_id %||% paste0(tolower(a$data), "_", tolower(pop))
  inn <- function(v, x) if (length(x)) {
    sprintf("%s %%in%% c(%s)", v, paste0("\"", x, "\"", collapse = ", "))
  }
  where <- c(a$where, inn("PARAMCD", a$params), inn("AVISIT", a$visits))
  # the parameters and visits as factors in their order: the ARD keeps it
  ord <- c(if (length(a$param_order))
             sprintf("PARAM = reorder(factor(PARAM), %s)", a$param_order),
           if (length(a$visit_order))
             sprintf("AVISIT = reorder(factor(AVISIT), %s)", a$visit_order))
  nn <- function(x) Filter(Negate(is.null), x)
  sets <- list(
    list(id = subj, label = .tpl_set_label(pop, ctx$population), from = "ADSL",
         population = pop),
    nn(list(id = set, label = a$data_label, from = a$data, subjects = subj,
            where = if (length(where)) tfl_r(paste(where, collapse = " & ")),
            derive = if (length(ord)) tfl_r(paste(ord, collapse = " | ")))),
    # the change: the records that have one (no baseline visit's, so its
    # change cells stay empty)
    nn(list(id = paste0(set, "_chg"), from = set, where = tfl_r("!is.na(CHG)"),
            derive = if (length(a$visit_order)) tfl_r("AVISIT = droplevels(AVISIT)"))))
  an <- function(id, data, v) list(
    id = id, method = "continuous", data = data, by = c(a$by, "PARAM", "AVISIT"),
    variables = v, statistics = a$statistics, purpose = "PRIMARY OUTCOME MEASURE")
  analyses <- c(
    list(list(id = "GROUPN", method = "categorical", data = subj, variables = a$by,
              purpose = "PRIMARY OUTCOME MEASURE")),
    if (isTRUE(a$baseline)) list(an("BASELINE", set, "BASE")),
    list(an("VALUE", set, "AVAL"), an("CHANGE", paste0(set, "_chg"), "CHG")))
  plan <- if (rows_in) {
    list(tfl_item("table_plan", list(cols = a$by, rows = list(
           group1 = "PARAM", group2 = "AVISIT", group3 = "variable"))),
         tfl_item("plan_cells", list(continuous = as.list(a$cells))),
         tfl_item("plan_sort", list(keys = c("group1", "group2", "group3"))),
         tfl_item("plan_col_header", list(header = list(
           list(line = "1", cols = ".values", span = "each", text = "{col}
(N={n})")))))
  } else {
    list(tfl_item("table_plan", list(cols = c(a$by, "variable"), rows = list(
           group1 = "PARAM", group2 = "AVISIT"))),
         tfl_item("plan_cells", list(continuous = as.list(a$cells))),
         # the arm and its N over its two columns (the N is the arm's, not
         # a value's or a change's)
         tfl_item("plan_col_header", list(header = list(
           list(line = "1", cols = ".values", span = a$by, text = "{col1}
(N={n})"),
           list(line = "2", cols = ".values", span = "each", text = "{col}")))))
  }
  content <- c(plan, list(
    tfl_item("plan_labels", list(variable = c(
      if (isTRUE(a$baseline)) list(BASE = a$baseline_label),
      list(AVAL = a$value_label, CHG = a$change_label)))),
    tfl_item("plan_paginate_rows", list(max_rows = 30L, split = "group_safe"))))
  list(ard = list(analysis_data = sets, analyses = analyses),
       data = list(source = "ard", object = "ard"), content = content)
}

.tfl_template("param_change", kind = "table", layout = "summary",
              params = .tpl_pc_params, body = .tpl_pc_body)

#' Parameters' value and change: a table plan from the template `param_change`
#'
#' Laboratory tests, vital signs or ECG parameters at each visit: n, mean
#' (SD), median and min, max of the value (`AVAL`) and of its change from
#' baseline (`CHG`), a block a parameter (`PARAM`, its heading with its
#' unit) and a visit (`AVISIT`).  The parameters and visits are in the
#' order of `PARAMN` and `AVISITN`.  The change is read from the records
#' that have one: the baseline visit's change cells stay empty.
#'
#' `layout = "cols"` puts each arm's value and change side by side;
#' `"rows"` puts the change's rows under the value's, in one column an arm;
#' `baseline = TRUE` puts the baseline's (`BASE`) before them.
#'
#' @inheritParams tpl_demog
#' @param ... The template's other parameters: `data` (required), `params`,
#'   `visits`, `where`, `by`, `layout`, `param_order`, `visit_order`,
#'   `baseline`, `baseline_label`, `value_label`, `change_label`, `statistics`,
#'   `cells` ...
#' @return A [rtfreporter::table_plan()].
#' @export
tpl_param_change <- function(ard, population, ..., codelists = NULL) {
  .tpl_object("param_change", ard, c(list(population = population), list(...)),
              codelists = codelists)
}

# ---- orr: best overall response and the objective response rate (95% CI) --------

.tfl_template("orr", kind = "table", layout = "summary",
  params = list(
    tfl_param("population", "ITT", doc = "the analysis set's id"),
    .tpl_subset_param(),
    tfl_param("by", "TRT01P", doc = "the column variable"),
    tfl_param("by_levels", NULL, doc = "its values in order; NULL: its code list's"),
    tfl_param("data", "ADRS", doc = "the response dataset"),
    tfl_param("param", "BOR", type = "levels", of = "PARAMCD",
              doc = "the PARAMCD of the best overall response"),
    tfl_param("category", "AVALC", doc = "the response category's variable"),
    tfl_param("responders", c("CR", "PR"), doc = "the categories that respond"),
    .tpl_where_param(doc = "the records kept besides the parameter (R)"),
    tfl_param("data_id", NULL, doc = "the analysis data's id; NULL: <data>_<param>"),
    tfl_param("ci_method", "clopper-pearson", doc = "the rate's CI (cardx's method)"),
    tfl_param("heading", "Response", doc = "the label column's heading")
  ),
  body = function(a, ctx) {
    set <- a$data_id %||% paste0(tolower(a$data), "_", tolower(a$param))
    resp <- paste(a$responders, collapse = " + ")
    ard <- list(
      analysis_data = list(list(
        id = set, from = a$data, population = a$population,
        where = tfl_r(.tpl_and(sprintf("PARAMCD == \"%s\"", a$param), a$where)),
        derive = tfl_r(sprintf("RESP = %s %%in%% c(%s)", a$category,
                               paste(sprintf("\"%s\"", a$responders), collapse = ", "))))),
      analyses = list(
        list(id = a$param, method = "categorical", data = set, by = a$by,
             variables = a$category),
        list(id = "ORR", method = "proportion_ci", data = set, by = a$by,
             variables = "RESP", args = tfl_r(sprintf("method = \"%s\"", a$ci_method)))))
    # (the arms' order: an order given, else the code list's, which the ARD
    # program puts on the survfit / CI analysis too, relevel_ard())
    by_lv <- a$by_levels
    content <- list(
      tfl_item("table_plan", list(cols = a$by, rows = list(group = "variable"))),
      tfl_item("plan_cells", list(proportion_ci = list(
        `n (%)` = "{n} ({estimate:.1f%})", `95% CI` = "[{conf.low:.1f%}, {conf.high:.1f%}]"))),
      # (the categories' order is the ARD's: its code list, applied in the ARD program)
      tfl_item("plan_levels", c(list(variable = c(a$category, "RESP")),
                                if (length(by_lv)) stats::setNames(list(by_lv), a$by))),
      tfl_item("plan_labels", list(variable = stats::setNames(
        list("Best overall response, n (%)", sprintf("Objective response rate (%s)", resp)),
        c(a$category, "RESP")))),
      tfl_item("plan_col_header", list(header = list(
        list(line = "1", cols = ".values", span = "each", text = "{col}"),
        list(line = "2", cols = "row_label", text = a$heading),
        list(line = "2", cols = ".values", span = "each", text = "(N={n})")))))
    list(ard = ard, data = list(source = "ard"), content = content)
  })

#' Objective response: a table plan from the template `orr`
#'
#' The best overall response (n (%)) and the objective response rate with
#' its 95% CI, by arm.
#'
#' @inheritParams tpl_demog
#' @param ... The template's other parameters: `by`, `by_levels`, `data`,
#'   `param`, `category`, `responders`, `ci_method`, `heading`.
#' @return A [rtfreporter::table_plan()].
#' @export
tpl_orr <- function(ard, population = "ITT", ..., codelists = NULL) {
  .tpl_object("orr", ard, c(list(population = population), list(...)),
              codelists = codelists)
}

# ---- ae_overview: the subjects with a TEAE of each kind, on summary ------------

.tpl_ae_flags <- list(
  list(id = "ANY", label = "Any TEAE"),
  list(id = "REL", label = "Any treatment-related TEAE", where = "AEREL == \"RELATED\""),
  list(id = "G3", label = "Any TEAE of CTCAE grade 3 or higher",
       where = "AETOXGR %in% c(\"3\", \"4\", \"5\")"),
  list(id = "SER", label = "Any serious TEAE", where = "AESER == \"Y\""),
  list(id = "DISC", label = "Any TEAE leading to discontinuation of study drug",
       where = "AEACN == \"DRUG WITHDRAWN\""),
  list(id = "DTH", label = "Any TEAE leading to death", where = "AEOUT == \"FATAL\""))

.tfl_template("ae_overview", kind = "table", layout = "summary",
  params = list(
    tfl_param("population", "SAF", doc = "the analysis set's id"),
    .tpl_subset_param(),
    tfl_param("by", "TRT01A", doc = "the column variable"),
    tfl_param("data", "ADAE", doc = "the dataset of events"),
    tfl_param("where", "TRTEMFL == \"Y\"", doc = "the events kept (R): the treatment-emergent"),
    tfl_param("data_id", NULL, doc = "the events' analysis data's id; NULL: <data>_<population>"),
    tfl_param("flags", .tpl_ae_flags, doc = paste(
      "the rows, in order: each `id` (the flag's name), `label` (the row's text)",
      "and `where` (R: the events it counts; none: every event kept)")),
    tfl_param("heading", "Subjects with", doc = "the label column's heading")
  ),
  body = function(a, ctx) {
    pop <- a$population
    subj <- paste0("adsl_", tolower(pop))
    set <- a$data_id %||% paste0(tolower(a$data), "_", tolower(pop))
    ids <- vapply(a$flags, `[[`, "", "id")
    # one `subjects` analysis a row: has the subject an event of the data
    # that the row's `where` keeps?
    flag <- function(f) Filter(Negate(is.null), list(
      id = f$id, label = f$label, method = "subjects", data = set,
      where = if (!is.null(f$where)) tfl_r(f$where), by = a$by, variables = f$id,
      purpose = "PRIMARY OUTCOME MEASURE"))
    ard <- list(
      analysis_data = list(
        list(id = subj, label = .tpl_set_label(pop, ctx$population), from = "ADSL",
             population = pop),
        Filter(Negate(is.null), list(id = set, from = a$data, subjects = subj,
                                     where = if (!is.null(a$where)) tfl_r(a$where)))),
      analyses = c(
        list(list(id = "GROUPN", method = "categorical", data = subj, variables = a$by,
                  purpose = "PRIMARY OUTCOME MEASURE")),
        lapply(a$flags, flag)))
    content <- list(
      # one row a flag: no label column (the flag's TRUE is the row)
      tfl_item("table_plan", list(cols = a$by, rows = list(group = "variable"), label = NA)),
      # (no plan_levels(): the rows are the flags in their analyses' order, the
      # columns the code list's, applied when the ARD is made)
      tfl_item("plan_labels", list(variable = stats::setNames(lapply(a$flags, `[[`, "label"), ids))),
      tfl_item("plan_col_header", list(header = list(
        list(line = "1", cols = "group"),
        list(line = "1", cols = ".values", span = "each", text = "{col}"),
        list(line = "2", cols = "group", text = a$heading),
        list(line = "2", cols = ".values", span = "each", text = "(N={n})")))),
      # the look is the template's (a report changes it with an item of its own)
      tfl_item("plan_paginate_rows", list(max_rows = 30L, split = "group_safe")))
    list(ard = ard, data = list(source = "ard", object = "ard"), content = content)
  })

#' Overview of adverse events: a table plan from the template `ae_overview`
#'
#' A display template on the `summary` layout: the subjects with a
#' treatment-emergent adverse event of each kind (any, related, grade 3 or
#' higher, serious, leading to discontinuation, leading to death), n (%) by
#' arm, one row a kind.
#'
#' @inheritParams tpl_demog
#' @param ... The template's other parameters: `by`, `data`, `where`,
#'   `data_id`, `flags` (the rows: a list, each `id`, `label` and `where`),
#'   `heading`.
#' @return A [rtfreporter::table_plan()].
#' @examples
#' tfl_template_expand("ae_overview")$ard$analyses[[3]]
#' @export
tpl_ae_overview <- function(ard, population = "SAF", ..., codelists = NULL) {
  .tpl_object("ae_overview", ard, c(list(population = population), list(...)),
              codelists = codelists)
}

# ---- the templates in files of their own (R/tpl_<name>.R, read before this one) ----

.tpl_la_register()
.tpl_eair_register()
.tpl_ancova_register()
.tpl_fig_register()
