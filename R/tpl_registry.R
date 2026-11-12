# Template SPEC: a report kind's items from a few parameters.  One
# definition (an R function of declared parameters that builds code-spec
# items) gives both uses: (A) the object, tpl_<name>(), and (B) the items,
# tfl_template_expand(), saved into a report's YAML.  Prototype (10/18):
# the registry and `demog`; see _archive/template-preset-spec-design.

.tpl_registry <- new.env(parent = emptyenv())

#' A template parameter
#'
#' One parameter of a template ([tfl_template()]): its name, default and
#' what it is.  What the template's purpose and scope decide is fixed; the
#' rest is a parameter with a default, the look too (the style, the
#' widths, the rows a page): a report changes it with an argument, or with
#' a card appended after the template's (the study's styles and defaults
#' sit under both).
#'
#' @param name The parameter's name.
#' @param default Its default value; `NULL` for none.  With `study`, the
#'   fallback: the value when the study standard has none.  [tfl_unset()]
#'   (`!unset` in a YAML): leave the argument out of the call, so the inner
#'   function's own default applies (distinct from `NA` and `NULL`); a
#'   report's `!unset` does the same over any default, and so does a study
#'   value or fallback that is `!unset`.  `tfl_tpl_args()` shows it as
#'   `(none)`.
#' @param required `TRUE` when a report must give it.  With `study`: stop
#'   when neither the report nor the study standard gives it.
#' @param doc One line saying what it is.
#' @param study A reference to the study standard: where the study's value
#'   of this parameter is, taken when the report's template SPEC does not
#'   give it.  The study standard is the study's own copy of the company
#'   standard (`study.yml`'s `settings:` and `spec/defaults.yml`), never
#'   the company's.  One reader resolves any key, so a new one needs no
#'   code:
#'   * `"settings.<name>"`: `study.yml`'s `settings: <name>:`
#'     (`"settings.sd_missing"`);
#'   * `"<verb>.<argument>[.<name>]"`: that argument of the verb's item in
#'     `spec/defaults.yml`'s `content:` (`"plan_cells.continuous"`; a
#'     named-dots argument's own name as the last part); for a template
#'     whose set holds its `extends` ([tfl_template()]), of the layers the
#'     report's `extends` and style lay, nearest first, when its content
#'     step lays any ([tfl_study_value()]); otherwise patterns and a style
#'     set are not read;
#'   * `"figure_style.<key>"` (`"figure_style.km.theme"`: the type, then the
#'     key): `defaults.yml`'s `figure_style:`, else tflspec's built-in
#'     figure style;
#'   * `"output.<field>"`: the report's own field (`"output.population"`).
#'
#'   A character vector names several keys one setting can be written under
#'   (`c("plan_cells.categorical", "plan_cells.default")`): read in order, the
#'   first the study has wins.
#'
#'   Three forms: with `default`, the study's value else that fallback;
#'   with neither `default` nor `required`, the study's value else the
#'   parameter is left unset (the inner function's own default applies:
#'   a rounding); with `required = TRUE`, the study's value else a stop.
#'   `NULL`: tflspec's for its name (`population`, `by`, `group`), else the
#'   template's own default only.  `FALSE`: none, for a parameter that
#'   shares such a name and is not the study's (the listing's `by`, the
#'   variables that divide it).
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
#' @param tier How often it is set, for a help page and an input form:
#'   `"required"`, `"key"` (it has a default but stays on top, with a marker),
#'   `"usual"` (shown with its default) or `"other"` (folded away); `NULL`:
#'   `"required"` when `required`, else `"other"`.
#' @param unset What a report's (or a template default's) `!unset` does to this
#'   parameter, which the template says once, here: `"omit"` (the default): the
#'   argument is left out of the inner call it goes into; `"step_off"`: the
#'   parameter stands for a step of the template (a Total column, a guard, a
#'   style set), and the step is not made -- no code for it in the program;
#'   `"stop"`: the body needs a value, so `!unset` stops, naming the parameter.
#' @param step Where its value is used: `"call"` (the default), an argument
#'   of the template's call in a report's program, or `"data"`, a step that
#'   prepares the analysis data (a figure's records, its analysis set),
#'   written in the program's data steps, not in the call.
#' @param maps_to The plan arguments the parameter stands for
#'   (`"plan_blanks.where"`): when its value turns its step off (`!unset`,
#'   `FALSE`, `"none"`), they are taken out of the study's layers too.
#'   `NULL`: none declared.
#' @return A `tfl_param`.
#' @examples
#' tfl_param("population", "SAF", doc = "the analysis set's id")
#' tfl_param("layout", "cols", type = "choice", choices = c("cols", "rows"))
#' @export
tfl_param <- function(name, default = NULL, required = FALSE, doc = NULL, study = NULL,
                      type = NULL, multiple = NULL, of = NULL, choices = NULL,
                      fields = NULL, group = NULL, tier = NULL, step = "call",
                      unset = c("omit", "step_off", "stop"), maps_to = NULL) {
  unset <- match.arg(unset)
  if (!is.null(type)) type <- match.arg(type, .tpl_param_types)
  if (!is.null(tier)) tier <- match.arg(tier, c("required", "key", "usual", "other"))
  step <- match.arg(step, c("call", "data"))
  .tpl_study_key_check(study, name)
  .tpl_maps_to_check(maps_to, name)
  structure(list(name = name, default = default, required = isTRUE(required),
                 doc = doc, study = study, type = type, multiple = multiple, of = of,
                 choices = choices, fields = fields, group = group, tier = tier,
                 step = step, unset = unset, maps_to = maps_to),
            class = "tfl_param")
}

.tpl_param_types <- c("vars", "levels", "flag", "number", "text", "choice", "list")

# A study reference's key must be one the reader can follow: a part of
# the study standard (`settings`, `figure_style`), the report's own field
# (`output`), or a verb of defaults.yml (`plan_cells`: one rtfreporter has).
# `km` is the Kaplan-Meier params' company figure style, read as before
# until their batch moves it to `figure_style`.
.tpl_study_roots <- c("settings", "output", "figure_style", "km")
.tpl_study_key_check <- function(key, name = NULL) {
  if (is.null(key) || isFALSE(key)) return(invisible(key))
  what <- if (is.null(name)) "" else sprintf(" (parameter %s)", name)
  if (!is.character(key) || !length(key) || anyNA(key) ||
      !all(grepl("^[A-Za-z][A-Za-z0-9_]*([.][A-Za-z0-9_]+)+$", key))) {
    .ard_stop(sprintf(paste0("tfl_param(): `study` is a key of the study standard%s: ",
                             "\"settings.<name>\" or \"<verb>.<argument>\" (or several, read in order)."),
                      what))
  }
  for (k in key) {
    verb <- sub("[.].*$", "", k)
    if (!verb %in% .tpl_study_roots &&
        !verb %in% tryCatch(getNamespaceExports("rtfreporter"), error = function(e) verb)) {
      .ard_stop(sprintf(paste0("tfl_param(): `study = \"%s\"`%s: %s is not a verb of the study's ",
                               "defaults.yml (a plan_*() function of rtfreporter), nor `settings`, ",
                               "`figure_style` or `output`."), k, what, sQuote(verb, FALSE)))
    }
  }
  invisible(key)
}

# A parameter's `maps_to`: plan arguments, "<verb>.<argument>", the verb one
# of rtfreporter's (character(0): it maps to nothing in the study)
.tpl_maps_to_check <- function(x, name = NULL) {
  if (is.null(x)) return(invisible(x))
  what <- if (is.null(name)) "" else sprintf(" (parameter %s)", name)
  if (!is.character(x) || anyNA(x) || !all(grepl("^[A-Za-z][A-Za-z0-9_]*[.][A-Za-z0-9_]+$", x))) {
    .ard_stop(sprintf(paste0("tfl_param(): `maps_to` is plan arguments%s: \"<verb>.<argument>\" ",
                             "(\"plan_blanks.where\")."), what))
  }
  verb <- sub("[.].*$", "", x)
  bad <- setdiff(verb, tryCatch(getNamespaceExports("rtfreporter"), error = function(e) verb))
  if (length(bad)) {
    .ard_stop(sprintf("tfl_param(): `maps_to`%s: %s is not a plan verb of rtfreporter.", what,
                      sQuote(bad[1L], FALSE)))
  }
  invisible(x)
}

# How a page break is placed (plan_paginate_rows(split = )): the template's
# choice, a value in its set; the demographics family fixes it
.tpl_split_choices <- c("none", "rows", "group_safe", "group_force", "by_value")

.tpl_split_param <- function(default, unset = "stop") {
  tfl_param("split", default, type = "choice", choices = .tpl_split_choices, tier = "other",
            unset = unset, doc = paste(
    "how a page break is placed (plan_paginate_rows(split = )): \"none\" one page;",
    "\"group_safe\" whole groups to a page; \"group_force\" a cut every `max_rows` rows,",
    "a group continued on the next page; \"rows\" and \"by_value\" are cut by position",
    "or by value (they need their own arguments, a `plan_paginate_rows` item)"))
}

# page_by of shift and param_change: the parameter's variable (`var`), a page
# a value; NULL: no pages by value (`!unset`, FALSE or "none").  Not given,
# it is the template's parameter variable; given, it is that variable
.tpl_page_by <- function(a, ctx, var, tn) {
  v <- a$page_by
  if ("page_by" %in% ctx$unset || !length(v) || .tpl_stop_value(v)) return(NULL)
  if ("page_by" %in% ctx$given && !identical(as.character(v), var)) {
    .ard_stop(sprintf(paste0("%s: page_by is the parameter's variable (%s) here: leave it out, ",
                             "or `!unset` for no page a value."), tn, var))
  }
  var
}

# tflspec's parameters' types by name (a template's tfl_param(type = )
# says it where a name means two things: `param`, `sort`)
.tpl_param_kinds <- local({
  k <- function(names, type, multiple = FALSE, ...) {
    stats::setNames(rep(list(list(type = type, multiple = multiple, ...)), length(names)),
                    names)
  }
  nv <- c("name", "text")
  c(k(c("by", "baseline", "post", "term", "grade", "category", "censor", "group", "time",
        "pop", "param_order", "visit_order", "exposure", "response", "value", "visit_var",
        "page_by"), "vars"),
    k(c("categorical", "continuous", "variables", "hierarchy", "covariates", "subgroups",
        "order"), "vars", TRUE),
    # the layouts' look, each a verb's argument the study standard may give
    k(c("max_rows", "stub_indent"), "number"),
    k(c("stub_before", "blanks_first", "blanks_last", "blanks_counted", "total",
        "missing_row"), "flag"),
    k(c("blanks", "total_label", "stub_name", "header_n", "cat_cell", "na",
        "rounding", "empty", "denominator", "stack_id", "stack_label"), "text"),
    k("split", "choice", choices = .tpl_split_choices),
    k("widths", "list", fields = c("name", "text")),
    k("comparison", "levels", of = "group"),
    k("n_by_arm", "levels", TRUE, of = "group"),
    k("by_levels", "levels", TRUE, of = "by"),
    k(c("levels", "worst"), "levels", TRUE, of = "post"),
    k("percent_of", "choice", choices = c("row", "total")),
    k("worst_by", "list", fields = c("name", "text")),
    k("grade_levels", "levels", TRUE, of = "grade"),
    k("responders", "levels", TRUE, of = "category"),
    k("params", "levels", TRUE, of = "PARAMCD"),
    k("visits", "levels", TRUE, of = "AVISIT"),
    k("visit", "levels", of = "AVISIT"),
    k("baseline_value", "levels", of = "baseline"),
    k(c("sort_by", "ref"), "levels", of = "by"),
    k(c("stack", "drop_time_0", "cumulative", "baseline_normal", "by_response"),
      "flag"),
    k(c("min_pct", "per", "conf_level"), "number"),
    k(c("layout", "direction"), "choice", choices = c("cols", "rows")),
    # the listing's `by`: how it divides the listing, and a page's name
    k("by_break", "choice", choices = c("page", "block")),
    k(c("by_heading", "by_na_label"), "text"),
    k("grade_in", "choice", choices = c("rows", "cols")),
    k("min_pct_in", "choice", choices = c("any", "all", "total")),
    # what the report is of: the analysis set, which subjects, which records
    k(c("population", "subset", "where"), "text", group = "target"),
    k(c("data", "data_id", "data_label", "analysis_id",
        "analysis_label", "overall", "any_grade", "cat_id", "cat_label", "ci_id", "ci_label",
        "ci_method", "groupn_label", "median_label", "time_prefix", "time_unit", "unit",
        "when", "visit_label", "param_label", "baseline_label", "value_label", "change_label", "cell", "start",
        "missing_label", "page_heading", "post_header", "sd_missing"),
      "text"),
    k(c("heading", "statistics", "cat_statistics", "continuous_statistics",
        "categorical_statistics"), "text", TRUE),
    k("any_row", "flag"),
    # a forest's look (forest_hr)
    k(c("size_by_n", "bold_headings"), "flag"),
    k("favours", "text", TRUE),
    k("x_breaks", "number", TRUE),
    k(c("count_cell", "any_row_label"), "text"),
    # a variable's heading each: given by name, merged with the default's
    k(c("labels", "param_labels", "subgroup_labels"), "list", fields = nv, merge = TRUE),
    # the rows: given, they are the rows (no merge)
    k("cells", "list", fields = c("label", "template")),
    k("formats", "list", fields = nv, merge = TRUE),
    # demog's: a step, the values shown, a heading text, a style set's name, a rule's edges
    k("missing_row", "flag"),
    k("categorical_levels", "list", fields = c("variable", "values")),
    k(c("col_header", "style"), "text"),
    k(c("border_header", "border_group"), "list", fields = c("edge", "line")),
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

# A parameter of several lines (a heading) as the one text of a header cell:
# the lines joined by a line break, as the hierarchical tables write theirs;
# none given stays none
.tpl_lines <- function(x) if (length(x)) paste(x, collapse = "\n")

# A parameter's study reference by its name, until each family drops it:
# the analysis set is the report's (`population:`), the treatment the study's
# (a parameter's own `study =` wins).  (`by` and `group` are required with
# no reference in the new design, canon 1.14; each family's batch drops the
# fill, so this table is the last per-name rule.)  One reader reads them
# and every other key: .tpl_study_read().
.tpl_study_by_name <- c(population = "output.population", by = "settings.treatment",
                        group = "settings.treatment")

# A parameter's type filled in: as declared, by its name, else from its
# default (a company's template that says none: `guessed`, which
# tfl_test_templates() lists)
.tpl_param_typed <- function(p) {
  # `study = FALSE`: no study source whatever the name.  `no_study` keeps it
  # for when this is done again (not a name starting `study`: `p$study`
  # would match it)
  if (isFALSE(p$study)) {
    p$study <- NULL
    p$no_study <- TRUE
  }
  if (is.null(p$study) && !isTRUE(p$no_study) && p$name %in% names(.tpl_study_by_name)) {
    p$study <- .tpl_study_by_name[[p$name]]
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
  # its tier: as declared, else required or other (a variant keeps its root's)
  if (is.null(p$tier)) p$tier <- if (isTRUE(p$required)) "required" else "other"
  # (a parameter made before `step`: an argument of the call)
  if (is.null(p$step)) p$step <- "call"
  if (is.null(p$unset)) p$unset <- "omit"
  p
}

# Register a template: its kind, layout, parameters and body.  The body is
# function(args, context) returning list(ard = , data = , content = ): the
# report's items, `content` a list of tfl_item().  `parts`: where each
# parameter acts, list(ard = <names>, content = <names>), for a SPEC written
# as `template: {name, ard:, content:}`; NULL: the template takes the two
# parts' names as written (every name in either).  `extends`: what its
# reports extend when they write none (its set's value, canon 1.27; NULL: a
# template not yet migrated, the old layer order); `extends_fixed`: the
# steps whose value it fixes.
.tfl_template <- function(name, kind, layout, params, body, label = NULL, call = NULL,
                          package = "tflspec", example = NULL, ai = FALSE, parts = NULL,
                          extends = NULL, extends_fixed = character()) {
  extends <- .extends_value(extends, sprintf("template %s", name))
  if (!is.character(extends_fixed) || !all(extends_fixed %in% .extends_steps) ||
        (length(extends_fixed) && is.null(extends))) {
    .ard_stop(sprintf("template %s: `extends_fixed` names steps (%s) of its `extends`.", name,
                      paste(.extends_steps, collapse = ", ")))
  }
  params <- lapply(params, .tpl_param_typed)
  names(params) <- vapply(params, `[[`, "", "name")
  # (a table's or a listing's call is the one tfl_tpl(); a figure's its own)
  call <- call %||% if (identical(kind, "figure")) paste0("tpl_", name) else "tfl_tpl"
  assign(name, list(name = name, kind = kind, layout = layout, params = params, body = body,
                    label = label, call = call, package = package,
                    example = example, ai = isTRUE(ai), parts = parts,
                    extends = extends, extends_fixed = extends_fixed),
         envir = .tpl_registry)
  invisible(name)
}

.tpl_get <- function(name, fn) {
  t <- get0(name, envir = .tpl_registry, inherits = FALSE)
  # an alias of the catalog's (stf_t14_ae_soc_pt): its template, as itself
  if (is.null(t)) {
    to <- .tpl_alias_target(name, fn)
    if (!is.null(to)) t <- get0(to, envir = .tpl_registry, inherits = FALSE)
  }
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

# A parameter that is a map of name to text (type list, fields name and text,
# not several): `param_labels: {ALT: "..."}`
.tpl_is_name_text <- function(p) {
  identical(p$type, "list") && !isTRUE(p$multiple) && setequal(p$fields, c("name", "text"))
}

# A map's value written as a list of {name, text} (the type the kit prints),
# as the mapping: `[{name: ALT, text: "..."}]` is `{ALT: "..."}`.  A value
# that is not such a list (a mapping, a vector, NULL, a tfl_r) stays as it
# is; a list that cannot be read as a mapping stops, naming the parameter.
.tpl_name_text_map <- function(v, tname, pname, fn) {
  if (!is.list(v) || inherits(v, "tfl_r") || !length(v) || !is.null(names(v))) return(v)
  bad <- function(why) {
    .ard_stop(sprintf(paste0("%s: template %s, parameter %s: %s. Write the mapping, ",
                             "`%s: {ALT: \"...\"}`."), fn, tname, sQuote(pname, FALSE), why, pname))
  }
  ok <- vapply(v, function(e) is.list(e) && setequal(names(e), c("name", "text")) &&
                 length(names(e)) == 2L, NA)
  if (!all(ok)) bad("a list takes items of `name` and `text` only")
  nm <- vapply(v, function(e) if (is.character(e$name) && length(e$name) == 1L) e$name else NA_character_, "")
  if (anyNA(nm) || !all(nzchar(nm))) bad("each `name` is a text")
  if (anyDuplicated(nm)) bad(sprintf("`name` %s is given twice", sQuote(nm[duplicated(nm)][1L], FALSE)))
  stats::setNames(lapply(v, `[[`, "text"), nm)
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
    if (inherits(v, "tfl_unset")) return(v)
    if (.tpl_is_name_text(p)) v <- .tpl_name_text_map(v, t$name, p$name, fn)
    if (isTRUE(p$merge) && length(p$default) && length(v) && !is.null(names(v))) {
      v <- .tpl_merge_named(p$default, v)
    }
    v
  })
  # `!unset` (a report's value or a template's default): the parameter is
  # left out -- the body reads it as NULL, and the call written omits it, so
  # the inner function's own default applies.  Recorded in `unset`.
  unset <- vapply(out, inherits, NA, "tfl_unset")
  stop_on <- unset & vapply(t$params[names(out)], function(p) identical(p$unset, "stop"), NA)
  if (any(stop_on)) {
    .ard_stop(sprintf(paste0("%s: template %s needs %s, so `!unset` cannot be used for it; ",
                             "give a value."), fn, t$name,
                      paste(sQuote(names(out)[stop_on], FALSE), collapse = ", ")))
  }
  if (any(unset)) out[unset] <- list(NULL)
  miss <- vapply(t$params, function(p) p$required && is.null(out[[p$name]]), NA)
  if (any(miss)) {
    .ard_stop(sprintf("%s: template %s needs %s.", fn, t$name,
                      paste(sQuote(names(miss)[miss], FALSE), collapse = ", ")))
  }
  attr(out, "unset") <- names(unset)[unset]
  out
}

# A parameter the body cannot do without a value for: `!unset` on it stops
# with a message that names it.  A check of a parameter's value (a choice,
# a range) lets an unset value through to here, so it is never reported as
# a wrong value; what unset means for each parameter is the template's own
# decision, made parameter by parameter.
.tpl_unset_stop <- function(a, name, tn) {
  if (name %in% attr(a, "unset")) {
    .ard_stop(sprintf("%s: `%s` cannot be left unset (!unset).", tn, name))
  }
  invisible(a)
}

# A choice parameter's value, checked (match.arg()); `!unset` stops as above
.tpl_choice <- function(a, name, choices, tn, value = a[[name]]) {
  .tpl_unset_stop(a, name, tn)
  match.arg(value, choices)
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
#' x <- tfl_template_expand("demog", list(population = "SAF", by = "TRT01A", continuous = "AGE"))
#' vapply(x$content, function(it) it$fn, "")
#' @export
tfl_template_expand <- function(name, args = list(), codelists = NULL,
                                population = NULL) {
  fn <- "tfl_template_expand()"
  t <- .tpl_get(name, fn)
  a <- .tpl_args(t, args, fn)
  # (an unset parameter is not given: the inner function's default applies)
  out <- t$body(a, list(codelists = codelists, population = population,
                        given = setdiff(names(args), attr(a, "unset")),
                        unset = attr(a, "unset"),
                        template = list(name = t$name, label = t$label)))
  out$unset <- attr(a, "unset")
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
  # the code lists' labels: the ARD holds the codes
  if (identical(t$kind, "table")) out$content <- .tpl_labels_items(out$content, codelists)
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

# A vector of conditions together (`&`), each in brackets when it is an
# "or" (what `.tpl_and()` does for the conditions it is given one each, here
# all the elements of the vectors); NULL when none
.tpl_and_all <- function(x) {
  do.call(.tpl_and, as.list(unlist(x, use.names = FALSE)))
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
  # (not a table of records that reads an analysis data of the ARD: its
  # subjects are narrowed above)
  if (identical(out$data$source, "dataset") && is.null(out$data[["data"]])) {
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

# A code list's codes in its order, as the ARD holds them: from a report's
# code lists as the YAML holds them (rows of value / label), or from a list
# of the values themselves (`list(TRT01A = c("Drug X", "Control"))`, what a
# program passes to tpl_*()); a vector named by its codes (`c(F = "Female")`)
# is its names.
.tpl_levels <- function(vars, codelists) {
  out <- list()
  for (v in intersect(names(codelists), vars)) {
    cl <- codelists[[v]]
    out[[v]] <- if (is.character(cl)) {
      if (is.null(names(cl))) unname(cl) else names(cl)
    } else vapply(cl, function(r) as.character(r$value), "")
  }
  out
}

# A code list's labels by code, `c(F = "Female", M = "Male")` (a code with no
# label is its own); NULL for a list that has none: the table's text of the
# codes the ARD holds, which plan_labels() applies.
.tpl_cl_dict <- function(cl) {
  if (is.character(cl)) {
    return(if (is.null(names(cl))) NULL else stats::setNames(unname(cl), names(cl)))
  }
  val <- vapply(cl, function(r) as.character(r$value), "")
  lab <- vapply(cl, function(r) as.character(r$label %||% NA_character_), "")
  lab[!is.na(lab) & !nzchar(trimws(lab))] <- NA
  if (all(is.na(lab))) return(NULL)
  stats::setNames(ifelse(is.na(lab), val, lab), val)
}

# A template's items with the code lists' labels in one plan_labels(): an
# entry a variable, `SEX = c(F = "Female")`, joined to the template's own
# entry of the variable (its text wins), the plan_labels() item the template
# has taking them, else an item of its own.  `covered`: a variable whose
# entry says one of its codes already is left as it is (items made from the
# sheets have it, narrowed).
.tpl_labels_items <- function(content, codelists, covered = FALSE) {
  if (inherits(content, "tfl_r") || !is.list(content)) return(content)
  dict <- Filter(Negate(is.null), lapply(codelists, .tpl_cl_dict))
  if (covered && length(dict)) {
    have <- function(v) {
      for (it in content) {
        if (!identical(it$fn, "plan_labels")) next
        own <- unlist(it$args[[v]])
        if (any(names(own) %in% names(dict[[v]]))) return(TRUE)
      }
      FALSE
    }
    dict <- dict[!vapply(names(dict), have, NA)]
  }
  if (!length(dict)) return(content)
  fn <- vapply(content, function(it) it$fn, "")
  at <- match("plan_labels", fn)
  if (is.na(at)) return(.insert_by_rank(content, tfl_item("plan_labels", dict)))
  args <- content[[at]]$args
  for (v in names(dict)) {
    own <- args[[v]]
    if (is.null(own)) {
      args[[v]] <- dict[[v]]
    } else if (is.character(own) || (is.list(own) && all(vapply(own, is.character, NA)))) {
      # (the text it has wins; the form it has stays)
      # (one unnamed text is the variable's own heading: keyed by its name)
      own <- if (is.null(names(unlist(own)))) stats::setNames(unlist(own), v) else own
      joined <- c(unlist(own), dict[[v]][!names(dict[[v]]) %in% names(own)])
      args[[v]] <- if (is.list(own)) as.list(joined) else joined
    }
  }
  content[[at]]$args <- args
  content
}

#' A table or listing template's plan: one call for every template
#'
#' The A use of a template SPEC, one function for them all: the template's
#' items run on the data, as the report's content.  `tfl_tpl(ard, "demog",
#' population = "SAF")` is what a template report's program calls; a
#' study's defaults the template does not take as its parameters are added
#' after the call -- or, for a template whose set has its `extends` (demog,
#' summary, disposition, ae_overview, param_change),
#' given as `study`, under the template's items.  The template's parameters,
#' their defaults and where each ends: `tfl_tpl_help()` (and
#' `?tpl_<name>`).
#'
#' The template's steps of the ARD program are not run here: the data is
#' used as given.  `where`, and `min_pct` of the SOC / PT templates, are
#' steps of the ARD the report's program writes (`keep_min_pct()`, of
#' [tfl_helpers_code()]); `tfl_tpl(ard, "ae_pt_freq", min_pct = 5)` on an
#' ARD made by hand shows all its terms.
#'
#' @param x The data: the ARD, not yet normalized (a table); the dataset as
#'   the program made it, its subjects kept already (a listing).
#' @param name The template's name ([tfl_templates()]): a layout, a display
#'   template or a variant.
#' @param ... Its parameters, by name.  A name it does not have is an error
#'   naming its parameters.
#' @param study The study's items under the template (its default and style
#'   layers, as the report's SPEC resolves them): a list by plan verb, each a
#'   named list of its arguments as the SPEC holds them, `list(plan_style =
#'   list(align_count_pct = TRUE))`.  They are merged right after
#'   `table_plan()`, as the SPEC's layers meet: the template's values win, an
#'   argument it does not write stays the study's ([tfl_unset()] leaves it to
#'   the function).  A verb may come again (a stacking verb's second item,
#'   another layer's arguments).  `NULL` none; not for a figure.
#' @param codelists The report's code lists: as [tfl_read_output()] reads
#'   them, or the values in order, `list(TRT01A = c("Drug X", "Control"))`.
#' @param sets The study's analysis sets (`data.yml`'s populations), for
#'   their labels; `NULL` none.
#' @return A [rtfreporter::table_plan()].
#' @seealso [tfl_template_expand()] for the items (the B use);
#'   [tfl_templates()]; a figure template's own function ([tpl_mean_se()]
#'   ...).
#' @examples
#' if (requireNamespace("cards", quietly = TRUE)) {
#'   saf <- adsl[adsl$SAFFL == "Y", ]
#'   ard <- cards::bind_ard(
#'     cards::ard_tabulate(saf, variables = TRT01A),
#'     cards::ard_summary(saf, by = TRT01A, variables = AGE),
#'     cards::ard_tabulate(saf, by = TRT01A, variables = c(AGEGR1, SEX, RACE, ETHNIC)))
#'   plan <- tfl_tpl(ard, "demog", population = "SAF", by = "TRT01A", continuous = "AGE",
#'                   categorical = c("AGEGR1", "SEX", "RACE", "ETHNIC"))
#' }
#' @export
tfl_tpl <- function(x, name, ..., study = NULL, codelists = NULL, sets = NULL) {
  fn <- "tfl_tpl()"
  if (!is.character(name) || length(name) != 1L || is.na(name)) {
    .ard_stop(sprintf("%s: `name` is a template's name, one text (\"demog\"); the templates: tfl_templates().",
                      fn))
  }
  t <- .tpl_get(name, fn)
  study <- .tpl_study_items(study, fn)
  if (identical(t$kind, "figure")) {
    if (length(study)) {
      .ard_stop(sprintf("%s: `study` is for a table or listing template; %s is a figure's.", fn, name))
    }
    return(.tpl_fig_call(t, x, rlang::enquos(...)))
  }
  if (identical(t$kind, "listing")) {
    it <- tfl_template_expand(name, list(...), codelists = codelists, population = sets)
    .listing_by_in_data(x, list(...)$by, fn)
    # `x` is the data as the program made it (its subjects kept already)
    it$data$steps <- NULL
    o <- tfl_output(paste0("tpl-", name), kind = "listing", extends = "none",
                    data = it$data, content = .tpl_study_under(study, it$content))
    return(tfl_content_run(o, x))
  }
  .tpl_object(name, x, list(...), codelists = codelists, population = sets, study = study)
}

# `tfl_tpl(study = )`: the study's items under the template (its layers 1
# and 2, canon 1.28), `list(<verb> = list(<argument> = <value>, ...), ...)`
# -- the arguments as the SPEC holds them, a verb named again for another
# layer's arguments or a stacking verb's second item.  The items, one per
# verb but a stacking verb's, in their order.
.tpl_study_items <- function(study, fn = "tfl_tpl()") {
  if (is.null(study) || !length(study)) return(list())
  bad <- !is.list(study) || is.object(study) || is.null(names(study)) || any(!nzchar(names(study)))
  if (!bad) bad <- !all(vapply(study, function(a) is.list(a) && !is.object(a) &&
                                 (!length(a) || (!is.null(names(a)) && all(nzchar(names(a))))), NA))
  if (bad) {
    .ard_stop(sprintf(paste0("%s: `study` is a list of the study's items by verb, each a named list ",
                             "of its arguments: study = list(plan_style = list(align = \"right\"))."), fn))
  }
  verbs <- names(study)
  nope <- verbs[!startsWith(verbs, "plan_")]
  if (length(nope)) {
    .ard_stop(sprintf("%s: `study` takes plan verbs' items; %s %s not.", fn,
                      paste(unique(nope), collapse = ", "), if (length(unique(nope)) > 1L) "are" else "is"))
  }
  out <- list()
  for (i in seq_along(study)) {
    v <- verbs[i]
    k <- .merge_kind(v)
    j <- if (k %in% c("arg", "dots")) match(v, vapply(out, `[[`, "", "fn")) else NA_integer_
    if (is.na(j)) {
      out <- c(out, list(tfl_item(v, study[[i]], from = "defaults")))
    } else {
      out[[j]]$args <- if (k == "dots") .merge_dots(out[[j]]$args, study[[i]])
                       else .merge_by_arg(out[[j]]$args, study[[i]])
    }
  }
  out
}

# The template's items over the study's (`tfl_tpl(study = )`), by tflspec's
# merge as the SPEC's layers meet: the template's values win, an argument
# it does not write stays the study's
.tpl_study_under <- function(study, content) {
  if (!length(study)) return(content)
  .merge_content(study, content)
}

# The A use: the template's object.  Its items run on the ARD as a report's
# content does; the study's items it is given (`study`, its layers 1 and 2)
# under them, merged right after table_plan().  The report's own items are
# the program's lines after the call.
.tpl_object <- function(name, ard, args, codelists = NULL, population = NULL, study = list()) {
  # (the arms of a template with a `by`: from the ARD when the code lists do
  # not say them -- the Total column's width and the like need how many)
  by <- args$by
  w <- if ("widths" %in% names(args)) args$widths else .tpl_get(name, "tpl")$params$widths$default
  if (identical(w, "auto") && is.character(by) && length(by) == 1L &&
      is.null(codelists[[by]]) &&
      all(c("group1", "group1_level") %in% names(ard))) {
    arms <- unique(as.character(unlist(ard$group1_level[!is.na(ard$group1) & ard$group1 == by])))
    arms <- arms[!is.na(arms) & nzchar(arms)]
    if (length(arms)) codelists <- c(codelists, stats::setNames(list(arms), by))
  }
  x <- tfl_template_expand(name, args, codelists = codelists, population = population)
  t <- .tpl_get(name, "tpl")
  # (the ARD is normalized first, as a report's content with no step of its
  # own: normalize_ard() with its defaults)
  content <- .tpl_study_under(study, x$content)
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
#' @param ... The template's other parameters: `by` and `continuous` and/or
#'   `categorical` are required (no default set of variables, no default
#'   `by`), then `data`, `labels`, `cells`, `formats` ...
#'   `labels` given by name change those names only (`labels = c(RACE =
#'   "Race, n (%) [a]")`); in YAML, `RACE: ~` drops one.  The variables are
#'   as written.  Every table and figure template also takes `subset` (which
#'   subjects) and, with records data, `where` (which records): see
#'   [tfl_param()].
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
#'   plan <- tpl_demog(ard, population = "SAF", by = "TRT01A", continuous = "AGE",
#'                     categorical = c("AGEGR1", "SEX", "RACE", "ETHNIC"))
#'   head(rtfreporter::plan_apply(plan, "table"))
#' }
#' @export
tpl_demog <- function(ard, population, ..., codelists = NULL) {
  .tpl_object("demog", ard, c(list(population = population), list(...)),
              codelists = codelists)
}

# ---- summary: continuous variables by a column key, the layout ------------------

# The Mean (SD) cell as a choice (rtfreporter's fallback chain: a cell whose
# statistics are not all there is NA and the next is tried): the mean and its
# SD, else -- a group of one has no SD -- the mean and `mark` in place of the
# SD; "" for the mean alone.  `mean` and `sd` are the statistics' tokens
# with their digits, if any.
.tpl_mean_sd <- function(mean = "{mean}", sd = "{sd}", mark = "-") {
  c(paste0(mean, " (", sd, ")"), if (nzchar(mark)) paste0(mean, " (", mark, ")") else mean)
}

# The cells a template runs with: its default, with the Mean (SD) cell's mark
# `sd_missing`; cells a report (or the study) gave are its own, left as given
.tpl_sd_missing <- function(cells, default, sd_missing, mean = "{mean}", sd = "{sd}") {
  if (!is.character(sd_missing) || length(sd_missing) != 1L || is.na(sd_missing) ||
      grepl("[{}]", sd_missing)) {
    .ard_stop("`sd_missing` is one text without { or } (\"-\", \"NA\", or \"\" for the mean alone).")
  }
  if (identical(cells, default)) cells[["Mean (SD)"]] <- .tpl_mean_sd(mean, sd, sd_missing)
  cells
}

# `sd_missing`: one parameter for the templates with a Mean (SD) cell
.tpl_sd_missing_param <- function() {
  tfl_param("sd_missing", "-", type = "text", study = "settings.sd_missing",
            doc = paste("what prints in place of the SD when a group has no SD (n = 1):",
                        "{mean} (<sd_missing>); \"\" in study.yml or a report's YAML prints the mean alone;",
                        "no effect when `cells` is given"))
}

# Is a template `demog` or a variant made from it (stf_t02_demog, a company's)?
.tpl_is_demog <- function(name) {
  for (i in 1:20) {
    if (identical(name, "demog")) return(TRUE)
    t <- get0(name, envir = .tpl_registry, inherits = FALSE)
    if (is.null(t) || is.null(t$from)) return(FALSE)
    name <- sub("^[.]base[.]", "", t$from)
  }
  FALSE
}

# `denominator` of summary: the base of the categorical variables'
# percentages (the continuous statistics have none); the column headings' N is
# not moved by it
.tpl_den_sum_doc <- paste(
  "the categorical variables' percentages' denominator, passed explicitly: NULL (default): the",
  "analysis set's subjects (with a `data` other than ADSL, the records kept); a number: that many",
  "in every column; numbers named by the values of `by` (`c(\"Drug X\" = 60, \"Control\" = 55)`):",
  "that many in each column; the id of an analysis data (`adsl_itt`, or one of the report's own):",
  "its rows in each column. A number is not for `total` = TRUE. The column headings' N is the",
  "analysis set's, not moved")

# The data step of the demog family's Missing row: of the categorical
# variables, those with a missing value (NA, or blank as an .xpt holds it)
# become a factor of their values and a last level "Missing"; the others
# stay as they are
.tpl_missing_level_code <- function(set, vars) {
  v <- paste0("c(", paste0("\"", vars, "\"", collapse = ", "), ")")
  paste0(
    "dplyr::mutate(", set, ", dplyr::across(dplyr::any_of(", v, "), function(x) {
",
    "  chr <- trimws(as.character(x))
",
    "  miss <- is.na(chr) | !nzchar(chr)
",
    "  if (!any(miss)) return(x)
",
    "  chr[miss] <- \"Missing\"
",
    "  values <- if (is.factor(x)) levels(x) else sort(unique(chr[!miss]))
",
    "  factor(chr, levels = unique(c(values, \"Missing\")))
",
    "}))")
}

# summary's `denominator`, checked: `value` what the categorical analysis's
# `denominator` takes (NULL: the body's default, the analysis set -- with
# records `data`, the records kept); `extra` the analysis data to add (another
# analysis set's adsl_<id>).
.tpl_sum_den <- function(a, ctx, subj, set) {
  tn <- ctx$template$name %||% "summary"
  v <- a$denominator
  # NULL: the report's ADSL population (the column headings' N counts it too);
  # on records, the records kept
  if (is.null(v)) {
    return(list(value = if (identical(toupper(a[["data"]]), "ADSL")) subj else set, extra = list()))
  }
  if (!length(a$categorical)) {
    .ard_stop(sprintf(paste0(
      "%s: denominator is the base of the categorical variables' percentages, and the table ",
      "has none: give `categorical`, or leave denominator out."), tn))
  }
  one <- is.character(v) && length(v) == 1L && !is.na(v)
  if (one && identical(trimws(v), "data") && identical(toupper(a[["data"]]), "ADSL")) {
    .ard_stop(sprintf(paste0(
      "%s: denominator = \"data\" is for a `data` other than ADSL (the subjects with a record ",
      "kept): ADSL is already one row a subject, the analysis set. Leave it out, or give a number, ",
      "numbers named by the values of %s, or an analysis data's id (%s counts every subject of the set)."),
      tn, a$by, subj))
  }
  if (one && identical(trimws(v), "data")) {
    .ard_stop(sprintf(paste0(
      "%s: denominator = \"data\" on records (%s) is not supported yet: the counts are of records ",
      "and \"data\" is a base of subjects, so a percentage could pass 100%%; leave it out (the ",
      "records kept), or give a number, numbers named by the values of %s, or an analysis data's id."),
      tn, a[["data"]], a$by))
  }
  dn <- .tpl_den(a, ctx, subj)
  value <- dn$value
  if (is.null(value) && one && identical(trimws(v), subj)) value <- subj
  list(value = value, extra = dn$extra)
}

# summary and disposition: R/tpl_registry_demog.R (on demog's body, batch 2)

#' Continuous variables by a column key: a table plan from the layout `summary`
#'
#' @inheritParams tpl_demog
#' @param continuous The continuous variables, in order; `NULL` for none.
#' @param ... The template's other parameters: `by` (required, no
#'   default) and `categorical` (the categorical variables), then `data`,
#'   `where`, `labels`, `order`, `formats`, `cells`, `count_cell`,
#'   `denominator`, `missing_row`, `total` ...: `tfl_tpl_help("summary")`.
#'   The statistics' digits are the ARD's (`formats`); the cells place them.
#' @return A [rtfreporter::table_plan()].
#' @examples
#' if (requireNamespace("cards", quietly = TRUE)) {
#'   saf <- adsl[adsl$SAFFL == "Y", ]
#'   ard <- cards::bind_ard(
#'     cards::ard_tabulate(saf, variables = TRT01A),
#'     cards::ard_summary(saf, by = TRT01A, variables = c(AGE, TRTDURD)))
#'   plan <- tpl_summary(ard, population = "SAF", continuous = c("AGE", "TRTDURD"), by = "TRT01A")
#'   head(rtfreporter::plan_apply(plan, "table"))
#' }
#' @export
tpl_summary <- function(ard, population, continuous = NULL, ..., codelists = NULL) {
  .tpl_object("summary", ard, c(list(population = population, continuous = continuous),
                                list(...)), codelists = codelists)
}

#' The templates
#'
#' Every table and figure template takes the same pair of filters, its
#' input form's first group with `population`: `subset` (which subjects: R
#' on the analysis set's ADSL) and, with records data, `where` (which
#' records).  Both are `NULL` by default; see [tfl_param()].
#'
#' A template is one program; the items of a published collection -- FDA's
#' Standard Safety Tables and Figures IG v2, PhUSE CSS's white papers,
#' CDISC ARS's examples -- are in the templates' catalog, each with an
#' alias (`stf_t14_ae_soc_pt`) that runs its template wherever a template's
#' name is taken ([tfl_tpl()], a report's `template: {name: }`, the
#' template sheet), as itself.  `source` lists a collection's every item,
#' whichever template writes it, and those none writes yet.  The catalog
#' keeps each item's number and a short title of our own, not the
#' publications' text; the templates are tflspec's, not endorsed or
#' recommended by FDA, PhUSE or CDISC.  The publications are cited below.
#'
#' @param source `NULL` (default): the templates.  A collection --
#'   `"tflspec"`, `"FDA STF IG v2"`, `"PhUSE CSS"`, `"CDISC ARS"` -- for its
#'   items.
#' @return A data frame.  Without `source`, one row a template: `name`,
#'   `kind`, `layout`, `params` (its parameters, comma-separated),
#'   `package` (the package that registered it: tflspec, or a company's,
#'   [tfl_template()]), `call` (the function its program calls, the A use),
#'   `from`, `label`, `ai` and `aliases` (its catalog aliases,
#'   comma-separated, or `NA`).  With `source`, one row an item: `source`,
#'   `item` (its number there), `title`, `alias`, `template` (the template
#'   that writes it, `NA` when none does yet), `status` (`"same"`: the
#'   template writes it; `"close"`: the template writes it, other defaults
#'   or a column it lacks would match the collection exactly; `"missing"`;
#'   `"not_output"`: a criteria table, not an output), `kind` and `note`.
#' @examples
#' tfl_templates()
#' head(tfl_templates(source = "FDA STF IG v2"))
#' @eval .tpl_rd_sources()
#' @export
tfl_templates <- function(source = NULL) {
  if (!is.null(source)) {
    if (!is.character(source) || length(source) != 1L || !source %in% .tpl_sources) {
      .ard_stop(sprintf("tfl_templates(source = ): one of %s.",
                        paste(sprintf("\"%s\"", .tpl_sources), collapse = ", ")))
    }
    return(.tpl_catalog_rows(source))
  }
  tt <- .tpl_registry_table()
  tt$aliases <- vapply(tt$name, function(n) {
    a <- .tpl_aliases_of(n)$alias
    if (length(a)) paste(a, collapse = ", ") else NA_character_
  }, "", USE.NAMES = FALSE)
  tt
}

# The registry, one row a template
.tpl_registry_table <- function() {
  nm <- sort(ls(.tpl_registry))
  t <- lapply(nm, .tpl_get, fn = "tfl_templates()")
  data.frame(name = nm, kind = vapply(t, `[[`, "", "kind"),
             layout = vapply(t, `[[`, "", "layout"),
             params = vapply(t, function(x) paste(names(.tpl_shown(x$params)), collapse = ", "), ""),
             package = vapply(t, function(x) x$package %||% "tflspec", ""),
             call = vapply(t, function(x) x$call %||% paste0("tpl_", x$name), ""),
             from = vapply(t, function(x) x$from %||% NA_character_, ""),
             label = vapply(t, function(x) x$label %||% NA_character_, ""),
             ai = vapply(t, function(x) isTRUE(x$ai), NA),
             stringsAsFactors = FALSE)
}

# ---- hierarchical: a hierarchy's counts (SOC / PT), the layout ------------------

.tpl_header_n_stack_doc <- paste(
  "the arm headings' N: \"denominator\" (default): the arm's subjects the percentages",
  "are of (`denominator`); \"set\": the same, when `denominator` is NULL (the analysis set);",
  "\"tested\": the same, when `denominator` is \"data\" (the records' subjects); a header N",
  "apart from the denominator is not supported yet; \"none\": no N")

# the stack templates' `denominator` (hierarchical, ae_soc_pt and its
# variants, ae_pt_freq, ae_max_grade)
.tpl_den_doc <- paste(
  "the percentages' denominator: NULL (default): the analysis set's subjects; \"data\": the",
  "subjects with a record kept; a number: that many subjects in every column; numbers named by",
  "the values of `by` (`c(\"Drug X\" = 60, \"Control\" = 55)`): that many in each column; the id of",
  "an analysis data (`adsl_itt`, or one of the report's own): its subjects in each column")

.tpl_hier_params <- list(
  tfl_param("population", required = TRUE, doc = "the analysis set's id (SAF)"),
  .tpl_subset_param(),
  tfl_param("hierarchy", required = TRUE, doc = "the levels, outer first"),
  tfl_param("data", "ADAE", doc = "the dataset of records"),
  tfl_param("where", NULL, tier = "usual", doc = "the records kept (R)"),
  tfl_param("by", "TRT01A", tier = "usual", doc = "the column variable"),
  tfl_param("any_row", FALSE, tier = "usual",
            doc = "TRUE: the row of the subjects with any record, first"),
  tfl_param("any_row_label", "Any event", doc = "that row's label"),
  tfl_param("heading", c("System Organ Class", "   Preferred Term"), tier = "usual",
            doc = "the label column's heading, a line a level"),
  tfl_param("sort", c(".overall", "group1", ".depth", "-n", "label"),
            type = "text", multiple = TRUE, doc = "the rows' order (plan_sort())"),
  tfl_param("min_pct", NULL, tier = "usual",
            doc = "the rows kept: this % of subjects or more (a step of the ARD); NULL: all"),
  tfl_param("min_pct_in", "any",
            doc = "where min_pct is met: \"any\" column, \"all\" of them, or the \"total\" column"),
  tfl_param("count_cell", NULL, study = "plan_cells.categorical",
            doc = "a count's cell, n (%) (plan_cells(categorical = )); NULL: the study standard's"),
  tfl_param("denominator", NULL, doc = .tpl_den_doc),
  tfl_param("total", FALSE, doc = "TRUE: a Total column (the analysis again without `by`, plan_total())"),
  tfl_param("header_n", "denominator", type = "choice",
            choices = c("denominator", "set", "tested", "none"), doc = .tpl_header_n_stack_doc),
  .tpl_split_param("group_force")
)
names(.tpl_hier_params) <- vapply(.tpl_hier_params, `[[`, "", "name")

# header_n, checked: `allowed` the values the template takes (the first is
# the default); any other stops with "<template>: header_n = "<x>" <why>".
# The values are "denominator", "set", "tested" and "none".
.tpl_header_n <- function(a, ctx, allowed, why) {
  tn <- ctx$template$name %||% "template"
  .tpl_unset_stop(a, "header_n", tn)
  hn <- a$header_n %||% allowed[1L]
  if (!(is.character(hn) && length(hn) == 1L && !is.na(hn) &&
        hn %in% c("denominator", "set", "tested", "none"))) {
    .ard_stop(sprintf("%s: header_n takes \"denominator\", \"set\", \"tested\" or \"none\".", tn))
  }
  if (!(hn %in% allowed)) .ard_stop(sprintf("%s: header_n = \"%s\" %s", tn, hn, why))
  hn
}

# the stack templates' header N is the stack's own denominator: "set" is the
# same when the denominator is the analysis set, "tested" when it is the
# records' subjects ("data"); a header N apart from it is not supported yet
.tpl_header_n_stack <- function(a, ctx, den = a$denominator) {
  .tpl_header_n(a, ctx, c("denominator", "none", if (is.null(den)) "set",
                          if (identical(den, "data")) "tested"),
                paste0("differs from the percentages' denominator; a header N apart from it ",
                       "is not supported yet: use header_n = \"denominator\" or \"none\"."))
}

# The analysis data `denominator = "data"` is for a template of records: the
# subjects of `set` (the records kept), one row a subject and arm
.tpl_den_records <- function(set, by) {
  list(id = paste0(set, "_den"), label = "the subjects with a record kept", from = set,
       code = tfl_r(sprintf("dplyr::distinct(%s, %s)", set, paste(c("USUBJID", by), collapse = ", "))))
}

# The values of a code list (as the YAML holds it: rows of value / label; or
# the values themselves), NULL when the variable has none
.tpl_cl_values <- function(codelists, var) {
  cl <- codelists[[var]]
  if (is.null(cl)) return(NULL)
  if (is.character(cl)) return(unname(cl))
  vapply(cl, function(r) as.character(r$value %||% r$label), "")
}

# The stack templates' `denominator`, checked.  It is NULL (the analysis
# set), "data" (the records' subjects), a number (every column's N), numbers
# named by the values of `by`, or the id of an analysis data (a text of
# numbers, "50" or "c(\"Drug X\" = 60)", is read as the numbers).  Returns
# `value`, what the analysis's `denominator` takes (NULL: the analysis set
# `subj`); `header`, the same but NULL when it is the analysis set (for
# header_n); `extra`, the analysis data to add (the subjects of another
# analysis set, `adsl_<id>`).
.tpl_den <- function(a, ctx, subj) {
  tn <- ctx$template$name %||% "template"
  by <- a$by
  v <- a$denominator
  out <- function(value, extra = list()) {
    list(value = value, header = if (identical(value, subj)) NULL else value, extra = extra)
  }
  if (is.null(v)) return(out(NULL))
  if (is.list(v) && length(v) && !is.null(names(v)) &&
      all(vapply(v, function(z) is.numeric(z) && length(z) == 1L, NA))) v <- unlist(v)
  if (is.character(v) && length(v) == 1L && !is.na(v)) {
    t <- trimws(v)
    if (identical(t, "data")) return(out("data"))
    num <- .den_numbers(t)
    if (is.null(num)) {
      if (grepl("^[a-z][a-z0-9_.]*$", t)) {
        if (identical(t, subj)) return(out(NULL))
        pops <- vapply(ctx$population %||% list(), function(p) as.character(p$id %||% ""), "")
        hit <- pops[nzchar(pops) & paste0("adsl_", tolower(pops)) == t]
        extra <- if (length(hit)) list(list(
          id = t, label = .tpl_set_label(hit[1L], ctx$population), from = "ADSL",
          population = hit[1L])) else list()
        return(out(t, extra))
      }
    } else if (length(num) == 1L && is.na(num)) {
      .ard_stop(sprintf(paste0(
        "%s: denominator \"%s\": numbers are whole and each named by a value of %s: ",
        "c(\"Drug X\" = 60, \"Control\" = 55)."), tn, t, by))
    } else v <- num
  }
  if (!is.numeric(v) || !length(v)) {
    .ard_stop(sprintf(paste0(
      "%s: denominator takes NULL (the analysis set), \"data\" (the records' subjects), a number, ",
      "numbers named by the values of %s, or an analysis data's id."), tn, by))
  }
  nm <- names(v)
  if (length(v) == 1L && is.null(nm)) {
    if (is.na(v) || v <= 0 || v != round(v) || v > .Machine$integer.max) {
      .ard_stop(sprintf(paste0(
        "%s: denominator = %s: a number is the subjects of every column: a positive whole number."),
        tn, format(v)))
    }
    if (isTRUE(a[["total"]])) {
      .ard_stop(sprintf(paste0(
        "%s: denominator = %s with total = TRUE: one number is each arm's N and would be the Total ",
        "column's too; give numbers named by the values of %s (the Total column is their sum), ",
        "or an analysis data's id."), tn, format(v), by))
    }
    return(out(as.integer(v)))
  }
  if (is.null(nm) || anyNA(nm) || !all(nzchar(nm))) {
    .ard_stop(sprintf(paste0(
      "%s: denominator: several numbers are named by the values of %s, ",
      "c(\"Drug X\" = 60, \"Control\" = 55)."), tn, by))
  }
  if (anyDuplicated(nm)) {
    .ard_stop(sprintf("%s: denominator names %s twice.", tn,
                      paste(dQuote(unique(nm[duplicated(nm)]), FALSE), collapse = ", ")))
  }
  if (anyNA(v) || any(v <= 0) || any(v != round(v)) || any(v > .Machine$integer.max)) {
    .ard_stop(sprintf(paste0(
      "%s: denominator: the numbers are the subjects of each column: positive whole numbers."),
      tn))
  }
  lv <- .tpl_cl_values(ctx$codelists, by)
  if (length(lv)) {
    unk <- setdiff(nm, lv)
    if (length(unk)) {
      .ard_stop(sprintf("%s: denominator names %s, not a value of %s (%s).", tn,
                        paste(dQuote(unk, FALSE), collapse = ", "), by,
                        paste(lv, collapse = ", ")))
    }
    miss <- setdiff(lv, nm)
    if (length(miss)) {
      .ard_stop(sprintf("%s: denominator has no number for %s, a value of %s: give one for each (%s).",
                        tn, paste(dQuote(miss, FALSE), collapse = ", "), by,
                        paste(lv, collapse = ", ")))
    }
  }
  out(stats::setNames(as.integer(v), nm))
}

# The terms kept: `min_pct` % of subjects or more, in any column, every
# column, or the Total column.  A step on the hierarchical ARD, so that it
# holds only the terms the table shows (the plan filters no rows):
# keep_min_pct() of the study's helpers.  NULL when none is set
.tpl_min_pct_post <- function(min_pct, min_pct_in = "any", a = NULL, tn = "template") {
  if (is.null(min_pct)) return(NULL)
  in_ <- .tpl_choice(a, "min_pct_in", c("any", "all", "total"), tn, value = min_pct_in)
  if (identical(in_, "total") && !isTRUE(a[["total"]])) {
    .ard_stop(sprintf(paste0("%s: min_pct_in = \"total\" needs the Total column: add it ",
                             "(total = TRUE), or use min_pct_in = \"any\"."), tn))
  }
  tfl_r(if (identical(in_, "any")) sprintf("keep_min_pct(%s)", format(min_pct))
        else sprintf("keep_min_pct(%s, \"%s\")", format(min_pct), in_))
}

.tpl_hier_body <- function(a, ctx) {
  pop <- a$population
  subj <- paste0("adsl_", tolower(pop))
  # (the header N is the stack's own denominator: only the text changes)
  dn <- .tpl_den(a, ctx, subj)
  hn <- .tpl_header_n_stack(a, ctx, den = dn$header)
  # the any-record row: its label, when it is shown
  overall <- if (isTRUE(a[["any_row"]])) a[["any_row_label"]]
  set <- a[["data_id"]] %||% paste0(tolower(a[["data"]]), "_", tolower(pop))
  d <- list(id = set, label = a[["data_label"]], from = a[["data"]], subjects = subj,
            where = if (!is.null(a$where)) tfl_r(a$where))
  nn <- function(x) Filter(Negate(is.null), x)
  # "data": the percentages' denominator is the records' subjects -- the
  # set's subjects with a record kept, one row each (so within the subset)
  rec_den <- identical(dn$value, "data")
  den_id <- paste0(set, "_den")
  ard <- list(
    analysis_data = c(list(
      list(id = subj, label = .tpl_set_label(pop, ctx$population), from = "ADSL",
           population = pop)),
      dn$extra, list(nn(d)),
      if (rec_den) list(list(
        id = den_id, label = "the subjects with a record kept", from = set,
        code = tfl_r(sprintf("dplyr::distinct(%s, %s)", set,
                             paste(c("USUBJID", a$by), collapse = ", ")))))),
    analyses = list(nn(list(
      id = a[["analysis_id"]] %||% "AE", label = a[["analysis_label"]] %||% ctx$template$label,
      method = "hierarchical", data = set,
      by = a$by, overall = if (isTRUE(a[["total"]])) TRUE, variables = a$hierarchy,
      denominator = if (rec_den) den_id else dn$value %||% subj,
      args = if (!is.null(overall)) tfl_r("over_variables = TRUE"),
      post = .tpl_min_pct_post(a[["min_pct"]], a$min_pct_in, a, ctx$template$name),
      purpose = "PRIMARY OUTCOME MEASURE"))))
  n <- length(a$hierarchy)
  cells <- nn(list(categorical = a[["count_cell"]]))
  content <- c(list(
    tfl_item("normalize_ard", nn(list(hierarchy = a$hierarchy, overall = overall))),
    # (one level: the term alone, no row keys above it)
    tfl_item("table_plan", nn(list(
      cols = a$by,
      rows = if (n > 1L) stats::setNames(as.list(a$hierarchy[-n]), paste0("group", seq_len(n - 1L))),
      label = list(label = a$hierarchy[n])))),
    # (the columns' order is the ARD's: the code list applied when it is made)
    tfl_item("plan_sort", list(keys = a$sort))),
    if (length(cells)) list(tfl_item("plan_cells", cells)),
    if (isTRUE(a[["total"]])) list(tfl_item("plan_total", list(position = "last"))),
    # (the layout's column header, its counts' line: the study standard's wins)
    list(tfl_item("plan_col_header", list(header = list(
      list(line = "1", cols = "row_label"),
      list(line = "1", cols = ".values", span = "each", text = if (identical(hn, "none")) "{col}\nn (%)" else "{col}\n(N={n})\nn (%)"),
      list(line = "2", cols = "row_label", text = paste(a$heading, collapse = "\n")),
      list(line = "2", cols = ".values"))))),
    list(tfl_item("plan_paginate_rows", list(
      max_rows = 22L, split = .tpl_choice(a, "split", .tpl_split_choices, ctx$template$name)))))
  list(ard = ard, data = list(source = "ard", object = "ard"), content = content)
}

.tfl_template("hierarchical", kind = "table", layout = "hierarchical",
              params = .tpl_hier_params, body = .tpl_hier_body)

# ae_soc_pt: treatment-emergent AEs by SOC and PT -- hierarchical's
# parameters, its own defaults; FDA's serious and discontinuation tables
# its variants
tfl_template_variant("ae_soc_pt", from = "hierarchical", ai = TRUE,
  label = "Adverse events by SOC and PT (FDA STF Table 14)",
  example = list(population = "SAF"),
  key = "hierarchy",
  defaults = list(hierarchy = c("AEBODSYS", "AEDECOD"), where = "TRTEMFL == \"Y\"",
                  any_row = TRUE, any_row_label = "Any TEAE"))
tfl_template_variant("ae_ser_soc_pt", from = "ae_soc_pt",
  label = "Serious adverse events by SOC and PT (FDA STF Table 10)",
  defaults = list(where = "TRTEMFL == \"Y\" & AESER == \"Y\"",
                  any_row_label = "Any SAE"))
tfl_template_variant("ae_disc_soc_pt", from = "ae_soc_pt",
  label = "Adverse events leading to treatment discontinuation by SOC and PT (FDA STF Table 12)",
  defaults = list(where = "TRTEMFL == \"Y\" & AEACN == \"DRUG WITHDRAWN\"",
                  any_row_label = "Subjects with at least one AE leading to treatment discontinuation"))

#' Adverse events by SOC and PT: a table plan from the template `ae_soc_pt`
#'
#' A display template on the `hierarchical` layout: treatment-emergent
#' events (`TRTEMFL == "Y"`), the subjects with any event first.  With
#' `min_pct`, the ARD program keeps the PTs of that percentage of subjects
#' or more (`min_pct_in`: in any column, every column, or the Total column
#' with `total = TRUE`) and the SOCs over them (`keep_min_pct()`); this
#' function shows the terms of the ARD it is given, so `min_pct` here does
#' not filter it.
#'
#' @inheritParams tpl_demog
#' @param ... The template's other parameters: `hierarchy`, `where`,
#'   `any_row`, `any_row_label`, `by`, `sort`, `heading` ...
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
#'   `any_row`, `any_row_label`, `sort`, `heading` ...
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
  tfl_param("term", "AEDECOD", tier = "key", doc = "the term variable (the PT)"),
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
  tfl_param("heading", "Preferred Term", doc = "the label column's heading"),
  tfl_param("denominator", NULL, doc = .tpl_den_doc),
  tfl_param("header_n", "denominator", type = "choice",
            choices = c("denominator", "set", "tested", "none"), doc = .tpl_header_n_stack_doc),
  .tpl_split_param("group_safe")
)
names(.tpl_pt_params) <- vapply(.tpl_pt_params, `[[`, "", "name")

.tpl_pt_body <- function(a, ctx) {
  in_ <- .tpl_choice(a, "min_pct_in", c("any", "all", "total"), ctx$template$name)
  if (!is.null(a$min_pct) && identical(in_, "total")) {
    .ard_stop(sprintf(paste0("%s: min_pct_in = \"total\" needs the Total column, which this ",
                             "template does not have: use min_pct_in = \"any\" (or \"all\")."),
                      ctx$template$name))
  }
  # hierarchical's ARD and items, of one level (the term); then its own
  # order, the terms kept and the page
  # (hierarchical's defaults for what ae_pt_freq does not take)
  h <- lapply(.tpl_hier_params, `[[`, "default")
  # (its own ids and labels too: the layout's body reads them when given)
  h[names(a)] <- a
  attr(h, "unset") <- attr(a, "unset")
  h$hierarchy <- a$term
  h$any_row <- !is.null(a$overall)
  h$any_row_label <- a$overall
  out <- .tpl_hier_body(h, ctx)
  # the order: a column's subjects, most first (the PT's name when equal)
  col <- a$sort_by %||%
    (if (identical(in_, "total")) "Total" else .tpl_levels(a$by, ctx$codelists)[[a$by]][1L])
  key <- if (length(col) && !is.na(col)) sprintf("-n[%s]", col) else "-n"
  it <- out$content
  fn <- vapply(it, `[[`, "", "fn")
  it[[match("plan_sort", fn)]]$args <- list(keys = c(".overall", key, "label"))
  it[[match("plan_paginate_rows", fn)]]$args <- list(
    max_rows = 22L, split = .tpl_choice(a, "split", .tpl_split_choices, ctx$template$name))
  out$content <- it
  out
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
#' adverse events) or in every column (`"all"`).  The terms kept are a
#' step of the ARD program (`keep_min_pct()`); this function shows the
#' terms of the ARD it is given, so `min_pct` here does not filter it.
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
  tfl_param("grade", "AESEV", tier = "key", doc = "the severity or grade variable (AESEV, AETOXGR)"),
  tfl_param("grade_levels", NULL,
            doc = paste("its values, lowest first: the rank of the maximum, and the order the",
                        "table shows them in; NULL: the code list of `grade`")),
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
            doc = "the label column's heading, a line a level"),
  tfl_param("denominator", NULL, doc = .tpl_den_doc),
  tfl_param("header_n", "denominator", type = "choice",
            choices = c("denominator", "set", "tested", "none"), doc = .tpl_header_n_stack_doc),
  .tpl_split_param("group_force")
)
names(.tpl_mg_params) <- vapply(.tpl_mg_params, `[[`, "", "name")

.tpl_mg_body <- function(a, ctx) {
  rows_in <- identical(.tpl_choice(a, "grade_in", c("rows", "cols"), ctx$template$name), "rows")
  pop <- a$population
  subj <- paste0("adsl_", tolower(pop))
  # (the header N is the stack's own denominator: only the text changes)
  dn <- .tpl_den(a, ctx, subj)
  hn <- .tpl_header_n_stack(a, ctx, den = dn$header)
  set <- a$data_id %||% paste0(tolower(a$data), "_", tolower(pop))
  # "data": the percentages' denominator is the records' subjects -- the
  # set's subjects with a record kept, one row each
  rec_den <- identical(dn$value, "data")
  den_id <- paste0(set, "_den")
  g <- a$grade
  h <- a$hierarchy
  # the grades in their order: `grade_levels`, else the report's code list
  lv <- a$grade_levels %||% .tpl_levels(g, ctx$codelists)[[g]]
  # a record's rank: its grade among the levels (lowest first)
  rank <- if (length(lv)) {
    sprintf("match(%s, c(%s))", g, paste0("\"", lv, "\"", collapse = ", "))
  } else sprintf("xtfrm(%s)", g)
  # the grades as the ARD holds them (the codes), in their
  # order on the table: a data step's factor order does not reach it --
  # cards' hierarchical ARD puts the innermost variable's values in
  # alphabetical order whatever its levels are -- so the plan says it
  lv_ard <- lv
  # each subject's record of the highest grade: overall, in each SOC, in each PT
  top <- function(suffix, within) {
    list(id = paste0(set, "_max", suffix), from = set,
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
    denominator = if (rec_den) den_id else dn$value %||% subj,
    args = if (over) tfl_r("over_variables = TRUE"),
    purpose = "PRIMARY OUTCOME MEASURE"))
  ard <- list(
    analysis_data = c(list(
      list(id = subj, label = .tpl_set_label(pop, ctx$population), from = "ADSL",
           population = pop)),
      dn$extra, list(Filter(Negate(is.null), d)),
      if (rec_den) list(list(
        id = den_id, label = "the subjects with a record kept", from = set,
        code = tfl_r(sprintf("dplyr::distinct(%s, %s)", set,
                             paste(c("USUBJID", a$by), collapse = ", "))))),
      lapply(maxes, function(m) Filter(Negate(is.null), m))),
    analyses = list(
      an("AE_MAX", maxes[[1L]]$id, c(h[1L], g)),
      an("AE_MAX_SOC", maxes[[2L]]$id, c(h[1L], g)),
      an("AE_MAX_PT", maxes[[3L]]$id, c(h, g))))
  n <- length(h)
  by_n <- identical(.tpl_choice(a, "sort", c("n", "name"), ctx$template$name), "n")
  # the terms' row keys (as rows: every level; as columns: all but the PT,
  # the label); each term after its own count (most first), or by name
  grp <- paste0("group", seq_len(if (rows_in) n else n - 1L))
  term_keys <- unlist(lapply(grp, function(k) c(if (by_n) sprintf("-n@%s", k), k)))
  keys <- c("levels(group1)", term_keys, ".depth",
            if (!rows_in) c(if (by_n) "-n", "label"),
            if (rows_in && length(lv_ard)) "levels(label)")
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
  # (grades as columns: the arm x grade columns, and the any-grade column
  # the total adds, are under the arm -- `{n1}`, the arm's N, is the one
  # number the ARD gives all of them)
  head_n <- if (identical(hn, "none")) "{col}\nn (%)" else if (rows_in)
    "{col}\n(N={n})\nn (%)" else "{col}\n(N={n1})\nn (%)"
  content <- c(
    list(tfl_item("normalize_ard", list(hierarchy = c(h, g))),
         # the overall block first, whatever the order of the others
         tfl_item("plan_levels", c(
           stats::setNames(list(a$overall), h[1L]),
           if (length(lv_ard)) {
             if (rows_in) list(label = lv_ard)
             else stats::setNames(list(c(a$any_grade, lv_ard)), g)
           }))),
    plan,
    list(tfl_item("plan_sort", list(keys = keys))),
    list(tfl_item("plan_col_header", list(header = list(
           list(line = "1", cols = "row_label"),
           list(line = "1", cols = ".values", span = "each", text = head_n),
           list(line = "2", cols = "row_label", text = paste(a$heading, collapse = "\n")),
           list(line = "2", cols = ".values")))),
         tfl_item("plan_paginate_rows", list(
           max_rows = 22L, split = .tpl_choice(a, "split", .tpl_split_choices, ctx$template$name)))))
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
#' @param ... The template's other parameters: `by` and `categorical` (the
#'   status variables, `EOSSTT`) are required, then `labels`, `formats`,
#'   `count_cell`, `missing_row`, `total`, `max_rows` (24) ...
#' @return A [rtfreporter::table_plan()].
#' @export
tpl_disposition <- function(ard, population, ..., codelists = NULL) {
  .tpl_object("disposition", ard, c(list(population = population), list(...)),
              codelists = codelists)
}

# ---- shift: a category at baseline by the same after it, the layout -------------

# (the title row `{PAGE_HEADING}` of the report's page: titles puts it
# above the last line, where the shell has it)
.tpl_param_heading_doc <- paste(
  "a parameter's page name (`{value}`: the parameter); the last title line,",
  "or where the title line `{PAGE_HEADING}` is; used with `page_by`")

.tpl_page_by_doc <- paste(
  "the variable a page a value is cut by: the parameter's (the default), named in the",
  "titles by `page_heading`; unset: no page a value, one table, the parameter a row heading")

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
  tfl_param("heading", "Parameter / Baseline", doc = "the label column's heading"),
  tfl_param("post_header", "{col}", doc = paste(
    "the header over each post-baseline category",
    "(`{col}`: the category; a typed \\n breaks the line)")),
  tfl_param("worst", c("HIGH", "LOW", "NORMAL"), doc = paste(
    "a subject once a parameter, at its worst category after baseline, the",
    "categories from the worst; NULL: every record (the baseline's records too)")),
  tfl_param("worst_by", NULL, doc = paste(
    "a parameter's own order from the worst, by its value (HGB: LOW, HIGH, NORMAL);",
    "the others take `worst`")),
  tfl_param("percent_of", "total", doc = paste(
    "what a cell's {p} is of, set apart from the header N (`header_n`):",
    "\"total\" (default) the Total row -- the arm's subjects with both a baseline",
    "and a category after baseline for the parameter, so the Total is 100%;",
    "\"row\" the subjects of its baseline category")),
  tfl_param("missing_label", NULL, doc = paste(
    "the row of the subjects with no baseline category, counted in the Total and",
    "in the % denominator; NULL (default): those subjects are left out of the",
    "table, not in any row or N")),
  tfl_param("total_label", "Total", doc = "the row of all the subjects; NULL: none"),
  tfl_param("page_by", "PARAMCD", type = "vars", multiple = FALSE, unset = "step_off",
            doc = .tpl_page_by_doc),
  tfl_param("page_heading", "Parameter: {value}",
            doc = .tpl_param_heading_doc),
  tfl_param("header_n", "set", type = "choice", choices = c("set", "tested", "none"),
            doc = paste(
    "the arm headings' N, set apart from the % denominator (`percent_of`):",
    "\"set\" (default): the arm's subjects in the analysis set (with `subset`);",
    "\"tested\": the page's subjects with a baseline and a post-baseline category",
    "(needs `page_by`); \"none\": no N")),
  .tpl_split_param("none")
)
names(.tpl_shift_params) <- vapply(.tpl_shift_params, `[[`, "", "name")

# shift's records, a subject once a parameter at its worst category after
# baseline (the code of its analysis data: the subjects in reach as
# adsl_sub), the baseline's missing category a row of its own, and the
# Total row as the baseline's last category (plan_total()'s Total row reads
# the column variable's margins, here the body's own variable)
.tpl_shift_code <- function(a, subj) {
  q <- function(x) paste0("\"", x, "\"", collapse = ", ")
  # (a transport file holds "" for no value: not a category)
  keep <- c("!ABLFL %in% \"Y\"", sprintf("!%s %%in%% c(NA, \"\")", a$post),
            # (no `missing_label`: a subject with no baseline category is out
            # of the table -- no row, not in the Total or any N)
            if (!length(a$missing_label)) sprintf("!%s %%in%% c(NA, \"\")", a$baseline),
            a$where)
  paste(c(
    "# the records after baseline, the subjects' column variable",
    sprintf("d <- dplyr::inner_join(dplyr::select(%s, -dplyr::any_of(%s)),",
            tolower(a$data), q(a$by)),
    sprintf("                       dplyr::select(%s, USUBJID, %s), by = \"USUBJID\")",
            subj, a$by),
    sprintf("d <- dplyr::filter(d, %s)", paste(keep, collapse = ", ")),
    "# a subject once a parameter: its worst category",
    sprintf("d <- dplyr::arrange(d, %s)", .tpl_shift_rank(a)),
    sprintf("d <- dplyr::distinct(d, USUBJID, %s, .keep_all = TRUE)", a$param),
    # (a blank category is missing too: a transport file holds "" for NA)
    if (length(a$missing_label)) sprintf(
      "d <- dplyr::mutate(d, %s = dplyr::coalesce(dplyr::na_if(as.character(%s), \"\"), \"%s\"))",
      a$baseline, a$baseline, a$missing_label),
    if (length(a$total_label)) c(
      "# the Total row: every subject once more, under the baseline's Total",
      sprintf("d <- dplyr::bind_rows(d, dplyr::mutate(d, %s = \"%s\"))",
              a$baseline, a$total_label)),
    "d"), collapse = "
")
}

# The order from the worst, the R a records' sort takes: `worst`, or a
# parameter's own (`worst_by`, by its value)
.tpl_shift_rank <- function(a) {
  q <- function(x) paste0("\"", x, "\"", collapse = ", ")
  m <- function(o) sprintf("match(%s, c(%s))", a$post, q(o))
  by <- a$worst_by
  if (!length(by)) return(m(a$worst))
  ord <- lapply(by, function(o) {
    o <- unlist(o)
    if (length(o) == 1L) trimws(strsplit(o, ",", fixed = TRUE)[[1L]]) else o
  })
  paste0("dplyr::case_when(", paste(c(
    sprintf("%s == \"%s\" ~ %s", a$param, names(ord), vapply(ord, m, "")),
    sprintf("TRUE ~ %s", m(a$worst))), collapse = ", "), ")")
}

# The baseline column's code list with the Missing and Total rows the
# analysis data's code adds (value = label), or NULL: the template's
# `codelists`, which the report's own lists take the values they lack from
.tpl_shift_codelists <- function(a, worst, codelists) {
  cl <- codelists[[a$baseline]]
  if (is.null(cl) || !worst) return(NULL)
  rows <- .tpl_cl_rows(cl)
  extra <- c(a$missing_label, a$total_label)
  rows <- c(rows, lapply(extra, function(v) list(value = v, label = v)))
  stats::setNames(list(rows), a$baseline)
}

# A code list as rows of value / label (a program's named or plain vector
# of codes too)
.tpl_cl_rows <- function(cl) {
  if (!is.character(cl)) return(cl)
  val <- names(cl) %||% unname(cl)
  lapply(seq_along(cl), function(i) list(value = val[[i]], label = unname(cl)[[i]]))
}

.tpl_shift_body <- function(a, ctx) {
  set <- a$data_id %||% paste0(tolower(a$data), "_", tolower(a$population))
  worst <- length(a$worst) > 0L
  # (a subset: the set's ADSL data keeps the subjects -- the subset's
  # where added to it after -- and the code reads them from it)
  sub <- worst && length(a$subset)
  subj <- if (sub) paste0("adsl_", tolower(a$population)) else "adsl_sub"
  # the percents of the Total row: the denominator the arm's subjects a
  # parameter each (the Total row's records, or every subject once)
  total <- identical(a$percent_of, "total")
  if (total && !worst) {
    # (the default percent_of needs `worst`; given, it is an error)
    if ("percent_of" %in% ctx$given) {
      .ard_stop("shift: percent_of = \"total\" counts a subject once a parameter; give `worst`.")
    }
    total <- FALSE
  }
  den <- paste0(set, "_den")
  .tpl_unset_stop(a, "header_n", ctx$template$name %||% "shift")
  hn <- a$header_n
  if (!(is.character(hn) && length(hn) == 1L && hn %in% c("set", "tested", "none"))) {
    .ard_stop(paste0("shift: header_n takes \"set\" (the arm's subjects in the analysis ",
                     "set), \"tested\" (the page's subjects with a baseline and a ",
                     "post-baseline category) or \"none\" (no N)."))
  }
  tn <- ctx$template$name %||% "shift"
  paged <- !is.null(.tpl_page_by(a, ctx, a$param, tn))
  split <- .tpl_choice(a, "split", .tpl_split_choices, tn)
  if (identical(hn, "tested") && !paged) {
    .ard_stop(sprintf(paste0("%s: header_n = \"tested\" needs page_by (the N is a ",
                             "page's parameter's); use header_n = \"set\" or \"none\"."), tn))
  }
  if (identical(hn, "tested") && !identical(split, "none")) {
    .ard_stop(sprintf("%s: split = \"%s\" does not go with header_n = \"tested\" (a page a parameter).",
                      tn, split))
  }
  # each page's N ("tested"): the arm's subjects for the page's parameter,
  # from the same records as the table (the Total row's, or the set's own)
  page_n <- paged && identical(hn, "tested")
  # the set's N ("set"): GROUPN counts the arm in the analysis set's ADSL
  # (the subset's subjects when there is one), as the other templates do
  set_n <- identical(hn, "set")
  # (with a `subset` the set's ADSL data is there: made here when `worst` is
  # given, or by the build for the records)
  gsubj <- paste0("adsl_", tolower(a$population))
  ard <- list(
    analysis_data = c(
      if (sub) list(list(id = subj, label = .tpl_set_label(a$population, ctx$population),
                         from = "ADSL", population = a$population)),
      list(Filter(Negate(is.null), list(
        id = set, from = a$data, population = a$population,
        code = if (worst) tfl_r(.tpl_shift_code(a, subj)),
        where = if (!worst && !is.null(a$where)) tfl_r(a$where)))),
      # (written as R: a `keep` would keep the columns the analysis reads,
      # the baseline's among them, and cards would look for its levels)
      if (total || page_n) list(list(
        id = den, label = "the subjects with a category after baseline, a parameter each",
        from = set,
        code = tfl_r(sprintf("dplyr::distinct(%s, %s)",
                             if (worst && length(a$total_label))
                               sprintf("dplyr::filter(%s, %s == \"%s\")", set, a$baseline,
                                       a$total_label) else set,
                             paste(c("USUBJID", a$by, a$param), collapse = ", "))))),
      if (set_n && !length(a$subset)) list(list(
        id = gsubj, label = .tpl_set_label(a$population, ctx$population),
        from = "ADSL", population = a$population))),
    analyses = c(
      list(Filter(Negate(is.null), list(
        id = a$analysis_id, method = "categorical", data = set,
        by = c(a$by, a$param, a$baseline), variables = a$post,
        denominator = if (total) den))),
      if (page_n) list(list(id = paste0(a$analysis_id, "_N"), method = "categorical",
                            data = den, by = a$param, variables = a$by)),
      if (set_n) list(list(id = "GROUPN", method = "categorical", data = gsubj,
                           variables = a$by, purpose = "PRIMARY OUTCOME MEASURE"))))
  # the columns: the ARD's order (the code list applied when it is made),
  # or `by_levels` when the report says them
  by_lv <- a$by_levels
  # (as the ARD holds them: the codes; the Missing and Total rows of the
  # code, last)
  post_lv <- a$levels
  # (the baseline's list when the post variable has none: the same categories)
  post_dict <- .tpl_cl_dict(ctx$codelists[[a$post]] %||% ctx$codelists[[a$baseline]])
  base_lv <- c(a$levels, if (worst) c(a$missing_label, a$total_label))
  lv <- c(if (length(by_lv)) stats::setNames(list(by_lv), a$by),
          stats::setNames(list(base_lv, post_lv), c(a$baseline, ".label")))
  content <- c(
    list(tfl_item("table_plan", list(cols = c(a$by, ".label"),
                                     rows = list(group = a$param), label = a$baseline)),
         tfl_item("plan_cells", list(categorical = a$cell)),
         tfl_item("plan_levels", lv)),
    # (the parameters' text, and the post-baseline categories' -- the
    # code list's labels of their codes -- under the column key `.label`)
    if (length(a$param_labels) || !is.null(post_dict)) list(tfl_item("plan_labels", c(
      if (length(a$param_labels)) stats::setNames(list(as.list(a$param_labels)), a$param),
      if (!is.null(post_dict)) list(.label = post_dict)))),
    # (a page a parameter: the parameter in the titles, not the stub)
    list(tfl_item("plan_stub", list(vars = c(if (!paged) "group", a$baseline),
                                    name = "row_label",
                                    before = TRUE)),
         tfl_item("plan_col_header", c(list(header = list(
           list(line = "1", cols = ".values", span = a$by,
                text = if (page_n || set_n) "{col1} (N={n})" else "{col1}"),
           list(line = "2", cols = "row_label", text = .tpl_lines(a$heading)),
           list(line = "2", cols = ".values", span = "each", text = a$post_header))),
           if (page_n) list(values = list(n = TRUE))))),
    if (page_n) list(
      tfl_item("plan_paginate_group", list(keep = FALSE, heading = a$page_heading,
                                           heading_at = "title")))
    else if (paged) list(
      tfl_item("plan_paginate_rows", list(split = split, page_by = "group", heading = a$page_heading,
                                          heading_at = "title")),
      tfl_item("plan_hide", list(cols = list("group"))))
    else if (!identical(split, "none")) list(
      tfl_item("plan_paginate_rows", list(split = split))))
  list(ard = ard, data = list(source = "ard"), content = content,
       codelists = .tpl_shift_codelists(a, worst, ctx$codelists))
}

.tfl_template("shift", kind = "table", layout = "shift",
              params = .tpl_shift_params, body = .tpl_shift_body)

# lab_shift: the normal range indicator at baseline by a visit's, on shift
.tfl_template("lab_shift", kind = "table", layout = "shift",
  params = c(.tpl_shift_params[c("population", "subset")],
             list(tfl_param("visit", required = TRUE, doc = "the visit (AVISIT)"),
                  tfl_param("params", required = TRUE, doc = "the PARAMCD values"),
                  .tpl_where_param(doc = "the records kept besides the visit and parameters (R)")),
             .tpl_shift_params[c("by", "data_id", "by_levels", "param_labels", "levels",
                                 "worst", "worst_by", "percent_of", "cell", "post_header",
                                 "missing_label", "total_label", "page_by",
                                 "page_heading", "header_n", "split")]),
  body = function(a, ctx) {
    b <- lapply(.tpl_shift_params, `[[`, "default")
    for (k in intersect(names(a), names(b))) b[k] <- list(a[[k]])
    attr(b, "unset") <- attr(a, "unset")
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
#' @details
#' The header N and the percentage's denominator are set separately.
#' `header_n` says the N printed in each arm's heading: by default
#' (`"set"`) the arm's N in the analysis set (with `subset`); `"tested"`
#' the arm's subjects in the Total row for the page's parameter (with
#' `page_by`); `"none"` none. `percent_of` says what a cell's percentage
#' is of: by default (`"total"`) the subjects with both a baseline and a
#' post-baseline category for the parameter, so the Total row is 100%;
#' `"row"` the subjects of the baseline category.
#'
#' A subject with no baseline category is left out of the table (in no row,
#' not in the Total or any N) unless `missing_label` is given (for example
#' `"Missing"`): then it has a row of its own, and is counted in the Total
#' and in the percentage's denominator. A subject with no post-baseline
#' category has nothing to shift and is never in the table.
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
    tfl_param("sort", NULL, type = "vars", multiple = TRUE, doc = "the records' order (variables)"),
    # (`study = FALSE`: a listing's `by` is not the study's treatment, which
    # the name `by` takes in the tables' templates)
    tfl_param("by", NULL, type = "vars", multiple = TRUE, study = FALSE, doc = paste(
      "the variables that divide the listing (`by_break`); the records are ordered",
      "by them first, then by `sort`")),
    tfl_param("by_break", "page", type = "choice", choices = c("page", "block"), doc = paste(
      "how `by` divides it: \"page\", each value starts a page; \"block\", one value's",
      "records stay together on a page where they fit (one `by` variable); the value",
      "prints once at the top of its block only when the `by` column is the first",
      "printed column (a change in a column to its left starts the repeats again), so",
      "put it first")),
    tfl_param("by_heading", NULL, type = "text", doc = paste(
      "with \"page\": the page's name, the last title line ({value} is its value;",
      "{VARIABLE} for each of several `by` variables); left out, a variable the listing",
      "does not print names its page, \"VARIABLE: value\"")),
    tfl_param("by_na_label", NULL, type = "text", doc = paste(
      "with \"page\": the text a blank or missing `by` value prints as in the page's",
      "name (`{value}`, `{VARIABLE}`); left out, nothing, so \"Severity: {value}\"",
      "prints \"Severity: \""))
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
    by <- .listing_by(a, ctx$given, cols)
    data <- Filter(Negate(is.null), list(
      source = "dataset", dataset = a$data,
      where = if (!is.null(a$where)) tfl_r(a$where),
      sort = if (is.null(by)) a$sort else .listing_by_sort(by$vars, a$sort)))
    content <- c(
      list(tfl_item("plan_listing", list(cols = if (is.null(by)) cols else by$cols,
                                         type = "multiline"))),
      by$items,
      list(tfl_item("plan_paginate_rows", c(list(max_rows = .listing_rows_a_page), by$paginate))))
    list(data = data, content = content)
  })

# A listing divided by variables (`by`): the columns (a `by` variable the
# listing does not print is one more, hidden), the items between the listing
# and its pages, the pages' own arguments.  NULL for a listing with no `by`.
# "page": each value starts a page (plan_paginate_rows(page_by = )); the
# variable is not printed unless a column is of it, so the page is named,
# as the last title line, unless the listing prints the variable.  "block":
# one variable's records kept together on a page where they fit
# (plan_row_group(group_col = ), plan_paginate_rows() cutting whole groups);
# its column is carried down the records (collapse_repeats), so the value
# prints once at the top of its block and again at the top of a page that
# continues it, when that column is the first printed one (collapse_repeats
# starts again when a column to its left changes).
.listing_by <- function(a, given, cols) {
  by <- as.character(unlist(a$by))
  .tpl_unset_stop(a, "by_break", "listing")
  brk <- a$by_break
  if (length(brk) != 1L || is.na(brk) || !brk %in% c("page", "block")) {
    .ard_stop("template listing: `by_break` is \"page\" (a page a value) or \"block\" (a value's records together).")
  }
  if (!length(by)) {
    asked <- unique(c(intersect(c("by_break", "by_heading", "by_na_label"), given),
                      if (length(a$by_heading)) "by_heading",
                      if (length(a$by_na_label)) "by_na_label"))
    if (length(asked)) {
      .ard_stop(sprintf("template listing: %s is about `by`, the variables that divide the listing; give `by`.",
                        paste(sQuote(asked, FALSE), collapse = " and ")))
    }
    return(NULL)
  }
  if (identical(brk, "block") && length(by) != 1L) {
    .ard_stop("template listing: `by_break` \"block\" keeps one variable's records together: give one `by` variable (or \"page\").")
  }
  if (identical(brk, "block") && length(a$by_heading)) {
    .ard_stop("template listing: `by_heading` names a page: it is for `by_break` \"page\".")
  }
  if (identical(brk, "block") && length(a$by_na_label)) {
    .ard_stop("template listing: `by_na_label` names a page's blank value: it is for `by_break` \"page\".")
  }
  if (length(a$by_na_label) &&
      (length(a$by_na_label) > 1L || !is.character(a$by_na_label) || is.na(a$by_na_label))) {
    .ard_stop("template listing: `by_na_label` is one text.")
  }
  if (length(a$by_heading) > 1L || (length(a$by_heading) && !is.character(a$by_heading))) {
    .ard_stop("template listing: `by_heading` is one text.")
  }
  # the column of each `by` variable: one of just that variable, else a hidden one
  one <- vapply(a$columns, function(cl) {
    v <- as.character(unlist(cl$vars))
    if (length(v) == 1L) v else NA_character_
  }, "")
  printed <- by %in% one
  for (v in by) {
    if (v %in% one) {
      if (identical(brk, "block")) cols[[match(v, one)]]$args$collapse_repeats <- TRUE
    } else {
      cols <- c(cols, list(tfl_call("listing_col", Filter(Negate(is.null), list(
        vars = v, collapse_repeats = if (identical(brk, "block")) TRUE)))))
    }
  }
  hide <- by[!printed]
  items <- c(
    if (length(hide)) list(tfl_item("plan_hide", list(cols = as.list(hide)))),
    if (identical(brk, "block")) list(tfl_item("plan_row_group", list(mode = "value", group_col = by))))
  paginate <- if (identical(brk, "page")) {
    heading <- a$by_heading
    if (!length(heading) && !all(printed)) {
      heading <- if (length(by) == 1L) paste0(by, ": {value}") else
        paste(sprintf("%s: {%s}", by, by), collapse = "\n")
    }
    c(list(page_by = if (length(by) == 1L) by else as.list(by)),
      if (length(heading)) list(heading = heading, heading_at = "title"),
      if (length(a$by_na_label)) list(na_label = a$by_na_label))
  }
  list(vars = by, cols = cols, items = items, paginate = paginate)
}

# The `by` variables are variables of the data (a listing given its data)
.listing_by_in_data <- function(x, by, fn) {
  miss <- setdiff(as.character(unlist(by)), names(x))
  if (length(miss)) {
    .ard_stop(sprintf("%s: `by` names %s, which the data does not have.", fn,
                      paste(sQuote(miss, FALSE), collapse = ", ")))
  }
  invisible(x)
}

# The records' order of a divided listing: the `by` variables first (one
# `sort` already gives, as "-AGE", keeps its place and direction), then the
# rest of `sort`.  A page or a block is a run of the rows of one value.
.listing_by_sort <- function(by, sort) {
  sort <- as.character(unlist(sort))
  bare <- sub("^-", "", sort)
  lead <- unname(vapply(by, function(v) if (v %in% bare) sort[match(v, bare)] else v, ""))
  c(lead, sort[!bare %in% by])
}

#' A listing: a listing plan from the layout `listing`
#'
#' One row a record of a dataset: the records kept (`where`), their order
#' (`sort`) and the columns, each its variables (one or more, stacked in the
#' cell), its heading, its width and whether a repeated value is printed
#' once.  The look is the template's: one record over lines as needed
#' (`multiline`) and 22 rows a page, which a study's style or a report's own
#' item changes.
#'
#' `by` divides the listing by the values of variables.  `by_break` `"page"`
#' (the default) starts a page for each value
#' ([rtfreporter::plan_paginate_rows()]'s `page_by`), 22 rows a page within
#' it; `"block"` keeps one variable's records together on a page where they
#' fit ([rtfreporter::plan_row_group()]'s `group_col`), the value printed
#' once at the top of its block, and again at the top of a page that
#' continues it, when the `by` column is the first printed column (the
#' repeats start again when a column to its left changes, so put it
#' first).  `by_na_label` is the text a blank or missing `by` value prints as
#' in a page's name ([rtfreporter::plan_paginate_rows()]'s `na_label`;
#' `"page"` only).  A `by` variable is not printed as a column unless
#' `columns` has a column of just that variable (for `"block"` that column
#' prints the value once a block; one not printed keeps the block together
#' without marking it).  A page whose variable is not printed is named
#' as the last title line, `"AESEV: MODERATE"`, or by `by_heading`.  The
#' records are ordered by the `by` variables first, then by `sort`; the data
#' of `tpl_listing()` is the program's own, in its order, so it is ordered
#' by the program (a value that comes back later is one page, a block is a
#' run of rows).
#'
#' @param x The dataset (a data frame), the listing's source.
#' @param ... The template's parameters: `columns` (a list of columns, each
#'   `list(vars = , label = , width = , collapse_repeats = )`), `where`,
#'   `sort`, `by` (the variables that divide the listing), `by_break`
#'   (`"page"` or `"block"`), `by_heading` (a page's name: `"{value}"` is
#'   its value, with several `by` variables `"{VARIABLE}"` for each) and
#'   `by_na_label` (the text of a blank `by` value in that name);
#'   `data` names the dataset in the spec (`"ADAE"`).
#' @return A [rtfreporter::table_plan()] of a listing.
#' @export
tpl_listing <- function(x, ...) {
  a <- list(...)
  it <- tfl_template_expand("listing", a)
  .listing_by_in_data(x, a$by, "tpl_listing()")
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

# ---- param_change: parameters' value and change from baseline at each visit -------

# The cells' digits (Q6, the user's 11-11 decision: the plan_digits() route,
# not the ARD's formats): one decimal for the mean, the median and the range,
# two for the SD; a parameter that differs its own map in `formats`, keyed by
# its PARAMCD (the ARD's by carries PARAMCD for it, user OK 11-11), before
# them in rtfreporter's order (a row key's value, then the kind's)
.tpl_pc_digits <- list(mean = 1L, sd = 2L, median = 1L, min = 1L, max = 1L)
.tpl_pc_stats <- c("N", "mean", "sd", "median", "min", "max")

# (made when the package loads, after R/tpl_registry_demog.R: the look's
# parameters are demog's, .tpl_look_params())
.tpl_pc_params <- function() {
  p <- c(
    list(
      tfl_param("population", required = TRUE, doc = "the analysis set's id (SAF)"),
      .tpl_subset_param(),
      tfl_param("data", required = TRUE, unset = "stop", doc = "the BDS dataset (ADLB, ADVS, ADEG)"),
      tfl_param("params", NULL, unset = "step_off", doc = "the PARAMCD values, in the data's order; NULL: all"),
      tfl_param("visits", NULL, unset = "step_off", doc = "the AVISIT values; NULL: all"),
      tfl_param("where", NULL, unset = "step_off", doc = "the records kept (R), e.g. ANL01FL == \"Y\""),
      tfl_param("by", required = TRUE, study = FALSE, unset = "stop",
                doc = "the column variable (the arms); not a default and not the study's treatment"),
      tfl_param("layout", "cols", unset = "stop", doc = paste(
        "cols: each arm's value and change side by side;",
        "rows: the value's rows, then the change's, under each visit")),
      tfl_param("param_order", "PARAMN", unset = "step_off",
                doc = "the parameters' order variable; NULL: by their PARAMCD"),
      tfl_param("visit_order", "AVISITN", unset = "step_off",
                doc = "the visits' order variable; NULL: by name"),
      tfl_param("baseline", FALSE, type = "flag", unset = "step_off",
                doc = "TRUE: the baseline's rows (BASE) before the value's"),
      tfl_param("baseline_label", "Baseline", doc = "the baseline's heading"),
      tfl_param("value_label", "Value", doc = "the value's heading"),
      tfl_param("change_label", "Change from baseline", doc = "the change's heading"),
      tfl_param("data_id", NULL, doc = "the analysis data's id; NULL: <data>_<population>"),
      tfl_param("data_label", NULL, doc = "the analysis data's label"),
      tfl_param("formats", .tpl_pc_digits, tier = "usual", unset = "step_off", doc = paste(
        "the cells' decimals (plan_digits()): by statistic (mean 1, sd 2, median 1, min 1, max 1),",
        "and a parameter's own, only for the parameters that differ, by its PARAMCD:",
        "{GLUC: {mean: 2, sd: 3}}; given by name, merged key by key; unset: none given here.",
        "The table's digits: an ARD built alone keeps cards' default formats")),
      # (the cells place the statistics; their digits are `formats`, plan_digits())
      tfl_param("cells", .tpl_demog_cells, tier = "usual", study = "plan_cells.continuous",
                doc = "the rows of a value or a change: where each statistic goes (placement only, {mean})"),
      tfl_param("sd_missing", "-", type = "text", study = "settings.sd_missing", unset = "step_off",
                doc = paste("what prints in place of the SD when a group has no SD (n = 1):",
                            "{mean} (<sd_missing>); \"\" prints the mean alone; unset: no such guard;",
                            "no effect when `cells` is given")),
      tfl_param("header_n", "set", type = "choice", choices = c("set", "none"), unset = "step_off",
                doc = "the arm headings' N: \"set\": the arm's subjects in the analysis set; \"none\" or unset: no N"),
      tfl_param("page_by", "PARAMCD", type = "vars", multiple = FALSE, unset = "step_off",
                doc = .tpl_page_by_doc),
      tfl_param("page_heading", "Parameter: {value}",
                doc = .tpl_param_heading_doc),
      .tpl_split_param("group_safe"),
      tfl_param("param_labels", NULL, doc = paste(
        "each parameter's text by its PARAMCD {GLUC: \"Glucose (mg/dL)\"}; given by name, merged;",
        "NULL: the study's PARAMCD code list's labels, else the codes print")),
      tfl_param("heading", NULL, doc = "the label column's heading")),
    .tpl_look_params(c("stub_name", "stub_before", "blanks", "blanks_first", "blanks_last",
                       "blanks_counted", "border_header", "widths", "max_rows")))
  stats::setNames(p, vapply(p, `[[`, "", "name"))
}

# A template's rows as a named list, label -> cell template: also from the
# items' form a GUI and an agent write, [{label: , template: }, ...]
.tpl_cells_named <- function(x) {
  if (!is.null(names(x)) || !length(x) || !all(vapply(x, is.list, NA))) return(as.list(x))
  ok <- vapply(x, function(e) is.character(e$label) && is.character(e$template), NA)
  if (!all(ok)) {
    .ard_stop("`cells` is a named list (label: template) or a list of {label: , template: }.")
  }
  stats::setNames(lapply(x, `[[`, "template"), vapply(x, `[[`, "", "label"))
}

# A code list's code -> text (rows of value / label), the codes it labels
.tpl_code_labels <- function(cl) {
  if (!is.list(cl)) return(NULL)
  v <- vapply(cl, function(r) as.character(r$value %||% NA), "")
  l <- vapply(cl, function(r) as.character(r$label %||% NA), "")
  k <- !is.na(v) & !is.na(l) & v != l
  if (any(k)) stats::setNames(l[k], v[k])
}

.tpl_pc_body <- function(a, ctx) {
  tn <- ctx$template$name %||% "param_change"
  a$cells <- .tpl_demog_cells_sd(.tpl_cells_named(a$cells), a[["sd_missing"]])
  rows_in <- identical(.tpl_choice(a, "layout", c("cols", "rows"), tn), "rows")
  hn <- if (is.null(a$header_n)) "none" else .tpl_choice(a, "header_n", c("set", "none"), tn)
  pop <- a$population
  subj <- paste0("adsl_", tolower(pop))
  set <- a$data_id %||% paste0(tolower(a$data), "_", tolower(pop))
  inn <- function(v, x) if (length(x)) {
    sprintf("%s %%in%% c(%s)", v, paste0("\"", x, "\"", collapse = ", "))
  }
  where <- c(a$where, inn("PARAMCD", a$params), inn("AVISIT", a$visits))
  # the parameters and visits as factors in their order: the ARD keeps it
  # (the ARD's by is the parameter's code, PARAMCD, user 11-11; its text,
  # PARAM, is plan_labels()' at the table: no PARAM x PARAMCD crossing)
  ord <- c(if (length(a$param_order))
             sprintf("PARAMCD = reorder_by(PARAMCD, %s)", a$param_order),
           if (length(a$visit_order))
             sprintf("AVISIT = reorder_by(AVISIT, %s)", a$visit_order))
  nn <- function(x) Filter(Negate(is.null), x)
  # the statistics: the six, and any other the cells name (canon 1.37)
  stats <- unique(c(.tpl_pc_stats, .tpl_cell_stats(a$cells)))
  dg <- .tpl_pc_digits_item(a, tn, ctx$unset)
  sets <- list(
    list(id = subj, label = .tpl_set_label(pop, ctx$population), from = "ADSL",
         population = pop),
    nn(list(id = set, label = a$data_label, from = a$data, subjects = subj,
            where = if (length(where)) tfl_r(.tpl_and_all(where)),
            derive = if (length(ord)) tfl_r(paste(ord, collapse = " | ")))),
    # the change: the records that have one (no baseline visit's, so its
    # change cells stay empty)
    nn(list(id = paste0(set, "_chg"), from = set, where = tfl_r("!is.na(CHG)"),
            derive = if (length(a$visit_order)) tfl_r("AVISIT = droplevels(AVISIT)"))))
  an <- function(id, data, v, ...) nn(list(
    id = id, method = "continuous", data = data, by = c(a$by, "PARAMCD", "AVISIT"),
    variables = v, statistics = stats, purpose = "PRIMARY OUTCOME MEASURE", ...))
  # the parameters' text (PARAM) for the table, outside the ARD (canon
  # 1.42): the report's, else the study's PARAMCD code list's
  ptext <- if (length(a$param_labels)) unlist(a$param_labels) else .tpl_code_labels(ctx$codelists[["PARAMCD"]])
  analyses <- c(
    list(list(id = "GROUPN", method = "categorical", data = subj, variables = a$by,
              purpose = "PRIMARY OUTCOME MEASURE")),
    if (isTRUE(a$baseline)) list(an("BASELINE", set, "BASE")),
    # (the change of a visit with no change records -- the baseline, whose
    # level a code list puts back -- has no rows: its cells stay empty)
    list(an("VALUE", set, "AVAL"),
         an("CHANGE", paste0(set, "_chg"), "CHG", post = tfl_r("drop_empty_groups()"))))
  # (`cells` unset: no plan_cells() item, its own default)
  cells_item <- if (length(a$cells)) list(tfl_item("plan_cells", list(continuous = as.list(a$cells))))
  # (`header_n` none or unset: no N line)
  nline <- if (identical(hn, "none")) "" else "\n(N={n})"
  plan <- if (rows_in) {
    c(list(tfl_item("table_plan", list(cols = a$by, rows = list(
        group1 = "PARAMCD", group2 = "AVISIT", group3 = "variable")))),
      cells_item, dg,
      list(tfl_item("plan_sort", list(keys = c("group1", "group2", "group3"))),
           # (one header row: the stub's heading is in it)
           tfl_item("plan_col_header", list(header = c(
             if (length(a$heading)) list(list(line = "1", cols = "row_label",
                                              text = paste(a$heading, collapse = "\n"))),
             list(list(line = "1", cols = ".values", span = "each", text = paste0("{col}", nline))))))))
  } else {
    c(list(tfl_item("table_plan", list(cols = c(a$by, "variable"), rows = list(
        group1 = "PARAMCD", group2 = "AVISIT")))),
      cells_item, dg,
      # the arm and its N over its two columns (the N is the arm's, not
      # a value's or a change's)
      # (the stub's heading on the second row, as shift's)
      list(tfl_item("plan_col_header", list(header = c(
        list(list(line = "1", cols = ".values", span = a$by, text = paste0("{col1}", nline))),
        if (length(a$heading)) list(list(line = "2", cols = "row_label",
                                         text = paste(a$heading, collapse = "\n"))),
        list(list(line = "2", cols = ".values", span = "each", text = "{col}")))))))
  }
  # the look: the template's defaults, the study's where they refer to it; a
  # page a parameter, its name in the titles, not in the body
  arms <- .tpl_levels(a$by, ctx$codelists)[[a$by]]
  paged <- !is.null(.tpl_page_by(a, ctx, "PARAMCD", tn))
  page <- if (paged) nn(list(page_by = "group1", heading = a$page_heading, heading_at = "title"))
  lk <- .tpl_look_items(a, tn, ncol = if (length(arms)) length(arms) * (2L - rows_in) else NA_integer_,
                        page = page)
  # (the heading is the stub's cell: the template names the stub column, as
  # shift's plan_stub does, when no study layer has a plan_stub)
  if (length(a$heading)) {
    if (is.null(lk$stub)) lk$stub <- tfl_item("plan_stub", list(name = "row_label"))
    lk$stub$args$name <- lk$stub$args$name %||% "row_label"
  }
  labels <- nn(list(BASE = if (isTRUE(a$baseline)) a$baseline_label, AVAL = a$value_label,
                    CHG = a$change_label))
  content <- c(plan,
    # (the stub's place: after the header, as before the look's items)
    if (!is.null(lk$stub)) list(lk$stub),
    # (one plan_labels(): the statistics' text and the parameters')
    if (length(labels) || length(ptext)) list(tfl_item("plan_labels", c(
      if (length(labels)) list(variable = labels),
      if (length(ptext)) list(PARAMCD = as.list(ptext))))),
    unname(Filter(Negate(is.null), lk[c("blank", "widths", "paginate")])),
    if (paged) list(tfl_item("plan_hide", list(cols = list("group1")))),
    # (the style last: where the study's lands in the old order, so A = B)
    unname(Filter(Negate(is.null), lk["style"])))
  list(ard = list(analysis_data = sets, analyses = analyses),
       data = list(source = "ard", object = "ard"), content = content)
}

# The digits item from `formats`: its statistics' numbers to
# plan_digits(continuous = ), its maps (a parameter's own, by PARAMCD) to
# plan_digits(PARAMCD = ); the template's default under the given, key by
# key; none when unset
.tpl_pc_digits_item <- function(a, tn, unset = character()) {
  if ("formats" %in% unset || !length(a$formats)) return(NULL)
  f <- as.list(a$formats)
  if (is.null(names(f)) || !all(nzchar(names(f)))) {
    .ard_stop(sprintf(paste0("template %s: `formats` is named: by statistic {mean: 1}, or by a ",
                             "parameter's PARAMCD {GLUC: {mean: 2}}."), tn))
  }
  map <- vapply(f, function(x) is.list(x) || length(x) > 1L || !is.null(names(x)), NA)
  f <- c(utils::modifyList(.tpl_pc_digits, f[!map]), f[map])
  map <- vapply(f, function(x) is.list(x) || length(x) > 1L || !is.null(names(x)), NA)
  one <- function(x, what) {
    if (!length(x)) return(NULL)
    if (is.null(names(x)) || !all(nzchar(names(x)))) {
      .ard_stop(sprintf("template %s: `%s` is by statistic: {mean: 1, sd: 2}.", tn, what))
    }
    ok <- vapply(x, function(v) is.numeric(v) && length(v) == 1L && !is.na(v) && v >= 0 && v == round(v), NA)
    if (!all(ok)) {
      .ard_stop(sprintf("template %s: `%s` gives whole numbers of decimals, 0 or more (%s).", tn, what,
                        paste(names(x)[!ok], collapse = ", ")))
    }
    lapply(x, as.integer)
  }
  cont <- one(f[!map], "formats")
  by <- f[map]
  bad <- setdiff(names(by), a$params)
  if (length(a$params) && length(bad)) {
    .ard_stop(sprintf("template %s: `formats` names %s, not in `params`.", tn, paste(bad, collapse = ", ")))
  }
  by <- if (length(by)) stats::setNames(lapply(names(by), function(k) one(as.list(by[[k]]), "formats")), names(by))
  args <- Filter(length, list(continuous = cont, PARAMCD = by))
  if (length(args)) list(tfl_item("plan_digits", args))
}

.tpl_pc_register <- function() {
  # (`extends`: its family's `[defaults]`, canon 1.27: the study's defaults are
  # laid under the template's items, and its references read them)
  .tfl_template("param_change", kind = "table", layout = "summary",
                params = .tpl_pc_params(), body = .tpl_pc_body, extends = "defaults")
}

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
#' Each parameter is a page of its own, `Parameter: <PARAM>` in its titles
#' (`page_by = !unset`: one table, the parameter a row heading).
#'
#' @inheritParams tpl_demog
#' @param ... The template's other parameters: `data` and `by` (required,
#'   no default), then `params`, `visits`, `where`, `layout`, `param_order`,
#'   `visit_order`, `baseline`, `baseline_label`, `value_label`,
#'   `change_label`, `formats` (the cells' decimals, plan_digits(): by
#'   statistic, and a parameter's own by its PARAMCD:
#'   `list(mean = 2, GLUC = list(sd = 3))`), `cells` (label = template, or a
#'   list of `list(label = , template = )`: placement only),
#'   `header_n`, `page_by`, `page_heading`, `split`, `heading` and the look
#'   (`stub_name`, `blanks`, `border_header`, `widths`, `max_rows` ...).
#' @return A [rtfreporter::table_plan()].
#' @export
tpl_param_change <- function(ard, population, ..., codelists = NULL) {
  .tpl_object("param_change", ard, c(list(population = population), list(...)),
              codelists = codelists)
}

# ---- stat_block: one record's statistics over several rows and columns ------------
# A block is one column key's value (an arm) x one analysis record (its N,
# n, estimate, CI ...); `cells` is the block's grid: an element a column
# (or, `direction = "rows"`, a row), its name the heading, its vector the
# lines within it.  The statistics each group shows are read from its
# templates' {tokens}.  The ARD is the report's own (or a display
# template's, which calls the body): the template does ARD -> table only.

# `cells` as given: a named list (heading = template(s)), or the YAML's
# records {label, template}
.tpl_cells_list <- function(cells) {
  if (!length(cells)) .ard_stop("template stat_block: `cells` is required (heading = template).")
  rec <- is.null(names(cells)) && all(vapply(cells, function(x) is.list(x) && !is.null(x$label), NA))
  if (rec) {
    cells <- stats::setNames(lapply(cells, function(x) unlist(x$template, use.names = FALSE)),
                             vapply(cells, function(x) as.character(x$label), ""))
  }
  if (is.null(names(cells)) || any(!nzchar(names(cells)))) {
    .ard_stop("template stat_block: each of `cells` has a heading (its name, or `label`).")
  }
  lapply(cells, function(x) if (inherits(x, "tfl_r")) x else as.character(unlist(x)))
}

# The statistics a group's templates name: their {tokens}, in order
.tpl_cell_tokens <- function(x) {
  tk <- regmatches(x, gregexpr("\\{[A-Za-z_.][A-Za-z0-9_.]*", x))
  unique(sub("^\\{", "", unlist(tk)))
}

# The body: content items for the ARD's record of `variable`; `others` the
# group the other variables' records go to whole (a display template's
# own rows), `rows_key` the row keys before the groups (direction rows)
.tpl_stat_block_body <- function(a, ctx = NULL, others = NULL, rows_key = NULL,
                                 header = TRUE, levels = FALSE) {
  cells <- .tpl_cells_list(a$cells)
  dir <- .tpl_choice(a, "direction", c("cols", "rows"), ctx$template$name %||% "template",
                     value = a$direction %||% "cols")
  rows <- a$rows
  # the row keys, by name: the variable's (or a display template's), then
  # the report's (subgroup, subgroup_level); no label column -- a block's
  # lines are its rows, the keys' values their titles, a key's repeats
  # collapsed
  keys <- c(rows_key %||% list(group = "variable"),
            if (length(rows)) stats::setNames(as.list(rows), rows))
  plan <- if (dir == "cols") {
    # (`levels`: a display template's own rows -- BOR's categories -- keep
    # their label column)
    c(list(cols = c(a$by, ".stat_group"), rows = keys), if (!levels) list(label = NA))
  } else {
    list(cols = a$by, rows = c(keys, list(stat = ".stat_group")), label = NA)
  }
  groups <- lapply(cells, function(x) .tpl_cell_tokens(if (inherits(x, "tfl_r")) as.character(x) else x))
  # (a vector: the block's lines; as columns, a one-line group beside a
  # longer one is its first line: N on the rate's line, not a row above)
  multi <- dir == "cols" && any(lengths(cells) > 1L)
  cell_of <- function(x) {
    if (inherits(x, "tfl_r") || (length(x) == 1L && !multi)) return(x)
    tfl_r(sprintf("cell_rows(%s)", paste(sprintf("\"%d\" = \"%s\"", seq_along(x),
                                                 gsub("\"", "\\\\\"", x)), collapse = ", ")))
  }
  by_lv <- a$by_levels
  list(
    tfl_item("table_plan", plan),
    tfl_item("plan_stat_groups", c(groups, list(.variables = a$variable),
                                   if (!is.null(others)) list(.others = others))),
    tfl_item("plan_cells", lapply(cells, cell_of)),
    if (length(by_lv)) tfl_item("plan_levels", stats::setNames(list(by_lv), a$by)),
    if (isTRUE(header) && dir == "cols") tfl_item("plan_col_header", list(header = list(
      list(line = "1", cols = ".values", span = a$by, text = "{col1}"),
      list(line = "2", cols = names(keys)[1L], text = a$label %||% ""),
      list(line = "2", cols = ".values", span = "each", text = "{col}")))),
    if (dir == "rows" || any(lengths(cells) > 1L))
      tfl_item("plan_row_group", list(mode = "value", collapse = as.list(names(keys)))))
}

.tfl_template("stat_block", kind = "table", layout = "stat_block",
  params = list(
    tfl_param("by", "TRT01A", doc = "the column key (the arms)"),
    tfl_param("by_levels", NULL, doc = "its values in order; NULL: its code list's"),
    tfl_param("variable", required = TRUE, type = "vars",
              doc = "the analysis variable whose record is split (RESP)"),
    tfl_param("rows", NULL, type = "vars", multiple = TRUE,
              doc = "the row keys (subgroup, subgroup_level); NULL: one row of blocks"),
    tfl_param("cells", required = TRUE, doc = paste(
      "the block's grid: heading = template, one element a column (or a row), a vector",
      "its lines (N: \"{N}\", ORR(%) 90%CI: [\"{n} ({estimate:.1f%})\", \"{conf.low:.1f%}, {conf.high:.1f%}\"])")),
    tfl_param("direction", "cols", doc = "the cells' elements as columns or rows"),
    tfl_param("label", NULL, type = "text", doc = "the row label column's heading")
  ),
  body = function(a, ctx) {
    list(data = list(source = "ard"), content = Filter(Negate(is.null), .tpl_stat_block_body(a, ctx)))
  })

#' One record's statistics over rows and columns: a table plan from the
#' template `stat_block`
#'
#' A layout template: each arm's block holds one analysis record's
#' statistics -- its N in one column, its rate and interval in the next,
#' or under each other -- as `cells` says (`plan_stat_groups()`).  The ARD
#' is the report's own; display templates such as [tpl_orr()] build on it.
#'
#' @inheritParams tpl_demog
#' @param ... The template's parameters: `variable` and `cells` (required),
#'   `by`, `by_levels`, `rows`, `direction`, `label`.  `cells` is a named
#'   list, heading = template, one element a column (`direction = "cols"`)
#'   or a row (`"rows"`), a vector its lines (`tpl_orr()`'s `cells`: an N
#'   column, then the rate over its interval).
#' @return A [rtfreporter::table_plan()].
#' @export
tpl_stat_block <- function(ard, ..., codelists = NULL) {
  .tpl_object("stat_block", ard, list(...), codelists = codelists)
}

# ---- orr: best overall response and the objective response rate (95% CI) --------

# The column variable's `by_levels` as a code list of the template (the data
# step puts the arms in that order), unless the report has a list for it: the
# report's own wins.  NULL: none.
.tpl_by_codelist <- function(a, ctx) {
  if (length(a$by_levels) && is.null(ctx$codelists[[a$by]])) {
    stats::setNames(list(lapply(a$by_levels, function(v) list(value = v, label = v))), a$by)
  }
}

.tfl_template("orr", kind = "table", layout = "stat_block",
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
    tfl_param("conf_level", 0.95, doc = "the rate's CI's level"),
    tfl_param("stat_layout", "rows", type = "choice", choices = c("rows", "cols"), doc = paste(
      "the rate and its CI: \"rows\", under each other in the arm's column, or \"cols\",",
      "each arm's two columns (stat_block)")),
    tfl_param("heading", "Response", doc = "the label column's heading"),
    tfl_param("header_n", "set", type = "choice", choices = c("set", "denominator", "none"),
              doc = paste(
      "the arm headings' N: \"set\" (default) or \"denominator\": the arm's subjects in the",
      "analysis set (with `subset`), the percentages' and the rate's base; \"none\": no N.",
      "\"tested\" is not supported, the rate row's base is the analysis set"))
  ),
  body = function(a, ctx) {
    hn <- .tpl_header_n(a, ctx, c("set", "denominator", "none"), paste0(
      "is not supported: the rate row's base is the analysis set; ",
      "use header_n = \"set\" or \"none\"."))
    set <- a$data_id %||% paste0(tolower(a$data), "_", tolower(a$param))
    resp <- paste(a$responders, collapse = " + ")
    lvl <- as.numeric(a$conf_level %||% 0.95)
    ci_lab <- sprintf("%s%% CI", format(lvl * 100))
    ci_args <- sprintf("method = \"%s\"%s", a$ci_method,
                       if (isTRUE(all.equal(lvl, 0.95))) "" else sprintf(", conf.level = %s", format(lvl)))
    # every subject of the analysis set is a row: one with no record of the
    # parameter is no responder, in the N and the rate's denominator (a
    # record's blank category is missing: no category's, no responder)
    subj <- paste0("adsl_", tolower(a$population))
    rec <- paste0(set, "_records")
    code <- paste0(
      sprintf("dplyr::left_join(\n  dplyr::select(%s, USUBJID, %s),\n", subj, a$by),
      sprintf("  dplyr::select(%s, -dplyr::any_of(\"%s\")),\n", rec, a$by),
      "  by = \"USUBJID\") |>\n",
      sprintf("  dplyr::mutate(RESP = %s %%in%% c(%s))", a$category,
              paste(sprintf("\"%s\"", a$responders), collapse = ", ")))
    ard <- list(
      analysis_data = list(
        list(id = subj, label = .tpl_set_label(a$population, ctx$population), from = "ADSL",
             population = a$population),
        list(id = rec, label = sprintf("%s records of %s", a$param, a$population),
             from = a$data, subjects = subj,
             where = tfl_r(.tpl_and(sprintf("PARAMCD == \"%s\"", a$param), a$where))),
        list(id = set, from = rec, code = tfl_r(code),
             label = sprintf("%s of %s, each subject of the set a row", a$param, a$population))),
      analyses = c(
        list(list(id = a$param, method = "categorical", data = set, by = a$by,
                  variables = a$category, denominator = subj),
             list(id = "ORR", method = "proportion_ci", data = set, by = a$by,
                  variables = "RESP", args = tfl_r(ci_args))),
        # the header N: the arm's subjects in the analysis set, counted apart
        # (not left to the best response and the rate agreeing on it)
        if (!identical(hn, "none")) list(list(
          id = "GROUPN", method = "categorical", data = subj, variables = a$by,
          purpose = "PRIMARY OUTCOME MEASURE"))))
    # (the arms' order: an order given, else the code list's -- the program
    # puts it on the data, and on the survfit / CI analysis too, relevel_ard();
    # the report's own list for the arms wins)
    own <- .tpl_by_codelist(a, ctx)
    levels_item <- tfl_item("plan_levels", list(variable = c(a$category, "RESP")))
    labels_item <- tfl_item("plan_labels", list(variable = stats::setNames(
      list("Best overall response, n (%)", sprintf("Objective response rate (%s)", resp)),
      c(a$category, "RESP"))))
    if (identical(.tpl_choice(a, "stat_layout", c("rows", "cols"), ctx$template$name,
                              value = a$stat_layout %||% "rows"), "cols")) {
      # each arm's two columns, the rate and its CI (stat_block); the best
      # overall response's n (%) in the first, by its own (categorical) cell
      blk <- Filter(Negate(is.null), .tpl_stat_block_body(list(
        by = a$by, variable = "RESP", label = a$heading,
        cells = stats::setNames(list("{n} ({estimate:.1f%})", "[{conf.low:.1f%}, {conf.high:.1f%}]"),
                                c("n (%)", ci_lab))), ctx, others = "n (%)", header = FALSE, levels = TRUE))
      fn <- vapply(blk, `[[`, "", "fn")
      blk[[match("plan_cells", fn)]]$args$categorical <- "{n} ({p:.1f%})"
      # (the rate's row: its block's heading says it, its level is no label;
      # the arm's N over its two columns, rtfreporter#709)
      content <- c(blk, list(levels_item, labels_item,
        tfl_item("plan_labels", list(RESP = c("TRUE" = ""))),
        tfl_item("plan_col_header", list(header = list(
          list(line = "1", cols = ".values", span = a$by, text = if (identical(hn, "none")) "{col1}" else "{col1} (N={n1})"),
          list(line = "2", cols = "row_label", text = .tpl_lines(a$heading)),
          list(line = "2", cols = ".values", span = "each", text = "{col}"))))))
      return(list(ard = ard, data = list(source = "ard"), content = content, codelists = own))
    }
    content <- list(
      tfl_item("table_plan", list(cols = a$by, rows = list(group = "variable"))),
      tfl_item("plan_cells", list(proportion_ci = stats::setNames(list(
        "{n} ({estimate:.1f%})", "[{conf.low:.1f%}, {conf.high:.1f%}]"), c("n (%)", ci_lab)))),
      # (the categories' order is the ARD's: its code list, applied in the ARD program)
      levels_item, labels_item,
      tfl_item("plan_col_header", list(header = c(list(
        list(line = "1", cols = ".values", span = "each", text = "{col}"),
        list(line = "2", cols = "row_label", text = .tpl_lines(a$heading))),
        if (!identical(hn, "none")) list(
          list(line = "2", cols = ".values", span = "each", text = "(N={n})"))))))
    list(ard = ard, data = list(source = "ard"), content = content, codelists = own)
  })

#' Objective response: a table plan from the template `orr`
#'
#' The best overall response (n (%)) and the objective response rate with
#' its 95% CI, by arm.  Every subject of the analysis set is in N, and the
#' rate's denominator: one with no record of the parameter, or a blank
#' response, is no responder (and in no response category).
#'
#' @inheritParams tpl_demog
#' @param ... The template's other parameters: `by`, `by_levels`, `data`,
#'   `param`, `category`, `responders`, `ci_method`, `conf_level` (0.95,
#'   the CI's level and its row's label), `stat_layout`, `heading`.
#'   `stat_layout = "rows"` (the default) puts the rate and its CI under
#'   each other in the arm's column; `"cols"` gives each arm two columns,
#'   the rate and the CI (the layout [tpl_stat_block()]).
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

# (made when the package loads, after R/tpl_registry_demog.R, as param_change's)
.tpl_ae_overview_params <- function() c(
  list(
    tfl_param("population", required = TRUE,
              doc = "the analysis set's id: the report's `population:` field"),
    .tpl_subset_param(),
    tfl_param("by", required = TRUE, study = FALSE, unset = "stop",
              doc = "the column variable (the arms); not a default and not the study's treatment"),
    tfl_param("data", "ADAE", unset = "stop", doc = "the dataset of events"),
    tfl_param("where", "TRTEMFL == \"Y\"", unset = "step_off",
              doc = "the events kept (R): the treatment-emergent; unset: every event"),
    tfl_param("data_id", NULL, doc = "the events' analysis data's id; NULL: <data>_<population>"),
    tfl_param("flags", .tpl_ae_flags, unset = "stop", doc = paste(
      "the rows, in order: each `id` (the flag's name), `label` (the row's text)",
      "and `where` (R: the events it counts; none: every event kept)")),
    tfl_param("heading", "Subjects with", doc = "the label column's heading; NULL: none"),
    tfl_param("denominator", NULL, doc = .tpl_den_doc),
    tfl_param("header_n", "set", type = "choice", choices = c("set", "denominator", "none"),
              unset = "step_off", doc = paste(
      "the arm headings' N: \"set\" (default): the arm's subjects in the analysis set (with",
      "`subset`); \"denominator\": the arm's rows of the data the percentages are of",
      "(`denominator`), the same as \"set\" when that is NULL, not with a number; \"none\": no N;",
      "unset: no N. \"tested\" is not supported: use \"denominator\" with denominator = \"data\"")),
    .tpl_split_param("group_safe")),
  .tpl_look_params(c("stub_name", "stub_before", "blanks", "blanks_first", "blanks_last",
                     "blanks_counted", "border_header", "widths", "max_rows")))

# (`extends`: its family's `[defaults]`, canon 1.27: the study's defaults are
# laid under the template's items, and its references read them)
.tpl_ae_overview_register <- function() .tfl_template("ae_overview", kind = "table", layout = "summary",
  params = .tpl_ae_overview_params(), extends = "defaults",
  body = function(a, ctx) {
    tn <- ctx$template$name %||% "ae_overview"
    pop <- a$population
    subj <- paste0("adsl_", tolower(pop))
    set <- a$data_id %||% paste0(tolower(a$data), "_", tolower(pop))
    if (!length(a$flags)) {
      .ard_stop(sprintf("%s: `flags` is the rows: give at least one (id, label, where).", tn))
    }
    dn <- .tpl_den(a, ctx, subj)
    # (`!unset`: no N, the step is not made)
    hn <- if ("header_n" %in% ctx$unset) "none" else .tpl_header_n(a, ctx, c("set", "denominator", "none"), paste0(
      "is not supported: use header_n = \"set\", \"denominator\" (the data the percentages ",
      "are of; \"data\" for the subjects with an event kept) or \"none\"."))
    if (identical(hn, "denominator") && is.numeric(dn$value)) {
      .ard_stop(paste0("ae_overview: header_n = \"denominator\" with a denominator that is a number ",
                       "has no data to count the arms' N from: use header_n = \"set\" or \"none\", ",
                       "or give the denominator as an analysis data."))
    }
    # the percentages' denominator, passed explicitly (canon 1.38): the set's
    # subjects (by default), a number, numbers by arm, an analysis data, or
    # the subjects with an event kept
    den <- dn$value %||% subj
    extra <- dn$extra
    rec <- NULL
    if (identical(den, "data")) {
      rec <- .tpl_den_records(set, a$by)
      den <- rec$id
    }
    ids <- vapply(a$flags, `[[`, "", "id")
    # one `subjects` analysis a row: has the subject an event of the data
    # that the row's `where` keeps?
    flag <- function(f) Filter(Negate(is.null), list(
      id = f$id, label = f$label, method = "subjects", data = set,
      where = if (!is.null(f$where)) tfl_r(f$where), by = a$by, variables = f$id,
      denominator = den, purpose = "PRIMARY OUTCOME MEASURE"))
    ard <- list(
      analysis_data = c(
        list(list(id = subj, label = .tpl_set_label(pop, ctx$population), from = "ADSL",
                  population = pop)),
        extra,
        list(Filter(Negate(is.null), list(id = set, from = a$data, subjects = subj,
                                          where = if (!is.null(a$where)) tfl_r(a$where)))),
        if (!is.null(rec)) list(rec)),
      analyses = c(
        # (the header N: the set's subjects, or the data the percentages are of)
        if (!identical(hn, "none")) list(list(
          id = "GROUPN", method = "categorical",
          data = if (identical(hn, "denominator")) den else subj,
          variables = a$by, purpose = "PRIMARY OUTCOME MEASURE")),
        lapply(a$flags, flag)))
    arms <- .tpl_levels(a$by, ctx$codelists)[[a$by]]
    lk <- .tpl_look_items(a, tn, ncol = if (length(arms)) length(arms) else NA_integer_)
    content <- c(list(
      # one row a flag: no label column (the flag's TRUE is the row)
      tfl_item("table_plan", list(cols = a$by, rows = list(group = "variable"), label = NA)),
      # (no plan_levels(): the rows are the flags in their analyses' order, the
      # columns the code list's, applied when the ARD is made)
      tfl_item("plan_labels", list(variable = stats::setNames(lapply(a$flags, `[[`, "label"), ids))),
      # (no heading: the stub's cell on line 2 is empty)
      tfl_item("plan_col_header", list(header = c(list(
        list(line = "1", cols = "group"),
        list(line = "1", cols = ".values", span = "each", text = "{col}"),
        Filter(Negate(is.null), list(line = "2", cols = "group", text = .tpl_lines(a$heading)))),
        if (!identical(hn, "none")) list(
          list(line = "2", cols = ".values", span = "each", text = "(N={n})")))))),
      # the look: the template's defaults, the study's where they refer to it
      unname(Filter(Negate(is.null), lk[.tpl_look_order(tn)])))
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
#' @param ... The template's other parameters: `by` (required, no
#'   default), then `data`, `where`, `data_id`, `flags` (the rows: a list,
#'   each `id`, `label` and `where`), `heading`, `denominator`, `header_n`,
#'   and the look (`stub_name`, `blanks`, `border_header`, `widths`,
#'   `max_rows` ...).
#' @return A [rtfreporter::table_plan()].
#' @examples
#' tfl_template_expand("ae_overview", list(population = "SAF", by = "TRT01A"))$ard$analyses[[3]]
#' @export
tpl_ae_overview <- function(ard, population, ..., codelists = NULL) {
  .tpl_object("ae_overview", ard, c(list(population = population), list(...)),
              codelists = codelists)
}

# ---- the templates in files of their own (R/tpl_<name>.R, read before this one) ----

# A file's own templates (R/tpl_fig.R ...): its parameters' types by name
# and examples, added to the shared tables.  Called from a
# .tpl_*_register() below, while the package loads.
.tpl_extend <- function(param_kinds = NULL, examples = NULL) {
  if (length(param_kinds)) .tpl_param_kinds[names(param_kinds)] <<- param_kinds
  if (length(examples)) .tpl_examples[names(examples)] <<- examples
  invisible(NULL)
}

.tpl_la_register()
.tpl_eair_register()
.tpl_ancova_register()
.tpl_pk_register()
.tpl_fig_register()
