# ---- figure templates: tfl_fig_template()'s presets as template SPECs --------------
# (registered at the end of tpl_registry.R: R reads this file before it)
#
# A figure template expands to the design form, as a designed report has
# it: its data steps, the layers as the content's items, the figure's
# settings (`plot:`).  The analysis set's subjects are a semi join after
# the read (data.yml's definition), in place of the preset's flag.  The
# data's last steps -- the statistics, the fit, the bars' order -- are its
# call's (`call_steps`): the program (A) writes the steps before them as
# plain dplyr, then `content <- tpl_<name>(<data>, ...)` and the report's
# own items `+`-ed after it.

# A preset's design (tfl_fig_template(), `args`), split where the call
# takes over: from the first step of a kind in `call_kinds` to the end
.tpl_fig_design <- function(preset, args, call_kinds) {
  f <- do.call(tfl_fig_template, c(list(preset), args))
  kinds <- vapply(f$data, function(s) s$step %||% "", "")
  at <- match(TRUE, kinds %in% call_kinds, nomatch = length(kinds) + 1L)
  call <- seq_along(kinds) >= at
  list(steps = f$data[!call], stats = f$data[call], plot = f$plot,
       content = lapply(f$layers, function(l) tfl_item(l$layer, l[setdiff(names(l), "layer")])))
}

# The template's body from its design: the analysis set's subjects in
# place of the preset's flag; `call_steps` the steps the call makes
.tpl_fig_body_of <- function(f, a, ctx) {
  steps <- Filter(function(s) !identical(s$step, "flag"), f$steps)
  if (!is.null(a$population)) {
    at <- match("read", vapply(steps, `[[`, "", "step"))
    steps <- append(steps, .tpl_population_step(a$population, ctx$population), at)
  }
  # the records kept besides the parameter (`where`): after it
  if (!is.null(a$where)) {
    kinds <- vapply(steps, `[[`, "", "step")
    at <- match("param", kinds, nomatch = match("read", kinds, nomatch = 0L))
    steps <- append(steps, list(list(step = "filter", expr = a$where)), at)
  }
  list(data = list(source = "code", steps = c(steps, f$stats)), content = f$content,
       plot = f$plot, call_steps = length(f$stats))
}

# The figure a tpl_<name>() call draws: the design's own steps and layers
# on the data the program made, the settings `plot` over the template's
.tpl_fig_draw <- function(name, f, data, plot = NULL, as = "df") {
  if (!is.data.frame(data)) {
    .ard_stop(sprintf("tpl_%s(): `%s` is the figure's %s, a data frame.", name,
                      if (as == "ard") "ard" else "data", if (as == "ard") "ARD" else "data"))
  }
  o <- tfl_output(paste0("tpl-", name), "figure", extends = "none",
                  data = list(source = "code", steps = f$stats), content = f$content)
  o$plot <- .merge_map(f$plot, plot)
  tfl_content_run(o, stats::setNames(list(data), as))
}

# a column given bare or as text (a quosure), NULL when not given
.tpl_col <- function(q) if (rlang::quo_is_null(q)) NULL else rlang::as_name(q)

# ---- km_simple ...: the km layout's former presets (R/tpl_fig_km.R) ----------

#' Kaplan-Meier curves: a figure from the template `km_simple`
#'
#' The A use of a figure's template SPEC: the program makes the figure's
#' data with its own steps (the dataset read, the analysis set's subjects
#' kept, the parameter, the time's unit), then one call draws it.  The
#' survival fit is made here, from the data (`ggsurvfit::survfit2()`): the
#' curves, the censor marks, the treatment palette, the axes and the
#' study's figure theme.  A report's own layers and settings go after the
#' call with `+` (`+ geom_hline(...) + labs(...)`).
#'
#' @param data The figure's data: one record a subject (the parameter's,
#'   the analysis set's subjects), its time in `time_unit`.
#' @param group The curves' variable (`TRT01A`), bare or as text; `NULL`:
#'   one curve.
#' @param time The time variable; `NULL`: `AVAL`.
#' @param censor The censoring variable (1: censored).
#' @param time_unit The time's unit, for the x axis' title.
#' @param plot The figure's settings over the template's (`x_label`,
#'   `y_by`, `legend` ..., as a designed figure's `plot:`).
#' @return A ggplot.
#' @seealso [tfl_template_expand()] for the design it draws (the B use).
#' @examples
#' if (requireNamespace("ggsurvfit", quietly = TRUE)) {
#'   saf <- adsl$USUBJID[adsl$SAFFL == "Y"]
#'   df <- adtte[adtte$PARAMCD == "PFS" & adtte$USUBJID %in% saf, ]
#'   p <- tpl_km_simple(df, group = TRT01A)
#'   class(p)
#' }
#' @export
tpl_km_simple <- function(data, group = NULL, time = NULL, censor = "CNSR",
                          time_unit = "months", plot = NULL) {
  f <- .tpl_km_design(.tpl_col(rlang::enquo(group)), .tpl_col(rlang::enquo(time)),
                      .tpl_col(rlang::enquo(censor)), time_unit)
  .tpl_fig_draw("km_simple", f, data, plot)
}

#' Kaplan-Meier curves and the number at risk: `km_risk_table`, `km_ci`
#'
#' The A use of the templates `km_risk_table` (the curves, the censor
#' marks, the median's line and the number at risk under them) and
#' `km_ci` (the same with the curves' confidence bands), as
#' [tpl_km_simple()]: the program makes the data, one call draws it.  The
#' number at risk is ggsurvfit's own table (`ggsurvfit::add_risktable()`),
#' a layer of the plot: a report's own items `+`-ed after the call go on
#' the curves.
#'
#' @inheritParams tpl_km_simple
#' @return A ggplot (a ggsurvfit, its risk table built when it is drawn).
#' @seealso [tfl_template_expand()] for the design it draws (the B use).
#' @examples
#' if (requireNamespace("ggsurvfit", quietly = TRUE)) {
#'   df <- adtte[adtte$PARAMCD == "PFS", ]
#'   p <- tpl_km_ci(df, group = TRT01A)
#'   class(p)
#' }
#' @export
tpl_km_risk_table <- function(data, group = NULL, time = NULL, censor = "CNSR",
                              time_unit = "months", plot = NULL) {
  f <- .tpl_km_design(.tpl_col(rlang::enquo(group)), .tpl_col(rlang::enquo(time)),
                      .tpl_col(rlang::enquo(censor)), time_unit, preset = "km_risk_table")
  .tpl_fig_draw("km_risk_table", f, data, plot)
}

#' @rdname tpl_km_risk_table
#' @export
tpl_km_ci <- function(data, group = NULL, time = NULL, censor = "CNSR",
                      time_unit = "months", plot = NULL) {
  f <- .tpl_km_design(.tpl_col(rlang::enquo(group)), .tpl_col(rlang::enquo(time)),
                      .tpl_col(rlang::enquo(censor)), time_unit, preset = "km_ci")
  .tpl_fig_draw("km_ci", f, data, plot)
}

# ---- mean_se / mean_ci: a mean over the visits, with SE or CI bars ------------------

.tpl_mean_design <- function(interval, group = "TRT01A", value = "AVAL", visit_var = "AVISIT",
                             visit_order = "AVISITN", param = NULL, data = NULL) {
  f <- .tpl_fig_design(paste0("mean_", interval),
                       list(data = data, param = param, group = group, value = value,
                            visit = visit_order, visit_label = visit_var, join_adsl = FALSE), "summary")
  # (no parameter named: the y axis' title says the statistic alone)
  if (is.null(param)) f$plot$y_label <- sub(" ALT$", "", f$plot$y_label)
  f
}

.tpl_mean_params <- function() list(
  tfl_param("param", required = TRUE, type = "levels", of = "PARAMCD",
            doc = "the PARAMCD (SYSBP)"),
  tfl_param("population", NULL, type = "text",
            doc = "the analysis set's id (SAF): its subjects kept; NULL: every record"),
  .tpl_subset_param(),
  tfl_param("data", "ADLB", type = "text", doc = "the BDS dataset (ADVS, ADLB)"),
  tfl_param("value", "AVAL", type = "vars", doc = "the variable averaged (AVAL, CHG)"),
  tfl_param("group", "TRT01A", type = "vars", doc = "the lines' variable"),
  tfl_param("visit_var", "AVISIT", type = "vars", doc = "the visits on the x axis"),
  tfl_param("visit_order", "AVISITN", type = "vars", doc = "the visits' order"),
  .tpl_where_param(doc = "the records kept besides the parameter (R)")
)

.tpl_mean_body <- function(interval) {
  force(interval)
  function(a, ctx) {
    f <- .tpl_mean_design(interval, a$group, a$value, a$visit_var, a$visit_order, a$param, a$data)
    .tpl_fig_body_of(f, a, ctx)
  }
}

#' Mean by visit: a figure from the template `mean_se` or `mean_ci`
#'
#' The A use of a figure's template SPEC: the program makes the records
#' (the dataset read, the analysis set's subjects kept, the parameter, the
#' records with a value and a visit, the visits in their order), then one
#' call draws the mean of each group at each visit, a line and points,
#' with bars of +/- one standard error (`mean_se`) or the 95% confidence
#' interval (`mean_ci`).  The means are made here, from the records.  A
#' report's own layers and settings go after the call with `+`.
#'
#' @param data The records: one a subject and visit, the parameter's.
#' @param group The lines' variable (`TRT01A`), bare or as text.
#' @param value The variable averaged (`AVAL`, `CHG`).
#' @param visit_var The visits on the x axis (`AVISIT`), a factor in their order.
#' @param visit_order The visits' order (`AVISITN`).
#' @param param The parameter, for the y axis' title; `NULL`: none.
#' @inheritParams tpl_km_simple
#' @return A ggplot.
#' @seealso [tfl_template_expand()] for the design it draws (the B use).
#' @examples
#' saf <- adsl$USUBJID[adsl$SAFFL == "Y"]
#' df <- advs[advs$PARAMCD == "SYSBP" & advs$USUBJID %in% saf, ]
#' df$AVISIT <- stats::reorder(factor(df$AVISIT), df$AVISITN)
#' p <- tpl_mean_se(df, group = TRT01A, param = "SYSBP")
#' class(p)
#' @export
tpl_mean_se <- function(data, group = "TRT01A", value = "AVAL", visit_var = "AVISIT",
                        visit_order = "AVISITN", param = NULL, plot = NULL) {
  f <- .tpl_mean_design("se", .tpl_col(rlang::enquo(group)), .tpl_col(rlang::enquo(value)),
                        .tpl_col(rlang::enquo(visit_var)), .tpl_col(rlang::enquo(visit_order)), param)
  .tpl_fig_draw("mean_se", f, data, plot)
}

#' @rdname tpl_mean_se
#' @export
tpl_mean_ci <- function(data, group = "TRT01A", value = "AVAL", visit_var = "AVISIT",
                        visit_order = "AVISITN", param = NULL, plot = NULL) {
  f <- .tpl_mean_design("ci", .tpl_col(rlang::enquo(group)), .tpl_col(rlang::enquo(value)),
                        .tpl_col(rlang::enquo(visit_var)), .tpl_col(rlang::enquo(visit_order)), param)
  .tpl_fig_draw("mean_ci", f, data, plot)
}

# ---- waterfall: each subject's change, one bar, in its order -----------------------

.tpl_wf_design <- function(value = "PCHG", by_response = FALSE, param = NULL, data = NULL,
                           where = NULL) {
  f <- .tpl_fig_design(if (isTRUE(by_response)) "waterfall_response" else "waterfall_plain",
                       list(data = data, param = param, value = value), "rank")
  # the records kept besides the parameter: one a subject (the best change)
  if (!is.null(where)) {
    at <- match("param", vapply(f$steps, `[[`, "", "step"))
    f$steps <- append(f$steps, list(list(step = "filter", expr = where)), at)
  }
  f
}

.tpl_wf_params <- function() list(
  tfl_param("param", required = TRUE, type = "levels", of = "PARAMCD",
            doc = "the PARAMCD of the tumour measurements (SUMDIAM)"),
  tfl_param("population", NULL, type = "text",
            doc = "the analysis set's id (FAS): its subjects kept; NULL: every record"),
  .tpl_subset_param(),
  tfl_param("data", "ADTR", type = "text", doc = "the dataset"),
  tfl_param("value", "PCHG", type = "vars", doc = "the bars' value (the % change)"),
  tfl_param("where", NULL, type = "text",
            doc = "the records kept besides the parameter (R): one a subject, its best change"),
  tfl_param("by_response", FALSE, type = "flag",
            doc = "TRUE: the bars coloured by the best overall response (ADRS, BOR)")
)

#' Waterfall: a figure from the template `waterfall`
#'
#' The A use of a figure's template SPEC: the program makes the records
#' (the dataset read, the analysis set's subjects kept, the parameter, the
#' record of each subject the report keeps (`where`), the best overall
#' response joined when the bars are coloured by it), then one call draws
#' them: one bar a subject, the largest change first, the lines at 0, +20%
#' and -30%.  The bars' order is made here.  A report's own layers and
#' settings go after the call with `+`.
#'
#' @param data The records: one a subject.
#' @param value The bars' value (`PCHG`), bare or as text.
#' @param by_response `TRUE`: the bars coloured by `BOR` (the best overall
#'   response, a column of `data`).
#' @inheritParams tpl_km_simple
#' @return A ggplot.
#' @seealso [tfl_template_expand()] for the design it draws (the B use).
#' @examples
#' df <- adtr[adtr$AVISIT == "WEEK 12", ]
#' p <- tpl_waterfall(df, value = PCHG)
#' class(p)
#' @export
tpl_waterfall <- function(data, value = "PCHG", by_response = FALSE, plot = NULL) {
  f <- .tpl_wf_design(.tpl_col(rlang::enquo(value)), by_response)
  .tpl_fig_draw("waterfall", f, data, plot)
}

# ---- forest_hr: the hazard ratio overall and by subgroup, from the ARD -------------

# The figure templates whose program is not `content <- tpl_<name>(df, ...)`:
# `input` the object the call takes; `assemble` the function the program
# calls after the report's own items, on `fig` (the panels side by side)
.tpl_fig_forms <- list(forest_hr = list(input = "ard", assemble = "tpl_forest_layout"))

# the text column beside the forest (the preset's), its layer
.tpl_forest_text <- function() {
  list(layer = "text_column", data = "est", y = "y",
       columns = "N = n | Hazard ratio (95% CI) = txt", width = 0.4)
}

# columns given bare or as text, one or c(...) of them (a quosure)
.tpl_cols <- function(q) {
  if (rlang::quo_is_null(q)) return(NULL)
  e <- rlang::quo_get_expr(q)
  if (is.character(e)) return(e)
  if (is.call(e) && identical(e[[1L]], as.name("c"))) {
    return(vapply(as.list(e)[-1L], function(x) if (is.character(x)) x else as.character(x), ""))
  }
  if (is.name(e)) return(as.character(e))
  as.character(rlang::eval_tidy(q))
}

.tpl_forest_design <- function(group = "TRT01A", subgroups = c("SEX", "AGEGR1"),
                               comparison = NULL) {
  d <- tfl_fig_template("forest_hr", group = group,
                        subgroups = paste(subgroups, collapse = ", "), comparison = comparison)
  # the ARD read and the rows made: all the call's (no data steps)
  keep <- vapply(d$data, function(st) st$step %in% c("ard_stats", "code"), NA)
  layers <- Filter(function(l) !identical(l$layer, "text_column"), d$layers)
  list(steps = list(), stats = d$data[keep], plot = d$plot,
       content = lapply(c(layers, list(.tpl_forest_text())),
                        function(l) tfl_item(l$layer, l[setdiff(names(l), "layer")])))
}

.tpl_forest_params <- function() list(
  tfl_param("param", required = TRUE, type = "levels", of = "PARAMCD",
            doc = "the PARAMCD of the time to event (PFS)"),
  tfl_param("population", required = TRUE, type = "text", doc = "the analysis set's id (SAF)"),
  .tpl_subset_param(),
  tfl_param("data", "ADTTE", type = "text", doc = "the dataset"),
  tfl_param("group", "TRT01A", type = "vars", doc = "the arms compared (the Cox model's term)"),
  tfl_param("subgroups", c("SEX", "AGEGR1"), type = "vars", multiple = TRUE,
            doc = "the subgroups, each its levels' hazard ratios"),
  tfl_param("comparison", NULL, type = "levels", of = "group",
            doc = "the arm compared with the reference; NULL: every other arm"),
  tfl_param("time", "AVAL", type = "vars", doc = "the time variable"),
  tfl_param("censor", "CNSR", type = "vars", doc = "the censoring variable"),
  .tpl_where_param(doc = "the records kept besides the parameter (R)")
)

.tpl_forest_body <- function(a, ctx) {
  set <- paste(tolower(a$data), tolower(a$population), tolower(a$param), sep = "_")
  an <- tfl_fig_forest_analyses(data = a$data, param = a$param, pop = a$population,
                                group = a$group, subgroups = a$subgroups, time = a$time,
                                censor = a$censor)$analyses
  ard <- list(
    analysis_data = list(Filter(Negate(is.null), list(
      id = set, label = sprintf("%s of %s, with the subgroups", a$param, a$population),
      from = a$data, population = a$population,
      where = tfl_r(.tpl_and(sprintf("PARAMCD == %s", q(a$param)), a$where)),
      add = if (length(a$subgroups)) as.list(a$subgroups)))),
    analyses = lapply(seq_len(nrow(an)), function(i) list(
      id = an$analysis_id[i], label = an$label[i], method = "custom", data = set,
      code = tfl_r(an$code[i]), purpose = "SECONDARY OUTCOME MEASURE")))
  f <- .tpl_forest_design(a$group, a$subgroups, a$comparison)
  c(list(ard = ard), .tpl_fig_body_of(f, list(), ctx))
}

#' Hazard ratio by subgroup: a forest plot from the template `forest_hr`
#'
#' The A use of the template SPEC `forest_hr`: the hazard ratios are the
#' report's ARD (its own analyses: a Cox model overall and within each
#' subgroup, made by the ARD program), and one call draws the forest from
#' it: `tpl_forest_hr()` the estimates and their 95% CIs, one row each,
#' the subgroups' levels under their heading.  The report's own items go
#' on it with `+`; `tpl_forest_layout()` then puts the text column (N, the
#' hazard ratio and its CI) at its right.
#'
#' @param ard The study ARD's rows of this report.
#' @param group The arms compared (`TRT01A`), bare or as text.
#' @param subgroups The subgroups (`c(SEX, AGEGR1)`), bare or as text.
#' @param comparison The arm compared with the reference; `NULL`: every
#'   other arm, each row saying which.
#' @param fig `tpl_forest_layout()`: the forest, with the report's own
#'   items.
#' @inheritParams tpl_km_simple
#' @return `tpl_forest_hr()`: a ggplot; `tpl_forest_layout()`: a patchwork
#'   of it and its text column.
#' @seealso [tfl_template_expand()] for its ARD and design (the B use).
#' @export
tpl_forest_hr <- function(ard, group = "TRT01A", subgroups = c("SEX", "AGEGR1"),
                          comparison = NULL, plot = NULL) {
  f <- .tpl_forest_design(.tpl_col(rlang::enquo(group)), .tpl_cols(rlang::enquo(subgroups)),
                          comparison)
  f$content <- Filter(function(it) !identical(it$fn, "text_column"), f$content)
  .tpl_fig_draw("forest_hr", f, ard, plot, as = "ard")
}

#' @rdname tpl_forest_hr
#' @export
tpl_forest_layout <- function(fig) {
  # the rows the forest draws: its layers' data
  est <- NULL
  for (l in fig$layers) {
    if (is.data.frame(l$data) && all(c("y", "txt") %in% names(l$data))) {
      est <- l$data
      break
    }
  }
  if (is.null(est)) .ard_stop("tpl_forest_layout(): `fig` is a forest of tpl_forest_hr().")
  l <- .tpl_forest_text()
  base <- tfl_fig_template("forest_hr")$plot$base_size
  x <- .fig_layer_code(l, 1L, list(base_size = base))
  w <- as.numeric(l$width)
  code <- c(x$chain, sprintf("fig <- fig + %s + plot_layout(widths = %s)", x$panel$name,
                             vec_code(round(c(1 - w, w), 3))))
  env <- new.env(parent = .fig_pkg_env(c("ggplot2", "patchwork")))
  env$est <- est
  env$fig <- fig
  eval(parse(text = code), env)
  env$fig
}

# ---- registered --------------------------------------------------------------------

.tpl_fig_register <- function() {
  .tfl_template("forest_hr", kind = "figure", layout = "forest", params = .tpl_fig_data_stage(.tpl_forest_params(),
                  c("param", "population", "subset", "data", "time", "censor", "where")),
                body = .tpl_forest_body, label = "Hazard ratio by subgroup (forest)", call = "tfl_tpl")
  .tpl_km_register()
  .tfl_template("mean_se", kind = "figure", layout = "mean", params = .tpl_fig_data_stage(.tpl_mean_params()),
                body = .tpl_mean_body("se"), label = "Mean +/- SE by visit", call = "tfl_tpl")
  .tfl_template("mean_ci", kind = "figure", layout = "mean", params = .tpl_fig_data_stage(.tpl_mean_params()),
                body = .tpl_mean_body("ci"), label = "Mean (95% CI) by visit", call = "tfl_tpl")
  .tfl_template("waterfall", kind = "figure", layout = "waterfall", params = .tpl_fig_data_stage(.tpl_wf_params(),
    c("param", "population", "subset", "data", "where")),
    body = function(a, ctx) {
      f <- .tpl_wf_design(a$value, a$by_response, a$param, a$data, a$where)
      .tpl_fig_body_of(f, a[setdiff(names(a), "where")], ctx)
    }, label = "Waterfall of each subject's change", call = "tfl_tpl")
  # the study's figure style: the km parameters' study values
  .tpl_study_sources <<- c(.tpl_study_sources, .tpl_km_style_sources())
}
