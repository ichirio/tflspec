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
  list(data = list(source = "code", steps = c(steps, f$stats)), content = f$content,
       plot = f$plot, call_steps = length(f$stats))
}

# The figure a tpl_<name>() call draws: the design's own steps and layers
# on the data the program made, the settings `plot` over the template's
.tpl_fig_draw <- function(name, f, data, plot = NULL) {
  if (!is.data.frame(data)) {
    .ard_stop(sprintf("tpl_%s(): `data` is the figure's data, a data frame.", name))
  }
  o <- tfl_output(paste0("tpl-", name), "figure", extends = "none",
                  data = list(source = "code", steps = f$stats), content = f$content)
  o$plot <- .merge_map(f$plot, plot)
  tfl_content_run(o, data)
}

# a column given bare or as text (a quosure), NULL when not given
.tpl_col <- function(q) if (rlang::quo_is_null(q)) NULL else rlang::as_name(q)

# ---- km_simple: Kaplan-Meier curves ----------------------------------------------

.tpl_km_design <- function(group = NULL, time = NULL, censor = "CNSR", time_unit = "months",
                           data = NULL, param = NULL) {
  .tpl_fig_design("km_simple", list(data = data, param = param, group = group, time = time,
                                    censor = censor, time_unit = time_unit), "survfit")
}

.tpl_km_simple_params <- function() list(
  tfl_param("param", required = TRUE, type = "levels", of = "PARAMCD",
            doc = "the PARAMCD of ADTTE"),
  tfl_param("population", NULL, type = "text",
            doc = "the analysis set's id (SAF): its subjects kept; NULL: every record"),
  tfl_param("group", NULL, doc = "the curves' variable (TRT01A)"),
  tfl_param("time", NULL, doc = "the time variable; NULL: AVAL"),
  tfl_param("censor", "CNSR", doc = "the censoring variable"),
  tfl_param("time_unit", "months", doc = "the time axis' unit"),
  tfl_param("data", NULL, doc = "the dataset; NULL: ADTTE")
)

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
  tfl_param("data", "ADLB", type = "text", doc = "the BDS dataset (ADVS, ADLB)"),
  tfl_param("value", "AVAL", type = "vars", doc = "the variable averaged (AVAL, CHG)"),
  tfl_param("group", "TRT01A", type = "vars", doc = "the lines' variable"),
  tfl_param("visit_var", "AVISIT", type = "vars", doc = "the visits on the x axis"),
  tfl_param("visit_order", "AVISITN", type = "vars", doc = "the visits' order")
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

# ---- registered --------------------------------------------------------------------

.tpl_fig_register <- function() {
  .tfl_template("km_simple", kind = "figure", layout = "km", params = .tpl_km_simple_params(),
    body = function(a, ctx) {
      f <- .tpl_km_design(a$group, a$time, a$censor, a$time_unit, data = a$data, param = a$param)
      .tpl_fig_body_of(f, a, ctx)
    })
  .tfl_template("mean_se", kind = "figure", layout = "mean", params = .tpl_mean_params(),
                body = .tpl_mean_body("se"), label = "Mean +/- SE by visit")
  .tfl_template("mean_ci", kind = "figure", layout = "mean", params = .tpl_mean_params(),
                body = .tpl_mean_body("ci"), label = "Mean (95% CI) by visit")
  .tfl_template("waterfall", kind = "figure", layout = "waterfall", params = .tpl_wf_params(),
    body = function(a, ctx) {
      f <- .tpl_wf_design(a$value, a$by_response, a$param, a$data, a$where)
      .tpl_fig_body_of(f, a, ctx)
    }, label = "Waterfall of each subject's change")
}
