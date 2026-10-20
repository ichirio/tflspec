# ---- ancova: the change from baseline at one visit, an ANCOVA ----------------------
# (registered at the end of tpl_registry.R: R reads this file before it)

.tpl_ancova_params <- function() list(
  tfl_param("population", required = TRUE, doc = "the analysis set's id (SAF)"),
  tfl_param("data", required = TRUE, doc = "the BDS dataset (ADVS, ADLB)"),
  tfl_param("param", required = TRUE, type = "levels", of = "PARAMCD",
            doc = "the PARAMCD (SYSBP)"),
  tfl_param("visit", required = TRUE, doc = "the AVISIT (CYCLE 9 DAY 1)"),
  tfl_param("by", "TRT01A", doc = "the arm variable (the columns)"),
  tfl_param("response", "CHG", doc = "the response variable"),
  tfl_param("covariates", "BASE", doc = "the model's covariates besides the arm"),
  tfl_param("where", NULL, doc = "the records kept besides the parameter and visit (R)"),
  tfl_param("ref", NULL, doc = "the reference arm; NULL: the last arm in the code list's order")
)

.tpl_ancova_body <- function(a, ctx) {
  pop <- a$population
  subj <- paste0("adsl_", tolower(pop))
  set <- paste0(tolower(a$data), "_", tolower(pop), "_", tolower(a$param))
  where <- c(sprintf("PARAMCD == \"%s\" & AVISIT == \"%s\"", a$param, a$visit), a$where)
  model <- sprintf("formula = %s ~ %s, method = \"lm\"", a$response,
                   paste(c(a$by, a$covariates), collapse = " + "))
  # an arm's LS mean and a difference as the arm's column: the arm (the
  # difference's first) as the group, the statistics' block as the variable
  post <- function(what, level) sprintf(paste(
    "dplyr::mutate(group1_level = %s) |",
    "dplyr::mutate(variable = \"%s\", variable_level = list(NULL))"), level, what)
  an <- function(id, fn, what, level, formats = NULL) Filter(Negate(is.null), list(
    id = id, method = paste0("cardx::ard_emmeans_", fn), data = set, args = tfl_r(model),
    post = post(what, level), formats = formats, purpose = "PRIMARY OUTCOME MEASURE"))
  ard <- list(
    analysis_data = list(
      list(id = subj, label = .tpl_set_label(pop, ctx$population), from = "ADSL",
           population = pop),
      list(id = set, from = a$data, subjects = subj,
           where = tfl_r(paste(where, collapse = " & ")))),
    analyses = list(
      list(id = "GROUPN", method = "categorical", data = subj, variables = a$by,
           purpose = "PRIMARY OUTCOME MEASURE"),
      an("LSMEAN", "emmeans", "LSMEAN", "variable_level"),
      # the differences against the reference arm: put last, each pair is
      # an arm minus it (emmeans' pairwise order)
      list(id = "LSDIFF", method = "custom", data = set, code = paste(c(
        if (is.null(a$ref)) sprintf("ref <- tail(levels(factor(data$%s)), 1)", a$by) else
          sprintf("ref <- \"%s\"", a$ref),
        "message(\"ancova: the reference arm is \", ref)",
        "lsdiff <- data |>",
        "  # the reference arm last: each difference is an arm minus it",
        sprintf("  dplyr::mutate(%s = factor(%s, levels = c(setdiff(levels(factor(%s)), ref), ref))) |>",
                a$by, a$by, a$by),
        sprintf("  cardx::ard_emmeans_contrast(%s)", model),
        "# one difference a row (with three arms or more, cardx gives them in one)",
        "rows <- rep(seq_len(nrow(lsdiff)), lengths(lsdiff$stat))",
        "lsdiff <- dplyr::mutate(lsdiff[rows, ], variable_level = as.list(unlist(lsdiff$variable_level)),",
        "  stat = unlist(lapply(lsdiff$stat, as.list), recursive = FALSE))",
        "dplyr::filter(lsdiff, endsWith(unlist(variable_level), paste(\" -\", ref)))"),
        collapse = "\n"),
        post = post("DIFF",
                    "lapply(variable_level, sub, pattern = \" - .*\", replacement = \"\")"),
        # the p-value as the program's fmt_pvalue() writes it (<0.001)
        formats = "p.value=pvalue", purpose = "PRIMARY OUTCOME MEASURE")))
  content <- list(
    tfl_item("table_plan", list(cols = a$by, rows = list(group = "variable"))),
    tfl_item("plan_cells", list(
      LSMEAN = list(n = "{n}", `LS mean (SE)` = "{estimate:.2f} ({std.error:.2f})",
                    `95% CI` = "({conf.low:.2f}, {conf.high:.2f})"),
      DIFF = list(`Difference (SE)` = "{estimate:.2f} ({std.error:.2f})",
                  `95% CI` = "({conf.low:.2f}, {conf.high:.2f})",
                  `p-value` = "{p.value}"))),
    tfl_item("plan_levels", list(variable = c("LSMEAN", "DIFF"))),
    tfl_item("plan_labels", list(variable = list(
      LSMEAN = sprintf("%s: LS mean", a$response),
      DIFF = sprintf("Difference in LS means (vs %s)", a$ref %||% "the reference arm")))),
    tfl_item("plan_col_header", list(header = list(
      list(line = "1", cols = ".values", span = "each", text = "{col}\n(N={n})")))))
  list(ard = ard, data = list(source = "ard", object = "ard"), content = content)
}

.tpl_ancova_register <- function() {
  .tfl_template("ancova", kind = "table", layout = "summary",
                params = .tpl_ancova_params(), body = .tpl_ancova_body)
}

#' An ANCOVA of the change from baseline: a table plan from the template `ancova`
#'
#' A parameter's change from baseline (`CHG`) at one visit, by an analysis
#' of covariance: `<response> ~ <by> + <covariates>` (`CHG ~ TRT01A +
#' BASE`), a linear model.  Each arm's n and LS mean (SE, 95% CI), and the
#' difference in LS means of each other arm minus the reference arm (SE,
#' 95% CI, p-value) in that arm's column, from cardx's
#' `ard_emmeans_emmeans()` and `ard_emmeans_contrast()` (emmeans).  The
#' reference arm is `ref`, else the last arm in the code list's order; the
#' program says which (a message).  The p-value is
#' written as the program's `fmt_pvalue()` writes it (`<0.001`, else 3
#' decimals).
#'
#' @inheritParams tpl_demog
#' @param ... The template's other parameters: `data`, `param`, `visit`
#'   (required), `by`, `response`, `covariates`, `where`, `ref`.
#' @return A [rtfreporter::table_plan()].
#' @examples
#' x <- tfl_template_expand("ancova", list(population = "SAF", data = "ADVS",
#'                          param = "SYSBP", visit = "CYCLE 2 DAY 1"))
#' x$ard$analyses[[2]]$args
#' @export
tpl_ancova <- function(ard, population, ..., codelists = NULL) {
  .tpl_object("ancova", ard, c(list(population = population), list(...)),
              codelists = codelists)
}
