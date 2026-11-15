# ---- ancova: the change from baseline at one visit, an ANCOVA ----------------------
# (registered at the end of tpl_registry.R: R reads this file before it)

# emmeans' `adjust` values (?emmeans::summary.emmGrid, "P-value adjustments")
.tpl_ancova_adjust <- c("none", "tukey", "dunnettx", "sidak", "bonferroni", "holm", "hommel",
                        "hochberg", "BH", "BY", "fdr", "mvt", "scheffe")

.tpl_ancova_params <- function() list(
  tfl_param("population", required = TRUE, doc = "the analysis set's id (SAF)"),
  .tpl_subset_param(),
  tfl_param("data", required = TRUE, doc = "the BDS dataset (ADVS, ADLB)"),
  tfl_param("param", required = TRUE, type = "levels", of = "PARAMCD",
            doc = "the PARAMCD (SYSBP)"),
  tfl_param("visit", required = TRUE, doc = "the AVISIT (CYCLE 9 DAY 1)"),
  tfl_param("by", "TRT01A", doc = "the arm variable (the columns)"),
  tfl_param("response", "CHG", doc = "the response variable"),
  tfl_param("covariates", "BASE", doc = "the model's covariates besides the arm"),
  tfl_param("where", NULL, doc = "the records kept besides the parameter and visit (R)"),
  tfl_param("ref", NULL, doc = "the reference arm, put first; NULL: the first arm in the code list's order, or in its number order"),
  tfl_param("adjust", "none", type = "choice", choices = .tpl_ancova_adjust,
            doc = "the multiplicity adjustment of the differences' CIs and p-values, as emmeans' adjust (none: no adjustment)")
)

.tpl_ancova_body <- function(a, ctx) {
  pop <- a$population
  .tpl_unset_stop(a, "adjust", "ancova")
  if (!is.character(a$adjust) || length(a$adjust) != 1L || !a$adjust %in% .tpl_ancova_adjust) {
    .ard_stop(sprintf("ancova: `adjust` is %s; it must be one of %s (emmeans' multiplicity adjustments).",
                      paste(format(a$adjust), collapse = ", "),
                      paste(sprintf("\"%s\"", .tpl_ancova_adjust), collapse = ", ")))
  }
  subj <- .tpl_subj_id()
  set <- paste0(tolower(a$data), "_sub")
  where <- c(sprintf("PARAMCD == \"%s\" & AVISIT == \"%s\"", a$param, a$visit), a$where)
  model <- sprintf("formula = %s ~ %s, method = \"lm\"", a$response,
                   paste(c(a$by, a$covariates), collapse = " + "))
  # cardx's label of the model: with covariates, "adjusted"
  method_label <- if (length(a$covariates) > 0L) "Least-squares adjusted mean difference" else
    "Least-squares mean difference"
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
      list(id = subj, from = "ADSL", population = pop),
      list(id = set, from = a$data, subjects = subj,
           where = tfl_r(.tpl_and_all(where)))),
    analyses = list(
      list(id = "GROUPN", method = "categorical", data = subj, variables = a$by,
           purpose = "PRIMARY OUTCOME MEASURE"),
      an("LSMEAN", "emmeans", "LSMEAN", "variable_level"),
      # the differences against the reference arm: put first, each pair is
      # an arm minus it (emmeans' trt.vs.ctrl, or revpairwise for Tukey)
      list(id = "LSDIFF", method = "custom", data = set, code = paste(c(
        if (is.null(a$ref)) sprintf("ref <- levels(factor(data$%s))[1L]", a$by) else
          sprintf("ref <- \"%s\"", a$ref),
        "message(\"ancova: the reference arm is \", ref)",
        "ref_data <- data |>",
        "  # the reference arm first (a relevel): each difference is an arm minus it",
        sprintf("  dplyr::mutate(%s = factor(%s, levels = c(ref, setdiff(levels(factor(%s)), ref))))",
                a$by, a$by, a$by),
        sprintf("fit <- stats::lm(%s ~ %s, data = ref_data)", a$response,
                paste(c(a$by, a$covariates), collapse = " + ")),
        if (identical(a$adjust, "tukey")) c(
          "# each other arm minus the reference arm, Tukey-adjusted.  emmeans changes \"tukey\"",
          "# to \"sidak\" for the contrast against one arm; the pairwise contrast keeps the",
          "# Tukey family of all pairs (\"revpairwise\": a later arm minus an earlier one, so",
          "# \"arm - reference\"), of which the rows against the reference arm are kept",
          sprintf("diffs <- emmeans::emmeans(fit, specs = ~ %s, data = ref_data) |>", a$by),
          "  emmeans::contrast(method = \"revpairwise\", adjust = \"tukey\") |>",
          "  summary(infer = TRUE, level = 0.95)",
          "diffs <- diffs[endsWith(as.character(diffs$contrast), paste(\" -\", ref)), ]") else c(
          "# each other arm minus the reference arm, adjusted as `adjust` says (emmeans'",
          "# own default for these differences is not \"none\", so it is always given)",
          sprintf("diffs <- emmeans::emmeans(fit, specs = ~ %s, data = ref_data) |>", a$by),
          sprintf("  emmeans::contrast(method = \"trt.vs.ctrl\", ref = 1L, adjust = \"%s\") |>",
                  a$adjust),
          "  summary(infer = TRUE, level = 0.95)"),
        "# the rows of cardx::ard_emmeans_contrast(): a statistic's rows, one a difference",
        "stat <- list(estimate = diffs$estimate, std.error = diffs$SE, df = diffs$df,",
        "  conf.low = diffs$lower.CL, conf.high = diffs$upper.CL, p.value = diffs$p.value,",
        sprintf("  conf.level = rep(0.95, nrow(diffs)), method = rep(\"%s\", nrow(diffs)))",
                method_label),
        "labels <- c(estimate = \"Mean Difference\", std.error = \"Standard Error\",",
        "  df = \"Degrees of Freedom\", conf.low = \"CI Lower Bound\", conf.high = \"CI Upper Bound\",",
        "  p.value = \"p-value\", conf.level = \"CI Confidence Level\", method = \"method\")",
        "cards::as_card(dplyr::tibble(",
        sprintf("  group1 = \"%s\", variable = \"contrast\",", a$by),
        "  variable_level = rep(as.list(as.character(diffs$contrast)), length(stat)),",
        "  context = \"emmeans_contrast\", stat_name = rep(names(stat), each = nrow(diffs)),",
        "  stat_label = unname(labels[stat_name]),",
        "  stat = unname(unlist(lapply(stat, as.list), recursive = FALSE)),",
        "  fmt_fun = lapply(stat_name, function(n) if (n == \"method\") as.character else 1L),",
        "  warning = list(NULL), error = list(NULL)), check = FALSE)"),
        collapse = "
"),
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
#' 95% CI, p-value) in that arm's column.  The LS means are cardx's
#' `ard_emmeans_emmeans()`; the differences are emmeans' own
#' (`emmeans::contrast(method = "trt.vs.ctrl")`, each arm against the
#' reference arm).  The
#' reference arm is the first level of the arm: the first arm in the code
#' list's order, else in its number order (`TRT01AN`), else the data's.  `ref`
#' only puts the named arm first (a relevel in the program).  The
#' program says which (a message).  The p-value is
#' written as the program's `fmt_pvalue()` writes it (`<0.001`, else 3
#' decimals).
#'
#' `adjust` is the multiplicity adjustment of the differences' CIs and
#' p-values, one of emmeans' `adjust` values (see "P-value adjustments"
#' in [emmeans::summary.emmGrid()]): `"none"`, `"tukey"`, `"dunnettx"`,
#' `"sidak"`, `"bonferroni"`, `"holm"`, `"hommel"`, `"hochberg"`, `"BH"`,
#' `"BY"`, `"fdr"`, `"mvt"` or `"scheffe"`.  **The default is `"none"`: no
#' adjustment.**  Before this argument existed, a table with three arms or
#' more was Tukey-adjusted without saying so (cardx's
#' `ard_emmeans_contrast()` takes emmeans' pairwise contrast, whose default
#' is Tukey); with two arms nothing was adjusted, and `"none"` gives the
#' same table.  Pass `adjust` always, even `"none"`: emmeans' own default
#' for these differences is `"dunnettx"`.  For differences against one
#' reference arm, `"tukey"` is the Tukey adjustment over all pairs of arms
#' (emmeans' pairwise contrast), of which the table shows the differences
#' against the reference arm, and it gives the table as it was before this
#' argument for three arms or more.
#' `"mvt"` is a Monte Carlo method: its numbers change a little from run
#' to run unless the seed is set.
#'
#' @inheritParams tpl_demog
#' @param ... The template's other parameters: `data`, `param`, `visit`
#'   (required), `by`, `response`, `covariates`, `where`, `ref`, `adjust`.
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
