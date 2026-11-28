# ============================================================================
#  The catalogs an ARD spec is checked against and written from
# ----------------------------------------------------------------------------
#  Which methods an analysis may name (tfl_ard_methods()) and which statistics it
#  may ask for (tfl_ard_statistics()).  The built-in ones below are the default;
#  a company's own are passed as `methods =` / `statistics =` to the
#  functions that use them (tflplanner passes its company standards), or set
#  site-wide with options(tflspec.ard_methods =, tflspec.ard_statistics =).
# ============================================================================

# The statistics an ARD analysis may ask for.  `kind` says which methods
# give it: continuous (in statistic = of ard_summary()), categorical
# (ard_tabulate(), ard_tabulate_value(), hierarchical ...), missing
# (ard_missing()) or result (what a CI / test / model gives: statistics
# keeps some of them).  A continuous statistic without `fun` is cards' own
# (continuous_summary_fns()); one with `fun` is computed by that function
# of the non-missing values, written into the ARD program.  `fmt` is the
# format stat_fmt gets unless the method or the analysis says another:
# xx.x (1 decimal), xx.x% (a proportion as a percent), xx.x*1000 (times
# 1000), a number of decimals, or pvalue (<0.001 / 0.xxx).
.ard_statistics_builtin <- function() {
  q <- function(p) sprintf(
    "function(x) stats::quantile(x, %s, type = 2, names = FALSE)", p)
  ci <- function(sign, tr = "x", back = "") sprintf(
    "function(x) %s(mean(%s) %s stats::qt(0.975, length(x) - 1) * stats::sd(%s) / sqrt(length(x)))",
    back, tr, sign, tr)
  pos <- function(body) paste0("function(x) if (any(x <= 0)) NA_real_ else ",
                               body)
  gci <- function(sign) pos(sprintf(
    "exp(mean(log(x)) %s stats::qt(0.975, length(x) - 1) * stats::sd(log(x)) / sqrt(length(x)))",
    sign))
  rows <- list(
    # continuous: cards
    c("N", "continuous", "cards", "Number of non-missing values", "xx", ""),
    c("mean", "continuous", "cards", "Mean", "xx.x", ""),
    c("sd", "continuous", "cards", "SD", "xx.xx", ""),
    c("median", "continuous", "cards", "Median", "xx.x", ""),
    c("p25", "continuous", "cards", "Q1 (25th percentile)", "xx.x", ""),
    c("p75", "continuous", "cards", "Q3 (75th percentile)", "xx.x", ""),
    c("min", "continuous", "cards", "Min", "xx", ""),
    c("max", "continuous", "cards", "Max", "xx", ""),
    # continuous: computed
    c("se", "continuous", "spread", "SE of the mean", "xx.xx",
      "function(x) stats::sd(x) / sqrt(length(x))"),
    c("var", "continuous", "spread", "Variance", "xx.xx",
      "stats::var"),
    c("cv", "continuous", "spread", "CV (%)", "xx.x",
      "function(x) stats::sd(x) / mean(x) * 100"),
    c("range", "continuous", "spread", "Range (max - min)", "xx",
      "function(x) max(x) - min(x)"),
    c("iqr", "continuous", "spread", "IQR (Q3 - Q1)", "xx.x",
      "function(x) diff(stats::quantile(x, c(0.25, 0.75), type = 2, names = FALSE))"),
    c("sum", "continuous", "spread", "Sum", "xx", "sum"),
    c("p5", "continuous", "percentiles", "5th percentile", "xx.x", q("0.05")),
    c("p10", "continuous", "percentiles", "10th percentile", "xx.x", q("0.10")),
    c("p90", "continuous", "percentiles", "90th percentile", "xx.x", q("0.90")),
    c("p95", "continuous", "percentiles", "95th percentile", "xx.x", q("0.95")),
    c("mean_lcl", "continuous", "confidence limits",
      "Mean: lower 95% confidence limit (t)", "xx.x", ci("-")),
    c("mean_ucl", "continuous", "confidence limits",
      "Mean: upper 95% confidence limit (t)", "xx.x", ci("+")),
    c("geo_mean", "continuous", "log scale", "Geometric mean", "xx.xx",
      pos("exp(mean(log(x)))")),
    c("geo_sd", "continuous", "log scale", "Geometric SD", "xx.xx",
      pos("exp(stats::sd(log(x)))")),
    c("geo_cv", "continuous", "log scale", "Geometric CV (%)", "xx.x",
      pos("sqrt(exp(stats::sd(log(x))^2) - 1) * 100")),
    c("geo_lcl", "continuous", "log scale",
      "Geometric mean: lower 95% confidence limit", "xx.xx", gci("-")),
    c("geo_ucl", "continuous", "log scale",
      "Geometric mean: upper 95% confidence limit", "xx.xx", gci("+")),
    c("log_mean", "continuous", "log scale", "Mean of log values", "xx.xxx",
      pos("mean(log(x))")),
    c("log_sd", "continuous", "log scale", "SD of log values", "xx.xxx",
      pos("stats::sd(log(x))")),
    # categorical
    c("n", "categorical", "counts", "n (subjects / records in the level)",
      "xx", ""),
    c("N", "categorical", "counts", "N (the denominator)", "xx", ""),
    c("p", "categorical", "counts", "Percent (n / N)", "xx.x%", ""),
    c("n_cum", "categorical", "counts", "Cumulative n", "xx", ""),
    c("p_cum", "categorical", "counts", "Cumulative percent", "xx.x%", ""),
    # missing
    c("N_obs", "missing", "missing", "Number of records", "xx", ""),
    c("N_miss", "missing", "missing", "Missing", "xx", ""),
    c("N_nonmiss", "missing", "missing", "Non-missing", "xx", ""),
    c("p_miss", "missing", "missing", "Percent missing", "xx.x%", ""),
    c("p_nonmiss", "missing", "missing", "Percent non-missing", "xx.x%", ""),
    # results of CIs, tests, models, survival
    c("estimate", "result", "estimates", "Estimate", "xx.xx", ""),
    c("conf.low", "result", "estimates", "Lower confidence limit", "xx.xx", ""),
    c("conf.high", "result", "estimates", "Upper confidence limit", "xx.xx", ""),
    c("std.error", "result", "estimates", "Standard error", "xx.xxx", ""),
    c("estimate1", "result", "estimates", "Estimate of group 1", "xx.xx", ""),
    c("estimate2", "result", "estimates", "Estimate of group 2", "xx.xx", ""),
    c("statistic", "result", "tests", "Test statistic", "xx.xx", ""),
    c("parameter", "result", "tests", "Degrees of freedom", "xx.x", ""),
    c("p.value", "result", "tests", "p-value", "pvalue", ""),
    c("conf.level", "result", "estimates", "Confidence level", "xx.xx", ""),
    c("n.risk", "result", "survival", "Number at risk", "xx", ""),
    c("n.event", "result", "survival", "Number of events", "xx", ""))
  m <- do.call(rbind, rows)
  d <- data.frame(statistic = m[, 1], kind = m[, 2], group = m[, 3],
                  label = m[, 4], fmt = m[, 5], fun = m[, 6],
                  stringsAsFactors = FALSE)
  d$note <- ""
  d$note[d$group == "log scale"] <- "computed on log(x); NA when a value is 0 or below"
  d$note[d$statistic == "N" & d$kind == "continuous"] <- "the missing are counted by the method `missing`"
  d$fun[!nzchar(d$fun)] <- NA
  d$note[!nzchar(d$note)] <- NA
  d
}

# The keywords an analysis's `method` may name, and the call each stands for.
.ard_methods_builtin <- function() {
  # the event as every writer writes it
  s <- .tte_surv("AVAL", "CNSR")
  d <- data.frame(
    method = c("continuous", "categorical", "dichotomous", "missing",
               "hierarchical", "max", "subjects", "total_n",
               "proportion_ci", "mean_ci", "ttest", "wilcox", "chisq",
               "fisher", "cmh", "prop_diff", "lsmeans", "lsmeans_diff", "km",
               "logrank", "abnormal", "incidence_rate", "regression", "anova",
               "kruskal", "paired_ttest", "paired_wilcox", "mcnemar",
               "km_diff", "custom"),
    # the name a person reads (a GUI's choice, a heading); `note` says more
    label = c("Summary statistics", "Counts and percents",
              "Count of one level", "Missing counts",
              "Nested counts (e.g. SOC / PT)", "Worst level per subject",
              "Subjects with a record", "Number of subjects",
              "Proportion with CI", "Mean with CI", "t test",
              "Wilcoxon rank-sum test", "Chi-square test",
              "Fisher's exact test", "Cochran-Mantel-Haenszel test",
              "Difference in proportions", "Least-squares means",
              "Difference in least-squares means", "Kaplan-Meier estimates",
              "Log-rank test", "Abnormal post-baseline values",
              "Incidence rate", "Model estimates", "ANOVA / ANCOVA table",
              "Kruskal-Wallis test", "Paired t test",
              "Wilcoxon signed-rank test", "McNemar's test",
              "Difference in Kaplan-Meier estimates", "Custom R code"),
    call = c("cards::ard_summary", "cards::ard_tabulate",
             "cards::ard_tabulate_value", "cards::ard_missing",
             "cards::ard_stack_hierarchical", "cardx::ard_tabulate_max",
             "(subjects)", "cards::ard_total_n",
             "cardx::ard_categorical_ci", "cardx::ard_continuous_ci",
             "cardx::ard_stats_t_test", "cardx::ard_stats_wilcox_test",
             "cardx::ard_stats_chisq_test", "cardx::ard_stats_fisher_test",
             "cardx::ard_stats_mantelhaen_test", "cardx::ard_stats_prop_test",
             "cardx::ard_emmeans_emmeans", "cardx::ard_emmeans_contrast",
             "cardx::ard_survival_survfit", "cardx::ard_survival_survdiff",
             "cardx::ard_tabulate_abnormal", "cardx::ard_incidence_rate",
             "cardx::ard_regression", "cardx::ard_car_anova",
             "cardx::ard_stats_kruskal_test", "cardx::ard_stats_paired_t_test",
             "cardx::ard_stats_paired_wilcox_test",
             "cardx::ard_stats_mcnemar_test", "cardx::ard_survival_survfit_diff",
             "(code)"),
    kind = c("continuous", "categorical", "categorical", "missing",
             "categorical", "categorical", "categorical", "none", "none",
             "none", "none", "none", "none", "none", "none", "none", "none",
             "none", "none", "none", "none", "none", "none", "none", "none",
             "none", "none", "none", "none", "none"),
    defaults = c("", "", "", "", "denominator = population, id = <id>",
                 "denominator = population, id = <id>", "", "", "", "",
                 "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
                 "", "", "", "", ""),
    statistics = c("", "", "", "", "", "", "", "", "", "", "", "", "",
                   "", "", "", "", "", "", "", "", "", "", "", "", "", "",
                   "", "", ""),
    formats = c("", "", "", "", "", "", "", "", paste(
      "estimate=xx.x% | conf.low=xx.x% | conf.high=xx.x%"),
      "estimate=xx.x | conf.low=xx.x | conf.high=xx.x", "", "", "", "",
      "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""),
    note = c(
      "summary statistics of numeric variables",
      "counts and percents of each level",
      "counts of one level (args: value = list(VAR = \"Y\"))",
      "missing and non-missing counts",
      "nested subject counts, outermost variable first (SOC | PT)",
      "the worst level per subject; variables = the graded variable",
      "subjects with a record of the data (after where); variables = a name for the count",
      "number of subjects",
      "confidence interval of a proportion (args: method = \"wilson\" ...); statistics keep some of estimate, conf.low, conf.high",
      "confidence interval of a mean (args: conf.level = 0.9 ...); statistics keep some of estimate, conf.low, conf.high",
      "two-sample t test between the `by` groups (two of them)",
      "Wilcoxon rank-sum test between the `by` groups (two of them)",
      "chi-square test of variables x by",
      "Fisher's exact test of variables x by",
      "CMH test of variables x by, within the strata (strata column, e.g. a stratification factor)",
      "difference in proportions between the two `by` groups, with its CI; variables = a logical / 0-1 response",
      "LS means of a model by arm, e.g. ANCOVA (args: formula = CHG ~ TRT01P + BASE, method = \"lm\", response_type = \"continuous\")",
      "differences in LS means between arms, with CI and p-value (args as for lsmeans)",
      sprintf("Kaplan-Meier survival at times, or quantiles of the time to event (args: y = \"%s\", times = c(6, 12) or probs = 0.5); variables = the arm", s),
      sprintf("log-rank test between arms (args: formula = %s ~ TRT01P); by and variables blank", s),
      "subjects with an abnormal post-baseline value, by baseline (args: postbaseline = ANRIND, baseline = BNRIND, id = USUBJID)",
      "events per person-time with CI (args: time = <person-time column>, count = <events column>, id = USUBJID)",
      sprintf("a model's estimates with CI, e.g. a Cox model's hazard ratio (args: formula = %s ~ TRTA, method = \"coxph\", package = \"survival\", exponentiate = TRUE) or a logistic model's odds ratio (method = \"glm\", method.args = list(family = binomial), exponentiate = TRUE)", s),
      "a model's ANOVA table, type II or III (args: x = lm(CHG ~ TRTA + BASE, data = data), type = \"III\"); by and variables blank",
      "Kruskal-Wallis test across the `by` groups (two or more)",
      "paired t test between the two `by` levels (e.g. two visits) of each subject (args: id = USUBJID)",
      "Wilcoxon signed-rank test between the two `by` levels of each subject (args: id = USUBJID)",
      "McNemar's test of variables x by, paired data (one record a subject, e.g. a baseline and a post-baseline response)",
      sprintf("differences in Kaplan-Meier survival between the arms at times, with CI (args: x = survival::survfit(%s ~ TRTA, data = data), times = c(6, 12)); by and variables blank", s),
      "any R code in `code`; data and population are bound"),
    stringsAsFactors = FALSE)
  # since 2026-11-03: AE event counts and the one-sample, balance, rate and
  # one-way tests clinical tables use, each its function by another name
  more <- data.frame(
    method = c("hierarchical_count", "onesample_ttest", "onesample_wilcox",
               "smd", "poisson", "oneway", "aov", "mcnemar_long"),
    label = c("Nested event counts", "One-sample t test",
              "One-sample Wilcoxon signed-rank test",
              "Standardized mean difference", "Poisson rate test",
              "One-way ANOVA (Welch)", "Analysis of variance",
              "McNemar's test, long data"),
    call = c("cards::ard_stack_hierarchical_count",
             "cardx::ard_stats_t_test_onesample",
             "cardx::ard_stats_wilcox_test_onesample", "cardx::ard_smd_smd",
             "cardx::ard_stats_poisson_test", "cardx::ard_stats_oneway_test",
             "cardx::ard_stats_aov", "cardx::ard_stats_mcnemar_test_long"),
    kind = "none", defaults = c("denominator = population", rep("", 7L)),
    statistics = "", formats = "",
    note = c(
      "events (records, not subjects) at each level of a hierarchy, e.g. AEBODSYS | AEDECOD; with by and denominator for the overall column (args: overall = TRUE)",
      "one-sample t test of each variable's mean against mu, e.g. the change from baseline in each arm (by), with CI (args: mu = 0)",
      "one-sample Wilcoxon signed-rank test of each variable against mu, in each `by` group (args: mu = 0, conf.int = TRUE)",
      "standardized mean difference between the two `by` groups, e.g. baseline balance (args: std.error = TRUE)",
      "exact Poisson test of a rate, or the ratio of two arms' rates (by, two levels); variables are the events and the person-time columns, in that order",
      "one-way ANOVA across the groups, not assuming equal variances (args: formula = AGE ~ TRT01A); by and variables blank",
      "analysis of variance of a model (args: formula = CHG ~ TRTA + BASE); by and variables blank",
      "McNemar's test on long data, a record per subject and `by` level, e.g. baseline and week 12 responses (args: id = USUBJID)"),
    stringsAsFactors = FALSE)
  # since 2026-11-04: the record counts, labels, nested-model test, effect
  # sizes, Mood's test and the model checks, each its function by another
  # name
  more2 <- data.frame(
    method = c("records", "labels", "nested_models", "regression_basic",
               "cohens_d", "hedges_g", "paired_cohens_d", "paired_hedges_g",
               "mood", "vif", "wald"),
    label = c("Number of records", "Variable labels",
              "Test of nested models", "Model estimates (basic)",
              "Cohen's d", "Hedges' g", "Paired Cohen's d",
              "Paired Hedges' g", "Mood's test of scale",
              "Variance inflation factors", "Wald test of model terms"),
    call = c("cards::ard_tabulate_rows", "cards::ard_attributes",
             "cardx::ard_stats_anova", "cardx::ard_regression_basic",
             "cardx::ard_effectsize_cohens_d", "cardx::ard_effectsize_hedges_g",
             "cardx::ard_effectsize_paired_cohens_d",
             "cardx::ard_effectsize_paired_hedges_g",
             "cardx::ard_stats_mood_test", "cardx::ard_car_vif",
             "cardx::ard_aod_wald_test"),
    kind = "none", defaults = "", statistics = "", formats = "",
    note = c(
      "the number of records (rows, not subjects) in each `by` group, e.g. the number of adverse events per arm",
      "the variables' labels (and classes), e.g. for a table's row headings",
      "nested models compared, e.g. a treatment-by-subgroup interaction (args: formulas = list(AVALC ~ TRTA + SEX, AVALC ~ TRTA * SEX), method = \"glm\", method.args = list(family = binomial)); by and variables blank",
      "a model's estimates with fewer columns than `regression` (args: formula = CHG ~ TRTA + BASE, method = \"lm\")",
      "Cohen's d between the two `by` groups, with CI",
      "Hedges' g (Cohen's d corrected for small samples) between the two `by` groups, with CI",
      "Cohen's d of paired data, the two `by` levels of each subject (args: id = USUBJID)",
      "Hedges' g of paired data, the two `by` levels of each subject (args: id = USUBJID)",
      "Mood's two-sample test of scale between the `by` groups (two of them)",
      "variance inflation factors of a model's terms (args: x = lm(CHG ~ TRTA + BASE + AGE, data = data)); by and variables blank",
      "Wald tests of a model's terms (args: x = glm(AVALN ~ TRTA + SEX, family = binomial, data = data)); by and variables blank"),
    stringsAsFactors = FALSE)
  # since 2026-11-05: the functions that run other analyses (the rows whose
  # `parent` names this one), each its function by another name
  more3 <- data.frame(
    method = names(.ard_wrapper_keywords),
    label = c("Several analyses together", "Within each subgroup",
              "Each pair of groups"),
    call = unname(.ard_wrapper_keywords),
    kind = "none", defaults = "", statistics = "", formats = "",
    note = c(
      "runs the analyses whose `parent` is this one on the same data, by the same `by` (e.g. a demographics table's summaries and counts; overall = TRUE for a Total column)",
      "runs the analysis whose `parent` is this one within each level of `strata` (and `by`), e.g. a response rate's difference in each subgroup of a forest plot's table",
      "runs the analysis whose `parent` is this one for each pair of levels of the one column in `variables`, e.g. each arm against placebo (args: include = \"Placebo\")"),
    stringsAsFactors = FALSE)
  last <- nrow(d)
  d <- rbind(d[-last, ], more, more2, more3, d[last, ])
  rownames(d) <- NULL
  d
}

#' The methods an analysis row may name
#'
#' An analysis's `method` is one of these keywords, or the name of any
#' function, `pkg::fun` (every `cards::ard_*` and `cardx::ard_*` among
#' them), which is called as `pkg::fun(data, by = , variables = , ...)` with
#' the analysis data first, `by` and `variables` when the row gives them,
#' and the row's `args` after them.  A function whose first argument is not
#' the data (`cardx::ard_survival_survdiff(formula, data)`) takes it the same
#' way once `args` names the first one (`formula = ...`).  The row's
#' `strata` and `denominator` columns are passed as those arguments.
#'
#' The function a keyword calls (its `call`: `cards::ard_summary`,
#' `cards::ard_stack_hierarchical` ...) is that keyword's analysis: it gets
#' the same `statistic =` from `statistics`, the same defaults and the same
#' formats, so either name writes the same code.
#'
#' A function of the study's own is a method too, by its plain name
#' (`ard_riskdiff_mn`), when the study key `source` names the R file that
#' defines it (the ARD program sources it first).  It is called the same
#' way: it takes the analysis data first, `by` and `variables` as bare
#' column names, and gives a cards ARD (class `card`); `population` is
#' there to pass in `args` (`denominator = population`).
#'
#' In `args` and `code`, `data` is the analysis data and `population` the
#' population's subjects.  `args` is read as the arguments of a call, in
#' any order.
#'
#' Each keyword has a `label` -- the name a person reads, for a GUI's choice
#' or a heading -- and a one-line `note`, and names its function (`(subjects)` and `(code)` are the two
#' built into the engine), its `kind` -- how its `statistics` are passed:
#' `continuous`, `categorical` or `none` -- the arguments it gets unless
#' `args` gives them (`<id>` stands for the subject key), its default
#' statistics and formats.
#'
#' The catalog is the built-in one unless `options(tflspec.ard_methods = )`
#' holds another, or a function that uses it is given `methods =` (as
#' tflplanner does with its company standards).
#'
#' A catalog without a `label` column (one written before it was added)
#' gets the method's own name as its label.
#'
#' @return A data frame: `method`, `label`, `call`, `kind`, `defaults`,
#'   `statistics`, `formats`, `note`.
#' @examples
#' tfl_ard_methods()[, c("method", "label", "note")]
#' @export
tfl_ard_methods <- function() {
  m <- getOption("tflspec.ard_methods") %||% .ard_methods_builtin()
  m[] <- lapply(m, function(v) ifelse(is.na(v), "", v))
  if (!"label" %in% names(m)) m$label <- ""
  m$label[!nzchar(m$label)] <- m$method[!nzchar(m$label)]
  m[c("method", "label", setdiff(names(m), c("method", "label")))]
}

#' The statistics an ARD analysis may ask for
#'
#' Each statistic's `kind` -- which methods give it: `continuous`,
#' `categorical`, `missing`, or `result` (what a confidence interval, a test
#' or a model gives) --, its label, the format its `stat_fmt` gets unless the
#' method or the analysis gives another, and, for the continuous statistics
#' cards does not compute itself (CV, geometric mean, percentiles ...), the R
#' function written into the ARD program.
#'
#' An analysis's `formats` are `statistic=format` pairs, `|` between them
#' (`mean=xx.xx | sd=xx.xxx`); `VARIABLE:statistic=format` for one variable
#' only.  A format is `xx.x` (as many x after the point as decimals),
#' `xx.x%` (a proportion as a percent), `xx.x*1000` (the value times 1000,
#' with that many decimals: a rate per 1,000 person-years written from a
#' rate per person-year), a number of decimals, or `pvalue` (`<0.001`, else
#' 3 decimals).
#'
#' The catalog is the built-in one unless
#' `options(tflspec.ard_statistics = )` holds another, or a function that
#' uses it is given `statistics =`.
#'
#' @section A statistic of one's own:
#' A statistic of the package is chosen by its name alone (an analysis's
#' `statistics`, a template's `statistics` param).  Another one is added by
#' its name and the function that computes it, in three layers, each over
#' the one before: the built-in catalog, the company standard's
#' `ard_statistics` (a whole catalog, made from the built-in one), and the
#' study's own rows, the block `ard_statistics:` of its
#' `spec/defaults.yml`.  A later row with the same `statistic` and `kind`
#' replaces the earlier one; another is added.  A study's row has the
#' company standard's columns: `statistic`, `kind`, `label`, `fmt`, `fun`,
#' and `group` (optional, default `"study"`).  `fun` is one R function of
#' the non-missing values, `function(x) ...` or a function's name
#' (`stats::mad`): a continuous statistic needs one, unless cards computes
#' it itself.  It is written into the ARD program (`tfl_stats`), so the
#' program runs without tflspec.  A row whose `fun` is not one R function
#' stops, naming the row.  The study's programs and ARDs are made with its
#' rows ([tfl_ard_program()], [tfl_report_program()], [tfl_build_ard()] and
#' [tfl_ard_code()] of a [tfl_read_study_spec()] study); the session's
#' options never add one.
#'
#' A name an analysis asks for that is in none of the layers stops, naming
#' this catalog.
#'
#' ```yaml
#' # spec/defaults.yml (fictional): the geometric mean of the values > 0
#' ard_statistics:
#'   - statistic: gmean_pos
#'     kind: continuous
#'     label: Geometric mean (values > 0)
#'     fmt: xx.xx
#'     fun: "function(x) exp(mean(log(x[x > 0])))"
#' ```
#'
#' A report then asks for it by its name, as for a package statistic
#' (`statistics: [gmean_pos]`, a cell `"{gmean_pos}"`).
#'
#' @param kind Only the statistics of these kinds; `NULL` for all.
#' @return A data frame: `statistic`, `kind`, `group`, `label`, `fmt`,
#'   `fun`, `note`.
#' @examples
#' tfl_ard_statistics("continuous")[, c("statistic", "label", "fun")]
#' @export
tfl_ard_statistics <- function(kind = NULL) {
  d <- getOption("tflspec.ard_statistics") %||% .ard_statistics_builtin()
  if (!is.null(kind)) d <- d[d$kind %in% kind, , drop = FALSE]
  rownames(d) <- NULL
  d
}

# ---- the study's own statistics (spec/defaults.yml ard_statistics:) -------

# (documented with the defaults file, tfl_read_study_spec(); the reader of
# the study files does not read the block: the programs' makers do)
#' @name tfl_read_study_spec
#' @rdname tfl_read_study_spec
#' @section The study's own statistics:
#' `spec/defaults.yml` may hold `ard_statistics:`, the statistics the study
#' adds to the catalog ([tfl_ard_statistics()]): a list of rows with the
#' company standard's columns, `statistic`, `kind` (`continuous`,
#' `categorical`, `missing` or `result`), `label`, `fmt`, `fun` (one R
#' function of the non-missing values, written into the ARD program) and
#' `group` (optional).  They go over the built-in catalog and the company
#' standard's, a row with the same `statistic` and `kind` replacing the
#' earlier one; a report asks for one by its name.  The study's ARD and
#' report programs are made with them; a row whose `fun` is not one R
#' function stops, naming the row.  For example (fictional):
#'
#' ```yaml
#' ard_statistics:
#'   - statistic: gmean_pos
#'     kind: continuous
#'     label: Geometric mean (values > 0)
#'     fmt: xx.xx
#'     fun: "function(x) exp(mean(log(x[x > 0])))"
#' ```
NULL

# the kinds a statistic is of, and the columns of a study's row
.ard_stat_kinds <- c("continuous", "categorical", "missing", "result")
.ard_stat_study_cols <- c("statistic", "kind", "group", "label", "fmt", "fun", "note")

# The study's own statistics: its spec/defaults.yml's `ard_statistics:`
# block as catalog rows (checked), or NULL when it has none.  `dir` is the
# study folder (or its spec folder).
.study_ard_statistics <- function(dir) {
  if (is.null(dir) || !is.character(dir) || length(dir) != 1L || is.na(dir)) return(NULL)
  f <- file.path(.spec_dir(dir), "defaults.yml")
  if (!file.exists(f)) return(NULL)
  y <- yaml::read_yaml(f, handlers = .yaml_handlers())
  .ard_statistics_rows(y$ard_statistics, "spec/defaults.yml ard_statistics:")
}

# `x` (a list of rows, each a map of .ard_stat_study_cols) as catalog rows;
# a row that is not one stops, naming it (`where` says the block)
.ard_statistics_rows <- function(x, where) {
  if (is.null(x) || !length(x)) return(NULL)
  if (!is.list(x) || !is.null(names(x))) {
    stop(where, " is a list of rows, each with statistic, kind, label, fmt and fun.",
         call. = FALSE)
  }
  # (cards' own: a continuous row of these needs no fun)
  b <- .ard_statistics_builtin()
  own <- b$statistic[b$kind == "continuous" & is.na(b$fun)]
  rows <- lapply(seq_along(x), function(i) {
    r <- x[[i]]
    nm <- if (is.list(r) && is.character(r$statistic) && length(r$statistic) == 1L) {
      sprintf(" (%s)", r$statistic)
    } else ""
    at <- sprintf("%s row %d%s", where, i, nm)
    no <- function(...) stop(at, ": ", ..., call. = FALSE)
    if (!is.list(r) || is.null(names(r)) || !all(nzchar(names(r)))) {
      no("a row is a map: statistic, kind, label, fmt, fun (group optional).")
    }
    bad <- setdiff(names(r), .ard_stat_study_cols)
    if (length(bad)) {
      no(paste(bad, collapse = ", "), " is not a column (the columns: ",
         paste(.ard_stat_study_cols, collapse = ", "), ").")
    }
    one <- function(k) {
      v <- r[[k]]
      if (is.null(v)) return(NA_character_)
      if (!is.atomic(v) || length(v) != 1L) no(k, " is one value.")
      v <- as.character(v)
      if (is.na(v) || !nzchar(trimws(v))) NA_character_ else trimws(v)
    }
    v <- stats::setNames(lapply(.ard_stat_study_cols, one), .ard_stat_study_cols)
    for (k in c("statistic", "kind", "label", "fmt")) {
      if (is.na(v[[k]])) no("it needs its ", k, ".")
    }
    if (!grepl("^[A-Za-z.][A-Za-z0-9._]*$", v$statistic)) {
      no("statistic is a name (letters, digits, `.` and `_`).")
    }
    if (!v$kind %in% .ard_stat_kinds) {
      no("kind is one of ", paste(.ard_stat_kinds, collapse = ", "), ".")
    }
    if (!.fmt_ok(v$fmt)) {
      no("fmt is xx.x, xx.x%, xx.x*1000, a number of decimals or pvalue.")
    }
    if (!is.na(v$fun) && !.ard_stat_fun_ok(v$fun)) {
      no("fun is not one R function (function(x) ... of the non-missing values, ",
         "or a function's name): ", v$fun)
    }
    if (is.na(v$fun) && v$kind == "continuous" && !v$statistic %in% own) {
      no("a continuous statistic cards does not compute itself needs its fun ",
         "(function(x) ... of the non-missing values).")
    }
    if (is.na(v$group)) v$group <- "study"
    as.data.frame(v, stringsAsFactors = FALSE)
  })
  d <- do.call(rbind, rows)
  rownames(d) <- NULL
  d
}

# Is `f` one R function: a `function(x) ...` (or `\(x) ...`), or a
# function's name (`sum`, `stats::mad`)?
.ard_stat_fun_ok <- function(f) {
  e <- tryCatch(parse(text = f, keep.source = FALSE), error = function(e) NULL)
  if (length(e) != 1L) return(FALSE)
  e <- e[[1L]]
  if (is.name(e)) return(TRUE)
  is.call(e) && (identical(e[[1L]], as.name("function")) ||
                   (identical(e[[1L]], as.name("::")) && length(e) == 3L))
}

# The catalog `layers` make, in order (the company's or the built-in one,
# then the study's rows): a later row with the same statistic and kind
# replaces the earlier one in its place, another is added at the end.
# NULL layers are left out; one layer is returned as it is.
.ard_statistics_merge <- function(...) {
  layers <- Filter(Negate(is.null), list(...))
  if (!length(layers)) return(NULL)
  out <- layers[[1L]]
  for (l in layers[-1L]) {
    for (k in setdiff(names(out), names(l))) l[[k]] <- NA_character_
    for (k in setdiff(names(l), names(out))) out[[k]] <- NA_character_
    l <- l[names(out)]
    key_out <- paste(out$statistic, out$kind, sep = "\r")
    key_l <- paste(l$statistic, l$kind, sep = "\r")
    hit <- match(key_l, key_out)
    for (i in which(!is.na(hit))) out[hit[i], ] <- l[i, ]
    out <- rbind(out, l[is.na(hit), , drop = FALSE])
  }
  rownames(out) <- NULL
  out
}

# The catalog a study's programs are made with: `company` (the company
# standard's, else the current catalog) with the study's own rows over it
# (`dir`, the study folder); `company` itself (NULL too) when the study has
# none
.ard_statistics_of_study <- function(dir, company = NULL) {
  own <- .study_ard_statistics(dir)
  if (is.null(own)) return(company)
  .ard_statistics_merge(company %||% tfl_ard_statistics(), own)
}

# Use `statistics` / `methods` (when given) for the rest of the CALLER:
#   old <- .set_catalogs(statistics, methods); on.exit(options(old), add = TRUE)
.set_catalogs <- function(statistics = NULL, methods = NULL) {
  new <- list(tflspec.ard_statistics = statistics,
              tflspec.ard_methods = methods)
  new <- new[!vapply(new, is.null, NA)]
  if (length(new)) options(new) else list()
}

#' An empty ARD spec
#'
#' The five sheets with their columns and no rows, and the `study` keys a
#' new spec starts with: `id` (the subject key) and `output` (where the
#' study ARD goes).
#'
#' @return A list of data frames: `study`, `datasets`, `populations`,
#'   `analysis_data`, `analyses`.
#' @export
tfl_ard_spec_template <- function() {
  out <- lapply(.ard_spec_sheets, function(cols)
    as.data.frame(stats::setNames(replicate(length(cols), character(),
                                            simplify = FALSE), cols),
                  stringsAsFactors = FALSE))
  out$study <- data.frame(key = c("id", "output"),
                          value = c("USUBJID", "output/ard/ard.rds"),
                          stringsAsFactors = FALSE)
  out
}
