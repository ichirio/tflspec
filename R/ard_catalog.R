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
# xx.x (1 decimal), xx.x% (a proportion as a percent), a number of
# decimals, or pvalue (<0.001 / 0.xxx).
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
    c("N", "continuous", "cards", "n (non-missing values)", "xx", ""),
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
      "function(x) stats::var(x)"),
    c("cv", "continuous", "spread", "CV (%)", "xx.x",
      "function(x) stats::sd(x) / mean(x) * 100"),
    c("range", "continuous", "spread", "Range (max - min)", "xx",
      "function(x) max(x) - min(x)"),
    c("iqr", "continuous", "spread", "IQR (Q3 - Q1)", "xx.x",
      "function(x) diff(stats::quantile(x, c(0.25, 0.75), type = 2, names = FALSE))"),
    c("sum", "continuous", "spread", "Sum", "xx", "function(x) sum(x)"),
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
  data.frame(
    method = c("continuous", "categorical", "dichotomous", "missing",
               "hierarchical", "max", "subjects", "total_n",
               "proportion_ci", "mean_ci", "ttest", "wilcox", "chisq",
               "fisher", "custom"),
    # the name a person reads (a GUI's choice, a heading); `note` says more
    label = c("Summary statistics", "Counts and percents",
              "Count of one level", "Missing counts",
              "Nested counts (e.g. SOC / PT)", "Worst level per subject",
              "Subjects with a record", "Number of subjects",
              "Proportion with CI", "Mean with CI", "t test",
              "Wilcoxon rank-sum test", "Chi-square test",
              "Fisher's exact test", "Custom R code"),
    call = c("cards::ard_summary", "cards::ard_tabulate",
             "cards::ard_tabulate_value", "cards::ard_missing",
             "cards::ard_stack_hierarchical", "cardx::ard_tabulate_max",
             "(subjects)", "cards::ard_total_n",
             "cardx::ard_categorical_ci", "cardx::ard_continuous_ci",
             "cardx::ard_stats_t_test", "cardx::ard_stats_wilcox_test",
             "cardx::ard_stats_chisq_test", "cardx::ard_stats_fisher_test",
             "(code)"),
    kind = c("continuous", "categorical", "categorical", "missing",
             "categorical", "categorical", "categorical", "none", "none",
             "none", "none", "none", "none", "none", "none"),
    defaults = c("", "", "", "", "denominator = population, id = <id>",
                 "denominator = population, id = <id>", "", "", "", "",
                 "", "", "", "", ""),
    statistics = c("", "", "", "", "", "", "", "", "", "", "", "", "",
                   "", ""),
    formats = c("", "", "", "", "", "", "", "", paste(
      "estimate=xx.x% | conf.low=xx.x% | conf.high=xx.x%"),
      "estimate=xx.x | conf.low=xx.x | conf.high=xx.x", "", "", "", "",
      ""),
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
      "any R code in `code`; data and population are bound"),
    stringsAsFactors = FALSE)
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
#' `xx.x%` (a proportion as a percent), a number of decimals, or `pvalue`
#' (`<0.001`, else 3 decimals).
#'
#' The catalog is the built-in one unless
#' `options(tflspec.ard_statistics = )` holds another, or a function that
#' uses it is given `statistics =`.
#'
#' @param kind Only the statistics of these kinds; `NULL` for all.
#' @return A data frame: `statistic`, `kind`, `group`, `label`, `fmt`,
#'   `fun`, `note`.
#' @export
tfl_ard_statistics <- function(kind = NULL) {
  d <- getOption("tflspec.ard_statistics") %||% .ard_statistics_builtin()
  if (!is.null(kind)) d <- d[d$kind %in% kind, , drop = FALSE]
  rownames(d) <- NULL
  d
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
