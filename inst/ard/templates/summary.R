#' Statistics of one's own on numeric variables
#'
#' Statistics cards does not have -- here the coefficient of variation and
#' the geometric mean -- for numeric variables, by group, as an ARD.  An
#' analysis row names it as its method ({name}, by TRT01A, variables
#' AGE | BMIBL).  Write your statistics in `statistic = ` and name them in
#' `stat_names`, so a check can see that it gives them
#' (tflspec::tfl_check_ard_function()).
#'
#' @param data The analysis data (its dataset and analysis set).
#' @param by The group columns, e.g. TRT01A.
#' @param variables The numeric variables.
#' @param ... Passed to cards::ard_summary().
{name} <- cards::as_cards_fn(
  function(data, by = NULL, variables, ...) {
    cards::ard_summary(
      data,
      by = {{ by }},
      variables = {{ variables }},
      statistic = ~ list(
        cv = function(x) stats::sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100,
        geo_mean = function(x) exp(mean(log(x[!is.na(x) & x > 0])))
      ),
      ...
    )
  },
  stat_names = c("cv", "geo_mean")
)
