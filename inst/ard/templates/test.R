#' A test across two groups
#'
#' A Wilcoxon rank-sum test of each variable between the two groups, made an
#' ARD with cards::tidy_as_ard().  An analysis row names it as its method
#' ({name}, by TRT01A, variables AGE).  Its errors and warnings go into the
#' ARD's error / warning columns (they do not stop the study ARD);
#' tflspec::tfl_ard_conditions() lists them.
#'
#' @param data The analysis data (its dataset and analysis set).
#' @param by The group column: two groups.
#' @param variables The numeric variables.
#' @param ... Passed to stats::wilcox.test(), e.g. exact = FALSE.
{name} <- cards::as_cards_fn(
  function(data, by, variables, ...) {
    cards::process_selectors(data, by = {{ by }}, variables = {{ variables }})
    cards::bind_ard(lapply(variables, function(v) {
      ard <- cards::tidy_as_ard(
        lst_tidy = cards::eval_capture_conditions(
          broom::tidy(stats::wilcox.test(stats::reformulate(by, v), data = data, ...))
        ),
        tidy_result_names = c("statistic", "p.value", "method", "alternative"),
        formals = formals(stats::wilcox.test),
        passed_args = list(...),
        lst_ard_columns = list(group1 = by, variable = v, context = "{name}")
      )
      ard$stat_label <- ard$stat_name
      ard
    }))
  },
  stat_names = c("statistic", "p.value")
)
