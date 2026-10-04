#' A calculation of one's own, group by group
#'
#' Any calculation within each group, made an ARD with cards::ard_strata()
#' and cards::ard_identity() -- here the number of subjects above the
#' overall median.  An analysis row names it as its method ({name}, by
#' TRT01A, variables AGE).  Each statistic is one element of the list given
#' to ard_identity(); name them all in `stat_names`.
#'
#' @param data The analysis data (its dataset and analysis set).
#' @param by The group columns, e.g. TRT01A.
#' @param variables The numeric variables.
#' @param ... Not used.
{name} <- cards::as_cards_fn(
  function(data, by, variables, ...) {
    cards::process_selectors(data, by = {{ by }}, variables = {{ variables }})
    cards::ard_strata(data, .by = dplyr::all_of(by), .f = function(df) {
      cards::bind_ard(lapply(variables, function(v) {
        x <- df[[v]]
        cutoff <- stats::median(data[[v]], na.rm = TRUE)
        cards::ard_identity(
          list(n_above = sum(x > cutoff, na.rm = TRUE), n = sum(!is.na(x))),
          variable = v,
          context = "{name}"
        )
      }))
    })
  },
  stat_names = c("n_above", "n")
)
