# {name}(): any calculation, group by group, made an ARD with
# cards::ard_strata() and cards::ard_identity().
#
# An analysis row names it as its method:  {name}
#   method {name}   by TRT01A   variables AGE
# Each statistic is one element of the list given to ard_identity(); name
# them all in `stat_names`.

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
