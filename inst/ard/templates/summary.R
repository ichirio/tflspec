# {name}(): statistics of one's own, on numeric variables, by group.
#
# An analysis row names it as its method:  {name}
#   method {name}   by TRT01A   variables AGE | BMIBL
# The statistics are functions of x (the values of one variable within one
# group); write yours in `statistic = ` and name them in `stat_names`, so a
# check can see that it gives them (tflspec::tfl_check_ard_function()).

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
