# The analyses of a forest plot: the figure's own ARD

A forest plot prints numbers – the hazard ratio, its interval, N – so
they are statistics of an ARD, computed by cards / cardx and traceable
in ARS, not fitted inside the figure program. This gives the rows of an
ARD definition the plot needs: one analysis data (the parameter's
records of the population, the subgroup variables added from the
population's data) and one `custom` analysis per estimate – a Cox model
of all subjects
([`cardx::ard_regression()`](https://rdrr.io/pkg/cardx/man/ard_regression.html)),
and one within each subgroup variable
([`cards::ard_strata()`](https://pharmaverse.github.io/cards/latest-tag/reference/ard_strata.html)).
`tfl_fig_template("forest_hr")` returns them as the design's attribute
`analyses`; a study manager writes them to the figure's ARD definition.

## Usage

``` r
tfl_fig_forest_analyses(
  data = "ADTTE",
  param = "OS",
  pop = "FASFL",
  group = "TRT01P",
  subgroups = c("SEX", "AGEGR1"),
  time = "AVAL",
  censor = "CNSR",
  id = "HR"
)
```

## Arguments

- data:

  The time-to-event dataset (`ADTTE`).

- param:

  Its parameter (`PARAMCD`).

- pop:

  The population (analysis set), by its id or its flag (`FASFL`). The
  analysis data is named `<data>_<population>_<param>` (`adtte_fas_os`),
  the population without its flag's `FL`: the rule tflplanner names a
  data by.

- group:

  The treatment variable; its first level is the reference.

- subgroups:

  The subgroup variables (of the population's data), a character vector
  or one string with `,` between them.

- time, censor:

  The time and the censoring indicator (1 = censored).

- id:

  The analysis id of the overall estimate; a subgroup's is
  `<id>_<variable>`.

## Value

A list of two data frames, `analysis_data` and `analyses`, with the
columns of those sheets they fill (no `output_id`).

## Examples

``` r
tfl_fig_forest_analyses("ADTTE", "OS", "FASFL", "TRT01P", c("SEX", "AGEGR1"))$analyses
#>   analysis_id                      label method         data
#> 1          HR Hazard ratio, all subjects custom adtte_fas_os
#> 2      HR_SEX        Hazard ratio by SEX custom adtte_fas_os
#> 3   HR_AGEGR1     Hazard ratio by AGEGR1 custom adtte_fas_os
#>                                                                                                                                                                                            code
#> 1                                                                cox <- survival::coxph(survival::Surv(AVAL, CNSR == 0) ~ TRT01P, data = data)\ncardx::ard_regression(cox, exponentiate = TRUE)
#> 2    cards::ard_strata(\n  data,\n  .strata = SEX,\n  .f = ~ cardx::ard_regression(\n    survival::coxph(survival::Surv(AVAL, CNSR == 0) ~ TRT01P, data = .x),\n    exponentiate = TRUE\n  )\n)
#> 3 cards::ard_strata(\n  data,\n  .strata = AGEGR1,\n  .f = ~ cardx::ard_regression(\n    survival::coxph(survival::Surv(AVAL, CNSR == 0) ~ TRT01P, data = .x),\n    exponentiate = TRUE\n  )\n)
```
