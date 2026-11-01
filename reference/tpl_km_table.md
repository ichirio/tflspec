# Kaplan-Meier estimates: a table plan from the template `km_table`

The median time to the event (95% CI) and the event-free probability
(95% CI) at each time, by arm, from an ARD of cardx's
`ard_survival_survfit()` (`probs = 0.5` and `times = `): the analyses
are the report's own, the template is the ARD to the table.

## Usage

``` r
tpl_km_table(ard, ..., codelists = NULL)
```

## Arguments

- ard:

  The study ARD's rows of this report (not normalized: the template's
  data step normalizes it).

- ...:

  The template's parameters: `by`, `by_levels`, `median_label`,
  `time_prefix`, `time_unit`, `drop_time_0`.

- codelists:

  The report's code lists; `NULL` takes the ARD's.

## Value

A
[`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html).
