# Mean by visit: a figure from the template `mean_se` or `mean_ci`

The A use of a figure's template SPEC: the program makes the records
(the dataset read, the analysis set's subjects kept, the parameter, the
records with a value and a visit, the visits in their order), then one
call draws the mean of each group at each visit, a line and points, with
bars of +/- one standard error (`mean_se`) or the 95% confidence
interval (`mean_ci`). The means are made here, from the records. A
report's own layers and settings go after the call with `+`.

## Usage

``` r
tpl_mean_se(
  data,
  group = "TRT01A",
  value = "AVAL",
  visit_var = "AVISIT",
  visit_order = "AVISITN",
  param = NULL,
  plot = NULL
)

tpl_mean_ci(
  data,
  group = "TRT01A",
  value = "AVAL",
  visit_var = "AVISIT",
  visit_order = "AVISITN",
  param = NULL,
  plot = NULL
)
```

## Arguments

- data:

  The records: one a subject and visit, the parameter's.

- group:

  The lines' variable (`TRT01A`), bare or as text.

- value:

  The variable averaged (`AVAL`, `CHG`).

- visit_var:

  The visits on the x axis (`AVISIT`), a factor in their order.

- visit_order:

  The visits' order (`AVISITN`).

- param:

  The parameter, for the y axis' title; `NULL`: none.

- plot:

  The figure's settings over the template's (`x_label`, `y_by`, `legend`
  ..., as a designed figure's `plot:`).

## Value

A ggplot.

## See also

[`tfl_template_expand()`](https://ichirio.github.io/tflspec/reference/tfl_template_expand.md)
for the design it draws (the B use).

## Examples

``` r
saf <- adsl$USUBJID[adsl$SAFFL == "Y"]
df <- advs[advs$PARAMCD == "SYSBP" & advs$USUBJID %in% saf, ]
df$AVISIT <- stats::reorder(factor(df$AVISIT), df$AVISITN)
p <- tpl_mean_se(df, group = TRT01A, param = "SYSBP")
class(p)
#> [1] "ggplot2::ggplot" "ggplot"          "ggplot2::gg"     "S7_object"      
#> [5] "gg"             
```
