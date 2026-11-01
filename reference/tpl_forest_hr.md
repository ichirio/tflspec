# Hazard ratio by subgroup: a forest plot from the template `forest_hr`

The A use of the template SPEC `forest_hr`: the hazard ratios are the
report's ARD (its own analyses: a Cox model overall and within each
subgroup, made by the ARD program), and one call draws the forest from
it: `tpl_forest_hr()` the estimates and their 95% CIs, one row each, the
subgroups' levels under their heading. The report's own items go on it
with `+`; `tpl_forest_layout()` then puts the text column (N, the hazard
ratio and its CI) at its right.

## Usage

``` r
tpl_forest_hr(
  ard,
  group = "TRT01A",
  subgroups = c("SEX", "AGEGR1"),
  comparison = NULL,
  plot = NULL
)

tpl_forest_layout(fig)
```

## Arguments

- ard:

  The study ARD's rows of this report.

- group:

  The arms compared (`TRT01A`), bare or as text.

- subgroups:

  The subgroups (`c(SEX, AGEGR1)`), bare or as text.

- comparison:

  The arm compared with the reference; `NULL`: every other arm, each row
  saying which.

- plot:

  The figure's settings over the template's (`x_label`, `y_by`, `legend`
  ..., as a designed figure's `plot:`).

- fig:

  `tpl_forest_layout()`: the forest, with the report's own items.

## Value

`tpl_forest_hr()`: a ggplot; `tpl_forest_layout()`: a patchwork of it
and its text column.

## See also

[`tfl_template_expand()`](https://ichirio.github.io/tflspec/reference/tfl_template_expand.md)
for its ARD and design (the B use).
