# Whether a figure's design reads an ARD

A design reads the figure's ARD (`ard` in its program) when it has an
`ard_stats` step or an `ard_number` layer – a composed figure, when one
of its figures does: what the script's `attr(, "ard")` says
([`tfl_fig_design_code()`](https://ichirio.github.io/tflspec/reference/tfl_fig_design.md)),
without writing the script.

## Usage

``` r
tfl_fig_uses_ard(design)
```

## Arguments

- design:

  A `tfl_fig_design`.

## Value

`TRUE` or `FALSE`.

## Examples

``` r
tfl_fig_uses_ard(tfl_fig_template("km_risk_table"))
#> [1] FALSE
```
