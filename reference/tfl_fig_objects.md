# The objects a figure's data steps read and make

Each data step of a design, in order, with the object it reads (`in`)
and the one it leaves (`out`): `df` after a read (`df2`, `df3` ... after
the next ones), the object of a named step (a KM fit, summary
statistics, any function's result), or what the step's own `in` and
`out` say. A GUI offers the objects made before a step as its `in`, and
those of the whole design to the layers.

## Usage

``` r
tfl_fig_objects(design)
```

## Arguments

- design:

  A `tfl_fig_design`.

## Value

A data frame, one row a step: `step`, `in`, `out` (`NA`: none).

## Examples

``` r
d <- tfl_fig_design(data = list(
  list(step = "read", dataset = "ADTTE"),
  list(step = "filter", expr = "AVAL > 0", out = "df_ev"),
  list(step = "survfit", `in` = "df_ev")))
tfl_fig_objects(d)
#>      step    in   out
#> 1    read  <NA>    df
#> 2  filter    df df_ev
#> 3 survfit df_ev   fit
```
