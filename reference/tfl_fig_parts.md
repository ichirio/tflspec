# The pieces of a figure design

Every piece a figure design
([`tfl_fig_design()`](https://ichirio.github.io/tflspec/reference/tfl_fig_design.md))
is made of – the data steps (those with a `name` make an object of their
own: a fit, summary statistics ...), the figure-wide settings and the
layers – with its fields: what a GUI lists, adds and edits one by one.

## Usage

``` r
tfl_fig_parts()
```

## Value

A data frame: `section` (`data`, `plot`, `layers`), `piece`,
`piece_label`, `piece_help`, `field`, `kind` (`dataset`, `variable`,
`variables`, `flag`, `param`, `object` (the data or a statistic by
name), `choice`, `number`, `logical`, `text`, `expr` (R), `code`,
`named` (`name = value | ...`), and of the figure's ARD: `analysis`,
`ard_variable`, `ard_stat`, `ard_stats` (one or several statistics)),
`label`, `default`, `choices` (`|` between them), `help`, `required`,
`of`.
