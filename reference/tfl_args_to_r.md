# An item's arguments as R, and R as an item's arguments

A card of the builder can be edited as the R a function takes:
`tfl_args_to_r()` writes an item's arguments (its `args`, as the spec
holds them) as the text of a call's arguments –
`cols = "TRTA", rows = c(group = "variable")`; `tfl_args_from_r()` reads
that text back into the item's arguments, each value in the form the
spec file holds it (the schema's types: a map for a named vector, `!r`
for an expression, `{fn:, args:}` for a call of constants), and checks
them
([`tfl_schema_check()`](https://ichirio.github.io/tflspec/reference/tfl_schema_check.md)).
A figure's functions (`ggplot`, `geom_line`, `labs` ...) are read as the
figure's content reads them: `aes()` as a map of columns, the plot's
data by its name.

## Usage

``` r
tfl_args_to_r(fn, args)

tfl_args_from_r(fn, text)
```

## Arguments

- fn:

  The function's name (`plan_cells`, `geom_line`).

- args:

  The item's arguments, a named list.

- text:

  The arguments as R: what goes between the call's parentheses.

## Value

`tfl_args_to_r()`: one string. `tfl_args_from_r()`: the arguments, a
named list, with the problems of the check as the attribute `problems`
(none: no rows).

## Examples

``` r
tfl_args_to_r("plan_cells", list(categorical = "{n} ({p:.1f%})"))
#> [1] "categorical = \"{n} ({p:.1f%})\""
a <- tfl_args_from_r("geom_line", "aes(x = AVISIT, y = mean), linewidth = 0.5")
str(a)
#> List of 2
#>  $ mapping  :List of 2
#>   ..$ x: chr "AVISIT"
#>   ..$ y: chr "mean"
#>  $ linewidth: num 0.5
#>  - attr(*, "problems")='data.frame': 0 obs. of  4 variables:
#>   ..$ fn     : chr(0) 
#>   ..$ arg    : chr(0) 
#>   ..$ level  : chr(0) 
#>   ..$ message: chr(0) 
tfl_args_to_r("geom_line", a)
#> [1] "mapping = aes(x = AVISIT, y = mean), linewidth = 0.5"
```
