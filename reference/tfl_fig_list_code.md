# Generate code for every row of a plot list

Generate code for every row of a plot list

## Usage

``` r
tfl_fig_list_code(x, adam = NULL, dir = NULL)
```

## Arguments

- x:

  Path to a plot list `.xlsx`
  ([`tfl_fig_list_template()`](https://ichirio.github.io/tflspec/reference/tfl_fig_list_template.md))
  or a data frame with the same columns.

- adam:

  Optional ADaM data.

- dir:

  If given, write `<plot_id>.R` files here.

## Value

A named character vector of scripts (invisibly when `dir` is given).
