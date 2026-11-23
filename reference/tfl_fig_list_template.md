# Write an Excel plot list template

One row per figure: `plot_id`, `type`, `style`, `title`, `param`,
`group`, `pop`, `legend`, `args`. Blank cells use the defaults. `args`
takes any further arguments in R syntax, e.g. `x_max = 24, x_by = 3` or
`events = c(Death = "DTHADY", Discontinued = "EOSDY")`.

## Usage

``` r
tfl_fig_list_template(adam = NULL, path, rows = NULL, n_rows = 300)
```

## Arguments

- adam:

  Optional ADaM data; offers PARAMCD values, grouping variables and
  population flags from the data in drop-downs.

- path:

  Output `.xlsx` path.

- rows:

  Optional data frame of rows to pre-fill (default: one example per
  type).

- n_rows:

  Rows prepared with drop-downs.

## Value

`path`, invisibly.
