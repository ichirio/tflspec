# The arguments of every figure type, described

One row per argument of each figure type's `tfl_fig_<type>()` (and, for
`km`, `waterfall` and `swimmer`, the axis and look options it takes
through `...`): what a figure design
([`tfl_fig_design()`](https://ichirio.github.io/tflspec/reference/tfl_fig_design.md))
may say, and what a GUI draws its form from.

## Usage

``` r
tfl_fig_schema(type = NULL)
```

## Arguments

- type:

  A figure type
  ([`tfl_fig_types()`](https://ichirio.github.io/tflspec/reference/tfl_fig_types.md));
  `NULL` for all.

## Value

A data frame: `type`, `fun`, `arg`, `section`, `kind`, `level`, `label`,
`help`, `of`, `default`, `choices`.

## Details

- `section`: `data`, `mapping` (the variables), `style`, `axes`,
  `legend` or `output`;

- `kind`: `dataset`, `param` (a PARAMCD of the dataset named in `of`),
  `variable` / `variables` / `flag` (of `of`), `value`, `choice`,
  `number`, `logical`, `text`, `expr` (R code), `named`
  (`label = variable` pairs);

- `level`: `basic` (shown first) or `advanced`;

- `default`: the function's default, as text (`NA` = none / computed);

- `choices`: for `choice`, the values, `|` between them.
