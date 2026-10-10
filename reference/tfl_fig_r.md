# Raw R code in a figure design

A figure design's YAML is mostly declarative (numbers, strings, small
maps) written out as R by
[`tfl_fig_design_code()`](https://ichirio.github.io/tflspec/reference/tfl_fig_design.md);
`tfl_fig_r()` marks a string that is already R code and must be emitted
verbatim (e.g. `vars(PARAM)`,
[`scales::label_number()`](https://scales.r-lib.org/reference/label_number.html),
a bare `NA`) instead of being quoted as a string. In YAML this is the
`!r` tag: `theme: !r theme_risktable_default(axis.text.y.size = 9)`.

## Usage

``` r
tfl_fig_r(code)
```

## Arguments

- code:

  One string of R code.

## Value

A length-1 character vector of class `tfl_fig_r`.
