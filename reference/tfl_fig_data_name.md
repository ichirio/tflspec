# The name of a figure's data in its program

A designed figure's data steps make one data frame that its layers draw
from. In the program it is named for what it is, as the ARD programs
name theirs: `<dataset>_<parameter>` (`adtte_ttde`), else
`<dataset>_<population>` (`adsl_saf`). Code of the design's own (a
`code` step, a `layer_code` layer) says `df` and is written with the
name too. A design with neither a parameter nor a population keeps `df`,
and so does one whose code has the name already.

## Usage

``` r
tfl_fig_data_name(design)
```

## Arguments

- design:

  A
  [`tfl_fig_design()`](https://ichirio.github.io/tflspec/reference/tfl_fig_design.md).

## Value

The name, or `NULL` when the program calls it `df`.

## Examples

``` r
tfl_fig_data_name(tfl_fig_template("km_simple", data = "ADTTE", param = "TTDE",
                                   pop = "SAFFL", group = "TRT01A"))
#> [1] "adtte_ttde"
```
