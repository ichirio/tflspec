# ggplot2 3.5 / 4.0 differences for figure designs

The table
[`tfl_fig_design_code()`](https://ichirio.github.io/tflspec/reference/tfl_fig_design.md),
[`tfl_check_fig_design()`](https://ichirio.github.io/tflspec/reference/tfl_fig_design.md)
and
[`tfl_fig_advice()`](https://ichirio.github.io/tflspec/reference/tfl_fig_advice.md)
use to write and check a design's `call` pieces for one of the two
ggplot2 versions supported, 3.5 and 4.0: functions and arguments added,
renamed, removed or deprecated. A renamed one is written under the
target version's name; one the target does not have is an error of the
check; a deprecated one is advice with a fix.

## Usage

``` r
tfl_fig_compat(ggplot2_version = NULL)
```

## Arguments

- ggplot2_version:

  `"3.5"` or `"4.0"`: adds `status`, what each row means for that
  version (`ok`, `absent`, `removed`, `renamed`, `deprecated`,
  `changed`) – e.g. for a GUI to grey out what the version does not
  have.

## Value

A data frame: `package`, `fn`, `arg`, `change`, `from`, `replacement`,
`level`, `note` (and `status`).

## Details

The target version is, in this order: the `ggplot2_version` argument,
the design's top-level `ggplot2_version:`, the option
`tflspec.ggplot2_version`, the installed ggplot2's (its major.minor).

## Examples

``` r
cp <- tfl_fig_compat("3.5")
cp[cp$status == "absent", c("fn", "arg", "from")]
#>                 fn           arg  from
#> 4     element_geom          <NA> 4.0.0
#> 5    element_point          <NA> 4.0.0
#> 6  element_polygon          <NA> 4.0.0
#> 7      stat_manual          <NA> 4.0.0
#> 8     stat_connect          <NA> 4.0.0
#> 9            theme          geom 4.0.0
#> 10           theme     palette.* 4.0.0
#> 11         theme_*           ink 4.0.0
#> 12         theme_*         paper 4.0.0
#> 13         theme_*        accent 4.0.0
#> 14 coord_cartesian       reverse 4.0.0
#> 15    coord_radial       reverse 4.0.0
#> 16        coord_sf       reverse 4.0.0
#> 17            labs    dictionary 4.0.0
#> 18      geom_label border.colour 4.0.0
#> 19      geom_label   text.colour 4.0.0
#> 20   stat_ydensity     quantiles 4.0.0
```
