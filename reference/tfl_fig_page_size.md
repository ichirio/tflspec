# A figure's size on its report's page

How big a figure report's figure is drawn and placed: the report sheet's
`figure_width_in` / `figure_height_in`, each a number of inches (that
size), `fit` (the page's body) or blank (the figure's own size, as
rtfreporter places an image: `native`). The body is the page (the page
sheet's `width_in` / `height_in`, else its `paper_size` in its
`orientation`) less its margins; its height less, too, the report's
title and footnote lines (and a line between each and the figure), at
the page's font size. A report program draws the figure at this size
(its text the size its theme says) and the report places it so; the dpi
only sets how many pixels it has.

## Usage

``` r
tfl_fig_page_size(spec, output_id = NULL, native = c(6.5, 4.5))
```

## Arguments

- spec:

  A report definition
  ([`tfl_read_report_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_report_spec.md)),
  or the path(s) to read it from.

- output_id:

  The report, when `spec` defines several.

- native:

  The figure's own size in inches, `c(width, height)`: what a blank
  gives (a study's figure style says it by the figure's kind;
  rtfreporter draws a plot at 6.5 x 4.5).

## Value

A list: `width_in`, `height_in` (inches, two decimals) and `mode`
(`c(width = , height = )`: `"native"`, `"fit"` or `"fixed"`).

## Examples

``` r
sp <- tfl_table_spec(
  report = data.frame(output_id = "F-1", type = "figure",
                      figure_width_in = "fit"),
  page = data.frame(output_id = NA, paper_size = "letter",
                    orientation = "landscape"),
  titles = data.frame(output_id = "F-1", line = c("1", "2"),
                      center = c("Figure 14.2.1", "Kaplan-Meier")))
tfl_fig_page_size(sp, "F-1")
#> $width_in
#> [1] 9.5
#> 
#> $height_in
#> [1] 4.5
#> 
#> $mode
#>    width   height 
#>    "fit" "native" 
#> 
```
