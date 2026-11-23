# A figure design as a figure report's content

Converts a figure design of the designer (its pieces and `plot:`
settings;
[`tfl_fig_design()`](https://ichirio.github.io/tflspec/reference/tfl_fig_design.md))
once into a figure report's `content:` of function calls, one item a
call: the plot's first item (`ggplot`, `ggsurvfit`), its layers by their
functions (`geom_line`, `add_censor_mark`, `geom_hline` ...), its
settings (`scale_*`, `coord_*`, `labs`, `theme_tfl`, `theme`), tflspec's
own items for what tflspec draws (`tfl_ard_number`, and the panels –
`tfl_risk_panel`, `tfl_text_column`, `tfl_n_panel` – as named plots with
the `wrap_plots` item that arranges them), and `tfl_image` last. The
figure is the one the design draws with the study's figure setup
([`tfl_fig_setup_code()`](https://ichirio.github.io/tflspec/reference/tfl_fig_setup_code.md)):
the palette is `scale_colour_tfl()` and the theme `theme_tfl()`, as the
setup has them.

## Usage

``` r
tfl_fig_content_from_design(design)
```

## Arguments

- design:

  A
  [`tfl_fig_design()`](https://ichirio.github.io/tflspec/reference/tfl_fig_design.md)
  (or its list).

## Value

A list: `data` (the report's `data:`, its steps the design's, with the x
axis's breaks as a `code` step when the figure uses them), `content`
(the items:
[`tfl_item()`](https://ichirio.github.io/tflspec/reference/tfl_r.md) and
[`tfl_plot()`](https://ichirio.github.io/tflspec/reference/tfl_plot.md)),
`template`.

## Examples

``` r
d <- tfl_fig_template("km_simple", data = "ADTTE", param = "TTDE",
                      pop = "SAFFL", group = "TRT01A")
x <- tfl_fig_content_from_design(d)
vapply(x$content, function(it) it$fn, "")
#> [1] "ggsurvfit"          "add_censor_mark"    "scale_colour_tfl"  
#> [4] "scale_y_continuous" "coord_cartesian"    "labs"              
#> [7] "theme_tfl"          "theme"              "tfl_image"         
```
