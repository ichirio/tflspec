# Extension functions a figure design knows

Functions of ggplot2 extension packages worth offering by name for a
`call` piece (see
[`tfl_fig_design()`](https://ichirio.github.io/tflspec/reference/tfl_fig_design.md)):
nested facets (ggh4x), markdown text (ggtext), a zoomed panel (ggforce),
a second colour or fill scale (ggnewscale). A `call` of one of them
needs no `package:`; the script calls it as `pkg::fn` and notes the
package under "# also needs". Any other function (cowplot, ggpubr,
ggbreak ...) is reached the same way with `package:` given. Geoms of
extension packages (ggrepel's `geom_text_repel`, ggforce's `geom_sina`)
are layers of the catalog instead
([`tfl_fig_add_layer()`](https://ichirio.github.io/tflspec/reference/tfl_fig_add_layer.md)).

## Usage

``` r
tfl_fig_calls()
```

## Value

A data frame: `package`, `fn`, `where` (`layers`, `plot.add`, or
`nested`: a value inside another call, e.g. in `theme()`), `label`,
`help`.
