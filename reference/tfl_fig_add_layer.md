# Add a layer to the figure designs' catalog

The layers a figure design draws are a catalog: each geom's function and
package, the aesthetics it maps and the settings it takes. tflspec lists
ggplot2's common geoms and some of extension packages; this adds one
more for the session – another package's geom, or a company's own layer
function – which
[`tfl_fig_parts()`](https://ichirio.github.io/tflspec/reference/tfl_fig_parts.md),
[`tfl_fig_design_code()`](https://ichirio.github.io/tflspec/reference/tfl_fig_design.md)
and a GUI then offer like the others.

## Usage

``` r
tfl_fig_add_layer(
  layer,
  fn,
  package,
  label = fn,
  aes = character(),
  params = NULL,
  position = NA,
  help = NA
)
```

## Arguments

- layer:

  The layer's name in a design (`layer: <name>`).

- fn, package:

  The function and its package.

- label:

  The name shown.

- aes:

  The aesthetics it maps: a character vector (names = the aesthetic,
  values = labels), or a data frame with `field`, `kind`, `label`,
  `default`, `choices`, `required`, `help`.

- params:

  The settings, as `aes`; `kind` is one of `number`, `text`, `choice`,
  `logical`, `shape`, `values` (several numbers), `expr` (R).

- position:

  `"dodge"` when it can be dodged.

- help:

  A line on what it draws.

## Value

The layer's name, invisibly.

## Examples

``` r
tfl_fig_add_layer("beeswarm", "geom_beeswarm", "ggbeeswarm", "Beeswarm",
                  aes = c(x = "X", y = "Y", colour = "Colour by"),
                  params = data.frame(field = "size", kind = "number",
                                      label = "Size", default = "1.5"))
```
