# A plot of a figure's content: its name and its items

A figure of several plots (KM curves and the number at risk under them,
a forest plot and its text column) gives each one a name and its items:
in a report file `- km: [items]`. `tfl_plot()` is that entry.

## Usage

``` r
tfl_plot(name, items = list(), from = "own")
```

## Arguments

- name:

  The plot's name (`km`; the code calls it `p_km`).

- items:

  Its items
  ([`tfl_item()`](https://ichirio.github.io/tflspec/reference/tfl_r.md)),
  the plot's first item first.

- from:

  Where it came from, as an item's.

## Value

A `tfl_plot`: a
[`tfl_item()`](https://ichirio.github.io/tflspec/reference/tfl_r.md)
whose `fn` is the name, with `items`.

## Examples

``` r
tfl_plot("risk", list(tfl_item("tfl_risk_panel", list(fit = "fit"))))
#> <tfl_item> risk()
```
