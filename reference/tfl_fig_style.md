# The figure style standard

Every look-and-feel value of a figure – font sizes, line widths, tick
lengths, reference lines, the censor mark, colour palettes, event
markers, the output size – in one catalog, so every figure of a study
looks the same, whoever (or whatever) writes its code. The
[`tfl_fig_km()`](https://ichirio.github.io/tflspec/reference/tfl_fig_km.md)
/
[`tfl_fig_waterfall()`](https://ichirio.github.io/tflspec/reference/tfl_fig_waterfall.md)
/
[`tfl_fig_swimmer()`](https://ichirio.github.io/tflspec/reference/tfl_fig_swimmer.md)
generators take their defaults from it, and
[`tfl_fig_setup_code()`](https://ichirio.github.io/tflspec/reference/tfl_fig_setup_code.md)
writes it into a helper script for figure programs written by hand.

## Usage

``` r
tfl_fig_style()

tfl_fig_style_template(path)

tfl_read_fig_style(path)
```

## Arguments

- path:

  A workbook.

## Value

`tfl_fig_style()` and `tfl_read_fig_style()`: a list of three data
frames; `tfl_fig_style_template()`: `path`, invisibly.

## Details

The built-in catalog is taken from the sample programs the generators
were built from. A company keeps its own in a workbook: write the
built-in one with `tfl_fig_style_template()`, edit it, read it with
`tfl_read_fig_style()` and set it with
`options(tflspec.fig_style = tfl_read_fig_style(path))`.

- `settings` (`type`, `key`, `value`): one value per key; `type` blank
  applies to every figure, `km` / `waterfall` / `swimmer` to that type
  (and wins over the blank row).

- `colors` (`palette`, `value`, `colour`): a palette whose rows name a
  data `value` colours that value (`response`: CR, PR ...); one whose
  `value` is blank is used in order (`treatment`): **the order of its
  rows is the arms' colour order** – the first row colours the first arm
  (its code list's first value), and so on. The built-in `treatment`
  palette is Okabe-Ito's, in a colour-blind-safe order (`#0072B2`,
  `#D55E00`, `#009E73`, `#CC79A7`, `#E69F00`, `#56B4E9`); more arms than
  rows repeat the colours, with a warning, and
  [`tfl_check_fig()`](https://ichirio.github.io/tflspec/reference/tfl_fig_setup_code.md)
  says which arms share one.

- `settings` `treatment_shapes` / `treatment_linetypes` (optional, blank
  by default): the arms' point shapes (`16, 17, 15`) and line types
  (`solid, dashed, dotted`) in the same order, for print in black and
  white; a designed figure that colours by the arms maps them too.

- `markers` (`marker`, `shape`, `fill`, `colour`, `size`): event markers
  by label (a swimmer event named `Death` takes the `Death` row) and the
  figures' own symbols (`censor`, `assessment`).
