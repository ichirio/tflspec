# Advice on a figure design

What is usually wanted and is missing or unusual in a design – a KM
figure without the number at risk, a legend inside the panel with many
groups, more groups than the palette has colours, text visits with no
order ... Not errors (those are
[`tfl_check_fig_design()`](https://ichirio.github.io/tflspec/reference/tfl_fig_design.md)):
the figure draws either way. Where one change would do it, the line
carries a fix that `tfl_fig_apply_fix()` makes: a layer or a step added,
a setting changed.

## Usage

``` r
tfl_fig_advice(design, adam = NULL, ggplot2_version = NULL)

tfl_fig_apply_fix(design, fix)
```

## Arguments

- design:

  A `tfl_fig_design`.

- adam:

  The data
  ([`tfl_read_adam()`](https://ichirio.github.io/tflspec/reference/tfl_read_adam.md));
  with it the groups' levels are counted against the palette and the
  legend.

- ggplot2_version:

  The ggplot2 the design is for (see
  [`tfl_fig_compat()`](https://ichirio.github.io/tflspec/reference/tfl_fig_compat.md)):
  what that version deprecates in a `call` is advice, with a fix that
  rewrites the call for it.

- fix:

  One row's `fix` (a list: `op` and its fields; `plot` names the plot of
  a composed design it is for).

## Value

`tfl_fig_advice()`: a data frame with `rule`, `level` (`info`,
`warning`), `part` (`data`, `stats`, `plot`, `layers`, `add`, `design`),
`message`, `template` and `args` (the message before
[`sprintf()`](https://rdrr.io/r/base/sprintf.html) and its values, for a
GUI that translates it) and `fix` (a list column; `NULL` where there is
no one-step fix). `tfl_fig_apply_fix()`: the design, changed.
