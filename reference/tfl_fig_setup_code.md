# The helper script of a study's figure programs

`tfl_fig_setup_code()` writes the figure style standard
([`tfl_fig_style()`](https://ichirio.github.io/tflspec/reference/tfl_fig_style.md))
as data – `tfl_settings`, `tfl_palettes`, `tfl_markers` – followed by
helpers in plain ggplot2:

## Usage

``` r
tfl_fig_setup_code(style = tfl_fig_style(), date = Sys.Date())

tfl_check_fig(plot, levels = NULL, quiet = FALSE)
```

## Arguments

- style:

  A figure style
  ([`tfl_fig_style()`](https://ichirio.github.io/tflspec/reference/tfl_fig_style.md)).

- date:

  The date stamped in the banner.

- plot:

  A ggplot (or a patchwork of them).

- levels:

  The values the legend must show, in this order.

- quiet:

  `TRUE` leaves out the summary message.

## Value

`tfl_fig_setup_code()`: the script, one element per line.
`tfl_check_fig()`: invisibly, a list: `problems` (each also a warning
starting "Figure check:"), `notes`, `fingerprint` (an md5 of the drawn
data, to tell whether a figure changed between two runs).

## Details

- `theme_tfl(type)` – the standard's theme, font sizes, lines and ticks;

- `scale_colour_tfl(palette)` / `scale_fill_tfl()` / `tfl_colours()` –
  the standard's palettes;

- `tfl_marker(name)` – an event or symbol marker (`Death`, `censor`
  ...);

- `tfl_save(plot, file, type)` – `ggsave()` at the standard's size;

- `tfl_km_risk(ard, fit)` – a KM figure's number at risk from the KM
  table's ARD (`cardx::ard_survival_survfit(times = )`), checked against
  the curve;

- `tfl_check(plot, levels = )` – the checks of `tfl_check_fig()`.

A figure program sources it (`source("programs/tfl/fig_setup.R")`) and
ends with `tfl_check(plot)`. `tfl_check_fig()` runs the same checks from
R, against the style in use.
