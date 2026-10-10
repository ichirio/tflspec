# Kaplan-Meier plot code (ggsurvfit)

Kaplan-Meier plot code (ggsurvfit)

## Usage

``` r
tfl_fig_km(
  adam = NULL,
  param = "OS",
  group = "TRT01P",
  style = c("risk_table", "simple", "ci", "single_arm"),
  legend = NULL,
  pop = "FASFL",
  data = "ADTTE",
  time = "AVAL",
  censor = "CNSR",
  time_unit = "months",
  title = NULL,
  file = NULL,
  plot_id = "km",
  ard = NULL,
  ...
)
```

## Arguments

- adam:

  Optional ADaM data
  ([`tfl_read_adam()`](https://ichirio.github.io/tflspec/reference/tfl_read_adam.md));
  values found in the data (groups, colours) are written literally into
  the code.

- param:

  PARAMCD of the time-to-event parameter.

- group:

  Grouping variable (`NULL` for one curve).

- style:

  One of
  [`tfl_fig_types()`](https://ichirio.github.io/tflspec/reference/tfl_fig_types.md)
  for `km`.

- legend:

  `none`, `right`, `bottom`, `inside`, `inside_bl`, `panel`,
  `panel_right`, `panel_inside` (`panel*` = legend drawn from an item
  table).

- pop:

  Population flag (`== "Y"`); `NULL` for none.

- data:

  Dataset name.

- time, censor:

  Time and censor (1 = censored) variables.

- time_unit:

  Unit of `time` in the data is days; the axis is shown in `days`,
  `weeks`, `months` or `years`.

- title:

  Figure title.

- file:

  Write the script to this file.

- plot_id:

  Used in the output file name.

- ard:

  R code (a string) that gives the ARD of the KM table –
  `cardx::ard_survival_survfit(times = )` – e.g.
  `'readRDS("output/ard/ard.rds") |> subset(output_id == "T-14-2-2")'`.
  The number at risk is then read from it, so the figure and the table
  agree, and checked against the curve's own count. The risk table's
  times are the ARD's; the ARD's time unit must be the axis's.

- ...:

  Engine options, e.g. `x_max = 24`, `x_by = 3`, `palette = "grey"`,
  `theme = "classic"`, `width = 7`, `height = 5`.

## Value

A `tfl_code` object (character; printed as the script).
