# Sunburst code for treatment sequences

The script builds paths with
[`tfl_sunburst_data()`](https://ichirio.github.io/tflspec/reference/tfl_sankey_data.md)
and draws them with
[`tfl_plot_sunburst()`](https://ichirio.github.io/tflspec/reference/tfl_plot_sunburst.md).

## Usage

``` r
tfl_fig_sunburst(
  adam = NULL,
  style = "rings",
  data = "ADLOT",
  id = "USUBJID",
  stage = "LINE",
  category = "TRT",
  pop = "FASFL",
  palette = "treatment",
  legend = "right",
  title = NULL,
  width = 6.5,
  height = 5.5,
  dpi = 300,
  units = "in",
  file = NULL,
  plot_id = "sunburst"
)
```

## Arguments

- adam:

  Optional ADaM data
  ([`tfl_read_adam()`](https://ichirio.github.io/tflspec/reference/tfl_read_adam.md));
  values found in the data (groups, colours) are written literally into
  the code.

- style:

  `rings` (one ring per line, inner = first line).

- data:

  Dataset with one row per subject and line.

- id, stage, category:

  Subject, line and category (e.g. treatment class) variables.

- pop:

  Population flag (`== "Y"`); `NULL` for none.

- palette:

  Palette preset for the categories.

- legend:

  `right`, `bottom` or `none`.

- title:

  Figure title.

- width, height, dpi, units:

  Figure size.

- file:

  Write the script to this file.

- plot_id:

  Used in the output file name.
