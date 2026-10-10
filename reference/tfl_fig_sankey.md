# Sankey diagram code for treatment sequences

The script builds nodes and links with
[`tfl_sankey_data()`](https://ichirio.github.io/tflspec/reference/tfl_sankey_data.md)
and draws them with
[`tfl_plot_sankey()`](https://ichirio.github.io/tflspec/reference/tfl_plot_sankey.md).

## Usage

``` r
tfl_fig_sankey(
  adam = NULL,
  style = c("grey_links", "colored_links", "subgroups"),
  data = "ADLOT",
  id = "USUBJID",
  stage = "LINE",
  category = "TRT",
  by = NULL,
  pop = "FASFL",
  palette = "treatment",
  title = NULL,
  width = 9,
  height = 5,
  dpi = 300,
  units = "in",
  file = NULL,
  plot_id = "sankey"
)
```

## Arguments

- adam:

  Optional ADaM data
  ([`tfl_read_adam()`](https://ichirio.github.io/tflspec/reference/tfl_read_adam.md));
  values found in the data (groups, colours) are written literally into
  the code.

- style:

  `grey_links` (links in light grey), `colored_links` (links in the
  colour of their source node) or `subgroups` (one panel per value of
  `by`, on a shared scale).

- data:

  Dataset with one row per subject and line.

- id, stage, category:

  Subject, line and category (e.g. treatment class) variables.

- by:

  Subgroup variable for `style = "subgroups"`.

- pop:

  Population flag (`== "Y"`); `NULL` for none.

- palette:

  Palette preset for the categories.

- title:

  Figure title.

- width, height, dpi, units:

  Figure size.

- file:

  Write the script to this file.

- plot_id:

  Used in the output file name.
