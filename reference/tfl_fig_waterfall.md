# Waterfall plot code

Waterfall plot code

## Usage

``` r
tfl_fig_waterfall(
  adam = NULL,
  style = c("response", "plain"),
  param = "BPCHG",
  value = "AVAL",
  response = "BOR",
  legend = "inside",
  pop = "FASFL",
  data = "ADTR",
  response_data = "ADRS",
  id = "USUBJID",
  title = NULL,
  file = NULL,
  plot_id = "waterfall",
  ...
)
```

## Arguments

- adam:

  Optional ADaM data
  ([`tfl_read_adam()`](https://ichirio.github.io/tflspec/reference/tfl_read_adam.md));
  values found in the data (groups, colours) are written literally into
  the code.

- style:

  One of
  [`tfl_fig_types()`](https://ichirio.github.io/tflspec/reference/tfl_fig_types.md)
  for `waterfall`.

- param:

  PARAMCD of the one-row-per-subject parameter holding the best percent
  change (`NULL` when the dataset has no PARAMCD).

- value:

  Variable with the best percent change.

- response:

  PARAMCD of the best overall response in `response_data` (value taken
  from `AVALC`).

- legend:

  `none`, `right`, `bottom`, `inside`, `inside_bl`, `panel`,
  `panel_right`, `panel_inside` (`panel*` = legend drawn from an item
  table).

- pop:

  Population flag (`== "Y"`); `NULL` for none.

- data:

  Dataset name.

- response_data:

  Dataset of the response.

- id:

  Subject key.

- title:

  Figure title.

- file:

  Write the script to this file.

- plot_id:

  Used in the output file name.

- ...:

  Engine options, e.g. `x_max = 24`, `x_by = 3`, `palette = "grey"`,
  `theme = "classic"`, `width = 7`, `height = 5`.
