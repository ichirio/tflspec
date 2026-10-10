# Swimmer plot code

Swimmer plot code

## Usage

``` r
tfl_fig_swimmer(
  adam = NULL,
  style = c("full", "assessment", "response", "bar"),
  duration = "TRTDURD",
  start = NULL,
  end = NULL,
  id = "SUBJID",
  response = "BOR",
  assessment = "OVR",
  day = "ADY",
  events = c(Death = "DTHADY"),
  ongoing = c(EOSSTT = "ONGOING"),
  legend = "panel",
  pop = "FASFL",
  data = "ADSL",
  response_data = "ADRS",
  time_unit = "months",
  visit_every = NULL,
  key = "USUBJID",
  title = NULL,
  file = NULL,
  plot_id = "swimmer",
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
  for `swimmer`.

- duration:

  Bar length variable (days) on `data`; bars start at 0.

- start, end:

  Subtype "from start": bars run from `start` to `end` (days on a common
  origin, e.g. randomization) instead of 0 to `duration`.

- id:

  Y-axis label variable.

- response:

  PARAMCD of the best overall response (bar colour).

- assessment:

  PARAMCD of the response at each assessment.

- day:

  Assessment day variable in `response_data`.

- events:

  Named vector of event day variables on `data`, e.g.
  `c(Death = "DTHADY", Discontinued = "EOSDY")`. Shapes and colours are
  assigned in order; change them in the script.

- ongoing:

  Named value marking ongoing subjects, e.g. `c(EOSSTT = "ONGOING")`;
  `NULL` for no arrows.

- legend:

  `none`, `right`, `bottom`, `inside`, `inside_bl`, `panel`,
  `panel_right`, `panel_inside` (`panel*` = legend drawn from an item
  table).

- pop:

  Population flag (`== "Y"`); `NULL` for none.

- data:

  Dataset name.

- response_data:

  Dataset with `response` and `assessment`.

- time_unit:

  Unit of `time` in the data is days; the axis is shown in `days`,
  `weeks`, `months` or `years`.

- visit_every:

  Draw dotted visit lines every this many time units.

- key:

  Join key between datasets.

- title:

  Figure title.

- file:

  Write the script to this file.

- plot_id:

  Used in the output file name.

- ...:

  Engine options, e.g. `x_max = 24`, `x_by = 3`, `palette = "grey"`,
  `theme = "classic"`, `width = 7`, `height = 5`.
