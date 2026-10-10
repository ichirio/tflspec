# eDISH plot code

Maximum post-baseline transaminase vs total bilirubin, both as multiples
of the upper limit of normal, with the Hy's law reference lines.

## Usage

``` r
tfl_fig_edish(
  adam = NULL,
  style = c("alt", "alt_ast"),
  data = "ADLB",
  alt = "ALT",
  ast = "AST",
  bili = "BILI",
  uln = "ANRHI",
  post_baseline = "AVISITN > 0",
  group = "TRT01A",
  pop = "SAFFL",
  where = NULL,
  legend = "inside",
  palette = "treatment",
  key = "USUBJID",
  theme = "boxed",
  title = NULL,
  width = 7,
  height = 6,
  dpi = 300,
  units = "in",
  file = NULL,
  plot_id = "edish"
)
```

## Arguments

- adam:

  Optional ADaM data
  ([`tfl_read_adam()`](https://ichirio.github.io/tflspec/reference/tfl_read_adam.md));
  values found in the data (groups, colours) are written literally into
  the code.

- style:

  `alt` (ALT on the x axis) or `alt_ast` (the larger of ALT and AST).

- data:

  Dataset name.

- alt, ast, bili:

  PARAMCD of ALT, AST and total bilirubin.

- uln:

  Upper limit of normal variable.

- post_baseline:

  Condition selecting post-baseline records (R code).

- group:

  Treatment variable; its first value is the reference.

- pop:

  Population flag (`== "Y"`); `NULL` for none.

- where:

  Extra record condition as R code, e.g. `'AVISIT != "Retrieval"'`;
  `NULL` for none.

- legend:

  Legend preset (`none`, `right`, `bottom`, `top`, `inside`, ...).

- palette:

  Palette preset of the group (`rate_ci`, `dodged`) or of the category
  (`stacked`, default `response`).

- key:

  Subject key used to join ADSL.

- theme:

  Theme preset.

- title:

  Figure title.

- width, height, dpi, units:

  Figure size.

- file:

  Write the script to this file.

- plot_id:

  Used in the output file name.
