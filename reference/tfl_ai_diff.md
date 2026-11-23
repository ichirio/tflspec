# What a drafting answer changes

Compares the current state with a read answer
([`tfl_ai_parse()`](https://ichirio.github.io/tflspec/reference/tfl_ai_parse.md)),
item by item, for a review where each item is accepted or rejected.

## Usage

``` r
tfl_ai_diff(current, answer, task = NULL)
```

## Arguments

- current:

  What there is now: `toc`, a data frame of TOC rows
  ([`tfl_read_toc()`](https://ichirio.github.io/tflspec/reference/tfl_read_toc.md)'s
  fields as columns; `NULL` for none); `figure`, a
  [`tfl_fig_design()`](https://ichirio.github.io/tflspec/reference/tfl_fig_design.md)
  (`NULL` for none). A `tfl_ai_answer` is accepted too.

- answer:

  A `tfl_ai_answer`, or what `current` is.

- task:

  One of
  [`tfl_ai_tasks()`](https://ichirio.github.io/tflspec/reference/tfl_ai_tasks.md);
  by default the answer's.

## Value

A data frame, one row an item and, for a changed item, one a changed
field: `part` (`toc`, or the section), `key` (the output id; the piece's
name, with `#2` for a second one of a name), `status` (`added`,
`changed`, `removed`, `moved`, `same`), `field`, `was`, `now` (for
`changed` the field's values; for `moved` the positions).

## Details

- `toc`: the rows matched by `output_id`; a changed row has one row a
  changed column.

- `figure`: the pieces of each section (`data`, `layers`) matched by
  their `step` / `layer` name in order of appearance (the second `point`
  layer with the second); the figure-wide `plot` settings as one piece,
  field by field; the design's `template`, `ggplot2_version` and
  `compose` as the piece `design`. A piece that keeps its fields but
  changes its order relative to the others is `moved` (one inserted in
  the middle moves nothing). A composed figure's plots are compared plot
  by plot (`plots$km/layers` ...).

## Examples

``` r
cur <- tfl_fig_template("km_simple")
new <- cur
new$layers <- c(new$layers, list(list(layer = "censor_mark", shape = "x")))
new$plot$x_label <- "Time (Months)"
d <- tfl_ai_diff(cur, new, "figure")
d[d$status != "same", ]
#>      part           key status field  was now
#> 10 layers censor_mark#2  added  <NA> <NA>   3
```
