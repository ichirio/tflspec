# Write the study ARD as JSON, YAML or XPT

A copy of the ARD for a reader that does not use R; the rds the ARD
program writes stays the record. What each format keeps:

## Usage

``` r
tfl_write_ard(
  ard,
  path,
  format = NULL,
  shape = c("rows", "nested"),
  xpt_version = 8L
)
```

## Arguments

- ard:

  An ARD (the study ARD,
  [`tfl_build_ard()`](https://ichirio.github.io/tflspec/reference/tfl_build_ard.md),
  or any cards ARD).

- path:

  The file to write.

- format:

  `"json"`, `"yaml"`, `"xpt"` or `"rds"`; `NULL` takes it from the
  file's extension.

- shape:

  For JSON / YAML: `"rows"` or `"nested"`.

- xpt_version:

  For XPT: `8` (default) or `5`.

## Value

`path`, invisibly, with the attribute `lost`: what the format could not
keep.

## Details

|  |  |  |
|----|----|----|
| format | shape | what is lost |
| rds | the ARD | nothing |
| JSON / YAML | `"rows"` (default): one record per statistic, with each variable's **levels** in order and the column types | the formatting functions (`fmt_fun`); `stat_fmt` keeps their result |
| JSON / YAML | `"nested"`: cards' [`cards::as_nested_list()`](https://pharmaverse.github.io/cards/latest-tag/reference/as_nested_list.html) | the formatting functions, `stat_label`, the id columns, the column types; the levels' order is only the keys' (not guaranteed) |
| XPT | a flat table (version 8; 5 shortens the column names, see below) | factors and their levels, the formatting functions, `warning` / `error`; `stat` becomes text when an analysis gives text as well as numbers |

A format that loses something says so in a warning.
[`tfl_read_ard()`](https://ichirio.github.io/tflspec/reference/tfl_read_ard.md)
reads `"rows"` back (and XPT as far as it can). XPT version 5 allows
8-character column names: `group1_level` is `GRP1LVL`, `variable_level`
`VARLVL`, `stat_name` `STATNAME`, `stat_label` `STATLBL`, `output_id`
`OUTID`, `analysis_id` `ANID`, `population_id` `POPID`, `variable`
`VARIABLE`, `context` `CONTEXT`, `stat_fmt` `STATFMT`.

## See also

[`tfl_read_ard()`](https://ichirio.github.io/tflspec/reference/tfl_read_ard.md)
