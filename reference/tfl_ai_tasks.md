# The tasks AI-assisted drafting knows

What each task of the drafting functions
([`tfl_ai_prompt()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prompt.md),
[`tfl_ai_parse()`](https://ichirio.github.io/tflspec/reference/tfl_ai_parse.md),
[`tfl_ai_check()`](https://ichirio.github.io/tflspec/reference/tfl_ai_check.md),
[`tfl_ai_diff()`](https://ichirio.github.io/tflspec/reference/tfl_ai_diff.md))
fills, which of tflspec's checks reads its answer, and what its items
are matched by.

## Usage

``` r
tfl_ai_tasks()
```

## Value

A data frame: `task`, `fills`, `checked_by`, `keyed_by`, `per` (`study`,
`report` or `batch`: one prompt a study, a report, or a few reports).

## Details

- `toc`: the study's table of contents, one row a report, read through
  [`tfl_read_toc()`](https://ichirio.github.io/tflspec/reference/tfl_read_toc.md)
  (one prompt for the study).

- `figure`: one report's whole figure design
  ([`tfl_fig_design()`](https://ichirio.github.io/tflspec/reference/tfl_fig_design.md)),
  checked by
  [`tfl_check_fig_design()`](https://ichirio.github.io/tflspec/reference/tfl_fig_design.md).

- `table`: the report files of the YAML spec (`spec/outputs/<id>.yml`)
  of a batch of 5 to 10 tables – each its `ard:` analyses, `data:`,
  `codelists:`, `content:` items (one call each: `normalize_ard`,
  `table_plan`, `plan_*`) and `page:` – over the study's defaults; read
  by
  [`tfl_read_output()`](https://ichirio.github.io/tflspec/reference/tfl_read_output.md),
  checked item by item against the schema
  ([`tfl_schema_check()`](https://ichirio.github.io/tflspec/reference/tfl_schema_check.md))
  and the ADaM summary, and written as files by
  [`tfl_ai_unpack()`](https://ichirio.github.io/tflspec/reference/tfl_ai_unpack.md).

The `ard` task (the ARD definition alone) comes in a later version.

## Examples

``` r
tfl_ai_tasks()
#>     task
#> 1    toc
#> 2 figure
#> 3  table
#>                                                                                                      fills
#> 1             TOC rows: output_id, type, title lines, population, footnote lines, section, datasets, label
#> 2                                                    one figure's whole design (data, stats, plot, layers)
#> 3 the report files (spec/outputs/<id>.yml) of a batch of tables: ard, data, codelists, content items, page
#>                                                                  checked_by
#> 1                                                            tfl_read_toc()
#> 2                                                    tfl_check_fig_design()
#> 3 tfl_read_output(), tfl_schema_check(), tfl_ard_code(), tfl_content_code()
#>                                  keyed_by    per
#> 1                               output_id  study
#> 2 section, then step / layer and position report
#> 3                               output_id  batch
```
