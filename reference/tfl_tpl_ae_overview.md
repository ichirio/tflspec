# ae_overview: the template ae_overview

The table template `ae_overview`, layout `summary`. Its program is one
call, `tfl_tpl(ard, "ae_overview", ...)`; a parameter a report leaves
out takes the study standard's value, else the variant's default, else
the layout's.

## Parameters

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| Parameter | Tier | Stage | Default | Maps to | What |
| `population` | other | call | SAF | output.population | the analysis set's id |
| `subset` | other | call |  |  | which subjects (R, on the analysis set's ADSL): PART == "Part 1" |
| `by` | other | call | TRT01A | settings.treatment | the column variable |
| `data` | other | call | ADAE |  | the dataset of events |
| `where` | other | call | TRTEMFL == "Y" |  | the events kept (R): the treatment-emergent |
| `data_id` | other | call |  |  | the events' analysis data's id; NULL: \_ |
| `flags` | other | call | (id = ANY; label = Any TEAE); (id = REL; label = Any trea... |  | the rows, in order: each 'id' (the flag's name), 'label' (the row's text) and 'where' (R: the events it counts; none: every event kept) |
| `heading` | other | call | Subjects with |  | the label column's heading |

## Example

    plan <- tfl_tpl(ard, "ae_overview", population = "SAF")

## See also

[`tfl_tpl()`](https://ichirio.github.io/tflspec/reference/tfl_tpl.md),
[`tfl_tpl_help()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_help.md),
[`tfl_tpl_args()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_args.md),
[`tfl_templates()`](https://ichirio.github.io/tflspec/reference/tfl_templates.md)

## Examples

``` r
tfl_tpl_help("ae_overview", tier = c("required", "usual"))
#> ae_overview (table, layout summary; tflspec)
#> Call: tfl_tpl(ard, "ae_overview", ...)
```
