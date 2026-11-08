# disposition: the template disposition

The table template `disposition`, layout `summary`. Its program is one
call, `tfl_tpl(ard, "disposition", ...)`; a parameter a report leaves
out takes the study standard's value, else the variant's default, else
the layout's.

## Parameters

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| Parameter | Tier | Stage | Default | Maps to | What |
| `population` | required | call |  | output.population | the analysis set's id (SAF) |
| `subset` | other | call |  |  | which subjects (R, on the analysis set's ADSL): PART == "Part 1" |
| `categorical` | other | call | EOSSTT |  | the status variable(s), in order |
| `cat_id` | other | call | DISP |  | the status analysis' id in the ARD |
| `cat_label` | other | call | Status at end of study |  | the status analysis' label in the ARD |
| `labels` | other | call | Status at end of study, n (%) |  | each variable's heading; given by name, merged |
| `by` | usual | call | TRT01A | settings.treatment | the column variable |
| `categorical_statistics` | other | call | n, p |  | the categorical statistics the ARD has; NULL: cards' own |
| `groupn_label` | other | call |  |  | the subjects-per-group analysis' label |

## Aliases

The same template by the names of other collections' items: each runs it
as itself (`tfl_tpl(x, "stf_t05_disc_reasons", ...)`).

|  |  |  |  |
|----|----|----|----|
| Alias | Collection | Item | Title |
| `stf_t05_disc_reasons` | FDA STF IG v2 | T05 | Treatment and study discontinuation reasons |

Sources:

- FDA STF IG v2: U.S. Food and Drug Administration, CDER. Standard
  Safety Tables and Figures: Integrated Guide, Version 2.0, April 2025.
  <https://www.fda.gov/media/187065/download> (accessed 2026-10-28). A
  U.S. Government work; FDA's website content is in the public domain
  unless noted
  (https://www.fda.gov/about-fda/about-website/website-policies).

The aliases follow the numbering of these publications, and the titles
are short descriptions of our own, not their text. The templates are
tflspec's, not endorsed or recommended by FDA.

## Example

    plan <- tfl_tpl(ard, "disposition", population = "SAF")

## See also

[`tfl_tpl()`](https://ichirio.github.io/tflspec/reference/tfl_tpl.md),
[`tfl_tpl_help()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_help.md),
[`tfl_tpl_args()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_args.md),
[`tfl_templates()`](https://ichirio.github.io/tflspec/reference/tfl_templates.md)

## Examples

``` r
tfl_tpl_help("disposition", tier = c("required", "usual"))
#> disposition (table, layout summary; tflspec)
#> Call: tfl_tpl(ard, "disposition", ...)
#> 
#> required (1):
#>   param      | effective (layer) | what
#>   population | - (template)      | the analysis set's id (SAF)
#> 
#> usual (1):
#>   param | effective (layer) | what
#>   by    | TRT01A (template) | the column variable
```
