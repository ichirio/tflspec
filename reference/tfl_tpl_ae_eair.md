# ae_eair: the template ae_eair

The table template `ae_eair`, layout `hierarchical`. Its program is one
call, `tfl_tpl(ard, "ae_eair", ...)`; a parameter a report leaves out
takes the study standard's value, else the variant's default, else the
layout's.

## Parameters

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| Parameter | Tier | Stage | Default | Maps to | What |
| `population` | required | call |  | output.population | the analysis set's id (SAF) |
| `subset` | other | call |  |  | which subjects (R, on the analysis set's ADSL): PART == "Part 1" |
| `data` | other | call | ADAE |  | the dataset of records |
| `where` | other | call | TRTEMFL == "Y" |  | the records kept (R) |
| `by` | other | call | TRT01A | settings.treatment | the column variable |
| `hierarchy` | other | call | AEBODSYS, AEDECOD |  | the levels, outer first |
| `exposure` | other | call | TRTDURD |  | the ADSL variable of the days of exposure |
| `start` | other | call | ASTDY |  | the event's day from the start of treatment: a variable or R (ASTDT - TRTSDT + 1) |
| `per` | other | call | 100 |  | the rate per this many person-years |
| `overall` | other | call | Any TEAE |  | the any-record row's label |

## Aliases

The same template by the names of other collections' items: each runs it
as itself (`tfl_tpl(x, "stf_t16_ae_ir", ...)`).

|                 |               |      |                                         |
|-----------------|---------------|------|-----------------------------------------|
| Alias           | Collection    | Item | Title                                   |
| `stf_t16_ae_ir` | FDA STF IG v2 | T16  | AE incidence rates per 100 person-years |

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

    plan <- tfl_tpl(ard, "ae_eair", population = "SAF")

## See also

[`tfl_tpl()`](https://ichirio.github.io/tflspec/reference/tfl_tpl.md),
[`tfl_tpl_help()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_help.md),
[`tfl_tpl_args()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_args.md),
[`tfl_templates()`](https://ichirio.github.io/tflspec/reference/tfl_templates.md)

## Examples

``` r
tfl_tpl_help("ae_eair", tier = c("required", "usual"))
#> ae_eair (table, layout hierarchical; tflspec)
#> Call: tfl_tpl(ard, "ae_eair", ...)
#> 
#> required (1):
#>   param      | effective (layer) | what
#>   population | - (template)      | the analysis set's id (SAF)
```
