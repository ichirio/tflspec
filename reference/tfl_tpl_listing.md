# listing: the template listing

The listing template `listing`, layout `listing`. Its program is one
call, `tfl_tpl(adae, "listing", ...)`; a parameter a report leaves out
takes the study standard's value, else the variant's default, else the
layout's.

## Parameters

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| Parameter | Tier | Stage | Default | Maps to | What |
| `data` | required | call |  |  | the dataset (ADAE) |
| `columns` | required | call |  |  | the columns, in order: each a list of vars (one or more), label, width and collapse_repeats |
| `where` | other | call |  |  | the records kept (R) |
| `subset` | other | call |  |  | which subjects (R, on ADSL): a listing has no analysis set, so ADSL's subjects it keeps (a semi join) |
| `sort` | other | call |  |  | the records' order (variables) |

## Aliases

The same template by the names of other collections' items: each runs it
as itself (`tfl_tpl(x, "stf_t01_trials", ...)`).

|  |  |  |  |
|----|----|----|----|
| Alias | Collection | Item | Title |
| `stf_t01_trials` | FDA STF IG v2 | T01 | Trials in the submission |
| `stf_t09_death_list` | FDA STF IG v2 | T09 | Listing of deaths |
| `stf_t39_disc_list` | FDA STF IG v2 | T39 | Listing of AEs leading to discontinuation |

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

    plan <- tfl_tpl(adae, "listing", data = "ADAE", columns = list(list(vars = "USUBJID", label = "Subject")))

## See also

[`tfl_tpl()`](https://ichirio.github.io/tflspec/reference/tfl_tpl.md),
[`tfl_tpl_help()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_help.md),
[`tfl_tpl_args()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_args.md),
[`tfl_templates()`](https://ichirio.github.io/tflspec/reference/tfl_templates.md)

## Examples

``` r
tfl_tpl_help("listing", tier = c("required", "usual"))
#> listing (listing, layout listing; tflspec)
#> Call: tfl_tpl(adae, "listing", ...)
#> 
#> required (2):
#>   param   | effective (layer) | what
#>   data    | - (template)      | the dataset (ADAE)
#>   columns | - (template)      | the columns, in order: each a list of vars (one or more), label, width and collapse_repeats
```
