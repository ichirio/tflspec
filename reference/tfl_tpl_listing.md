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
