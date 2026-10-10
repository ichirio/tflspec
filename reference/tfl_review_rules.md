# The rules of the review

The catalog of what
[`tfl_review_spec()`](https://ichirio.github.io/tflspec/reference/tfl_review_spec.md)
checks: each rule's id, its level (`error`: the definition cannot be
used; `check`: valid, probably wrong; `hand`: something the rules
require is missing, to be set by hand), its area, what it needs (`spec`,
`catalog`, `ard`: what the report's ARD holds, `data`: the facts of the
data), which package runs it (`tflspec`, or `tflplanner` for what needs
a study folder: the report list, the files), the message (a template:
`%s` filled in) and a hint. A rule that is not in the catalog does not
exist.

## Usage

``` r
tfl_review_rules()
```

## Value

A data frame: `rule`, `level`, `area`, `needs`, `checked_by`, `message`,
`hint`.

## See also

[`tfl_review_spec()`](https://ichirio.github.io/tflspec/reference/tfl_review_spec.md)

## Examples

``` r
r <- tfl_review_rules()
r[r$checked_by == "tflspec", c("rule", "level", "area", "needs")]
#>      rule level      area   needs
#> 1     S01 error      spec    spec
#> 10    C01 check codelists    spec
#> 11    C02 check codelists    data
#> 12    C03 error codelists    data
#> 14    A01 error       ard    data
#> 15    A02 error       ard    data
#> 16    A03 check       ard    data
#> 17    A04 check       ard    data
#> 18    A05 check       ard    data
#> 19    A06 check       ard    spec
#> 20    A08 check       ard catalog
#> 24    A15 error       ard    spec
#> 25    A16 check       ard    data
#> 26    A17 check   ard_run     ard
#> 27    T01 check     table    spec
#> 28    T02 check     table    spec
#> 29    T03 check     table catalog
#> 30    T04 check     table    spec
#> 31    T05 check     table    spec
#> 32    T06 check     table     ard
#> 33    T07 check     table     ard
#> 34    T08 check     table     ard
#> 35    T09  hand     table    spec
#> 36    T10  hand     table    spec
#> 37    L01  hand   listing    spec
#> 38    L02 error   listing    data
#> 39    F01  hand    figure    spec
#> 40    F02 check    figure    spec
#> 41    F03 error    figure    data
#> 45    C05 check codelists    data
#> 47 review check      spec    spec
```
