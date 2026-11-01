# Test templates: no parameter is the look, each has its type

Runs each template's body with its example, and again with one parameter
at a time changed, and looks which items change with it: a parameter
that changes the look (the rows a page, column widths, the digits, the
blank rows, the style, a listing's type) is listed. So is a parameter
with no type
([`tfl_param()`](https://ichirio.github.io/tflspec/reference/tfl_param.md)'s
`type`, nor tflspec's for its name): an input form guesses its field
from the default; and the analysis set or the treatment not read from
the study standard (no `study =`). A study's value of them is the
study's, not each report's. So is a template of data of its own without
`subset`, and one of records data without `where`
([`tfl_templates()`](https://ichirio.github.io/tflspec/reference/tfl_templates.md):
the standard pair). tflspec's own templates have none; a company's
package tests its own with it (one line in its tests,
[`tfl_use_company_templates()`](https://ichirio.github.io/tflspec/reference/tfl_use_company_templates.md)).

## Usage

``` r
tfl_test_templates(names = NULL)
```

## Arguments

- names:

  The templates; `NULL`: every one registered.

## Value

A data frame, one row a parameter that ends in the look (`status`
`"look"`: `param`, and the verb `fn` and argument `arg` it ends in), a
parameter with no type (`"no type (guessed: <type>)"`), one with no
study source (`"no study source (population / treatment)"`), a template
without the standard pair (`"no subset"`,
`"no where (a records template)"`) or a template whose run stopped
(`"not checked: ..."`): `template`, `param`, `fn`, `arg`, `status`. No
rows: none.

## Examples

``` r
tfl_test_templates("demog")
#> [1] template param    fn       arg      status  
#> <0 rows> (or 0-length row.names)
```
