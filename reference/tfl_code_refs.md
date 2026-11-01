# The names a SPEC's R code uses

Which of `names` each piece of R code reads – a `where`, a `derive`, an
analysis's `args` or `code`, an `!r` value. For a screen that renames a
data or an object: the code that still says the old name is found, to
warn of it (the code is not changed). The code is parsed and its symbols
read, so a name inside a string or a comment is not one; code that does
not parse is searched for the names as whole words.

## Usage

``` r
tfl_code_refs(code, names)
```

## Arguments

- code:

  A character vector of R code (or a list of
  [`tfl_r()`](https://ichirio.github.io/tflspec/reference/tfl_r.md)
  values).

- names:

  The names to look for (data, objects).

## Value

A data frame, one row a use: `code` (the element's position in `code`,
or its name when `code` is named) and `name`. No rows when none is used.

## Examples

``` r
tfl_code_refs(c(where = "AGE >= 65 & USUBJID %in% adsl_sub$USUBJID",
                derive = "n_jp / 2"),
              c("adsl_sub", "n_jp", "adae_sub"))
#>     code     name
#> 1  where adsl_sub
#> 2 derive     n_jp
```
