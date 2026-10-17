# Check one spec item against the schema

One item of a spec's `content:`
([`tfl_item()`](https://ichirio.github.io/tflspec/reference/tfl_r.md)):
its function is one the schema or its package knows; each argument is
the function's (its formals, or the grammar of its `...`); each value
has the argument's type. What the reader of a spec file calls on every
item, and what the AI review reports.

## Usage

``` r
tfl_schema_check(item)
```

## Arguments

- item:

  A [`tfl_item()`](https://ichirio.github.io/tflspec/reference/tfl_r.md)
  (or a list with `fn` and `args`).

## Value

The problems: a data frame (`fn`, `arg`, `level` – `"error"` or
`"warning"` – and `message`), no rows when the item is fine.

## Details

An argument the function does not have is a **warning**, not an error:
the last word is the installed package's formals when the code runs (a
newer package may have it). An argument the schema marks `renamed_to`
says what it is now and since which version, in the words the GUI's
suggestion and the AI review also use.

## See also

[`tfl_schema()`](https://ichirio.github.io/tflspec/reference/tfl_schema.md)

## Examples

``` r
tfl_schema_check(tfl_item("plan_sort", list(keys = c("-n", "SOC"))))
#> [1] fn      arg     level   message
#> <0 rows> (or 0-length row.names)
tfl_schema_check(tfl_item("plan_sort", list(keys = c("-n"), sort = 1)))
#>          fn  arg   level
#> 1 plan_sort sort warning
#>                                                       message
#> 1 plan_sort has no argument `sort` (it has: stat, keep, keys)
```
