# Adopt conventions learned from past TFLs

The company standard's conventions with the learned ones a person chose
from
[`tfl_ai_style_diff()`](https://ichirio.github.io/tflspec/reference/tfl_ai_style_diff.md):
each chosen row's learned value takes the standard's place (or is
added), the rest stay as they were. What becomes of the result – the
company standard's store – is the app's.

## Usage

``` r
tfl_ai_style_adopt(diff, rows = NULL, standard = NULL)
```

## Arguments

- diff:

  What
  [`tfl_ai_style_diff()`](https://ichirio.github.io/tflspec/reference/tfl_ai_style_diff.md)
  returns.

- rows:

  The rows of `diff` to adopt: numbers, or a logical vector; `NULL`
  (default) every row whose `adopt` is `"proposed"` and whose `status`
  is `"differs"` or `"new"`.

- standard:

  The company standard's conventions (`kind`, `key`, `value`), as given
  to
  [`tfl_ai_style_diff()`](https://ichirio.github.io/tflspec/reference/tfl_ai_style_diff.md).

## Value

The conventions: a data frame with `kind`, `key`, `value`, `from`
(`"standard"`, or `"learned"` for one adopted now), ordered by kind and
key.

## See also

[`tfl_ai_reference_build()`](https://ichirio.github.io/tflspec/reference/tfl_ai_reference_build.md)

## Examples

``` r
f <- tempfile(fileext = ".yml")
writeLines(c("tfl_company_hints: {version: 1, files: 3, tables: 9}",
             "kinds:",
             "  demog:",
             "    n_form: {value: \"(N=xx)\", seen: 3, of: 3}"), f)
std <- data.frame(kind = "all", key = "n_form", value = "N = xx")
tfl_ai_style_adopt(tfl_ai_style_diff(f, std), standard = std)
#>    kind    key  value     from
#> 1   all n_form N = xx standard
#> 2 demog n_form (N=xx)  learned
```
