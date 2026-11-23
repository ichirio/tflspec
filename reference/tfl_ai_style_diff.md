# Set what an agent learned from past TFLs against the company standard

Lines up each convention
[`tfl_ai_style_read()`](https://ichirio.github.io/tflspec/reference/tfl_ai_style_read.md)
read with the company standard's for the same kind of report and name,
so that a person can adopt them a line at a time. A standard's value for
kind `all` stands for every kind that has none of its own.

## Usage

``` r
tfl_ai_style_diff(learned, standard = NULL)
```

## Arguments

- learned:

  What
  [`tfl_ai_style_read()`](https://ichirio.github.io/tflspec/reference/tfl_ai_style_read.md)
  returns, or the agent's file.

- standard:

  The company standard's conventions: a data frame with `kind`, `key`
  and `value` (one row a convention; kind `all` for every kind), or
  `NULL` for none.

## Value

A data frame, one row a convention either side has: `kind`, `key`,
`standard`, `learned`, `seen`, `of`, `adopt` (the learned one's, see
[`tfl_ai_style_read()`](https://ichirio.github.io/tflspec/reference/tfl_ai_style_read.md)),
and `status`: `"same"`, `"differs"`, `"new"` (the standard has none) or
`"not learned"` (the agent found none). Values are compared with their
spaces trimmed. Ordered by kind (as
[`tfl_ai_style_prompt()`](https://ichirio.github.io/tflspec/reference/tfl_ai_style_prompt.md)
lists them) and then as learned.

## See also

[`tfl_ai_style_prompt()`](https://ichirio.github.io/tflspec/reference/tfl_ai_style_prompt.md)

## Examples

``` r
f <- tempfile(fileext = ".yml")
writeLines(c("tfl_company_hints: {version: 1, files: 3, tables: 9}",
             "kinds:",
             "  demog:",
             "    n_form: {value: \"(N=xx)\", seen: 3, of: 3}",
             "    zero: {value: \"0\", seen: 1, of: 3}"), f)
std <- data.frame(kind = c("all", "demog"), key = c("n_form", "categorical"),
                  value = c("N = xx", "xx (xx.x%)"))
tfl_ai_style_diff(f, std)
#>    kind         key   standard learned seen of    adopt      status
#> 1 demog      n_form     N = xx  (N=xx)    3  3 proposed     differs
#> 2 demog        zero       <NA>       0    1  3  by hand         new
#> 3 demog categorical xx (xx.x%)    <NA>   NA NA     <NA> not learned
```
