# What the ARD functions of one's own in some R files are

Reads the files – it does not run them – and returns one row per
function an analysis row could name as its method: a top-level
`name <- function(...)`, or
`name <- cards::as_cards_fn(function(...), stat_names = c(...))` as the
templates of
[`tfl_ard_function_template()`](https://ichirio.github.io/tflspec/reference/tfl_ard_function_template.md)
write it. What a person reads to choose one comes from the roxygen block
above it: the title (its first line), the description (the paragraph
after), and each argument's `@param`. The statistics are the ones
`as_cards_fn()` declares. Test files (`test-*.R`) are skipped.

## Usage

``` r
tfl_ard_function_info(files)
```

## Arguments

- files:

  R files: a study's own functions (its key `source`), a company's (a
  standards folder), or both.

## Value

A data frame: `name`, `file`, `title`, `description`, `stat_names` (`|`
between them; empty when the function declares none), and `args`, a list
column of data frames (`arg`, `default`, `hint`). A file that does not
parse is a row with `name` `NA` and the parse error as its
`description`.

## See also

[`tfl_ard_function_template()`](https://ichirio.github.io/tflspec/reference/tfl_ard_function_template.md),
[`tfl_check_ard_function()`](https://ichirio.github.io/tflspec/reference/tfl_check_ard_function.md)

## Examples

``` r
f <- file.path(tempdir(), "ard_mine.R")
tfl_ard_function_template("ard_mine", "test", file = f, overwrite = TRUE)
info <- tfl_ard_function_info(f)
info[, c("name", "title", "stat_names")]
#>       name                    title          stat_names
#> 1 ard_mine A test across two groups statistic | p.value
info$args[[1]]
#>         arg default                                                hint
#> 1      data    <NA>   The analysis data (its dataset and analysis set).
#> 2        by    <NA>                       The group column: two groups.
#> 3 variables    <NA>                              The numeric variables.
#> 4       ...    <NA> Passed to stats::wilcox.test(), e.g. exact = FALSE.
```
