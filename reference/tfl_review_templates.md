# The sentences of the review beyond its rules

A row of
[`tfl_review_spec()`](https://ichirio.github.io/tflspec/reference/tfl_review_spec.md)
carries its sentence in `template` and the values that fill it in `args`
(`message` is the two put together). For most rows the template is the
rule's message
([`tfl_review_rules()`](https://ichirio.github.io/tflspec/reference/tfl_review_rules.md));
the rules whose message is the whole sentence (`%s`) take one of these
when the check knows its words: a table against its ARD (T06, T07), a
listing's columns (L01, L02), a figure against the data (F03). A
figure's advice (F02) carries
[`tfl_fig_advice()`](https://ichirio.github.io/tflspec/reference/tfl_fig_advice.md)'s
own template. An app translates a template once and puts the values in.

## Usage

``` r
tfl_review_templates()
```

## Value

A data frame: `rule`, `name`, `template`.

## See also

[`tfl_review_spec()`](https://ichirio.github.io/tflspec/reference/tfl_review_spec.md),
[`tfl_review_rules()`](https://ichirio.github.io/tflspec/reference/tfl_review_rules.md)

## Examples

``` r
tfl_review_templates()
#>    rule            name
#> 1   T07         ard_key
#> 2   T07   ard_cells_var
#> 3   T06        ard_stat
#> 4   L01    listing_cols
#> 5   L01    listing_vars
#> 6   L02 listing_dataset
#> 7   L02    listing_sort
#> 8   L02     listing_col
#> 9   F03     f03_dataset
#> 10  F03    f03_variable
#>                                                                      template
#> 1  the table's %s name %s, which is neither a group nor a variable of the ARD
#> 2                the cells are written for %s, which the ARD does not analyse
#> 3                    a template reads {%s}, a statistic the ARD does not have
#> 4                                    listing %s: no columns in `listing_cols`
#> 5                                            listing %s, column %s: no `vars`
#> 6           The listing %s reads the dataset %s, which is not in the catalog.
#> 7                         The listing %s sorts by %s, which %s does not have.
#> 8               Column %s of the listing %s shows %s, which %s does not have.
#> 9                                                         %s %s no dataset %s
#> 10                                                 %s %s no variable %s in %s
```
