# Waterfall: a figure from the template `waterfall`

The A use of a figure's template SPEC: the program makes the records
(the dataset read, the analysis set's subjects kept, the parameter, the
record of each subject the report keeps (`where`), the best overall
response joined when the bars are coloured by it), then one call draws
them: one bar a subject, the largest change first, the lines at 0, +20%
and -30%. The bars' order is made here. A subject with several records
is one bar, its smallest value (the first record of a tie), and a
warning says how many records were left out; give the records you want
with `where` in a report. A bar beyond the axis (-100 to 100) is cut
off, and a warning says how many. A subject with no response
(`by_response`) is a bar in its own colour, Missing in the legend. A
report's own layers and settings go after the call with `+`.

## Usage

``` r
tpl_waterfall(data, value = "PCHG", by_response = FALSE, plot = NULL)
```

## Arguments

- data:

  The records: one a subject.

- value:

  The bars' value (`PCHG`), bare or as text.

- by_response:

  `TRUE`: the bars coloured by `BOR` (the best overall response, a
  column of `data`).

- plot:

  The figure's settings over the template's (`x_label`, `y_by`, `legend`
  ..., as a designed figure's `plot:`).

## Value

A ggplot.

## See also

[`tfl_template_expand()`](https://ichirio.github.io/tflspec/reference/tfl_template_expand.md)
for the design it draws (the B use).

## Examples

``` r
df <- adtr[adtr$AVISIT == "WEEK 12", ]
p <- tpl_waterfall(df, value = PCHG)
#> Warning: waterfall: 1 bars beyond the axis (-100 to 100) are cut off
class(p)
#> [1] "ggplot2::ggplot" "ggplot"          "ggplot2::gg"     "S7_object"      
#> [5] "gg"             
```
