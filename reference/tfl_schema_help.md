# The help of a spec item's argument

What the GUI's `?` and the AI kit say about one argument of a spec item:
the schema's description of it, or, for an argument a spec sheet's
column also holds (`rtf_page(paper_size = )` is the `page` sheet's
`paper_size`), that column's help – one text, wherever it is read.

## Usage

``` r
tfl_schema_help(fn, arg)
```

## Arguments

- fn:

  A function of the spec (`"rtf_page"`, `"plan_cells"`).

- arg:

  One of its arguments.

## Value

The text, or `NA` when the schema says nothing of it.

## See also

[`tfl_schema()`](https://ichirio.github.io/tflspec/reference/tfl_schema.md),
[`tfl_spec_columns()`](https://ichirio.github.io/tflspec/reference/tfl_spec_columns.md)

## Examples

``` r
tfl_schema_help("rtf_page", "orientation")
#> [1] "landscape / portrait; blank: landscape."
```
