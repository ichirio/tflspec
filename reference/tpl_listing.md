# A listing: a listing plan from the layout `listing`

One row a record of a dataset: the records kept (`where`), their order
(`sort`) and the columns, each its variables (one or more, stacked in
the cell), its heading, its width and whether a repeated value is
printed once. The look is the template's: one record over lines as
needed (`multiline`) and 22 rows a page, which a study's style or a
report's own item changes.

## Usage

``` r
tpl_listing(x, ...)
```

## Arguments

- x:

  The dataset (a data frame), the listing's source.

- ...:

  The template's parameters: `columns` (a list of columns, each
  `list(vars = , label = , width = , collapse_repeats = )`), `where`,
  `sort`; `data` names the dataset in the spec (`"ADAE"`).

## Value

A
[`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html)
of a listing.
