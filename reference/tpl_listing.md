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
  `sort`, `by` (the variables that divide the listing), `by_break`
  (`"page"` or `"block"`) and `by_heading` (a page's name: `"{value}"`
  is its value, with several `by` variables `"{VARIABLE}"` for each);
  `data` names the dataset in the spec (`"ADAE"`).

## Value

A
[`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html)
of a listing.

## Details

`by` divides the listing by the values of variables. `by_break` `"page"`
(the default) starts a page for each value
([`rtfreporter::plan_paginate_rows()`](https://ichirio.github.io/rtfreporter/reference/plan_paginate_rows.html)'s
`page_by`), 22 rows a page within it; `"block"` keeps one variable's
records together on a page where they fit
([`rtfreporter::plan_row_group()`](https://ichirio.github.io/rtfreporter/reference/plan_row_group.html)'s
`group_col`), the value printed once at the top of its block, and again
at the top of a page that continues it. A `by` variable is not printed
as a column unless `columns` has a column of just that variable (for
`"block"` that column prints the value once a block; one not printed
keeps the block together without marking it). A page whose variable is
not printed is named as the last title line, `"AESEV: MODERATE"`, or by
`by_heading`. The records are ordered by the `by` variables first, then
by `sort`; the data of `tpl_listing()` is the program's own, in its
order, so it is ordered by the program (a value that comes back later is
one page, a block is a run of rows).
