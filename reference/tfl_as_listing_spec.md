# A listing written in code, as a listing spec

Writes an rtfreporter listing – a `listing_spec()` object, or a
`table_plan()` with `plan_listing()` – as a
[`tfl_listing_spec()`](https://ichirio.github.io/tflspec/reference/tfl_listing_spec.md):
one `listings` row and its `listing_cols`. What the sheets cannot carry
is listed in `attr(, "not_converted")`: a column's `name`, `rel_width`
or `layout`, the listing's spacer, `layout`, `blank_row_first`, an
unnamed `wrap` function, `record = FALSE`, and from a plan any layer
besides the listing and its rows a page. The listing's own default `sep`
/ `align` go to each column that does not set its own.

## Usage

``` r
tfl_as_listing_spec(
  x,
  output_id = "L",
  dataset = NA_character_,
  compare = TRUE
)
```

## Arguments

- x:

  A `listing_spec()` object, or a `table_plan()` with `plan_listing()`.

- output_id:

  The listing's id.

- dataset:

  The dataset it lists (of the data catalog), or `NA`.

- compare:

  For a plan: `TRUE` (default) makes the listing's pages from the spec
  ([`tfl_listing()`](https://ichirio.github.io/tflspec/reference/tfl_listing.md)
  on the plan's data) and compares their RTF, byte by byte, with the
  plan's.

## Value

A
[`tfl_listing_spec()`](https://ichirio.github.io/tflspec/reference/tfl_listing_spec.md)
(not checked: `dataset` may be blank), with attributes `"not_converted"`
and `"same_pages"` (`TRUE` / `FALSE`: the same RTF; `NA` when not
compared).

## Details

The data, its subset and its order are the program's: `dataset` names
the dataset of the data catalog the listing reads, for the `listings`
row (`where` and `sort` stay blank).

## See also

[`tfl_as_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_as_table_spec.md),
the table's.
