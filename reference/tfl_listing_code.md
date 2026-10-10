# The code that makes a listing from its definition

Writes the part of a listing program that reads the data, subsets,
reworks and sorts it, and lays it out:
`content <- as_rtftables(data, listing = lst)`. The program's setup
([`library(rtfreporter)`](https://github.com/ichirio/rtfreporter)) and
its report are the caller's.

## Usage

``` r
tfl_listing_code(
  spec,
  output_id = NULL,
  datasets,
  rework = NULL,
  type = "multiline",
  codelists = NULL
)
```

## Arguments

- spec:

  A
  [`tfl_listing_spec()`](https://ichirio.github.io/tflspec/reference/tfl_listing_spec.md),
  or the path of its workbook.

- output_id:

  The listing; may be left out when the definition has one.

- datasets:

  The data catalog (see
  [`tfl_read_data_code()`](https://ichirio.github.io/tflspec/reference/tfl_read_data_code.md)).

- rework:

  Code run on `data` before it is sorted, or `NULL`.

- type:

  The listing type when the listing's `type` is blank (one of
  rtfreporter's `listing_spec()` types).

- codelists:

  The reports' code lists (a table definition's `codelists` sheet, or a
  data frame with `output_id`, `variable`, `value`, `label`, `order`):
  the listing's rows of the columns it shows or sorts by are put on its
  data after its condition (`cl_<variable>` and `set_levels()`, see
  [`tfl_helpers_code()`](https://ichirio.github.io/tflspec/reference/tfl_helpers_code.md))
  – each such column a factor in its list's order (it sorts so), its
  values as the list has them.

## Value

The code, one element per line; `NULL` when the listing names no dataset
yet.

## See also

[`tfl_listing()`](https://ichirio.github.io/tflspec/reference/tfl_listing.md)
for the pages themselves.
