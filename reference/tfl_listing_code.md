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
  datasets = NULL,
  rework = NULL,
  type = "multiline",
  codelists = NULL,
  part = c("all", "data", "content"),
  ard = NULL
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
  [`tfl_read_data_code()`](https://ichirio.github.io/tflspec/reference/tfl_read_data_code.md));
  `NULL`: `ard`'s.

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

- part:

  `"all"` (the whole of it), `"data"` (the data, up to its order: what a
  report's content step of its own code reads as `data`) or `"content"`
  (the listing laid out from a `data` made before – by a report's data
  step of its own code: its code lists, its order, its columns).

- ard:

  The ARD definition
  ([`tfl_ard_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_ard_spec.md)),
  for a listing that reads an analysis data (`data`, a data_id of its
  report's `analysis_data`) or keeps the subjects of an analysis set
  (`population_id`): the program starts with their part of the ARD
  program
  ([`tfl_ard_code()`](https://ichirio.github.io/tflspec/reference/tfl_ard_code.md)'s
  `part = "data"`), and lists the data it makes.

## Value

The code, one element per line; `NULL` when the listing names no dataset
yet (`part = "content"`: no columns yet).

## See also

[`tfl_listing()`](https://ichirio.github.io/tflspec/reference/tfl_listing.md)
for the pages themselves.
