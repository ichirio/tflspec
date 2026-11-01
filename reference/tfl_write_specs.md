# Write several specs to one workbook

The specs are written each with its sheets, as their own writers would,
into one workbook: one `study` sheet with every key the specs read, then
the ARD sheets, the table sheets, the report sheets and the listing
sheets, and `about`. Each reader
([`tfl_read_ard_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_ard_spec.md),
[`tfl_read_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_table_spec.md),
[`tfl_read_report_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_report_spec.md),
[`tfl_read_listing_spec()`](https://ichirio.github.io/tflspec/reference/tfl_listing_spec.md))
takes its own sheets back from it. Writing each spec to its own workbook
(the default of every writer) is easier to read; one workbook is for
handing a study's specs over as one file.

## Usage

``` r
tfl_write_specs(path, ard = NULL, table = NULL, report = NULL, listing = NULL)
```

## Arguments

- path:

  Destination `.xlsx`.

- ard:

  An ARD spec
  ([`tfl_ard_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_ard_spec.md)),
  or `NULL`.

- table:

  A table spec
  ([`tfl_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_table_spec.md)):
  its table sheets, or `NULL`.

- report:

  A report spec (a
  [`tfl_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_table_spec.md),
  as
  [`tfl_read_report_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_report_spec.md)
  reads it): its report sheets, or `NULL`. The same object as `table`
  may be given for both.

- listing:

  A listing spec
  ([`tfl_listing_spec()`](https://ichirio.github.io/tflspec/reference/tfl_listing_spec.md)),
  or `NULL`.

## Value

`path`, invisibly.

## See also

[`tfl_write_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_write_table_spec.md),
[`tfl_write_ard_spec()`](https://ichirio.github.io/tflspec/reference/tfl_write_ard_spec.md),
[`tfl_write_listing_spec()`](https://ichirio.github.io/tflspec/reference/tfl_listing_spec.md)
