# A listing's pages from its definition

What the program
[`tfl_listing_code()`](https://ichirio.github.io/tflspec/reference/tfl_listing_code.md)
writes does, done on data in hand: keep the rows `where` says, sort
them, print dates as text, and lay them out with rtfreporter's
`listing_spec()` / `as_rtftables()`. The pages go to
[`tfl_report()`](https://ichirio.github.io/tflspec/reference/tfl_report.md)
or `rtf_tables()` like a table's.

## Usage

``` r
tfl_listing(data, spec, output_id = NULL, type = "multiline")
```

## Arguments

- data:

  The listing's dataset (already read, and reworked if it needs to be).

- spec:

  A
  [`tfl_listing_spec()`](https://ichirio.github.io/tflspec/reference/tfl_listing_spec.md),
  or the path of its workbook.

- output_id:

  The listing; may be left out when the definition has one.

- type:

  The listing type when the listing's `type` is blank (one of
  rtfreporter's `listing_spec()` types).

## Value

A list of `rtftable` pages.

## Details

The data comes first, as in
[`tfl_table_plan()`](https://ichirio.github.io/tflspec/reference/tfl_table_plan.md),
so a listing can be piped from its data. (Before 0.0.24.9015 the spec
came first; a call in that order stops, saying so.)

## Examples

``` r
spec <- tfl_listing_spec(
  listings = data.frame(output_id = "L-1", dataset = "ADSL",
                        sort = "-AGE", max_rows = "10"),
  listing_cols = data.frame(output_id = "L-1",
                            vars = c("USUBJID", "AGE | SEX"),
                            label = c("Subject", "Age / Sex")))
adsl <- data.frame(USUBJID = sprintf("S-%02d", 1:12),
                   AGE = 40 + 1:12, SEX = rep(c("F", "M"), 6))
pages <- tfl_listing(adsl, spec)
length(pages)
#> [1] 3
```
