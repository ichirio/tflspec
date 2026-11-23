# A listing definition: read, write and check it

A listing is defined in two sheets, keyed by `output_id`:

## Usage

``` r
tfl_listing_spec(listings = NULL, listing_cols = NULL, check = TRUE)

tfl_read_listing_spec(path, output_id = NULL, check = TRUE)

tfl_write_listing_spec(spec, path)
```

## Arguments

- listings:

  The `listings` sheet (a data frame), or a list with `listings` and
  `listing_cols`, or a `tfl_listing_spec`.

- listing_cols:

  The `listing_cols` sheet.

- check:

  `FALSE` keeps a definition still being written (a listing with no
  dataset yet, a column with no variable) without refusing it.

- path:

  The workbook (`.xlsx`).

- output_id:

  Only these listings; `NULL` for all.

- spec:

  A `tfl_listing_spec`.

## Value

A `tfl_listing_spec`: a list of the two sheets, all text.

`tfl_write_listing_spec()`: `path`, invisibly.

## Details

- `listings`, one row a listing: `output_id`; `type` (an rtfreporter
  listing type, blank for the default); `dataset` (of the data catalog);
  `where` (an R condition on its columns); `sort` (variables, `|`
  between them, `-` in front for descending); `max_rows` (rows a page);
  `blank_row` (`TRUE` / `FALSE`: a blank row after each record; blank:
  the type's); `wrap` (the name of an R function that breaks a cell into
  lines, `listing_spec(wrap = )`; blank: the type's own rule).

- `listing_cols`, one row a printed column, in order: `output_id`;
  `vars` (`|` between variables stacked in the column); `label` (the
  header, `\n` for a line break); `width` (characters); `sep` (what
  separates the stacked variables – quote it, `" / "`, to keep its
  spaces; blank: the type's, `/`); `align` (`left` / `center` / `right`;
  blank: the type's); `collapse_repeats` (`TRUE` prints a value once
  until it changes).

`tfl_listing_spec()` makes the definition from those two data frames (or
a list holding them, as a GUI keeps them; other elements are ignored)
and checks it. `tfl_read_listing_spec()` reads it from a workbook –
other sheets, such as tflplanner's `figures`, are ignored – and
`tfl_write_listing_spec()` writes it;
`tfl_write_listing_spec(tfl_listing_spec(), path)` gives an empty
workbook to fill in.

## See also

[`tfl_listing_code()`](https://ichirio.github.io/tflspec/reference/tfl_listing_code.md)
(the program),
[`tfl_listing()`](https://ichirio.github.io/tflspec/reference/tfl_listing.md)
(the pages).

## Examples

``` r
spec <- tfl_listing_spec(
  listings = data.frame(output_id = "L-16-2-7", dataset = "ADAE",
                        where = "AESEV == 'SEVERE'",
                        sort = "USUBJID | -ASTDY", max_rows = "20"),
  listing_cols = data.frame(
    output_id = "L-16-2-7",
    vars  = c("USUBJID", "AEDECOD | AESEV", "ASTDY"),
    label = c("Subject", "Preferred term / Severity", "Study day"),
    width = c("12", "30", "8"),
    collapse_repeats = c("TRUE", NA, NA)))
spec
#> <tfl_listing_spec> 1 listing
#>   L-16-2-7     ADAE where AESEV == 'SEVERE', 3 columns, 20 rows a page
```
