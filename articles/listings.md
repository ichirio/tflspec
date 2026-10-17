# Listings

A listing is two sheets keyed by `output_id`: `listings`, one row a
listing (its dataset, the records it keeps, the order, rows a page), and
`listing_cols`, one row a printed column.
[`tfl_listing_code()`](https://ichirio.github.io/tflspec/reference/tfl_listing_code.md)
writes the program – read the data, keep, sort, lay out – and
[`tfl_listing()`](https://ichirio.github.io/tflspec/reference/tfl_listing.md)
makes the pages from data in hand. The layout is rtfreporter’s
[`listing_spec()`](https://ichirio.github.io/rtfreporter/reference/listing_spec.html)
/
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.html).

``` r

library(tflspec)
library(rtfreporter)
```

## The sheets

| Sheet | Columns |
|----|----|
| `listings` | `output_id`, `type` (an rtfreporter listing type; blank: `multiline`), `dataset`, `where` (R), `sort` (`-` for descending), `max_rows`, `blank_row`, `wrap` |
| `listing_cols` | `output_id`, `vars` (`|` stacks variables in one column), `label`, `width` (characters), `sep`, `align`, `collapse_repeats` |

``` r

spec <- tfl_listing_spec(
  listings = data.frame(output_id = "L-16-2-7", dataset = "ADAE",
                        where = 'AESER == "Y"',
                        sort = "USUBJID | ASTDY", max_rows = "15"),
  listing_cols = data.frame(
    output_id = "L-16-2-7",
    vars  = c("USUBJID", "TRT01A", "AEBODSYS | AEDECOD", "ASTDY", "AESEV"),
    label = c("Subject", "Treatment", "System organ class /\nPreferred term",
              "Study day", "Severity"),
    width = c("14", "22", "40", "8", "10"),
    collapse_repeats = c("TRUE", "TRUE", NA, NA, NA)))
```

A workbook is read with
[`tfl_read_listing_spec()`](https://ichirio.github.io/tflspec/reference/tfl_listing_spec.md)
and written with
[`tfl_write_listing_spec()`](https://ichirio.github.io/tflspec/reference/tfl_listing_spec.md);
`tfl_write_listing_spec(tfl_listing_spec(), path)` gives an empty one to
fill in.

## The program

The data catalog – the `datasets` sheet of the ARD spec: each dataset’s
`path` and derived columns – says how the data is read, so the listing’s
program reads it the same way the ARD program does:

``` r

catalog <- data.frame(dataset = "ADAE", path = "data/adae.rds")
cat(tfl_listing_code(spec, "L-16-2-7", datasets = catalog), sep = "\n")
# the data: ADAE (data catalog)
adae <- readRDS("data/adae.rds")
# its records, in its order; dates as they print
data <- adae |>
  dplyr::filter(AESER == "Y") |>
  dplyr::arrange(USUBJID, ASTDY) |>
  dplyr::mutate(
    dplyr::across(dplyr::where(~ inherits(.x, c("Date", "POSIXt"))), format)
  )

lst <- listing_spec(
  list(
    listing_col("USUBJID", width = 14, label = "Subject", collapse_repeats = TRUE),
    listing_col(
      "TRT01A",
      width = 22,
      label = "Treatment",
      collapse_repeats = TRUE
    ),
    listing_col(
      c("AEBODSYS", "AEDECOD"),
      width = 40,
      label = "System organ class /\nPreferred term"
    ),
    listing_col("ASTDY", width = 8, label = "Study day"),
    listing_col("AESEV", width = 10, label = "Severity")
  ),
  type = "multiline"
)
content <- as_rtftables(data, listing = lst, max_rows = 15)
```

What the sheets cannot say about the data – a derived column, a join –
is `rework`: code run on `data` before it is sorted, written into the
program by the spec, not added afterwards by hand:

``` r

cat(tfl_listing_code(spec, "L-16-2-7", datasets = catalog,
                     rework = "data$AESEV <- tools::toTitleCase(tolower(data$AESEV))"),
    sep = "\n")
# the data: ADAE (data catalog)
adae <- readRDS("data/adae.rds")
data <- dplyr::filter(adae, AESER == "Y")

# rework
data$AESEV <- tools::toTitleCase(tolower(data$AESEV))

# in its order; dates as they print
data <- data |>
  dplyr::arrange(USUBJID, ASTDY) |>
  dplyr::mutate(
    dplyr::across(dplyr::where(~ inherits(.x, c("Date", "POSIXt"))), format)
  )

lst <- listing_spec(
  list(
    listing_col("USUBJID", width = 14, label = "Subject", collapse_repeats = TRUE),
    listing_col(
      "TRT01A",
      width = 22,
      label = "Treatment",
      collapse_repeats = TRUE
    ),
    listing_col(
      c("AEBODSYS", "AEDECOD"),
      width = 40,
      label = "System organ class /\nPreferred term"
    ),
    listing_col("ASTDY", width = 8, label = "Study day"),
    listing_col("AESEV", width = 10, label = "Severity")
  ),
  type = "multiline"
)
content <- as_rtftables(data, listing = lst, max_rows = 15)
```

The report around it is the report spec’s, as for a table:
`tfl_report_code(report_spec, "L-16-2-7", content = "content")`. In a
study whose report programs source the setup script
([`tfl_report_setup_code()`](https://ichirio.github.io/tflspec/reference/tfl_report_setup_code.md):
the company’s font, the study’s tokens, the running header and footer),
the listing’s program is written with `setup = TRUE` and says only its
own values, as a table’s does (see
[Reports](https://ichirio.github.io/tflspec/articles/reports.md)).

## The pages

[`tfl_listing()`](https://ichirio.github.io/tflspec/reference/tfl_listing.md)
does what the program does, on data in hand. The data comes first, so it
pipes:

``` r

pages <- adae |> tfl_listing(spec, "L-16-2-7")   # the sample study's ADAE
length(pages)
#> [1] 16
class(pages[[1]])
#> [1] "rtftable"
```

The pages go to `tfl_report(report_spec, content = pages)` or
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.html).

## A listing written in code, back to a spec

[`tfl_as_listing_spec()`](https://ichirio.github.io/tflspec/reference/tfl_as_listing_spec.md)
writes a listing coded with rtfreporter
([`listing_spec()`](https://ichirio.github.io/rtfreporter/reference/listing_spec.html),
or a
[`plan_listing()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.html)
plan) back as a listing spec, listing what the sheets cannot carry
(`attr(, "not_converted")`):

``` r

lst <- listing_spec(list(
  listing_col("USUBJID", label = "Subject", width = 14),
  listing_col(c("AEDECOD", "AESEV"), label = "Term / Severity", width = 30)))
back <- suppressMessages(tfl_as_listing_spec(lst, "L-16-2-8", dataset = "ADAE"))
back$listing_cols[, c("vars", "label", "width")]
#>              vars           label width
#> 1         USUBJID         Subject    14
#> 2 AEDECOD | AESEV Term / Severity    30
```
