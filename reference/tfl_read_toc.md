# Read a table of contents (TOC) as report specs

Reads a study's list of outputs from a workbook or a `.csv` in the
company's own layout, through `map` – which of its columns is what – and
gives the report sheets of a table spec: `report` (`output_id`, `type`,
`program`, `file`, `note`), `titles` and `footnotes`. The result goes to
[`tfl_write_specs()`](https://ichirio.github.io/tflspec/reference/tfl_write_specs.md)
/
[`tfl_write_report_spec()`](https://ichirio.github.io/tflspec/reference/tfl_write_table_spec.md)
or into tflplanner. Nothing else (the table, the ARD) is made.

## Usage

``` r
tfl_read_toc(path, map, sheet = NULL, skip = 0L, type_from_id = TRUE)
```

## Arguments

- path:

  An `.xlsx` or `.csv` file.

- map:

  Which column is what: a named character vector or list, the names
  among `output_id` (required), `type`, `title`, `population`,
  `footnote`, `program`, `file`, `note`, `section`, `datasets`, `label`,
  each the TOC's column name (or names, for `title` and `footnote`).
  Names are matched ignoring case and surrounding blanks.

- sheet:

  The sheet of a workbook (name or number); `NULL` for the first.

- skip:

  Rows above the header row.

- type_from_id:

  Guess a missing or unreadable kind from the output id or the first
  title.

## Value

A table spec
([`tfl_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_table_spec.md))
holding the report sheets, with attributes `guessed` and `skipped`.

## Details

- `title` and `footnote` may each be one column or several (in order). A
  cell holding several lines – a line break, or `" | "` between them –
  gives one line each.

- `population` (e.g. "Safety Population") becomes the last title line.

- The kind: `type` read loosely (`Table`, `tbl`, `T`, `Figure`, `Fig`,
  `Listing`, `Lst` ...); else, with `type_from_id`, from the start of
  the output id (`T-14-1-1`, `F14.2`, `L-16-2-7`, `Table 14.1.1`) or of
  the first title ("Figure 14.2.1 ..."); else `table`. Each report whose
  kind was guessed is listed in `attr(, "guessed")`.

- A row with no output id that says nothing else, or one thing only (a
  section heading such as "14.1 Demographics"), is passed over and
  listed in `attr(, "skipped")`; a row with no output id that has more
  is an error, as is an output id given twice.

- For a report list's tokens: `attr(, "labels")` (the map's `label`, the
  report's ID as printed, "Table 14.1.1"), `attr(, "first_titles")` and
  `attr(, "populations")`, each named by output id, `NA` for none.

- Each report's section is in `attr(, "sections")` (named by output id;
  `NA` for none): its `section` column when the map names one, else the
  heading row above it (the text of the last row passed over as a
  heading). It is not part of the spec: what keeps a report list (an
  app) may keep it.

- Each report's datasets are in `attr(, "datasets")` (named by output
  id; `NA` for none), from its `datasets` column: "ADSL, ADAE", "ADSL /
  ADAE" or one a line become `"ADSL | ADAE"`. Not part of the spec
  either.

## Examples

``` r
toc <- tempfile(fileext = ".csv")
writeLines(c("No.,Kind,Title,Population,Footnotes",
             "T-14-1-1,Table,Demographics,Safety Population,N: subjects",
             "F-14-2-1,,Mean SBP by visit | Mean (SE),Safety Population,"),
           toc)
sp <- tfl_read_toc(toc, map = c(output_id = "No.", type = "Kind",
                                title = "Title", population = "Population",
                                footnote = "Footnotes"))
sp$report[c("output_id", "type")]
#>   output_id   type
#> 1  T-14-1-1  table
#> 2  F-14-2-1 figure
sp$titles
#>   output_id line left            center right
#> 1  T-14-1-1    1 <NA>      Demographics  <NA>
#> 2  T-14-1-1    2 <NA> Safety Population  <NA>
#> 3  F-14-2-1    1 <NA> Mean SBP by visit  <NA>
#> 4  F-14-2-1    2 <NA>         Mean (SE)  <NA>
#> 5  F-14-2-1    3 <NA> Safety Population  <NA>
```
