# Write the default column widths into the reports that have none

A report says its column widths: a table's `plan_columns(widths = )`,
one width a printed column by name, a listing's `listing_col(width = )`.
For each report of the study that does not, this builds it (its ARD, its
plan) and writes tflspec's default widths into its file, as numbers to
change: each value column 60 / (n + 1) % of the page (at least 12
characters), the row headings the rest – one value column 70:30, two
60:20:20, three 55:15:15:15; when that leaves the row headings less than
their content (at most 30 characters), value columns of 8 characters (a
cell on two lines), unless that leaves the row headings less than two
value columns (16 characters): then all columns equal and the row
headings two; a listing's columns get a width from their variables and
headers (the first guess; see below). Each report it writes widths into
gets an assumption in `spec/_assumptions.yml` (item `widths`): the
widths are the default ones, set for now. Its program
([`tfl_report_code()`](https://ichirio.github.io/tflspec/reference/tfl_report_code.md))
then writes them too.

## Usage

``` r
tfl_fill_widths(study_dir = ".", output_id = NULL, assumption = TRUE)
```

## Arguments

- study_dir:

  The study folder.

- output_id:

  The reports, by id; `NULL` (default): every report in `spec/outputs/`.

- assumption:

  Add the assumption for each report written (default `TRUE`).

## Value

The ids of the reports given widths, invisibly.

## Details

A report that says its widths is left as it is. A table that cannot be
built (its data missing) is left too, and named in a message.

A listing (code, or the `listing` template) whose data can be read gets
its widths from
[`rtfreporter::fit_listing_widths()`](https://ichirio.github.io/rtfreporter/reference/fit_listing_widths.html)
instead: the records as its program reads them (its `where`, its
subjects), the report's page and font, written as whole numbers
(`listing_col(width = )`; for the template, the `width` of each entry of
`template: args: columns:`), and its assumption's value is
`fitted widths` (else `default widths`). A column that has a width is
never changed; the others are fitted around it. Where the data cannot be
read (no ADaM yet, a column the data lacks, an older rtfreporter,
columns too many for the page), a code listing gets the first guess, as
above, and the template is left as it is. Reading the data never makes
the function fail.

## See also

[`tfl_ai_collect_answers()`](https://ichirio.github.io/tflspec/reference/tfl_ai_collect_answers.md),
which does the same for the AI's reports.

## Examples

``` r
# \donttest{
d <- file.path(tempdir(), "fill-widths")
dir.create(file.path(d, "adam"), recursive = TRUE, showWarnings = FALSE)
saveRDS(tflspec::adsl, file.path(d, "adam", "adsl.rds"))
invisible(tfl_ai_study_init(d, adam = "adam", study_id = "ONCO-01", overwrite = TRUE))
o <- tfl_output("T-1", "table", population = "SAF")
o$extends <- list("defaults")
o$ard <- list(analyses = list(list(id = "AGE", method = "continuous", dataset = "ADSL",
                                   population = "SAF", by = list("TRT01A"),
                                   variables = list("AGE"))))
o$data <- list(source = "ard")
o$content <- list(tfl_item("table_plan", list(cols = "TRT01A",
                                             rows = list(group = "variable"))),
                  tfl_item("plan_cells", list(continuous = list(
                    n = "{N:.0f}", "Mean (SD)" = "{mean:.1f} ({sd:.2f})"))))
tfl_write_output(o, file.path(d, "spec", "outputs", "T-1.yml"))
tfl_fill_widths(d)
#> widen_ard(): 10 ARD rows were not used.
#>   summary        max              2  no template named it
#>   summary        median           2  no template named it
#>   summary        min              2  no template named it
#>   summary        p25              2  no template named it
#>   summary        p75              2  no template named it
#>   (notes = FALSE to silence; attr(, "ard_ignored") has the detail)
unlink(d, recursive = TRUE)
# }
```
