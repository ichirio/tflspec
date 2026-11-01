# Write the default column widths into the reports that have none

A report says its column widths: a table's `plan_columns(widths = )`,
one width a printed column by name, a listing's `listing_col(width = )`.
For each report of the study that does not, this builds it (its ARD, its
plan) and writes the widths rtfreporter uses when none are given
([`rtfreporter::default_col_widths()`](https://ichirio.github.io/rtfreporter/reference/default_col_widths.html):
the row headings by their content, the value columns sharing the rest)
into its file, as numbers to change; a listing's columns get a width
from their variables and headers. Each report it writes widths into gets
an assumption in `spec/_assumptions.yml` (item `widths`): the widths are
the default ones, set for now. Its program
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
