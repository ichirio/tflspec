# Run a template: its table plan

The A use of any registered template: its items run on the data, as a
report's content. tflspec's `tpl_<name>()` and a company's exported
functions call this. A study's defaults are not in it; the program adds
them after the call.

## Usage

``` r
tfl_template_run(name, x, ..., codelists = NULL, sets = NULL)
```

## Arguments

- name:

  The template's name.

- x:

  The data its items run on: the ARD (a table), the dataset (a listing).

- ...:

  Its parameters, by name.

- codelists:

  The report's code lists: as
  [`tfl_read_output()`](https://ichirio.github.io/tflspec/reference/tfl_read_output.md)
  reads them, or the values in order,
  `list(TRT01A = c("Drug X", "Control"))`.

- sets:

  The study's analysis sets (`data.yml`'s populations), for their
  labels; `NULL` none.

## Value

What the template's items make: a
[`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html)
for a table or a listing.

## Examples

``` r
if (requireNamespace("cards", quietly = TRUE)) {
  saf <- adsl[adsl$SAFFL == "Y", ]
  ard <- cards::bind_ard(cards::ard_tabulate(saf, variables = TRT01A),
                         cards::ard_summary(saf, by = TRT01A, variables = AGE))
  plan <- tfl_template_run("summary", ard, population = "SAF", continuous = "AGE")
}
```
