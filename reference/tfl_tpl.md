# A table or listing template's plan: one call for every template

The A use of a template SPEC, one function for them all: the template's
items run on the data, as the report's content.
`tfl_tpl(ard, "demog", population = "SAF")` is what a template report's
program calls; a study's defaults the template does not take as its
parameters are added after the call. The template's parameters, their
defaults and where each ends:
[`tfl_tpl_help()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_help.md)
(and `?tpl_<name>`).

## Usage

``` r
tfl_tpl(x, name, ..., codelists = NULL, sets = NULL)
```

## Arguments

- x:

  The data: the ARD, not yet normalized (a table); the dataset as the
  program made it, its subjects kept already (a listing).

- name:

  The template's name
  ([`tfl_templates()`](https://ichirio.github.io/tflspec/reference/tfl_templates.md)):
  a layout, a display template or a variant.

- ...:

  Its parameters, by name. A name it does not have is an error naming
  its parameters.

- codelists:

  The report's code lists: as
  [`tfl_read_output()`](https://ichirio.github.io/tflspec/reference/tfl_read_output.md)
  reads them, or the values in order,
  `list(TRT01A = c("Drug X", "Control"))`.

- sets:

  The study's analysis sets (`data.yml`'s populations), for their
  labels; `NULL` none.

## Value

A
[`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html).

## See also

[`tfl_template_expand()`](https://ichirio.github.io/tflspec/reference/tfl_template_expand.md)
for the items (the B use);
[`tfl_templates()`](https://ichirio.github.io/tflspec/reference/tfl_templates.md);
a figure template's own function
([`tpl_mean_se()`](https://ichirio.github.io/tflspec/reference/tpl_mean_se.md)
...).

## Examples

``` r
if (requireNamespace("cards", quietly = TRUE)) {
  saf <- adsl[adsl$SAFFL == "Y", ]
  ard <- cards::bind_ard(
    cards::ard_tabulate(saf, variables = TRT01A),
    cards::ard_summary(saf, by = TRT01A, variables = AGE),
    cards::ard_tabulate(saf, by = TRT01A, variables = c(AGEGR1, SEX, RACE, ETHNIC)))
  plan <- tfl_tpl(ard, "demog", population = "SAF")
}
```
