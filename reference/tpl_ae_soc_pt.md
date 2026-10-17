# Adverse events by SOC and PT: a table plan from the template `ae_soc_pt`

A display template on the `hierarchical` layout: treatment-emergent
events (`TRTEMFL == "Y"`), the subjects with any event first.

## Usage

``` r
tpl_ae_soc_pt(ard, population, ..., codelists = NULL)
```

## Arguments

- ard:

  The normalized ARD's source: the study ARD's rows of this report.

- population:

  The analysis set's id (`"SAF"`).

- ...:

  The template's other parameters: `hierarchy`, `where`, `overall`,
  `by`, `sort`, `heading` ...

- codelists:

  The report's code lists, for the order and labels of values; `NULL`
  takes the ARD's.

## Value

A
[`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html).
