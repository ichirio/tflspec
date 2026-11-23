# Overview of adverse events: a table plan from the template `ae_overview`

A display template on the `summary` layout: the subjects with a
treatment-emergent adverse event of each kind (any, related, grade 3 or
higher, serious, leading to discontinuation, leading to death), n (%) by
arm, one row a kind.

## Usage

``` r
tpl_ae_overview(ard, population = "SAF", ..., codelists = NULL)
```

## Arguments

- ard:

  The normalized ARD's source: the study ARD's rows of this report.

- population:

  The analysis set's id (`"SAF"`).

- ...:

  The template's other parameters: `by`, `data`, `where`, `data_id`,
  `flags` (the rows: a list, each `id`, `label` and `where`), `heading`.

- codelists:

  The report's code lists, for the order and labels of values; `NULL`
  takes the ARD's.

## Value

A
[`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html).

## Examples

``` r
tfl_template_expand("ae_overview")$ard$analyses[[3]]
#> $id
#> [1] "REL"
#> 
#> $label
#> [1] "Any treatment-related TEAE"
#> 
#> $method
#> [1] "subjects"
#> 
#> $data
#> [1] "adae_saf"
#> 
#> $where
#> [1] "AEREL == \"RELATED\""
#> attr(,"class")
#> [1] "tfl_r"
#> attr(,"tag")
#> [1] "!r"
#> 
#> $by
#> [1] "TRT01A"
#> 
#> $variables
#> [1] "REL"
#> 
#> $purpose
#> [1] "PRIMARY OUTCOME MEASURE"
#> 
```
