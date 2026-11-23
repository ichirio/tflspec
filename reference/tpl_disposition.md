# Disposition: a table plan from the template `disposition`

A display template on the `summary` layout: the subjects' status at the
end of the study (`EOSSTT`), n (%).

## Usage

``` r
tpl_disposition(ard, population, ..., codelists = NULL)
```

## Arguments

- ard:

  The normalized ARD's source: the study ARD's rows of this report.

- population:

  The analysis set's id (`"SAF"`).

- ...:

  The template's other parameters: `categorical`, `by`,
  `categorical_statistics` ...

- codelists:

  The report's code lists, for the order and labels of values; `NULL`
  takes the ARD's.

## Value

A
[`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html).
