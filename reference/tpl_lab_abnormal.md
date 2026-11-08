# Laboratory abnormalities: a table plan from the template `lab_abnormal`

The subjects with a laboratory value in an abnormal category, a block a
parameter (`PARAM`): the normal range indicator (`ANRIND`: `LOW`,
`HIGH`), or a criterion's levels (`MCRIT1ML`). A cell is n/N (%): N is
the arm's subjects with a value of the parameter in the records kept, n
those with a value in the category; every parameter analysed has its
rows, 0/N when no subject is in a category. A subject is counted once in
each category; with `cumulative = TRUE`, in every level up to their
worst (`levels` lowest first: "level 2 or more"). With
`baseline_normal = TRUE`, n and N are the subjects normal at baseline
only (`BNRIND == "NORMAL"`). The categories' order is `levels`, or the
code list of `category`; the parameters' is `PARAMN` (`param_order`;
`NULL`: by name).

## Usage

``` r
tpl_lab_abnormal(ard, population, ..., codelists = NULL)
```

## Arguments

- ard:

  The normalized ARD's source: the study ARD's rows of this report.

- population:

  The analysis set's id (`"SAF"`).

- ...:

  The template's other parameters: `data`, `params`, `where`, `by`,
  `param_order`, `category`, `levels`, `cumulative`, `baseline_normal`,
  `baseline`, `baseline_value`, `heading` ...

- codelists:

  The report's code lists, for the order and labels of values; `NULL`
  takes the ARD's.

## Value

A
[`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html).

## Examples

``` r
x <- tfl_template_expand("lab_abnormal", list(population = "SAF",
                         levels = c("LOW", "HIGH"), where = "ONTRTFL == \"Y\""))
vapply(x$ard$analysis_data, `[[`, "", "id")
#> [1] "adsl_saf"     "adlb_saf"     "adlb_saf_n"   "adlb_saf_cat"
```
