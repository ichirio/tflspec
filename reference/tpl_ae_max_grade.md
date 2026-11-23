# Adverse events by SOC and PT at the maximum severity or grade: the template `ae_max_grade`

A display template on the `hierarchical` layout: each subject counted
once at the highest severity (`AESEV`) or grade (`AETOXGR`) of their
treatment-emergent events – overall, in each system organ class and in
each preferred term. The grades are rows under each term
(`grade_in = "rows"`, as FDA's tables and chevron's AET03), or columns
in each group (`"cols"`); a term's count at any grade is its own row or
column (`any_grade`). The grades' order is `grade_levels`, or the code
list of `grade`.

## Usage

``` r
tpl_ae_max_grade(ard, population, ..., codelists = NULL)
```

## Arguments

- ard:

  The normalized ARD's source: the study ARD's rows of this report.

- population:

  The analysis set's id (`"SAF"`).

- ...:

  The template's other parameters: `grade`, `grade_levels`, `grade_in`,
  `hierarchy`, `by`, `where`, `overall`, `any_grade`, `heading` ...

- codelists:

  The report's code lists, for the order and labels of values; `NULL`
  takes the ARD's.

## Value

A
[`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html).

## Examples

``` r
x <- tfl_template_expand("ae_max_grade", list(population = "SAF",
                         grade_levels = c("MILD", "MODERATE", "SEVERE")))
vapply(x$ard$analysis_data, `[[`, "", "id")
#> [1] "adsl_saf"         "adae_saf"         "adae_saf_max"     "adae_saf_max_soc"
#> [5] "adae_saf_max_pt" 
```
