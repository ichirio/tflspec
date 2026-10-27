# Exposure-adjusted incidence rates by SOC and PT: the template `ae_eair`

A display template on the `hierarchical` layout: each arm's subjects
with an event (n) and the exposure-adjusted incidence rate (EAIR) –
overall, in each system organ class and in each preferred term. The EAIR
is the subjects with the event over their total time at risk, per `per`
person-years (365.25 days a year): a subject with the event is at risk
until their first one, a subject without it for their whole exposure
(`exposure`, ADSL's `TRTDURD`). The rates are cardx's
`ard_incidence_rate()`.

## Usage

``` r
tpl_ae_eair(ard, population, ..., codelists = NULL)
```

## Arguments

- ard:

  The normalized ARD's source: the study ARD's rows of this report.

- population:

  The analysis set's id (`"SAF"`).

- ...:

  The template's other parameters: `data`, `where`, `by`, `hierarchy`,
  `exposure`, `start`, `per`, `overall`.

- codelists:

  The report's code lists, for the order and labels of values; `NULL`
  takes the ARD's.

## Value

A
[`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html).

## Details

The days to a subject's first event are the smallest `start` of their
records of the term: `ASTDY` (the study standard), or R that makes the
day from ADAE's columns, `ASTDT - TRTSDT + 1`.

## Examples

``` r
x <- tfl_template_expand("ae_eair", list(population = "SAF"))
vapply(x$ard$analyses, `[[`, "", "method")
#> [1] "categorical"               "cardx::ard_incidence_rate"
#> [3] "cardx::ard_incidence_rate" "cardx::ard_incidence_rate"
```
