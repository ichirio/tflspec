# A template's parameters, as a table

Each parameter of a registered template
([`tfl_templates()`](https://ichirio.github.io/tflspec/reference/tfl_templates.md)):
its tier, its type, the layout's default, the default a variant changes,
the study standard's value and the value that takes effect. The order of
precedence is a report's argument, then the study standard, then the
variant's default, then the layout's default; this table has no report,
so its `effective` value is the one a report that does not give the
parameter gets.

## Usage

``` r
tfl_tpl_args(name, study = NULL, population = NULL)
```

## Arguments

- name:

  The template's name (`"demog"`).

- study:

  The study standard: a
  [`tfl_read_study_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_study_spec.md)
  object or a study's spec folder; `NULL` for none (the `study` column
  is then `NA`). Its `defaults.yml` content is where a parameter's
  `"<verb>.<argument>"` source is read, `study.yml`'s
  `settings: treatment:` the `by` (`"settings.treatment"`).

- population:

  The report's analysis set: the value of the parameters taken from the
  report's `population:` (`"output.population"`); `NULL` for none.

## Value

A data frame, one row a parameter in the template's order: `param`,
`tier` (`"required"`, `"usual"`, `"other"`), `stage` (`"call"`, an
argument of the template's call, or `"data"`, of a step preparing the
analysis data:
[`tfl_param()`](https://ichirio.github.io/tflspec/reference/tfl_param.md)`(stage = )`),
`type`, `required`, `default` (the layout's, a short text), `variant`
(the default a variant changes it to, else `NA`), `study` (the study
standard's value, else `NA`), `effective` (the value that takes effect),
`layer` (where it is from: `"study"`, `"variant:<name>"` or
`"template"`), `maps_to` (its study source, `"plan_stub.indent"`, or the
verb its doc names) and `doc`.

## See also

[`tfl_tpl_help()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_help.md)
to print it;
[`tfl_tpl()`](https://ichirio.github.io/tflspec/reference/tfl_tpl.md) to
run a template.

## Examples

``` r
a <- tfl_tpl_args("demog")
a[a$tier != "other", c("param", "effective", "layer")]
#>          param                                                    effective
#> 1   population                                                         <NA>
#> 5           by                                                       TRT01A
#> 6   continuous                                                          AGE
#> 7  categorical                                    AGEGR1, SEX, RACE, ETHNIC
#> 8       labels AGE = Age (years); AGEGR1 = Age group, n (%); SEX = Sex, ...
#> 10       cells n = {N:.0f}; Mean (SD) = {mean:.1f} ({sd:.2f}); Median = ...
#> 14       total                                                        FALSE
#>            layer
#> 1       template
#> 5       template
#> 6  variant:demog
#> 7  variant:demog
#> 8  variant:demog
#> 10      template
#> 14      template
s <- tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))
a <- tfl_tpl_args("demog", study = s, population = "SAF")
a[a$layer == "study", c("param", "effective", "maps_to")]
#>         param          effective                maps_to
#> 1  population                SAF      output.population
#> 11 count_cell {n:.0f} ({p:.1f%}) plan_cells.categorical
```
