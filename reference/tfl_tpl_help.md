# A template's help

Prints a registered template's parameters by tier: each one's value that
takes effect, where it is from (the study standard, a variant, the
template) and what it is. A report's argument wins over all of them.

## Usage

``` r
tfl_tpl_help(
  name,
  study = NULL,
  population = NULL,
  tier = c("required", "usual", "other")
)
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

- tier:

  The tiers to print: `"required"`, `"usual"`, `"other"` (all by
  default; `c("required", "usual")` folds the others away).

## Value

[`tfl_tpl_args()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_args.md)`(name, study, population)`,
invisibly.

## See also

[`tfl_tpl()`](https://ichirio.github.io/tflspec/reference/tfl_tpl.md);
the template's page, `?tfl_tpl_<name>`
([`?tfl_tpl_demog`](https://ichirio.github.io/tflspec/reference/tfl_tpl_demog.md)).

## Examples

``` r
tfl_tpl_help("demog", tier = c("required", "usual"))
#> demog (table, layout summary; variant of summary; tflspec)
#> Demographics
#> Call: tfl_tpl(ard, "demog", ...)
#> 
#> required (1):
#>   param      | effective (layer) | what
#>   population | - (template)      | the analysis set's id (SAF)
#> 
#> usual (6):
#>   param       | effective (layer)                                                            | what
#>   by          | TRT01A (template)                                                            | the column variable
#>   continuous  | AGE (variant:demog)                                                          | the continuous variables, in order
#>   categorical | AGEGR1, SEX, RACE, ETHNIC (variant:demog)                                    | the categorical variables, in order
#>   labels      | AGE = Age (years); AGEGR1 = Age group, n (%); SEX = Sex, ... (variant:demog) | each variable's heading; given by name, merged (RACE: ~ drops one)
#>   cells       | n = {N:.0f}; Mean (SD) = {mean:.1f} ({sd:.2f}); Median = ... (template)      | the rows of each continuous variable
#>   total       | FALSE (template)                                                             | TRUE: a Total column (each analysis again without `by`, plan_total())
```
