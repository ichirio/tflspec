# Parameters' value and change: a table plan from the template `param_change`

Laboratory tests, vital signs or ECG parameters at each visit: n, mean
(SD), median and min, max of the value (`AVAL`) and of its change from
baseline (`CHG`), a block a parameter (`PARAM`, its heading with its
unit) and a visit (`AVISIT`). The parameters and visits are in the order
of `PARAMN` and `AVISITN`. The change is read from the records that have
one: the baseline visit's change cells stay empty.

## Usage

``` r
tpl_param_change(ard, population, ..., codelists = NULL)
```

## Arguments

- ard:

  The normalized ARD's source: the study ARD's rows of this report.

- population:

  The analysis set's id (`"SAF"`).

- ...:

  The template's other parameters: `data` (required), `params`,
  `visits`, `where`, `by`, `layout`, `param_order`, `visit_order`,
  `baseline`, `baseline_label`, `value_label`, `change_label`,
  `statistics`, `cells` ...

- codelists:

  The report's code lists, for the order and labels of values; `NULL`
  takes the ARD's.

## Value

A
[`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html).

## Details

`layout = "cols"` puts each arm's value and change side by side;
`"rows"` puts the change's rows under the value's, in one column an arm;
`baseline = TRUE` puts the baseline's (`BASE`) before them.
