# Read reports' template SPECs from a workbook

A workbook of two sheets, many reports at once:

- `templates`:

  one row a report: `output_id`, `kind`, `label`, `population`,
  `template`, `by`, `where`, and `args` – the other arguments of one
  value, `name = value; name = value`.

- `template_rows`:

  the arguments that are lists, one row a value: `output_id`, `param`
  (`continuous`, `categorical`, `hierarchy` ...), `value` (`AGE`) and
  `label` (its heading, for `labels`).

## Usage

``` r
tfl_read_template_sheets(path)
```

## Arguments

- path:

  The workbook.

## Value

A list of reports, by id: each `id`, `kind`, `label`, `population`,
`extends` and `template` (`name`, `args`), as a report file
([`tfl_write_template_yaml()`](https://ichirio.github.io/tflspec/reference/tfl_write_template_yaml.md))
holds it.

## Examples

``` r
f <- system.file("extdata", "template-spec", "templates.xlsx", package = "tflspec")
r <- tfl_read_template_sheets(f)
str(r[["T-14-1-1"]]$template)
#> List of 2
#>  $ name: chr "demog"
#>  $ args:List of 5
#>   ..$ population : chr "SAF"
#>   ..$ by         : chr "TRT01A"
#>   ..$ continuous : chr "AGE"
#>   ..$ labels     : Named chr [1:5] "Age (years)" "Age group, n (%)" "Sex, n (%)" "Race, n (%)" ...
#>   .. ..- attr(*, "names")= chr [1:5] "AGE" "AGEGR1" "SEX" "RACE" ...
#>   ..$ categorical: chr [1:4] "AGEGR1" "SEX" "RACE" "ETHNIC"
```
