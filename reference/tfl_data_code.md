# The code that makes the objects a report's content reads

A table's content reads one object – the study's ARD (`ard`), or the
records of a dataset (`data`, a table of records:
`table_plan(value = )`) – and each part appended to it (`plan_append()`)
its own: the ARD, or a dataset's records under the part's object name
(`adpc`). This writes the code that makes each of them, as the report's
program has it before the content's code
([`tfl_content_code()`](https://ichirio.github.io/tflspec/reference/tfl_content_code.md)):
a dataset's records as a listing's data part
([`tfl_listing_code()`](https://ichirio.github.io/tflspec/reference/tfl_listing_code.md)
`part = "data"`: read, the analysis set's subjects, the condition), the
ARD as the study ARD's rows of the report. A statement two objects share
(a dataset read once) is written once, with the first.

## Usage

``` r
tfl_data_code(output, study, ard = TRUE)
```

## Arguments

- output:

  A report
  ([`tfl_output()`](https://ichirio.github.io/tflspec/reference/tfl_r.md))
  or its id.

- study:

  The study
  ([`tfl_read_study_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_study_spec.md)),
  or its folder.

- ard:

  `FALSE` leaves the ARD's code out (a program that writes its own: the
  study's ARD program, its rework).

## Value

A named list, one element an object (`data`, `ard`, a part's object),
each its lines of code.

## Examples

``` r
o <- tfl_output("T-1", "table", content = list(
  tfl_item("table_plan", list(cols = "TRT01A"))))
st <- tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))
tfl_data_code(o, st)
#> $ard
#> [1] "# the study ARD's rows of this report"                                    
#> [2] "ard <- readRDS(\"output/ard/ard.rds\") |>\n  subset(output_id == \"T-1\")"
#> 
```
