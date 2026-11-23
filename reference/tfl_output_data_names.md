# Every data and object a report's program makes, by name

The names a report's program gives what it makes: the analysis data of
its ARD definition (`data_id`, and the analysis sets' and records' own
`<dataset>_sub`), the ARD (`ard`), a figure's data steps that name what
they make (`out:`, a fit's `name:`). For a screen that lists a report's
data and lets a person rename one (the rename itself is the SPEC's,
[`tfl_code_refs()`](https://ichirio.github.io/tflspec/reference/tfl_code_refs.md)
finds the code that still says the old name).

## Usage

``` r
tfl_output_data_names(output, study)
```

## Arguments

- output:

  A report
  ([`tfl_read_output()`](https://ichirio.github.io/tflspec/reference/tfl_read_output.md));
  one with several objects (`contents:`) gives each object's.

- study:

  The study
  ([`tfl_read_study_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_study_spec.md)).

## Value

A data frame, one row a name: `output_id`, `object` (the object of a
report with `contents:`, else `NA`), `name`, `kind` (`subset`: a data
the program keeps; `ard`; `fit`: a model a figure's step fits), `from`
(what it is made from: a dataset, a data above, `NA`), `used_by` (the
analyses or steps that read it, `|` between them).

## See also

[`tfl_ard_data_names()`](https://ichirio.github.io/tflspec/reference/tfl_ard_data_names.md),
[`tfl_code_refs()`](https://ichirio.github.io/tflspec/reference/tfl_code_refs.md)
