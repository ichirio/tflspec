# Make the ARD of a CDISC ARS reporting event with siera

Has siera (pharmaverse) make the ARD the reporting event describes:
writes the ADaM it reads as CSV files (siera's programmes read
`<DATASET>.csv`), the event as ARS JSON, has
[`siera::readARS()`](https://clymbclinical.github.io/siera/reference/readARS.html)
write one programme per output, and runs them. With
[`tfl_build_ard()`](https://ichirio.github.io/tflspec/reference/tfl_build_ard.md)
on the same spec this is a round trip: the ARS says the same analyses as
the spec when the two ARDs hold the same numbers.

## Usage

``` r
tfl_ars_ard(ars, adam, dir = tempfile("tfl_ars_"), keep = FALSE)
```

## Arguments

- ars:

  A
  [`tfl_ars()`](https://ichirio.github.io/tflspec/reference/tfl_ars.md)
  written with `profile = "siera"`.

- adam:

  The ADaM: a named list of data frames, or a folder
  ([`tfl_read_adam()`](https://ichirio.github.io/tflspec/reference/tfl_read_adam.md)).

- dir:

  A folder for the CSV files, the JSON and siera's programmes.

- keep:

  Keep `dir` (by default it is removed on exit).

## Value

The ARD (one data frame, all outputs), with siera's `OutputId` and
`AnalysisId` given back as the spec's `output_id` and the ARS analysis
id. With `keep = TRUE`, attribute `dir`.

## Details

CSV keeps text and numbers; a date becomes text and a code with leading
zeros may become a number. A variable of the event that is a date, or
text of digits with a leading zero, is warned about before it is
written.

## See also

[`tfl_ars()`](https://ichirio.github.io/tflspec/reference/tfl_ars.md),
[`tfl_build_ard()`](https://ichirio.github.io/tflspec/reference/tfl_build_ard.md)
