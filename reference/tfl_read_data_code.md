# The code that reads one dataset of a data catalog

Writes `adsl <- haven::read_xpt("data/adam/adsl.xpt")` (the reader
follows the file's extension: `.rds`, `.xpt`, `.sas7bdat`, `.csv`,
`.parquet`, and `.rda` / `.RData` holding one dataset) into an object
named after the dataset, and its derived columns.

## Usage

``` r
tfl_read_data_code(datasets, dataset)
```

## Arguments

- datasets:

  The data catalog: a data frame with `dataset`, `path` and `derive`
  (`NAME = R expression`, `|` between them), as the `datasets` sheet of
  an
  [`tfl_ard_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_ard_spec.md).

- dataset:

  The dataset to read.

## Value

The code, one element per line. A dataset the catalog does not have
gives a [`stop()`](https://rdrr.io/r/base/stop.html) line, so the
program says so when it runs.
