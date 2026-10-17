# Read a study's YAML spec

`tfl_read_study_spec()` reads the study-level files (every one optional)
and `tfl_read_outputs()` the report files under `spec/outputs/`
(subfolders too; a file with `outputs:` holds several). An id in two
files stops the read.

## Usage

``` r
tfl_read_study_spec(dir)

tfl_read_outputs(dir)
```

## Arguments

- dir:

  The study folder (holds `study.yml` and `spec/`).

## Value

`tfl_read_study_spec()`: a list of class `tfl_study_spec` – `study`,
`data`, `defaults`, `codelists`, `patterns` (by name), `toc`, `dir`.
`tfl_read_outputs()`: a named list of `tfl_output`, by id.

## Examples

``` r
d <- system.file("extdata", "spec-yaml", package = "tflspec")
s <- tfl_read_study_spec(d)
names(s$defaults$content)
#> NULL
```
