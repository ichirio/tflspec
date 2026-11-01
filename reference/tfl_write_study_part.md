# Write one part of a study's YAML spec

Writes the study-level file of one part of a
[`tfl_read_study_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_study_spec.md)
result – what a builder changes for the whole study (the rounding of
every table, a pattern, a code list) – in the form the reader reads.

## Usage

``` r
tfl_write_study_part(
  study,
  part = c("defaults", "patterns", "codelists", "data", "toc"),
  path = NULL,
  name = NULL
)
```

## Arguments

- study:

  A `tfl_study_spec`.

- part:

  `"defaults"`, `"patterns"` (one, by `name`), `"codelists"`, `"data"`
  or `"toc"`.

- path:

  The file; `NULL`: the study's own (`spec/defaults.yml`,
  `spec/patterns/<name>.yml` ...).

- name:

  For `part = "patterns"`: the pattern.

## Value

`path`, invisibly.

## Examples

``` r
d <- system.file("extdata", "spec-yaml", package = "tflspec")
s <- tfl_read_study_spec(d)
f <- tempfile(fileext = ".yml")
tfl_write_study_part(s, "defaults", f)
identical(tfl_read_study_spec(d)$defaults$content[[1]]$fn,
          yaml::read_yaml(f)$content[[1]] |> names())
#> [1] TRUE
```
