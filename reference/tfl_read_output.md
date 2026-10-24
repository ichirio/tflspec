# Read and write a report's YAML file

`tfl_read_output()` reads `spec/outputs/<id>.yml`: the `!r` and `!unset`
tags become
[`tfl_r()`](https://ichirio.github.io/tflspec/reference/tfl_r.md) and
[`tfl_unset()`](https://ichirio.github.io/tflspec/reference/tfl_r.md), a
`{fn:, args:}` map a
[`tfl_call()`](https://ichirio.github.io/tflspec/reference/tfl_r.md),
and each item of `content:` (`- plan_cells: {...}`) a
[`tfl_item()`](https://ichirio.github.io/tflspec/reference/tfl_r.md)
with `from = "own"`. A file whose top is `outputs:` holds several
reports (a family written in one go): a list of them comes back, each
with the file's `base:` merged under it as a plain list (the layering
with `defaults` is not done here). `tfl_write_output()` writes one
report back in the same form.

## Usage

``` r
tfl_read_output(path)

tfl_write_output(x, path)
```

## Arguments

- path:

  The file.

- x:

  A `tfl_output`.

## Value

`tfl_read_output()`: a `tfl_output`, or a list of them.
`tfl_write_output()`: `path`, invisibly.

## Examples

``` r
f <- system.file("extdata", "spec-yaml", "outputs", "T-14-1-1.yml", package = "tflspec")
out <- tfl_read_output(f)
out$content[[1]]
#> <tfl_item> table_plan(cols, rows)
```
