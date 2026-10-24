# Read an ARD written by tfl_write_ard()

The `"rows"` JSON / YAML gives the ARD back: its columns with their
types, each variable's levels as factors in their order, the statistics
as numbers or text as written (the formatting functions are not kept;
`stat_fmt` is). An XPT or CSV is read as far as a flat table allows: the
level columns become list columns again, with no factors. An rds is read
as it is.

## Usage

``` r
tfl_read_ard(path, format = NULL)
```

## Arguments

- path:

  The file.

- format:

  As
  [`tfl_write_ard()`](https://ichirio.github.io/tflspec/reference/tfl_write_ard.md);
  `NULL` from the extension (`.csv` too).

## Value

A cards ARD (class `card` when cards is installed).

## See also

[`tfl_write_ard()`](https://ichirio.github.io/tflspec/reference/tfl_write_ard.md)
