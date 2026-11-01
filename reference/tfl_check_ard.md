# Check an ARD before a report uses it

For an ARD made elsewhere and taken in (a report's `ard_source` is
`import:<file>`). Checks, each a row of the result:

## Usage

``` r
tfl_check_ard(ard, spec = NULL, output_id = NULL)
```

## Arguments

- ard:

  The ARD (a data frame or a cards ARD), e.g.
  [`tfl_read_ard()`](https://ichirio.github.io/tflspec/reference/tfl_read_ard.md).

- spec:

  Optional: the report's table definition
  ([`tfl_read_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_table_spec.md)),
  narrowed to the report or with `output_id`.

- output_id:

  The report, when `spec` defines several.

## Value

A data frame, one row per problem: `level` (`"error"`: the report cannot
be made; `"warning"`: something it reads is missing; `"note"`), `check`
and `message`. No rows: nothing found.

## Details

- **shape**: the columns every ARD has (`variable`, `stat_name`,
  `stat`); with cards installed, its own
  [`cards::check_ard_structure()`](https://pharmaverse.github.io/cards/latest-tag/reference/check_ard_structure.html)
  (as notes, less its wish for `method` rows, which no report reads); an
  old column name (`fmt_fn`, now `fmt_fun`). A `groupN` without
  `groupN_level` is a result across the groups (a test), as cardx gives
  it.

- with `spec`, the report's table definition: the columns its `tables`
  roles name (`cols`, `rows`) are in the ARD (as a group or a variable),
  the variables its `cells` name are analysed, and the statistics its
  templates read (`{mean}`, `{n:d}`) are there.

## See also

[`tfl_read_ard()`](https://ichirio.github.io/tflspec/reference/tfl_read_ard.md),
[`tfl_write_ard()`](https://ichirio.github.io/tflspec/reference/tfl_write_ard.md)
