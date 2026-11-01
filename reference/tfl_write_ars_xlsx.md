# Write a CDISC ARS reporting event as CDISC's Excel template

The reporting event in the shape of the Excel template of CDISC's ARS
repository ("ARS Template.xlsx"): its 25 sheets in their order, each
with the template's columns – one row per list item, group, operation
and display sub-section, a condition as rows of `level` / `order`,
several values joined by `" | "`. It is the form to read and to set
beside other ARS workbooks; CDISC's converter (`excel2ars.py`) reads it
back to the JSON. The JSON
([`tfl_write_ars_json()`](https://ichirio.github.io/tflspec/reference/tfl_write_ars_json.md))
stays the one to exchange. Results (`AnalysisResults`) are not written:
a reporting event of
[`tfl_ars()`](https://ichirio.github.io/tflspec/reference/tfl_ars.md)
has none. A display without sections has no row in the template: its
name is written as its one title line.

## Usage

``` r
tfl_write_ars_xlsx(ars, path, overwrite = FALSE)
```

## Arguments

- ars:

  A
  [`tfl_ars()`](https://ichirio.github.io/tflspec/reference/tfl_ars.md)
  or
  [`tfl_read_ars_json()`](https://ichirio.github.io/tflspec/reference/tfl_read_ars_json.md).

- path:

  Destination `.xlsx`.

- overwrite:

  Replace an existing file.

## Value

`path`, invisibly.

## See also

[`tfl_write_ars_json()`](https://ichirio.github.io/tflspec/reference/tfl_write_ars_json.md)
