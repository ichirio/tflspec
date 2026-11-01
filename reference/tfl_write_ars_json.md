# Write a CDISC ARS reporting event as JSON

The JSON of the CDISC Analysis Results Standard v1.0 model, the form ARS
is exchanged in (and what siera's `readARS()` and CDISC's own tools
read). The same specs always give the same file.

## Usage

``` r
tfl_write_ars_json(ars, path, pretty = TRUE)
```

## Arguments

- ars:

  A
  [`tfl_ars()`](https://ichirio.github.io/tflspec/reference/tfl_ars.md).

- path:

  Destination `.json`.

- pretty:

  Indent the JSON.

## Value

`path`, invisibly.

## See also

[`tfl_check_ars()`](https://ichirio.github.io/tflspec/reference/tfl_check_ars.md)
