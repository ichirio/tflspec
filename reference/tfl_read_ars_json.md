# Read a CDISC ARS reporting event from JSON

Reads an ARS v1.0 JSON file – written by
[`tfl_write_ars_json()`](https://ichirio.github.io/tflspec/reference/tfl_write_ars_json.md)
or by any other tool – as the model
[`tfl_ars()`](https://ichirio.github.io/tflspec/reference/tfl_ars.md)
makes. A reference that names nothing (a method, an analysis set, a
grouping ...) stops it; a missing purpose or reason is warned about.
[`tfl_ars_to_specs()`](https://ichirio.github.io/tflspec/reference/tfl_ars_to_specs.md)
turns it into specs.

## Usage

``` r
tfl_read_ars_json(path)
```

## Arguments

- path:

  An ARS `.json` file.

## Value

A `tfl_ars`. Its profile is `"siera"` when its methods carry siera code
templates, else `"cdisc"`.

## See also

[`tfl_ars_to_specs()`](https://ichirio.github.io/tflspec/reference/tfl_ars_to_specs.md),
[`tfl_check_ars()`](https://ichirio.github.io/tflspec/reference/tfl_check_ars.md)
