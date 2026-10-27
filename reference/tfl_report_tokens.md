# A report's tokens

The values a report's header, footer, titles and footnotes fill their
`{TOKENS}` with – the study's, the report's own rows, and the report's
own tokens it says (`OUTPUT_LABEL` "Table 14.1.1" ...) – as
[`tfl_report_code()`](https://ichirio.github.io/tflspec/reference/tfl_report_code.md)
gives them to `rtf_document(tokens = )`: for a preview of the page.

## Usage

``` r
tfl_report_tokens(spec, output_id = NULL, study = NULL)
```

## Arguments

- spec:

  A report definition
  ([`tfl_read_report_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_report_spec.md))
  narrowed to one report, or the path(s) to read it from; or a report of
  the YAML spec
  ([`tfl_output()`](https://ichirio.github.io/tflspec/reference/tfl_r.md)),
  as read or resolved.

- output_id:

  The report, when `spec` is a path or still defines several.

- study:

  For a YAML report as read: the study
  ([`tfl_read_study_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_study_spec.md)),
  its defaults and patterns.

## Value

A named character vector (none: empty).

## Examples

``` r
sp <- tfl_table_spec(
  report = data.frame(output_id = "T-14-1-1", type = "table"),
  header = data.frame(output_id = NA, line = "1", center = "{OUTPUT_LABEL}"))
tfl_report_tokens(sp, "T-14-1-1")
#>   OUTPUT_LABEL 
#> "Table 14.1.1" 
```
