# The code of a report's document, from its definition

Writes the rtfreporter calls a report definition stands for – the page,
the running header and footer, the content, the titles and footnotes –
as `doc <- rtf_document(...)` and one `doc <- rtf_*(doc, ...)` a step:
what
[`tfl_report()`](https://ichirio.github.io/tflspec/reference/tfl_report.md)
does, as a program that needs only rtfreporter. Write it out with
`generate_rtfreport(doc, "<file>.rtf")`
([`tfl_report_path()`](https://ichirio.github.io/tflspec/reference/tfl_report_path.md)
says which).

## Usage

``` r
tfl_report_code(
  spec,
  output_id = NULL,
  content = "content",
  doc = "doc",
  setup = FALSE
)
```

## Arguments

- spec:

  A report definition
  ([`tfl_read_report_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_report_spec.md))
  narrowed to one report, or the path(s) to read it from.

- output_id:

  The report, when `spec` is a path or still defines several.

- content:

  The name of the report's content in the program: a plan
  ([`tfl_table_code()`](https://ichirio.github.io/tflspec/reference/tfl_table_code.md)),
  `rtftable` pages
  ([`tfl_listing_code()`](https://ichirio.github.io/tflspec/reference/tfl_listing_code.md))
  or the figures.

- doc:

  The name the document is assigned to.

- setup:

  `TRUE`: the program runs after the study's setup code
  ([`tfl_report_setup_code()`](https://ichirio.github.io/tflspec/reference/tfl_report_setup_code.md)),
  so it leaves out what that defines: the study's tokens (rtfreporter
  fills from the document's first, then
  `options(rtfreporter.tokens = )`), and the study's header and footer,
  said by name (`study_header`, `study_footer`; a line the report's
  tokens leave empty, a blank `<{OUTPUT_POPULATION}>`, is left out when
  the file is written) – unless the report has its own, written here as
  before. `FALSE` (default): the program stands alone.

## Value

The code, one element per line.

## Examples

``` r
spec <- tfl_read_report_spec(
  system.file("extdata", "ard-spec", c("report.xlsx", "study.xlsx"),
              package = "tflspec"), output_id = "DM")
cat(tfl_report_code(spec, content = "plan"), sep = "\n")
#> doc <- rtf_document(program = "C:\\study\\tfl\\DM")
#> doc <- rtf_section(
#>   doc,
#>   secinfo = list(
#>     header = rtf_header(
#>       list(
#>         c(l = "Example Pharma", r = "Clinical Study Report"),
#>         c(l = "Study EX-001", r = "Page {PAGE} of {TOTAL_PAGES}"),
#>         c(c = ""),
#>         c(c = "Table 14.1.2"),
#>         c(c = "Demographics and Baseline Characteristics"),
#>         c(c = "Safety Population")
#>       )
#>     ),
#>     footer = rtf_footer(
#>       list(
#>         c(l = "SD: standard deviation; Min: minimum; Max: maximum."),
#>         c(l = "{PROGRAM}       Generated on: {DATETIME}")
#>       )
#>     )
#>   )
#> )
#> doc <- rtf_tables(doc, plan)
```
