# The study's setup code for its report programs

What every report of a study shares, as code written once: the study's
font and size (the page sheet's default row: `font`,
`font_size_half_points`) as
`options(rtfreporter.font = , rtfreporter.font_size_half_points = )`,
its tokens (the tokens sheet's default rows, a blank `output_id`:
`COMPANY`, `STUDY_ID` ...) as `options(rtfreporter.tokens = list(...))`,
and its running header and footer (the header and footer sheets' default
rows) as `study_header <- rtf_header(..., drop_empty_rows = TRUE)` and
`study_footer <- rtf_footer(...)` (a line a report's tokens leave empty
is left out of that report's file). A study's report programs source it
and are written with `tfl_report_code(setup = TRUE)`: each then says
only its own tokens (`OUTPUT_LABEL`, `OUTPUT_TITLE` ...), and the
study's values are in one place, in the definition and in the code.

## Usage

``` r
tfl_report_setup_code(spec)
```

## Arguments

- spec:

  A report definition (the study's default rows: one with several
  reports serves).

## Value

The code, one element per line; none when the study has no tokens,
header or footer.

## Examples

``` r
sp <- tfl_table_spec(
  header = data.frame(output_id = NA, line = c("1", "2"),
                      left = c("{COMPANY}", "PROTOCOL: {STUDY_ID}"),
                      right = c(NA, "Page {PAGE} of {TOTAL_PAGES}")),
  tokens = data.frame(output_id = NA, name = c("COMPANY", "STUDY_ID"),
                      value = c("Sample Pharma", "ABC-123")))
cat(tfl_report_setup_code(sp), sep = "\n")
#> options(
#>   rtfreporter.tokens = list(COMPANY = "Sample Pharma", STUDY_ID = "ABC-123")
#> )
#> study_header <- rtf_header(
#>   list(
#>     c(l = "{COMPANY}"),
#>     c(l = "PROTOCOL: {STUDY_ID}", r = "Page {PAGE} of {TOTAL_PAGES}")
#>   ),
#>   drop_empty_rows = TRUE
#> )
```
