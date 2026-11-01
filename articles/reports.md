# Reports: headers, footers, titles, footnotes and tokens

The report spec is the document around a table, a listing or a figure:
the page, the running header and footer, the titles and footnotes, a
watermark.
[`tfl_report_code()`](https://ichirio.github.io/tflspec/reference/tfl_report_code.md)
writes the rtfreporter calls it stands for (`doc <- rtf_document(...)`,
[`rtf_section()`](https://ichirio.github.io/rtfreporter/reference/rtf_section.html),
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.html),
[`rtf_titles()`](https://ichirio.github.io/rtfreporter/reference/rtf_titles.html),
[`rtf_footnotes()`](https://ichirio.github.io/rtfreporter/reference/rtf_footnotes.html)),
and
[`tfl_report()`](https://ichirio.github.io/tflspec/reference/tfl_report.md)
makes the same document from a plan or pages in hand.

``` r

library(tflspec)
library(rtfreporter)
```

## The sheets

| Sheet | One row per | Holds |
|----|----|----|
| `report` | report | `type` (`table`, `listing`, `figure`, `user`), `file` (`{output_id}.rtf`), `program`, font sizes, `page_header` / `page_footer`, `watermark`, a figure’s size |
| `page` | report | paper, orientation, margins, the document’s text defaults; `font` and `font_size_half_points` (the study’s row is the company’s) |
| `header`, `footer`, `titles`, `footnotes` | line | `line`, `left`, `center`, `right` |
| `tokens` | token | `name`, `value`: a token of one’s own, `{STUDY}` in any cell above |

plus the `study` keys `output_path` (where the RTF files go) and
`program_dir` (where the programs are, for `{PROGRAM}`).

## Written once for the study

The `output_id` rule makes the study’s look a matter of **default rows**
(a blank `output_id`), written once; each report adds or replaces only
what is its own. On the line sheets **the line number is the key**:

- the study’s page header is default lines 1 and 2, and each report’s
  titles follow on its own lines 3, 4, …;
- a run line 99 in the default footer closes every report;
- a report’s line replaces the default line of the same number, and a
  report’s line that says `(none)` takes it out.

``` r

df <- function(...) data.frame(..., stringsAsFactors = FALSE)

spec <- tfl_table_spec(
  study = c(output_path = "output", program_dir = "programs"),
  report = df(output_id = c(NA, "T-14-1-1", "T-14-3-1"),
              file = c("{output_id}.rtf", NA, NA),
              watermark = c("DRAFT", NA, NA)),
  header = df(output_id = c(NA, NA, "T-14-1-1", "T-14-1-1", "T-14-3-1"),
              line = c(1, 2, 3, 4, 3),
              left = c("{SPONSOR}", "Protocol: {STUDY}", NA, NA, NA),
              center = c(NA, NA, "Table 14.1.1", "Demographics",
                         "Table 14.3.1"),
              right = c("{CUTOFF}", "Page {PAGE} of {TOTAL_PAGES}",
                        NA, NA, NA)),
  footer = df(output_id = c(NA, "T-14-3-1"), line = c(99, 99),
              left = c("{PROGRAM}   Generated on: {DATETIME}", "(none)")),
  footnotes = df(output_id = "T-14-3-1", line = 1,
                 left = "MedDRA version {MEDDRA}."),
  tokens = df(output_id = c(NA, NA, NA, NA, "T-14-3-1"),
              name = c("SPONSOR", "STUDY", "CUTOFF", "MEDDRA", "CUTOFF"),
              value = c("ACME Pharma", "ABC-123", "Data cut-off: 01JUN2026",
                        "27.1", "Data cut-off: 01SEP2026")))
```

The demographics table gets the study’s header with its own two title
lines, the study’s run line, and the study’s tokens:

``` r

cat(tfl_report_code(spec, "T-14-1-1", content = "plan"), sep = "\n")
doc <- rtf_document(
  watermark = "DRAFT",
  program_fallback = "programs/T-14-1-1",
  tokens = list(
    SPONSOR = "ACME Pharma",
    STUDY = "ABC-123",
    CUTOFF = "Data cut-off: 01JUN2026",
    MEDDRA = "27.1"
  ),
  file = "output/T-14-1-1.rtf"
) |>
  rtf_section(
    header = rtf_header(
      list(
        c(l = "{SPONSOR}", r = "{CUTOFF}"),
        c(l = "Protocol: {STUDY}", r = "Page {PAGE} of {TOTAL_PAGES}"),
        c(c = "Table 14.1.1"),
        c(c = "Demographics")
      )
    ),
    footer = rtf_footer(list(c(l = "{PROGRAM}   Generated on: {DATETIME}")))
  ) |>
  rtf_tables(plan)
```

The AE table has its own cut-off (its `CUTOFF` row replaces the
study’s), a footnote, and no run line (its line 99 says `(none)`):

``` r

cat(tfl_report_code(spec, "T-14-3-1", content = "plan"), sep = "\n")
doc <- rtf_document(
  watermark = "DRAFT",
  program_fallback = "programs/T-14-3-1",
  tokens = list(
    SPONSOR = "ACME Pharma",
    STUDY = "ABC-123",
    MEDDRA = "27.1",
    CUTOFF = "Data cut-off: 01SEP2026"
  ),
  file = "output/T-14-3-1.rtf"
) |>
  rtf_section(
    header = rtf_header(
      list(
        c(l = "{SPONSOR}", r = "{CUTOFF}"),
        c(l = "Protocol: {STUDY}", r = "Page {PAGE} of {TOTAL_PAGES}"),
        c(c = "Table 14.3.1")
      )
    )
  ) |>
  rtf_tables(plan) |>
  rtf_footnotes(list(list(c(l = "MedDRA version {MEDDRA}."))))
```

## Tokens

A cell of the header, footer, titles or footnotes may say a token:

- **rtfreporter’s own**, filled when the file is written: `{PAGE}`,
  `{TOTAL_PAGES}`, `{PROGRAM}` (or `{PROGRAM_FULL}`, its absolute path)
  and `{DATETIME}`. `{PROGRAM}` is the report sheet’s `program` joined
  to `program_dir`; with `program` blank, it is the file name of the
  program that runs (and the report’s id only when none is found).
- **one’s own**, from the `tokens` sheet: a name in upper case (a
  letter, then letters, digits or `_`), not one of rtfreporter’s. A
  blank `output_id` is the study’s default; a report’s row of the same
  name replaces it, and `(none)` takes it out. They go to
  `rtf_document(tokens = )`.

A value that is the same for every report – the sponsor, the protocol,
the cut-off – is written once, as a study row of the `tokens` sheet; the
header and footer say `{SPONSOR}` and never repeat it.

## Each report’s own tokens

A report’s header, footer, titles and footnotes may also say its own
tokens, so that one header serves every report:

- `{OUTPUT_ID}` – the report’s id;
- `{OUTPUT_LABEL}` – made from the id: “T-14-1-1” is “Table 14.1.1” (the
  kind from the first letter, the number from the first digit,
  separators made dots); `tokens` rows `OUTPUT_KIND_TABLE` / `_LISTING`
  / `_FIGURE` give other words;
- `{OUTPUT_TITLE}`, `{OUTPUT_POPULATION}`, `{OUTPUT_SECTION}`,
  `{STUDY_ID}` – blank unless the `tokens` sheet gives them (tflplanner
  writes them from a TOC).

A band line left with nothing but empty tokens (and brackets:
`<{OUTPUT_POPULATION}>`) is not printed.
[`tfl_report_tokens()`](https://ichirio.github.io/tflspec/reference/tfl_report_tokens.md)
shows the values a report fills them with:

``` r

sp <- tfl_table_spec(
  report = df(output_id = c("T-14-1-1", "T-14-3-1"), type = "table"),
  page = df(output_id = c(NA, "T-14-3-1"), font = c("Courier New", NA),
            font_size_half_points = c("18", "16")),
  header = df(output_id = NA, line = c("1", "2", "3", "4", "5"),
              left = c("{COMPANY}", "PROTOCOL: {STUDY_ID}", NA, NA, NA),
              center = c(NA, NA, "{OUTPUT_LABEL}", "{OUTPUT_TITLE}",
                         "<{OUTPUT_POPULATION}>"),
              right = c(NA, "Page {PAGE} of {TOTAL_PAGES}", NA, NA, NA)),
  tokens = df(output_id = c(NA, NA, "T-14-1-1", "T-14-1-1", "T-14-3-1"),
              name = c("COMPANY", "STUDY_ID", "OUTPUT_TITLE",
                       "OUTPUT_POPULATION", "OUTPUT_TITLE"),
              value = c("Sample Pharma", "ABC-123",
                        "Demographic Characteristics", "Safety Analysis Set",
                        "Treatment-Emergent Adverse Events")))
tfl_report_tokens(sp, "T-14-1-1")
#>                       COMPANY                  OUTPUT_LABEL 
#>               "Sample Pharma"                "Table 14.1.1" 
#>                  OUTPUT_TITLE             OUTPUT_POPULATION 
#> "Demographic Characteristics"         "Safety Analysis Set" 
#>                      STUDY_ID 
#>                     "ABC-123"
```

## The company’s font, and the study’s setup written once

The `page` sheet’s `font` and `font_size_half_points` set the document’s
font. The study’s row (a blank `output_id`) is the company’s; a report’s
row that says its own wins (the AE table above is a size smaller).
Blank: rtfreporter’s own (Courier New, 9 pt).

What every report shares – that font, the study’s tokens, the running
header and footer –
[`tfl_report_setup_code()`](https://ichirio.github.io/tflspec/reference/tfl_report_setup_code.md)
writes once, as the code a study’s report programs source:

``` r

cat(tfl_report_setup_code(sp), sep = "\n")
options(
  rtfreporter.font = "Courier New",
  rtfreporter.font_size_half_points = 18
)
options(
  rtfreporter.tokens = list(COMPANY = "Sample Pharma", STUDY_ID = "ABC-123")
)
study_header_lines <- list(
  c(l = "{COMPANY}"),
  c(l = "PROTOCOL: {STUDY_ID}", r = "Page {PAGE} of {TOTAL_PAGES}"),
  c(c = "{OUTPUT_LABEL}"),
  c(c = "{OUTPUT_TITLE}"),
  c(c = "<{OUTPUT_POPULATION}>")
)
study_header <- rtf_header(study_header_lines, drop_empty_rows = TRUE)
```

A report’s program written with `tfl_report_code(setup = TRUE)` then
says only what is its own – its tokens, a font of its own – and the
study’s values are in one place, in the definition and in the code:

``` r

cat(tfl_report_code(sp, "T-14-3-1", content = "plan", setup = TRUE), sep = "\n")
doc <- rtf_document(
  default_format = rtf_default_format(font_size_half_points = 16),
  program_fallback = "T-14-3-1",
  tokens = list(
    OUTPUT_LABEL = "Table 14.3.1",
    OUTPUT_TITLE = "Treatment-Emergent Adverse Events",
    OUTPUT_POPULATION = ""
  ),
  file = "T-14-3-1.rtf"
) |>
  rtf_section(header = study_header) |>
  rtf_tables(plan)
```

A program standing alone (`setup = FALSE`, the default) says them
itself; the file is the same either way.

## Page patterns

Some reports want a page that differs from the study’s: a compact
landscape page, another footer. The report sheets (`report`, `page`,
`header`, `footer`, `titles`, `footnotes`, `tokens`) hold **named page
patterns** besides the blank rows: rows whose `output_id` is `@` and a
name (`@Compact`), saying only what differs from Standard (the blank
rows). A report chooses one in `report$pattern` (blank: Standard). Its
own rows win over its pattern’s, and the pattern’s over Standard’s, by
each sheet’s usual rules; `(none)` takes a line out at either level.
What a pattern changes is written in its reports’ programs; Standard
stays in the study’s setup.

## The document

[`tfl_report()`](https://ichirio.github.io/tflspec/reference/tfl_report.md)
makes the document from the content – a plan, `rtftable` pages, or a
plain data frame as here;
[`tfl_report_path()`](https://ichirio.github.io/tflspec/reference/tfl_report_path.md)
says where it goes (`output_path` and the report’s `file`):

``` r

content <- data.frame(Statistic = "n", `Drug X` = "80", Control = "78", check.names = FALSE)
doc <- tfl_report(spec, "T-14-1-1", content = content)
tfl_report_path(spec, "T-14-1-1")
#> [1] "output/T-14-1-1.rtf"
```

``` r

generate_rtfreport(doc, tfl_report_path(spec, "T-14-1-1"), overwrite = TRUE)
```

## The example workbook

`report.xlsx` in the package is a study’s report spec for the five
example tables: the sponsor and protocol on lines 1 and 2 of the default
header, each table’s titles on its lines 3 to 6, a run line 99 in the
default footer, and a PK table that puts its run line under the table
instead (`page_footer = FALSE`).

``` r

ex <- system.file("extdata", "ard-spec", package = "tflspec")
rs <- tfl_read_report_spec(file.path(ex, c("report.xlsx", "study.xlsx")))
rs$report[, c("output_id", "type", "file", "page_footer", "note")]
#>   output_id  type            file page_footer                              note
#> 1      <NA> table {output_id}.rtf        <NA>              the study's defaults
#> 2        LB  <NA>            <NA>        <NA>         one section per parameter
#> 3        PK  <NA>            <NA>       FALSE the run line goes under the table
```

## A company’s TOC as report specs

A study’s list of outputs usually exists before any table, in the
company’s own workbook.
[`tfl_read_toc()`](https://ichirio.github.io/tflspec/reference/tfl_read_toc.md)
reads it through a map of its columns, and gives the `report`, `titles`
and `footnotes` sheets – every report exists, titled, before a table is
written:

``` r

toc <- file.path(tempdir(), "toc.csv")
write.csv(df(
  `Output No.` = c("", "T-14-1-1", "T-14-3-1", "L-16-2-7"),
  Title = c("14.1 Demographics", "Demographic Characteristics",
            "Treatment-Emergent Adverse Events", "Listing of Serious AEs"),
  `Analysis Set` = c("", "Safety Analysis Set", "Safety Analysis Set",
                     "Safety Analysis Set"),
  Datasets = c("", "ADSL", "ADSL, ADAE", "ADAE"),
  check.names = FALSE), toc, row.names = FALSE)

sp <- tfl_read_toc(toc, map = list(output_id = "Output No.", title = "Title",
                                   population = "Analysis Set",
                                   datasets = "Datasets"))
sp$report[, c("output_id", "type")]
#>   output_id    type
#> 1  T-14-1-1   table
#> 2  T-14-3-1   table
#> 3  L-16-2-7 listing
sp$titles[, c("output_id", "line", "center")]
#>   output_id line                            center
#> 1  T-14-1-1    1       Demographic Characteristics
#> 2  T-14-1-1    2               Safety Analysis Set
#> 3  T-14-3-1    1 Treatment-Emergent Adverse Events
#> 4  T-14-3-1    2               Safety Analysis Set
#> 5  L-16-2-7    1            Listing of Serious AEs
#> 6  L-16-2-7    2               Safety Analysis Set
attr(sp, "sections")
#>            T-14-1-1            T-14-3-1            L-16-2-7 
#> "14.1 Demographics" "14.1 Demographics" "14.1 Demographics"
attr(sp, "datasets")
#>      T-14-1-1      T-14-3-1      L-16-2-7 
#>        "ADSL" "ADSL | ADAE"        "ADAE"
```

The kind of each report comes from its id (`T-`, `L-`, `F-`) when the
TOC does not say it; a heading row (“14.1 Demographics”) is each
report’s section. Write the result with
`tfl_write_specs("spec/report.xlsx", report = sp)` (or
[`tfl_write_report_spec()`](https://ichirio.github.io/tflspec/reference/tfl_write_table_spec.md)),
and open it in Excel or in tflplanner.
