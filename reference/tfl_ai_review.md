# Review the spec an AI assistant wrote

Checks what the assistant returned – a study folder its answer was
unpacked into
([`tfl_ai_unpack()`](https://ichirio.github.io/tflspec/reference/tfl_ai_unpack.md)),
or one file: a workbook holding any of the ARD, table, report and
listing specs, a figure design `.yml`, a report file of the YAML spec –
and sorts what it finds by **who acts on it**:

## Usage

``` r
tfl_ai_review(
  file,
  toc = NULL,
  adam = NULL,
  others = NULL,
  metadata = NULL,
  write = TRUE,
  overwrite = FALSE
)
```

## Arguments

- file:

  A study folder of the YAML spec (each report file against the schema;
  the definition through the workbook's checks, each finding named by
  its file and key; `spec/_assumptions.yml`), a spec workbook (`.xlsx`),
  a figure design (`.yml`), or one report of the YAML spec
  (`spec/outputs/<id>.yml`: its items against the schema, and its `!r`
  where the schema has a form of its own).

- toc:

  The TOC the specs were written from: a
  [`tfl_read_toc()`](https://ichirio.github.io/tflspec/reference/tfl_read_toc.md)
  result, or a list of its arguments (`path`, `map`, `sheet`, `skip`).
  With it, the outputs, titles and footnotes are checked against it.

- adam:

  The study's data
  ([`tfl_read_adam()`](https://ichirio.github.io/tflspec/reference/tfl_read_adam.md)),
  for the checks against the data; `NULL` for none.

- others:

  Specs of the same study already read, for the checks across specs when
  the file holds only some: a list with any of `ard`
  ([`tfl_read_ard_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_ard_spec.md)),
  `table`
  ([`tfl_read_report_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_report_spec.md))
  and `listing`
  ([`tfl_read_listing_spec()`](https://ichirio.github.io/tflspec/reference/tfl_listing_spec.md)).

- metadata:

  The study's ADaM metadata
  ([`tfl_adam_metadata()`](https://ichirio.github.io/tflspec/reference/tfl_adam_metadata.md),
  or the kit's `08_adam_metadata.csv` read back), for the checks of the
  names a workbook writes: a dataset or variable it does not have is the
  AI's to fix (K14, with the nearest names), and so is a code list value
  that is not among a variable's levels (K15). `NULL` for none.

- write:

  Write `<file>_repair.md` and `<file>_questions.md` next to the file.

- overwrite:

  Write over them if they are there.

## Value

A `tfl_ai_review`: the rows of
[`tfl_review_spec()`](https://ichirio.github.io/tflspec/reference/tfl_review_spec.md)
plus `who` (`ai` or `human`), with attributes `file`, `kinds`,
`assumptions` (the assistant's own list), `repair` and `questions` (the
two texts) and `paths` (the files written). Invisibly when
`write = TRUE`.

## Details

- **the AI**: what it can correct from the kit
  ([`tflspec_ai_manual()`](https://ichirio.github.io/tflspec/reference/tflspec_ai_manual.md))
  and the TOC: a sheet or column that is not the spec's, an output that
  is not the TOC's, a title or footnote that differs from it, `(N=86)`
  where `(N={n})` belongs, analyses no table uses, a missing
  `_assumptions` sheet, and every error and check of the review
  ([`tfl_review_spec()`](https://ichirio.github.io/tflspec/reference/tfl_review_spec.md),
  [`tfl_check_fig_design()`](https://ichirio.github.io/tflspec/reference/tfl_fig_design.md));

- **the person**: what only they can decide – each assumption the
  assistant listed, each analysis set's flag, each percentage's
  denominator, each test or model, each fixed order of levels – and the
  review's items that are set by hand.

The first half is written as a prompt to paste back into the same chat
(`<file>_repair.md`), the second as a checklist the person answers and
pastes back (`<file>_questions.md`), next to the file – a study folder's
in the folder, named after it. The kit's rules and who acts on each
review rule are `inst/review/ai_rules.csv`.

## See also

[`tflspec_ai_manual()`](https://ichirio.github.io/tflspec/reference/tflspec_ai_manual.md)
for the kit the assistant works from.

## Examples

``` r
# \donttest{
kit <- tflspec_ai_manual(file.path(tempdir(), "kit-review"), overwrite = TRUE)
#> tflspec AI kit: 7 files, 278 KB, in /tmp/RtmprDOQfE/kit-review
#>   00_START_HERE.md  (  5 KB)
#>   01_tflspec-ai-user-manual.md  ( 88 KB)
#>   02_tflspec-ai-toc-shell-guide.md  ( 39 KB)
#>   03_spec_schema.yml  (117 KB)
#>   04_blank_spec.md  (  5 KB)
#>   05_example_toc_shell.xlsx  ( 14 KB)
#>   06_example_spec.md  ( 10 KB)
#> Attach all files in /tmp/RtmprDOQfE/kit-review, plus your TOC and shells, and paste the request in 00_START_HERE.md.
# the example's answer, unpacked into a study folder
st <- file.path(tempdir(), "XYZ-001")
tfl_ai_unpack(kit[basename(kit) == "06_example_spec.md"], st, overwrite = TRUE)
r <- tfl_ai_review(st, write = FALSE)
summary(r)
#>        level
#> who     error check hand
#>   ai        0     0    0
#>   human     0     0   19
head(as.data.frame(r)[r$who == "human", c("output_id", "rule", "message")])
#>   output_id rule
#> 1       T-1  K10
#> 2       T-2  K10
#> 3      <NA>  K11
#> 4       T-1  K13
#> 5       T-1  K13
#> 6       T-2  K13
#>                                                           message
#> 1           T-1: the percentages of CAT are of the analysed rows.
#> 2             T-2: the percentages of AE are of the analysis set.
#> 3 The analysis set SAF is the subjects of ADSL with SAFFL == "Y".
#> 4               T-1: TRT01A prints in the order Drug X | Control.
#> 5                     T-1: AGEGR1 prints in the order <65 | >=65.
#> 6               T-2: TRT01A prints in the order Drug X | Control.
# }
```
