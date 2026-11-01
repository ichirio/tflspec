# The ADaM datasets as a summary for an AI chat

What an AI chat needs to know of the study's data to draft its specs,
and nothing more: per dataset its label and size, and per variable its
name, label and type, with the values of a character or factor variable
that has few of them (SEX, AGEGR1, the arms, PARAMCD ...). No record is
written: identifiers, dates and times never have their values listed.
Paste the text into the chat; never attach the datasets themselves.

## Usage

``` r
tfl_ai_adam_summary(
  path,
  datasets = NULL,
  max_levels = 15L,
  file = NULL,
  define = NULL,
  adam_spec = NULL
)
```

## Arguments

- path:

  A folder of ADaM files (`.xpt`, also `.sas7bdat`, `.rds`), one file,
  or a named list of data frames. `.xpt` and `.sas7bdat` need the haven
  package.

- datasets:

  The datasets to keep (`"ADSL"`, `"ADAE"` ...); `NULL` for all.

- max_levels:

  A variable's values are listed when it has this many at most (PARAMCD,
  PARCAT, AVISIT, the grades: four times as many).

- file:

  A file to write the text to (UTF-8), or `NULL`.

- define, adam_spec:

  The study's `define.xml` (2.0 or 2.1), or its ADaM spec workbook in
  the Pinnacle 21 layout
  ([`tfl_read_define_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_define_spec.md)),
  or what either reader returns – one of them. What the define says is
  added to what the data shows: each code list's decodes
  (`[codes: M = Male | F = Female]`; the values listed stay the data's,
  in the code list's order where it has them), a variable's label the
  data lacks, the digits and display format of a numeric variable, and
  the value-level metadata (a variable's digits and format by where
  clause, `AVAL where PARAMCD == "BILI": digits 1, format 8.1`) as a
  section of its own. `NULL` (default): the data alone.

## Value

A `tfl_ai_adam`: a list with `text` (one string, Markdown), `variables`
(one row a variable: `dataset`, `dataset_label`, `variable`, `label`,
`kind`, `levels` – `|` between the values – as
[`tfl_adam_metadata()`](https://ichirio.github.io/tflspec/reference/tfl_adam_metadata.md)
gives it; `missing`, how many records have no value (`NA`, and `""` for
a character variable); and `codes`: what each code of a code variable
stands for, `"1 = <65 | 2 = >=65"`, from its value labels or the
variable it pairs with – `AGEGR1N` and `AGEGR1`, `ARMCD` and `ARM`,
`PARAMCD` and `PARAM` – when each code has one text) and `datasets`
(`dataset`, `label`, `records`, `subjects` – the distinct `USUBJID`s,
`NA` without one – `variables`), and `value_level` (the define's, one
row an entry; none without a define).
[`format()`](https://rdrr.io/r/base/format.html) gives the text.

## Details

The summary's `variables` frame is what
[`tfl_ai_context()`](https://ichirio.github.io/tflspec/reference/tfl_ai_context.md)
takes as `datasets`:
[`tfl_ai_check()`](https://ichirio.github.io/tflspec/reference/tfl_ai_check.md)
then checks every dataset and variable an answer names against it.

## See also

[`tfl_ai_read_shell()`](https://ichirio.github.io/tflspec/reference/tfl_ai_read_shell.md),
[`tfl_ai_context()`](https://ichirio.github.io/tflspec/reference/tfl_ai_context.md).

## Examples

``` r
s <- tfl_ai_adam_summary(list(adsl = tflspec::adsl, adae = tflspec::adae))
cat(substr(format(s), 1, 700))
#> # ADaM datasets: variables only
#> 
#> Each line: `NAME (type) Label: values [missing: ...]`. The values are listed for a character variable with few of them, as the data holds them; `[missing: 3 of 120]` is how many records have no value (NA, or blank); no record is shown. A code variable shows what each code stands for: `[codes: 1 = <65 | 2 = >=65]`.
#> 
#> ## ADSL (200 records, 200 subjects, 45 variables)
#> STUDYID (char) Study Identifier [missing: none]
#> USUBJID (char) Unique Subject Identifier [missing: none]
#> SUBJID (char) Subject Identifier for the Study [missing: none]
#> SITEID (char) Study Site Identifier [missing: none]
#> COUNTRY (char) Country: CAN | DEU | ESP | FRA | GBR | JPN | KOR | USA [missing: 
head(s$variables)
#>   dataset dataset_label variable                            label      kind
#> 1    ADSL          <NA>  STUDYID                 Study Identifier character
#> 2    ADSL          <NA>  USUBJID        Unique Subject Identifier character
#> 3    ADSL          <NA>   SUBJID Subject Identifier for the Study character
#> 4    ADSL          <NA>   SITEID            Study Site Identifier character
#> 5    ADSL          <NA>  COUNTRY                          Country character
#> 6    ADSL          <NA>  REGION1              Geographic Region 1 character
#>                                          levels missing codes
#> 1                                          <NA>       0  <NA>
#> 2                                          <NA>       0  <NA>
#> 3                                          <NA>       0  <NA>
#> 4                                          <NA>       0  <NA>
#> 5 CAN | DEU | ESP | FRA | GBR | JPN | KOR | USA       0  <NA>
#> 6                 ASIA | EUROPE | NORTH AMERICA       0  <NA>
# \donttest{
if (requireNamespace("haven", quietly = TRUE)) {
  d <- file.path(tempdir(), "adam")
  dir.create(d, showWarnings = FALSE)
  haven::write_xpt(tflspec::adsl, file.path(d, "adsl.xpt"))
  tfl_ai_adam_summary(d)
}
#> <tfl_ai_adam: 1 dataset(s), 45 variables, 3739 characters>
#>   ADSL (200 records, 200 subjects, 45 variables)
#> format(x) gives the text to paste.
# }
```
