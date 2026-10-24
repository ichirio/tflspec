# The ADaM datasets as a summary for an AI chat

What an AI chat needs to know of the study's data to draft its specs,
and nothing more: per dataset its label and size, and per variable its
name, label and type, with the values of a character or factor variable
that has few of them (SEX, AGEGR1, the arms, PARAMCD ...). No record is
written: identifiers, dates and times never have their values listed.
Paste the text into the chat; never attach the datasets themselves.

## Usage

``` r
tfl_ai_adam_summary(path, datasets = NULL, max_levels = 15L, file = NULL)
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

## Value

A `tfl_ai_adam`: a list with `text` (one string, Markdown), `variables`
(one row a variable: `dataset`, `dataset_label`, `variable`, `label`,
`kind`, `levels` – `|` between the values – as
[`tfl_adam_metadata()`](https://ichirio.github.io/tflspec/reference/tfl_adam_metadata.md)
gives it) and `datasets` (`dataset`, `label`, `records`, `variables`).
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
#> Each line: `NAME (type) Label: values`. The values are listed for a character variable with few of them; no record is shown.
#> 
#> ## ADSL (200 records, 45 variables)
#> STUDYID (char) Study Identifier
#> USUBJID (char) Unique Subject Identifier
#> SUBJID (char) Subject Identifier for the Study
#> SITEID (char) Study Site Identifier
#> COUNTRY (char) Country: CAN | DEU | ESP | FRA | GBR | JPN | KOR | USA
#> REGION1 (char) Geographic Region 1: ASIA | EUROPE | NORTH AMERICA
#> AGE (num) Age
#> AGEU (char) Age Units: YEARS
#> AGEGR1 (char) Pooled Age Group 1: <65 | >=65
#> AGEGR1N (num) Pooled Age Group 1 (N)
#> SEX (char) Sex: F | M
#> RACE (char) Race: ASIAN | BLACK OR AFRICAN AMERICAN | OTHER | WHIT
head(s$variables)
#>   dataset dataset_label variable                            label      kind
#> 1    ADSL          <NA>  STUDYID                 Study Identifier character
#> 2    ADSL          <NA>  USUBJID        Unique Subject Identifier character
#> 3    ADSL          <NA>   SUBJID Subject Identifier for the Study character
#> 4    ADSL          <NA>   SITEID            Study Site Identifier character
#> 5    ADSL          <NA>  COUNTRY                          Country character
#> 6    ADSL          <NA>  REGION1              Geographic Region 1 character
#>                                          levels
#> 1                                          <NA>
#> 2                                          <NA>
#> 3                                          <NA>
#> 4                                          <NA>
#> 5 CAN | DEU | ESP | FRA | GBR | JPN | KOR | USA
#> 6                 ASIA | EUROPE | NORTH AMERICA
# \donttest{
if (requireNamespace("haven", quietly = TRUE)) {
  d <- file.path(tempdir(), "adam")
  dir.create(d, showWarnings = FALSE)
  haven::write_xpt(tflspec::adsl, file.path(d, "adsl.xpt"))
  tfl_ai_adam_summary(d)
}
#> <tfl_ai_adam: 1 dataset(s), 45 variables, 2608 characters>
#>   ADSL (200 records, 45 variables)
#> format(x) gives the text to paste.
# }
```
