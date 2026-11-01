# Check a study's SPEC against the packages installed: one call

Everything tflspec can say about a study's reports before their programs
run, in one table, each row at one of three levels – the person's
definition of 2026-10-29:

## Usage

``` r
tfl_check_spec(study, output_id = NULL, build = FALSE)
```

## Arguments

- study:

  A study read with
  [`tfl_read_study_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_study_spec.md),
  or its folder.

- output_id:

  The reports to check; `NULL` for every report.

- build:

  `TRUE`: each report the checks above pass is also built
  ([`tfl_check_build()`](https://ichirio.github.io/tflspec/reference/tfl_check_build.md))
  – its data read, its ARD made, its table, listing or figure object
  made, no RTF – so that a report with no row can be made; one that
  cannot has an `error` row, `part` `"build"`. A few seconds a report.
  `FALSE` (default): the checks above only.

## Value

A data frame, one row a problem: `output_id` (`NA` for the study's ARD
definition as a whole), `part` (`"file"`, `"ard"`, `"content"`,
`"program"`, `"figure"`, `"build"`), `item` (the file, the analysis, the
function and argument, or the figure's part and field), `level`
(`"error"`, `"warning"`, `"note"`, `"check"`) and `message`. Its
attributes: `versions`, a data frame of the key packages (`package`,
`installed`, `NA` when not installed); `ids`, the reports checked (the
OK ones too); `kinds`, each report's kind, named by id; `outputs_dir`,
the folder the report files are in, from the study folder.

## Details

- `error`: the file cannot be read as a SPEC (its YAML, or a key or a
  value the schema does not have, an analysis the ARD definition
  refuses): the report cannot be copied into the study;

- `warning`: the SPEC reads, but a program that runs cannot be written
  from it (the ARD program or the table's code stops; a content function
  the installed rtfreporter does not have yet);

- `note`: the program is written all the same – an argument the
  installed cards / cardx does not know, a version to look at.

A report with no `error` and no `warning` row is OK: its code is written
(whether the report is right is another matter). What the AI marked to
confirm (`ai: confirm`,
[`tfl_ai_confirm()`](https://ichirio.github.io/tflspec/reference/tfl_ai_confirm.md))
is a row of level `check`, apart from the three: it does not keep a
report from being adopted.
[`tfl_check_reports()`](https://ichirio.github.io/tflspec/reference/tfl_check_reports.md)
gives the three levels a report at a time, with its confirms;
[`tfl_check_fix_text()`](https://ichirio.github.io/tflspec/reference/tfl_check_fix_text.md)
writes, for a report at `error` or `warning`, the text to paste into the
agent's chat.

Each report is read and checked on its own: a file that cannot be read
is an `error` of that report (by its file's name) and stops no other.
The checks: each report's ARD definition against the installed cards /
cardx
([`tfl_ard_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_ard_spec.md),
written as its program,
[`tfl_ard_code()`](https://ichirio.github.io/tflspec/reference/tfl_ard_code.md)),
its content items against the schema and the installed rtfreporter
([`tfl_schema_check()`](https://ichirio.github.io/tflspec/reference/tfl_schema_check.md),
the items of an appended part too; its code written,
[`tfl_content_code()`](https://ichirio.github.io/tflspec/reference/tfl_content_code.md)),
and a figure's design against the installed ggplot2 and the version it
is written for
([`tfl_check_fig_design()`](https://ichirio.github.io/tflspec/reference/tfl_fig_design.md)).
The packages' installed versions come with it, as an attribute. A GUI's
check button and the AI's run call it.

## See also

[`tfl_check_reports()`](https://ichirio.github.io/tflspec/reference/tfl_check_reports.md),
[`tfl_check_fix_text()`](https://ichirio.github.io/tflspec/reference/tfl_check_fix_text.md),
[`tfl_ard_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_ard_spec.md),
[`tfl_schema_check()`](https://ichirio.github.io/tflspec/reference/tfl_schema_check.md),
[`tfl_check_fig_design()`](https://ichirio.github.io/tflspec/reference/tfl_fig_design.md)

## Examples

``` r
if (requireNamespace("haven", quietly = TRUE)) {
  # a study of the bundled ADSL, with one report
  d <- file.path(tempfile(), "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  chr <- function(x) if (is.factor(x)) as.character(x) else x
  haven::write_xpt(as.data.frame(lapply(adsl, chr)), file.path(d, "adam", "adsl.xpt"))
  tfl_ai_study_init(d, adam = "adam", study_id = "EX-1")
  tfl_write_output(
    tfl_output("T-DM", "table", population = "SAF",
               template = list(name = "demog", args = list(population = "SAF"))),
    file.path(d, "spec", "outputs", "T-DM.yml"))
  p <- tfl_check_spec(d)
  p
  tfl_check_reports(p)
  attr(p, "versions")
}
#>       package   installed
#> 1     tflspec 0.0.24.9238
#> 2 rtfreporter  0.8.2.9100
#> 3       cards       0.9.0
#> 4       cardx       0.3.4
#> 5     ggplot2       4.0.3
```
