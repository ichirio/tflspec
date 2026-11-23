# The settings `tfl_ai_prepare()` kept with a study

Reads `ai/prepare.yml` of the study folder: the arguments of the last
[`tfl_ai_prepare()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prepare.md)
call that wrote the requests, as that function takes them, for
[`tfl_ai_prepare()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prepare.md)
(`tfl_ai_prepare(dir)` repeats them) and for a screen that fills its
fields from them.

## Usage

``` r
tfl_ai_prepare_settings(dir)
```

## Arguments

- dir:

  The study folder.

## Value

A named list of the settings, the paths absolute again (a path of the
file that is relative is from `dir`), `toc_cols` a list; with the
attributes `file` (the file's path), `written` and `tflspec` – or `NULL`
when the study has no `ai/prepare.yml`. The paths are not checked.

## Details

The file is plain YAML, keys in a fixed order, strings quoted, the
`version:` of the format first (1), then `written:` (the date-time) and
`tflspec:` (the version that wrote it), then the settings under the
argument names of
[`tfl_ai_prepare()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prepare.md):

- paths – `toc`, `shells`, `adam`, `adam_spec`, `define`, `docs`: from
  the study folder, with forward slashes, when inside it; else as given
  (absolute);

- `toc_sheet`, `sheets`, `toc_cols`, `sheet_roles`, `study_id`, `title`,
  `size`, `language`, `workbook`, `max_chars`, `unit`, `templates`,
  `area`, `datasets`, `max_levels`, `agent_reads`, `agent_max_reads`:
  the values; a list (`sheets`, `shells`, `docs`, `datasets`,
  `agent_reads`) is a YAML list, `toc_cols` and `sheet_roles` a YAML
  map.

Only the arguments the call was given are in the file; a key that is
missing is the argument's default. Left out: `dir`, `overwrite`,
`dry_run`, `quiet`, `progress` and `add` (they describe the call, not
the settings), `adam_summary` (an R object) and `numbers` (a data frame:
`ai/sheet_numbers.csv` keeps the result and the next call reads it); a
value that is not plain text, numbers or `TRUE` / `FALSE` is left out
too, and named in a comment of the file. With `add = TRUE` the file
keeps the study's first workbook's settings (`toc`, `toc_sheet`,
`sheets`, `shells`, `toc_cols`, `sheet_roles`, `unit`); the other
settings the call gave replace the file's, and `docs`, `agent_reads` and
`datasets` are joined (in order, once each). A study with no file,
`add = TRUE`: none is written. The agent does not read the file.

## See also

[`tfl_ai_prepare()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prepare.md)

## Examples

``` r
d <- file.path(tempdir(), "no-prepare-yml")
tfl_ai_prepare_settings(d)
#> NULL
```
