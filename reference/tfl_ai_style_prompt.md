# A learning kit for an AI agent to write a company's hints from its past TFLs

Writes what an AI agent needs to learn how a company's past reports are
made, by the kind of report, and to write it down as the company's hints
(tflspec#547): R's numbers of the reports' look (`learn/scan.csv`,
[`tfl_rtf_style_scan()`](https://ichirio.github.io/tflspec/reference/tfl_rtf_style_scan.md);
`learn/tables.csv`, one row a table,
[`tfl_rtf_style_tables()`](https://ichirio.github.io/tflspec/reference/tfl_rtf_style_tables.md))
and the prompt (`AGENTS-learn.md`). The agent opens a few files of each
kind for what R cannot read – the statistics and their labels, the n (%)
and header N forms, the decimals, the sort, the footnotes' wording – and
writes the conventions it found, a kind at a time, with how many of the
files it read show each, to `company_hints.yml`. The file is kept beside
the company standard, never merged into it: every study prepared for an
agent afterwards gets a copy, and its agent takes a hint where the
shell, the SAP, the study's documents and the company standard say
nothing, before its general knowledge.
[`tfl_ai_style_read()`](https://ichirio.github.io/tflspec/reference/tfl_ai_style_read.md)
shows it as a table. Nothing is sent anywhere; the files and what is
learned stay in the folder.

## Usage

``` r
tfl_ai_style_prompt(rtf_dir = NULL, dir = NULL, standard = NULL)
```

## Arguments

- rtf_dir:

  The folder of the past RTF reports (a subfolder a study). `NULL`:
  `past_tfl/` in `dir`.

- dir:

  The company's AI folder. `NULL`: the option `tflspec.ai_home`, else
  `ai/` of tflspec's data folder.

- standard:

  The company standard's conventions, or `NULL` for none: a data frame
  with `kind`, `key` and `value` (kind `all` for every kind), as
  [`tfl_ai_style_diff()`](https://ichirio.github.io/tflspec/reference/tfl_ai_style_diff.md)
  takes it. Given, it is written to `learn/standard.csv`, and the agent
  writes no hint for a point the standard says: where the past TFLs
  differ from it, it lists the point under `conflicts:`.

## Value

The prompt (a `tfl_ai_text`), with the files' paths in its attribute
`files` (`prompt`, `scan`, `tables`, `standard` – `NA` when none –,
`result`).

## Details

The folder is the company's AI folder (`dir`; default the option
`tflspec.ai_home`, else `ai/` of tflspec's data folder,
[`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html);
tflplanner sets it to `ai/` of its home): `past_tfl/` (the past RTF
reports, a subfolder a study: the person puts them there),
`AGENTS-learn.md`, `learn/` and `company_hints.yml`.

## See also

[`tfl_ai_style_read()`](https://ichirio.github.io/tflspec/reference/tfl_ai_style_read.md),
[`tfl_ai_style_diff()`](https://ichirio.github.io/tflspec/reference/tfl_ai_style_diff.md)
