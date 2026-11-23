# Read the company's hints an AI agent wrote from its past TFLs

The hints
[`tfl_ai_style_prompt()`](https://ichirio.github.io/tflspec/reference/tfl_ai_style_prompt.md)
asks the agent to write (`company_hints.yml`): by kind of report, each
convention's form and how many of the files the agent read show it. A
form fewer than half of them show is kept, but to be adopted by hand
only (`adopt`), as is a convention whose name is none of those the
prompt asks for. (Study agents read the file as it is; adopting is only
for starting a company standard, see
[`tfl_ai_style_diff()`](https://ichirio.github.io/tflspec/reference/tfl_ai_style_diff.md).)

## Usage

``` r
tfl_ai_style_read(file = NULL)
```

## Arguments

- file:

  The agent's file (YAML), with the header `tfl_company_hints`; `NULL`:
  `company_hints.yml` in the company's AI folder (the option
  `tflspec.ai_home`, else `ai/` of tflspec's data folder,
  [`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html)).

## Value

A data frame, one row a convention, in the order written: `kind` (`all`
for every kind), `key`, `value`, `seen`, `of`, `evidence` (the kind's
files the agent read, `|` between), `adopt` (`"proposed"`, or
`"by hand"`: fewer than half the files, no count, or a name the prompt
does not know). Its attribute `text` is a short summary (a line a
convention and the conflicts' count), which
[`print()`](https://rdrr.io/r/base/print.html) shows; `files` / `tables`
the counts the file gives; `notes` a named character vector of the
notes, by kind (`""` the top level's); `conflicts` a data frame (`kind`,
`key`, `standard`, `files`, `seen`, `of`) of the points where the past
TFLs differ from the company standard (0 rows for none).

## See also

[`tfl_ai_style_diff()`](https://ichirio.github.io/tflspec/reference/tfl_ai_style_diff.md)
