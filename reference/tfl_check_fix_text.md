# The text that asks the agent to fix a draft

For each report
[`tfl_check_spec()`](https://ichirio.github.io/tflspec/reference/tfl_check_spec.md)
put at `ERROR` or `WARNING`, the text to paste into the agent's chat:
which file, which report, what is wrong where (each `error` and
`warning` row), and what not to change – the common rules are in
`ai/AGENTS.md` and the instructions, which the text names rather than
repeats. In English, or in Japanese (`language = "ja"`); tflspec's own
messages stay as they are.

## Usage

``` r
tfl_check_fix_text(problems, language = "en", output_id = NULL)
```

## Arguments

- problems:

  [`tfl_check_spec()`](https://ichirio.github.io/tflspec/reference/tfl_check_spec.md)'s
  result.

- language:

  `"en"` (default) or `"ja"` (`"Japanese"` too).

- output_id:

  The reports to write the text for; `NULL` (default): every report at
  `ERROR` or `WARNING`.

## Value

A character vector named by report id, one text a report (its lines
joined with newlines); empty when no report needs a fix.

## See also

[`tfl_check_spec()`](https://ichirio.github.io/tflspec/reference/tfl_check_spec.md),
[`tfl_check_reports()`](https://ichirio.github.io/tflspec/reference/tfl_check_reports.md)
