# A prompt for an AI agent to survey a company's past TFLs for their look

Writes what an AI agent needs to tell how a company's past reports look,
so that a company standard can be written from it: the prompt
(`ai_style_prompt.md`) and the numbers R read out of the RTF files
(`ai_style_scan.csv`,
[`tfl_rtf_style_scan()`](https://ichirio.github.io/tflspec/reference/tfl_rtf_style_scan.md):
cell widths, borders, sizes, alignment – no text of the reports). The
agent opens the RTF files where it needs to, counts the forms – where
the lines are drawn, the stub's width to the value columns', the fonts,
how titles and footnotes are aligned – and writes the most common ones,
with their counts, to `out`, which
[`tfl_ai_style_read()`](https://ichirio.github.io/tflspec/reference/tfl_ai_style_read.md)
reads. Nothing is sent anywhere; nothing of the reports' text is
written.

## Usage

``` r
tfl_ai_style_prompt(rtf_dir, out = NULL, dir = NULL)
```

## Arguments

- rtf_dir:

  The folder of the past RTF reports.

- out:

  The file the agent writes its survey to (YAML); its folder gets the
  prompt and the numbers too. `NULL`: `ai_style.yml` under `standards/`
  of tflspec's data folder
  ([`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html)).

- dir:

  Instead of `out`: the folder (`out` is `ai_style.yml` in it).

## Value

The prompt (a `tfl_ai_text`), with the files' paths in its attribute
`files` (`prompt`, `scan`, `result`).

## See also

[`tfl_ai_style_read()`](https://ichirio.github.io/tflspec/reference/tfl_ai_style_read.md)
