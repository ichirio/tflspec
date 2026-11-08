# Write the company's reference pack for AI agents

Writes the company's conventions, by kind of report, as an AI agent
reads them (`conventions.md`, with `README.md`). Every study prepared
for an agent afterwards
([`tfl_ai_prepare()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prepare.md))
gets a copy in its `ai/reference/`, and its agent takes a convention
from it before the usual one (`from: "company convention"`); a study
prepared before gets the new pack the next time its agent files are
written.

## Usage

``` r
tfl_ai_reference_build(conventions, dir = NULL, notes = NULL)
```

## Arguments

- conventions:

  The conventions: a data frame with `kind`, `key` and `value` (kind
  `all` for every kind), such as
  [`tfl_ai_style_adopt()`](https://ichirio.github.io/tflspec/reference/tfl_ai_style_adopt.md)
  returns.

- dir:

  The pack's folder; `NULL`: `reference/` in the company's AI folder
  (the option `tflspec.ai_home`, else `ai/` of tflspec's data folder,
  [`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html)).

- notes:

  Lines the agent should read first (the company's own words), or
  `NULL`.

## Value

The files written, invisibly.

## See also

[`tfl_ai_style_adopt()`](https://ichirio.github.io/tflspec/reference/tfl_ai_style_adopt.md)

## Examples

``` r
d <- tempfile()
conv <- data.frame(kind = c("all", "demog"), key = c("font", "n_form"),
                   value = c("Courier New 9 pt", "(N=xx)"))
f <- tfl_ai_reference_build(conv, dir = d)
cat(readLines(f[["conventions"]]), sep = "\n")
#> # The company's conventions
#> 
#> How this company makes each kind of report. Take a point from here when the study's documents do not settle it, before the usual convention, and write it as an assumption with `from: "company convention"`. A study's documents (the shell, the SAP, a decision) win over this file.
#> 
#> A kind's own line wins over the same line under *Every kind*. The kinds: `demog` (demographics, baseline), `ae_soc_pt` (AEs by SOC and PT), `ae` (other AE tables), `param_change` (labs, vital signs, ECG by visit with change), `shift`, `listing`, `figure`, `other`.
#> 
#> ## Every kind
#> 
#> - `font`: Courier New 9 pt  _(the font and its size (`Courier New 9 pt`))_
#> 
#> ## `demog`
#> 
#> - `n_form`: (N=xx)  _(the column header's N (`(N=xx)`, `N = xx`))_
#> 
```
