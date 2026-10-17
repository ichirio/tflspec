# The TOC's tables in batches, one family each

A chat drafts a few reports well and many badly: the `table` prompt
([`tfl_ai_prompt()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prompt.md))
is made for 5 to 10 reports a request. This splits the TOC's tables into
batches of one family each – disposition, demographics, exposure,
adverse events, laboratory, vital signs, efficacy, PK, other, by the
words of the title (English or Japanese) – so that one request holds
tables that look alike, and a reviewed table of a family can be the
example for the rest of it
([`tfl_ai_context()`](https://ichirio.github.io/tflspec/reference/tfl_ai_context.md)'s
`examples`).

## Usage

``` r
tfl_ai_batches(x, size = 8L, types = "table", families = NULL)
```

## Arguments

- x:

  A
  [`tfl_ai_read_shell()`](https://ichirio.github.io/tflspec/reference/tfl_ai_read_shell.md)
  result (its `toc`), or a data frame with `output_id` and `title` (and
  optionally `type`, `family`).

- size:

  The reports a batch holds at most.

- types:

  The kinds of report kept (`type`; a report whose kind is not known is
  kept); `NULL` for all.

- families:

  Your own families: a named character vector of regular expressions
  (Perl) tried on the title before the built-in ones.

## Value

A data frame: `output_id`, `title`, `family`, `batch` (`ae-1`, `ae-2`
...), in the TOC's order within a batch; `split(x$output_id, x$batch)`
gives the requests.

## Examples

``` r
f <- system.file("ai", "example", "onco_toc_shell.xlsx", package = "tflspec")
b <- tfl_ai_batches(tfl_ai_read_shell(f), size = 2)
b
#>   output_id
#> 1  T-14-1-1
#> 2  T-14-3-1
#> 3  T-14-3-5
#> 4  T-14-2-1
#>                                                                        title
#> 1                                   Demographic and Baseline Characteristics
#> 2 Treatment-Emergent Adverse Events by System Organ Class and Preferred Term
#> 3  Shift from Baseline in Laboratory Normal Range Indicator at Cycle 2 Day 1
#> 4                          Best Overall Response and Objective Response Rate
#>         family          batch
#> 1 demographics demographics-1
#> 2           ae           ae-1
#> 3         labs         labs-1
#> 4     efficacy     efficacy-1
split(b$output_id, b$batch)
#> $`ae-1`
#> [1] "T-14-3-1"
#> 
#> $`demographics-1`
#> [1] "T-14-1-1"
#> 
#> $`efficacy-1`
#> [1] "T-14-2-1"
#> 
#> $`labs-1`
#> [1] "T-14-3-5"
#> 
```
