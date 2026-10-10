# The AI assistant manual that ships with this package

tflspec is too new to be in any chat model's training data: asked for a
table specification or the program made from one, an assistant invents
sheet columns and function names and flags neither as a guess. The fix
is to give it the facts first, and this file is those facts – the
specification formats (ARD, table, report, listing, figure design), the
functions that turn them into R code, what that code looks like, the
former names that are refused, and the complete export list – sized to
sit in one chat session.

## Usage

``` r
tflspec_ai_manual(file = NULL, overwrite = FALSE)
```

## Arguments

- file:

  Optional destination. When given, the manual is copied there (ready to
  attach to a chat session) and the destination is returned invisibly. A
  directory is accepted, and the file keeps its own name.

- overwrite:

  Overwrite `file` if it already exists. Default `FALSE`.

## Value

The path to the manual – the installed file when `file` is `NULL`,
otherwise the copy, returned invisibly.

## Details

`tflspec_ai_manual()` returns the path to the copy **installed with this
package**, so the manual you attach always describes the version you
actually have. For the rendering side attach rtfreporter's own manual
instead
([`rtfreporter::rtfreporter_ai_manual()`](https://ichirio.github.io/rtfreporter/reference/rtfreporter_ai_manual.html)).

## Examples

``` r
# where the manual lives
tflspec_ai_manual()
#> [1] "/home/runner/work/_temp/Library/tflspec/ai/tflspec-ai-user-manual.md"

# read it here, or copy it out to attach to a chat session
writeLines(head(readLines(tflspec_ai_manual()), 3))
#> # tflspec — AI user manual
#> 
#> **This manual documents tflspec 0.0.24.9083** (the development version,
tflspec_ai_manual(file = tempfile(fileext = ".md"))
```
