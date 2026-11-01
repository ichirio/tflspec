# The decisions of a study's AI drafts

`ai/decisions.md` holds what the reports follow over the agent's own
choices, one line a decision:
`- D<n> [<scope>] <topic>: <decision> (<source>)`. `scope` is `all`, a
dataset, a TOC section or report ids; `topic` the item decided. The
agent adds the person's chat answers there
([`tfl_ai_prepare()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prepare.md))
and applies a decision to a later report when its scope covers it. A
line of the older form, `- Q<n> (<reports>): <decision>`, is read too
(its scope the reports, no topic), and a person's own line without a
scope is `all`.

## Usage

``` r
tfl_ai_decisions_read(study_dir)

tfl_ai_decisions_write(study_dir, x)
```

## Arguments

- study_dir:

  The study folder.

- x:

  The decisions to add: a data frame with `scope`, `topic`, `decision`,
  `source` (`source` may be left out: `"person"`).

## Value

`tfl_ai_decisions_read()`: a data frame, one row a decision: `id`,
`scope`, `topic`, `decision`, `source`, `line` (the line's number in the
file); no rows when the file has none.

## Details

`tfl_ai_decisions_read()` reads the file; `tfl_ai_decisions_write()`
adds decisions (a data frame with `scope`, `topic`, `decision` and
`source`; their ids are given) and returns the file's path.

## Examples

``` r
d <- file.path(tempfile(), "study")
dir.create(file.path(d, "ai"), recursive = TRUE)
tfl_ai_decisions_write(d, data.frame(scope = "all", topic = "treatment_var.SAF",
                                     decision = "TRT01A", source = "person"))
#> Warning: file("") only supports open = "w+" and open = "w+b": using the former
tfl_ai_decisions_read(d)
#>   id scope             topic decision source line
#> 1 D1   all treatment_var.SAF   TRT01A person    2
```
