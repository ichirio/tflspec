# What the AI marked to confirm in a study's drafts

The agent marks, in a report file's `ai: confirm`, what it could not
settle and the person decides in the builder: one row a mark, from the
drafts (`ai/spec/outputs/`) and the adopted reports (`spec/outputs/`).
[`tfl_check_spec()`](https://ichirio.github.io/tflspec/reference/tfl_check_spec.md)
lists them too (`level` `"check"`), and the AI review as rule K19.

## Usage

``` r
tfl_ai_confirm(study_dir, where = c("both", "drafts", "adopted"))
```

## Arguments

- study_dir:

  The study folder.

- where:

  `"drafts"`, `"adopted"` or `"both"` (default).

## Value

A data frame: `output_id`, `where`, `item`, `why`, `file`.
