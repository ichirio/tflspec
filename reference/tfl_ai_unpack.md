# Write an AI assistant's answer into a study's spec files

The AI kit
([`tflspec_ai_manual()`](https://ichirio.github.io/tflspec/reference/tflspec_ai_manual.md))
asks the assistant for one answer holding every file of the study's YAML
spec: each file a heading with its path (`### spec/outputs/T-1.yml`) and
its content in a ```` ```yaml ```` block, the way the kit's example
(`06_example_spec.md`) is written. `tfl_ai_unpack()` writes those files
into a study folder, ready for
[`tfl_ai_review()`](https://ichirio.github.io/tflspec/reference/tfl_ai_review.md)
and the study's other functions
([`tfl_read_study_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_study_spec.md),
[`tfl_read_outputs()`](https://ichirio.github.io/tflspec/reference/tfl_read_study_spec.md)).

## Usage

``` r
tfl_ai_unpack(answer, dir, overwrite = FALSE)
```

## Arguments

- answer:

  The answer: its text (a character vector, one element a line or the
  whole text), the path of a `.md` / `.txt` file holding it, or a
  `table` answer read by
  [`tfl_ai_parse()`](https://ichirio.github.io/tflspec/reference/tfl_ai_parse.md).

- dir:

  The study folder to write into.

- overwrite:

  Write over files already there?

## Value

Invisibly, the paths written.

## Details

The answer to a `table` prompt
([`tfl_ai_prompt()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prompt.md))
– one block, its `outputs:` the report files of a batch – is written
too: give the answer read by
[`tfl_ai_parse()`](https://ichirio.github.io/tflspec/reference/tfl_ai_parse.md)
(or its text). Each report becomes `spec/outputs/<id>.yml`, written as
[`tfl_write_output()`](https://ichirio.github.io/tflspec/reference/tfl_read_output.md)
writes one, and its assumptions join `spec/_assumptions.yml` (the items
of these reports replaced, the others kept), so the batches of a study
add up in one folder. Check the answer first
([`tfl_ai_check()`](https://ichirio.github.io/tflspec/reference/tfl_ai_check.md)):
a report that could not be read is not written.

Only spec files are written: `study.yml`, and `.yml` files under `spec/`
(`spec/data.yml`, `spec/toc.yml`, `spec/defaults.yml`,
`spec/patterns/<name>.yml`, `spec/outputs/<id>.yml`,
`spec/_assumptions.yml`). A block under any other heading, a path that
leaves the folder, or a file twice is an error that names it, and
nothing is written. Each file is checked as YAML before any is written.

## See also

[`tflspec_ai_manual()`](https://ichirio.github.io/tflspec/reference/tflspec_ai_manual.md),
[`tfl_ai_review()`](https://ichirio.github.io/tflspec/reference/tfl_ai_review.md)

## Examples

``` r
answer <- c("### spec/outputs/T-1.yml", "", "```yaml", "id: T-1", "kind: table",
            "content:", "  - table_plan: {cols: TRT01A}", "```")
d <- tempfile("study-")
tfl_ai_unpack(answer, d)
tfl_read_output(file.path(d, "spec", "outputs", "T-1.yml"))
#> <tfl_output> T-1 (table)
#>   extends: defaults
#>   content:
#>     - table_plan(cols)
```
