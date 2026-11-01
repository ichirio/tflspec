# Make a report's content, from its spec file, without its code

Runs what
[`tfl_content_code()`](https://ichirio.github.io/tflspec/reference/tfl_content_code.md)
writes: the same items, in the same order, each applied with
[`do.call()`](https://rdrr.io/r/base/do.call.html) – for a table, the
data's steps (`normalize_ard()`), `table_plan()` and its layers. The two
read one list of calls, so running the written code gives the same
object; it is the quick way for a preview, the app or an assistant's
check. A report whose content is its own code (`content: !r`) runs it,
after the data's steps, and gives what it leaves in `content`.

## Usage

``` r
tfl_content_run(output, data)
```

## Arguments

- output:

  A report
  ([`tfl_output()`](https://ichirio.github.io/tflspec/reference/tfl_r.md)),
  resolved.

- data:

  The data the content starts from: the ARD (or the normalized ARD when
  the content has no `normalize_ard()` item). With `data.steps` (R that
  makes `data` from the output's `data.object`, `ard` by default), the
  object the steps start from. For a figure, a named list of the objects
  its code reads: the datasets by their names in lower case (`adtte`),
  and `ard`.

## Value

For a table, the
[`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html);
for a figure, the ggplot.

## See also

[`tfl_content_code()`](https://ichirio.github.io/tflspec/reference/tfl_content_code.md)

## Examples

``` r
if (requireNamespace("cards", quietly = TRUE)) {
  ard <- cards::ard_stack(cards::ADSL, .by = ARM,
                          cards::ard_tabulate(variables = SEX))
  out <- tfl_output("T-1", "table", content = list(
    tfl_item("normalize_ard", list()),
    tfl_item("table_plan", list(cols = "ARM", rows = list(group = "variable"))),
    tfl_item("plan_cells", list(categorical = "{n} ({p:.1f%})"))))
  p <- tfl_content_run(out, ard)
  class(p)
}
#> [1] "table_plan"
```
