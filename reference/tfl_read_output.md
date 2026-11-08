# Read and write a report's YAML file

`tfl_read_output()` reads `spec/outputs/<id>.yml`: the `!r` and `!unset`
tags become
[`tfl_r()`](https://ichirio.github.io/tflspec/reference/tfl_r.md) and
[`tfl_unset()`](https://ichirio.github.io/tflspec/reference/tfl_r.md), a
`{fn:, args:}` map a
[`tfl_call()`](https://ichirio.github.io/tflspec/reference/tfl_r.md),
and each item of `content:` (`- plan_cells: {...}`) a
[`tfl_item()`](https://ichirio.github.io/tflspec/reference/tfl_r.md)
with `from = "own"`. A file whose top is `outputs:` holds several
reports (a family written in one go): a list of them comes back, each
with the file's `base:` merged under it as a plain list (the layering
with `defaults` is not done here). `tfl_write_output()` writes one
report back in the same form.

## Usage

``` r
tfl_read_output(path)

tfl_write_output(x, path)
```

## Arguments

- path:

  The file.

- x:

  A `tfl_output`.

## Value

`tfl_read_output()`: a `tfl_output`, or a list of them.
`tfl_write_output()`: `path`, invisibly.

## Several objects in one report

One report (one RTF) can hold several objects – tables, figures – on its
pages: `contents:` in place of `content:` names them. An object is a
list of items (it reads the report's `data:` / `ard:`), or a map of
`kind`, `data`, `content`, `template`, `plot` (its own data: a figure's
steps, a template). Its kind, when not said, is what its items make
(`table_plan` a table) or its template's. The report's `page: layout:`
rows say each object's place, as
[`rtfreporter::rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.html)
/ `rtf_figures()` take it: `object` (its name), `page_break` (each after
the first goes on under the one before unless `true`), `space_before`,
`first_rows`, `cont_title` (a table's), `titles`, `footnotes` (the
object's own, as `page: titles:` writes them), `width_in`, `height_in`
(a figure's). The report's `page: titles:` go above the first object,
its `page: footnotes:` below the last.
[`tfl_run_output()`](https://ichirio.github.io/tflspec/reference/tfl_run_output.md)
makes the RTF;
[`tfl_output_contents()`](https://ichirio.github.io/tflspec/reference/tfl_output_contents.md)
builds the objects.

    contents:
      demog: [{table_plan: {cols: TRT01A}}, ...]
      km:
        data: {steps: [{step: read, dataset: ADTTE}, {step: param, value: OS}]}
        template: {name: km_survival, args: {by: TRT01A}}
    page:
      layout:
        - {object: demog}
        - {object: km, page_break: false, space_before: 1, height_in: 3}

## Examples

``` r
f <- system.file("extdata", "spec-yaml", "outputs", "T-14-1-1.yml", package = "tflspec")
out <- tfl_read_output(f)
out$content[[1]]
#> <tfl_item> table_plan(cols, rows)
```
