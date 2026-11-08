# The look of a folder of RTF reports, as numbers

Reads every `.rtf` file of a folder (and its subfolders) and returns,
one row a table row, what its look is made of – never its text: the
cells' widths (`\cellx`), each cell's top and bottom border (`\clbrdrt`,
`\clbrdrb`), the font size (`\fs`), the cells' alignment (`\ql`, `\qc`,
`\qr`), and the file's fonts (its font table). Rows in a run, with
nothing but row definitions between them, are one table. A table that
goes on over pages, as SAS's ODS RTF writes it (a page a section, the
column header rows, `\trhdr`, again at the top of each), is one table:
its later pages' header rows are dropped, so a header counts once.

## Usage

``` r
tfl_rtf_style_scan(rtf_dir)
```

## Arguments

- rtf_dir:

  The folder of RTF files.

## Value

A data frame: `file` (its path under `rtf_dir`), `table` (its place in
the file), `row` (in the table), `page` (the file's page it is on),
`header` (a column header row, `\trhdr`), `n_cells`, `width_twips` (the
row's width), `rel_widths` (each cell's width as parts of the narrowest,
`|` between them), `border_top`, `border_bottom` (one character a cell:
`1` a border, `0` none), `font_size_pt`, `align` (one character a cell:
`l`, `c`, `r`), `fonts` (the file's font table, `|` between),
`font_uses` (the fonts the row's text switches to, `\fN`, by name, one
for each use, `|` between).

## See also

[`tfl_ai_style_prompt()`](https://ichirio.github.io/tflspec/reference/tfl_ai_style_prompt.md)
