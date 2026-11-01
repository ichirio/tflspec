# The look of a folder of RTF reports, as numbers

Reads every `.rtf` file of a folder (and its subfolders) and returns,
one row a table row, what its look is made of – never its text: the
cells' widths (`\cellx`), each cell's top and bottom border (`\clbrdrt`,
`\clbrdrb`), the font size (`\fs`), the cells' alignment (`\ql`, `\qc`,
`\qr`), and the file's fonts (its font table). Rows in a run, with
nothing but row definitions between them, are one table.

## Usage

``` r
tfl_rtf_style_scan(rtf_dir)
```

## Arguments

- rtf_dir:

  The folder of RTF files.

## Value

A data frame: `file` (its name), `table` (its place in the file), `row`
(in the table), `n_cells`, `width_twips` (the row's width), `rel_widths`
(each cell's width as parts of the narrowest, `|` between them),
`border_top`, `border_bottom` (one character a cell: `1` a border, `0`
none), `font_size_pt`, `align` (one character a cell: `l`, `c`, `r`),
`fonts` (the file's font table, `|` between).

## See also

[`tfl_ai_style_prompt()`](https://ichirio.github.io/tflspec/reference/tfl_ai_style_prompt.md)
