# A CDISC ARS reporting event as tflspec specs

Turns a reporting event
([`tfl_read_ars_json()`](https://ichirio.github.io/tflspec/reference/tfl_read_ars_json.md),
or a
[`tfl_ars()`](https://ichirio.github.io/tflspec/reference/tfl_ars.md))
into the specs it says:

## Usage

``` r
tfl_ars_to_specs(ars, table = FALSE)
```

## Arguments

- ars:

  A
  [`tfl_ars()`](https://ichirio.github.io/tflspec/reference/tfl_ars.md)
  or
  [`tfl_read_ars_json()`](https://ichirio.github.io/tflspec/reference/tfl_read_ars_json.md).

- table:

  Also a table spec.

## Value

A list: `ard` (a
[`tfl_ard_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_ard_spec.md)),
`report` (a
[`tfl_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_table_spec.md)
holding the report sheets) and, with `table = TRUE`, `table`. Attribute
`unmapped`: a data frame `where`, `item`, `reason`.

## Details

- the **ARD spec**: analysis sets as `populations`, analyses as
  `analyses` rows (method, dataset, population, `where`, `by`,
  `variables`, `statistics`, `purpose`, `reason`), the datasets they
  read as `datasets` (without a path: ARS does not say where the ADaM
  is);

- the **report spec**: one `report` row per output in the list of
  contents' order, its file, and its display's titles, footnotes, header
  and footer;

- with `table = TRUE`, a **table spec** with the `variables` rows the
  groupings give: a grouping's listed groups as the variable's `levels`.

tfl_ars()'s own layout comes back as it was written (one row with
several variables, a hierarchy as one row); anyone else's analysis
becomes one row each. A method is read from tflspec's ids, else from
what its operations compute; an analysis whose method tflspec has no
keyword for (an ANOVA, SAS code only ...) is not made a row and is
listed in `unmapped`.

## See also

[`tfl_read_ars_json()`](https://ichirio.github.io/tflspec/reference/tfl_read_ars_json.md),
[`tfl_ars()`](https://ichirio.github.io/tflspec/reference/tfl_ars.md)
