# An analysis as R: the code of a custom analysis

The call an analysis row of the definition stands for, written as the
`code` of a `custom` analysis (where `data` and `population` are the
analysis's data and analysis set), with the formats its method gives by
default written out: an analysis the columns cannot say starts from what
they say now, and gives the same ARD. The definition keeps the R (the
program written from it is never edited).

## Usage

``` r
tfl_ard_as_custom(spec, output_id, analysis_id)
```

## Arguments

- spec:

  A `tfl_ard_spec` (or the list of its sheets).

- output_id, analysis_id:

  The analysis.

## Value

A list: `code` (the R; `NA` for an analysis that runs others inside it,
or one run inside another), `formats` (the row's `formats` with its
method's defaults, as the column writes them; `NA` for none), `method`
(`"custom"`).
