# The ADaM metadata to give an AI assistant

One data frame, one row a variable, of what an assistant may name when
it writes specs: the datasets, their variables, labels and kinds, and
each variable's levels (its code list's values). It is what
[`tflspec_ai_manual()`](https://ichirio.github.io/tflspec/reference/tflspec_ai_manual.md)
writes as `08_adam_metadata.csv`, what
[`tfl_ai_context()`](https://ichirio.github.io/tflspec/reference/tfl_ai_context.md)
takes as `datasets`, and what
[`tfl_ai_review()`](https://ichirio.github.io/tflspec/reference/tfl_ai_review.md)
checks a spec's names against.

## Usage

``` r
tfl_adam_metadata(
  define = NULL,
  adam_spec = NULL,
  datasets = NULL,
  max_levels = 30L
)
```

## Arguments

- define:

  A `define.xml`, or a
  [`tfl_read_define()`](https://ichirio.github.io/tflspec/reference/tfl_read_define.md)
  result.

- adam_spec:

  An ADaM spec workbook (`.xlsx`, read by
  [`tfl_read_define_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_define_spec.md)
  with its default profile), or a
  [`tfl_read_define_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_define_spec.md)
  result.

- datasets:

  The ADaM datasets: a named list of data frames
  (`list(ADSL = adsl, ADAE = adae)`), or a metadata frame of this form
  already.

- max_levels:

  For `datasets`: the most distinct values a character column may have
  for them to be listed as its levels.

## Value

A data frame: `dataset`, `dataset_label`, `variable`, `label`, `kind`
(`character`, `numeric`, `date`, `factor`, `logical`), `levels` (the
values, `" | "` between them; `NA` for none), `codelist` (the code
list's name or OID; `NA` for none) and `source` (`"define.xml"`,
`"ADaM spec"` or `"datasets"`), with `attr(, "source")`.

## Details

Give **one** source. When several are given, the first of this order is
used, and a message says which:

1.  `define`: the study's `define.xml`
    ([`tfl_read_define()`](https://ichirio.github.io/tflspec/reference/tfl_read_define.md)),
    the delivered datasets as described for submission;

2.  `adam_spec`: the ADaM specification workbook
    ([`tfl_read_define_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_define_spec.md)),
    for when the define is older than the data or not written yet;

3.  `datasets`: the datasets themselves, of which only the column
    information is taken: the names, the labels (the `label` attribute),
    the kinds, the levels of a factor, and the distinct values of a
    short coded character column (at most `max_levels` of them). A
    subject's values never are: identifiers (`...ID`), dates and times
    (`...DTC`, `...DT`, `...DTM`, `...TM`) and free text (a value longer
    than 40 characters) are left without levels.

Two sources that disagree (a variable renamed in the spec but not yet in
the define) would leave the assistant to choose, which is why one is
used.

## See also

[`tflspec_ai_manual()`](https://ichirio.github.io/tflspec/reference/tflspec_ai_manual.md)
for the kit it goes into.

## Examples

``` r
if (requireNamespace("xml2", quietly = TRUE)) {
  md <- tfl_adam_metadata(define = system.file("extdata", "define",
                                               "define.xml",
                                               package = "tflspec"))
  md[md$variable %in% c("SEX", "AGEGR1"), c("dataset", "variable", "levels")]
}
#>    dataset variable     levels
#> 3     ADSL   AGEGR1 <65 | >=65
#> 4     ADSL      SEX      M | F
#> 10    ADAE      SEX      M | F
# from the data: names, labels, kinds and short code lists only
adsl <- data.frame(USUBJID = c("01-001", "01-002"), SEX = c("F", "M"),
                   AGE = c(63, 71))
tfl_adam_metadata(datasets = list(ADSL = adsl))
#>   dataset dataset_label variable label      kind levels codelist   source
#> 1    ADSL          <NA>  USUBJID  <NA> character   <NA>     <NA> datasets
#> 2    ADSL          <NA>      SEX  <NA> character  F | M     <NA> datasets
#> 3    ADSL          <NA>      AGE  <NA>   numeric   <NA>     <NA> datasets
```
