# The methods an analysis row may name

An analysis's `method` is one of these keywords, or the name of any
function, `pkg::fun` (every `cards::ard_*` and `cardx::ard_*` among
them), which is called as `pkg::fun(data, by = , variables = , ...)`
with the analysis data first, `by` and `variables` when the row gives
them, and the row's `args` after them. A function whose first argument
is not the data (`cardx::ard_survival_survdiff(formula, data)`) takes it
the same way once `args` names the first one (`formula = ...`). The
row's `strata` and `denominator` columns are passed as those arguments.

## Usage

``` r
tfl_ard_methods()
```

## Value

A data frame: `method`, `label`, `call`, `kind`, `defaults`,
`statistics`, `formats`, `note`.

## Details

The function a keyword calls (its `call`:
[`cards::ard_summary`](https://pharmaverse.github.io/cards/latest-tag/reference/ard_summary.html),
[`cards::ard_stack_hierarchical`](https://pharmaverse.github.io/cards/latest-tag/reference/ard_stack_hierarchical.html)
...) is that keyword's analysis: it gets the same `statistic =` from
`statistics`, the same defaults and the same formats, so either name
writes the same code.

A function of the study's own is a method too, by its plain name
(`ard_riskdiff_mn`), when the study key `source` names the R file that
defines it (the ARD program sources it first). It is called the same
way: it takes the analysis data first, `by` and `variables` as bare
column names, and gives a cards ARD (class `card`); `population` is
there to pass in `args` (`denominator = population`).

In `args` and `code`, `data` is the analysis data and `population` the
population's subjects. `args` is read as the arguments of a call, in any
order.

Each keyword has a `label` – the name a person reads, for a GUI's choice
or a heading – and a one-line `note`, and names its function
(`(subjects)` and `(code)` are the two built into the engine), its
`kind` – how its `statistics` are passed: `continuous`, `categorical` or
`none` – the arguments it gets unless `args` gives them (`<id>` stands
for the subject key), its default statistics and formats.

The catalog is the built-in one unless `options(tflspec.ard_methods = )`
holds another, or a function that uses it is given `methods =` (as
tflplanner does with its company standards).

A catalog without a `label` column (one written before it was added)
gets the method's own name as its label.

## Examples

``` r
tfl_ard_methods()[, c("method", "label", "note")]
#>           method                         label
#> 1     continuous            Summary statistics
#> 2    categorical           Counts and percents
#> 3    dichotomous            Count of one level
#> 4        missing                Missing counts
#> 5   hierarchical Nested counts (e.g. SOC / PT)
#> 6            max       Worst level per subject
#> 7       subjects        Subjects with a record
#> 8        total_n            Number of subjects
#> 9  proportion_ci            Proportion with CI
#> 10       mean_ci                  Mean with CI
#> 11         ttest                        t test
#> 12        wilcox        Wilcoxon rank-sum test
#> 13         chisq               Chi-square test
#> 14        fisher           Fisher's exact test
#> 15        custom                 Custom R code
#>                                                                                                                        note
#> 1                                                                                   summary statistics of numeric variables
#> 2                                                                                         counts and percents of each level
#> 3                                                                       counts of one level (args: value = list(VAR = "Y"))
#> 4                                                                                            missing and non-missing counts
#> 5                                                                nested subject counts, outermost variable first (SOC | PT)
#> 6                                                              the worst level per subject; variables = the graded variable
#> 7                                        subjects with a record of the data (after where); variables = a name for the count
#> 8                                                                                                        number of subjects
#> 9  confidence interval of a proportion (args: method = "wilson" ...); statistics keep some of estimate, conf.low, conf.high
#> 10        confidence interval of a mean (args: conf.level = 0.9 ...); statistics keep some of estimate, conf.low, conf.high
#> 11                                                                  two-sample t test between the `by` groups (two of them)
#> 12                                                             Wilcoxon rank-sum test between the `by` groups (two of them)
#> 13                                                                                        chi-square test of variables x by
#> 14                                                                                    Fisher's exact test of variables x by
#> 15                                                                      any R code in `code`; data and population are bound
```
