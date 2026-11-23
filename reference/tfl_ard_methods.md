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
#>                method                                label
#> 1          continuous                   Summary statistics
#> 2         categorical                  Counts and percents
#> 3         dichotomous                   Count of one level
#> 4             missing                       Missing counts
#> 5        hierarchical        Nested counts (e.g. SOC / PT)
#> 6                 max              Worst level per subject
#> 7            subjects               Subjects with a record
#> 8             total_n                   Number of subjects
#> 9       proportion_ci                   Proportion with CI
#> 10            mean_ci                         Mean with CI
#> 11              ttest                               t test
#> 12             wilcox               Wilcoxon rank-sum test
#> 13              chisq                      Chi-square test
#> 14             fisher                  Fisher's exact test
#> 15                cmh         Cochran-Mantel-Haenszel test
#> 16          prop_diff            Difference in proportions
#> 17            lsmeans                  Least-squares means
#> 18       lsmeans_diff    Difference in least-squares means
#> 19                 km               Kaplan-Meier estimates
#> 20            logrank                        Log-rank test
#> 21           abnormal        Abnormal post-baseline values
#> 22     incidence_rate                       Incidence rate
#> 23         regression                      Model estimates
#> 24              anova                 ANOVA / ANCOVA table
#> 25            kruskal                  Kruskal-Wallis test
#> 26       paired_ttest                        Paired t test
#> 27      paired_wilcox            Wilcoxon signed-rank test
#> 28            mcnemar                       McNemar's test
#> 29            km_diff Difference in Kaplan-Meier estimates
#> 30 hierarchical_count                  Nested event counts
#> 31    onesample_ttest                    One-sample t test
#> 32   onesample_wilcox One-sample Wilcoxon signed-rank test
#> 33                smd         Standardized mean difference
#> 34            poisson                    Poisson rate test
#> 35             oneway                One-way ANOVA (Welch)
#> 36                aov                 Analysis of variance
#> 37       mcnemar_long            McNemar's test, long data
#> 38            records                    Number of records
#> 39             labels                      Variable labels
#> 40      nested_models                Test of nested models
#> 41   regression_basic              Model estimates (basic)
#> 42           cohens_d                            Cohen's d
#> 43           hedges_g                            Hedges' g
#> 44    paired_cohens_d                     Paired Cohen's d
#> 45    paired_hedges_g                     Paired Hedges' g
#> 46               mood                 Mood's test of scale
#> 47                vif           Variance inflation factors
#> 48               wald             Wald test of model terms
#> 49              stack            Several analyses together
#> 50          subgroups                 Within each subgroup
#> 51           pairwise                  Each pair of groups
#> 52             custom                        Custom R code
#>                                                                                                                                                                                                                                                                                               note
#> 1                                                                                                                                                                                                                                                          summary statistics of numeric variables
#> 2                                                                                                                                                                                                                                                                counts and percents of each level
#> 3                                                                                                                                                                                                                                              counts of one level (args: value = list(VAR = "Y"))
#> 4                                                                                                                                                                                                                                                                   missing and non-missing counts
#> 5                                                                                                                                                                                                                                       nested subject counts, outermost variable first (SOC | PT)
#> 6                                                                                                                                                                                                                                     the worst level per subject; variables = the graded variable
#> 7                                                                                                                                                                                                               subjects with a record of the data (after where); variables = a name for the count
#> 8                                                                                                                                                                                                                                                                               number of subjects
#> 9                                                                                                                                                                         confidence interval of a proportion (args: method = "wilson" ...); statistics keep some of estimate, conf.low, conf.high
#> 10                                                                                                                                                                               confidence interval of a mean (args: conf.level = 0.9 ...); statistics keep some of estimate, conf.low, conf.high
#> 11                                                                                                                                                                                                                                         two-sample t test between the `by` groups (two of them)
#> 12                                                                                                                                                                                                                                    Wilcoxon rank-sum test between the `by` groups (two of them)
#> 13                                                                                                                                                                                                                                                               chi-square test of variables x by
#> 14                                                                                                                                                                                                                                                           Fisher's exact test of variables x by
#> 15                                                                                                                                                                                                     CMH test of variables x by, within the strata (strata column, e.g. a stratification factor)
#> 16                                                                                                                                                                                        difference in proportions between the two `by` groups, with its CI; variables = a logical / 0-1 response
#> 17                                                                                                                                                                      LS means of a model by arm, e.g. ANCOVA (args: formula = CHG ~ TRT01P + BASE, method = "lm", response_type = "continuous")
#> 18                                                                                                                                                                                                                 differences in LS means between arms, with CI and p-value (args as for lsmeans)
#> 19                                                                                                                            Kaplan-Meier survival at times, or quantiles of the time to event (args: y = "survival::Surv(AVAL, 1 - CNSR)", times = c(6, 12) or probs = 0.5); variables = the arm
#> 20                                                                                                                                                                                    log-rank test between arms (args: formula = survival::Surv(AVAL, 1 - CNSR) ~ TRT01P); by and variables blank
#> 21                                                                                                                                                                       subjects with an abnormal post-baseline value, by baseline (args: postbaseline = ANRIND, baseline = BNRIND, id = USUBJID)
#> 22                                                                                                                                                                                       events per person-time with CI (args: time = <person-time column>, count = <events column>, id = USUBJID)
#> 23 a model's estimates with CI, e.g. a Cox model's hazard ratio (args: formula = survival::Surv(AVAL, 1 - CNSR) ~ TRTA, method = "coxph", package = "survival", exponentiate = TRUE) or a logistic model's odds ratio (method = "glm", method.args = list(family = binomial), exponentiate = TRUE)
#> 24                                                                                                                                                                      a model's ANOVA table, type II or III (args: x = lm(CHG ~ TRTA + BASE, data = data), type = "III"); by and variables blank
#> 25                                                                                                                                                                                                                                        Kruskal-Wallis test across the `by` groups (two or more)
#> 26                                                                                                                                                                                                paired t test between the two `by` levels (e.g. two visits) of each subject (args: id = USUBJID)
#> 27                                                                                                                                                                                                      Wilcoxon signed-rank test between the two `by` levels of each subject (args: id = USUBJID)
#> 28                                                                                                                                                                              McNemar's test of variables x by, paired data (one record a subject, e.g. a baseline and a post-baseline response)
#> 29                                                                                             differences in Kaplan-Meier survival between the arms at times, with CI (args: x = survival::survfit(survival::Surv(AVAL, 1 - CNSR) ~ TRTA, data = data), times = c(6, 12)); by and variables blank
#> 30                                                                                                                                     events (records, not subjects) at each level of a hierarchy, e.g. AEBODSYS | AEDECOD; with by and denominator for the overall column (args: overall = TRUE)
#> 31                                                                                                                                                                    one-sample t test of each variable's mean against mu, e.g. the change from baseline in each arm (by), with CI (args: mu = 0)
#> 32                                                                                                                                                                            one-sample Wilcoxon signed-rank test of each variable against mu, in each `by` group (args: mu = 0, conf.int = TRUE)
#> 33                                                                                                                                                                                        standardized mean difference between the two `by` groups, e.g. baseline balance (args: std.error = TRUE)
#> 34                                                                                                                                             exact Poisson test of a rate, or the ratio of two arms' rates (by, two levels); variables are the events and the person-time columns, in that order
#> 35                                                                                                                                                                            one-way ANOVA across the groups, not assuming equal variances (args: formula = AGE ~ TRT01A); by and variables blank
#> 36                                                                                                                                                                                                     analysis of variance of a model (args: formula = CHG ~ TRTA + BASE); by and variables blank
#> 37                                                                                                                                                                      McNemar's test on long data, a record per subject and `by` level, e.g. baseline and week 12 responses (args: id = USUBJID)
#> 38                                                                                                                                                                                        the number of records (rows, not subjects) in each `by` group, e.g. the number of adverse events per arm
#> 39                                                                                                                                                                                                                            the variables' labels (and classes), e.g. for a table's row headings
#> 40                                                                                 nested models compared, e.g. a treatment-by-subgroup interaction (args: formulas = list(AVALC ~ TRTA + SEX, AVALC ~ TRTA * SEX), method = "glm", method.args = list(family = binomial)); by and variables blank
#> 41                                                                                                                                                                                     a model's estimates with fewer columns than `regression` (args: formula = CHG ~ TRTA + BASE, method = "lm")
#> 42                                                                                                                                                                                                                                                  Cohen's d between the two `by` groups, with CI
#> 43                                                                                                                                                                                                          Hedges' g (Cohen's d corrected for small samples) between the two `by` groups, with CI
#> 44                                                                                                                                                                                                              Cohen's d of paired data, the two `by` levels of each subject (args: id = USUBJID)
#> 45                                                                                                                                                                                                              Hedges' g of paired data, the two `by` levels of each subject (args: id = USUBJID)
#> 46                                                                                                                                                                                                                           Mood's two-sample test of scale between the `by` groups (two of them)
#> 47                                                                                                                                                                      variance inflation factors of a model's terms (args: x = lm(CHG ~ TRTA + BASE + AGE, data = data)); by and variables blank
#> 48                                                                                                                                                                       Wald tests of a model's terms (args: x = glm(AVALN ~ TRTA + SEX, family = binomial, data = data)); by and variables blank
#> 49                                                                                                                           runs the analyses whose `parent` is this one on the same data, by the same `by` (e.g. a demographics table's summaries and counts; overall = TRUE for a Total column)
#> 50                                                                                                                              runs the analysis whose `parent` is this one within each level of `strata` (and `by`), e.g. a response rate's difference in each subgroup of a forest plot's table
#> 51                                                                                                                                runs the analysis whose `parent` is this one for each pair of levels of the one column in `variables`, e.g. each arm against placebo (args: include = "Placebo")
#> 52                                                                                                                                                                                                                                             any R code in `code`; data and population are bound
```
