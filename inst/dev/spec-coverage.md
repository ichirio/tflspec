# SPEC coverage

How much of each package the SPEC (a report's `content:` items, checked by
`tfl_schema_check()`, catalogued by `inst/spec/schema.yml`) can write.

## cards / cardx (table and listing SPEC: the ARD definition)

How much of cards' and cardx's ARD functions an analysis row (the ARD
definition's `analyses`: `method`, `by`, `variables`, `strata`,
`denominator`, `statistics`, `args`, `post`) can write.  One row an
argument: `spec-coverage-cards.csv` (columns `package`, `function`,
`argument`, `covered`, `where`, `note`, `group`, `weight`), made by
`dev/spec-coverage-cards.R` from the installed cards / cardx (their
data.frame methods' formals, as `tfl_ard_args()` reads them) and the
catalog (`inst/ard/functions.csv`, `args.csv`, `passed_args.csv`).  Scope:
every exported `ard_*()` -- its own arguments and, since 2026-10-30, the
arguments it passes on through `...` to the function doing the work
(`ard_regression(exponentiate =)` to `broom.helpers::tidy_plus_plus()`,
`ard_car_anova(type =)` to `car::Anova()`, `ard_stats_t_test(var.equal =)`
to `stats::t.test()` ...: 55 arguments of 18 functions) -- and cards' helpers
a row writes in `post` (`add_calculated_row()`, `filter_ard_hierarchical()`,
`sort_ard_hierarchical()`, `diff_ard_hierarchical()`,
`replace_null_statistic()`, `update_ard_stat_label()` and, since
2026-10-31, `update_ard_fmt_fun()`).  Since 2026-11-01 also the helpers
the generated code writes from a row's columns -- cards'
`continuous_summary_fns()` (the statistics column), `label_round()`,
`alias_as_fmt_fun()` and `apply_fmt_fun()` (the formats column) -- and
cardx's `proportion_ci_*()` (the choices of `ard_categorical_ci(method =)`,
whose arguments are `ard_categorical_ci()`'s).

* **yes**: the row's own column writes it, or the catalog describes it
  (its kind and a hint: the GUI shows it) and the check judges the value
  written in `args` / `post` by that kind (TRUE / FALSE, a number, one of
  the choices, text, a formula, a column, a list, selectors, an expression).
* **partial**: written as R in `args` / `post` and its name checked against
  the installed function, but not described, or its value taken as it is.
* **no**: a SPEC cannot write it.
* `[by design]`: not offered for a new analysis (the old names, the survey
  designs, `ard_formals()`; see `?tfl_ard_functions`) -- left out.

A function's coverage is its arguments' mean (yes 1, partial 0.5); the
percentage is the functions' mean weighted by their use in clinical tables:
3 for what demographics, disposition, AE by SOC / PT and severity, lab
summary and shift, vital signs and model-based efficacy (ANCOVA / LS means,
CMH, KM, log-rank, Cox, incidence rates) use; 2 sometimes; 1 rarely.

Since 2026-11-02 the measure also counts the ard_*() functions a method
keyword names (`tfl_ard_methods()`: the name a GUI offers and a person
reads, "Kruskal-Wallis test" rather than `cardx::ard_stats_kruskal_test`),
by weight.  A keyword writes the same code as its function, so this
measures how easily a SPEC is written, not what it can say; the argument
percentages do not move with it.

| ard_*() with a method keyword | 2026-11-02 before | 2026-11-02 after | 2026-11-03 after | 2026-11-04 before* | 2026-11-04 after | 2026-11-05 after |
|---|---:|---:|---:|---:|---:|---:|
| weight 3 | 21 of 27 | 23 of 27 | 24 of 27 | 24 of 27 | 24 of 27 | **25 of 27** |
| weight 2 | 0 of 19 | 5 of 19 | 12 of 19 | 12 of 23 | 20 of 23 | **22 of 23** |
| weight 1 | 0 of 8 | 0 of 8 | 0 of 8 | 0 of 4 | 3 of 4 | **3 of 4** |

\* 2026-11-04 corrects the weights: the measure named the effect sizes
`ard_stats_cohens_d()` / `ard_stats_paired_cohens_d()`, which cardx does
not have, so its four `ard_effectsize_*()` (Cohen's d, Hedges' g, paired)
were weight 1.  They are weight 2 now.  The argument percentages do not
move (every argument is yes).

(Without a keyword after 2026-11-05 -- weight 3: `ard_hierarchical()`
and `ard_hierarchical_count()`, the stacked counts' building blocks
(`hierarchical` / `hierarchical_count` are their stacked forms; one
keyword a function), written by their name.  Weight 2: `ard_mvsummary()`,
a summary by the study's own function (`custom` or a study function says
it more plainly).  Weight 1: `ard_identity()`, statistics already
computed.  Every cardx `ard_*()` but the survey designs has a keyword;
since 2026-11-05 so have the three that run other analyses, `ard_stack()`,
`ard_strata()` and `ard_pairwise()`.)

cards 0.9.0, cardx 0.3.4; 71 functions, 412 arguments (2026-11-01 scope:
313 of the ard_*() and post helpers' own, 72 passed on, 27 of the code
and CI helpers; `label_round(width =)` left out by design).

| Scope (2026-11-01, wider) | 2026-11-01 before | 2026-11-01 after |
|---|---:|---:|
| **cards / cardx, weighted** | **98.3%** (394 yes / 18 partial) | **99.5%** (411 / 1) |
| cards ard_*() | 100.0% | 100.0% |
| cardx ard_*() | 97.6% (237 / 17) | 100.0% (254 / 0) |
| cards post helpers | 100.0% | 100.0% |
| cards code helpers (new) | 92.5% (8 / 1) | 92.5% (8 / 1) |
| cardx CI helpers (new) | 100.0% | 100.0% |
| weight-3 functions only | 98.3% | 99.3% |

2026-11-02 (the same scope and cards / cardx): 99.5% before and after (411
yes / 1 partial); the run added keywords (above), not arguments.

2026-11-03 (the same scope; cards 0.9.0, cardx 0.3.4): **99.5% before (411
yes / 1 partial), 100.0% after (412 / 0)** -- cards' code helpers 92.5% to
100.0%: the formats column takes `xx.x*1000`, `label_round(scale =)`'s
other scales.  Weight-3 functions 99.3% to 100.0%.

2026-11-04 (the same scope; cards 0.9.0, cardx 0.3.4, still the CRAN
versions): **100.0% before and after (412 yes / 0 partial)**.  The run
added keywords (above) and a check of the names a function passes on
(below), not arguments.

2026-11-05 (the same scope; cards 0.9.0, cardx 0.3.4, still the CRAN
versions): **100.0% before and after (412 yes / 0 partial)**.  The run
added the wrappers' keywords (above) and closed two of the gaps below
(3: a `custom` row's calls by a plain name; 4: the names passed on to
`car::Anova()`), not arguments.

(2026-11-01 before: the tflspec of this branch before the run, on the wider
scope -- the 17 newly catalogued passed-on arguments could be written as R
in `args` and reached their function, but were not described and their
values not judged: partial.  On the 2026-10-31 scope the run starts and
ends at 100.0%.)

Earlier runs, on their scope:

| Scope | 2026-10-29 before | 2026-10-29 after | 2026-10-30 before (wider scope) | 2026-10-30 after | 2026-10-31 before (+ update_ard_fmt_fun) | 2026-10-31 after |
|---|---:|---:|---:|---:|---:|---:|
| **cards / cardx, weighted** | **75.9%** (153 yes / 155 partial) | **99.3%** (304 / 4) | **93.0%** (304 / 59) | **99.3%** (359 / 4) | **99.0%** (362 / 6) | **100.0%** (368 / 0) |
| cards ard_*() | 79.9% | 99.4% | 99.4% | 99.4% | 99.4% | 100.0% |
| cardx ard_*() | 76.0% | 99.8% | 89.3% | 99.8% | 99.8% | 100.0% |
| cards post helpers | 63.9% | 96.1% | 96.1% | 96.1% | 94.2% | 100.0% |
| weight-3 functions only | 75.1% | 99.6% | 93.1% | 99.6% | 99.6% | 100.0% |

(2026-10-30 before: the tflspec of `next` measured on the wider scope -- a
passed-on argument could be written as R in `args` and reached the
function, but the catalog did not describe it and its value was not
judged: partial.  2026-10-31 before: the same, with
`update_ard_fmt_fun()` in the `post` scope -- written as R in `post`, its
names checked, its values not judged.)

### Added 2026-11-05

* Three method keywords for the functions that run other analyses (the
  rows whose `parent` names this one), each its function by another name
  -- the same code:
  * `stack` -- `cards::ard_stack()`: several analyses on the same data and
    `by` (a demographics table's summaries and counts; `overall = TRUE` for
    its Total column);
  * `subgroups` -- `cards::ard_strata()`: an analysis within each level of
    `strata` (a response rate's difference in each subgroup of a forest
    plot's table);
  * `pairwise` -- `cards::ard_pairwise()`: an analysis for each pair of the
    levels of the one column in `variables`, `include = "Placebo"` for each
    arm against placebo.
  Every check of a parent row (one analysis inside `subgroups` /
  `pairwise`, ONE column for `pairwise`, no `overall` on `subgroups`) and
  the ARS's flattening take the keyword as the function.  A test writes
  the three by keyword and by function (the same code), builds the ARD and
  compares the `pairwise` p-values with `cards::ard_pairwise(include =
  "Placebo")` called by hand.
* **A `custom` row's calls by a plain name are judged too** (gap 3): after
  `library(cards)`, `ard_tabulate(data, denominatr = population)` is
  `cards::ard_tabulate()`'s call and said as one -- any function cards or
  cardx defines and exports (cards' first; not the tidyselect helpers they
  re-export), unless the code binds the name itself (`ard_tabulate <-
  ...`, a function's argument, a loop's variable).
* **A name `ard_car_anova()` passes on is looked for in `car::Anova()`'s
  method of the model** (gap 4): `x = lm(...), tpye = "III"` reaches
  `car:::Anova.lm()`, whose `...` takes nothing else -- the typo gave type
  II silently.  A name R matches by its beginning (`typ =` is `type =`)
  is the argument's.  The model is the call `x` writes (`lm`, `glm`, `coxph`,
  `stats::aov` ...; a name, `x = fit`, or a model car has no method for is
  not judged).

### Added 2026-11-04

* Eleven method keywords, each its function by another name (the same code;
  a row naming the function is the keyword's, in the ARS too):
  * `records` -- `cards::ard_tabulate_rows()`: the number of records (not
    subjects) in each `by` group, e.g. the number of adverse events per
    arm; the ARS lays it out as counts (`n`).
  * `labels` -- `cards::ard_attributes()`: the variables' labels.
  * `nested_models` -- `cardx::ard_stats_anova()`: nested models compared
    (`formulas = list(AVALC ~ TRTA + SEX, AVALC ~ TRTA * SEX)`), the
    treatment-by-subgroup interaction test of a forest plot's table; the
    ARS's operation is `p.value`.  A test builds it and compares the
    p-value with `stats::anova()` of the two `lm()` fits.
  * `regression_basic` -- `cardx::ard_regression_basic()`.
  * `cohens_d`, `hedges_g`, `paired_cohens_d`, `paired_hedges_g` --
    `cardx::ard_effectsize_*()`: effect sizes with CI, laid out as a test
    of values (`estimate`, `conf.low`, `conf.high`).
  * `mood` -- `cardx::ard_stats_mood_test()` (`p.value`).
  * `vif` -- `cardx::ard_car_vif()`; `wald` -- `cardx::ard_aod_wald_test()`
    (`p.value`): a model's checks, the model in `args` (`x = lm(...)`).
  * Each has an exact case in `fixtures/ard-cases.R` (the row's ARD against
    the hand-written call).
* **A name passed on through `...` is checked against the function it
  reaches.**  `ard_stats_t_test(var.eqaul = TRUE)` reaches
  `stats::t.test()`, whose default method ignores a name it does not
  have: the typo gave the default silently.  `tfl_ard_spec()` now warns
  when a name is neither the cardx function's, nor the catalog's
  (`passed_args.csv`), nor an argument of the function it is passed to (its
  default and formula methods too).  Only stats and survival functions are
  judged; broom.helpers, car, effectsize and smd pass `...` on again.  This
  closes most of gap 4 below.  The checks of `args` moved from
  `R/ard_spec.R` (1994 lines) to `R/ard_args_check.R`.

### Added 2026-11-03

* Eight method keywords, each its function by another name (the same code;
  a row naming the function is the keyword's, in the ARS too):
  * `hierarchical_count` -- `cards::ard_stack_hierarchical_count()`: the
    number of events (records, not subjects) by SOC / PT, the AE tables'
    "events" column; its denominator is the population unless `args` says
    another (`overall = TRUE` gives the total column);
  * `onesample_ttest` / `onesample_wilcox` --
    `cardx::ard_stats_t_test_onesample()` / `_wilcox_test_onesample()`:
    the change from baseline within each arm, with CI and p-value
    (`mu = 0`);
  * `smd` -- `cardx::ard_smd_smd()`: the standardized mean difference of
    two arms (baseline balance);
  * `poisson` -- `cardx::ard_stats_poisson_test()`: a rate, or two arms'
    rate ratio (variables: the events and the person-time columns);
  * `oneway` -- `cardx::ard_stats_oneway_test()` (Welch's one-way ANOVA) and
    `aov` -- `cardx::ard_stats_aov()` (a model's ANOVA): `formula =` in
    `args`;
  * `mcnemar_long` -- `cardx::ard_stats_mcnemar_test_long()`: McNemar's
    test on long data (a record per subject and visit, `id = USUBJID`), the
    shift tables' test.
  The ARS lays out the one-sample tests as values (estimate, CI,
  p-value), `smd` as a test of values, `mcnemar_long` as a test of counts,
  `hierarchical_count` as counts (`n`).
* A format times a number: `estimate=xx.xx*1000` is
  `cards::label_round(2, scale = 1000)` -- an incidence rate per 1,000
  person-years from cardx's rate per person-year.  The formats check takes
  it (`xx.x*`, `*0` and `xx.x%*100` are refused).
* Fix: the program's `fmt_ard()` keeps a factor statistic as text too
  (`ard_survival_survfit_diff()`'s `reference_level` was given a number
  of decimals: a warning, `stat_fmt` NA).
* Each keyword has an exact case (`tests/testthat/fixtures/ard-cases.R`).

### Added 2026-11-02

* Seven method keywords for the analyses clinical tables use most that had
  none, each its cardx function by another name (the same code; a row
  naming the function is the keyword's, in the ARS too):
  * `regression` -- `cardx::ard_regression()`: a model's estimates with CI,
    a Cox model's hazard ratio or a logistic model's odds ratio
    (`exponentiate = TRUE`);
  * `anova` -- `cardx::ard_car_anova()`: a model's ANOVA table, type II / III
    (`x = lm(CHG ~ TRTA + BASE, data = data), type = "III"`);
  * `kruskal` -- `cardx::ard_stats_kruskal_test()`;
  * `paired_ttest` / `paired_wilcox` -- `cardx::ard_stats_paired_t_test()` /
    `_paired_wilcox_test()` (two visits of each subject, `id = USUBJID`);
  * `mcnemar` -- `cardx::ard_stats_mcnemar_test()`;
  * `km_diff` -- `cardx::ard_survival_survfit_diff()`: the difference in
    Kaplan-Meier survival between arms at times, with CI.
  The ARS lays out `kruskal` as a test of values, `mcnemar` as a test of
  counts, both with `p.value`; the paired tests give `p.value`.
* Fix: a `km_diff` (`cardx::ard_survival_survfit_diff()`) row failed in the
  format step: cardx gives its text statistic `method` a number of
  decimals, which `cards::apply_fmt_fun()` cannot apply.  The program's
  `fmt_ard()` (`inst/helpers/study_helpers.R`) now keeps a text statistic
  as text.
* Each keyword has an exact case (`tests/testthat/fixtures/ard-cases.R`):
  the spec row's ARD against the hand-written cards / cardx call.

### Added 2026-11-01

* More arguments cardx functions pass on through `...`, described in
  `inst/ard/passed_args.csv` and their values judged (17 arguments of 9
  functions):
  * `ard_continuous_ci()`: `alternative` (to `stats::t.test()`), `exact`
    and `correct` (`method = "wilcox.test"`, the median's CI, to
    `stats::wilcox.test()`);
  * `ard_survival_survdiff(timefix =)` (to `survival::survdiff()`);
  * `ard_stats_mood_test(alternative =)` (to `stats::mood.test()`);
  * `ard_smd_smd()`: `gref` (the reference group) and `na.rm` (to
    `smd::smd()`);
  * the effect sizes, `ard_effectsize_cohens_d()` / `_hedges_g()`:
    `pooled_sd`, `mu`, `alternative`; their paired forms `mu`,
    `alternative` (to `effectsize`).
  The tests run them: Mood's one-sided p-value, the SMD's sign with
  `gref = 2L`, Cohen's d with `pooled_sd = FALSE`, the median's CI with
  `exact = FALSE, correct = FALSE` -- each against the function called
  directly.
* A `custom` row's code is no longer taken whole: its calls of cards' and
  cardx's functions written with their package (`cards::ard_tabulate(data,
  denominatr = population)`, `data |> cardx::ard_categorical_ci(method =
  "wilson_cc")`, `cards::ard_summary()` inside `cards::ard_stack()`, a
  `post` helper on a name the code assigns) have their arguments' names
  checked against the function (an `ard_*()` generic's data.frame method)
  and their values judged by the catalog's kinds, a warning of
  `tfl_ard_spec()` as for `args` and `post`.  The rest of the code is the
  program's.
* The measure's scope takes in the helpers the generated code writes and
  cardx's CI helpers (see above): all written by a row's columns except
  `label_round(scale =)` (100 only, by `xx.x%`: partial).

### Added 2026-10-31

* The last values taken as R are judged by their shape:
  * `ard_categorical_ci(weights = )` (the stratified Wilson weights): numbers
    between 0 and 1 that add up to 1, one a stratum (`c(0.5, 0.3, 0.2)`);
  * `ard_pairwise(include = )`: a vector of the levels compared
    (`c("Placebo", "Drug A")`), not a list or a formula;
  * `add_calculated_row(fmt_fun = )`: a whole number of decimals (`2L`), a
    format cards takes (`"xx.x"`, `"xx.x%"`: an `x`, then `x`'s, one point at
    most, `%` only at the end) or a function (`cards::label_round(1)`);
  * `replace_null_statistic(value = )`: one value (`NA`, `0`, `"NE"`).
  A call that makes the value (`rep(0.25, 4L)`) or a name is the program's.
* `update_ard_fmt_fun()` is a described `post` step (`variables`,
  `stat_names`, `fmt_fun`, `filter`): a format for some rows only, e.g. one
  arm's mean with 3 decimals (`filter = group1_level == "Placebo"`).  Its
  format is no longer lost: a `post` step that sets formats now runs after
  the format step (`fmt_ard()`, which gives every statistic its default) and
  the ARD is formatted again, `cards::apply_fmt_fun(replace = TRUE)`.
  The same for `add_calculated_row(stat_name = "se", fmt_fun = 4L)`: `se`
  has a default format, which used to win; its format is now said again
  after the format step (`update_ard_fmt_fun(stat_names = "se", fmt_fun =
  4L)`).
* `?tfl_ard_args` says which shapes a `code` argument's value is checked by.

### Added 2026-10-30

* The arguments a cardx function passes on through `...`, described and
  their values judged (`inst/ard/passed_args.csv`, `tfl_ard_args()`'s new
  `passed_to` column; a company adds rows with
  `options(tflspec.ard_passed_args =)`):
  * `ard_regression()` / `ard_regression_basic()` to
    `broom.helpers::tidy_plus_plus()`: `exponentiate` (hazard, odds and rate
    ratios), `conf.level`, `conf.int`, `intercept`, `add_reference_rows`,
    `add_header_rows`, `add_n`, `show_single_row`, `include`;
  * `ard_car_anova()` to `car::Anova()`: `type` ("II" / "III", or 2 / 3:
    a number among the choices is taken as a number), `test.statistic`;
  * the tests to `stats`: `alternative`, `mu`, `var.equal` (t tests, one-way
    ANOVA), `exact` / `correct` (Wilcoxon, CMH, McNemar, chi-square, the
    test of proportions), `simulate.p.value` / `B` (chi-square, Fisher), `r`
    (Poisson).
* The tests run them: a 2 x 2 chi-square with `correct = FALSE` is
  `chisq.test(correct = FALSE)`'s p-value, and a Cox model's row with
  `exponentiate = TRUE` gives `exp(coef(coxph()))`.

### Where the gaps are

The scope is close to its ceiling; what is left and what is next:

Every argument in scope is described and its value judged (since
2026-11-03).  What is left:

1. (closed 2026-11-03: `label_round(scale =)` is the format `xx.x*1000`.)
2. A `post` step of the study's own or of another package that sets formats
   is not moved: the format step after it gives the defaults.
3. A `custom` row's code: its cards / cardx calls written with the
   package or (since 2026-11-05) by a plain name cards / cardx exports are
   judged; a function bound to another name (`f <- cards::ard_tabulate`)
   is not.
4. A passed-on argument's name is checked (since 2026-11-04) when it
   reaches a stats or survival function, and (since 2026-11-05) the
   `car::Anova()` method of a model written in `x`; one passed to
   broom.helpers, effectsize or smd, which pass `...` on again, is not.  The catalog
   names the arguments clinical tables use, not every argument of the
   function they reach (`stats::aov()`'s, `car::vif(type =)` -- a method's
   argument, not the generic's -- are not described).
5. Not in scope by design: `rename_ard_groups_shift()` /
   `_reverse()` (the table code reads the by columns in the row's order:
   reorder `by` instead), `bind_ard()` (tflspec #118), the `mock_*()`
   shells.  A denominator of numbers is the denominator column (`86`,
   `c("Drug X" = 86, "Placebo" = 85)`); a data frame written in `args` is
   the program's.

## rtfreporter (table SPEC)

How much of rtfreporter's exported API (rtfreporter@next) a tflspec SPEC
(tflspec@next: the YAML spec's `content:` and `page:`, the sheets' columns
and `content_more` / `page_more`) can write.  The table, one row per
exported function and argument, is `spec-coverage-rtfreporter.csv`
(columns `package`, `function`, `argument`, `covered`, `where`, `note`);
it is made by `dev/spec_coverage.R` from the installed packages' formals
and `inst/spec/schema.yml`.

* **yes**: a SPEC writes it -- a content item the schema checks, a `page:`
  argument the report code passes on (`R/spec_report.R`), or a value
  tflspec fills itself (the pipe, the page's lines, the study's file).
* **partial**: it can be written, but not as the function's own argument
  (`rtf_tables(cell_valign = )` is `plan_style(cell_valign = )`), or a
  content item the schema has no entry for (no check, rank or help).
* **no**: a SPEC cannot write it.
* Notes `[helper]`: options, inspection helpers, code writers, file
  assemblers, the validation kit, `plan_apply()` (`rtf_tables()` takes the
  plan itself), deprecated and superseded arguments, and -- since
  2026-10-31 -- rtfreporter's own deprecated exports
  (`rtfreporter:::.deprecated_exports`: the old border constructors,
  `paginate()`, `assemble_toc()` ..., removed in 0.9.0 and left out of
  rtfreporter's own API measure) -- no SPEC item by design.  `[out of scope]`: `plan_layers()`, designed in another session.

Percentage: yes counts 1, partial 0.5.

| Run        | All arguments | SPEC-relevant (helpers out) | Functions yes / partial / no |
|------------|-------------------|----------------------------------|------------------------------|
| 2026-10-28 before | 41.9% (287 / 30 / 403 of 720) | 47.5% (287 / 30 / 319) | 38 / 7 / 90 |
| 2026-10-28 after  | 43.3% (300 / 24 / 396 of 720) | 49.1% (300 / 24 / 312) | 40 / 6 / 89 |
| 2026-10-29 before | 43.7% (305 / 25 / 397 of 727) | 49.6% (305 / 25 / 310 of 640) | 41 / 6 / 90 |
| 2026-10-29 after  | 43.8% (306 / 25 / 396 of 727) | 49.8% (306 / 25 / 309 of 640) | 41 / 6 / 90 |
| 2026-10-30 before | 43.8% (306 / 25 / 397 of 728) | 49.7% (306 / 25 / 310 of 641) | 41 / 6 / 90 |
| 2026-10-30 after  | 44.3% (310 / 25 / 393 of 728) | 50.3% (310 / 25 / 306 of 641) | 43 / 5 / 89 |
| 2026-10-31 before | 44.3% (310 / 25 / 393 of 728) | 50.3% (310 / 25 / 306 of 641) | 43 / 5 / 89 |
| 2026-10-31 after  | 45.4% (310 / 41 / 377 of 728) | 57.2% (310 / 41 / 227 of 578) | 43 / 5 / 89 |
| 2026-11-01 before | 51.7% (310 / 40 / 288 of 638) | 57.9% (310 / 40 / 220 of 570) | 43 / 5 / 66 |
| 2026-11-01 after  | 49.6% (308 / 17 / 313 of 638) | 58.1% (308 / 17 / 220 of 545) | 43 / 5 / 66 |
| 2026-11-02 before | 51.6% (325 / 19 / 304 of 648) | 58.8% (325 / 19 / 225 of 569) | 41 / 7 / 67 |
| 2026-11-02 after  | 51.8% (326 / 19 / 303 of 648) | 59.2% (326 / 19 / 222 of 567) | 42 / 6 / 67 |
| 2026-11-03 before / after | 51.8% (326 / 19 / 303 of 648) | 59.2% (326 / 19 / 222 of 567) | 42 / 6 / 67 |
| 2026-11-04 before / after | 51.6% (326 / 19 / 305 of 650) | 59.0% (326 / 19 / 224 of 569) | 41 / 7 / 67 |
| 2026-11-05 before / after | 51.6% (326 / 19 / 305 of 650) | 59.0% (326 / 19 / 224 of 569) | 41 / 7 / 67 |

(2026-11-05: rtfreporter@next 0872829 (its changes since fc1a438 are the
listing's wrapping of a value with a separator, the flextable / huxtable
adapters' options, tests and quality documents: no new exported argument)
and tflspec@next 48d8a3b merged; the run went to cards / cardx -- the
table is unchanged.  The "no" rows left are the lower-level table API the
plan verbs stand for; the SPEC-relevant gaps are the four below.)

(2026-11-04: rtfreporter@next fc1a438 and tflspec@next a9c791a merged.
rtfreporter's one new argument is `render_format` ("png" or "emf", #888)
of `rtfplot()` and `rtf_figures()`: a figure's, not passed on yet, which
takes the share from 59.2% to 59.0%.  It is left to the figure SPEC's
routine: with "emf", `render_dpi` and `png_device` must not be given, and
the report code gives them for every figure.  The run went to cards /
cardx; the table and listing SPEC's share is unchanged.)

(2026-11-03: rtfreporter@next ffdf080 (its changes since 825d4d7 are checks,
pagination fixes and validation tests: no new argument) and tflspec@next
29dae66 merged; the run went to cards / cardx -- nothing new a SPEC
writes here, the table is unchanged.)

(2026-11-02 before: tflspec@next merged into the branch (#569:
`page.rtf_style`, rtfreporter's `rtf_style()`; #564 ...) and
rtfreporter@next (825d4d7: `rtf_style()`, the cells' top / bottom padding,
the bands' `color`).  After: `page.rtf_header.color` /
`page.rtf_footer.color` passed on, and the measure's corrections --
`rtf_document(style =)` is written from `page.rtf_style` (yes; it was
counted as not passed on), `rtf_document(default_format =)` is deprecated
(by `style =`) and `rtf_style(...)` is not an argument (rtfreporter stops
on one): helpers.  The corrections alone leave 58.8%.)

(2026-11-01: rtfreporter@next no longer exports its deprecated functions
(the old border constructors, `assemble_*()`, `format_count_pct()` ...: 90
arguments, 23 functions, most of them helpers already).  Nothing new a
SPEC writes in this run, which went to cards / cardx; after it: tflspec@next
(#543) no longer passes `rtf_tables(font_size_half_points =,
read_attributes =)` on, which rtfreporter deprecated with the rest of
`rtf_tables()`' table-format arguments (`rtfreporter:::.rtf_tables_fmt_args`,
removed in 0.9.0).  Those 25 are now helpers -- the plan verbs and
`rtftable()` say them -- and leave the SPEC-relevant count.)

(2026-10-31: the measure's corrections only, nothing new a SPEC writes --
rtfreporter's deprecated exports and the superseded `rtf_section(secinfo =)`
/ `generate_rtfreport(report =)` are helpers (63 arguments out of the
SPEC-relevant count); `rtf_table_style()`'s 15 fields are said by
`plan_style()`'s own arguments, `rtf_tables(col_spec = )` by
`plan_cell_style(cols =)` and `generate_rtfreport(program =,
program_fallback =)` by `rtf_document()`, which tflspec fills: partial.)

(2026-10-30 before is rtfreporter@next with one more argument,
`fmt_value_paren(rounding =)`.)

(2026-10-29 before is rtfreporter@next with 7 more arguments --
`plan_append()`, `table_plan(value = )`, `rtfreporter_validation_kit()` --
and the measure's corrections: `rtf_tables(style = )` is said by
`plan_style()`'s `rtf_table_style()` fields (partial), `read_gt` is
deprecated, the validation kit a helper.)

### Added 2026-11-02

* A running band's text colour: `page.rtf_header.color` /
  `page.rtf_footer.color` (`"#7F7F7F"`), passed on to rtfreporter's
  `rtf_header(color =)` / `rtf_footer(color =)` in the report's call and the
  study setup's `study_header` / `study_footer`; the colour is added to the
  document's colour table.

### Added 2026-10-30

* A border's side with a look of its own: `rtf_border_line()` (`style`,
  `width`, `color`) in the schema, a side of `rtf_border()` taking
  `{fn: rtf_border_line, args: {style: double, width: 30}}`; `rtf_border()`'s
  `all`, `inside_h` and `inside_v` typed; `plan_style(border =)` takes an
  `rtf_border()` call besides "tfl" / "none".
* The schema check looks inside a call a value holds: a wrong style in
  `{fn: rtf_border, args: {top: {fn: rtf_border_line, args: {style: wavy}}}}`
  is said as `rtf_border(top = ): rtf_border_line(style = ) is ...` (table and
  listing items; the figure items are checked as before).
* `rtf_document(color_table =)`: `page.rtf_document.color_table`, passed on.

### Where the gaps are

Most "no" rows are rtfreporter's lower-level table API -- `rtftable()`,
`widen_ard()`, `as_rtftable(s)()`, the `style_*()`, `set_*()`, `fmt_*()`,
`paginate*()` and `listing_*()` helpers -- which the plan verbs already
stand for: a SPEC says the plan, not these calls.  The gaps that a SPEC
could usefully close next:

1. `rtf_section(page =)`: a section of its own header from a page on (a
   report of several objects, `contents:`, could give each its own).
2. `rtfplot(width_twips =, height_twips =)` (the figure SPEC's).
3. `rtf_table_style()` as a value (`plan_style(style = )` of a company's
   own style object): its fields are said one by one today.
4. `rtf_figures(render_format =)` / `rtfplot(render_format =)` (EMF
   figures, rtfreporter#888): the figure SPEC's.

## ggplot2 (figure SPEC)

Measured by `dev/spec-coverage-ggplot2.R` (one row an argument:
`inst/dev/spec-coverage-ggplot2.csv`) with ggplot2 4.0.3.

- **yes**: the function has a card in the catalog and the argument is typed
  and labelled there or by its kind (`inst/fig/gg_args.csv`) -- the GUI shows
  it, the check types its value.
- **partial**: written and accepted by the check, but untyped, or the
  function has no card (written from its formals).
- **no**: the check does not know it (an error, or "no argument").

A function's coverage is its arguments' mean (yes 1, partial 0.5, no 0);
the percentage is the functions' mean weighted by their use in clinical
figures: 3 for what KM, forest, waterfall, spider / swimmer, box / violin,
line over time with mean +/- SE, bar, scatter, heat map and CDF plots use;
2 sometimes; 1 rarely.  Deprecated and internal arguments (`size` of line
geoms, `fun.y`, `trans`, `label.size` ...) are left out.

Two rounds.  The first counted 2015 arguments (156 functions and geoms with
a stat); the second widened the scope by 19 functions (line widths, binned / stepped / default
colour scales, scale_x_time(), guide_axis_logticks() for a forest plot's
ratio axis, guide_axis_theta() / guide_axis_stack(), filled contours) to
2303 arguments of 175 functions.

| Scope | Start | Round 1 | Round 2 (wider scope: before / after) |
|---|---:|---:|---:|
| **ggplot2: geoms (with their stats), scales, coords, themes, facets, guides, labs, positions** | **24.7%** | **94.9%** | **91.4% / 100.0%** |
| geom_*() | 55.9% | 91.4% | 90.6% / 100.0% |
| a geom with another stat (geom_step(stat = "ecdf"), geom_pointrange(stat = "summary") ...) | 0.0% | 100.0% | 100.0% / 100.0% |
| scale_*() and expansion() / sec_axis() / dup_axis() | 8.1% | 95.9% | 87.7% / 100.0% |
| coord_*() | 34.5% | 98.8% | 98.8% / 100.0% |
| theme(), complete themes, element_*(), margin() | 5.0% | 91.2% | 91.2% / 100.0% |
| facet_wrap() / facet_grid() | 57.4% | 100.0% | 100.0% / 100.0% |
| guides() and guide_*() | 10.7% | 97.3% | 86.8% / 100.0% |
| labs(), xlab(), ylab(), ggtitle(), annotate() | 38.8% | 97.4% | 97.4% / 100.0% |
| position_*() | 0.0% | 100.0% | 100.0% / 100.0% |
| **stat_*()** (shown apart) | **44.2%** | **73.1%** | **73.1% / 100.0%** |

Round 2, 2303 arguments: before 1533 yes / 770 partial / 0 no; after 2303
yes.  (ggplot2 4.0's `box_gp`, `whisker_gp` ... are left out: geom_boxplot()
makes them from `whisker.colour` and the like, and given too they clash.)

100% is this measure's ceiling, not ggplot2's: every argument in the scope
has a card and a type, which says the GUI shows it and the check types its
value; the tests run the code of a sample of them (test-fig-ggplot2.R).
Next: the extension packages (ggsurvfit, ggh4x, ggrepel, ggtext,
ggnewscale, patchwork), and the scope's edge (geom_sf, geom_hex, the
rarer stat_*()).

What made the difference, round 1:

- Any function of a plot ggplot2 exports is written and checked by its
  formals; before, a function without a card (`scale_colour_brewer`,
  `theme_bw`, `guide_axis`, `coord_trans`) was an error of the check.
- A geom given `stat:` takes that stat's parameters (`pad` for ecdf;
  `fun`, `fun.data`, `fun.min`, `fun.max`, `fun.args` for summary; `level`,
  `type`, `segments` for ellipse), a stat given `geom:` that geom's
  (`stat_summary(geom = "errorbar", width = 0.2)`).
- `fun.args`, `method.args`, `args`, `dparams`, `override.aes` are written
  `list(...)` (before: `c(...)`) and read back as a map.
- A scale's `...` are its constructor's arguments (`scale_colour_manual(name
  =, labels =, limits =)` warned before); coord_fixed()'s are
  coord_cartesian()'s; coord_trans() of ggplot2 4.0 is coord_transform()'s.
- The shared arguments are typed and labelled once by kind
  (`inst/fig/gg_args.csv`: layer, scale, coord, facet, theme, element,
  guide, position, helper, labs); theme()'s settings each take its element
  (`axis.text.x` an element_text, not an element_line), the per-side
  variants (`axis.text.x.top`) as the setting they refine.
- Cards for 14 more geoms, the 5 stats with no geom twin (stat_ecdf,
  stat_summary, stat_summary_bin, stat_ellipse, stat_unique), 8 positions,
  35 scales, expansion / sec_axis / dup_axis / margin, 4 coords, xlab /
  ylab / ggtitle, 8 complete themes, element_text / line / rect / blank and
  6 guides.

Round 2:

- Cards for the 2-d layers (geom_contour / _filled, geom_density_2d /
  _filled, geom_bin_2d), geom_quantile, geom_spoke, geom_function,
  geom_blank, and the 11 stat_*() with a geom twin (stat_smooth, stat_bin,
  stat_count, stat_boxplot, stat_density, stat_ydensity, stat_identity,
  stat_function, stat_qq, stat_qq_line, stat_sum).
- Cards for 14 scales (scale_linewidth / _manual, scale_*_discrete /
  _continuous, scale_x_binned, scale_*_steps / steps2 / fermenter,
  scale_x_time) and 3 guides (guide_axis_logticks, guide_axis_theta,
  guide_axis_stack).
- Typed by kind (inst/fig/gg_args.csv, 106 more rows): ggplot2 4.0's look
  of a part of a box plot, a violin, a crossbar or a label
  (`whisker.colour`, `staple.linewidth`, `median.linetype`, `box.colour`,
  `quantile.linetype`, `border.colour`, `text.colour`, `size.unit` ...),
  the stats' parameters (`bounds`, `xseq`, `h`, `contour_var`, `args`,
  `xlim` ...), the rarer theme settings (`axis.minor.ticks.*`,
  `legend.frame`, `legend.ticks*`, `legend.box.*`, `legend.location`,
  `plot.tag.location`, ggplot2 4.0's `panel.widths` / `panel.heights`,
  `point`, `polygon`, `geom`, `spacing`, `margins`), the complete themes'
  `header_family` / `accent`, coord_radial()'s 4.0 names, labs()'s
  `dictionary`.
- Fix: `visible: [.., y]` / `[.., n]` read as TRUE / FALSE by YAML in
  tfl_text_column, tfl_ard_number, geom_function, stat_function (quoted).
