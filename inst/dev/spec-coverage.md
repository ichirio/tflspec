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
whose arguments are `ard_categorical_ci()`'s).  Since 2026-11-06 also the
arguments of the model cardx fits itself, written in `method.args =
list(...)` or in a model's call in `x`: `survival::survfit()` (with
`survfitKM()`, where its `...` go), `survival::coxph()` (with
`coxph.control()`), `stats::glm()` (with `glm.control()`), `stats::lm()`
and `mmrm::mmrm()` (with `mmrm_control()`) -- the Kaplan-Meier, Cox,
logistic, ANCOVA and MMRM models, catalogued in `inst/ard/model_args.csv`;
since 2026-11-07 also `MASS::glm.nb()` (a negative binomial rate model,
weight 2) and `survival::survreg()` (a parametric survival model, weight
1), each with the control function its `...` reach.

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

2026-11-06 (wider scope; cards 0.9.0, cardx 0.3.4, still the CRAN
versions; survival 3.8, mmrm 0.3.19): the model arguments are 41 more
arguments of 5 functions (weight 3; 32 more left out by design: the
population's `subset`, starting values, multi-state, computing).

| Scope (2026-11-06, wider) | 2026-11-06 before | 2026-11-06 after |
|---|---:|---:|
| **cards / cardx, weighted** | **95.9%** (412 yes / 41 partial) | **99.8%** (451 / 2) |
| cards ard_*() | 100.0% | 100.0% |
| cardx ard_*() | 100.0% | 100.0% |
| cards post helpers | 100.0% | 100.0% |
| cards code helpers | 100.0% | 100.0% |
| cardx CI helpers | 100.0% | 100.0% |
| cardx model arguments (new) | 50.0% (0 / 41) | 97.1% (39 / 2) |
| weight-3 functions only | 93.6% | 99.6% |

2026-11-07 (wider scope; cards 0.9.0, cardx 0.3.4, still the CRAN
versions; MASS 7.3-60.0.1, survival 3.8, mmrm 0.3.19): the two models are 19 more
arguments of 2 functions (by design left out: their starting values,
fitting routine, `control` written as its parts, the fit's parts).

| Scope (2026-11-07, wider) | 2026-11-07 before | 2026-11-07 after |
|---|---:|---:|
| **cards / cardx, weighted** | **99.0%** (451 yes / 21 partial) | **100.0%** (472 / 0) |
| cards ard_*() | 100.0% | 100.0% |
| cardx ard_*() | 100.0% | 100.0% |
| cards post helpers | 100.0% | 100.0% |
| cards code helpers | 100.0% | 100.0% |
| cardx CI helpers | 100.0% | 100.0% |
| cardx model arguments | 89.3% (39 / 21) | 100.0% (60 / 0) |
| weight-3 functions only | 99.6% | 100.0% |

2026-11-08 (wider scope; cards 0.9.0, cardx 0.3.4, still the CRAN
versions; broom.helpers 1.23.0): the rest of the arguments
`ard_regression()` / `ard_regression_basic()` pass on to
`broom.helpers::tidy_plus_plus()` that clinical tables use -- 12 more
arguments of each (pairwise contrasts, reference rows, labels; left out:
`model_matrix_attr`, `instrumental_suffix`, `relabel_poly`,
`disambiguate_*`, `group_by` / `group_labels` (multinomial models),
`keep_model`, `tidy_post_fun`, `quiet`, `strict` -- rarely changed).

| Scope (2026-11-08, wider) | 2026-11-08 before | 2026-11-08 after |
|---|---:|---:|
| **cards / cardx, weighted** | **99.4%** (472 yes / 24 partial) | **100.0%** (496 / 0) |
| cards ard_*() | 100.0% | 100.0% |
| cardx ard_*() | 98.8% (254 / 24) | 100.0% (278 / 0) |
| cards post helpers | 100.0% | 100.0% |
| cards code helpers | 100.0% | 100.0% |
| cardx CI helpers | 100.0% | 100.0% |
| cardx model arguments | 100.0% | 100.0% |
| weight-3 functions only | 99.4% | 100.0% |

2026-11-09 (wider scope; cards 0.9.0, cardx 0.3.4, still the CRAN
versions; car 3.1, survival 3.8): 14 more passed-on arguments of 7
functions that clinical tables use -- the Wilcoxon tests' `conf.int` (the
Hodges-Lehmann estimate and its interval) and `digits.rank`, the CMH
test's `conf.level` (the common odds ratio's interval), Fisher's
`conf.int`, `or`, `workspace` and `hybrid`, and `car::Anova()`'s
`white.adjust` (a robust, HC3 ANCOVA), `singular.ok` and
`error.estimate`.  Left out: `car::Anova(vcov. =, error.df =)` (a matrix
or function of the program's), `stats::wilcox.test(tol.root =)`,
`effectsize`'s `verbose`, the tests' `subset` / `na.action` (the
population's and the derivation's), `chisq.test(p =, rescale.p =)` and
`prop.test(p =)` (one-sample forms cardx does not run).

| Scope (2026-11-09, wider) | 2026-11-09 before | 2026-11-09 after |
|---|---:|---:|
| **cards / cardx, weighted** | **98.8%** (496 yes / 14 partial) | **100.0%** (510 / 0) |
| cards ard_*() | 100.0% | 100.0% |
| cardx ard_*() | 97.6% (278 / 14) | 100.0% (292 / 0) |
| cards post helpers | 100.0% | 100.0% |
| cards code helpers | 100.0% | 100.0% |
| cardx CI helpers | 100.0% | 100.0% |
| cardx model arguments | 100.0% | 100.0% |
| weight-3 functions only | 98.5% | 100.0% |

2026-11-10 (wider scope; cards 0.9.0, cardx 0.3.4, effectsize 1.0.3, smd
0.8.0): 3 more passed-on arguments of the effect sizes -- `reference` of
`effectsize::cohens_d()` / `hedges_g()` (the by level compared with:
"Placebo", which sets the sign) and `cohens_d(adjust =)` (the small-sample
correction).  One argument left the scope: `ard_smd_smd(na.rm =)`, which
cardx gives `smd::smd()` itself (`na.rm = TRUE`) -- written in `args` too,
it stopped the analysis, so the catalog no longer offers it.

| Scope (2026-11-10, wider) | 2026-11-10 before | 2026-11-10 after |
|---|---:|---:|
| **cards / cardx, weighted** | **99.8%** (510 yes / 3 partial) | **100.0%** (512 / 0) |
| cards ard_*() | 100.0% | 100.0% |
| cardx ard_*() | 99.6% (292 / 3) | 100.0% (294 / 0) |
| cards post helpers | 100.0% | 100.0% |
| cards code helpers | 100.0% | 100.0% |
| cardx CI helpers | 100.0% | 100.0% |
| cardx model arguments | 100.0% | 100.0% |
| weight-3 functions only | 100.0% | 100.0% |

2026-11-11 (the same scope; cards 0.9.0, cardx 0.3.4, still the CRAN
versions): **100.0% before and after (512 yes / 0 partial)**.  The run
added checks (below), not arguments: a name a function takes in `...` and
drops, and a hint to `method.args` for a model's argument written beside
the function's own.

2026-11-12 (wider scope; cards 0.9.0, cardx 0.3.4, still the CRAN
versions; geepack 1.3.13, MASS 7.3-60.0.1): two more models cardx fits
from `method` / `package` -- `geepack::geeglm()` (a GEE model: a
responder analysis over visits, weight 2) and `MASS::polr()` (a
proportional odds model of an ordinal scale, weight 2) -- 18 more
arguments of 2 functions (by design left out: their starting values,
`subset` / `na.action`, geeglm()'s fitting routine, `control` written as
its parts and `zcor`, polr()'s `model`).

| Scope (2026-11-12, wider) | 2026-11-12 before | 2026-11-12 after |
|---|---:|---:|
| **cards / cardx, weighted** | **99.0%** (512 yes / 18 partial) | **100.0%** (530 / 0) |
| cards ard_*() | 100.0% | 100.0% |
| cardx ard_*() | 100.0% | 100.0% |
| cards post helpers | 100.0% | 100.0% |
| cards code helpers | 100.0% | 100.0% |
| cardx CI helpers | 100.0% | 100.0% |
| cardx model arguments | 90.9% (60 / 18) | 100.0% (78 / 0) |
| weight-3 functions only | 100.0% | 100.0% |

2026-11-13 (wider scope; cards 0.9.0, cardx 0.3.4, still the CRAN
versions; broom.helpers 1.23.0, emmeans 2.0.4, mmrm 0.3.19): the arguments
of `emmeans::emmeans()` (and `ref_grid()`, where its `...` go) a
regression row writes in `emmeans_args = list(...)` for its pairwise
contrasts -- 15 more arguments of 1 function (weight 2: the LS means'
weights, SAS's OM, a logistic model's differences of proportions, the
covariates' values; by design left out: `object` / `specs`, which
tidy_plus_plus() gives, `data`, `options` written as its parts, `contr`,
`counterfactuals`, `fac.reduce`, the multivariate `mult.names` /
`mult.levs`, `rg.limit`).

| Scope (2026-11-13, wider) | 2026-11-13 before | 2026-11-13 after |
|---|---:|---:|
| **cards / cardx, weighted** | **99.5%** (530 yes / 15 partial) | **99.9%** (541 / 4) |
| cards ard_*() | 100.0% | 100.0% |
| cardx ard_*() | 100.0% | 100.0% |
| cards post helpers | 100.0% | 100.0% |
| cards code helpers | 100.0% | 100.0% |
| cardx CI helpers | 100.0% | 100.0% |
| cardx model arguments | 100.0% | 100.0% |
| emmeans arguments in emmeans_args (new) | 50.0% (0 / 15) | 86.7% (11 / 4) |
| weight-3 functions only | 100.0% | 100.0% |

2026-11-14 (wider scope; cards 0.9.0, cardx 0.3.4, still the CRAN
versions; broom.helpers 1.23.0, emmeans 2.0.4): the rest of
`broom.helpers::tidy_plus_plus()`'s arguments `ard_regression()` /
`ard_regression_basic()` pass on -- `quiet`, `strict`, `tidy_post_fun`,
`group_by` / `group_labels` (a multinomial model's response levels),
`relabel_poly`, `disambiguate_terms` / `disambiguate_sep`,
`model_matrix_attr`, `instrumental_suffix`: 10 more arguments of each
(left out on 2026-11-08 as rarely changed; still left out: `keep_model`,
a model column the ARD cannot hold, and `model` / `tidy_fun`, which cardx
writes itself).  And the 4 emmeans arguments left partial on 2026-11-13
(`cov.reduce`, `cov.keep`, `tran`, `nesting`) are judged.

| Scope (2026-11-14, wider) | 2026-11-14 before | 2026-11-14 after |
|---|---:|---:|
| **cards / cardx, weighted** | **99.5%** (541 yes / 24 partial) | **100.0%** (565 / 0) |
| cards ard_*() | 100.0% | 100.0% |
| cardx ard_*() | 99.3% (294 / 20) | 100.0% (314 / 0) |
| cards post helpers | 100.0% | 100.0% |
| cards code helpers | 100.0% | 100.0% |
| cardx CI helpers | 100.0% | 100.0% |
| cardx model arguments | 100.0% | 100.0% |
| emmeans arguments in emmeans_args | 86.7% (11 / 4) | 100.0% (15 / 0) |
| weight-3 functions only | 99.7% | 100.0% |

(2026-11-14 before: this branch before the run (tflspec@next 6fa6b81
merged; a tflspec built from it) measured by this run's script on the
wider scope -- the 20 tidy_plus_plus() arguments could be written as R in
`args` and reached it (their names known as its own), but were not
described and their values not judged; emmeans' 4 as on 2026-11-13:
partial.  On the 2026-11-13 scope the run goes from 99.9% to 100.0%.)

(2026-11-13 before: this branch before the run (tflspec@next c4aaa4e
merged) on the wider scope -- `emmeans_args` had to be a list, but its
names reached emmeans() unchecked (emmeans drops one it does not know) and
their values were not judged: partial.  The 4 partial after: `cov.reduce`,
`cov.keep`, `tran` and `nesting` -- their names checked, their R (a
function, a formula, a list of nests) taken as it is.  On the 2026-11-12
scope the run starts and ends at 100.0%.)

(2026-11-12 before: this branch before the run (tflspec@next 7af5a42
merged) on the wider scope -- the two models' arguments could be written
as R in `method.args` and reached the model, but were neither described
nor their names checked: partial.  On the 2026-11-11 scope the run starts
and ends at 100.0%.)

(2026-11-10 before: this branch before the run (tflspec@next f04b06d
merged) on the wider scope -- the 3 arguments could be written as R in
`args` and reached effectsize, but were not described and their values
not judged: partial.  On the 2026-11-09 scope the run starts and ends at
100.0%.)

(2026-11-09 before: this branch before the run (tflspec@next 0890a63
merged) on the wider scope -- the 14 arguments could be written as R in
`args` and reached their function, but were not described and their
values not judged: partial.  On the 2026-11-08 scope the run starts and
ends at 100.0%.)

(2026-11-08 before: tflspec@next 8a948f7 on the wider scope -- the 24
arguments could be written as R in `args` and reached tidy_plus_plus(),
but were not described and their values not judged: partial.  On the
2026-11-07 scope the run starts and ends at 100.0%.)

(2026-11-07 before: this branch before the run (tflspec@next d237a12
merged), measured by this run's script: `mmrm(covariance =, weights =)`
taken as R; glm.nb()'s arguments reached the model unchecked (its `...`
were not known to go to `glm.control()`), survreg()'s names were checked
but not described: partial.  On the 2026-11-06 scope the run goes from
99.8% to 100.0%.)

(2026-11-06 before: tflspec@next d220f07 on the wider scope -- a model's
argument could be written as R in `method.args` and reached the model, but
was neither described nor its name checked: partial.  On the 2026-11-05
scope the run starts and ends at 100.0%.)

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

### Added 2026-11-14

* **emmeans' `cov.reduce`, `cov.keep`, `tran` and `nesting` are judged**
  (`R/ard_emmeans_args.R`; catalog kinds `reducer`, `keep`, `tran`,
  `nesting` in `inst/ard/model_args.csv`), in a regression row's
  `emmeans_args = list(...)`:
  * `cov.reduce`: a function not in quotes (`median`, `function(x) ...`,
    `stats::median`), TRUE / FALSE, a two-sided formula (`BASE ~ TRT01P`)
    or a list of functions / formulas by covariate.  `"median"` in quotes,
    a number or a one-sided formula stops `ref_grid()` ("Invalid
    'cov.reduce' argument") -- the ARD keeps the error, the cells are empty.
  * `cov.keep`: names in quotes or a whole number.
  * `tran`: a name the installed emmeans knows (read from its
    `.make.link()`: log, logit, sqrt, log10, log2, log1p, inverse ...) or
    `make.tran(...)`.  emmeans takes **any other name as the identity
    without a word**: reproduced on R's mtcars and in a test on cards'
    ADSL -- `tran = "logg", type = "response"` on a response made log
    beforehand gave the arms' differences on the log scale instead of
    their ratios.
  * `nesting`: `"SITEID %in% COUNTRY"` (several separated by commas) or a
    list; a number or TRUE stops emmeans.
  A test compares a row with `tran = "log", type = "response"` with
  cardx's call written by hand and with `emmeans()` itself.
* **tidy_plus_plus()'s other arguments** (`inst/ard/passed_args.csv`, 10
  rows for each regression function): `quiet`, `strict`, `relabel_poly`,
  `disambiguate_terms`, `model_matrix_attr` (TRUE / FALSE),
  `disambiguate_sep`, `instrumental_suffix` (text), `tidy_post_fun` (a
  function), `group_by` (columns of the tidied table, `auto_group_by()` or
  NULL: a new code shape, `select`) and `group_labels` (labels by name).
* Looked at but not added: **a proportional odds model's `p.values = TRUE`**
  (broom's `tidy.polr()`).  Inside cardx it never gives broom's p-values:
  `tidy.polr()` refits the model with `MASS::dropterm()` for its LR
  tests, which cannot find cardx's model data, so broom.helpers falls back
  to `parameters` and its Wald p-values (cards' ADSL, `AGEGR1 ~ SEX`: 0.4815
  through cardx, broom's LR 0.480 directly); with a 3-level factor broom
  gives none at all.  A row that wants polr's p-values writes `tidy_fun =
  broom.helpers::tidy_parameters` (Wald) -- gap 8.  `p.values` stays
  uncatalogued, so it is said for a model whose tidier does not take it
  (an lm).

### Added 2026-11-13

* **`emmeans_args = list(...)` is judged** (`R/ard_emmeans_args.R`): a
  regression row's pairwise contrasts (`add_pairwise_contrasts = TRUE`)
  are `emmeans::emmeans(object = <model>, specs = <term>, <emmeans_args>)`
  then `pairs()` and `confint(level = conf.level)` (broom.helpers 1.23.0).
  emmeans 2.0.4 gives its `...` to `ref_grid()` and `update()`, which keep
  the names they know (by their beginning) and drop the rest without a
  word: `wieghts = "proportional"` gave the equal-weights LS means
  (reproduced on R's mtcars: an LS mean of 19.88, the equal weights', instead of 22.26).
  Now said when the spec is read:
  * a name neither `emmeans()`, `ref_grid()`, `update()`'s options nor a
    model method of the installed emmeans (`mode`, `vcov.`, `lmer.df` ...)
    takes;
  * `object` / `specs` (tidy_plus_plus() replaces them), `adjust` (pairs()
    is called with Tukey or `contrasts_adjust`: verified, the emmeans
    option is overridden) and `level` / `alpha` (confint() is called with
    `conf.level`: verified);
  * `emmeans_args` without `add_pairwise_contrasts = TRUE` (emmeans() is
    never called);
  * values the catalog describes (`inst/ard/model_args.csv`, fn
    `emmeans::emmeans`): `weights` (equal, proportional -- SAS's OM --,
    outer, cells, flat, or numbers), `type`, `regrid` (response: a
    logistic model's differences of proportions), `at` (a list), `df`
    (above 0), `nuisance`, `non.nuisance`, `wt.nuis`, `offset`, `by`,
    `mode`, `bias.adjust`, `sigma`.
  `tfl_ard_model_args("emmeans", "emmeans")` lists them for a GUI.  A
  test builds a logistic row with `regrid = "response", weights =
  "proportional"`, compares it with cardx's call written by hand and with
  `emmeans()` itself, and shows the equal-weights result differs.
* **MMRM's `cov_struct()`: its other names** (gap 5): `group` is a column
  in quotes (`group = "TRTA"`: a covariance by arm; `group = TRTA` would
  stop mmrm), `heterogeneous` TRUE / FALSE, and a name `cov_struct()` does
  not have (`groups =`) is said -- mmrm would stop on it.
* Looked at but not added: emmeans arguments for `ard_emmeans_emmeans()` /
  `ard_emmeans_contrast()` -- cardx 0.3.4 checks their `...` (it stops on
  any name) and calls `emmeans::emmeans()` itself, so an LS means row
  cannot set `weights`; the regression row's pairwise contrasts can.

### Added 2026-11-12

* **A GEE model, `geepack::geeglm()`**, through `method = "geeglm",
  package = "geepack"` and `method.args = list(family = binomial, id =
  factor(USUBJID), corstr = "exchangeable")` (or a call in `x`): the
  responder analysis over visits that some efficacy tables give with
  `exponentiate = TRUE` (odds ratios with robust, sandwich intervals).
  `inst/ard/model_args.csv` describes `family`, `id`, `waves`, `corstr`
  (independence, exchangeable, ar1, unstructured ...), `std.err` (san.se,
  jack, j1s, fij), `scale.fix`, `scale.value`, `weights`, `offset`,
  `contrasts`, and the `maxit`, `epsilon` and `trace` that its `...` give
  both `glm.control()` and `geese.control()`.  Names and values are
  judged.  Two mistakes are now reported when the spec is read:
  * no `id`: geeglm() stops ("id variable not found");
  * **`id = USUBJID`**: geeglm() finds its clusters where `as.numeric(id)`
    changes.  ADaM's USUBJID is text, so the whole data is one cluster.
    The program runs, with only a warning that cardx keeps in the ARD, and
    the standard errors and p-values are wrong (a p-value of 0 on cards'
    ADLB, against 0.099 with `id = factor(USUBJID)`).  The warning says to
    write `id = factor(USUBJID)`.
* **A proportional odds model, `MASS::polr()`**, through `method =
  "polr", package = "MASS"`: an ordinal scale's odds ratios.  The catalog
  describes `method` (logistic, probit, loglog, cloglog, cauchit),
  `Hess`, `weights`, `contrasts` and `control`.  `control` is the only
  name polr()'s `...` may carry to `optim()`: polr() gives optim() its
  `par`, `fn`, `gr`, `method` and `hessian` itself, and a second one
  stops it.  broom's `tidy.polr(p.values = TRUE)` is passed on as before
  and is known.
* The tests run both: a GEE row and a polr row give the same ARD as
  cardx's call written by hand, and the GEE odds ratios are those of
  `geeglm()` fitted directly.
* geepack joins Suggests (for the tests and the example of
  `tfl_ard_model_args("geeglm", "geepack")`); MASS ships with R.

### Added 2026-11-11

* **A name a function drops is said.**  Some functions take `...` and do
  nothing with them -- neither pass them on nor check them: cardx 0.3.4's
  `ard_survival_survfit()` (its data.frame method), `ard_tabulate_max()`
  and `ard_aod_wald_test()`, and cards 0.9.0's `ard_tabulate_value()`.  A
  name they do not have was ignored without a word: `conf.type =
  "log-log"` written beside `times` (not in `method.args`) left the
  Kaplan-Meier intervals "log"; `denominater = ADSL` in an AE by maximum
  severity row left the default denominator.  It is now a warning when
  the spec is read.  Which functions drop their `...` is read from the
  installed code, as for the ones that check them.
* **A model's argument beside the function's own points to
  `method.args`.**  When the misplaced name is an argument of the model
  the row fits (`conf.type` of `survival::survfit()`, `family` of
  `stats::glm()` beside `ard_regression(method = "glm")`), the warning
  says to write it in `method.args = list(conf.type = ...)`.  The same
  hint is added to the warnings for a name `tidy_plus_plus()` and broom's
  tidier do not have, and for a name a function that checks its `...`
  does not have (`ard_stats_anova()`, `ard_emmeans_*()`).
* **Gap 4: the CMH test's passed-on names.**  cardx lists the row's `...`
  for `mantelhaen.test()` and fills only the names the row leaves out with
  the test's defaults, so no name reaches the call twice.  A test now
  checks that and compares `correct = FALSE` with
  `stats::mantelhaen.test()`.
* Looked at, not added: `ard_stats_aov()` passes `...` to `stats::aov()`,
  but its table is the sequential (type I) sums of squares, which
  `contrasts =` does not change, and `weights =` stops in cardx's call
  ("..1 used in an incorrect context").  Use `ard_car_anova()` with a
  weighted model in `x`.  `ard_stats_anova()` checks its `...` (it stops
  on any), so `anova(test = "Chisq")` cannot be passed on; its `method`
  chooses the test.

### Added 2026-11-10

* **Cohen's d / Hedges' g against a reference level**:
  `ard_effectsize_cohens_d(reference = "Placebo")` and `hedges_g()`'s --
  the difference is the other level minus the reference (by default the
  first level minus the second); `cohens_d(adjust = TRUE)` gives the
  small-sample (Hedges) correction.  A test compares the row's estimate
  with `effectsize::cohens_d(reference =)` and `hedges_g()`.
* **A name cardx writes itself is said** (all passed-on calls): cardx
  writes some of the arguments of the call that takes `...`
  (`paired = TRUE` of `ard_stats_paired_t_test()`, `paired` and `ci` of
  the effect sizes, `na.rm = TRUE` of `ard_smd_smd()`).  The same name in
  `args` reaches that call twice, and R stops ("formal argument matched by
  multiple actual arguments"); cardx keeps the error in the ARD and the
  table's cells are empty.  The names are read from the installed cardx's
  code.  A test checks that the catalog offers none of them, and so
  `ard_smd_smd(na.rm =)` left it.
* **Names passed on to effectsize and smd are checked** (gap 4):
  effectsize's `cohens_d()` / `hedges_g()` drop a name they do not have
  (`pooled_SD = FALSE` left the pooled SD without a word); `smd::smd()`
  has no `...` and stops on one.

### Added 2026-11-09

* **The Hodges-Lehmann estimate** (`ard_stats_wilcox_test()`,
  `ard_stats_paired_wilcox_test()`, `ard_stats_wilcox_test_onesample()`
  with `conf.int = TRUE`): the location shift (median difference) and its
  interval besides the p-value; `digits.rank` (above 0) makes values equal
  but for floating point ties.  A test compares the row's estimate and
  lower limit with `stats::wilcox.test(conf.int = TRUE)`.
* **The CMH common odds ratio's level** (`ard_stats_mantelhaen_test(conf.level
  =)`, between 0 and 1); a test compares the 90% limits with
  `stats::mantelhaen.test()`.
* **Fisher's options**: `conf.int` (the conditional odds ratio),
  `or` (the null odds ratio, above 0), `workspace` (a whole number, 1 or
  more: a grade by arm table that stops with FEXACT error 7 needs
  `workspace = 2e7`), `hybrid`.  `B` (fisher.test() and chisq.test()) is
  1 or more.
* **A robust ANCOVA** (`ard_car_anova(white.adjust =)`, car's
  Anova.lm(): FALSE, TRUE or one of `"hc3"`, `"hc0"` ... `"hc4"` -- the
  heteroscedasticity-consistent covariance the covariate-adjustment
  guidances suggest), `singular.ok`, and a GLM's F test's
  `error.estimate`.  A test compares the treatment p-value with
  `car::Anova(type = "III", white.adjust = "hc3")` and with the
  model-based one (they differ).  A choice that is TRUE / FALSE is judged
  as written (`white.adjust = TRUE`).
* **A catalogued name car's method of the model does not have is said**
  (gap 4): `white.adjust =` with `x = glm(...)` (Anova.glm() would ignore
  it: no robust covariance), `test.statistic =` with `x = lm(...)`.  Before,
  a name the catalog described was taken whatever the model.

### Added 2026-11-08

* **The pairwise comparisons of a model's arms** (`ard_regression()`'s
  `add_pairwise_contrasts = TRUE`, by emmeans): each arm against each
  other in one row -- `pairwise_variables` (the terms compared),
  `pairwise_reverse` (Drug A / Placebo or the other way round),
  `keep_model_terms`, `contrasts_adjust` (one of emmeans' adjustments:
  `none`, `tukey`, `bonferroni`, `holm`, `sidak`, `dunnettx`, `mvt` ...; a
  value not one of them is said) and `emmeans_args` (a list:
  `list(type = "response")`).  A test builds an ANCOVA row's contrasts
  and compares them with the arms' mean differences and the model's own
  t test; `pairwise_reverse = FALSE` turns each sign.
* **The reference rows and labels**: `no_reference_row` (columns),
  `add_estimate_to_reference_rows` (a hazard ratio of 1.00 for placebo),
  `variable_labels` (a list or formula), `term_labels` (labels by name:
  `c(SEXM = "Male")`; a vector with no names is said),
  `categorical_terms_pattern` (`"{level} vs {reference_level}"`),
  `interaction_sep`.  `ard_regression_basic()` the same, with its own
  default (`add_estimate_to_reference_rows = FALSE`).
* **A name passed on to `tidy_plus_plus()` is looked for in it and in
  broom's `tidy()` method of the model** (gap 4): `exponentaite = TRUE`
  reached broom's `tidy.coxph()` / `tidy.glm()`, whose `...` is ignored --
  the table showed log hazard (odds) ratios without a word.  The model is
  the one `method` / `package` name or the call `x` writes (`lm`, `glm`,
  `coxph`, `glm.nb` (`tidy.negbin()`), `survreg`, `mmrm` ...).  A name R
  matches by its beginning (`exponent =`) is tidy_plus_plus()'s; a study's
  own tidier (`tidy_fun =`), a model by a name (`x = fit`) or one broom has
  no method for is not judged.
* rtfreporter's new `plan_paginate_rows(na_label =)` (the name of a page
  whose BY value is blank, rtfreporter@next 29e1c24) has a schema entry
  (type, label, help); it was passed on by its formals before.

### Added 2026-11-07

* **MMRM's covariance structure judged** (`mmrm(covariance =)`, kind
  `covariance`): `method.args = list(covariance = cov_struct("toep", visits
  = "AVISIT", subject = "USUBJID"))` or `~ ar1(AVISIT | USUBJID)`, for a
  formula without a structure.  A type mmrm does not have (`"un"`, `~
  un(...)`: its `cov_types()` of the installed mmrm), `visits = AVISIT`
  not in quotes, a missing `subject`, or text (`covariance = "us"`) is a
  warning when the spec is read.
* **mmrm(weights =) judged** (kind `vector`): a vector as long as the
  data (`data$WTS`) or the program's name; a column's name in quotes
  (`weights = "WTS"`) is said -- mmrm() does not look a column up.
* **A negative binomial rate model** (`MASS::glm.nb()`: exacerbations,
  relapses, recurrent events -- PROC GENMOD's DIST=NEGBIN): `method =
  "glm.nb", package = "MASS"` in `regression` / `lsmeans` / `lsmeans_diff`
  rows, or `x = MASS::glm.nb(...)`.  Its `link` (kind `link`: `log`,
  `sqrt`, `identity`, a name or text; `link = logit` is said),
  `init.theta` (above 0), `weights`, `contrasts` and glm.control()'s
  `maxit` / `epsilon` / `trace` (its `...` go there: a name neither has is
  said, as `lnik =`).  A test builds the rate ratios from a row and
  compares them with `cardx::ard_regression(method = "glm.nb", ...)`
  called by hand.
* **A parametric survival model** (`survival::survreg()`): `dist` (one of
  weibull, exponential, lognormal, loglogistic ...), `scale` (0 or more),
  `robust`, `cluster`, `weights`, `parms`, and survreg.control()'s
  `maxiter`, `rel.tolerance`, `outer.max`, `debug`.
* A model number with a floor is judged: `init.theta = 0`, `scale = -1`,
  `maxit = 0`.
* `tfl_ard_model_args("glm.nb", "MASS")` / `("survreg", "survival")` list
  them for a GUI.

### Added 2026-11-06

* **The arguments of the model cardx fits** (`inst/ard/model_args.csv`, 41
  arguments of 5 model functions, `tfl_ard_model_args()`): the
  Kaplan-Meier interval (`km`: `method.args = list(conf.type = "log-log")`,
  SAS PROC LIFETEST's; `conf.int`, `conf.lower`, `stype`, `ctype`,
  `start.time`, `robust`, `cluster` ...), a Cox model's ties (`regression`
  with `method = "coxph"`: `method.args = list(ties = "breslow")`, PROC
  PHREG's; `robust`, `cluster`, `iter.max` ...), a logistic or Poisson
  model's `family` / `offset` (`glm`), an ANCOVA's `weights` / `contrasts`
  (`lm`), and MMRM's degrees of freedom (`lsmeans` / `lsmeans_diff` with
  `method = "mmrm", package = "mmrm"`: `method.args = list(method =
  "Kenward-Roger")`, PROC MIXED's DDFM=KR; `reml`, `vcov`).
* `tfl_ard_spec()` looks for each name in the installed model function
  and where its `...` go (`survfitKM()`, `coxph.control()`,
  `glm.control()`, `mmrm_control()`): `conf.tpye =`, `tise =`, `metod =`
  would stop the program, `wieghts =` would be ignored by `lm()` -- each
  is said when the spec is read, as a warning.  The model is the one
  `method` / `package` name (`"glm"` with the default package is
  `stats::glm`), or survival::survfit() for `km`; a study's own model
  function, or one whose `...` take anything, is not judged.  `formula =`
  / `data =` in `method.args` are cardx's own (the program would stop).
* Their values are judged by kind: one of the choices (`conf.type =
  "loglog"`, `ties = "breslov"`, `method = "KR"`), a number (`conf.int =
  95` is between 0 and 1), TRUE / FALSE, a column not in quotes
  (`cluster = "SITEID"` would stop `coxph()`), a family (`family = 1`).
* The same for a model's call written in `x`: `km_diff`'s `x =
  survival::survfit(..., conf.type = "log-log")`, `wald`'s `x = glm(...,
  fmaily = binomial)`; and in a `custom` row's cardx call.
* A test builds a `km` row with the log-log interval and compares it with
  `cardx::ard_survival_survfit(method.args = list(conf.type = "log-log"))`
  called by hand and with `survival::survfit()`'s own limits.

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
   reaches a stats or survival function, (since 2026-11-05) the
   `car::Anova()` method of a model written in `x` (since 2026-11-09 a
   catalogued name too: `white.adjust =` with a glm) and (since 2026-11-08)
   `broom.helpers::tidy_plus_plus()` with broom's tidier of the model and
   (since 2026-11-10) effectsize's `cohens_d()` / `hedges_g()` and
   `smd::smd()`; since 2026-11-10 a name cardx writes itself in that call
   (`paired =`, `na.rm =`) is said too.  (Closed 2026-11-11:
   `mantelhaen.test()`'s arguments cardx gives as a list fill only what
   the row leaves out, so none reaches the call twice.)  Since 2026-11-11
   a name a function takes in `...` and drops is said too
   (`ard_survival_survfit()`, `ard_tabulate_max()`,
   `ard_tabulate_value()`, `ard_aod_wald_test()`); since 2026-11-13 a
   name in `emmeans_args = list(...)` that emmeans() would drop (and since
   2026-11-14 a `tran` name emmeans would take as the identity).  The catalog
   names the arguments clinical tables use, not every argument of the
   function they reach (`stats::aov()`'s, `car::vif(type =)` -- a method's
   argument, not the generic's -- are not described).
5. (since 2026-11-06) A model's argument is checked for the five model
   functions catalogued (survfit, coxph, glm, lm, mmrm); another model
   (`survival::survreg()`, `MASS::glm.nb()`, `nlme::gls()`, a study's own)
   takes its names as written.  (Since 2026-11-07 also `MASS::glm.nb()`
   and `survival::survreg()`, and `mmrm(covariance =, weights =)` are
   judged; since 2026-11-12 also `geepack::geeglm()` and `MASS::polr()`.)
   Left: `nlme::gls()` (its formula is `model =`, which cardx does not
   give: not offered) and `nlme::lme()` (its formula is `fixed =`, which
   cardx's `formula =` does not reach either) and a study's own model
   (since 2026-11-13 `cov_struct(group =, heterogeneous =)` and its names
   are judged).  A list made by the program
   (`method.args = my_args`) is the program's.
6. Not in scope by design: `rename_ard_groups_shift()` /
   `_reverse()` (the table code reads the by columns in the row's order:
   reorder `by` instead), `bind_ard()` (tflspec #118), the `mock_*()`
   shells.  A denominator of numbers is the denominator column (`86`,
   `c("Drug X" = 86, "Placebo" = 85)`); a data frame written in `args` is
   the program's.
7. (since 2026-11-13) An LS means row (`lsmeans` / `lsmeans_diff`:
   `ard_emmeans_emmeans()` / `ard_emmeans_contrast()`) cannot set
   emmeans' `weights` (SAS's OM): cardx 0.3.4 stops on any name in their
   `...`.  A regression row's pairwise contrasts can (`emmeans_args`); the
   LS means rows need a cardx change first.
8. (since 2026-11-14) A proportional odds model's p-values: `p.values =
   TRUE` (broom's `tidy.polr()`) cannot run its LR tests inside cardx
   (`MASS::dropterm()` cannot refit cardx's model), so broom.helpers falls
   back to `parameters`' Wald p-values with a message.  Write `tidy_fun =
   broom.helpers::tidy_parameters` for Wald p-values on purpose; LR
   p-values need a change in cardx or broom.

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
| 2026-11-06 before / after | 51.6% (326 / 19 / 305 of 650) | 59.0% (326 / 19 / 224 of 569) | 41 / 7 / 67 |
| 2026-11-07 before / after | 51.6% (326 / 19 / 305 of 650) | 59.0% (326 / 19 / 224 of 569) | 41 / 7 / 67 |
| 2026-11-08 before / after | 51.7% (327 / 19 / 305 of 651) | 59.0% (327 / 19 / 224 of 570) | 41 / 7 / 67 |
| 2026-11-09 before / after | 51.7% (327 / 19 / 305 of 651) | 59.0% (327 / 19 / 224 of 570) | 41 / 7 / 67 |
| 2026-11-10 before | 51.9% (331 / 20 / 306 of 657) | 59.2% (331 / 20 / 225 of 576) | 40 / 8 / 67 |
| 2026-11-10 after  | 52.1% (332 / 20 / 305 of 657) | 59.4% (332 / 20 / 224 of 576) | 41 / 7 / 67 |
| 2026-11-11 before / after | 52.1% (332 / 20 / 305 of 657) | 59.4% (332 / 20 / 224 of 576) | 41 / 7 / 67 |
| 2026-11-12 before / after | 53.0% (332 / 20 / 293 of 645) | 59.4% (332 / 20 / 224 of 576) | 42 / 6 / 66 |
| 2026-11-13 before / after | 53.2% (332 / 20 / 291 of 643) | 59.4% (332 / 20 / 224 of 576) | 42 / 6 / 66 |
| 2026-11-14 before | 53.2% (332 / 20 / 291 of 643) | 59.4% (332 / 20 / 224 of 576) | 42 / 6 / 66 |
| 2026-11-14 after  | 54.7% (332 / 39 / 272 of 643) | 61.0% (332 / 39 / 205 of 576) | 42 / 7 / 65 |

(2026-11-14: rtfreporter@next 2629a3c (its changes since 93951c8 are the
CRAN review before 0.9.0 -- graphics and tools in Imports, resource files
read from the installed package only, `?plan_after`'s example, tests'
skips: no new exported argument) and tflspec@next 6fa6b81 merged.  The
measure's correction: `widen_ard()`'s 19 arguments were "no" (tflspec
calls it, the argument is not said), but a SPEC writes the plan, and
rtfreporter's `plan_apply()` builds `widen_ard()`'s argument list from the
roles and the plan verbs (`.plan_spread_args()`: `cols` / `rows` /
`label` / `show_keys` / `value` from `table_plan()`, `cells` / `stats` /
`na` / `notes` from `plan_cells()`, `levels`, `labels`, `sort` /
`sort_stat`, `sep` from `plan_columns()`, `rounding` from
`plan_digits()`, `stat_groups`, `compare`, `filter`) with the names
unchanged.  Each is now partial -- said by the verb that owns it, as
`rtf_table_style()`'s fields are by `plan_style()`'s.  Nothing new a SPEC
writes; the run went mostly to cards / cardx.)

(2026-11-13: rtfreporter@next 93951c8 (its changes since 5c7d0ac: the
table of contents -- long entries wrap before the page numbers, one page
sequence from the first page, `toc_page_numbering` removed from
`assemble_rtf()` / `assemble_folder()` --, the gt adapter's captions,
footnote marks and stub header, `stub_spec(label =)` on a promoted labels
row, `as_rtftables()` listing defaults for a list, rlistings' `max_rows`,
`blank_rows` counted over the whole body: no new exported argument a SPEC
writes) and tflspec@next c4aaa4e merged.  The two removed arguments were
helpers (file assembly), so only "all arguments" moves.  The run went to
cards / cardx.)

(2026-11-12: rtfreporter@next 5c7d0ac (its changes since dd1e0cb:
`rtf_default_format()` and `rtf_document(default_format =)` removed, as
0.9.0 plans; `assemble_rtf()`'s table of contents -- `toc_style`, a
hanging indent, `toc_title` line breaks -- and `pull_ard(levels =)` in
the data's order: no new argument a SPEC writes) and tflspec@next
7af5a42 merged.  The 12 removed arguments were helpers (deprecated), so
only "all arguments" moves; `assemble_rtf(toc_style =)` is a new helper
(file assembly).  tflspec writes no call to `rtf_default_format()`: an
old SPEC's `page.rtf_default_format` is read as `rtf_style()`'s shared
level.  The run went to cards / cardx.)

(2026-11-11: rtfreporter@next dd1e0cb (its changes since 48b954d are the
validation kit's traceability matrix and the quality plan: no new exported
argument) and tflspec@next 3e8c9c1 merged; the run went to cards / cardx --
the table is unchanged.  Gap 1 below, `rtf_section(page =)`, was looked at
again: it takes page numbers, which a SPEC does not know before the
document is laid out, so it stays a gap until rtfreporter can start a
section at an object.  Gap 3, `rtf_table_style()` as a value, needs a
`plan_style()` argument that takes one in rtfreporter first.)

(2026-11-10: rtfreporter@next 48b954d (its changes since b1182ae: the
header, footer, title and footnote borders as the whole block's,
`rtf_titles(border =)`, `plan_titles(border =)`, `plan_footnotes(border
=)`, `plan_cell_style(row =)`, `plan_style(border_group =)`, band rows'
`bold` / `italic` / `underline` as list fields) and tflspec@next f04b06d.
The plan verbs' new arguments passed on by their formals already (yes);
the run gives `plan_titles(border =)`, `plan_footnotes(border =)` and
`plan_cell_style(row =)` schema entries (type, check, help) and passes
`page.rtf_titles.border` on to `rtf_titles(border =)` (no -> yes).
`plan_style(border_group =)`'s schema entry is tflspec#762's.)

(2026-11-09: rtfreporter@next b1182ae (its changes since 29e1c24 are the
header N read from a total stated by group, a maintainer change, tests and
an internal helper dropped: no new exported argument) and tflspec@next
0890a63 merged; the run went to cards / cardx -- the table is unchanged.)

(2026-11-08: rtfreporter@next 29e1c24 (its changes since ccbcfe4 are
pagination under `page_by` -- a listing record kept whole, a stub's
heading rows on each page, a total column's {n} -- and one new argument,
`plan_paginate_rows(na_label =)`, the text a blank BY value prints as)
and tflspec@next 8a948f7.  The new argument was passed on by its formals
already (yes); the run gives it a schema entry (its type and help).  The
run went mostly to cards / cardx.)

(2026-11-07: rtfreporter@next ccbcfe4 (its changes since 1e1a693 are
widen_ard()'s labels, plan_digits()' message for a row key's value and the
{PAGE_HEADING} title row -- which tflspec#703 takes up: no new exported
argument) and tflspec@next d237a12; the run went to cards / cardx -- the
table is unchanged.)

(2026-11-06: rtfreporter@next 1e1a693 (its changes since 0872829 are text
tokens filled once, natural order of long digit runs in file names, wording
and validation records: no new exported argument) and tflspec@next d220f07;
the run went to cards / cardx -- the table is unchanged.  The gaps are the
four below.)

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

Three rounds.  The first counted 2015 arguments (156 functions and geoms with
a stat); the second widened the scope by 19 functions (line widths, binned / stepped / default
colour scales, scale_x_time(), guide_axis_logticks() for a forest plot's
ratio axis, guide_axis_theta() / guide_axis_stack(), filled contours) to
2303 arguments of 175 functions; the third (2026-10-31) to ggplot2's
other functions of a plot -- 89 more functions, 3409 arguments of 264:
the limits (xlim(), ylim(), lims(), expand_limits()), annotation_logticks()
/ _custom() / _raster(), a facet's vars() and labellers, the other scales
of a size, an alpha, a shape, a line type and width, a colour or fill by
date, ordinal and binned, scale_discrete_manual() and the identity scales,
coord_transform(), facet_null(), theme_test(), ggplot2 4.0's
theme_sub_*() and element_geom() / _point() / _polygon(), rel(), unit(),
arrow(), margin_auto() / _part(), guide_custom(), geom_hex() and the
rarer stat_*() (2-d bins and summaries, contours, 2-d density, quantile,
align, connect, manual).

| Scope | Start | Round 1 | Round 2 (wider scope: before / after) | Round 3 (wider scope: before / after) |
|---|---:|---:|---:|---:|
| **ggplot2: geoms (with their stats), scales, coords, themes, facets, guides, labs, positions** | **24.7%** | **94.9%** | **91.4% / 100.0%** | **84.2% / 100.0%** |
| geom_*() | 55.9% | 91.4% | 90.6% / 100.0% | 99.4% / 100.0% |
| a geom with another stat (geom_step(stat = "ecdf"), geom_pointrange(stat = "summary") ...) | 0.0% | 100.0% | 100.0% / 100.0% | 100.0% / 100.0% |
| scale_*(), the limits and expansion() / sec_axis() / dup_axis() | 8.1% | 95.9% | 87.7% / 100.0% | 80.7% / 100.0% |
| coord_*() | 34.5% | 98.8% | 98.8% / 100.0% | 92.9% / 100.0% |
| theme(), theme_sub_*(), complete themes, element_*(), margin(), rel(), unit() | 5.0% | 91.2% | 91.2% / 100.0% | 75.0% / 100.0% |
| facet_wrap() / facet_grid() / facet_null(), vars(), labellers | 57.4% | 100.0% | 100.0% / 100.0% | 32.5% / 100.0% |
| guides() and guide_*() | 10.7% | 97.3% | 86.8% / 100.0% | 97.4% / 100.0% |
| labs(), xlab(), ylab(), ggtitle(), annotate(), annotation_*() | 38.8% | 97.4% | 97.4% / 100.0% | 69.2% / 100.0% |
| position_*() | 0.0% | 100.0% | 100.0% / 100.0% | 100.0% / 100.0% |
| **stat_*()** (shown apart) | **44.2%** | **73.1%** | **73.1% / 100.0%** | **82.4% / 100.0%** |

Round 2, 2303 arguments: before 1533 yes / 770 partial / 0 no; after 2303
yes.  Round 3, 3409 arguments: before 2303 yes / 1054 partial / 52 no
(xlim(), lims(), annotation_logticks(), the labellers, vars() were an
error of the check: no kind without a prefix); after 3409 yes.  (ggplot2
4.0's `box_gp`, `whisker_gp` ... are left out: geom_boxplot() makes them
from `whisker.colour` and the like, and given too they clash.)

100% is this measure's ceiling, not ggplot2's: every argument in the scope
has a card and a type, which says the GUI shows it and the check types its
value; the tests run the code of a sample of them (test-fig-ggplot2.R).
The extension packages are measured apart (below).  Left out by design:
maps and simple features (geom_sf*, coord_sf, coord_map / _quickmap,
geom_map, annotation_map / _borders), the deprecated (geom_errorbarh,
stat_spoke), the
programming helpers (aes_*(), layer(), ggproto ...), and ggplot2's other
spellings, which are not counted: the script checks that each of the 33
(scale_color_*, guide_colorbar, guide_colorsteps, theme_grey, stat_bin2d,
geom_density2d ...) reads as its function's card.

Round 3:

- A function of a plot without the prefix has a kind: xlim(), ylim() and
  lims() a scale's, annotation_*() and expand_limits() a layer's, vars(),
  labeller() and the label_*() labellers helpers (`.schema_gg_kind()`);
  before, the check said "a function neither the schema nor its package
  has".
- `xlim(0, 52)`: a card's positional `...` is written by position and read
  back (`limits: [0, 52]` in the SPEC), not `xlim(limits = c(0, 52))`.
- ggplot2's other spellings read as the card of the function they spell
  (`.schema_gg_alias()`: scale_color_manual is scale_colour_manual's card);
  the generated code keeps the name written.
- theme_sub_*() (ggplot2 4.0) are typed as the settings they stand for:
  theme_sub_axis_bottom(text =) is axis.text.x.bottom, an element_text;
  theme_sub_legend(position =) theme()'s legend.position.
- A facet's `labeller` takes a labeller's call ({fn: label_wrap_gen, args:
  {width: 20}}, {fn: labeller, args: {PARAMCD: !r label_both}});
  facet_wrap()'s `facets` the columns (`facets: PARAMCD`), as ggplot2 takes
  them (before: only !r vars(PARAMCD)).
- 89 cards (inst/spec/schema.yml), the arguments without a kind's type
  typed there: annotation_logticks()'s `base`, `sides`, `scaled`,
  `outside`, `short` / `mid` / `long`; element_geom()'s `ink`, `paper`,
  `accent`, `fontsize` ...; stat_connect()'s `connection`; unit()'s and
  rel()'s `x`; guide_custom()'s `grob`.

Round 4 (2026-11-01): a layer's aesthetics.  The arguments' measure says
nothing of what a layer's `mapping` takes, so each aesthetic of each layer
in the scope is a row of its own (`argument` "aes: ymin": 826 rows, 719 of
ggplot2's geoms and stats, 107 of the extensions' layers): **yes** when the
check judges a mapping's names (an unknown aesthetic warns) and the card
marks it as ggplot2 does -- required (or one of its alternatives:
geom_errorbar's `ymin` or `xmin`, a bar across) when the layer's
`required_aes` say so, not required when they do not; **partial** when one
of the two holds; **no** neither.  The same weights; the arguments' table
above is unchanged (3409 arguments, 100%).

| Aesthetics | Before | After |
|---|---:|---:|
| **ggplot2, the arguments and the geoms' aesthetics together** | **89.5%** | **100.0%** |
| geom_*()'s aesthetics | 42.1% | 100.0% |
| **stat_*()'s aesthetics** (shown apart) | **47.0%** | **100.0%** |
| the extensions' layers' aesthetics (ggrepel, ggtext, ggsurvfit) | 50.0% | 100.0% |
| rows yes / partial / no | 0 / 754 / 72 | 826 / 0 / 0 |

Before, no mapping's names were checked (`geom_line(mapping = {colr:
TRTA})` passed), and 16 cards wrote the aesthetic `y` bare, which YAML
reads as TRUE: `tfl_schema("geom_line")$aes_required` was `x, TRUE`, and
`y` came last, from the installed layer.  The 72 "no" rows: the `y` of
those cards, and the cards that marked x of an "x|y" layer (geom_bar,
geom_errorbar, stat_ecdf ...) with no word of y, or a fill ggplot2 does
not need (geom_tile, geom_raster).

Round 4:

- A layer's `mapping` is checked: an aesthetic the layer (its geom and its
  stat, the stat it is given too) does not have warns, as ggplot2's
  "Ignoring unknown aesthetics" does; ggplot2's other spellings (`color`,
  `pch`, `lwd`) are its own.
- A plot is checked for the aesthetics its layers need (ggplot2's
  `required_aes`, of the stat when the stat computes them: geom_bar's x or
  y, geom_step(stat = "ecdf")'s x or y): mapped in the layer, given a
  value (`geom_hline(yintercept = 0)`) or inherited from the first item's
  `ggplot(mapping =)` unless `inherit.aes: false`.  A missing one warns
  (`geom_errorbar needs the aesthetic ymax (or xmax)`), a note of
  tfl_check_spec(); ggplot2 would stop at the draw.  A plot not begun by
  ggplot() or a mapping written as R is not judged.
- The cards (inst/spec/schema.yml, 34 layers) mark what ggplot2 needs,
  with `or:` for the alternatives (`x: {required: true, or: [y]}`), and
  write `"y"` quoted; `tfl_schema()` says them as `aes_required` and
  `aes_or`.  geom_hline / _vline / _abline say `yintercept`, `xintercept`,
  `slope` and `intercept`.

### Round 5 (2026-11-02): ggplot2's own examples, through a SPEC and back

The arguments' and the aesthetics' measures are at their ceiling: they say
a SPEC can name every argument and the check types it, not that what
ggplot2's users write reads as a SPEC and draws the same.  A third measure
does: `dev/spec-coverage-ggplot2-examples.R` takes every call of a
function in the scope in the examples of ggplot2's help pages (759 calls,
1272 uses of an argument, 138 functions; read from the installed ggplot2,
not copied), reads it as a SPEC item (`.fig_lang_item()`, what tflspec
reads from R code), checks it, writes it back (`.fig_item_lang()`) and
draws the example with that code in its place (data, layout, theme,
labels and the scales' labels compared with the original's).  One row an
argument as the examples write it: `spec-coverage-ggplot2-examples.csv`
(the same columns, and `uses`).

- **yes**: read as a typed value (not `!r`), no problem of the check on it,
  and it draws the same (`!r NULL` and a number written as arithmetic,
  `!r 1/5`, count as typed: the check judges them).
- **partial**: read as R, or with R in it, or a warning of the check; it
  draws the same.
- **no**: an error of the check, the call not read as an item, or drawn
  otherwise (or not at all).

An example that does not draw the same twice by itself (data at random,
a global theme, a package not installed) is not judged on its drawing; a
call inside a function's definition (`aes({{ x }})`) is left out.  The
same weights.

| Examples | Before | After |
|---|---:|---:|
| **ggplot2: geoms, scales, coords, themes, facets, guides, labs, positions** | **89.7%** | **94.5%** |
| weight-3 functions only | 85.0% | 91.2% |
| geom_*() | 90.6% | 95.2% |
| scale_*() | 87.2% | 92.1% |
| coord_*() | 96.1% | 99.4% |
| theme(), elements | 94.1% | 96.8% |
| facets | 82.2% | 92.4% |
| guides | 72.0% | 88.2% |
| labs(), annotate(), annotation_*() | 89.3% | 96.8% |
| position_*() | 100.0% | 100.0% |
| **stat_*()** (shown apart) | **90.8%** | **92.1%** |
| uses yes / partial / no | 986 / 248 / 35 | 1100 / 169 / 3 |

Round 5:

- The check (false problems on ggplot2's own code): a layer's argument by
  ggplot2's other spelling (`color =`, `pch =`, `lty =`) is the aesthetic
  it spells; a layer takes what it passes on to `layer()` (`key_glyph`,
  ggplot2 4.0's `layout`) and its position's (4.0's `geom_text(nudge_x =)`,
  and `nudge_x` / `nudge_y` as aesthetics); `theme()` takes 4.0's
  `palette.colour.discrete` ... (14 settings, typed); a number written as
  arithmetic of numbers (`!r 1/5`, `!r -0.4 * pi`) is a number.
- Types (`inst/fig/gg_args.csv`, schema.yml): the vector forms ggplot2
  takes (`annotate(label = c(...))`, a 2-d `binwidth`, `expand = c(0.1,
  0.1)` on the position scales' cards, a 4.0 `palette` as colours), R where
  a value may be computed (`colours`, `colour`, `fill`, `quantiles`,
  `bins`, `args`, `lineend`, the geoms' and ggplot()'s `data`),
  `margin_auto()` / `margin_part()` as a margin, `facet_grid()`'s `rows` /
  `cols` as columns.  (`|r` comes last: the GUI's field is the same.)
- The reader: `aes(displ, hwy)` is x and y; a facet's variables as its
  columns (`~A`, `vars(A, B)`, `A ~ B`, `. ~ B`); any constructor of a
  value (a position, a guide, an element, `theme()`, `unit()`, `rel()`,
  `sec_axis()`, `margin_auto()` ...) as `{fn:, args:}`, its positional
  arguments named and R in it (before: R, unless every argument was named
  and a constant); a named vector of constants as a map; `1:5` as the
  numbers.  Fix: R of several statements (`function(d) { ...; ... }`,
  `quote({ ... })`) was joined into one line that did not parse; it keeps
  its lines.
- The writer: `unit()` and `rel()` by position (`unit(0.5, "cm")`, was
  `unit(x = 0.5, units = "cm")`); `facet_grid()`'s columns as `vars(SEX)`
  (ggplot2 takes no text for rows with cols).

What is left (partial): a function given by name (`fun = mean`, `method
= lm`, `fun = dnorm`), the labels and breaks functions of scales
(`scales::label_math()`), objects of the example (`values = cols`,
`breaks = price_bins`), dates (`Sys.Date() - 30`), and grobs:
R is the SPEC's form for them.  No: `expand_limits(positions)`, a data
frame given by position.

### Round 6 (2026-11-03): a function by its name, a formula, scales' helpers

The examples' measure (round 5), rerun after three typed forms for what
was R:

| Examples | Before | After |
|---|---:|---:|
| **ggplot2: geoms, scales, coords, themes, facets, guides, labs, positions** | **94.5%** | **95.3%** |
| geom_*() | 95.2% | 95.7% |
| scale_*() | 92.1% | 93.9% |
| theme(), elements | 96.8% | 97.0% |
| coord_*(), facets, guides, labs, positions | unchanged | unchanged |
| **stat_*()** (shown apart) | **92.1%** | **97.1%** |
| uses yes / partial / no | 1100 / 169 / 3 | 1141 / 128 / 3 |

The arguments' and the aesthetics' measures stay at 100% (3409 arguments,
826 aesthetics; `spec-coverage-ggplot2.csv` unchanged).

Round 6:

- A function by its name, the type `function`: `fun`, `fun.data`,
  `fun.min`, `fun.max` (stat_summary(), stat_summary_bin(), a geom with
  `stat: summary`, stat_function() / geom_function(), stat_manual(),
  stat_summary_2d()), a scale's `oob` and `rescaler`, sec_axis()'s
  `transform`.  The SPEC writes the name (`fun.data: mean_se`,
  `fun.data: Hmisc::smean.cl.normal`, `oob: scales::oob_squish`), the
  check takes a name R writes bare (an error for `mean se`), the code
  writes it bare: `stat_summary(fun = mean, fun.min = min, fun.max =
  max)`; read back from that code as the names (before: `!r mean`).
- A formula as its text, the type `tilde`: geom_smooth()'s `formula: y ~
  poly(x, 2)`, sec_axis()'s `transform: ~ . / 7`, a lambda `fun: ~
  sum(.x^2)`, written as the formula (before: only `!r y ~ x`).
- scales' helpers as `{fn:, args:}`: a scale's `labels`
  (`{fn: scales::label_percent, args: {accuracy: 1}}`, `label_*`,
  `*_format`), `breaks` / `minor_breaks` (`breaks_width`, `breaks_pretty`,
  `extended_breaks` ...), `transform` (`transform_log`, `*_trans`),
  `palette` (`pal_brewer` ...), on the scales and on sec_axis() /
  dup_axis(); `call<label_*>` in the type notation takes a function by a
  pattern of its name.  Read from code too (`percent_format()` of the
  scales attached as `scales::percent_format`).
- From code: a smooth's `method = lm` (glm, loess) is `"lm"`, a legend's
  `key_glyph = draw_key_rect` is `"rect"`, as ggplot2 takes them.

What is left (partial): a mapping's expressions (`factor(cyl)`,
`after_stat(count)`, `after_scale(alpha(colour, 0.4))`: R in the mapping),
objects of the example (`values = cols`, `labeller = to_string`), dates
(`Sys.Date() - 30`), grobs, R inside a scales helper
(`label_math(10^.x)`).  Next: a typed form for a mapping's `after_stat()`
and `factor()` of a column.

### Round 7 (2026-11-04): a mapping's expressions

The examples' measure (round 5), rerun after a typed form for what a
mapping takes besides a column:

| Examples | Before | After |
|---|---:|---:|
| **ggplot2: geoms, scales, coords, themes, facets, guides, labs, positions** | **95.3%** | **96.1%** |
| geom_*() | 95.7% | 97.6% |
| scale_*(), coord_*(), theme(), facets, guides, labs, positions | unchanged | unchanged |
| **stat_*()** (shown apart) | **97.1%** | **98.4%** |
| uses yes / partial / no | 1141 / 128 / 3 | 1180 / 89 / 3 |

The arguments' and the aesthetics' measures stay at 100% (3409 arguments,
826 aesthetics; `spec-coverage-ggplot2.csv` unchanged).

Round 7 (R/fig_aes.R):

- A mapping's value is a column's name, or an expression of a mapping's
  form written as its text, untagged (before: `!r`): a call of factor(),
  as.factor(), ordered(), as.numeric(), as.integer(), as.character(),
  interaction(), reorder(), rev(), abs(), sqrt(), exp(), log() / log2() /
  log10() / log1p(), round(), signif(), paste() / paste0(), sprintf(),
  format(), cut_width() / cut_interval() / cut_number(), after_stat(),
  after_scale() or stage(), or arithmetic, of columns, constants and
  sum(), max(), min(), mean(), median(), length(), alpha(), pmin(),
  pmax(), cumsum(), c().  `x: factor(AVISITN)`, `y: after_stat(density)`,
  `y: after_stat(count / sum(count))`, `x: reorder(USUBJID, -PCHG)` (a
  waterfall), `ymin: mean - se` (mean +/- SE), `label: paste0(n, " (",
  pct, "%)")`, `fill: after_scale(alpha(colour, 0.4))`.  Written as it
  is; read back from code as its text.
- A constant: a number (`group: 1`, before written `` `1` ``), a text in
  its quotes (`colour: '"Overall"'`, a legend's label); a column's name in
  backticks (`` `a b` ``).
- The check: a text that is neither a column's name nor of the form warns
  (it is written as a column of that name); an after_stat()'s variable
  the layer's stat does not compute warns (`after_stat(densty)` of
  geom_histogram; of geom_step(stat = "ecdf") only `ecdf`), the
  variables by stat (count, bin, density, ecdf, summary, boxplot,
  ydensity, smooth, qq, sum, 2-d bins, contours ...) tested against the
  installed ggplot2's layer data.

What is left (partial): R outside the form (`cut_width()` is in it now;
`n[!duplicated(group)]`, `ifelse()`), objects of the example (`values =
cols`, `labeller = to_string`), dates (`Sys.Date() - 30`), grobs, R
inside a scales helper (`label_math(10^.x)`); `size` of line geoms
(deprecated, a warning by design).  Next: the guides' examples (88.2%)
and the facets' (92.4%).

### Round 8 (2026-11-05): base R's values, and conditions in a mapping

The examples' measure (round 5), rerun after typed forms for what
clinical figures write with base R in a value or a mapping:

| Examples | Before | After |
|---|---:|---:|
| **ggplot2: geoms, scales, coords, themes, facets, guides, labs, positions** | **96.1%** | **96.6%** |
| geom_*() | 97.6% | 97.9% |
| scale_*() | 93.9% | 95.4% |
| coord_*(), theme(), facets, guides, labs, positions | unchanged | unchanged |
| **stat_*()** (shown apart) | **98.4%** | **98.4%** |
| uses yes / partial / no | 1180 / 89 / 3 | 1187 / 82 / 3 |

The arguments' and the aesthetics' measures stay at 100% (3409 arguments,
826 aesthetics; `spec-coverage-ggplot2.csv` rerun, only the date scales'
`where` changes).  ggplot2's examples rarely write the forms below; the
clinical figures do (an axis of weeks, a waterfall coloured by response,
a band in a transparent grey).

- A condition is of a mapping's form (R/fig_aes.R): the comparisons (`<`,
  `<=`, `==`, `!=`, `%in%`), `!`, `&`, `|`, and `ifelse()`, `cut()`,
  `is.na()`, `nchar()`, `toupper()` / `tolower()`, `substr()`,
  `trimws()`; inside one also `[`, `duplicated()`, `unique()`, `rank()`.
  `fill: PCHG <= -30`, `fill: ifelse(PCHG <= -30, "PR", "SD/PD")`,
  `colour: cut(PCHG, c(-Inf, -30, 20, Inf))`, `shape: '!is.na(ADT)'`
  (in YAML a text that starts with `!` is quoted).  Before: `!r`.
- `seq()` is a value of breaks: `{fn: seq, args: {from: 0, to: 52, by: 4}}`,
  written `seq(0, 52, by = 4)` (a scale's `breaks` and `minor_breaks`,
  the continuous position scales' cards, a histogram's `breaks`).
- `alpha()` is a colour: a layer's `colour` / `fill`, an element's, a
  continuous scale's `low` / `mid` / `high` / `na.value`
  (`alpha("grey50", 0.4)`).
- grDevices' palettes are a scale's `colours` (gradientn, stepsn) and a
  manual scale's `values`: `hcl.colors()`, `palette.colors()`,
  `gray.colors()`, `rainbow()`, `heat.colors()`, `terrain.colors()`,
  `topo.colors()`, `cm.colors()`.
- A date: the new type `date`, a date as its text (`2024-01-01`), for
  scale_x_date() / scale_y_date()'s `limits` and `breaks`; `NA` (YAML's
  text "NA") an open end; written `as.Date(c("2024-01-01", NA))` and
  read back from it.  Before, `limits: [2024-01-01, NA]` was written as
  text, which a date scale does not take.
- These calls are read from code as `{fn:, args:}` with their positional
  arguments named (`.fig_find_value_fun()`: base R's and grDevices'
  functions, seq()'s default method), and written back by position
  (`.fig_by_position`).

What is left (partial): objects of the example (`values = cols`,
`labeller = to_string`, `breaks = price_bins`), `Sys.Date()` (not a
constant), grobs and rasters, R inside a scales helper (`label_math(10^.x)`),
a function written inline (`fun = function(x) ...`), `size` of line geoms
(deprecated, a warning by design).  Next: the guides' examples (88.2%) and
the facets' (92.4%: `facet_wrap(year ~ cyl)`, a labeller's function).

### The extension packages (ggsurvfit, ggrepel, ggh4x, ggtext, ggnewscale, patchwork)

The same measure, the same file (`package` says which), over the
functions of a plot of the extension packages tflspec suggests: 46
functions, 351 arguments (ggsurvfit 1.2.1, ggrepel 0.9.8, ggh4x 0.3.1,
ggtext 0.2.0, ggnewscale 0.5.2, patchwork 1.3.2).  Weights: 3 for a KM
plot's (ggsurvfit(), add_risktable(), add_censor_mark(),
add_confidence_interval(), add_pvalue(), scale_ggsurvfit()), a label that
avoids overlaps (geom_text_repel()), markdown text (element_markdown())
and patchwork's arrangement (wrap_plots(), plot_layout(),
plot_annotation()); 2 sometimes; 1 rarely.  A function with no card is
named `pkg::fn` in a SPEC (`ggh4x::facet_manual`).

| Package | 2026-10-30 before | 2026-10-30 after |
|---|---:|---:|
| **extensions, weighted** | **58.9%** (106 yes / 240 partial / 5 no) | **100.0%** (351 yes) |
| ggsurvfit | 52.8% | 100.0% |
| ggrepel | 69.6% | 100.0% |
| ggh4x | 62.2% | 100.0% |
| ggtext | 77.0% | 100.0% |
| ggnewscale | 90.0% | 100.0% |
| patchwork | 45.6% | 100.0% |

ggplot2 itself stayed at 100.0% (2303 arguments; 3409 after round 3).

Added in this run:

- An extension's function without a card is its kind's, by `pkg::fn`
  (`.schema_ext_kind()`): ggh4x's facets, strips and element lists,
  ggtext's elements and geoms, ggsurvfit's `add_*()`, patchwork's
  arrangement; before, `ggh4x::facet_grid2` was checked by its formals
  only and `patchwork::plot_spacer()` was an error ("neither the schema
  nor its package has").
- Cards for 26 more functions: ggsurvfit's `add_legend_title()`,
  `add_risktable_strata_symbol()`, `stat_stepribbon()`,
  `scale_ggsurvfit()`, `theme_ggsurvfit_default()` / `_KMunicate()`,
  `theme_risktable_default()` / `_boxed()`; ggrepel's
  `position_nudge_repel()`; ggh4x's `facet_grid2()`, `facet_wrap2()`,
  `force_panelsizes()`, `strip_nested()` / `_themed()` / `_vanilla()`,
  `elem_list_text()` / `_rect()`; ggtext's `element_textbox()`,
  `geom_textbox()`; ggnewscale's `new_scale()`; patchwork's
  `plot_spacer()`, `wrap_elements()`, `inset_element()`, `guide_area()`,
  `area()`, `free()`.
- Typed by kind (`inst/fig/gg_args.csv`, 123 rows, new kinds `plot`,
  `ggsurvfit_layer`, `compose`): the risk table's `risktable_stats`,
  `stats_label`, `risktable_group`, `risktable_height`, `combine_groups`,
  `theme`; `add_pvalue()`'s `caption`, `location`, `rho`, `pvalue_fun`;
  `add_quantile()`'s `y_value` / `x_value`; ggrepel's `box.padding`,
  `point.padding`, `min.segment.length`, `force`, `max.overlaps`, `seed`
  ...; ggh4x's `independent`, `remove_labels`, `strip`, `nest_line`,
  `bleed`, `render_empty`; ggtext's `halign`, `padding`, `box.colour`,
  `width`, `orientation`; patchwork's `design`, `guides`, `axes`,
  `tag_levels`, `inset_element()`'s place and plot (an object by name).
  A card's shown `...` arguments are typed too (add_quantile's
  `linetype`, add_censor_mark's `shape`).
- `x_scales`, `y_scales` and `stats_label` are written `list(...)`.
- Fixes: ggrepel's `direction` is `both|y|x` (it was typed as
  geom_step's `hv|vh|mid`, so `direction: y` was an error); a ggsurvfit
  theme or `stat_stepribbon()` on a plain ggplot attaches ggsurvfit, a
  patchwork function inside a plot attaches patchwork.

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
