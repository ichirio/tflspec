# The arguments of a model cardx fits: `method.args`

cardx fits some models itself: `ard_survival_survfit()` the Kaplan-Meier
estimate
([`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html)),
`ard_regression()`, `ard_emmeans_emmeans()`, `ard_emmeans_contrast()`
and `ard_stats_anova()` the model their `method` and `package` name
(`"glm"`, `"coxph"` of survival, `"mmrm"` of mmrm ...). The model's own
arguments are written in `method.args = list(...)` of the analysis row's
`args`: `method.args = list(conf.type = "log-log")` (the Kaplan-Meier
interval SAS's PROC LIFETEST gives),
`method.args = list(ties = "breslow")` (a Cox model as SAS's PROC PHREG
fits it), `method.args = list(family = binomial)` (a logistic model),
`method.args = list(method = "Kenward-Roger")` (MMRM's degrees of
freedom),
`method.args = list(covariance = cov_struct("toep", visits = "AVISIT", subject = "USUBJID"))`
(MMRM's covariance when the formula has none),
`method = "glm.nb", package = "MASS"` with
`method.args = list(link = log)` (a negative binomial rate model:
exacerbations, recurrent events) and
`method = "survreg", package = "survival"` with
`method.args = list(dist = "lognormal")` (a parametric survival model).
The same arguments are the model's in a call written in `x`
(`ard_survival_survfit_diff(x = survival::survfit(..., conf.type = "log-log"))`).

## Usage

``` r
tfl_ard_model_args(method, package = "base")
```

## Arguments

- method:

  The model function: `"survfit"`, `"coxph"`, `"glm"`, or `"pkg::fun"`.

- package:

  The package `method` is in, as cardx's `package =` (`"base"`: the
  search path, stats then survival).

## Value

A data frame, one row per argument the installed model function takes
(`formula`, `data`, `x`, `y` and `...` left out; those its `...` reach
after its own): `fn`, `arg`, `default`, `kind`, `hint`, `choices`,
`described` (whether the catalog describes it). NULL when the function
cannot be found.

## Details

[`tfl_ard_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_ard_spec.md)
looks for each name in the installed model function (and where its `...`
go: survfit()'s survfitKM(), glm()'s and glm.nb()'s glm.control(),
survreg()'s survreg.control(), coxph()'s coxph.control(), mmrm()'s
mmrm_control()) and judges the values this catalog describes by their
kind: `conf.type = "loglog"` would stop the program, `tise = "breslow"`
too, and `wieghts = W` would be ignored by
[`lm()`](https://rdrr.io/r/stats/lm.html); a covariance type mmrm does
not have (`cov_struct("un", ...)`, `~ un(AVISIT | USUBJID)`), the visit
column not in quotes, `weights = "WTS"` given to mmrm() (a vector, not a
column name), `link = logit` of glm.nb() and `init.theta = 0` too.
`formula` and `data` are cardx's to give.

The catalog is `inst/ard/model_args.csv` (`fn`, `arg`, `default`,
`kind`, `hint`, `choices`); a company adds rows with
`options(tflspec.ard_model_args = <data frame>)`.

## Examples

``` r
tfl_ard_model_args("survfit", "survival")
#>                   fn        arg default     kind
#> 1  survival::survfit    weights    <NA> variable
#> 2  survival::survfit     subset    <NA>     code
#> 3  survival::survfit  na.action    <NA>     code
#> 4  survival::survfit      stype       1   choice
#> 5  survival::survfit      ctype       1   choice
#> 6  survival::survfit         id    <NA> variable
#> 7  survival::survfit    cluster    <NA> variable
#> 8  survival::survfit     robust    <NA>  logical
#> 9  survival::survfit     istate    <NA>     code
#> 10 survival::survfit    timefix    TRUE  logical
#> 11 survival::survfit      etype    <NA>     code
#> 12 survival::survfit      model   FALSE  logical
#> 13 survival::survfit      error    <NA>     code
#> 14 survival::survfit      entry   FALSE  logical
#> 15 survival::survfit      time0   FALSE  logical
#> 16 survival::survfit     se.fit    TRUE  logical
#> 17 survival::survfit   conf.int    0.95   number
#> 18 survival::survfit  conf.type     log   choice
#> 19 survival::survfit conf.lower   usual   choice
#> 20 survival::survfit start.time    <NA>   number
#> 21 survival::survfit  influence   FALSE  logical
#> 22 survival::survfit       type    <NA>     code
#>                                                                                                                            hint
#> 1                                                                      Case weights: a column of the data, not in quotes (WTS).
#> 2                                                                                                                              
#> 3                                                                                                                              
#> 4                                   How survival is estimated: 1 the Kaplan-Meier product (default), 2 exp(-cumulative hazard).
#> 5                           The cumulative hazard: 1 Nelson-Aalen (default), 2 with the Fleming-Harrington correction for ties.
#> 6                                                 The subject column, not in quotes (USUBJID), when a subject has several rows.
#> 7                                                 The cluster of the robust standard errors: a column, not in quotes (USUBJID).
#> 8                                                      TRUE: robust (infinitesimal jackknife) standard errors of the estimates.
#> 9                                                                                                                              
#> 10                                                        TRUE (default): times that differ only by rounding are taken as tied.
#> 11                                                                                                                             
#> 12                                                                                                                             
#> 13                                                                                                                             
#> 14                                                                                                                             
#> 15                                                                                                                             
#> 16                                                     TRUE (default): the standard errors, and so the intervals, are computed.
#> 17                                           Confidence level of the Kaplan-Meier estimates' intervals, between 0 and 1 (0.95).
#> 18 How the interval is computed: log (survival's default), log-log (SAS PROC LIFETEST's default), plain, logit, arcsin or none.
#> 19                                        The lower limit's standard error: usual (default), peto or modified (Dorey and Korn).
#> 20                                                     Survival from this time on: the estimate conditional on surviving to it.
#> 21                                                                                                                             
#> 22                                                                                                                             
#>                                          choices described
#> 1                                                     TRUE
#> 2                                                    FALSE
#> 3                                                    FALSE
#> 4                                          1 | 2      TRUE
#> 5                                          1 | 2      TRUE
#> 6                                                     TRUE
#> 7                                                     TRUE
#> 8                                                     TRUE
#> 9                                                    FALSE
#> 10                                                    TRUE
#> 11                                                   FALSE
#> 12                                                   FALSE
#> 13                                                   FALSE
#> 14                                                   FALSE
#> 15                                                   FALSE
#> 16                                                    TRUE
#> 17                                                    TRUE
#> 18 log | log-log | plain | none | logit | arcsin      TRUE
#> 19                       usual | peto | modified      TRUE
#> 20                                                    TRUE
#> 21                                                   FALSE
#> 22                                                   FALSE
tfl_ard_model_args("glm")
#>            fn         arg   default     kind
#> 1  stats::glm      family  gaussian   family
#> 2  stats::glm     weights      <NA> variable
#> 3  stats::glm      subset      <NA>     code
#> 4  stats::glm   na.action      <NA>     code
#> 5  stats::glm       start      NULL     code
#> 6  stats::glm    etastart      <NA>     code
#> 7  stats::glm     mustart      <NA>     code
#> 8  stats::glm      offset      <NA> variable
#> 9  stats::glm     control list(...)     code
#> 10 stats::glm       model      TRUE  logical
#> 11 stats::glm      method "glm.fit"     text
#> 12 stats::glm singular.ok      TRUE  logical
#> 13 stats::glm   contrasts      NULL     list
#> 14 stats::glm     epsilon     1e-08   number
#> 15 stats::glm       maxit        25   number
#> 16 stats::glm       trace     FALSE  logical
#>                                                                                                                            hint
#> 1  The error distribution and link: binomial (logistic regression: odds ratios), poisson (rates), binomial(link = "probit") ...
#> 2                                                                     Prior weights: a column of the data, not in quotes (WTS).
#> 3                                                                                                                              
#> 4                                                                                                                              
#> 5                                                                                                                              
#> 6                                                                                                                              
#> 7                                                                                                                              
#> 8                                                      An offset of the linear predictor: log(EXPDUR) for a Poisson rate model.
#> 9                                                                                                                              
#> 10                                                                                                                             
#> 11                                                                                                                             
#> 12                                                    TRUE (default): a term that cannot be estimated is dropped, not an error.
#> 13                                                                    The contrasts of the factors: list(TRT01P = "contr.sum").
#> 14                                                                             The fit's convergence tolerance (glm.control()).
#> 15                                                                           The most iterations the fit takes (glm.control()).
#> 16                                                                            TRUE: the iterations are printed (glm.control()).
#>    choices described
#> 1               TRUE
#> 2               TRUE
#> 3              FALSE
#> 4              FALSE
#> 5              FALSE
#> 6              FALSE
#> 7              FALSE
#> 8               TRUE
#> 9              FALSE
#> 10             FALSE
#> 11             FALSE
#> 12              TRUE
#> 13              TRUE
#> 14              TRUE
#> 15              TRUE
#> 16              TRUE
tfl_ard_model_args("glm.nb", "MASS")
#>              fn        arg          default     kind
#> 1  MASS::glm.nb    weights             <NA> variable
#> 2  MASS::glm.nb     subset             <NA>     code
#> 3  MASS::glm.nb  na.action             <NA>     code
#> 4  MASS::glm.nb      start             NULL     code
#> 5  MASS::glm.nb   etastart             <NA>     code
#> 6  MASS::glm.nb    mustart             <NA>     code
#> 7  MASS::glm.nb    control glm.control(...)     code
#> 8  MASS::glm.nb     method        "glm.fit"     text
#> 9  MASS::glm.nb      model             TRUE  logical
#> 10 MASS::glm.nb  contrasts             NULL     list
#> 11 MASS::glm.nb init.theta             <NA>   number
#> 12 MASS::glm.nb       link              log     link
#> 13 MASS::glm.nb    epsilon            1e-08   number
#> 14 MASS::glm.nb      maxit               25   number
#> 15 MASS::glm.nb      trace            FALSE  logical
#>                                                                                                     hint
#> 1                                              Prior weights: a column of the data, not in quotes (WTS).
#> 2                                                                                                       
#> 3                                                                                                       
#> 4                                                                                                       
#> 5                                                                                                       
#> 6                                                                                                       
#> 7                                                                                                       
#> 8                                                                                                       
#> 9                                                                                                       
#> 10                                             The contrasts of the factors: list(TRT01P = "contr.sum").
#> 11                    A starting value of the dispersion theta (above 0); by default from a Poisson fit.
#> 12 The link of the negative binomial model: log (default: rate ratios), sqrt or identity, not in quotes.
#> 13                                                      The fit's convergence tolerance (glm.control()).
#> 14                                                    The most iterations the fit takes (glm.control()).
#> 15                                                     TRUE: the iterations are printed (glm.control()).
#>                  choices described
#> 1                             TRUE
#> 2                            FALSE
#> 3                            FALSE
#> 4                            FALSE
#> 5                            FALSE
#> 6                            FALSE
#> 7                            FALSE
#> 8                            FALSE
#> 9                            FALSE
#> 10                            TRUE
#> 11                            TRUE
#> 12 log | sqrt | identity      TRUE
#> 13                            TRUE
#> 14                            TRUE
#> 15                            TRUE
```
