# ============================================================================
#  nlme's models in a regression row (since 2026-11-19): `nlme::gls()` --
#  repeated measures with an unstructured covariance, SAS PROC MIXED's
#  REPEATED / TYPE=UN with GROUP= written as corSymm() and varIdent() -- and
#  `nlme::lme()` -- a random intercept or slope.  Neither takes `formula =`
#  (gls()'s model is `model =`, lme()'s `fixed =`), so cardx cannot fit
#  them from `method` / `package` (its call is `method(formula = formula,
#  data = data, ...)`): the model is written in `x` of ard_regression()
#  (`x = nlme::gls(AVAL ~ TRT01P * AVISIT, data = data, correlation =
#  corSymm(form = ~ AVISITN | USUBJID), weights = varIdent(form = ~ 1 |
#  AVISIT))`).  The values of the arguments inst/ard/model_args.csv
#  describes are judged here by their kind:
#    twosided   a two-sided formula (the model);
#    corstruct  an nlme corStruct call with the subject after `|` and, for
#               the ones that read the records' positions, a visit number
#               before it;
#    varfunc    an nlme varFunc call, a one-sided formula or text;
#    random     lme()'s random effects: a formula with `|`, a list or a
#               pdMat.
# ============================================================================

# The model functions whose formula argument has another name: a row that
# names them in `method` stops in cardx ("unused argument (formula = ...)")
.model_formula_arg <- c(`nlme::gls` = "model", `nlme::lme` = "fixed")

# nlme's correlation structures, and those whose covariate is the record's
# position (by default the records' order within the subject): corSymm()'s
# position, corAR1()'s and corARMA()'s lag.  A missed visit moves the later
# records up one place, with no warning (nlme 3.1: the treatment estimate
# of a gls() with one missed visit in a third of the subjects moved from
# -0.71 to -0.59 with `form = ~ 1 | ID` instead of `~ VIS | ID`).
.nlme_cor <- c("corSymm", "corCompSymm", "corAR1", "corCAR1", "corARMA",
               "corExp", "corGaus", "corLin", "corRatio", "corSpher",
               "corIdent", "corNatural")
.nlme_cor_position <- c("corSymm", "corAR1", "corARMA")
.nlme_var <- c("varIdent", "varFixed", "varPower", "varExp", "varConstPower",
               "varConstProp", "varComb")
.nlme_pd <- c("pdSymm", "pdDiag", "pdIdent", "pdCompSymm", "pdBlocked",
              "pdLogChol", "pdNatural")

# The function a call names, without its package (nlme::corSymm -> corSymm)
.call_head <- function(e) {
  if (!is.call(e)) return("")
  h <- paste(deparse(e[[1L]]), collapse = "")
  sub("^nlme:::?", "", h)
}

# A one-sided formula's right-hand side split at `|`: list(covariate,
# group), the group NULL when there is no `|`; NULL when `e` is not a
# one-sided formula
.formula_bar <- function(e) {
  if (!is.call(e) || !identical(e[[1L]], as.name("~")) || length(e) != 2L) {
    return(NULL)
  }
  rhs <- e[[2L]]
  if (is.call(rhs) && identical(rhs[[1L]], as.name("|"))) {
    return(list(covariate = rhs[[2L]], group = rhs[[3L]]))
  }
  list(covariate = rhs, group = NULL)
}

# Why a value does not fit an nlme argument of this kind, or NULL
.nlme_value_problem <- function(kind, e, shown) {
  switch(kind,
    twosided = .nlme_twosided_problem(e, shown),
    corstruct = .nlme_cor_problem(e, shown),
    varfunc = .nlme_var_problem(e, shown),
    random = .nlme_random_problem(e, shown),
    NULL)
}

.nlme_twosided_problem <- function(e, shown) {
  # (a name is the program's formula)
  if (is.name(e)) return(NULL)
  if (is.call(e) && identical(e[[1L]], as.name("~")) && length(e) == 3L) return(NULL)
  sprintf("is a two-sided formula, not in quotes: AVAL ~ TRT01P * AVISIT; got %s", shown)
}

.nlme_cor_problem <- function(e, shown) {
  if (is.name(e) || is.null(e)) return(NULL)
  h <- .call_head(e)
  want <- "is an nlme corStruct: corSymm(form = ~ AVISITN | USUBJID)"
  if (!h %in% .nlme_cor) {
    return(sprintf("%s (one of %s); got %s", want,
                   paste(.nlme_cor[1:5], collapse = ", "), shown))
  }
  a <- as.list(e)[-1L]
  nm <- names(a) %||% rep("", length(a))
  form <- a[["form"]]
  if (is.null(form)) {
    # (corSymm(value, form, fixed): form is the second by position, the
    # first of corAR1(value, form, fixed) and corARMA(value, form, p, q,
    # fixed) too)
    pos <- which(!nzchar(nm))
    if (length(pos) >= 2L) form <- a[[pos[2L]]]
  }
  if (is.null(form)) {
    return(sprintf(paste("%s: %s() is given no `form`, so the whole data is one",
                         "subject's series -- write form = ~ %s | USUBJID; got %s"),
                   want, h, if (h %in% .nlme_cor_position) "AVISITN" else "1", shown))
  }
  if (is.name(form)) return(NULL)
  fb <- .formula_bar(form)
  if (is.null(fb)) {
    return(sprintf("%s: `form` is a one-sided formula (~ AVISITN | USUBJID); got %s",
                   want, shown))
  }
  out <- character()
  if (is.null(fb$group)) {
    out <- c(out, sprintf(paste(
      "`form` has no subject after | (~ %s | USUBJID), so the whole data would be one",
      "subject's series"), paste(deparse(fb$covariate), collapse = "")))
  }
  if (h %in% .nlme_cor_position) {
    cv <- fb$covariate
    if (is.numeric(cv) && identical(as.numeric(cv), 1)) {
      out <- c(out, sprintf(paste(
        "%s(form = ~ 1 | ...) reads the visits from the records' order, so a missed",
        "visit pairs each later visit with the wrong one, with no warning -- write the",
        "visit number (1, 2, 3 ...): form = ~ AVISITN | USUBJID"), h))
    } else if (is.name(cv) && grepl("^A?VISIT$|^AVISIT|VISIT$", as.character(cv)) &&
               !grepl("N$", as.character(cv))) {
      out <- c(out, sprintf(paste(
        "%s() needs the visit as a whole number (1, 2, 3 ...), not the visit's text %s:",
        "write AVISITN (nlme stops on a text or factor column)"),
        h, as.character(cv)))
    }
  }
  if (!length(out)) return(NULL)
  sprintf("%s: %s; got %s", want, paste(out, collapse = "; "), shown)
}

.nlme_var_problem <- function(e, shown) {
  if (is.name(e) || is.null(e) || is.character(e)) return(NULL)
  if (is.call(e) && identical(e[[1L]], as.name("~")) && length(e) == 2L) return(NULL)
  if (.call_head(e) %in% .nlme_var) return(NULL)
  sprintf(paste("is an nlme varFunc: varIdent(form = ~ 1 | AVISIT) (a variance a visit),",
                "or ~ X (the variance proportional to X); got %s"), shown)
}

.nlme_random_problem <- function(e, shown) {
  if (is.name(e) || is.null(e)) return(NULL)
  h <- .call_head(e)
  if (h %in% c("list", .nlme_pd)) return(NULL)
  fb <- .formula_bar(e)
  if (!is.null(fb) && !is.null(fb$group)) return(NULL)
  sprintf(paste("is the random effects with the subject after |: ~ 1 | USUBJID,",
                "~ AVISITN | USUBJID, a list or a pdMat (lme() stops without the",
                "subject); got %s"), shown)
}

# A row that names gls() or lme() in `method`: cardx would give it
# `formula =`, which it does not have.  One line, or character().
.model_formula_problem <- function(tag, fn, key) {
  if (is.na(key) || !key %in% names(.model_formula_arg)) return(character())
  sprintf(paste(
    "%s: %s() fits `method` as %s(formula = , data = ), but %s() takes its model",
    "in `%s =`, not `formula =` -- the program would stop on it; write the model in",
    "x = %s(%s = AVAL ~ ..., data = data, ...)"),
    tag, fn, sub("^.*::", "", key), sub("^.*::", "", key),
    .model_formula_arg[[key]], key, .model_formula_arg[[key]])
}

# A regression row with gls() or lme() in `x`: broom.helpers 1.23 cannot
# read these models' variables (their model.frame() fails), so cardx gives
# the coefficients by term ("TRTAXanomeline High Dose"), with no reference
# row and no variable / level a table can lay out, and stops on
# `add_pairwise_contrasts = TRUE`.  Said once, with the way to LS means.
.nlme_tidy_problem <- function(tag, fn, key, given) {
  if (!key %in% names(.model_formula_arg)) return(character())
  out <- sprintf(paste(
    "%s: %s(x = %s()) gives the coefficients by term (TRTAXanomeline High Dose),",
    "not by variable and level: broom.helpers cannot read %s()'s variables, so",
    "the table lays them out by term; for LS means and their differences fit the",
    "same model with method = \"lsmeans\" / \"lsmeans_diff\" and mmrm (method =",
    "\"mmrm\", package = \"mmrm\", covariance = cov_struct(\"us\", ...))"),
    tag, fn, sub("^.*::", "", key), sub("^.*::", "", key))
  pw <- .literal_value(given[["add_pairwise_contrasts"]] %||% FALSE)
  if (isTRUE(pw)) {
    out <- c(out, sprintf(paste(
      "%s: %s(add_pairwise_contrasts = TRUE) with x = %s(): broom.helpers cannot",
      "list the model's variables, so the program would stop on it"),
      tag, fn, sub("^.*::", "", key)))
  }
  out
}
