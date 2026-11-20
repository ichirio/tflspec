# ============================================================================
#  The scale a model row's estimates are on, judged by the model the row
#  fits (since 2026-11-16).  Two settings give numbers on the wrong scale
#  with no error: `exponentiate = TRUE` on a model with no ratios, and an
#  LS means row of a logistic (or count) model left on the link scale.
#
#  `exponentiate = TRUE` in a regression row (cardx::ard_regression(),
#  ard_regression_basic()), judged by the model the row fits.  The
#  estimates and their confidence limits are exp()'d, which gives a ratio
#  only when the model's coefficients are on a log scale: a log or logit
#  link (rate, risk and odds ratios), a complementary log-log one (hazard
#  ratios of grouped times), a Cox model.  Seen with cardx 0.3.4 and
#  broom.helpers 1.23.0:
#
#  * a linear model (`lm`, `aov`, `mmrm`, `nlme::gls()` / `lme()`) or a
#    glm with no family (gaussian) or an identity, inverse, probit ... link
#    gives exp() of differences -- `exp(-0.83) = 0.437` for a difference of
#    mean ages -- with no warning (lm() alone is stopped by
#    broom.helpers, and cardx keeps that error in the ARD: the cells come
#    out empty);
#  * survival::survreg(): broom's tidy.survreg() stops on it, and cardx
#    keeps that error: the cells come out empty.
#
#  Said as a warning when the spec is read (.exponentiate_problems()).  A
#  model the row does not name (`x = fit`), a family held in an object of
#  the program, or a study's own tidier (`tidy_fun =`) is not judged.
#
#  An LS means row (cardx::ard_emmeans_emmeans(), ard_emmeans_contrast():
#  the keywords lsmeans / lsmeans_diff) gives emmeans' estimates on the
#  model's link scale unless `response_type = "dichotomous"`, which has
#  cardx regrid them to the response (emmeans(regrid = "response")).  A
#  logistic model left at the default ("continuous") gives each arm's log
#  odds -- 0.47 for a proportion of 61.6% -- and the differences of log
#  odds, with no warning (cardx 0.3.4).  Said by
#  .emmeans_scale_problems().
# ============================================================================

# The models whose coefficients are on the response's own scale: means
# (and survreg()'s, the log of time, which broom will not exp())
.exponentiate_never <- c("stats::lm", "stats::aov", "mmrm::mmrm",
                         "nlme::gls", "nlme::lme", "survival::survreg")

# The links whose coefficients exp() into a ratio
.exponentiate_links <- c("log", "logit", "cloglog")

# The default link of each family glm() and geeglm() take
.family_default_link <- c(
  binomial = "logit", quasibinomial = "logit", poisson = "log",
  quasipoisson = "log", gaussian = "identity", Gamma = "inverse",
  inverse.gaussian = "1/mu^2", quasi = "identity")

# The model a regression row fits and the arguments written for it: from a
# model's call in `x` (`x = survival::coxph(...)`) or from `method` /
# `package` with `method.args = list(...)`.  list(key = "pkg::fun" or NA,
# args = a named list of expressions, as written; NULL when the arguments
# are not written out (`method.args = my_args`).
.regression_model <- function(a) {
  x <- a[["x"]]
  if (is.call(x)) {
    head <- paste(deparse(x[[1L]]), collapse = "")
    key <- if (grepl("::", head, fixed = TRUE)) sub(":::", "::", head, fixed = TRUE) else
      .model_key(head, "base")
    return(list(key = key, args = as.list(x)[-1L]))
  }
  if (is.null(a[["method"]])) return(list(key = NA_character_, args = NULL))
  p <- if (is.null(a[["package"]])) "base" else .literal_value(a[["package"]])
  key <- if (is.null(p)) NA_character_ else .model_key(a[["method"]], p)
  ma <- a[["method.args"]]
  els <- if (is.null(ma)) list() else
    if (is.call(ma) && identical(ma[[1L]], as.name("list"))) as.list(ma)[-1L] else NULL
  list(key = key, args = els)
}

# The link a family is written with (`binomial`, `binomial(link =
# "probit")`, `"poisson"`, `stats::Gamma(link = log)`), or NA when it
# cannot be told (an object of the program: `family = fam`)
.family_link <- function(e) {
  name_of <- function(h) {
    h <- paste(deparse(h), collapse = "")
    sub("^stats::", "", h)
  }
  if ((is.character(e) && length(e) == 1L) || is.name(e)) {
    v <- .family_default_link[as.character(e)]
    return(if (is.na(v)) NA_character_ else unname(v))
  }
  if (!is.call(e)) return(NA_character_)
  fam <- name_of(e[[1L]])
  if (fam %in% c("::", ":::")) {
    return(.family_link(as.name(as.character(e[[3L]]))))
  }
  if (!fam %in% names(.family_default_link)) return(NA_character_)
  args <- as.list(e)[-1L]
  nm <- names(args) %||% rep("", length(args))
  ln <- if ("link" %in% nm) args[["link"]] else if (length(args) && !nzchar(nm[1L])) args[[1L]] else NULL
  if (is.null(ln)) return(unname(.family_default_link[fam]))
  if (is.name(ln)) return(as.character(ln))
  if (is.character(ln) && length(ln) == 1L) return(ln)
  NA_character_
}

# Why `exponentiate = TRUE` gives no ratio for the model `key` fitted with
# `els` (its arguments as written), or NULL when it does (or cannot be
# told)
.exponentiate_reason <- function(key, els) {
  if (is.na(key)) return(NULL)
  if (key %in% .exponentiate_never) {
    fn <- sub("^.*::", "", key)
    if (identical(key, "survival::survreg")) {
      return(paste(
        "broom's tidy.survreg() stops on it, and cardx keeps that error in the",
        "ARD: the cells would come out empty (exp() of its coefficients, the",
        "time ratios, is for the program to compute)"))
    }
    return(sprintf(paste(
      "%s() estimates differences on the response's own scale, which exp()",
      "does not make into ratios%s"), fn,
      if (identical(key, "stats::lm")) paste(
        " -- and broom.helpers stops on it: cardx keeps that error in the",
        "ARD, so the cells would come out empty") else
        paste(" -- the table would show exp() of each difference, with no",
              "warning")))
  }
  if (is.null(els)) return(NULL)
  link <- NA_character_
  what <- ""
  if (key %in% c("stats::glm", "geepack::geeglm")) {
    fam <- els[["family"]]
    if (is.null(fam)) {
      link <- "identity"
      what <- "with no family (gaussian, an identity link)"
    } else {
      link <- .family_link(fam)
      what <- sprintf("with `family = %s` (its link: %s)",
                      paste(deparse(fam, width.cutoff = 500L), collapse = " "), link)
    }
  } else if (identical(key, "MASS::glm.nb")) {
    ln <- els[["link"]]
    if (is.null(ln)) return(NULL)
    link <- if (is.name(ln)) as.character(ln) else .literal_value(ln)
    if (!is.character(link) || length(link) != 1L) return(NULL)
    what <- sprintf("with `link = %s`", link)
  } else if (identical(key, "MASS::polr")) {
    m <- els[["method"]]
    if (is.null(m)) return(NULL)
    m <- .literal_value(m)
    if (!is.character(m) || length(m) != 1L) return(NULL)
    link <- c(logistic = "logit", cloglog = "cloglog")[m]
    if (is.na(link)) link <- m
    what <- sprintf("with `method = \"%s\"`", m)
  } else {
    return(NULL)
  }
  if (is.na(link) || link %in% .exponentiate_links) return(NULL)
  sprintf(paste(
    "%s() %s estimates differences on that scale, which exp() does not make",
    "into ratios -- the table would show exp() of each difference, with no",
    "warning (odds, risk and rate ratios need a logit or log link)"),
    sub("^.*::", "", key), what)
}

# The problem of `exponentiate = TRUE` in a row of `call` (its `args` as R
# text), one line or none
.exponentiate_problems <- function(tag, call, args) {
  if (!call %in% c("cardx::ard_regression", "cardx::ard_regression_basic")) {
    return(character())
  }
  a <- tryCatch(.args_list(args), error = function(e) list())
  if (!isTRUE(.literal_value(a[["exponentiate"]] %||% FALSE))) return(character())
  if ("tidy_fun" %in% names(a)) return(character())
  m <- .regression_model(a)
  why <- .exponentiate_reason(m$key, m$args)
  if (is.null(why)) return(character())
  sprintf("%s: %s(exponentiate = TRUE): %s; leave `exponentiate` out",
          tag, sub("^.*::", "", call), why)
}

# The cardx functions of the LS means rows
.emmeans_rows <- c("cardx::ard_emmeans_emmeans", "cardx::ard_emmeans_contrast",
                   "cardx::ard_emmeans_mean_difference")

# The problem of an LS means row whose model has a link other than the
# identity (a logistic, Poisson or negative binomial model) and whose
# `response_type` is not "dichotomous": its estimates are on the link
# scale.  One line or none.
.emmeans_scale_problems <- function(tag, call, args) {
  if (!call %in% .emmeans_rows) return(character())
  a <- tryCatch(.args_list(args), error = function(e) list())
  rt <- a[["response_type"]]
  if (!is.null(rt)) {
    rt <- .literal_value(rt)
    # (an object of the program, or "dichotomous": the program's / right)
    if (!identical(rt, "continuous")) return(character())
  }
  m <- .regression_model(a)
  if (is.na(m$key) || is.null(m$args)) return(character())
  link <- NA_character_
  if (m$key %in% c("stats::glm", "geepack::geeglm")) {
    fam <- m$args[["family"]]
    if (is.null(fam)) return(character())
    link <- .family_link(fam)
  } else if (identical(m$key, "MASS::glm.nb")) {
    ln <- m$args[["link"]]
    link <- if (is.null(ln)) "log" else if (is.name(ln)) as.character(ln) else
      .literal_value(ln)
  }
  if (!is.character(link) || length(link) != 1L || is.na(link) ||
      identical(link, "identity")) return(character())
  what <- c(logit = "log odds", log = "logs of the means (log rates or risks)",
            cloglog = "complementary log-logs", probit = "probits")[link]
  if (is.na(what)) what <- sprintf("the %s scale", link)
  sprintf(paste(
    "%s: %s() fits %s() with a %s link and %s, so its estimates would be",
    "%s, not %s -- write `response_type = \"dichotomous\"` (cardx then",
    "regrids them to the response scale)"),
    tag, sub("^.*::", "", call), sub("^.*::", "", m$key), link,
    if (is.null(rt)) "no response_type (\"continuous\")" else
      "response_type = \"continuous\"",
    what, if (identical(link, "logit")) "proportions" else "means of the response")
}
