# ============================================================================
#  The arguments of the model a cardx function fits.  cardx fits the model
#  itself from `method` / `package` and passes `method.args = list(...)` on
#  to it (`ard_survival_survfit()` to survival::survfit(), `ard_regression()`,
#  `ard_emmeans_*()` and `ard_stats_anova()` to `package::method(formula,
#  data, ...)`), or takes a model's call in `x`
#  (`ard_survival_survfit_diff(x = survival::survfit(...))`,
#  `ard_regression(x = survival::coxph(...))`).  The arguments written
#  there are the model function's: catalogued in inst/ard/model_args.csv
#  (the Kaplan-Meier interval's `conf.type`, a Cox model's `ties`, a
#  logistic model's `family`, MMRM's degrees of freedom `method` ...), their
#  names looked for in the installed model function and their values
#  judged by their kind.  tfl_ard_model_args() lists them.
# ============================================================================

# Where a model function's `...` go, when it names them: an argument it
# does not have stops it there (survfit()'s survfitKM(), glm()'s
# glm.control(), coxph()'s coxph.control()).  `lm` and `aov` pass `...` to
# lm.fit(), which ignores (warns about) a name it does not have: judged
# too, as "ignored".  A model function with `...` not listed here takes
# what it does not name: not judged.
.model_dots_to <- list(
  `survival::survfit` = c("survival:::survfit.formula", "survival:::survfitKM"),
  `survival::coxph` = "survival::coxph.control",
  `survival::survreg` = "survival::survreg.control",
  `stats::glm` = "stats::glm.control",
  `stats::lm` = character(),
  `stats::aov` = "stats::lm",
  `mmrm::mmrm` = c("mmrm::mmrm_control", "mmrm:::h_get_optimizers"))
.model_dots_ignored <- c("stats::lm", "stats::aov")

# `pkg::fun` or `pkg:::fun` -> the function, or NULL
.model_fun <- function(key) {
  pkg <- sub(":::?.*$", "", key)
  fn <- sub("^.*:::?", "", key)
  if (!nzchar(pkg) || !requireNamespace(pkg, quietly = TRUE)) return(NULL)
  get0(fn, envir = asNamespace(pkg), mode = "function")
}

# The model function a `method` (and `package`) name, as `pkg::fun`:
# `method = "coxph", package = "survival"` is survival::coxph; `method =
# "glm"` with the default package ("base": the search path) is stats::glm.
# `method` may be written as a function too (`method = survival::coxph`).
# NA when it cannot be told (a name, a function of the program).
.model_key <- function(method, package = "base") {
  if (is.call(method) && identical(method[[1L]], as.name("::")) &&
      length(method) == 3L) {
    return(paste0(as.character(method[[2L]]), "::", as.character(method[[3L]])))
  }
  if (is.name(method)) method <- as.character(method)
  if (!is.character(method) || length(method) != 1L || is.na(method)) {
    return(NA_character_)
  }
  if (grepl("::", method, fixed = TRUE)) return(sub(":::", "::", method, fixed = TRUE))
  if (!is.character(package) || length(package) != 1L || is.na(package)) {
    return(NA_character_)
  }
  pkgs <- if (identical(package, "base")) c("stats", "survival", "base") else package
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) next
    if (method %in% getNamespaceExports(p)) return(paste0(p, "::", method))
  }
  NA_character_
}

# The argument names a model function takes, one vector per function
# (its own, then where its `...` go: .model_dots_to), or NULL when its
# `...` take anything (not judged) or it cannot be found
.model_arg_names <- function(key) {
  f <- .model_fun(key)
  if (is.null(f)) return(NULL)
  own <- names(formals(f))
  if (!"..." %in% own) return(list(own))
  if (!key %in% names(.model_dots_to)) return(NULL)
  to <- lapply(.model_dots_to[[key]], function(k) {
    g <- .model_fun(k)
    if (is.null(g)) NULL else names(formals(g))
  })
  if (any(vapply(to, is.null, NA))) return(NULL)
  c(list(own), to)
}

#' The arguments of a model cardx fits: `method.args`
#'
#' cardx fits some models itself: `ard_survival_survfit()` the Kaplan-Meier
#' estimate (`survival::survfit()`), `ard_regression()`,
#' `ard_emmeans_emmeans()`, `ard_emmeans_contrast()` and `ard_stats_anova()`
#' the model their `method` and `package` name (`"glm"`, `"coxph"` of
#' survival, `"mmrm"` of mmrm ...).  The model's own arguments are written
#' in `method.args = list(...)` of the analysis row's `args`:
#' `method.args = list(conf.type = "log-log")` (the Kaplan-Meier interval
#' SAS's PROC LIFETEST gives), `method.args = list(ties = "breslow")` (a
#' Cox model as SAS's PROC PHREG fits it), `method.args = list(family =
#' binomial)` (a logistic model), `method.args = list(method =
#' "Kenward-Roger")` (MMRM's degrees of freedom).  The same arguments are
#' the model's in a call written in `x`
#' (`ard_survival_survfit_diff(x = survival::survfit(..., conf.type =
#' "log-log"))`).
#'
#' `tfl_ard_spec()` looks for each name in the installed model function
#' (and where its `...` go: survfit()'s survfitKM(), glm()'s glm.control(),
#' coxph()'s coxph.control(), mmrm()'s mmrm_control()) and judges the
#' values this catalog describes by their kind: `conf.type = "loglog"`
#' would stop the program, `tise = "breslow"` too, and `wieghts = W` would
#' be ignored by `lm()`.  `formula` and `data` are cardx's to give.
#'
#' The catalog is `inst/ard/model_args.csv` (`fn`, `arg`, `default`,
#' `kind`, `hint`, `choices`); a company adds rows with
#' `options(tflspec.ard_model_args = <data frame>)`.
#'
#' @param method The model function: `"survfit"`, `"coxph"`, `"glm"`, or
#'   `"pkg::fun"`.
#' @param package The package `method` is in, as cardx's `package =`
#'   (`"base"`: the search path, stats then survival).
#' @return A data frame, one row per argument the installed model function
#'   takes (`formula`, `data`, `x`, `y` and `...` left out; those its `...` reach
#'   after its own): `fn`, `arg`, `default`, `kind`, `hint`, `choices`,
#'   `described` (whether the catalog describes it).  NULL when the
#'   function cannot be found.
#' @examples
#' tfl_ard_model_args("survfit", "survival")
#' tfl_ard_model_args("glm")
#' @export
tfl_ard_model_args <- function(method, package = "base") {
  key <- .model_key(method, package)
  if (is.na(key) || is.null(.model_fun(key))) return(NULL)
  cat_rows <- .ard_catalog_rows("model_args.csv", "tflspec.ard_model_args",
                                c("fn", "arg"))
  cat_rows <- cat_rows[cat_rows$fn == key, , drop = FALSE]
  fs <- c(key, if (key %in% names(.model_dots_to)) .model_dots_to[[key]])
  rows <- list()
  for (k in fs) {
    f <- .model_fun(k)
    if (is.null(f)) next
    fm <- formals(f)
    # (an argument with no default is the empty symbol: not touched as a value)
    no_default <- vapply(fm, is.name, NA) & as.character(fm) == ""
    for (a in setdiff(names(fm), c("formula", "data", "...", "x", "y"))) {
      if (a %in% vapply(rows, `[[`, "", "arg")) next
      required <- no_default[[a]]
      d <- if (required) NULL else fm[[a]]
      dtext <- if (required) NA_character_ else
        paste(deparse(d, width.cutoff = 500L), collapse = " ")
      r <- cat_rows[cat_rows$arg == a, , drop = FALSE]
      kind <- if (nrow(r) && nzchar(r$kind[1L])) r$kind[1L] else
        .ard_arg_kind(d, required)
      ch <- if (nrow(r)) r$choices[1L] else ""
      if (!nzchar(ch) && is.call(d) && identical(d[[1L]], as.name("c")) &&
          all(vapply(as.list(d)[-1L], is.character, NA))) {
        ch <- paste(unlist(as.list(d)[-1L]), collapse = " | ")
      }
      rows[[length(rows) + 1L]] <- data.frame(
        fn = key, arg = a,
        default = if (nrow(r) && nzchar(r$default[1L])) r$default[1L] else dtext,
        kind = kind, hint = if (nrow(r)) r$hint[1L] else "", choices = ch,
        described = nrow(r) > 0L, stringsAsFactors = FALSE)
    }
  }
  # (an argument the catalog adds that the function's formals do not show)
  for (a in setdiff(cat_rows$arg, vapply(rows, `[[`, "", "arg"))) {
    r <- cat_rows[cat_rows$arg == a, , drop = FALSE]
    rows[[length(rows) + 1L]] <- data.frame(
      fn = key, arg = a, default = r$default[1L], kind = r$kind[1L],
      hint = r$hint[1L], choices = r$choices[1L], described = TRUE,
      stringsAsFactors = FALSE)
  }
  if (!length(rows)) return(NULL)
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# What is wrong with the model arguments an analysis writes for `call` (a
# cardx function, `args` its R text): those in `method.args = list(...)`
# and those of a model's call in `x`.  One line a problem.
.model_args_problems <- function(tag, call, args) {
  if (!startsWith(call, "cardx::")) return(character())
  given <- tryCatch(.args_list(args), error = function(e) list())
  if (!length(given)) return(character())
  fn <- sub("^.*::", "", call)
  fm <- .ard_formals(call)
  out <- character()
  ma <- given[["method.args"]]
  if ("method.args" %in% names(fm) && is.call(ma) &&
      identical(ma[[1L]], as.name("list"))) {
    key <- if (identical(call, "cardx::ard_survival_survfit")) {
      "survival::survfit"
    } else {
      m <- given[["method"]]
      p <- given[["package"]]
      p <- if (is.null(p)) {
        d <- fm[["package"]]
        if (is.character(d)) d else "base"
      } else .literal_value(p)
      if (is.null(m) || is.null(p)) NA_character_ else .model_key(m, p)
    }
    if (!is.na(key)) {
      out <- c(out, .model_call_problems(
        tag, key, as.list(ma)[-1L], sprintf("%s(method.args = )", fn),
        cardx_sets = TRUE))
    }
  }
  x <- given[["x"]]
  if (is.call(x) && "x" %in% names(fm)) {
    head <- paste(deparse(x[[1L]]), collapse = "")
    key <- if (grepl("::", head, fixed = TRUE)) sub(":::", "::", head, fixed = TRUE) else
      .model_key(head, "base")
    known <- !is.na(key) &&
      (key %in% names(.model_dots_to) || !is.null(.model_catalog(key)))
    if (known) {
      out <- c(out, .model_call_problems(
        tag, key, as.list(x)[-1L], sprintf("%s(x = %s())", fn, sub("^.*::", "", key)),
        cardx_sets = FALSE))
    }
  }
  out
}

# the catalog's rows of one model function, or NULL
.model_catalog <- function(key) {
  d <- .ard_catalog_rows("model_args.csv", "tflspec.ard_model_args", c("fn", "arg"))
  d <- d[d$fn == key, , drop = FALSE]
  if (nrow(d)) d else NULL
}

# The problems of the arguments `els` (a named list of expressions, as
# written) given to the model function `key`, said as written in `where`
.model_call_problems <- function(tag, key, els, where, cardx_sets = FALSE) {
  nm <- names(els) %||% rep("", length(els))
  els <- els[nzchar(nm)]
  if (!length(els)) return(character())
  out <- character()
  fn <- sub("^.*::", "", key)
  pkg <- sub("::.*$", "", key)
  given <- names(els)
  if (cardx_sets) {
    own <- intersect(given, c("formula", "data"))
    if (length(own)) {
      out <- c(out, sprintf(
        "%s: %s gives %s; cardx gives the model its %s itself -- the program would stop on it",
        tag, where, paste0("`", own, "`", collapse = ", "),
        paste(own, collapse = " and ")))
    }
    given <- setdiff(given, own)
  }
  has <- .model_arg_names(key)
  if (!is.null(has)) {
    bad <- given
    for (fm in has) bad <- .args_not_matched(bad, fm)
    if (length(bad)) {
      ignored <- key %in% .model_dots_ignored
      out <- c(out, sprintf(
        "%s: %s gives %s, which %s %s's %s() does not have (it has: %s); %s",
        tag, where, paste0("`", bad, "`", collapse = ", "), pkg,
        utils::packageVersion(pkg), fn,
        paste(setdiff(unique(unlist(has)), c("...", "formula", "data", "x", "y")),
              collapse = ", "),
        if (ignored) "it would be ignored" else "the program would stop on it"))
    }
  }
  d <- .model_catalog(key)
  if (!is.null(d)) {
    for (a in intersect(given, d$arg)) {
      r <- d[d$arg == a, , drop = FALSE][1L, ]
      p <- .model_value_problem(a, els[[a]], r$kind, r$choices, fn)
      if (!is.null(p)) out <- c(out, sprintf("%s: %s: %s(%s = ) %s", tag, where, fn, a, p))
    }
  }
  out
}

# Why a value does not fit a model argument of this kind, or NULL.  Besides
# the kinds of tfl_ard_args() (logical, number, choice, text): `variable`
# a column of the data not in quotes, or R of the columns (`weights = WTS`,
# `offset = log(EXPDUR)`: "WTS" in quotes would stop the model); `family`
# a family (`binomial`, `binomial(link = "probit")`, `"binomial"`); `list`
# a list (`contrasts = list(TRT01P = "contr.sum")`) or NULL.
.model_value_problem <- function(arg, e, kind, choices = "", fn = "") {
  shown <- paste(deparse(e, width.cutoff = 500L), collapse = " ")
  if (identical(kind, "variable")) {
    if (is.name(e) || is.call(e) || is.null(e)) return(NULL)
    return(sprintf("is a column of the data, not in quotes; got %s", shown))
  }
  if (identical(kind, "family")) {
    if (is.name(e) || is.call(e) || is.character(e)) return(NULL)
    return(sprintf("is a family: binomial, poisson, binomial(link = \"probit\"); got %s",
                   shown))
  }
  if (identical(kind, "list")) {
    if (is.name(e) || is.call(e) || is.null(e)) return(NULL)
    return(sprintf("is a list: list(...); got %s", shown))
  }
  .arg_value_problem(arg, e, kind, choices, fn)
}
