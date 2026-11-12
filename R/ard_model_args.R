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
  `MASS::glm.nb` = "stats::glm.control",
  `stats::lm` = character(),
  `stats::aov` = "stats::lm",
  `mmrm::mmrm` = c("mmrm::mmrm_control", "mmrm:::h_get_optimizers"),
  # (since 2026-11-12: geeglm()'s `...` go to both geese.control() and the
  # glm() it fits first, so only the names glm.control() has too pass both)
  `geepack::geeglm` = "stats::glm.control")
.model_dots_ignored <- c("stats::lm", "stats::aov")

# A model function whose `...` go to a function that already gets most of
# its arguments from it: the names a row may write there.  MASS::polr()
# passes `...` to optim(), which it gives par, fn, gr, method and hessian
# itself (a second one would stop it): only `control` is left.
.model_dots_names <- list(`MASS::polr` = "control")

# The arguments a model function cannot go without, which cardx does not
# give: geeglm()'s subject column (it stops on "id variable not found").
.model_required <- list(`geepack::geeglm` = "id")

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
  if (key %in% names(.model_dots_names)) return(list(own, .model_dots_names[[key]]))
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
#' "Kenward-Roger")` (MMRM's degrees of freedom), `method.args =
#' list(covariance = cov_struct("toep", visits = "AVISIT", subject =
#' "USUBJID"))` (MMRM's covariance when the formula has none), `method =
#' "glm.nb", package = "MASS"` with `method.args = list(link = log)` (a
#' negative binomial rate model: exacerbations, recurrent events),
#' `method = "survreg", package = "survival"` with `method.args = list(dist
#' = "lognormal")` (a parametric survival model), `method = "geeglm",
#' package = "geepack"` with `method.args = list(family = binomial, id =
#' factor(USUBJID), corstr = "exchangeable")` (a GEE model of a response
#' over visits: its `id` is required, a factor or a number, and the data
#' sorted by it) and `method =
#' "polr", package = "MASS"` with `method.args = list(Hess = TRUE)` (a
#' proportional odds model of an ordinal scale).  The same arguments are
#' the model's in a call written in `x`
#' (`ard_survival_survfit_diff(x = survival::survfit(..., conf.type =
#' "log-log"))`).
#'
#' `tfl_ard_spec()` looks for each name in the installed model function
#' (and where its `...` go: survfit()'s survfitKM(), glm()'s and glm.nb()'s
#' glm.control(), survreg()'s survreg.control(),
#' coxph()'s coxph.control(), mmrm()'s mmrm_control(), geeglm()'s
#' glm.control() and geese.control(), polr()'s optim(control =)) and judges the
#' values this catalog describes by their kind: `conf.type = "loglog"`
#' would stop the program, `tise = "breslow"` too, and `wieghts = W` would
#' be ignored by `lm()`; a covariance type mmrm does not have
#' (`cov_struct("un", ...)`, `~ un(AVISIT | USUBJID)`), the visit
#' column not in quotes, `weights = "WTS"` given to mmrm() (a vector, not a
#' column name), `link = logit` of glm.nb() and `init.theta = 0` too, and
#' so are `corstr = "exch"`, no `id` or `id = USUBJID` (text: one cluster
#' for the whole data) of geeglm() and `method = "logit"` of polr().
#' `formula` and `data` are cardx's to give.
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
#' tfl_ard_model_args("glm.nb", "MASS")
#' tfl_ard_model_args("polr", "MASS")
#' if (requireNamespace("geepack", quietly = TRUE)) {
#'   tfl_ard_model_args("geeglm", "geepack")
#' }
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
    key <- .row_model_key(call, given, fm)
    if (!is.na(key)) {
      out <- c(out, .model_call_problems(
        tag, key, as.list(ma)[-1L], sprintf("%s(method.args = )", fn),
        cardx_sets = TRUE))
    }
  } else if ("method.args" %in% names(fm) && is.null(ma) &&
             !is.null(given[["method"]])) {
    # (no method.args at all: a model that needs an argument cardx does
    # not give stops -- geeglm() without `id`)
    key <- .row_model_key(call, given, fm)
    need <- if (is.na(key)) NULL else .model_required[[key]]
    if (length(need)) {
      out <- c(out, sprintf(
        "%s: %s() fits %s() with no method.args; it needs %s -- the program would stop on it",
        tag, fn, sub("^.*::", "", key),
        paste0("method.args = list(", need, " = factor(USUBJID))", collapse = ", ")))
    }
  }
  x <- given[["x"]]
  if (is.call(x) && "x" %in% names(fm)) {
    head <- paste(deparse(x[[1L]]), collapse = "")
    key <- if (grepl("::", head, fixed = TRUE)) sub(":::", "::", head, fixed = TRUE) else
      .model_key(head, "base")
    known <- !is.na(key) &&
      (key %in% c(names(.model_dots_to), names(.model_dots_names)) ||
         !is.null(.model_catalog(key)))
    if (known) {
      out <- c(out, .model_call_problems(
        tag, key, as.list(x)[-1L], sprintf("%s(x = %s())", fn, sub("^.*::", "", key)),
        cardx_sets = FALSE))
    }
  }
  out
}

# the catalog's rows of one model function, or NULL
# The model function a cardx row with `method.args` fits, as `pkg::fun`:
# survival::survfit for ard_survival_survfit(), else the one `method` /
# `package` name (`given`: the row's `args` as a named list; `fm`: the
# function's formals).  NA when it cannot be told.
.row_model_key <- function(call, given, fm) {
  if (identical(call, "cardx::ard_survival_survfit")) return("survival::survfit")
  m <- given[["method"]]
  p <- given[["package"]]
  p <- if (is.null(p)) {
    d <- fm[["package"]]
    if (is.character(d)) d else "base"
  } else .literal_value(p)
  if (is.null(m) || is.null(p)) NA_character_ else .model_key(m, p)
}

# Since 2026-11-11: the names in `bad`, written in a row's `args`, that are
# the model's arguments -- `conf.type = "log-log"` beside
# ard_survival_survfit()'s own, `family = binomial` beside
# ard_regression()'s -- said as a hint to write them in `method.args =
# list(...)`, where cardx gives them to the model.  "" when the function
# has no `method.args`, the model cannot be told or none of them is its.
.method_args_hint <- function(call, args, bad) {
  if (!length(bad)) return("")
  fm <- .ard_formals(call)
  if (is.null(fm) || !"method.args" %in% names(fm)) return("")
  given <- tryCatch(.args_list(args), error = function(e) list())
  key <- .row_model_key(call, given, fm)
  if (is.na(key)) return("")
  has <- unique(c(unlist(.model_arg_names(key)), .model_catalog(key)$arg))
  hit <- setdiff(intersect(bad, has), c("...", "formula", "data"))
  if (!length(hit)) return("")
  sprintf(" -- %s %s %s()'s: write %s in `method.args = list(%s)`",
          paste0("`", hit, "`", collapse = ", "),
          if (length(hit) > 1L) "are" else "is", key,
          if (length(hit) > 1L) "them" else "it",
          paste0(hit, " = ...", collapse = ", "))
}

# geeglm() finds its clusters where `as.numeric(id)` changes; ADaM's subject
# columns (USUBJID, SUBJID) are text, which gives NA there, so the whole
# data is one cluster: the robust standard errors and p-values are wrong
# with only a warning, which cardx keeps in the ARD (geepack 1.3; seen with
# a p-value of 0).  `id = factor(USUBJID)` or a numeric subject column is
# what it needs.
.model_id_text_problem <- function(tag, key, els, where) {
  if (!identical(key, "geepack::geeglm")) return(character())
  id <- els[["id"]]
  if (!is.name(id) || !grepl("SUBJID$", as.character(id))) return(character())
  sprintf(paste(
    "%s: %s gives `id = %s`, a text column: geeglm() reads its clusters from",
    "as.numeric(id), so the whole data would be one cluster and the standard",
    "errors wrong -- write `id = factor(%s)`"),
    tag, where, as.character(id), as.character(id))
}

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
  need <- setdiff(.model_required[[key]], given)
  if (length(need)) {
    out <- c(out, sprintf(
      "%s: %s gives no %s, which %s() needs (%s) -- the program would stop on it",
      tag, where, paste0("`", need, "`", collapse = ", "), fn,
      paste0(need, " = factor(USUBJID)", collapse = ", ")))
  }
  out <- c(out, .model_id_text_problem(tag, key, els, where))
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
  if (identical(kind, "link")) return(.model_link_problem(e, choices, shown))
  if (identical(kind, "vector")) {
    # (a name or a call is the program's vector: d$WTS, rep(1, nrow(d)))
    v <- .literal_value(e)
    if (is.name(e) || is.null(e) || (is.call(e) && is.null(v)) ||
        (is.numeric(v) && length(v) > 1L)) return(NULL)
    return(sprintf(paste("is a numeric vector as long as the data (data$WTS), not",
                         "a column name: %s() does not look a column up; got %s"),
                   fn, shown))
  }
  if (identical(kind, "covariance")) return(.model_covariance_problem(e, shown))
  if (identical(kind, "number") && arg %in% names(.model_number_floor)) {
    v <- .literal_value(e)
    lo <- .model_number_floor[[arg]]
    if (is.numeric(v) && !anyNA(v) && any(if (lo[2L] > 0) v <= lo[1L] else v < lo[1L])) {
      return(sprintf("is a number %s %s; got %s",
                     if (lo[2L] > 0) "above" else "at least", lo[1L], shown))
    }
  }
  .arg_value_problem(arg, e, kind, choices, fn)
}

# The model numbers with a floor: c(floor, 1 when the floor itself is not
# allowed).  glm.nb()'s theta starts above 0; survreg()'s fixed scale is 0
# (estimated) or more; geeglm()'s fixed scale and a convergence tolerance
# are above 0; the iterations are 1 or more.
.model_number_floor <- list(init.theta = c(0, 1), scale = c(0, 0),
                            scale.value = c(0, 1), epsilon = c(0, 1),
                            maxit = c(1, 0), maxiter = c(1, 0),
                            iter.max = c(1, 0), outer.max = c(1, 0))

# A link written as glm.nb() takes it: a name (log) or text ("log"), one of
# the choices; a call (power(1/3)) is the program's
.model_link_problem <- function(e, choices, shown) {
  ch <- .split_bar(choices)
  v <- if (is.name(e)) as.character(e) else .literal_value(e)
  if (is.call(e) && is.null(v)) return(NULL)
  if (is.character(v) && length(v) == 1L && v %in% ch) return(NULL)
  sprintf("is one of %s (not in quotes: link = log); got %s",
          paste(ch, collapse = ", "), shown)
}

# A covariance structure as mmrm() takes it in `covariance =`: a
# cov_struct() call (its type one of mmrm's cov_types(), `visits` and
# `subject` the columns in quotes) or a formula (~ us(AVISIT | USUBJID)).
# A name is the program's object.
.model_covariance_problem <- function(e, shown) {
  if (is.name(e) || is.null(e)) return(NULL)
  types <- if (requireNamespace("mmrm", quietly = TRUE)) {
    tryCatch(unique(c(mmrm::cov_types(), mmrm::cov_types(c("abbr", "habbr")))),
             error = function(err) NULL)
  }
  want <- paste("is cov_struct(\"us\", visits = \"AVISIT\", subject = \"USUBJID\")",
                "or ~ us(AVISIT | USUBJID)")
  head <- if (is.call(e)) paste(deparse(e[[1L]]), collapse = "") else ""
  if (identical(head, "~")) {
    rhs <- e[[length(e)]]
    f <- if (is.call(rhs)) paste(deparse(rhs[[1L]]), collapse = "") else ""
    ok_shape <- is.call(rhs) && length(rhs) == 2L && is.call(rhs[[2L]]) &&
      identical(rhs[[2L]][[1L]], as.name("|"))
    if (!ok_shape) return(sprintf("%s; got %s", want, shown))
    if (!is.null(types) && !f %in% types) {
      return(sprintf("%s: %s is not one of mmrm %s's cov_types() (%s); got %s",
                     want, f, utils::packageVersion("mmrm"),
                     paste(mmrm::cov_types("abbr"), collapse = ", "), shown))
    }
    return(NULL)
  }
  if (!head %in% c("cov_struct", "mmrm::cov_struct")) {
    return(sprintf("%s; got %s", want, shown))
  }
  a <- as.list(e)[-1L]
  nm <- names(a) %||% rep("", length(a))
  pos <- c("type", "visits", "subject")
  for (i in which(!nzchar(nm))) {
    nm[i] <- setdiff(pos, nm)[1L]
  }
  names(a) <- nm
  out <- character()
  ty <- .literal_value(a[["type"]])
  if (is.null(a[["type"]])) {
    out <- c(out, "its type (\"us\", \"toep\", \"ar1\", \"cs\" ...) is not given")
  } else if (is.character(ty) && !is.null(types) && !all(ty %in% types)) {
    out <- c(out, sprintf("type \"%s\" is not one of mmrm %s's cov_types() (%s)",
                          ty[1L], utils::packageVersion("mmrm"),
                          paste(mmrm::cov_types("abbr"), collapse = ", ")))
  }
  for (k in c("visits", "subject")) {
    if (is.null(a[[k]])) {
      out <- c(out, sprintf("`%s` (the column, in quotes) is not given", k))
    } else if (is.name(a[[k]])) {
      out <- c(out, sprintf("`%s` is a column in quotes (\"%s\"), not %s", k,
                            as.character(a[[k]]), as.character(a[[k]])))
    }
  }
  if (!length(out)) return(NULL)
  sprintf("%s: %s; got %s", want, paste(out, collapse = "; "), shown)
}
