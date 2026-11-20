# ============================================================================
#  The arguments of emmeans::emmeans() a regression row writes in
#  `emmeans_args = list(...)` (since 2026-11-13).  cardx's ard_regression()
#  and ard_regression_basic() pass `emmeans_args` on to
#  broom.helpers::tidy_plus_plus(), which, with `add_pairwise_contrasts =
#  TRUE`, calls `emmeans::emmeans(object = <the model>, specs = <the
#  term>, <emmeans_args>)` and then `pairs()` and `confint(level =
#  conf.level)` (broom.helpers 1.23.0).  emmeans() gives its `...` to
#  ref_grid() and to update(), which keep the names they know (by their
#  beginning) and drop the rest without a word: `wieghts = "proportional"`
#  gave the default equal weights, the LS means SAS's OM option is meant to
#  change.  So the names are looked for in emmeans() and ref_grid(), the
#  options update() takes and the model methods' own arguments of the
#  installed emmeans (`mode` of an ordinal model); the values the catalog
#  describes (inst/ard/model_args.csv, fn `emmeans::emmeans`) are judged.
# ============================================================================

# The names tidy_plus_plus() gives emmeans() itself: a row's value is
# replaced (`object`, `specs`), or another of its arguments says it (the
# intervals' level is `conf.level`, the contrasts' adjustment
# `contrasts_adjust`: pairs() and confint() are called with their own).
.emmeans_set_by_tidy <- c(object = "", specs = "",
                          level = "conf.level", alpha = "conf.level",
                          adjust = "contrasts_adjust")

# The argument names the installed emmeans takes in emmeans(), one vector
# a set: emmeans()'s own, ref_grid()'s (where its `...` go), the options
# update() keeps (matched by their beginning), and the arguments of the
# model methods emmeans has (emm_basis() / recover_data(): `mode`,
# `lmer.df`, `vcov.` ...).  NULL when emmeans is not installed.
.emmeans_arg_names <- function() {
  if (!requireNamespace("emmeans", quietly = TRUE)) return(NULL)
  ns <- asNamespace("emmeans")
  own <- names(formals(emmeans::emmeans))
  rg <- names(formals(emmeans::ref_grid))
  misc <- get0(".valid.misc", envir = ns, inherits = FALSE) %||% character()
  tbl <- get0(".__S3MethodsTable__.", envir = ns, inherits = FALSE)
  meth <- if (is.environment(tbl)) {
    m <- ls(tbl, all.names = TRUE)
    m <- m[grepl("^(emm_basis|recover_data)\\.", m)]
    unique(unlist(lapply(m, function(k) names(formals(get(k, envir = tbl))))))
  } else character()
  list(own = own, ref_grid = rg, options = misc,
       methods = setdiff(meth, c("object", "trms", "xlev", "grid", "...")))
}

# Of the names `given`, those emmeans() keeps none of: not an argument of
# emmeans() or ref_grid() (exactly, or by the beginning of one before the
# `...`, as R matches), not the beginning of one option of update() (as
# emmeans matches them), not a model method's argument
.emmeans_args_not_matched <- function(given, has) {
  bad <- given
  for (fm in has[c("own", "ref_grid")]) bad <- .args_not_matched(bad, fm)
  bad <- bad[!vapply(bad, function(g) sum(startsWith(has$options, g)) == 1L ||
                       g %in% has$options, NA)]
  setdiff(bad, has$methods)
}

# What is wrong with `emmeans_args = list(...)` in a row's `args` for
# `call` (cardx::ard_regression / ard_regression_basic).  One line a
# problem.
.emmeans_args_problems <- function(tag, call, args) {
  if (!call %in% c("cardx::ard_regression", "cardx::ard_regression_basic")) {
    return(character())
  }
  given <- tryCatch(.args_list(args), error = function(e) list())
  ea <- given[["emmeans_args"]]
  if (!is.call(ea) || !identical(ea[[1L]], as.name("list"))) return(character())
  els <- as.list(ea)[-1L]
  nm <- names(els) %||% rep("", length(els))
  els <- els[nzchar(nm)]
  if (!length(els)) return(character())
  fn <- sub("^.*::", "", call)
  where <- sprintf("%s(emmeans_args = )", fn)
  out <- character()
  # (without the pairwise contrasts, tidy_plus_plus() never calls emmeans())
  pc <- .literal_value(given[["add_pairwise_contrasts"]])
  if (is.null(given[["add_pairwise_contrasts"]]) || identical(pc, FALSE)) {
    out <- c(out, sprintf(paste(
      "%s: %s is given but not `add_pairwise_contrasts = TRUE`: tidy_plus_plus()",
      "calls emmeans() only for the pairwise contrasts, so it would be ignored"),
      tag, where))
  }
  set <- intersect(names(els), names(.emmeans_set_by_tidy))
  for (a in set) {
    instead <- .emmeans_set_by_tidy[[a]]
    out <- c(out, if (nzchar(instead)) {
      sprintf(paste("%s: %s gives `%s`; tidy_plus_plus() calls pairs() / confint()",
                    "with its own -- write `%s =` in args instead (it would be ignored)"),
              tag, where, a, instead)
    } else {
      sprintf(paste("%s: %s gives `%s`, which tidy_plus_plus() gives emmeans() itself",
                    "(the model and the term compared) -- it would be replaced"),
              tag, where, a)
    })
  }
  has <- .emmeans_arg_names()
  if (!is.null(has)) {
    bad <- .emmeans_args_not_matched(setdiff(names(els), set), has)
    if (length(bad)) {
      out <- c(out, sprintf(paste(
        "%s: %s gives %s, which emmeans %s's emmeans() and ref_grid() do not",
        "take (they have: %s); emmeans() would drop it without a word"),
        tag, where, paste0("`", bad, "`", collapse = ", "),
        utils::packageVersion("emmeans"),
        paste(setdiff(unique(c(has$own, has$ref_grid)),
                      c("object", "specs", "...", "options", "data")),
              collapse = ", ")))
    }
  }
  d <- .model_catalog("emmeans::emmeans")
  if (!is.null(d)) {
    for (a in intersect(names(els), d$arg)) {
      r <- d[d$arg == a, , drop = FALSE][1L, ]
      e <- els[[a]]
      # (weights as numbers, one a cell of the grid, are emmeans' too)
      if (identical(a, "weights") && is.numeric(.literal_value(e))) next
      p <- .model_value_problem(a, e, r$kind, r$choices, "emmeans")
      if (!is.null(p)) {
        out <- c(out, sprintf("%s: %s: emmeans(%s = ) %s", tag, where, a, p))
      }
    }
  }
  out
}

# ---- the values of cov.reduce, cov.keep, tran and nesting (2026-11-14) ----
# emmeans takes R of its own shapes in these four; the catalog's kinds
# `reducer`, `keep`, `tran` and `nesting` (inst/ard/model_args.csv), judged
# by .model_value_problem().  A name (an object of the program) is the
# program's; a call that makes the value (make.tran(...), my_reducers())
# too.

# cov.reduce: a function (mean, median, function(x) ..., stats::median),
# TRUE / FALSE, a two-sided formula (BASE ~ TRT01P: the covariate predicted
# from the arms), or a list of functions and formulas by covariate.
# ref_grid() stops on anything else ("Invalid 'cov.reduce' argument"):
# `"median"` in quotes, a number, a one-sided formula.
.emmeans_reducer_problem <- function(e, shown) {
  want <- paste("is a function, not in quotes (median, function(x) ...), TRUE / FALSE,",
                "a two-sided formula (BASE ~ TRT01P) or a list of them by covariate")
  one <- function(x, top) {
    if (is.name(x)) return(TRUE)
    if (top && is.logical(x) && length(x) == 1L && !is.na(x)) return(TRUE)
    if (!is.call(x)) return(FALSE)
    fn <- x[[1L]]
    if (identical(fn, as.name("~"))) return(length(x) == 3L)
    if (identical(fn, as.name("function")) || identical(fn, as.name("::")) ||
        identical(fn, as.name(":::"))) return(TRUE)
    if (identical(fn, as.name("list"))) {
      if (!top) return(FALSE)
      els <- as.list(x)[-1L]
      nm <- names(els) %||% rep("", length(els))
      return(length(els) > 0L && all(nzchar(nm)) &&
               all(vapply(els, one, NA, top = FALSE)))
    }
    # (any other call makes the value: the program's)
    is.null(.literal_value(x))
  }
  if (is.null(e) || one(e, TRUE)) return(NULL)
  if (is.call(e) && identical(e[[1L]], as.name("~"))) {
    return(sprintf("is a two-sided formula (BASE ~ TRT01P): emmeans stops on %s", shown))
  }
  if (is.call(e) && identical(e[[1L]], as.name("list"))) {
    return(sprintf(paste("is a list of functions or two-sided formulas named by",
                         "covariate (list(BASE = median)); got %s"), shown))
  }
  sprintf("%s; got %s", want, shown)
}

# cov.keep: covariates kept with all their values, by name, or a number (a
# covariate with that many distinct values or fewer is kept).
.emmeans_keep_problem <- function(e, shown) {
  if (is.name(e) || is.null(e)) return(NULL)
  v <- .literal_value(e)
  if (is.call(e) && is.null(v)) return(NULL)
  if (is.character(v)) return(NULL)
  if (is.numeric(v) && length(v) == 1L && !is.na(v) && v >= 0 && v == round(v)) {
    return(NULL)
  }
  sprintf(paste("is covariates by name in quotes (c(\"VISITNUM\")) or a whole",
                "number (3: a covariate with 3 values or fewer is kept); got %s"), shown)
}

# The transformations emmeans knows by name: the links its .make.link()
# names (the `%in%` vector and the switch()'s cases of the installed
# emmeans).  Any other name is taken as the identity, without a word.
# NULL when they cannot be read (then the catalog's choices are used).
.emmeans_tran_names <- function() {
  if (!requireNamespace("emmeans", quietly = TRUE)) return(NULL)
  f <- get0(".make.link", envir = asNamespace("emmeans"), mode = "function")
  if (is.null(f)) return(NULL)
  out <- character()
  walk <- function(x) {
    if (!is.call(x)) return(invisible())
    if (identical(x[[1L]], as.name("%in%")) && is.call(x[[3L]])) {
      v <- .literal_value(x[[3L]])
      if (is.character(v)) out <<- c(out, v)
    }
    if (identical(x[[1L]], as.name("switch"))) {
      nm <- names(as.list(x))[-c(1L, 2L)]
      out <<- c(out, nm[nzchar(nm)])
    }
    xs <- as.list(x)[-1L]
    for (i in seq_along(xs)) if (is.call(xs[[i]])) walk(xs[[i]])
    invisible()
  }
  walk(body(f))
  if (length(out)) unique(out) else NULL
}

# tran: a name emmeans knows ("log", "logit", "sqrt" ...), or make.tran(...)
# / a list of linkfun, linkinv, mu.eta (R that makes it: the program's).
.emmeans_tran_problem <- function(e, choices, shown) {
  if (is.name(e) || is.null(e)) return(NULL)
  v <- .literal_value(e)
  if (is.call(e) && is.null(v)) return(NULL)
  known <- .emmeans_tran_names() %||% .split_bar(choices)
  if (is.character(v) && length(v) == 1L && v %in% known) return(NULL)
  sprintf(paste("is a transformation emmeans %s knows by name (%s) or make.tran(...);",
                "got %s -- emmeans would take it as no transformation, so",
                "type = \"response\" would not back-transform"),
          if (requireNamespace("emmeans", quietly = TRUE))
            as.character(utils::packageVersion("emmeans")) else "",
          paste0("\"", utils::head(known, 12L), "\"", collapse = ", "), shown)
}

# nesting: "A %in% B" (several separated by commas; B may be (B1 * B2)),
# a list (list(SITEID = "COUNTRY")) or NULL.  A number or TRUE stops
# emmeans; text without %in% names no nesting.
.emmeans_nesting_problem <- function(e, shown) {
  if (is.name(e) || is.null(e)) return(NULL)
  v <- .literal_value(e)
  if (is.call(e) && is.null(v)) return(NULL)
  if (is.character(v) && !anyNA(v)) {
    parts <- trimws(unlist(strsplit(v, ",", fixed = TRUE)))
    if (length(parts) && all(grepl("^\\S.*\\s+%in%\\s+\\S", parts))) return(NULL)
  }
  sprintf(paste("is the factors nested, in quotes: \"SITEID %%in%% COUNTRY\"",
                "(several separated by commas) or list(SITEID = \"COUNTRY\"); got %s"),
          shown)
}
