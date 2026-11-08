# ============================================================================
#  The arguments an analysis row writes, checked against the installed
#  cards / cardx: their names (`args`, the `post` steps, a `custom` row's
#  cards / cardx calls, and the arguments a function passes on through
#  `...` to the one doing the work) and their values, by the kind the
#  catalog gives them (tfl_ard_args()).  tfl_ard_spec() says each as a
#  warning: what is wrong depends on the installed packages.
# ============================================================================

# The arguments in `args` the installed function does not have, one line
# per analysis: "T1 / AGE: cards 0.9.0's ard_summary() has no argument
# `nope` (it has: by, variables, ...)".  A keyword's function is the
# catalog's; `custom`, a subject flag and a study's own function are not
# looked at.  The names tflspec takes for itself (.overall, .missing) and
# the column arguments are known.  A function with `...` is judged only
# when its own code checks them (rlang's check_dots_empty() /
# check_dots_used(), as cards' functions do): one that passes them on
# (cardx::ard_car_anova(type = ) to car::Anova()) takes what it does not
# name, and the name is looked for in the function it reaches
# (.passed_args_unknown()).  Then the values written as they are (.ard_arg_value_problems()),
# the arguments of the model cardx fits (`method.args = list(...)`, a
# model's call in `x`: .model_args_problems()) and the arguments of the
# `post` steps (.post_args_unknown()).
.ard_args_unknown <- function(x) {
  a <- x$analyses
  keys <- tfl_ard_methods()
  out <- character()
  for (i in which(!is.na(a$args))) {
    m <- a$method[i]
    if (identical(m, "custom")) next
    k <- .method_key(m, keys)
    call <- if (is.na(k)) m else keys$call[k]
    if (startsWith(call, "(") || !grepl("::", call, fixed = TRUE)) next
    fm <- .ard_formals(call)
    if (is.null(fm)) next
    pkg <- sub("::.*$", "", call)
    tag <- paste(a$output_id[i], a$analysis_id[i], sep = " / ")
    out <- c(out, .ard_arg_value_problems(tag, call, a$args[i]),
             .model_args_problems(tag, call, a$args[i]))
    given <- tryCatch(.args_given(a$args[i]), error = function(e) character())
    known <- c(names(fm), ".overall", ".missing", "by", "variables", "strata",
               "denominator", "statistic")
    if ("..." %in% names(fm) && !.dots_checked(call)) {
      # (a name R matches by its beginning is the function's own argument)
      out <- c(out, .passed_args_unknown(
        tag, call, .args_not_matched(setdiff(given, known), names(fm)),
        a$args[i]))
      next
    }
    bad <- .args_not_matched(setdiff(given, known), names(fm))
    if (!length(bad)) next
    has <- setdiff(names(fm), c("...", if (isTRUE(attr(fm, "data_first"))) names(fm)[1L]))
    out <- c(out, sprintf(
      "%s: %s %s's %s() has no argument %s (it has: %s); the program would stop on it",
      tag, pkg, utils::packageVersion(pkg),
      sub("^.*::", "", call), paste0("`", bad, "`", collapse = ", "),
      paste(has, collapse = ", ")))
  }
  c(out, .post_args_unknown(a))
}

# What is wrong with the values `args` writes, by the argument's kind
# (tfl_ard_args()): `overall = "yes"` (TRUE or FALSE), `conf.level = 95`
# (between 0 and 1), `method = "wilsoncc"` (not one of the function's
# choices), `method.args = 0.9` (a list), `formula = 1` (a formula).  A
# value that is a name (an object of the program: a code list,
# `population`) is the program's to judge; so is a function not of cards
# or cardx.
.ard_arg_value_problems <- function(tag, call, args) {
  # cards' and cardx's: the catalog's kinds are written for them
  if (!sub("::.*$", "", call) %in% c("cards", "cardx")) return(character())
  given <- tryCatch(.args_list(args), error = function(e) list())
  nm <- names(given) %||% rep("", length(given))
  given <- given[nzchar(nm)]
  if (!length(given)) return(character())
  d <- tryCatch(tfl_ard_args(call), error = function(e) NULL)
  if (is.null(d)) return(character())
  fn <- sub("^.*::", "", call)
  out <- character()
  for (arg in intersect(names(given), d$arg)) {
    r <- d[d$arg == arg, ]
    p <- .arg_value_problem(arg, given[[arg]], r$kind, r$choices, fn)
    if (!is.null(p)) out <- c(out, sprintf("%s: %s(%s = ) %s", tag, fn, arg, p))
  }
  out
}

# A value written as it is -- TRUE, 0.9, "wilson", c(0.25, 0.5), -1 -- as
# that value; NULL for a name or a call of anything else
.literal_value <- function(e) {
  if (is.atomic(e) && length(e) == 1L) return(e)
  if (is.call(e) && identical(e[[1L]], as.name("-")) && length(e) == 2L &&
      is.numeric(e[[2L]])) return(-e[[2L]])
  if (is.call(e) && identical(e[[1L]], as.name("c")) && length(e) > 1L) {
    v <- lapply(as.list(e)[-1L], .literal_value)
    if (any(vapply(v, is.null, NA))) return(NULL)
    return(unlist(v))
  }
  NULL
}

# What an argument of kind `code` (or `levels`) holds, when cards / cardx
# take one shape of R only: a list or formula of cards' selectors
# (`fmt_fun = AGE ~ list(mean = 1L)`, `value = list(AEREL = "Y")`), a list
# (`method.args = list(var.equal = TRUE)`), names (`stats_to_remove`), a
# function (`tidy_fun`, `.f`), labels by name (`term_labels = c(SEXM =
# "Male")`: a list or a named vector), a model (`x = lm(...)`), an expression of
# the ARD's columns (a `post` step's `filter`, `expr`, `rows`).  An
# argument of one function only is `function$arg`: the stratum weights
# (numbers between 0 and 1 that add up to 1), the levels a pairwise
# analysis compares (a vector of values), the format of one statistic
# (cards::alias_as_fmt_fun(): a whole number of decimals, "xx.x" or a
# function), the value a NULL statistic gets (one value).  NA: any R.
.ard_code_shapes <- c(
  fmt_fun = "selectors", stat_label = "selectors", value = "selectors",
  label = "selectors", abnormal = "list", formulas = "list",
  method.args = "list", stats_to_remove = "character", tidy_fun = "function",
  emmeans_args = "list", variable_labels = "selectors", term_labels = "labels",
  .f = "function", x = "call", expr = "expression", filter = "expression",
  rows = "expression", by_level = "list", levels = "list", sort = "sort",
  `ard_categorical_ci$weights` = "weights", `ard_pairwise$include` = "values",
  `add_calculated_row$fmt_fun` = "fmt", `update_ard_fmt_fun$fmt_fun` = "fmt",
  `replace_null_statistic$value` = "scalar")

.ard_code_shape <- function(fn, arg) {
  key <- paste0(fn, "$", arg)
  if (key %in% names(.ard_code_shapes)) return(unname(.ard_code_shapes[key]))
  if (arg %in% names(.ard_code_shapes)) unname(.ard_code_shapes[arg]) else NA_character_
}

# Why the value (an expression, as written) does not fit an argument of
# this kind, or NULL
.arg_value_problem <- function(arg, e, kind, choices = "", fn = "") {
  if (is.name(e) || is.null(e)) return(NULL)
  v <- .literal_value(e)
  if (!is.null(v) && anyNA(v)) return(NULL)
  shown <- paste(deparse(e, width.cutoff = 500L), collapse = " ")
  is_fn <- function(e) is.call(e) && (identical(e[[1L]], as.name("function")) ||
    identical(e[[1L]], as.name("::")) || identical(e[[1L]], as.name("~")))
  is_call_of <- function(e, f) is.call(e) && identical(e[[1L]], as.name(f))
  if (kind %in% c("code", "levels")) {
    shape <- if (identical(kind, "levels")) "selectors" else
      .ard_code_shape(fn, arg)
    if (is.na(shape)) return(NULL)
    # (a call that makes the value -- rep(0.25, 4L), seq_len(3),
    # stats::setNames(list(2L), "mean") -- is the program's, as a name is;
    # a literal list(...) or formula is not "made")
    made <- is.call(e) && is.null(v) &&
      !is_call_of(e, "list") && !is_call_of(e, "~")
    ok <- switch(shape,
      weights = made || (is.numeric(v) && all(v >= 0 & v <= 1) &&
                           abs(sum(v) - 1) < 1e-8),
      values = made || (!is.null(v) && (is.character(v) || is.numeric(v))),
      fmt = is_fn(e) || (made && !is_call_of(e, "c")) ||
        (is.numeric(v) && length(v) == 1L && v >= 0 && v == round(v)) ||
        (is.character(v) && length(v) == 1L && .fmt_string_ok(v)),
      scalar = made || (!is.null(v) && length(v) == 1L),
      selectors = made || is_call_of(e, "list") || is_call_of(e, "~"),
      list = made || is_call_of(e, "list"),
      labels = made || is_call_of(e, "list") ||
        (is.character(v) && !is.null(names(v)) && all(nzchar(names(v)))),
      character = made || is.character(v),
      `function` = made || is_fn(e),
      call = is.call(e) && is.null(v),
      expression = is.call(e) || is.logical(v),
      sort = made || is_call_of(e, "list") || is_call_of(e, "~") ||
        (is.character(v) && length(v) == 1L && v %in% .split_bar(choices)))
    if (ok) return(NULL)
    want <- switch(shape,
      selectors = "a list (list(mean = 1L)) or a formula (AGE ~ list(mean = 1L))",
      list = "a list: list(...)",
      labels = "labels by name, a list or a named vector: c(SEXM = \"Male\")",
      character = "names in quotes: c(\"term\", \"label\")",
      `function` = "a function: pkg::fun, function(x) ... or ~ ...",
      call = "R that makes the object: lm(AVAL ~ TRT01P, data = data)",
      expression = "an expression of the ARD's statistics, not in quotes: n >= 2",
      sort = "\"descending\", \"alphanumeric\", or a list of them by variable",
      weights = "numbers between 0 and 1 that add up to 1, one a stratum: c(0.5, 0.3, 0.2)",
      values = "the levels compared, as a vector: c(\"Placebo\", \"Drug A\")",
      fmt = "a whole number of decimals (1L), a format (\"xx.x\", \"xx.x%\") or a function (cards::label_round(1))",
      scalar = "one value: NA, 0 or \"NE\"")
    return(sprintf("is %s; got %s", want, shown))
  }
  # a call (a formula, paste0(...), an object's part): the program's
  if (is.null(v)) return(NULL)
  if (identical(kind, "logical") && !(is.logical(v) && length(v) == 1L)) {
    return(sprintf("is TRUE or FALSE; got %s", shown))
  }
  if (identical(kind, "number")) {
    if (!is.numeric(v)) return(sprintf("is a number; got %s", shown))
    if (arg %in% c("conf.level", "conf.int") && any(v <= 0 | v >= 1)) {
      return(sprintf("is between 0 and 1 (0.95 for 95%%); got %s", shown))
    }
  }
  if (identical(kind, "choice") && nzchar(choices %||% "")) {
    ch <- .split_bar(choices)
    # a number among the choices as a number (car::Anova(type = 3))
    if (!(is.character(v) || is.numeric(v)) || !all(as.character(v) %in% ch)) {
      return(sprintf("is one of %s; got %s",
                     paste0("\"", ch, "\"", collapse = ", "), shown))
    }
  }
  if (kind %in% c("column", "columns") && !is.character(v)) {
    return(sprintf("is a column of the data (USUBJID); got %s", shown))
  }
  if (identical(kind, "text") && !is.character(v)) {
    return(sprintf("is text in quotes; got %s", shown))
  }
  if (identical(kind, "formula")) {
    if (!is.character(v)) {
      return(sprintf("is a formula (AVAL ~ TRT01P) or R text of one; got %s", shown))
    }
    bad <- tryCatch({
      str2lang(v)
      NULL
    }, error = function(err) conditionMessage(err))
    if (!is.null(bad)) return(sprintf("does not read as R (%s)", gsub("\\s+", " ", bad)))
  }
  NULL
}

# A format string cards takes for a statistic (alias_as_fmt_fun()): an x,
# then x's, at most one point and an optional % at the end ("xx",
# "xx.x", "xx.xx%")
.fmt_string_ok <- function(x) {
  grepl("^x[x.%]+$", x) && lengths(regmatches(x, gregexpr(".", x, fixed = TRUE))) <= 1L &&
    (!grepl("%", x, fixed = TRUE) || grepl("^[^%]*%$", x))
}

# The arguments of a `post` step its function does not have (a cards
# helper: add_calculated_row(expr =, stat_name =) ...), and the values that
# do not fit theirs, one line per step.  A step written by the plain name of
# a function cards or cardx exports is judged as if written with its
# package, as a `custom` row's code is (the step's outermost call only).  A
# step of a function with `...` its code does not check, or of any other
# name (a study's own function), is not judged.
.post_args_unknown <- function(a) {
  out <- character()
  post <- a$post %||% rep(NA_character_, nrow(a))
  exports <- if (any(!is.na(post))) .ard_exports() else character()
  for (i in which(!is.na(post))) {
    tag <- paste(a$output_id[i], a$analysis_id[i], sep = " / ")
    for (st in .split_post(post[i])) {
      e <- tryCatch(str2lang(st), error = function(e) NULL)
      if (!is.call(e)) next
      if (is.name(e[[1L]]) && as.character(e[[1L]]) %in% names(exports)) {
        e[[1L]] <- call("::", as.name(exports[[as.character(e[[1L]])]]), e[[1L]])
      }
      out <- c(out, .call_args_problems(tag, e, "post"))
    }
  }
  c(out, .custom_args_unknown(a))
}

# The same for the calls of cards' and cardx's functions in a `custom`
# row's code (`cards::ard_tabulate(data, variables = SEX, denominatr =
# population)`, `cardx::ard_categorical_ci(data, ..., method = "wilsn")`):
# their arguments' names against the function's (its data.frame method's,
# as `args`), their values by the catalog's kinds.  A call written with its
# package (cards:: / cardx::), or -- since 2026-11-05 -- by the plain name of
# a function cards or cardx exports (`ard_tabulate(...)` after
# `library(cards)`) when the code does not bind that name itself; the rest
# of the code is the program's.
.custom_args_unknown <- function(a) {
  out <- character()
  code <- a$code %||% rep(NA_character_, nrow(a))
  for (i in which(!is.na(code) & a$method %in% "custom")) {
    tag <- paste(a$output_id[i], a$analysis_id[i], sep = " / ")
    ex <- tryCatch(parse(text = code[i], keep.source = FALSE),
                   error = function(e) NULL)
    for (e in .ard_code_calls(ex)) out <- c(out, .call_args_problems(tag, e, "code"))
  }
  unique(out)
}

# The calls of cards' and cardx's functions (pkg::fun(...)) in parsed code,
# outermost first, those inside their arguments too.  A call by a plain
# name cards or cardx exports (cards' first) is one of them, written with
# its package, unless the code binds that name (`ard_tabulate <- ...`,
# a function's argument of that name).
.ard_code_calls <- function(ex) {
  found <- list()
  own <- .code_bound_names(ex)
  exports <- if (length(ex)) .ard_exports() else character()
  walk <- function(e) {
    if (!is.call(e)) return(invisible())
    head <- e[[1L]]
    pkg <- if (is.name(head) && !as.character(head) %in% own)
      unname(exports[as.character(head)]) else NA_character_
    if (is.call(head) && identical(head[[1L]], as.name("::")) &&
        as.character(head[[2L]]) %in% c("cards", "cardx")) {
      found[[length(found) + 1L]] <<- e
    } else if (!is.na(pkg)) {
      q <- e
      q[[1L]] <- call("::", as.name(pkg), head)
      found[[length(found) + 1L]] <<- q
    } else if (is.call(head)) {
      walk(head)
    }
    for (k in seq_along(e)[-1L]) if (is.call(e[[k]])) walk(e[[k]])
    invisible()
  }
  for (k in seq_along(ex)) walk(ex[[k]])
  found
}

# The functions cards and cardx define and export (not those they re-export
# from tidyselect / dplyr), as their package by name: cards' first.  Empty
# when neither is installed.
.ard_exports <- function() {
  out <- character()
  for (pkg in c("cards", "cardx")) {
    if (!requireNamespace(pkg, quietly = TRUE)) next
    ns <- asNamespace(pkg)
    nm <- setdiff(getNamespaceExports(pkg), names(out))
    own <- vapply(nm, function(n) {
      f <- get0(n, envir = ns, inherits = FALSE)
      is.function(f) && identical(environment(f), ns)
    }, NA)
    out <- c(out, stats::setNames(rep(pkg, sum(own)), nm[own]))
  }
  out
}

# The names parsed code binds: assigned (`<-`, `=`, `->`, `assign("x")`),
# a function's arguments, a for loop's variable
.code_bound_names <- function(ex) {
  out <- character()
  walk <- function(e) {
    if (!is.call(e)) return(invisible())
    head <- e[[1L]]
    if (is.name(head)) {
      h <- as.character(head)
      if (h %in% c("<-", "=", "<<-") && is.name(e[[2L]])) {
        out <<- c(out, as.character(e[[2L]]))
      } else if (h %in% c("->", "->>") && is.name(e[[3L]])) {
        out <<- c(out, as.character(e[[3L]]))
      } else if (h == "assign" && length(e) > 1L && is.character(e[[2L]])) {
        out <<- c(out, e[[2L]])
      } else if (h == "function") {
        out <<- c(out, names(e[[2L]]))
      } else if (h == "for" && is.name(e[[2L]])) {
        out <<- c(out, as.character(e[[2L]]))
      }
    }
    for (k in seq_along(e)) if (is.call(e[[k]])) walk(e[[k]])
    invisible()
  }
  for (k in seq_along(ex)) walk(ex[[k]])
  unique(out)
}

# What is wrong with one call of a cards / cardx function, `pkg::fun(...)`
# (a plain name is made `pkg::fun` before, by the callers): the arguments it
# does not have (`where`: the column it is written in, post or code) and
# the values that do not fit theirs.  An `ard_*()`
# generic is judged by its data.frame method, as `args` is; a function
# with `...` its code does not check takes any name.
.call_args_problems <- function(tag, e, where) {
  fn <- paste(deparse(e[[1L]]), collapse = "")
  if (!grepl("^[A-Za-z.][A-Za-z0-9.]*::[A-Za-z._][A-Za-z0-9._]*$", fn)) return(character())
  f <- .ard_fun(fn)
  if (is.null(f)) return(character())
  # (a custom row's ard_*() takes the data: its data.frame method's)
  fm <- names(if (identical(where, "code")) .ard_formals(fn) else formals(f))
  given <- names(as.list(e)[-1L]) %||% character()
  given <- given[nzchar(given)]
  args <- paste(vapply(given, function(g)
    paste0(.r_arg_name(g), " = ", paste(deparse(e[[g]], width.cutoff = 500L),
                                        collapse = " ")), ""),
    collapse = ", ")
  out <- if (length(given)) {
    c(.ard_arg_value_problems(tag, fn, args), .model_args_problems(tag, fn, args))
  } else character()
  if ("..." %in% fm && !.dots_checked(fn)) return(out)
  bad <- .args_not_matched(given, fm)
  if (length(bad)) out <- c(out, sprintf(
    "%s: `%s` %s() has no argument %s (it has: %s); the program would stop on it",
    tag, where, sub("^.*::", "", fn), paste0("`", bad, "`", collapse = ", "),
    paste(setdiff(fm, c("...", fm[1L])), collapse = ", ")))
  out
}

# Of the names `given`, those that match no argument in `fm` (a function's
# argument names, in their order) the way R matches them: exactly, or by
# the beginning of one argument before the `...` (`denom` is `denominator`).
# A beginning that fits two arguments is not a match: R stops on it.
.args_not_matched <- function(given, fm) {
  before <- fm[seq_len(match("...", fm, nomatch = length(fm) + 1L) - 1L)]
  hit <- vapply(given, function(g)
    g %in% fm || sum(startsWith(before, g)) == 1L, NA)
  given[!hit]
}

# Does the function check its `...` (rlang::check_dots_empty() /
# check_dots_used())?  Then an argument it does not name stops it.
.dots_checked <- function(call) {
  f <- .ard_fun(call)
  if (is.null(f)) return(FALSE)
  m <- tryCatch(utils::getS3method(sub("^.*::", "", call), "data.frame",
                                   optional = TRUE, envir = environment(f)),
                error = function(e) NULL)
  code <- paste(deparse(body(if (is.function(m)) m else f)), collapse = "\n")
  grepl("check_dots_(empty|used)", code)
}


# The names written in `args` that a function passes on through `...` and
# the function it reaches does not have either:
# `cardx::ard_stats_t_test(var.eqaul = TRUE)` reaches stats::t.test(),
# whose default method ignores a name it does not have, so the mistake
# would go unseen.  The functions reached are the catalog's (`passed_to`
# of inst/ard/passed_args.csv); a name the catalog describes is known.
# Only the functions that take nothing else are judged: those of stats and
# survival (their tests' methods name every argument they use; a `...`
# there is ignored or stops), car::Anova()'s method of the model
# (.anova_args_unknown()) and, since 2026-11-08, broom.helpers'
# tidy_plus_plus() with broom's tidier of the model (.tidy_args_unknown()).
# effectsize and smd pass `...` on again, so a name not theirs may still be
# right: not judged.
.passed_args_unknown <- function(tag, call, given, args = NA_character_) {
  if (!length(given)) return(character())
  d <- .ard_catalog_rows("passed_args.csv", "tflspec.ard_passed_args",
                         c("call", "arg"))
  d <- d[d$call == call, , drop = FALSE]
  to <- unique(d$passed_to)
  if (identical(to, "car::Anova")) {
    return(.anova_args_unknown(tag, call, given, args, d$arg))
  }
  if (identical(to, "broom.helpers::tidy_plus_plus")) {
    return(.tidy_args_unknown(tag, call, given, args, d$arg))
  }
  if (!length(to) || !all(sub("::.*$", "", to) %in% c("stats", "survival"))) {
    return(character())
  }
  has <- lapply(to, .passed_to_formals)
  if (any(vapply(has, is.null, NA))) return(character())
  has <- unlist(has, recursive = FALSE)
  # (a name that begins an argument before a method's `...` is that
  # argument: `var.eq` is `var.equal` of t.test.default())
  bad <- setdiff(given, d$arg)
  for (fm in has) bad <- .args_not_matched(bad, fm)
  if (!length(bad)) return(character())
  pkg <- sub("::.*$", "", call)
  sprintf(paste(
    "%s: %s %s's %s() has no argument %s, and passes it on to %s, which has",
    "none either (it has: %s); it would be ignored or stop the program"),
    tag, pkg, utils::packageVersion(pkg), sub("^.*::", "", call),
    paste0("`", bad, "`", collapse = ", "),
    paste0(to, "()", collapse = " / "),
    paste(setdiff(unique(unlist(has)), c("...", "x", "y", "formula", "data")),
          collapse = ", "))
}

# The arguments a function a cardx function passes `...` on to takes: its
# own and its default and formula methods' (stats::t.test() is a generic:
# t.test.default() names `var.equal`), one vector of names, in their order,
# for each of them.  NULL when it cannot be found.
.passed_to_formals <- function(to) {
  pkg <- sub("::.*$", "", to)
  fn <- sub("^.*::", "", to)
  if (!requireNamespace(pkg, quietly = TRUE)) return(NULL)
  ns <- asNamespace(pkg)
  f <- get0(fn, envir = ns, mode = "function")
  if (is.null(f)) return(NULL)
  ms <- lapply(c("default", "formula"), function(cl) {
    tryCatch(utils::getS3method(fn, cl, optional = TRUE, envir = ns),
             error = function(e) NULL)
  })
  fs <- c(list(f), Filter(is.function, ms))
  lapply(fs, function(g) names(formals(g)))
}

# The same for cardx::ard_car_anova(), which passes `...` to car::Anova():
# since 2026-11-05 a name is looked for in the method of the model that
# `x` fits (`x = lm(...)`: car's Anova.lm(); glm, coxph, aov, mlm ... the
# same), whose `...` takes nothing else -- `tpye = "III"` would be ignored
# and give type II.  A name R matches by its beginning (`typ = ` is
# `type = `) is the argument's.  A model by a name (`x = fit`), or one car has no
# method for, is not judged.
.anova_args_unknown <- function(tag, call, given, args, catalogued) {
  if (!requireNamespace("car", quietly = TRUE)) return(character())
  x <- tryCatch(.args_list(args)[["x"]], error = function(e) NULL)
  if (!is.call(x)) return(character())
  fit <- sub("^.*::", "", paste(deparse(x[[1L]]), collapse = ""))
  m <- tryCatch(utils::getS3method("Anova", fit, optional = TRUE,
                                   envir = asNamespace("car")),
                error = function(e) NULL)
  if (!is.function(m)) return(character())
  fm <- names(formals(m))
  has <- setdiff(unique(c(names(formals(car::Anova)), fm)), "...")
  # (`typ = "III"` is `type =`, as R matches it)
  bad <- .args_not_matched(setdiff(given, c(has, catalogued)), fm)
  if (!length(bad)) return(character())
  sprintf(paste(
    "%s: cardx %s's %s() has no argument %s, and passes it on to car's",
    "Anova.%s(), which has none either (it has: %s); it would be ignored"),
    tag, utils::packageVersion("cardx"), sub("^.*::", "", call),
    paste0("`", bad, "`", collapse = ", "), fit,
    paste(setdiff(has, c("mod", "x")), collapse = ", "))
}

# The same for cardx::ard_regression() / ard_regression_basic(), which
# pass `...` to broom.helpers::tidy_plus_plus(), which passes what it does
# not name to the tidier -- by default broom's tidy() method of the model
# (tidy.coxph(), tidy.glm(), tidy.lm(), tidy.negbin(), tidy.survreg() ...),
# whose `...` is ignored: `exponentaite = TRUE` gave log hazard ratios
# without a word.  Since 2026-11-08 a name is looked for in both.  The
# model is the one `method` / `package` names (`method = "coxph", package =
# "survival"`) or the call `x` writes (`x = survival::coxph(...)`); a
# study's own tidier (`tidy_fun =`), a model by a name (`x = fit`) or one
# broom has no tidy() method for (broom.helpers then tries parameters) is
# not judged.  A name R matches by its beginning (`exponent =`) is
# tidy_plus_plus()'s.
.tidy_args_unknown <- function(tag, call, given, args, catalogued) {
  if (!requireNamespace("broom.helpers", quietly = TRUE) ||
      !requireNamespace("broom", quietly = TRUE)) return(character())
  a <- tryCatch(.args_list(args), error = function(e) list())
  if ("tidy_fun" %in% names(a)) return(character())
  key <- NA_character_
  if (is.call(a[["x"]])) {
    head <- paste(deparse(a[["x"]][[1L]]), collapse = "")
    key <- if (grepl("::", head, fixed = TRUE)) sub(":::", "::", head, fixed = TRUE) else
      .model_key(head, "base")
  } else if (!is.null(a[["method"]])) {
    p <- if (is.null(a[["package"]])) "base" else .literal_value(a[["package"]])
    if (!is.null(p)) key <- .model_key(a[["method"]], p)
  }
  if (is.na(key)) return(character())
  # the class of the model the function makes (glm.nb()'s is "negbin")
  cls <- switch(sub("^.*::", "", key), glm.nb = "negbin", sub("^.*::", "", key))
  pkg <- sub("::.*$", "", key)
  # (a model package registers its own tidy() method: mmrm's tidy.mmrm())
  if (!requireNamespace(pkg, quietly = TRUE)) return(character())
  m <- tryCatch(utils::getS3method("tidy", cls, optional = TRUE,
                                   envir = asNamespace("broom")),
                error = function(e) NULL)
  if (!is.function(m)) return(character())
  tpp <- names(formals(broom.helpers::tidy_plus_plus))
  has <- setdiff(unique(c(tpp, names(formals(m)))), "...")
  bad <- .args_not_matched(setdiff(given, c(has, catalogued)), tpp)
  if (!length(bad)) return(character())
  sprintf(paste(
    "%s: cardx %s's %s() has no argument %s, and passes it on to",
    "broom.helpers::tidy_plus_plus() and broom's tidy.%s(), which have none",
    "either (they have: %s); it would be ignored"),
    tag, utils::packageVersion("cardx"), sub("^.*::", "", call),
    paste0("`", bad, "`", collapse = ", "), cls,
    paste(setdiff(has, c("model", "x", "tidy_fun")), collapse = ", "))
}
