# ---- the ARD's analyses as calls: code and run from one list ----------------
#
# An analysis of the ARD definition, as .ard_analysis_call() gives it (the
# call, the steps of its pipe, the Total column), written as the program
# writes it (.ard_call_lines()) and made without its code
# (.ard_call_run()).  The two read the same calls, so the code, run, makes
# what the run makes -- the ARD's half of tfl_content_code() and
# tfl_content_run().

# The analysis's call without `by`: the Total column's, bound under it
.ard_call_no_by <- function(cl) {
  nm <- names(cl$args) %||% rep("", length(cl$args))
  cl$args <- cl$args[nm != "by"]
  cl
}

# the steps after the call, those that are calls (a step that is nothing,
# no format to apply, is left out)
.ard_call_steps <- function(res) {
  Filter(function(s) !is.null(s$fn) || !is.null(s$code), res$steps)
}

# a step as the program writes it: as it was written (a post step), a call,
# or R as it was written
.ard_step_cl <- function(s) {
  if (!is.null(s$text)) return(s$text)
  if (is.null(s$fn)) as.character(s$code) else .call_as_cl(s)
}

# One analysis as code: `name <- data |> call(...) |> steps`, its id (and
# label) a comment above it -- what .ard_analysis_lines() writes, for an
# analysis that is a call (not a custom one's code, not a subject flag).
.ard_call_lines <- function(res, r, data, name) {
  node <- .call_as_cl(res$call)
  comment <- if (!is.na(r$label)) sprintf("# %s: %s", r$analysis_id, r$label) else
    sprintf("# %s", r$analysis_id)
  if (isTRUE(res$overall)) {
    comment <- c(comment, "# (and the Total column: the same call without `by`)")
    node <- .cl("dplyr::bind_rows", list(.piped(node, data),
                                         .piped(.call_as_cl(.ard_call_no_by(res$call)), data)))
  }
  steps <- lapply(.ard_call_steps(res), .ard_step_cl)
  c(comment, .data_pipe(name, node, data, steps))
}

# One analysis made: the call applied with do.call() on the objects the
# program has made (`env`: the analysis data, the analysis sets, the
# code lists, the company standards' formats), then each step on its
# result.  An argument is R as the program writes it, given as that
# expression -- a tidyselect argument (`by = ARM`) selects as it does in
# the program -- and evaluated in `env`.
.ard_call_run <- function(res, env) {
  if (is.null(res$call)) {
    return(.ard_steps_run(eval(str2lang(as.character(res$code)), env), res, env))
  }
  x <- .ard_one_call(res$call, env)
  if (isTRUE(res$overall)) {
    x <- dplyr::bind_rows(x, .ard_one_call(.ard_call_no_by(res$call), env))
  }
  .ard_steps_run(x, res, env)
}

.ard_steps_run <- function(x, res, env) {
  for (s in .ard_call_steps(res)) {
    if (is.null(s$fn)) {
      f <- eval(str2lang(as.character(s$code)), env)
      x <- f(x)
    } else {
      x <- do.call(.ard_call_fun(s$fn, env), c(list(x), .ard_call_args(s$args)), envir = env)
    }
  }
  x
}

.ard_one_call <- function(cl, env) {
  do.call(.ard_call_fun(cl$fn, env), .ard_call_args(cl$args), envir = env)
}

# the arguments as expressions: R text parsed, a nested call as the
# program writes it (`~ cards::continuous_summary_fns(...)`, its
# function by name, so the program's own helpers are found)
.ard_call_args <- function(args) {
  lapply(args, function(a) {
    if (is.list(a) && !inherits(a, "tfl_r") && !is.null(a$fn)) {
      return(str2lang(.lay(.call_as_cl(a))))
    }
    str2lang(as.character(a))
  })
}

# a function by its name as the program writes it: `pkg::fn`, or one the
# program has (its helpers), else the attached packages'
.ard_call_fun <- function(fn, env) {
  if (grepl("::", fn, fixed = TRUE)) {
    return(getExportedValue(sub("::.*$", "", fn), sub("^.*::", "", fn)))
  }
  get(fn, envir = env, mode = "function")
}
