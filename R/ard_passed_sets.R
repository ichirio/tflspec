# ============================================================================
#  The arguments a cardx function passes on through `...`: the names cardx
#  writes itself in that call (.cardx_sets_args()), and the names
#  effectsize's and smd's functions take (.effectsize_args_unknown(),
#  .smd_args_unknown()).  Called from .passed_args_unknown().
# ============================================================================

# The names a cardx function writes itself in its call of `to`, the call
# that takes the row's `...`: `ard_effectsize_cohens_d()` calls
# `effectsize::cohens_d(<formula>, data = , paired = FALSE, ci = conf.level,
# ...)`, `ard_smd_smd()` calls `smd::smd(x = , g = , na.rm = TRUE,
# std.error = , ...)`.  The same name in `args` reaches that call twice,
# and R stops on it ("formal argument matched by multiple actual
# arguments"); cardx keeps the error in the ARD, so the table's cells are
# empty.  Read from the installed cardx's code (its data.frame method's,
# else the function's): a positional argument is named as `to` matches
# it.  character() when the call is not found there.
.cardx_sets_args <- function(call, to) {
  f <- .ard_fun(call)
  if (is.null(f)) return(character())
  fn <- sub("^.*::", "", call)
  m <- tryCatch(utils::getS3method(fn, "data.frame", optional = TRUE,
                                   envir = environment(f)),
                error = function(e) NULL)
  if (is.function(m)) f <- m
  tpkg <- sub("::.*$", "", to)
  tfn <- sub("^.*::", "", to)
  target <- if (requireNamespace(tpkg, quietly = TRUE)) {
    get0(tfn, envir = asNamespace(tpkg), mode = "function")
  }
  dots <- quote(...)
  found <- character()
  walk <- function(e) {
    if (!is.call(e)) return(invisible())
    head <- paste(deparse(e[[1L]]), collapse = "")
    if (head %in% c(to, tfn)) {
      els <- as.list(e)[-1L]
      has_dots <- any(vapply(els, identical, NA, dots))
      if (has_dots) {
        e2 <- e[c(TRUE, !vapply(els, identical, NA, dots))]
        nm <- names(e2)[-1L] %||% rep("", length(e2) - 1L)
        found <<- c(found, nm[nzchar(nm)])
        if (is.function(target)) {
          mc <- tryCatch(match.call(target, e2, expand.dots = FALSE),
                         error = function(err) NULL)
          if (!is.null(mc)) found <<- c(found, names(as.list(mc))[-1L])
        }
      }
    }
    for (i in seq_along(e)) {
      if (!identical(e[[i]], quote(expr = ))) walk(e[[i]])
    }
    invisible()
  }
  walk(body(f))
  setdiff(unique(found), c("", "..."))
}

# A name in `args` the cardx function gives `to` itself, as a line of the
# check: "T1 / AGE: cardx 0.3.4's ard_smd_smd() gives smd::smd() `na.rm`
# itself (na.rm = TRUE) ...".
.cardx_sets_problems <- function(tag, call, given, to) {
  if (!length(given) || !length(to)) return(character())
  sets <- unique(unlist(lapply(to, function(t) .cardx_sets_args(call, t))))
  own <- intersect(given, sets)
  if (!length(own)) return(character())
  sprintf(paste(
    "%s: cardx %s's %s() gives %s %s itself; written in `args` too it",
    "reaches that call twice and the analysis stops (the ARD holds the",
    "error, the table's cells are empty)"),
    tag, utils::packageVersion("cardx"), sub("^.*::", "", call),
    paste0(to, "()", collapse = " / "),
    paste0("`", own, "`", collapse = ", "))
}

# effectsize's cohens_d() / hedges_g(): what they do not name goes to
# model.frame() (`subset`, `na.action`, with a formula) and the rest is
# dropped without a word -- `pooled_SD = FALSE` would leave the pooled SD.
# `var.equal` is cohens_d()'s other name for `pooled_sd`.
.effectsize_args_unknown <- function(tag, call, given, to) {
  fs <- lapply(to, function(t) {
    f <- .ard_fun(t)
    if (is.null(f)) NULL else names(formals(f))
  })
  if (any(vapply(fs, is.null, NA))) return(character())
  has <- unique(c(unlist(fs), "var.equal", "subset", "na.action"))
  bad <- given
  for (fm in fs) bad <- .args_not_matched(bad, fm)
  bad <- setdiff(bad, has)
  if (!length(bad)) return(character())
  sprintf(paste(
    "%s: cardx %s's %s() has no argument %s, and passes it on to %s, which",
    "has none either (it has: %s); it would be ignored"),
    tag, utils::packageVersion("cardx"), sub("^.*::", "", call),
    paste0("`", bad, "`", collapse = ", "), paste0(to, "()", collapse = " / "),
    paste(setdiff(has, c("x", "y", "data", "paired", "ci", "...")),
          collapse = ", "))
}

# smd's smd(): an S4 generic with no `...`; a name it does not have stops
# the call ("unused argument"), which cardx keeps in the ARD.
.smd_args_unknown <- function(tag, call, given) {
  if (!requireNamespace("smd", quietly = TRUE)) return(character())
  fm <- names(formals(smd::smd))
  bad <- .args_not_matched(given, fm)
  if (!length(bad)) return(character())
  sprintf(paste(
    "%s: cardx %s's %s() has no argument %s, and passes it on to smd::smd(),",
    "which has none either (it has: %s); the analysis would stop on it"),
    tag, utils::packageVersion("cardx"), sub("^.*::", "", call),
    paste0("`", bad, "`", collapse = ", "),
    paste(setdiff(fm, c("x", "g", "w", "na.rm", "std.error")), collapse = ", "))
}
