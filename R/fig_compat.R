# ggplot2 3.5 / 4.0: which version a figure design's script is written for,
# and what differs between the two for the `call` pieces (fig_call.R).
#
# inst/fig/ggplot2_compat.csv lists the differences, one row each:
#   package, fn, arg   the function (and argument); `*` globs (theme_*, scale_*)
#   change             fn_added | fn_renamed | fn_deprecated | arg_added |
#                      arg_renamed | arg_removed | arg_deprecated |
#                      value_deprecated | behaviour
#   from               the ggplot2 version it happened in
#   replacement        the new name (or call) for renamed / deprecated ones
#   level              error (does not work in the target) | warning | info
#
# What each change means for a target (ge = target >= from):
#   *_added       !ge: error in tfl_check_fig_design()
#                  ge and from > 3.5.0: the script gets a version guard
#   *_renamed     written as the target's name (either way), with a comment
#   arg_removed    ge: error (the argument is dropped, not translated)
#   *_deprecated,  ge: advice (tfl_fig_advice()) with a one-click fix
#   value_deprecated
#   behaviour      ge: advice (info)

.fig_gg_targets <- c("3.5", "4.0")

# the lowest ggplot2 the package allows (DESCRIPTION): what needs no guard
.fig_gg_floor <- "3.5.0"

.fig_compat <- function() {
  if (is.null(.fig_registry$compat)) {
    .fig_registry$compat <- utils::read.csv(
      system.file("fig", "ggplot2_compat.csv", package = "tflspec", mustWork = TRUE),
      stringsAsFactors = FALSE, na.strings = "", colClasses = "character",
      encoding = "UTF-8")
  }
  .fig_registry$compat
}

#' ggplot2 3.5 / 4.0 differences for figure designs
#'
#' The table [tfl_fig_design_code()], [tfl_check_fig_design()] and
#' [tfl_fig_advice()] use to write and check a design's `call` pieces for
#' one of the two ggplot2 versions supported, 3.5 and 4.0: functions and
#' arguments added, renamed, removed or deprecated. A renamed one is
#' written under the target version's name; one the target does not have
#' is an error of the check; a deprecated one is advice with a fix.
#'
#' The target version is, in this order: the `ggplot2_version` argument,
#' the design's top-level `ggplot2_version:`, the option
#' `tflspec.ggplot2_version`, the installed ggplot2's (its major.minor).
#'
#' @param ggplot2_version `"3.5"` or `"4.0"`: adds `status`, what each row
#'   means for that version (`ok`, `absent`, `removed`, `renamed`,
#'   `deprecated`, `changed`) -- e.g. for a GUI to grey out what the
#'   version does not have.
#' @return A data frame: `package`, `fn`, `arg`, `change`, `from`,
#'   `replacement`, `level`, `note` (and `status`).
#' @export
#' @examples
#' cp <- tfl_fig_compat("3.5")
#' cp[cp$status == "absent", c("fn", "arg", "from")]
tfl_fig_compat <- function(ggplot2_version = NULL) {
  x <- .fig_compat()
  if (is.null(ggplot2_version)) return(x)
  v <- .fig_norm_version(ggplot2_version)
  ge <- vapply(x$from, function(f) .fig_ver_ge(v, f), logical(1))
  x$status <- ifelse(
    grepl("_added$", x$change), ifelse(ge, "ok", "absent"),
    ifelse(!ge, "ok", c(fn_renamed = "renamed", arg_renamed = "renamed",
                        arg_removed = "removed", fn_deprecated = "deprecated",
                        arg_deprecated = "deprecated", value_deprecated = "deprecated",
                        behaviour = "changed")[x$change]))
  x
}

# "4.0", 4, "4.0.3" -> "4.0"; an error for other than 3.5 / 4.0
.fig_norm_version <- function(x, what = "ggplot2_version") {
  s <- if (is.numeric(x)) formatC(x, format = "f", digits = 1) else trimws(as.character(x)[1L])
  mm <- sub("^(\\d+)\\.(\\d+).*$", "\\1.\\2", s)
  if (grepl("^\\d+$", s)) mm <- paste0(s, ".0")
  if (!mm %in% .fig_gg_targets) {
    stop(sprintf("%s must be one of %s, not '%s'", what,
                 paste(q(.fig_gg_targets), collapse = ", "), s), call. = FALSE)
  }
  mm
}

# target (major.minor) >= a version such as "3.4.0"
.fig_ver_ge <- function(target, from) {
  if (is.na(from) || !nzchar(from)) return(TRUE)
  numeric_version(paste0(target, ".9999")) >= numeric_version(from)
}

.gg_memo <- new.env(parent = emptyenv())
.fig_installed_gg <- function() {
  # (packageVersion() reads the DESCRIPTION file each time)
  v <- .gg_memo$v %||% (.gg_memo$v <- utils::packageVersion("ggplot2"))
  mm <- paste(v$major, v$minor, sep = ".")
  if (mm %in% .fig_gg_targets) mm else if (v >= "4.0.0") "4.0" else "3.5"
}

# the target: list(version, explicit, source)
.fig_target_version <- function(ggplot2_version = NULL, design = NULL) {
  from <- list(argument = ggplot2_version,
               design = if (!is.null(design)) design$ggplot2_version,
               option = getOption("tflspec.ggplot2_version"))
  for (src in names(from)) {
    x <- from[[src]]
    if (!is.null(x) && length(x) && !is.na(x[1L]) && nzchar(as.character(x[1L]))) {
      return(list(version = .fig_norm_version(x, if (src == "argument") "ggplot2_version"
                                                  else if (src == "design") "the design's ggplot2_version"
                                                  else "option tflspec.ggplot2_version"),
                  explicit = TRUE, source = src))
    }
  }
  list(version = .fig_installed_gg(), explicit = FALSE, source = "installed")
}

# glob match: one pattern against many names, or many patterns against one
.fig_glob <- function(pattern, x) {
  if (length(pattern) != 1L) {
    return(vapply(pattern, function(p) any(.fig_glob(p, x)), logical(1), USE.NAMES = FALSE))
  }
  if (is.na(pattern) || !nzchar(pattern)) return(rep(FALSE, length(x)))
  grepl(utils::glob2rx(pattern), x)
}

# replacement "geom_errorbar(orientation = \"y\")" -> list(fn, args)
.fig_parse_repl <- function(repl) {
  e <- tryCatch(str2lang(repl), error = function(e) NULL)
  if (is.call(e)) {
    args <- lapply(as.list(e)[-1L], function(a) if (is.language(a)) tfl_fig_r(deparse(a)) else a)
    return(list(fn = as.character(e[[1L]]), args = args))
  }
  list(fn = repl, args = list())
}

# a deprecated value: legend.position = c(x, y); guide = FALSE
.fig_value_deprecated <- function(arg, val) {
  if (.is_fig_r(val)) return(FALSE)
  switch(arg,
    legend.position = is.numeric(unlist(val)) && length(unlist(val)) == 2L,
    guide = isFALSE(val),
    FALSE)
}

.fig_value_fix <- function(args, arg, repl) {
  if (identical(arg, "legend.position")) {
    args[[repl]] <- unlist(args[[arg]])
    args[[arg]] <- "inside"
  } else if (identical(arg, "guide")) {
    args[[arg]] <- repl
  }
  args
}

# One call spec (and its nested {fn:} calls) against the table for
# `target`. fix = FALSE: renames only (what the script is written with);
# fix = TRUE: every mechanical fix (renames, deprecated / removed arguments,
# deprecated values) -- the advice's one-click fix. Returns the spec
# rewritten, the renames (for the line's comment), the versions the
# written code needs (guard), errors (part, field, problem) and warnings
# (messages, for the advice).
.fig_compat_walk <- function(spec, target, part, fix = FALSE) {
  res <- list(spec = spec, renames = character(), guard = character(),
              features = character(),
              errors = data.frame(part = character(), field = character(),
                                  problem = character(), stringsAsFactors = FALSE),
              warns = character())
  err <- function(f, x) res$errors[nrow(res$errors) + 1L, ] <<- list(part, f, x)
  need <- function(from, what) if (numeric_version(from) > .fig_gg_floor) {
    res$guard <<- c(res$guard, from); res$features <<- c(res$features, what)
  }
  tv <- paste("ggplot2", target)
  rf <- .fig_resolve_fn(spec$fn %||% "", spec$package)
  fn <- rf$fn
  if (!is.null(rf$pkg) && !identical(rf$pkg, "ggplot2")) fn <- ""
  pkg_prefix <- if (grepl("::", spec$fn %||% "", fixed = TRUE)) "ggplot2::" else ""
  cp <- .fig_compat()
  ge <- vapply(cp$from, function(f) .fig_ver_ge(target, f), logical(1))
  if (nzchar(fn)) {
    # the function
    fr <- which(is.na(cp$arg) & cp$package == "ggplot2")
    for (r in fr) {
      old <- cp$fn[r]; new <- cp$replacement[r]; from <- cp$from[r]
      switch(cp$change[r],
        fn_added = if (.fig_glob(old, fn)) {
          if (!ge[r]) err("fn", sprintf("%s: %s() is not available (added in ggplot2 %s)", tv, fn, from))
          else need(from, paste0(fn, "()"))
        },
        fn_renamed = {
          if (ge[r] && identical(fn, old)) {
            res$renames <- c(res$renames, paste(old, "->", new))
            res$warns <- c(res$warns, sprintf("%s: %s() is deprecated; written as %s()", tv, old, new))
            fn <- new
            need(from, paste0(new, "()"))
          } else if (!ge[r] && identical(fn, new)) {
            res$renames <- c(res$renames, paste(new, "->", old))
            res$warns <- c(res$warns, sprintf("%s has no %s(); written as %s()", tv, new, old))
            fn <- old
          }
        },
        fn_deprecated = if (ge[r] && identical(fn, old)) {
          res$warns <- c(res$warns, sprintf("%s: %s() is deprecated; use %s", tv, old, new))
          if (fix) {
            p <- .fig_parse_repl(new)
            fn <- p$fn
            res$spec$args <- c(res$spec$args %||% list(), p$args)
          }
        })
    }
    res$spec$fn <- paste0(pkg_prefix, fn)
    # its arguments
    ar <- which(!is.na(cp$arg) & !is.na(cp$fn) & .fig_glob(cp$fn, fn))
    removed <- character()
    for (r in ar[cp$change[ar] == "arg_removed"]) {
      a <- cp$arg[r]
      if (ge[r] && a %in% names(res$spec$args)) {
        removed <- c(removed, a)
        err(paste0("args$", a), sprintf("%s: %s() drops %s (not translated); use %s",
                                        tv, fn, a, cp$replacement[r]))
        res$warns <- c(res$warns, sprintf("%s: %s() drops %s; use %s", tv, fn, a, cp$replacement[r]))
        if (fix) names(res$spec$args)[names(res$spec$args) == a] <- cp$replacement[r]
      }
    }
    for (r in ar[cp$change[ar] != "arg_removed"]) {
      a <- cp$arg[r]; new <- cp$replacement[r]; from <- cp$from[r]
      nms <- names(res$spec$args) %||% character()
      hit <- nms[.fig_glob(a, nms)]
      switch(cp$change[r],
        arg_added = for (h in hit) {
          if (!ge[r]) err(paste0("args$", h), sprintf("%s: %s(%s =) is not available (added in ggplot2 %s)", tv, fn, h, from))
          else need(from, sprintf("%s(%s =)", fn, h))
        },
        arg_renamed = {
          if (ge[r] && a %in% nms) {
            names(res$spec$args)[nms == a] <- new
            res$renames <- c(res$renames, paste(a, "->", new))
            res$warns <- c(res$warns, sprintf("%s: %s(%s =) is deprecated; written as %s", tv, fn, a, new))
            need(from, sprintf("%s(%s =)", fn, new))
          } else if (!ge[r] && new %in% nms) {
            names(res$spec$args)[nms == new] <- a
            res$renames <- c(res$renames, paste(new, "->", a))
            res$warns <- c(res$warns, sprintf("%s: %s() has no %s; written as %s", tv, fn, new, a))
          }
        },
        arg_deprecated = if (ge[r] && a %in% nms && !a %in% removed) {
          res$warns <- c(res$warns, sprintf("%s: %s(%s =) is deprecated%s", tv, fn, a,
            if (!is.na(new)) paste0("; use ", new) else if (!is.na(cp$note[r])) paste0("; ", cp$note[r]) else ""))
          if (fix && !is.na(new) && make.names(new) == new) names(res$spec$args)[nms == a] <- new
        },
        value_deprecated = if (ge[r] && a %in% nms && .fig_value_deprecated(a, res$spec$args[[a]])) {
          res$warns <- c(res$warns, sprintf("%s: %s(%s =) %s is deprecated; %s", tv, fn, a,
            .fig_arg_code(res$spec$args[[a]]), cp$note[r]))
          if (fix) res$spec$args <- .fig_value_fix(res$spec$args, a, new)
        })
    }
  }
  # nested calls
  sub_walk <- function(x, where) {
    if (is.null(x) || .is_fig_r(x) || !is.list(x)) return(x)
    nms <- names(x)
    if (!is.null(nms) && "fn" %in% nms) {
      s <- .fig_compat_walk(x, target, paste0(part, " > ", x$fn %||% "?"), fix)
      res$renames <<- c(res$renames, s$renames)
      res$guard <<- c(res$guard, s$guard)
      res$features <<- c(res$features, s$features)
      res$errors <<- rbind(res$errors, s$errors)
      res$warns <<- c(res$warns, s$warns)
      return(s$spec)
    }
    for (i in seq_along(x)) {
      xi <- sub_walk(x[[i]], where)
      if (!is.null(xi)) x[[i]] <- xi
    }
    x
  }
  for (f in c("aes", "pos", "args")) {
    if (!is.null(res$spec[[f]])) res$spec[[f]] <- sub_walk(res$spec[[f]], f)
  }
  res
}

# the call pieces of a design: list(sec, i, spec, part)
.fig_call_pieces <- function(design) {
  out <- list()
  for (i in seq_along(design$layers)) {
    l <- design$layers[[i]]
    if (identical(l$layer, "call")) out[[length(out) + 1L]] <- list(
      sec = "layers", i = i, spec = l, part = sprintf("layers[%d] call", i))
  }
  add <- design$plot$add %||% list()
  for (i in seq_along(add)) out[[length(out) + 1L]] <- list(
    sec = "add", i = i, spec = add[[i]], part = sprintf("plot.add[%d]", i))
  comp <- design$compose$add %||% list()
  for (i in seq_along(comp)) out[[length(out) + 1L]] <- list(
    sec = "compose", i = i, spec = comp[[i]][setdiff(names(comp[[i]]), "op")],
    part = sprintf("compose.add[%d]", i))
  out
}

# names the table knows for a function (its args and their replacements,
# and whether the function itself is in it): the installed-version checks
# of fig_call.R leave these to the table, so a name of the other version is
# not reported as unknown.
.fig_compat_names <- function(fn) {
  cp <- .fig_compat()
  cp <- cp[cp$package == "ggplot2", , drop = FALSE]
  a <- cp[!is.na(cp$arg) & .fig_glob(cp$fn, fn), , drop = FALSE]
  f <- cp[is.na(cp$arg) & (.fig_glob(cp$fn, fn) | (!is.na(cp$replacement) & cp$replacement == fn)), , drop = FALSE]
  list(args = unique(c(a$arg, a$replacement[!is.na(a$replacement)])),
       arg_globs = a$arg[grepl("*", a$arg, fixed = TRUE)],
       fn = nrow(f) > 0L)
}

# the one-click fix: a call piece rewritten for the target
.fig_compat_fix_piece <- function(design, sec, i, target) {
  if (sec == "layers") {
    design$layers[[i]] <- .fig_compat_walk(design$layers[[i]], target, "", fix = TRUE)$spec
  } else if (sec == "compose") {
    design$compose$add[[i]] <- .fig_compat_walk(design$compose$add[[i]], target, "", fix = TRUE)$spec
  } else {
    design$plot$add[[i]] <- .fig_compat_walk(design$plot$add[[i]], target, "", fix = TRUE)$spec
  }
  design
}

# the guard line(s) for the script: only when a feature above the floor is written
.fig_guard_code <- function(guard, features) {
  if (!length(guard)) return(character())
  v <- as.character(max(numeric_version(guard)))
  c(sprintf("# needs ggplot2 >= %s: %s", v, paste(unique(features), collapse = ", ")),
    sprintf("stopifnot(utils::packageVersion(\"ggplot2\") >= \"%s\")", v))
}
