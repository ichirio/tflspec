# ============================================================================
#  A program's own plot functions, %+% and last_plot()
# ----------------------------------------------------------------------------
#  A programmer who draws the same figure for several parameters writes the
#  plot once, in a function of the program (lab_plot <- function(param,
#  ylab) { ... ggplot() ... }), and calls it: p <- lab_plot("ALT", "ALT
#  (U/L)"), lab_plot("ALT") / lab_plot("AST").  tfl_fig_content_from_r()
#  reads such a call as the lines the function runs with its arguments in
#  place of its formals: the data it derives a line of the data part (its
#  local objects named for the call, d_alt), the plot it returns a plot of
#  the program.  `p %+% ast` is p with that data, `last_plot()` the plot
#  made last.
# ============================================================================

# Is `e` a function written out that makes a plot (a call of ggplot(),
# ggsurvfit() ... in its body)?
.fig_prog_is_plot_fn <- function(e) {
  is.call(e) && identical(e[[1L]], as.name("function")) && length(e) >= 3L &&
    any(all.names(e[[3L]]) %in% .fig_p_fns)
}

# Inside aes() / vars(), a column named by a function's argument as the
# column itself: .data[[yvar]] (yvar = "AVAL"), !!sym(yvar), {{ y }} (y =
# AVAL) -- or NULL when `e` is none of them.  `m` the formals' values.
.fig_prog_tidy_col <- function(e, m) {
  val <- function(x) {
    if (is.name(x) && as.character(x) %in% names(m)) x <- m[[as.character(x)]]
    x
  }
  as_col <- function(x) {
    x <- val(x)
    if (is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)) return(as.name(x))
    NULL
  }
  f <- .fig_deparse(e[[1L]])
  # (.data[[yvar]], .data[["AVAL"]])
  if (identical(f, "[[") && length(e) == 3L && identical(e[[2L]], as.name(".data"))) {
    return(as_col(e[[3L]]))
  }
  # (!!sym(yvar): `!`(`!`(sym(yvar))))
  if (identical(f, "!") && length(e) == 2L && is.call(e[[2L]]) &&
      identical(e[[2L]][[1L]], as.name("!")) && is.call(e[[2L]][[2L]])) {
    s <- e[[2L]][[2L]]
    if (sub("^rlang::", "", .fig_deparse(s[[1L]])) %in% c("sym", "ensym", "as.name", "as.symbol") &&
        length(s) == 2L) {
      return(as_col(s[[2L]]))
    }
    return(NULL)
  }
  # ({{ y }}: `{`(`{`(y)), y given a column bare)
  if (identical(f, "{") && length(e) == 2L && is.call(e[[2L]]) &&
      identical(e[[2L]][[1L]], as.name("{")) && length(e[[2L]]) == 2L && is.name(e[[2L]][[2L]])) {
    v <- val(e[[2L]][[2L]])
    if (is.name(v)) return(v)
    if (is.character(v)) return(as_col(v))
  }
  NULL
}

# `e` with a function's formals (and its locals, as they are assigned)
# replaced by their values (`m`), `...` by the arguments given there
# (`dots`); inside aes() / vars() only a column named by an argument
# (.fig_prog_tidy_col()), never a bare name (a column there)
.fig_prog_fn_subst <- function(e, m, dots = NULL, in_aes = FALSE) {
  if (is.name(e)) {
    nm <- as.character(e)
    if (!in_aes && nzchar(nm) && nm %in% names(m)) return(m[[nm]])
    return(e)
  }
  if (!is.call(e)) return(e)
  f <- .fig_deparse(e[[1L]])
  if (identical(f, "function")) {
    e[[3L]] <- .fig_prog_fn_subst(e[[3L]], m[setdiff(names(m), names(e[[2L]]))], dots, in_aes)
    return(e)
  }
  if (in_aes) {
    col <- .fig_prog_tidy_col(e, m)
    if (!is.null(col)) return(col)
  }
  if (f %in% c("aes", "ggplot2::aes", "vars", "ggplot2::vars")) in_aes <- TRUE
  # (x$col, x@slot: the part's name kept)
  last <- if (f %in% c("$", "@")) 2L else length(e)
  for (i in seq_len(last)[-1L]) {
    if (is.null(e[[i]]) || identical(e[[i]], quote(expr = ))) next
    e[i] <- list(.fig_prog_fn_subst(e[[i]], m, dots, in_aes))
  }
  # (g(...): the arguments given at the call)
  if (length(e) > 1L) {
    is_dots <- vapply(as.list(e)[-1L], function(x) identical(x, as.name("...")), NA)
    if (any(is_dots)) {
      k <- which(is_dots)[1L] + 1L
      l <- as.list(e)
      e <- as.call(c(l[seq_len(k - 1L)], as.list(dots), l[-seq_len(k)]))
    }
  }
  e
}

# A call of a plot function of the program (`def`, the function written
# out) as the lines it runs: `lines` (its locals assigned, named for the
# call by `fresh`, tagged `tag`) and `value`, the expression it returns
.fig_prog_inline <- function(call, def, fresh, tag) {
  fun <- tryCatch(eval(def, baseenv()), error = function(err) NULL)
  mc <- if (is.function(fun)) tryCatch(match.call(fun, call, expand.dots = FALSE), error = function(err) NULL)
  if (is.null(mc)) return(NULL)
  fmls <- formals(fun)
  given <- as.list(mc)[-1L]
  m <- list()
  dots <- NULL
  for (k in names(fmls)) {
    if (identical(k, "...")) {
      dots <- given[["..."]]
      next
    }
    if (k %in% names(given)) {
      m[k] <- list(given[[k]])
    } else if (!identical(fmls[[k]], quote(expr = ))) {
      m[k] <- list(.fig_prog_fn_subst(fmls[[k]], m, dots))
    }
  }
  body <- def[[3L]]
  stmts <- if (is.call(body) && identical(body[[1L]], as.name("{"))) as.list(body)[-1L] else list(body)
  lines <- list()
  local <- character()
  value <- NULL
  for (j in seq_along(stmts)) {
    s <- stmts[[j]]
    last <- j == length(stmts)
    # (return(p), invisible(p), print(p) last: p)
    while (is.call(s) && .fig_deparse(s[[1L]]) %in% c("return", "invisible", "print") &&
           length(s) == 2L) {
      s <- s[[2L]]
      last <- TRUE
    }
    if (is.call(s) && .fig_deparse(s[[1L]]) %in% c("<-", "=") && is.name(s[[2L]])) {
      x <- as.character(s[[2L]])
      rhs <- .fig_prog_fn_subst(s[[3L]], m, dots)
      if (!x %in% names(local)) local[[x]] <- fresh(paste0(x, "_", tag))
      m[x] <- list(as.name(local[[x]]))
      lines <- c(lines, list(call("<-", m[[x]], rhs)))
      if (last) value <- m[[x]]
    } else {
      s <- .fig_prog_fn_subst(s, m, dots)
      if (last) value <- s else lines <- c(lines, list(s))
    }
    if (last) break
  }
  list(lines = lines, value = value)
}

# a call's tag, for the names of the objects it makes: its first text
# argument (lab_plot("ALT"): alt), else its number
.fig_prog_call_tag <- function(call, n) {
  for (a in as.list(call)[-1L]) {
    if (is.character(a) && length(a) == 1L && grepl("[A-Za-z0-9]", a)) {
      out <- tolower(gsub("^_+|_+$", "", gsub("[^A-Za-z0-9]+", "_", a)))
      if (nchar(out) <= 20L && grepl("^[a-z0-9_]+$", out)) return(out)
    }
  }
  as.character(n)
}

# the calls whose body runs many times (or not at all): a plot function's
# call inside one is left as it is
.fig_prog_loops <- c("function", "for", "while", "repeat", "lapply", "sapply", "vapply", "Map",
                     "mapply", "map", "map2", "pmap", "walk", "walk2", "iwalk", "imap")

# `e` with each call of a plot function of the program (`fns`) given a name:
# list(e = the line with the names in place of the calls, lines = the
# lines that make them, before it).  Not inside a function written out.
.fig_prog_hoist <- function(e, fns, inline_one) {
  pre <- list()
  walk <- function(x) {
    if (!is.call(x)) return(x)
    # (a function written out, a loop: run many times, not read)
    if (sub("^.*::", "", .fig_deparse(x[[1L]])) %in% .fig_prog_loops) return(x)
    for (i in seq_along(x)[-1L]) {
      if (is.null(x[[i]]) || identical(x[[i]], quote(expr = ))) next
      x[i] <- list(walk(x[[i]]))
    }
    if (is.name(x[[1L]]) && as.character(x[[1L]]) %in% names(fns)) {
      r <- inline_one(x)
      if (!is.null(r)) {
        pre <<- c(pre, r$lines)
        return(r$name)
      }
    }
    x
  }
  e <- walk(e)
  list(e = e, lines = pre)
}

# `last_plot()` in `e` as the name of the plot made last (`nm`)
.fig_prog_last_plot <- function(e, nm) {
  if (is.null(nm) || !is.call(e)) return(e)
  if (identical(sub("^ggplot2::", "", .fig_deparse(e[[1L]])), "last_plot") && length(e) == 1L) {
    return(as.name(nm))
  }
  if (identical(e[[1L]], as.name("function"))) return(e)
  for (i in seq_along(e)[-1L]) {
    if (is.null(e[[i]]) || identical(e[[i]], quote(expr = ))) next
    e[i] <- list(.fig_prog_last_plot(e[[i]], nm))
  }
  e
}

# `p %+% x`: is `x` a term of a plot (p %+% geom_point(), as `+`) rather
# than the plot's new data?
.fig_prog_is_term <- function(x) {
  if (!is.call(x)) return(FALSE)
  f <- sub("^ggplot2::", "", .fig_deparse(x[[1L]]))
  f %in% c("aes", "labs", "theme", "guides", "xlab", "ylab", "ggtitle", "xlim", "ylim", "lims") ||
    grepl("^(geom|stat|scale|coord|facet|theme|annotat|guide)_?", f)
}

# a plot's terms (`terms`, the first its ggplot() call) with new data,
# p %+% ast: ggplot(ast, ...)
.fig_prog_new_data <- function(terms, data) {
  h <- terms[[1L]]
  if (!is.call(h)) return(terms)
  f <- .fig_deparse(h[[1L]])
  h <- .fig_name_args(h, .fig_find_fun(f))
  nm <- names(h) %||% rep("", length(h))
  if ("data" %in% nm) {
    h[["data"]] <- data
  } else {
    l <- as.list(h)
    h <- as.call(c(l[1L], list(data = data), l[-1L]))
  }
  terms[[1L]] <- h
  terms
}
