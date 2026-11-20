# A layer's data as rows of a data by its name (round 19 of the figure SPEC
# coverage): {fn: subset, args: {x: km, subset: censored}}, written
# subset(km, censored).  Read from the ways a program writes it:
# subset(km, censored), km[km$censored, ] and dplyr::filter(km, censored,
# ...) -- the rows kept the same (a row whose condition is NA is not drawn
# either way: ggplot2 drops a row of NA values).

# The data and the condition of rows of a data by name: list(name, cond)
# of x[x$a == 1, ], subset(x, a == 1) and filter(x, a == 1, b) (the
# conditions joined by &), or NULL for any other.  `name`, when given, is
# the data it must be (a part of the plot's data).
.fig_data_rows <- function(v, name = NULL) {
  if (!is.call(v) || length(v) < 3L || !is.name(v[[2L]])) return(NULL)
  nm <- as.character(v[[2L]])
  if (!is.null(name) && !identical(nm, name)) return(NULL)
  f <- .fig_deparse(v[[1L]])
  cond <- NULL
  if (identical(f, "[") && length(v) == 4L && identical(v[[4L]], quote(expr = ))) {
    # (km$censored: the column, censored)
    strip <- function(x) {
      if (is.call(x) && identical(x[[1L]], as.name("$")) && identical(x[[2L]], as.name(nm))) {
        return(x[[3L]])
      }
      if (is.call(x)) {
        for (i in seq_along(x)[-1L]) {
          if (!is.null(x[[i]]) && !identical(x[[i]], quote(expr = ))) x[[i]] <- strip(x[[i]])
        }
      }
      x
    }
    cond <- strip(v[[3L]])
    # (another of the data's names left: not only its columns; rows by
    # number or order, km[1:3, ], km[order(km$time), ]: no condition)
    if (nm %in% all.names(cond)) return(NULL)
    if (!.fig_rows_logical(cond, is.name(v[[3L]]))) return(NULL)
  } else if (f %in% c("subset", "base::subset") && length(v) == 3L &&
             (is.null(names(v)) || names(v)[3L] %in% c("", "subset"))) {
    cond <- v[[3L]]
  } else if (f %in% c("filter", "dplyr::filter")) {
    a <- as.list(v)[-(1:2)]
    if (!is.null(names(a)) && any(nzchar(names(a)))) return(NULL)
    cond <- Reduce(function(x, y) call("&", x, y), a)
  }
  if (is.null(cond) || identical(cond, quote(expr = ))) return(NULL)
  list(name = nm, cond = cond)
}

# whether a condition of rows is TRUE / FALSE as written: a comparison, a
# logical operator, is.na(), %in%, grepl() ...; a column by its name (not
# when it is an object of the program, `bare`: km[keep, ] may be numbers)
.fig_rows_logical_fns <- c("==", "!=", "<", ">", "<=", ">=", "&", "|", "!", "&&", "||",
                           "%in%", "is.na", "isTRUE", "isFALSE", "xor", "grepl",
                           "startsWith", "endsWith", "between", "dplyr::between",
                           "is.finite", "complete.cases", "duplicated")
.fig_rows_logical <- function(e, bare = FALSE) {
  if (is.name(e)) return(!bare)
  if (!is.call(e)) return(is.logical(e) && length(e) == 1L && !is.na(e))
  f <- .fig_deparse(e[[1L]])
  if (identical(f, "(") && length(e) == 2L) return(.fig_rows_logical(e[[2L]]))
  f %in% .fig_rows_logical_fns
}

# whether a value is rows of a data by name, as the SPEC writes them:
# {fn: subset, args: {x: <a name>, subset: <a condition, R's text>}}
.fig_is_data_rows <- function(v) {
  a <- v$args %||% list()
  if (!identical(sort(names(a)), c("subset", "x"))) return(FALSE)
  if (!is.character(a$x) || length(a$x) != 1L || !grepl("^[A-Za-z.][A-Za-z0-9._]*$", a$x)) {
    return(FALSE)
  }
  !is.null(.fig_rows_cond(a$subset))
}

# a condition of rows as its text, parsed: one expression that is no
# formula, function or assignment; NULL for any other
.fig_rows_cond <- function(txt) {
  if (inherits(txt, "tfl_r")) txt <- as.character(unclass(txt))
  if (!is.character(txt) || length(txt) != 1L || is.na(txt) || !nzchar(trimws(txt))) return(NULL)
  e <- tryCatch(parse(text = txt, keep.source = FALSE), error = function(err) NULL)
  if (length(e) != 1L) return(NULL)
  e <- e[[1L]]
  if (!is.call(e) && !is.name(e)) return(NULL)
  if (is.call(e) && .fig_deparse(e[[1L]]) %in% c("~", "function", "<-", "<<-", "=", "{")) return(NULL)
  e
}

# The rows' value as R writes it, subset(km, censored); NULL when it is
# not rows of a data by name
.fig_data_rows_lang <- function(v) {
  if (!.fig_is_data_rows(v)) return(NULL)
  call("subset", as.name(v$args$x), .fig_rows_cond(v$args$subset))
}

# The rows' value read from R (km[km$censored, ]), a tfl_call, or NULL
.fig_data_rows_value <- function(x) {
  r <- .fig_data_rows(x)
  if (is.null(r)) return(NULL)
  tfl_call("subset", list(x = r$name, subset = .fig_deparse(r$cond)))
}

# subset()'s problems, as .schema_check_calls_in() says them
.schema_check_data_rows <- function(v) {
  if (.fig_is_data_rows(v)) return(list())
  list(list(level = "error", message = paste0(
    "subset() takes the data by its name and the rows' condition as R's text: ",
    "{fn: subset, args: {x: km, subset: censored}}")))
}
