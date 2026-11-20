# ============================================================================
#  A ggplot2 program as a figure's content
# ----------------------------------------------------------------------------
#  tfl_args_from_r() reads one call; a figure's program is more than its
#  calls: the objects it makes first and uses in them (pd <-
#  position_dodge(0.3), cols <- c(Placebo = "grey40", ...), my_theme <-
#  theme_bw() + theme(...)), the plot built in steps (p <- p + ...),
#  several plots arranged by patchwork, ggsave()'s size.  An object of
#  constants is read in place of its name, so the item is typed as if the
#  value had been written there (a position, a map of colours); an object
#  of the data stays its name (the data part's), and the program's other
#  lines are the data part, one code step.
# ============================================================================

# the functions whose call is no constant: they read or draw, or depend on
# the session (a value of these is the data part's)
.fig_prog_not_const <- c("ggplot", "ggsurvfit", "ggcuminc", "qplot", "ggplot_build",
                         "ggplot_gtable", "ggsave", "last_plot", "set_last_plot", "layer_data",
                         "layer_scales", "layer_grob", "get_layer_data", "get_layer_grob",
                         "get_panel_scales", "get_guide_data", "theme_get", "theme_set",
                         "theme_update", "theme_replace", "print", "plot", "summary",
                         "Sys.Date", "Sys.time", "sample", "set.seed", "eval", "get", "get0",
                         "assign", "source", "file", "readRDS", "load", "system", "nrow",
                         "ncol", "length", "levels", "unique", "max", "min", "sum",
                         "mean", "median", "table", "names")

# base R's (and grid's) functions a value of constants is built with
.fig_prog_base_fns <- c("c", "list", "setNames", "seq", "seq_len", "seq_along", "rep", "rev",
                        "paste", "paste0", "sprintf", "format", "formatC", "toupper", "tolower",
                        "trimws", "round", "signif", "abs", "sqrt", "exp", "log", "log2",
                        "log10", "as.Date", "as.numeric", "as.integer", "as.character",
                        "factor", "data.frame", "expression", "quote", "bquote", "function",
                        "unit", "rgb", "hcl", "hcl.colors", "rainbow", "grey.colors",
                        "gray.colors", "adjustcolor", "colorRampPalette", "qnorm", "qt",
                        "ifelse", "pmax", "pmin", "range", "sort", "(", ":", "+", "-", "*", "/", "^", "%%",
                        "%/%", "==", "!=", "<", ">", "<=", ">=", "!", "&", "|", "&&", "||")

# the packages whose exports build a value of constants
.fig_prog_pkgs <- c("ggplot2", "scales", "grid", "RColorBrewer", "viridisLite", "patchwork",
                    "ggsurvfit", "ggh4x", "ggtext", "ggrepel", "ggnewscale")

# the objects a value of constants may name besides the program's own
.fig_prog_base_objs <- c("LETTERS", "letters", "month.abb", "month.name", "pi", "T", "F")

# Is the call's function one a value of constants may call?
.fig_prog_const_fn <- function(head) {
  f <- .fig_deparse(head)
  if (grepl("::", f, fixed = TRUE)) {
    pkg <- sub("::.*$", "", f)
    fn <- sub("^.*::", "", f)
    if (!pkg %in% c(.fig_prog_pkgs, "base", "stats", "grDevices")) return(FALSE)
    f <- fn
  } else if (!is.name(head)) {
    return(FALSE)
  }
  if (f %in% .fig_prog_not_const) return(FALSE)
  if (f %in% .fig_prog_base_fns) return(TRUE)
  for (pkg in .fig_prog_pkgs) {
    if (requireNamespace(pkg, quietly = TRUE) && f %in% getNamespaceExports(pkg)) return(TRUE)
  }
  FALSE
}

# Is `e` a value of constants: constants, the program's objects of
# constants (`known`), and calls of the functions above of them?  aes()
# and vars() (columns, quoted) and a formula are; a function written out
# is when its body is, its formals known.
.fig_prog_is_const <- function(e, known = character()) {
  if (is.null(e) || (is.atomic(e) && !is.object(e))) return(TRUE)
  if (is.name(e)) {
    nm <- as.character(e)
    return(nm %in% c(known, .fig_prog_base_objs) || !nzchar(nm))
  }
  if (!is.call(e)) return(FALSE)
  f <- .fig_deparse(e[[1L]])
  if (f %in% c("aes", "ggplot2::aes", "vars", "ggplot2::vars", "~")) return(TRUE)
  if (identical(f, "function")) {
    return(.fig_prog_is_const(e[[3L]], c(known, names(e[[2L]]))))
  }
  if (!.fig_prog_const_fn(e[[1L]])) return(FALSE)
  # (a function by its name as an argument, labels = abs: no object)
  a <- as.list(e)[-1L]
  all(vapply(a, function(x) {
    if (is.name(x) && !as.character(x) %in% known && .fig_prog_const_fn(x)) return(TRUE)
    .fig_prog_is_const(x, known)
  }, NA))
}

# `e` with the program's objects of constants (`consts`, a named list of
# their values) in place of their names -- not inside aes() / vars() (a
# column's name there), not a formal of a function written out
.fig_prog_subst <- function(e, consts) {
  if (!length(consts)) return(e)
  if (is.name(e)) {
    nm <- as.character(e)
    return(if (nzchar(nm) && nm %in% names(consts)) consts[[nm]] else e)
  }
  if (!is.call(e)) return(e)
  f <- .fig_deparse(e[[1L]])
  if (f %in% c("aes", "ggplot2::aes", "vars", "ggplot2::vars", "after_stat", "after_scale",
               "stage")) {
    return(e)
  }
  if (identical(f, "function")) {
    keep <- consts[setdiff(names(consts), names(e[[2L]]))]
    e[[3L]] <- .fig_prog_subst(e[[3L]], keep)
    return(e)
  }
  # (an object's part, df$colour or cols[1:2]: the object kept, its part R)
  if (f %in% c("$", "@", "[", "[[")) return(e)
  for (i in seq_along(e)[-1L]) {
    if (!is.null(e[[i]])) e[[i]] <- .fig_prog_subst(e[[i]], consts)
  }
  e
}

# the data piped into a call (magrittr's `x %>% f(a)`, f's first argument):
# the call as written without the pipe, f(x, a); the native pipe (`|>`) is
# R's own and already f(x, a) when parsed
.fig_prog_unpipe <- function(e) {
  if (!is.call(e) || !identical(.fig_deparse(e[[1L]]), "%>%") || length(e) != 3L) return(e)
  rhs <- e[[3L]]
  # (the data before it kept as written, a pipe too)
  lhs <- e[[2L]]
  if (is.name(rhs)) return(as.call(list(rhs, lhs)))
  if (!is.call(rhs)) return(e)
  # (a dot as an argument is where the data goes: f(a, .))
  dots <- vapply(as.list(rhs)[-1L], function(x) identical(x, as.name(".")), NA)
  if (any(dots)) {
    rhs[[which(dots)[1L] + 1L]] <- lhs
    return(rhs)
  }
  as.call(c(list(rhs[[1L]], lhs), as.list(rhs)[-1L]))
}

# a plot's terms: the `+` chain's, the data piped into its first
# (adlb %>% filter(...) %>% ggplot(aes(...)) + geom_line(): ggplot(the
# piped data, aes(...)), geom_line())
.fig_prog_terms <- function(e) {
  terms <- .fig_split_plus(e)
  head <- terms[[1L]]
  if (is.call(head) && identical(.fig_deparse(head[[1L]]), "%>%")) {
    h <- .fig_prog_unpipe(head)
    if (is.call(h) && sub("^.*::", "", .fig_deparse(h[[1L]])) %in% .fig_p_fns) terms[[1L]] <- h
  }
  terms
}

# The plot's data as the program computes it in the plot's line (a pipe,
# filter(), adlb[adlb$PARAMCD == "ALT", ]; ggsurvfit()'s fit): the call
# with its arguments named, the data's position among them (0 when the
# data is an object's name, a constant or none) and the object's name
.fig_prog_data_arg <- function(head) {
  none <- list(call = head, i = 0L, name = "")
  if (!is.call(head)) return(none)
  f <- .fig_deparse(head[[1L]])
  fun <- .fig_find_fun(f)
  # (ggsurvfit() / ggcuminc()'s fit is its `x`)
  arg <- if (sub("^.*::", "", f) %in% c("ggsurvfit", "ggcuminc")) "x" else "data"
  if (is.null(fun) || !arg %in% names(formals(fun))) return(none)
  h <- .fig_name_args(head, fun)
  nm <- names(h) %||% rep("", length(h))
  i <- match(arg, nm)
  if (is.na(i)) return(none)
  v <- h[[i]]
  if (is.name(v)) return(list(call = h, i = 0L, name = as.character(v)))
  if (is.null(v) || is.atomic(v)) return(none)
  # (a formula or function of the plot's data, a table of constants: the SPEC's own)
  if (is.call(v) && .fig_deparse(v[[1L]]) %in% c("~", "function", "data.frame")) return(none)
  list(call = h, i = i, name = "")
}

# the program's calls without `ggplot2::` (the figure's code attaches
# ggplot2): ggplot2::geom_line() is geom_line()
.fig_prog_strip_ns <- function(e) {
  if (!is.call(e)) return(e)
  h <- e[[1L]]
  if (is.call(h) && identical(h[[1L]], as.name("::")) && identical(h[[2L]], as.name("ggplot2"))) {
    e[[1L]] <- h[[3L]]
  }
  for (i in seq_along(e)[-1L]) {
    if (!is.null(e[[i]]) && !identical(e[[i]], quote(expr = ))) e[[i]] <- .fig_prog_strip_ns(e[[i]])
  }
  e
}

# A layer's data as a part of the plot's data (`pd`), the SPEC's formula
# of it (~ subset(.x, censored)), or NULL: pd[pd$censored, ], subset(pd,
# censored), filter(pd, censored, ...), pd %>% filter(...) and magrittr's
# function of the plot's data, . %>% filter(...) -- the rows
# kept the same (a row whose condition is NA is not drawn either way)
.fig_prog_layer_subset <- function(v, pd) {
  if (!is.call(v)) return(NULL)
  # (magrittr's `. %>% filter(...)`: a function of the plot's data, its dot)
  if (identical(.fig_deparse(v[[1L]]), "%>%") && identical(v[[2L]], as.name("."))) pd <- "."
  if (!nzchar(pd)) return(NULL)
  r <- .fig_data_rows(.fig_prog_unpipe(v), pd)
  if (is.null(r)) return(NULL)
  call("~", call("subset", as.name(".x"), r$cond))
}

# A term that is several terms or none, as ggplot2 adds it: a list of
# terms, NULL, `{ term }`, `if (cond) term` with a condition of constants
# (the program's flags, read in place).  Returns a list of terms.
.fig_prog_expand_term <- function(t) {
  if (is.null(t)) return(list())
  if (!is.call(t)) return(list(t))
  f <- .fig_deparse(t[[1L]])
  # (a branch of several terms, if (x) a else b + c: each)
  if (identical(f, "+") && length(t) == 3L) {
    return(c(.fig_prog_expand_term(t[[2L]]), .fig_prog_expand_term(t[[3L]])))
  }
  if (identical(f, "{") && length(t) == 2L) return(.fig_prog_expand_term(t[[2L]]))
  if (identical(f, "(") && length(t) == 2L) return(.fig_prog_expand_term(t[[2L]]))
  nms <- names(t)[-1L]
  if (f %in% c("list", "base::list") && (is.null(nms) || !any(nzchar(nms)))) {
    return(unlist(lapply(as.list(t)[-1L], .fig_prog_expand_term), recursive = FALSE))
  }
  if (identical(f, "if")) {
    cond <- t[[2L]]
    v <- if (.fig_prog_is_const(cond)) tryCatch(eval(cond, baseenv()), error = function(e) NULL)
    if (is.logical(v) && length(v) == 1L && !is.na(v)) {
      if (v) return(.fig_prog_expand_term(t[[3L]]))
      return(if (length(t) == 4L) .fig_prog_expand_term(t[[4L]]) else list())
    }
  }
  list(t)
}

# a program's line that makes a plot: a `+` chain whose first term is a
# call of a plot-making function, or a plot of the program's (p + ...)
.fig_prog_plot_head <- function(e, plots) {
  terms <- .fig_prog_terms(e)
  head <- terms[[1L]]
  # (p1 + p2: patchwork's arrangement of two plots, no plot's term)
  if (any(vapply(terms[-1L], function(t) !is.null(.fig_prog_compose_plots(t, plots)), NA))) {
    return(NULL)
  }
  if (is.name(head) && as.character(head) %in% names(plots)) return(as.character(head))
  # (p %+% ast: p with other data)
  if (is.call(head) && identical(.fig_deparse(head[[1L]]), "%+%") && length(head) == 3L &&
      is.name(head[[2L]]) && as.character(head[[2L]]) %in% names(plots)) {
    return(as.character(head[[2L]]))
  }
  if (is.call(head) && sub("^.*::", "", .fig_deparse(head[[1L]])) %in% .fig_p_fns) return("")
  NULL
}

# the names a patchwork arrangement of the program's plots holds (p1 / p2,
# p1 + p2 + plot_layout(), wrap_plots(p1, p2)), or NULL when it is none
.fig_prog_compose_plots <- function(e, plots) {
  if (is.name(e)) return(if (as.character(e) %in% names(plots)) as.character(e))
  if (!is.call(e)) return(NULL)
  f <- sub("^.*::", "", .fig_deparse(e[[1L]]))
  if (f %in% c("/", "|", "+", "-", "*", "&")) {
    l <- .fig_prog_compose_plots(e[[2L]], plots)
    if (is.null(l)) return(NULL)
    if (f %in% c("+", "*", "&") && is.call(e[[3L]]) &&
        sub("^.*::", "", .fig_deparse(e[[3L]][[1L]])) %in% c("plot_layout", "plot_annotation", "theme")) {
      return(l)
    }
    r <- .fig_prog_compose_plots(e[[3L]], plots)
    return(if (!is.null(r)) c(l, r))
  }
  if (identical(f, "wrap_plots")) {
    a <- as.list(e)[-1L]
    nm <- names(a) %||% rep("", length(a))
    # (plots by a design's letter, A = p1: the named that are no argument of it)
    own <- setdiff(names(formals(.fig_find_fun("wrap_plots") %||% function(...) NULL)), "...")
    letters_named <- nzchar(nm) & !nm %in% c(own, "plots")
    if (any(letters_named) && !any(!nzchar(nm))) {
      p <- a[letters_named]
      if (!all(vapply(p, function(x) is.name(x) && as.character(x) %in% names(plots), NA))) return(NULL)
      return(vapply(p, as.character, ""))
    }
    p <- a[!nzchar(nm) | nm == "plots"]
    p <- unlist(lapply(p, function(x) {
      if (is.call(x) && .fig_deparse(x[[1L]]) %in% c("list", "c")) as.list(x)[-1L] else list(x)
    }), recursive = FALSE)
    if (!length(p) || !all(vapply(p, function(x) is.name(x) && as.character(x) %in% names(plots), NA))) {
      return(NULL)
    }
    return(unname(vapply(p, as.character, "")))
  }
  NULL
}

# a plot's name in the content: the program's, without p_ (p_km: km)
.fig_prog_plot_name <- function(nm) {
  out <- sub("^p_(?=[A-Za-z])", "", nm, perl = TRUE)
  if (!grepl("^[A-Za-z][A-Za-z0-9_]*$", out) || out %in% c(.fig_p_fns, .fig_compose_fns) ||
      exists(out, envir = asNamespace("ggplot2"), inherits = FALSE)) {
    out <- paste0("plot_", gsub("[^A-Za-z0-9_]", "_", nm))
  }
  out
}

# a patchwork arrangement as its items: wrap_plots(plots = [names], ncol /
# nrow ...), then plot_annotation() / theme() / plot_layout() given after
.fig_prog_compose_items <- function(e, names_of) {
  extra <- list()
  layout <- list()
  # (the terms added to the arrangement: plot_layout(), plot_annotation(), & theme())
  while (is.call(e) && .fig_deparse(e[[1L]]) %in% c("+", "*", "&") && is.call(e[[3L]]) &&
         sub("^.*::", "", .fig_deparse(e[[3L]][[1L]])) %in% c("plot_layout", "plot_annotation", "theme")) {
    t <- e[[3L]]
    f <- sub("^.*::", "", .fig_deparse(t[[1L]]))
    it <- .fig_lang_item(t)
    if (identical(f, "plot_layout")) layout <- c(it$args, layout)
    else {
      if (identical(f, "theme")) it$args$op <- .fig_deparse(e[[1L]])
      extra <- c(list(it), extra)
    }
    e <- e[[2L]]
  }
  if (is.call(e) && identical(sub("^.*::", "", .fig_deparse(e[[1L]])), "wrap_plots")) {
    a <- as.list(e)[-1L]
    nm <- names(a) %||% rep("", length(a))
    syms <- .fig_prog_compose_plots(e, names_of)
    # (by a design's letter, A = p1: the plots a map)
    pl <- if (!is.null(names(syms))) as.list(stats::setNames(unname(names_of[syms]), names(syms)))
          else unname(names_of[syms])
    rest <- a[nzchar(nm) & nm != "plots" & !nm %in% names(syms)]
    args <- c(list(plots = pl),
              lapply(stats::setNames(names(rest), names(rest)),
                     function(k) .fig_lang_value(rest[[k]], k)), layout)
    return(c(list(tfl_item("wrap_plots", args)), extra))
  }
  syms <- .fig_prog_compose_plots(e, names_of)
  below <- grepl("/", .fig_deparse(e), fixed = TRUE)
  args <- c(list(plots = unname(names_of[syms])), if (below) list(ncol = 1L) else list(nrow = 1L),
            layout[setdiff(names(layout), if (below) "ncol" else "nrow")])
  c(list(tfl_item("wrap_plots", args)), extra)
}

# ggsave()'s size, resolution and file as the image's item (inches)
.fig_prog_image <- function(e) {
  e <- .fig_name_args(e, .fig_find_fun("ggsave"))
  a <- as.list(e)[-1L]
  val <- function(k) {
    v <- a[[k]]
    if (is.null(v)) return(NULL)
    v <- tryCatch(eval(v, baseenv()), error = function(err) NULL)
    if (is.atomic(v) && length(v) == 1L && !is.na(v)) v
  }
  units <- val("units") %||% "in"
  per_in <- unname(c("in" = 1, cm = 2.54, mm = 25.4)[units])
  args <- list()
  for (k in c("width", "height")) {
    v <- val(k)
    if (is.numeric(v) && !is.na(per_in)) args[[paste0(k, "_in")]] <- round(v / per_in, 3)
  }
  dpi <- val("dpi")
  if (is.numeric(dpi)) args$dpi <- as.integer(dpi)
  file <- val("filename")
  if (is.character(file)) {
    ext <- tolower(tools::file_ext(file))
    if (ext %in% c("png", "svg", "pdf")) args$format <- ext
    args$file <- file
  }
  tfl_item("tfl_image", args)
}

#' A ggplot2 program as a figure's content
#'
#' Reads the R program of a figure -- the way it is written without
#' tflspec -- into a figure report's `content:` items and its data part.
#' Where [tfl_args_from_r()] reads one call, this reads the program:
#'
#' * **The objects it makes of constants and uses in the plot** are read
#'   in place of their names, so each item is typed as if the value had
#'   been written there: `pd <- position_dodge(0.3)` then
#'   `geom_line(position = pd)` is `position: {fn: position_dodge, args:
#'   {width: 0.3}}`; `cols <- c(Placebo = "grey40", Drug = "#1F78B4")` then
#'   `scale_colour_manual(values = cols)` is the map of colours; a theme
#'   of the study's (`my_theme <- theme_bw(base_size = 9) +
#'   theme(legend.position = "bottom")`, then `+ my_theme`) is its terms.
#'   A value of constants is built of constants and ggplot2's, scales',
#'   grid's, RColorBrewer's and viridisLite's functions and base R's that
#'   build values (`c()`, `setNames()`, `seq()`, `paste0()`, `unit()`,
#'   arithmetic ...); `aes()`, `vars()` and a formula are too.  A name
#'   inside `aes()` is a column, never an object.
#' * **The plot**: a `+` chain from `ggplot()` (or `ggsurvfit()`,
#'   `ggcuminc()`), bare or assigned, built in steps too (`p <- p +
#'   geom_point()`).  The plot printed or saved last is the figure; with
#'   several plots arranged by patchwork (`p_km / p_risk`, `p1 | p2`,
#'   `wrap_plots(p1, p2, ncol = 1)`, `+ plot_layout(heights = c(3, 1))`)
#'   each is a [tfl_plot()] named as in the program (without `p_`), then
#'   `wrap_plots`.  `theme_set(theme_bw())` before a plot is the plot's
#'   theme, its first term.
#' * **The data piped into the plot** (`adlb %>% filter(PARAMCD == "ALT")
#'   %>% ggplot(aes(...))`, the native `|>`, magrittr's `.`) or computed
#'   in `ggplot()`'s own `data` (or `ggsurvfit()`'s fit) is an object of
#'   the data part, `plot_data` (`p_data` for a plot `p`, `plot_fit` for a
#'   fit), so the item names it.  A layer's data that is a part of the
#'   plot's (`km[km$censored, ]`, `subset(km, censored)`, `km %>%
#'   filter(censored)`, `. %>% filter(censored)`) is its formula, `~
#'   subset(.x, censored)`; any other computed there is an object of the
#'   data part too.
#' * **Terms as ggplot2 adds them**: a list of terms (`+ list(geom_hline(...),
#'   theme_bw())`, or an object of them), `+ NULL`, `{ }`, and `if (flag)
#'   term` or a step `if (flag) p <- p + term` whose flag is a constant of
#'   the program (`show_ci <- TRUE`) -- the branch it takes.  A condition
#'   of the data stays R.  `theme_update()` after `theme_set()` is the
#'   plot's theme with those settings, and `ggplot2::` before a call is
#'   dropped (the figure's code attaches ggplot2).
#' * **A plot function of the program** (`lab_plot <- function(data,
#'   param, ylab = "Value") { ... ggplot() ... }`): each call
#'   (`lab_plot(adlb, "ALT")`, also inside `print()`, `ggsave()` or a
#'   patchwork arrangement) is read as the lines the function runs, its
#'   arguments (or defaults) in place of its formals.  Its local objects are
#'   named for the call (`d` of `lab_plot(adlb, "ALT")` is `d_alt`), so two
#'   calls keep their data apart; inside `aes()` / `vars()` a column named
#'   by an argument (`.data[[yvar]]`, `!!sym(yvar)`, `{{ y }}`) is the
#'   column, and `...` is the arguments given there.  A call inside a loop
#'   (`for`, `lapply()`, `purrr::map()`) is left to the data part, the
#'   function's definition with it.  `p %+% ast` is `p` with that data, and
#'   `last_plot()` the plot made last.
#' * **`ggsave()`**'s width, height (in its `units`), dpi and file are the
#'   `tfl_image` item's.
#' * **Every other line** (reading and deriving the data, an object of the
#'   data's) is the data part: one code step, in the program's order.
#'   `library()` / `require()` and `print()` are left out.
#'
#' The calls are read as [tfl_args_from_r()] reads them, ggplot2's
#' spellings of before 3.4 / 3.5 as 4.0 writes them.
#'
#' @param code The program: R code, a character vector (its lines) or a
#'   file's path (`.R`).
#' @return A list: `data` (`source: code` and its `steps`, none when every
#'   line is the plot's), `content` (the items, the image's last) and the
#'   attributes `objects` (the names read in place, a character vector)
#'   and `problems` ([tfl_schema_check()]'s of every item, with the column
#'   `item`, its position in the content).
#' @examples
#' code <- c(
#'   'pd <- position_dodge(width = 0.3)',
#'   'cols <- c(Placebo = "grey40", Drug = "#1F78B4")',
#'   'p <- ggplot(adlb_mean, aes(AVISITN, mean, colour = TRTA)) +',
#'   '  geom_line(position = pd) +',
#'   '  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2, position = pd)',
#'   'p <- p + scale_colour_manual(values = cols) + theme_bw()',
#'   'ggsave("f_lab_mean.png", p, width = 16, height = 10, units = "cm", dpi = 300)')
#' x <- tfl_fig_content_from_r(code)
#' vapply(x$content, function(it) it$fn, "")
#' x$content[[2]]$args$position
#' @export
tfl_fig_content_from_r <- function(code) {
  if (!is.character(code)) .ard_stop("tfl_fig_content_from_r(): `code` is R code, as text.")
  if (length(code) == 1L && !grepl("\n", code, fixed = TRUE) && grepl("[.][Rr]$", code) &&
      file.exists(code)) {
    code <- readLines(code, warn = FALSE, encoding = "UTF-8")
  }
  ex <- tryCatch(parse(text = code, keep.source = FALSE),
                 error = function(err) .ard_stop(sprintf("tfl_fig_content_from_r(): not R: %s",
                                                         conditionMessage(err))))
  consts <- list()
  plots <- list()
  data_lines <- character()
  # (each data line's constant, "" for none: kept when a name is left)
  line_const <- character()
  shown <- NULL
  compose <- NULL
  compose_name <- ""
  image <- NULL
  global_theme <- NULL
  n_bare <- 0L
  if_flags <- character()
  # (the program's plot functions, the plot made last)
  plot_fns <- list()
  fns_used <- character()
  n_inline <- 0L
  last_made <- NULL
  add_plot <- function(nm, e) {
    head <- .fig_prog_plot_head(e, plots)
    terms <- .fig_prog_terms(e)
    if (nzchar(head)) {
      base <- plots[[head]]
      h1 <- terms[[1L]]
      # (p %+% ast: p's terms with ast its data; p %+% geom_point(): a term)
      if (is.call(h1) && identical(.fig_deparse(h1[[1L]]), "%+%")) {
        if (.fig_prog_is_term(h1[[3L]])) base <- c(base, list(h1[[3L]]))
        else base <- .fig_prog_new_data(base, h1[[3L]])
      }
      terms <- c(base, terms[-1L])
    } else if (!is.null(global_theme)) terms <- c(terms[1L], list(global_theme), terms[-1L])
    plots[[nm]] <<- terms
    shown <<- nm
    last_made <<- nm
  }
  # a call of a plot function of the program: the lines it runs, and the
  # name of the plot it returns
  inline_one <- function(x) {
    fn <- as.character(x[[1L]])
    n_inline <<- n_inline + 1L
    if (n_inline > 100L) return(NULL)
    tag <- .fig_prog_call_tag(x, n_inline)
    r <- .fig_prog_inline(x, plot_fns[[fn]], fresh, tag)
    if (is.null(r) || is.null(r$value)) return(NULL)
    fns_used <<- c(fns_used, fn)
    # (a function that saves or prints its plot last: that line, read as
    # the program's own; the call itself no value)
    v <- r$value
    if (is.call(v) && sub("^.*::", "", .fig_deparse(v[[1L]])) %in% c("ggsave", "print", "plot")) {
      return(list(lines = c(r$lines, list(v)), name = NULL))
    }
    if (is.name(v)) return(list(lines = r$lines, name = v))
    target <- as.name(fresh(paste0(fn, "_", tag)))
    list(lines = c(r$lines, list(call("<-", target, v))), name = target)
  }
  # (the names the program uses: a name made for its data is none of them)
  taken <- unique(all.names(ex))
  fresh <- function(base) {
    out <- base
    k <- 1L
    while (out %in% taken) {
      k <- k + 1L
      out <- paste0(base, k)
    }
    taken <<- c(taken, out)
    out
  }
  queue <- lapply(as.list(ex), .fig_prog_strip_ns)
  while (length(queue)) {
    e <- queue[[1L]]
    queue <- queue[-1L]
    # (a step on the program's flag, if (show_ci) p <- p + ...: the branch
    # its constants take; a block its lines)
    if (is.call(e) && identical(e[[1L]], as.name("{"))) {
      queue <- c(as.list(e)[-1L], queue)
      next
    }
    if (is.call(e) && identical(e[[1L]], as.name("if"))) {
      cond <- .fig_prog_subst(e[[2L]], consts)
      v <- if (.fig_prog_is_const(cond)) tryCatch(eval(cond, baseenv()), error = function(err) NULL)
      if (is.logical(v) && length(v) == 1L && !is.na(v)) {
        br <- if (v) e[[3L]] else if (length(e) == 4L) e[[4L]]
        # (the flag's line kept when the flag is used elsewhere)
        used <- intersect(all.names(e[[2L]]), names(consts))
        if (length(used)) if_flags <- c(if_flags, used)
        if (!is.null(br)) queue <- c(list(br), queue)
        next
      }
    }
    # (a call of a plot function that saved its plot: nothing left)
    if (is.null(e)) next
    # (last_plot(): the plot made last)
    e <- .fig_prog_last_plot(e, last_made)
    f <- if (is.call(e)) sub("^.*::", "", .fig_deparse(e[[1L]])) else ""
    # (a plot function of the program: kept, its calls read as its lines)
    if (f %in% c("<-", "=") && is.name(e[[2L]]) && .fig_prog_is_plot_fn(e[[3L]])) {
      plot_fns[[as.character(e[[2L]])]] <- e[[3L]]
      data_lines <- c(data_lines, .fig_deparse_r(e))
      line_const <- c(line_const, as.character(e[[2L]]))
      next
    }
    if (length(plot_fns)) {
      h <- .fig_prog_hoist(e, plot_fns, inline_one)
      if (length(h$lines) || !identical(h$e, e)) {
        queue <- c(h$lines, list(h$e), queue)
        next
      }
    }
    if (f %in% c("library", "require", "requireNamespace", "suppressPackageStartupMessages")) next
    if (f %in% c("<-", "=") && is.name(e[[2L]])) {
      lhs <- as.character(e[[2L]])
      rhs <- e[[3L]]
      if (!is.null(.fig_prog_plot_head(rhs, plots))) {
        add_plot(lhs, rhs)
      } else if (length(plots) && !is.null(.fig_prog_compose_plots(rhs, plots)) &&
                 !is.name(rhs)) {
        compose <- rhs
        compose_name <- lhs
        shown <- lhs
      } else if (.fig_prog_is_const(rhs, names(consts))) {
        consts[[lhs]] <- .fig_prog_subst(rhs, consts)
        data_lines <- c(data_lines, .fig_deparse_r(e))
        line_const <- c(line_const, lhs)
      } else {
        consts[[lhs]] <- NULL
        data_lines <- c(data_lines, .fig_deparse_r(e))
        line_const <- c(line_const, "")
      }
      next
    }
    if (identical(f, "theme_set") && length(e) == 2L) {
      global_theme <- .fig_prog_subst(e[[2L]], consts)
      next
    }
    # (theme_update(...): the session's theme with these settings, the
    # plots' after it)
    if (identical(f, "theme_update")) {
      upd <- .fig_prog_subst(e, consts)
      upd[[1L]] <- as.name("theme")
      global_theme <- call("+", global_theme %||% quote(theme_grey()), upd)
      next
    }
    if (identical(f, "ggsave")) {
      image <- .fig_prog_image(e)
      pa <- as.list(.fig_name_args(e, .fig_find_fun("ggsave")))$plot
      if (is.name(pa)) shown <- as.character(pa)
      next
    }
    if (f %in% c("print", "plot") && length(e) == 2L) e <- e[[2L]]
    if (is.name(e) && (as.character(e) %in% names(plots) || identical(as.character(e), compose_name))) {
      shown <- as.character(e)
      next
    }
    if (!is.null(.fig_prog_plot_head(e, plots))) {
      n_bare <- n_bare + 1L
      add_plot(if (n_bare == 1L) "content" else paste0("content", n_bare), e)
      next
    }
    if (length(plots) && !is.null(.fig_prog_compose_plots(e, plots))) {
      compose <- e
      shown <- NULL
      next
    }
    data_lines <- c(data_lines, .fig_deparse_r(e))
    line_const <- c(line_const, "")
  }
  if (!length(plots)) .ard_stop("tfl_fig_content_from_r(): no plot: no ggplot() (or ggsurvfit(), ggcuminc()) in the code.")
  read_in <- character()
  left <- character()
  # the plot's data computed in its line (a pipe, filter(), x[i, ]) as an
  # object of the data part (<plot>_data), and a layer's: a part of the
  # plot's data its formula (~ subset(.x, censored)), any other an object
  lift_data <- function(terms, nm) {
    d <- .fig_prog_data_arg(terms[[1L]])
    pd <- d$name
    if (d$i > 0L) {
      fit <- identical(names(d$call)[d$i], "x")
      pd <- fresh(paste0(if (identical(nm, "content")) "plot" else nm, if (fit) "_fit" else "_data"))
      data_lines <<- c(data_lines, paste(pd, "<-", .fig_deparse_r(d$call[[d$i]])))
      line_const <<- c(line_const, "")
      d$call[[d$i]] <- as.name(pd)
      terms[[1L]] <- d$call
    }
    for (k in seq_along(terms)[-1L]) {
      l <- .fig_prog_data_arg(terms[[k]])
      if (l$i == 0L) next
      sub <- .fig_prog_layer_subset(l$call[[l$i]], pd)
      if (is.null(sub)) {
        sub <- as.name(fresh(paste0(if (nzchar(pd)) pd else "layer", "_", .fig_deparse(l$call[[1L]]))))
        data_lines <<- c(data_lines, paste(as.character(sub), "<-", .fig_deparse_r(l$call[[l$i]])))
        line_const <<- c(line_const, "")
      }
      l$call[[l$i]] <- sub
      terms[[k]] <- l$call
    }
    terms
  }
  read_plot <- function(terms, nm = "content") {
    before <- unlist(lapply(terms, all.names))
    terms <- lapply(terms, function(t) .fig_prog_subst(t, consts))
    # (a name left, inside aes() or an object's part: its line stays)
    after <- unlist(lapply(terms, all.names))
    left <<- c(left, intersect(after, names(consts)))
    n_of <- function(x, k) sum(x == k)
    read_in <<- c(read_in, Filter(function(k) n_of(before, k) > n_of(after, k), names(consts)))
    # (a list of terms, NULL, if (flag) term: the terms ggplot2 adds)
    terms <- unlist(lapply(terms, .fig_prog_expand_term), recursive = FALSE)
    terms <- lift_data(terms, nm)
    lapply(terms, function(t) {
      # (a term of R but a call of a function, if (x) ...: R as it is)
      if (!is.call(t) || .fig_deparse(t[[1L]]) %in% c("if", "{", "(", "function", "[", "$")) {
        return(tfl_item(".r", list(code = tfl_r(.fig_deparse_r(t)))))
      }
      .fig_lang_item(t)
    })
  }
  if (!is.null(compose) && (is.null(shown) || !shown %in% names(plots))) {
    used <- .fig_prog_compose_plots(compose, plots)
    names_of <- stats::setNames(vapply(used, .fig_prog_plot_name, ""), used)
    content <- lapply(used, function(nm) tfl_plot(names_of[[nm]], read_plot(plots[[nm]], nm)))
    compose <- .fig_prog_subst(compose, consts)
    left <- c(left, intersect(all.names(compose), names(consts)))
    content <- c(content, .fig_prog_compose_items(compose, names_of))
  } else {
    nm <- shown %||% names(plots)[length(plots)]
    content <- read_plot(plots[[nm]], nm)
  }
  content <- c(content, list(image %||% tfl_item("tfl_image", list())))
  # (a plot function still called in the data part: its definition kept)
  if (length(plot_fns)) {
    own <- line_const %in% names(plot_fns)
    used <- unlist(lapply(data_lines[!own], function(l) {
      tryCatch(all.names(parse(text = l, keep.source = FALSE)), error = function(err) character())
    }))
    left <- c(left, intersect(names(plot_fns), used))
  }
  # an object of constants is the data part's no more, but where its name is left
  data_lines <- data_lines[!nzchar(line_const) | line_const %in% left]
  steps <- if (length(data_lines)) {
    list(list(step = "code", label = "the program's data", code = data_lines))
  } else {
    list()
  }
  out <- list(data = list(source = "code", steps = steps), content = content)
  attr(out, "objects") <- unique(c(read_in, if_flags, fns_used))
  probs <- lapply(seq_along(content), function(i) {
    its <- if (inherits(content[[i]], "tfl_plot")) content[[i]]$items else content[i]
    p <- do.call(rbind, lapply(its, function(it) {
      if (identical(it$fn, ".r")) return(NULL)
      tryCatch(tfl_schema_check(it), error = function(err) NULL)
    }))
    if (!is.null(p) && nrow(p)) cbind(item = i, p)
  })
  probs <- do.call(rbind, probs)
  attr(out, "problems") <- probs %||% data.frame(item = integer(), fn = character(),
                                                 arg = character(), level = character(),
                                                 message = character())
  out
}
