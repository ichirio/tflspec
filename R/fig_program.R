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

# a program's line that makes a plot: a `+` chain whose first term is a
# call of a plot-making function, or a plot of the program's (p + ...)
.fig_prog_plot_head <- function(e, plots) {
  terms <- .fig_split_plus(e)
  head <- terms[[1L]]
  # (p1 + p2: patchwork's arrangement of two plots, no plot's term)
  if (any(vapply(terms[-1L], function(t) !is.null(.fig_prog_compose_plots(t, plots)), NA))) {
    return(NULL)
  }
  if (is.name(head) && as.character(head) %in% names(plots)) return(as.character(head))
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
    rest <- a[nzchar(nm) & nm != "plots"]
    args <- c(list(plots = unname(names_of[syms])),
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
  add_plot <- function(nm, e) {
    head <- .fig_prog_plot_head(e, plots)
    terms <- .fig_split_plus(e)
    if (nzchar(head)) terms <- c(plots[[head]], terms[-1L])
    else if (!is.null(global_theme)) terms <- c(terms[1L], list(global_theme), terms[-1L])
    plots[[nm]] <<- terms
    shown <<- nm
  }
  for (e in as.list(ex)) {
    f <- if (is.call(e)) sub("^.*::", "", .fig_deparse(e[[1L]])) else ""
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
  read_plot <- function(terms) {
    before <- unlist(lapply(terms, all.names))
    terms <- lapply(terms, function(t) .fig_prog_subst(t, consts))
    # (a name left, inside aes() or an object's part: its line stays)
    after <- unlist(lapply(terms, all.names))
    left <<- c(left, intersect(after, names(consts)))
    n_of <- function(x, k) sum(x == k)
    read_in <<- c(read_in, Filter(function(k) n_of(before, k) > n_of(after, k), names(consts)))
    terms <- unlist(lapply(terms, .fig_split_plus), recursive = FALSE)
    lapply(terms, .fig_lang_item)
  }
  if (!is.null(compose) && (is.null(shown) || !shown %in% names(plots))) {
    used <- .fig_prog_compose_plots(compose, plots)
    names_of <- stats::setNames(vapply(used, .fig_prog_plot_name, ""), used)
    content <- lapply(used, function(nm) tfl_plot(names_of[[nm]], read_plot(plots[[nm]])))
    compose <- .fig_prog_subst(compose, consts)
    left <- c(left, intersect(all.names(compose), names(consts)))
    content <- c(content, .fig_prog_compose_items(compose, names_of))
  } else {
    content <- read_plot(plots[[shown %||% names(plots)[length(plots)]]])
  }
  content <- c(content, list(image %||% tfl_item("tfl_image", list())))
  # an object of constants is the data part's no more, but where its name is left
  data_lines <- data_lines[!nzchar(line_const) | line_const %in% left]
  steps <- if (length(data_lines)) {
    list(list(step = "code", label = "the program's data", code = data_lines))
  } else {
    list()
  }
  out <- list(data = list(source = "code", steps = steps), content = content)
  attr(out, "objects") <- unique(read_in)
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
