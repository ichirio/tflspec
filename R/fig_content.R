# ============================================================================
#  A figure's content: one item a function call, plot by plot
# ----------------------------------------------------------------------------
#  A figure report's `content:` is an ordered list of items, one a function
#  call (design: _archive/figure-designer-redesign-2026-10-15.md, 2):
#
#    content:
#      - ggplot: {data: sm, mapping: {x: AVISITN, y: mean, colour: TRTA}}
#      - geom_line: {position: {fn: position_dodge, args: {width: 0.6}}}
#      - labs: {x: Week}
#      - tfl_image: {dpi: 300}
#
#  The first item makes the plot (ggplot, ggsurvfit, ggcuminc, or one of
#  tflspec's own: tfl_risk_panel, tfl_text_column, tfl_n_panel); the items
#  after it are its layers and its settings, `+`-ed in their order.  A
#  figure of several plots names each one: `- km: [items]`, `- risk:
#  [items]` (a tfl_plot), then the patchwork items that arrange them
#  (wrap_plots, plot_layout, plot_annotation); `tfl_image` (the image's
#  size, dpi, format and file) is always last.  tflspec's own items are
#  written as the code they stand for: tfl_ard_number (a number of the
#  ARD as a text), and the three panels.
#
#  The figure designer's design (pieces, `plot:`) is the old form: read
#  and written as before until the designer is rebuilt on this one;
#  tfl_fig_content_from_design() converts a design once.
# ============================================================================

# the functions that make a plot: a plot's first item
.fig_p_fns <- c("ggplot", "ggsurvfit", "ggcuminc", "tfl_risk_panel", "tfl_text_column",
                "tfl_n_panel")
# the items that arrange the plots, after them
.fig_compose_fns <- c("wrap_plots", "plot_layout", "plot_annotation")
# tflspec's own items, written as the code they stand for
.fig_own_fns <- c("tfl_ard_number", "tfl_risk_panel", "tfl_text_column", "tfl_n_panel",
                  "tfl_image")

#' A plot of a figure's content: its name and its items
#'
#' A figure of several plots (KM curves and the number at risk under them,
#' a forest plot and its text column) gives each one a name and its items:
#' in a report file `- km: [items]`.  `tfl_plot()` is that entry.
#'
#' @param name The plot's name (`km`; the code calls it `p_km`).
#' @param items Its items ([tfl_item()]), the plot's first item first.
#' @param from Where it came from, as an item's.
#' @return A `tfl_plot`: a [tfl_item()] whose `fn` is the name, with
#'   `items`.
#' @examples
#' tfl_plot("risk", list(tfl_item("tfl_risk_panel", list(fit = "fit"))))
#' @export
tfl_plot <- function(name, items = list(), from = "own") {
  if (!is.character(name) || length(name) != 1L || !grepl("^[A-Za-z][A-Za-z0-9_]*$", name)) {
    stop("tfl_plot(): `name` is a plot's name: a letter, then letters, digits or _.", call. = FALSE)
  }
  x <- tfl_item(name, list(), from)
  x$items <- items
  class(x) <- c("tfl_plot", class(x))
  x
}

# Is a figure's content of this form (its first item a plot-making
# function, a named plot, or R), not the designer's pieces?
.fig_new_content <- function(content) {
  if (inherits(content, "tfl_r") || !length(content)) return(FALSE)
  first <- content[[1L]]
  if (inherits(first, "tfl_plot")) return(TRUE)
  fn <- .schema_fn_name(first$fn %||% "")
  fn %in% .fig_p_fns || identical(fn, ".r")
}

# The content as plots (each a name and its items), then the items that
# arrange them, then the image's: a content of one plot is the plot
# `content`.  Stops on a content that mixes a plot's items with named
# plots, a plot whose first item makes no plot, a plot named as a
# function.
.fig_split_content <- function(content) {
  is_plot <- vapply(content, inherits, NA, "tfl_plot")
  fns <- vapply(content, function(it) .schema_fn_name(it$fn %||% ""), "")
  # after the named plots: what arranges them -- patchwork's items, and any
  # item said with its operator (`op: "&"`, a theme for every plot)
  has_op <- vapply(content, function(it) !is.null(it$args$op), NA)
  tail <- !is_plot & (fns %in% c(.fig_compose_fns, "tfl_image") | (any(is_plot) & has_op))
  if (any(is_plot)) {
    if (any(!is_plot & !tail)) {
      .ard_stop(paste0("A figure's content is its items, or its named plots (- km: [items]) ",
                       "and what arranges them -- not both: ",
                       paste(unique(fns[!is_plot & !tail]), collapse = ", "), "."))
    }
    plots <- lapply(content[is_plot], function(p) list(name = p$fn, items = p$items))
  } else {
    plots <- list(list(name = NULL, items = content[!tail]))
  }
  for (p in plots) {
    if (!is.null(p$name) && (p$name %in% c(.fig_p_fns, .fig_compose_fns) ||
                             exists(p$name, envir = asNamespace("ggplot2"), inherits = FALSE))) {
      .ard_stop(sprintf("A figure's plot is named `%s`, a function's name: name it otherwise.", p$name))
    }
    f <- if (length(p$items)) .schema_fn_name(p$items[[1L]]$fn %||% "") else ""
    if (!f %in% c(.fig_p_fns, ".r")) {
      .ard_stop(sprintf(paste0("A figure's plot%s starts with the function that makes it ",
                               "(%s, or !r), not `%s`."),
                        if (is.null(p$name)) "" else paste0(" `", p$name, "`"),
                        paste(.fig_p_fns, collapse = ", "), f))
    }
  }
  img <- content[tail & fns == "tfl_image"]
  if (length(img) && !identical(tail(fns, 1L), "tfl_image")) {
    .ard_stop("A figure's tfl_image item is its last.")
  }
  list(plots = plots, compose = content[tail & fns != "tfl_image"],
       image = if (length(img)) img[[1L]]$args %||% list() else list())
}

# ---- an item <-> its call --------------------------------------------------

# the arguments that name an object of the figure's data (written bare)
.fig_object_args <- c("data", "fit")

# the arguments of `fn` that name an object of the figure's data: the
# schema's of type `object` (ggsurvfit's x), and data / fit
.fig_objects_of <- function(fn) {
  ent <- tfl_schema(fn)
  own <- names(Filter(function(a) identical(a$type, "object"), ent$args %||% list()))
  unique(c(.fig_object_args, own))
}

# The types an item's arguments are written and read by (parsed), by name:
# its function's card, with the stat or geom it names (fun.data of
# geom_pointrange(stat = "summary")); only the arguments given
.fig_arg_types <- function(fn, args = list()) {
  nms <- names(args) %||% character()
  if (!length(nms) || !is.character(fn) || length(fn) != 1L) return(list())
  # (the stat or geom an item names is read from its text)
  given <- lapply(args, function(x) if (is.character(x) && length(x) == 1L) x)
  ent <- tryCatch(.schema_item_entry(tfl_schema(fn), fn, given), error = function(e) NULL)
  if (is.null(ent)) return(list())
  out <- lapply(stats::setNames(nms, nms), function(k) {
    t <- ent$args[[k]]$type
    if (!is.null(t)) .schema_type(t)
  })
  Filter(Negate(is.null), out)
}

# a type's alternatives (a type of one kind is its one)
.fig_type_alts <- function(ty) if (identical(ty$kind, "either")) ty$args else list(ty)

# A text value of a type that is code: a function by its name, written
# bare (fun.data = mean_se, Hmisc::smean.cl.normal), a formula as its text
# written as the formula (formula = y ~ x); NULL for text written as text
# -- the first of the type's alternatives the value has decides (method:
# "lm" of string|r stays "lm")
.fig_text_lang <- function(v, ty) {
  for (t in .fig_type_alts(ty)) {
    if (!.schema_ok(v, t)) next
    if (identical(t$kind, "function")) {
      if (!grepl("::", v, fixed = TRUE)) return(as.name(v))
      return(call("::", as.name(sub("::.*$", "", v)), as.name(sub("^.*::", "", v))))
    }
    if (identical(t$kind, "tilde")) return(str2lang(v))
    return(NULL)
  }
  NULL
}

# whether a type takes a call of `f` by name (call<label_*>: scales'
# label_percent); a call<> of no names takes no other package's
.fig_type_takes_call <- function(ty, f) {
  if (is.null(ty)) return(FALSE)
  any(vapply(.fig_type_alts(ty), function(t) {
    identical(t$kind, "call") && length(t$args) > 0L && .schema_call_named(f, unlist(t$args))
  }, NA))
}

.fig_type_has <- function(ty, kind) {
  !is.null(ty) && any(vapply(.fig_type_alts(ty), function(t) identical(t$kind, kind), NA))
}

# A value of an item as the R it stands for (a language object); `type`,
# the argument's (parsed), says a text that is code (.fig_text_lang())
.fig_value_lang <- function(v, arg = "", objects = .fig_object_args, type = NULL) {
  if (inherits(v, "tfl_unset")) {
    warning(sprintf(paste0("A figure's `%s` is !unset, which removes a key in the layering; ",
                           "NULL is !r NULL: written as NULL."), arg), call. = FALSE)
    return(NULL)
  }
  if (inherits(v, "tfl_r")) return(str2lang(as.character(unclass(v))))
  if (inherits(v, "tfl_call")) {
    ty <- .fig_arg_types(v$fn, v$args %||% list())
    a <- lapply(stats::setNames(names(v$args %||% list()), names(v$args %||% list())),
                function(k) .fig_value_lang(v$args[[k]], k, type = ty[[k]]))
    # unit(0.5, "cm"), rel(1.2): their leading arguments by position, as
    # they are written
    lead <- .fig_by_position[[v$fn]]
    if (!is.null(lead)) {
      n <- 0L
      while (n < length(a) && n < length(lead) && identical(names(a)[n + 1L], lead[n + 1L])) n <- n + 1L
      names(a)[seq_len(n)] <- ""
    }
    return(as.call(c(list(.fig_fn_sym(v$fn)), a)))
  }
  if (identical(arg, "mapping") && is.list(v)) {
    # a column by its name, an expression of a mapping's form (factor(AVISITN),
    # after_stat(density)) as it is written (R/fig_aes.R)
    a <- lapply(v, function(e) if (inherits(e, "tfl_r")) str2lang(as.character(unclass(e)))
                               else if (is.numeric(e) || is.logical(e)) {
                                 if (is.integer(e)) as.numeric(e[[1L]]) else e[[1L]]
                               }
                               else .fig_aes_lang(e) %||% as.name(e))
    return(as.call(c(list(as.name("aes")), a)))
  }
  if (arg %in% objects && is.character(v) && length(v) == 1L) return(as.name(v))
  if (!is.null(type) && is.character(v) && length(v) == 1L && is.null(names(v))) {
    w <- .fig_text_lang(v, type)
    if (!is.null(w)) return(w)
  }
  # dates of a date scale (limits: [2024-01-01, NA]): as.Date(c(...))
  if (.fig_type_dates(type) && .fig_is_dates(v)) {
    d <- lapply(as.list(v), function(e) if (.fig_is_na_end(e)) NA else as.character(e))
    inner <- if (length(d) == 1L) d[[1L]] else as.call(c(list(as.name("c")), d))
    return(call("as.Date", inner))
  }
  # an argument R takes as a list (fun.args = list(mult = 1.96))
  if (arg %in% .fig_list_args && is.list(v)) {
    el <- lapply(seq_along(v), function(i) .fig_value_lang(v[[i]], names(v)[i] %||% ""))
    names(el) <- names(v)
    return(as.call(c(list(as.name("list")), el)))
  }
  if (is.list(v)) {
    el <- lapply(v, .fig_value_lang)
    return(as.call(c(list(as.name("c")), el)))
  }
  # (a whole number read from YAML is an integer: written as R writes 16)
  if (is.integer(v)) v <- as.numeric(v)
  if (length(v) == 1L && is.null(names(v))) return(v)
  as.call(c(list(as.name("c")), as.list(v)))
}

# whether a type takes dates: a sequence whose elements may be a `date`
.fig_type_dates <- function(ty) {
  !is.null(ty) && any(vapply(.fig_type_alts(ty), function(t) {
    identical(t$kind, "list") && .schema_has(t$args[[1L]], "date")
  }, NA))
}

# a value that is dates: a sequence of dates as their text (2024-01-01) and
# NA, one date at least, no names
.fig_is_dates <- function(v) {
  if (!is.null(names(v)) || !length(v) || inherits(v, c("tfl_r", "tfl_call"))) return(FALSE)
  el <- as.list(v)
  if (!all(vapply(el, function(e) length(e) == 1L, NA))) return(FALSE)
  is_na <- vapply(el, .fig_is_na_end, NA)
  any(!is_na) && all(vapply(el[!is_na], .schema_is_date, NA))
}

# an open end of a range: NA, or YAML's NA read as the text "NA"
.fig_is_na_end <- function(e) is.atomic(e) && length(e) == 1L && (is.na(e) || identical(e, "NA"))

# the functions whose leading arguments are written by position (when given
# first, in this order): unit(0.5, "cm"), rel(1.2), seq(0, 52, by = 4),
# alpha("grey50", 0.4), hcl.colors(3, "Dark 3")
.fig_by_position <- list(unit = c("x", "units"), rel = "x", seq = c("from", "to"),
                         alpha = c("colour", "alpha"), hcl.colors = c("n", "palette"),
                         palette.colors = c("n", "palette"), gray.colors = "n",
                         grey.colors = "n", rainbow = "n", heat.colors = "n",
                         terrain.colors = "n", topo.colors = "n", cm.colors = "n")

# the arguments R takes as a list, not a vector: a map of the spec is
# written list(...) and read back from it
.fig_list_args <- c("fun.args", "method.args", "args", "dparams", "override.aes",
                    "x_scales", "y_scales", "stats_label")

# a function as the code calls it: bare for the packages the figure's
# program attaches (ggplot2, ggsurvfit, patchwork) and tflspec's own,
# pkg::fn for another package's (the schema says which: ggrepel's
# geom_text_repel)
.fig_fn_sym <- function(fn) {
  pkg <- if (grepl("::", fn, fixed = TRUE)) sub("::.*$", "", fn) else .fig_fn_package(fn)
  nm <- sub("^.*::", "", fn)
  if (!is.null(pkg) && !pkg %in% .fig_attached) return(call("::", as.name(pkg), as.name(nm)))
  as.name(nm)
}

.fig_attached <- c("ggplot2", "ggsurvfit", "patchwork", "tflspec", "dplyr")

.fig_fn_package <- function(fn) {
  ent <- .schema_all(figure = FALSE)[[fn]]
  if (is.null(ent$figure)) return(NULL)
  ent$package
}

# An item as its call; a card's positional `...` (xlim's limits) written
# by position, xlim(0, 52)
.fig_item_lang <- function(it) {
  a <- it$args %||% list()
  obj <- .fig_objects_of(it$fn)
  ty <- .fig_arg_types(it$fn, a)
  args <- lapply(stats::setNames(names(a), names(a)), function(k) .fig_value_lang(a[[k]], k, obj, ty[[k]]))
  # facet_grid()'s rows and cols by their columns: vars(SEX), as ggplot2
  # takes them together (not "SEX")
  if (identical(it$fn, "facet_grid")) {
    for (k in intersect(c("rows", "cols"), names(a))) {
      v <- a[[k]]
      if (is.character(v) || (is.list(v) && !is.object(v) && all(vapply(v, is.character, NA)))) {
        args[[k]] <- as.call(c(list(as.name("vars")), lapply(unlist(v), as.name)))
      }
    }
  }
  # facet_wrap()'s facets: a column that is not syntactic (`A b`) in vars(),
  # as facet_grid() takes it (ggplot2 parses a string: "A b" fails); the
  # syntactic ones are written as they are
  if (identical(it$fn, "facet_wrap") && !is.null(a$facets) && !inherits(a$facets, c("tfl_r", "tfl_call"))) {
    v <- a$facets
    if (is.null(names(v)) && (is.character(v) || (is.list(v) && all(vapply(v, is.character, NA))))) {
      w <- .fig_facet_vars(unlist(v))
      if (!is.null(w)) args$facets <- w
    }
  }
  key <- .fig_positional_key(it$fn)
  if (!is.null(key) && key %in% names(a) && !inherits(a[[key]], c("tfl_r", "tfl_call"))) {
    i <- match(key, names(args))
    pos <- lapply(unname(as.list(a[[key]])), .fig_value_lang, arg = key, objects = obj)
    args <- c(args[seq_len(i - 1L)], pos, args[-seq_len(i)])
  }
  as.call(c(list(.fig_fn_sym(it$fn)), args))
}

# facet_wrap()'s facets (the texts of a SPEC) as vars(), when one is a column
# that is not syntactic: a name in backticks (`A b`), or a text that does not
# parse ("A b"); NULL when none is (they are written as before).  A text
# that parses as another expression (AGE > 65) is that expression in vars();
# a formula text (~ A + B) is not for vars(): NULL.
.fig_facet_vars <- function(txt) {
  if (anyNA(txt) || !all(nzchar(txt))) return(NULL)
  need <- FALSE
  parts <- list()
  for (t in txt) {
    e <- tryCatch(str2lang(t), error = function(err) NULL)
    if (is.null(e) && !identical(t, "NULL")) {
      e <- as.name(t)
      need <- TRUE
    } else if (is.name(e)) {
      need <- need || make.names(as.character(e)) != as.character(e)
    } else if (is.call(e) && identical(e[[1L]], as.name("~"))) {
      return(NULL)
    }
    parts <- c(parts, list(e))
  }
  if (need) as.call(c(list(as.name("vars")), parts))
}

# the key a figure function's card names its positional `...` by (xlim:
# limits), or NULL
.fig_positional_key <- function(fn) {
  dots <- .schema_all(figure = FALSE)[[.schema_fn_name(fn)]]$dots
  if (identical(dots$style, "positional")) dots$key
}

.fig_deparse <- function(e) paste(deparse(e, width.cutoff = 500L), collapse = " ")

# An expression as the text of its !r: on one line, or on its lines when
# one line is not the same R (a function's body of several statements,
# quote({ a <- 1; b }))
.fig_deparse_r <- function(e) {
  one <- .fig_deparse(e)
  back <- tryCatch(str2lang(one), error = function(err) NULL)
  if (!is.null(back) && identical(back, e)) return(one)
  paste(deparse(e, width.cutoff = 500L), collapse = "\n")
}

# A call as the code writes it: on one line when it fits, else one
# argument a line
.fig_term_text <- function(e, width = 76L) {
  one <- .fig_deparse(e)
  if (nchar(one) <= width || !is.call(e)) return(one)
  a <- as.list(e)[-1L]
  nm <- names(a) %||% rep("", length(a))
  parts <- vapply(seq_along(a), function(i) {
    v <- .fig_deparse(a[[i]])
    if (nzchar(nm[i])) paste(nm[i], "=", v) else v
  }, "")
  paste0(.fig_deparse(e[[1L]]), "(\n  ", paste(parts, collapse = ",\n  "), "\n)")
}

# A call's argument as an item's value: a constant as it is, a column of
# aes() by its name, an object (data, fit) by its name, a constructor of
# constants (position_dodge, element_text ...) as {fn:, args:}, anything
# else as R (!r).  `type`, the argument's (parsed), reads code as a typed
# value: a function by its name as the name (fun = mean: "mean"), a
# formula as its text (formula = y ~ x: "y ~ x"), a call of another
# package the type names as {fn:, args:} (labels =
# scales::label_percent()).
.fig_lang_value <- function(x, arg = "", objects = .fig_object_args, type = NULL) {
  if (is.null(x)) return(tfl_r("NULL"))
  if (is.atomic(x) && length(x) == 1L) return(x)
  if (is.name(x)) {
    if (arg %in% objects) return(as.character(x))
    nm <- as.character(x)
    if (.fig_type_has(type, "function") && .schema_is_fun_name(nm)) return(nm)
    # a smooth's method by its function (method = lm) is its name, as
    # ggplot2 takes it; a legend key's draw_key_rect its "rect"
    if (identical(arg, "method") && nm %in% c("lm", "glm", "loess")) return(nm)
    if (identical(arg, "key_glyph") && startsWith(nm, "draw_key_")) return(sub("^draw_key_", "", nm))
    return(tfl_r(nm))
  }
  if (is.call(x)) {
    f <- if (is.name(x[[1L]])) as.character(x[[1L]]) else .fig_deparse(x[[1L]])
    a <- as.list(x)[-1L]
    # Hmisc::smean.cl.normal, a function by its name
    if (identical(f, "::") && .fig_type_has(type, "function") && .schema_is_fun_name(.fig_deparse(x))) {
      return(.fig_deparse(x))
    }
    if (identical(f, "~") && .fig_type_has(type, "tilde")) {
      txt <- .fig_deparse(x)
      if (.schema_is_tilde(txt) && identical(str2lang(txt), x)) return(txt)
    }
    # a date scale's dates: as.Date("2024-01-01"), as.Date(c("2024-01-01", NA))
    if (identical(f, "as.Date") && length(a) == 1L && .fig_type_dates(type)) {
      d <- a[[1L]]
      if (is.call(d) && identical(d[[1L]], as.name("c"))) d <- as.list(d)[-1L]
      d <- lapply(as.list(d), function(e) if (is.atomic(e) && length(e) == 1L && is.na(e)) NA else e)
      if (length(d) && is.null(names(d)) && .fig_is_dates(d)) return(unlist(d))
    }
    if (identical(f, "aes") && identical(arg, "mapping")) {
      # aes(displ, hwy): the first two unnamed are x and y, as aes() takes them
      nm <- names(a) %||% rep("", length(a))
      un <- which(!nzchar(nm))
      if (length(un) <= 2L) nm[un] <- c("x", "y")[seq_along(un)]
      if (any(!nzchar(nm)) || anyDuplicated(nm)) return(tfl_r(.fig_deparse_r(x)))
      names(a) <- nm
      out <- lapply(a, function(e) {
        # (a name that is not syntactic keeps its backticks: `A-B`)
        if (is.name(e)) return(.fig_col_text(as.character(e)))
        # a constant of a number (group = 1): the number; a text in its
        # quotes (colour = "Overall": '"Overall"')
        if ((is.numeric(e) || is.logical(e)) && length(e) == 1L) return(e)
        .fig_aes_text(e) %||% tfl_r(.fig_deparse_r(e))
      })
      return(out)
    }
    if (identical(f, "list") && arg %in% .fig_list_args && length(a) &&
        all(nzchar(names(a) %||% rep("", length(a))))) {
      vals <- lapply(stats::setNames(names(a), names(a)), function(k) .fig_lang_value(a[[k]], k))
      if (!any(vapply(vals, inherits, NA, "tfl_r"))) return(vals)
    }
    # a negative number, a vector of constants
    if (identical(f, "-") && length(a) == 1L && is.numeric(a[[1L]])) return(-a[[1L]])
    if (identical(f, "c") && length(a) && is.null(names(a)) &&
        all(vapply(a, function(e) is.atomic(e) && length(e) == 1L, NA))) {
      return(unlist(a))
    }
    # a named vector of constants (values = c(A = "red", B = "blue")) as a
    # map, which is written back c(...)
    if (identical(f, "c") && length(a) && all(nzchar(names(a) %||% "")) && !anyDuplicated(names(a)) &&
        all(vapply(a, function(e) is.atomic(e) && length(e) == 1L && !is.null(e), NA)) &&
        !arg %in% .fig_list_args) {
      return(lapply(a, identity))
    }
    # 1:5 (whole numbers): the numbers
    if (identical(f, ":") && all(vapply(a, function(e) is.numeric(e) && length(e) == 1L &&
                                         e == round(e), NA)) && abs(a[[2L]] - a[[1L]]) < 100) {
      return(as.numeric(seq(a[[1L]], a[[2L]])))
    }
    # a constructor of a plot's value (a position, a guide, an element,
    # theme(), unit(), rel(), sec_axis() ...): {fn:, args:}, its positional
    # arguments named by its formals, an argument that is no constant as R
    # in it
    # (scales' percent_format() bare, the package attached: scales::)
    fq <- .fig_scales_name(f)
    if (.fig_reads_as_call(f) || .fig_type_takes_call(type, fq)) {
      named <- .fig_name_args(x, .fig_find_value_fun(fq))
      a <- as.list(named)[-1L]
      if (all(nzchar(names(a) %||% rep("", length(a))))) {
        ty <- .fig_arg_types(fq, lapply(a, function(e) if (is.character(e)) e))
        vals <- lapply(stats::setNames(names(a), names(a)),
                       function(k) .fig_lang_value(a[[k]], k, type = ty[[k]]))
        return(tfl_call(fq, vals))
      }
    }
  }
  tfl_r(.fig_deparse_r(x))
}

# a function of scales called bare (percent_format(), the package
# attached) by its pkg::name, as the code writes it; any other as it is
.fig_scales_name <- function(f) {
  if (grepl("::", f, fixed = TRUE) || !requireNamespace("scales", quietly = TRUE) ||
      !f %in% getNamespaceExports("scales")) return(f)
  if (requireNamespace("ggplot2", quietly = TRUE) && f %in% getNamespaceExports("ggplot2")) return(f)
  paste0("scales::", f)
}

# whether a call in an argument is read as {fn:, args:}: the constructors
# below, and ggplot2's functions of a guide, a position, an element, a
# theme and the helpers (expansion(), sec_axis(), margin_auto(), unit(),
# labeller() ...); not vars() (its columns are no arguments)
.fig_reads_as_call <- function(f) {
  if (f %in% .fig_constructors) return(TRUE)
  if (identical(f, "vars") || grepl("::", f, fixed = TRUE)) return(FALSE)
  k <- .schema_gg_kind(f)
  !is.na(k) && k %in% c("guide", "position", "element", "theme", "helper") &&
    requireNamespace("ggplot2", quietly = TRUE) && f %in% getNamespaceExports("ggplot2")
}

# A call with its positional arguments named by the function's formals, in
# the order written; as it is when `fun` is no closure or cannot name them
.fig_name_args <- function(e, fun) {
  given <- names(as.list(e)[-1L]) %||% rep("", length(e) - 1L)
  if (is.null(fun) || !is.function(fun) || is.primitive(fun)) return(e)
  m <- tryCatch(match.call(fun, e, expand.dots = TRUE), error = function(err) NULL)
  if (is.null(m)) return(e)
  # match.call() puts them in the formals' order and names the positional
  # ones -- those (not named as given) take the positional places, in
  # their order
  mn <- names(as.list(m)[-1L])
  pos <- setdiff(mn, given[nzchar(given)])
  ord <- given
  if (length(pos) >= sum(!nzchar(ord))) ord[!nzchar(ord)] <- pos[seq_len(sum(!nzchar(ord)))]
  if (all(nzchar(ord)) && !anyNA(ord) && setequal(ord, mn)) {
    return(as.call(c(list(m[[1L]]), as.list(m)[-1L][ord])))
  }
  m
}

# the constructors an argument is written as {fn:, args:}
.fig_constructors <- c("position_dodge", "position_dodge2", "position_jitter",
                       "position_jitterdodge", "position_nudge", "position_stack",
                       "position_fill", "position_identity",
                       "element_text", "element_line", "element_rect", "element_blank",
                       "margin", "unit", "expansion", "dup_axis", "guide_legend",
                       "guide_none", "guide_colourbar", "guide_colorbar", "guide_axis",
                       "guide_bins", "guide_coloursteps",
                       "arrow", "rel", "element_markdown", "element_textbox_simple")

# A term of a ggplot chain (a call) as an item: its function, its
# arguments by name (positional ones named by the function's formals)
.fig_lang_item <- function(e, env = NULL) {
  f <- .fig_deparse(e[[1L]])
  fun <- .fig_find_fun(f, env)
  e <- .fig_name_args(e, fun)
  a <- as.list(e)[-1L]
  nm <- names(a) %||% rep("", length(a))
  # an aes() given first, unnamed (geom_line(aes(...)))
  for (i in which(!nzchar(nm))) {
    if (is.call(a[[i]]) && identical(.fig_deparse(a[[i]][[1L]]), "aes")) nm[i] <- "mapping"
  }
  # a card's positional `...` (xlim(0, 52)): its key, when they are constants
  key <- .fig_positional_key(f)
  if (any(!nzchar(nm)) && !is.null(key) && !key %in% nm) {
    pos <- lapply(a[!nzchar(nm)], .fig_lang_value, arg = key)
    if (all(vapply(pos, function(v) is.atomic(v) && length(v) == 1L && !is.object(v), NA))) {
      a <- c(a[nzchar(nm)], stats::setNames(list(as.call(c(list(as.name("c")), unname(pos)))), key))
      nm <- names(a)
    }
  }
  if (any(!nzchar(nm))) {
    # an argument the function's formals cannot name: the whole call as R
    return(tfl_item(".r", list(code = tfl_r(.fig_deparse_r(e)))))
  }
  names(a) <- nm
  obj <- .fig_objects_of(f)
  ty <- .fig_arg_types(f, lapply(a, function(e) if (is.character(e)) e))
  args <- lapply(stats::setNames(nm, nm), function(k) .fig_lang_value(a[[k]], k, obj, ty[[k]]))
  if (f %in% c("facet_wrap", "facet_grid")) args <- .fig_facet_args(f, a, args)
  tfl_item(f, args)
}

# A facet's variables as its columns, the SPEC's form (facets: PARAMCD,
# rows: [SEX], cols: [TRTA]): facet_wrap(~PARAMCD), facet_wrap(vars(A, B)),
# facet_grid(SEX ~ TRTA), facet_grid(rows = vars(SEX)), . for none.  Any
# other (an expression of a column) stays R.
.fig_facet_args <- function(f, a, args) {
  # (facet_wrap()'s column that is not syntactic keeps its backticks: `A b`)
  col_text <- if (identical(f, "facet_wrap")) .fig_col_text else identity
  cols_of <- function(x) {
    if (is.name(x)) return(if (identical(x, as.name("."))) character() else col_text(as.character(x)))
    if (!is.call(x)) return(NULL)
    op <- .fig_deparse(x[[1L]])
    if (identical(op, "+") && length(x) == 3L) {
      l <- cols_of(x[[2L]])
      r <- cols_of(x[[3L]])
      return(if (is.null(l) || is.null(r)) NULL else c(l, r))
    }
    if (identical(op, "vars") && length(x) > 1L && is.null(names(x)) &&
        all(vapply(as.list(x)[-1L], is.name, NA))) {
      return(vapply(as.list(x)[-1L], function(n) col_text(as.character(n)), ""))
    }
    NULL
  }
  as_cols <- function(v) if (length(v) == 1L) v else as.list(v)
  one_sided <- function(x) is.call(x) && identical(x[[1L]], as.name("~")) && length(x) == 2L
  two_sided <- function(x) is.call(x) && identical(x[[1L]], as.name("~")) && length(x) == 3L
  if (identical(f, "facet_wrap") && !is.null(a$facets)) {
    x <- if (one_sided(a$facets)) a$facets[[2L]] else a$facets
    v <- if (is.call(x) || one_sided(a$facets)) cols_of(x)
    if (length(v)) args$facets <- as_cols(v)
    return(args)
  }
  if (identical(f, "facet_grid")) {
    if (two_sided(a$rows) && is.null(a$cols)) {
      r <- cols_of(a$rows[[2L]])
      cc <- cols_of(a$rows[[3L]])
      if (is.null(r) || is.null(cc) || !length(c(r, cc))) return(args)
      out <- args[setdiff(names(args), "rows")]
      i <- match("rows", names(args))
      new <- c(if (length(r)) list(rows = as_cols(r)), if (length(cc)) list(cols = as_cols(cc)))
      return(c(out[seq_len(i - 1L)], new, out[seq_along(out) >= i]))
    }
    for (k in intersect(c("rows", "cols"), names(a))) {
      v <- if (is.call(a[[k]]) && identical(a[[k]][[1L]], as.name("vars"))) cols_of(a[[k]])
      if (length(v)) args[[k]] <- as_cols(v)
    }
  }
  args
}

.fig_find_fun <- function(f, env = NULL) {
  if (grepl("::", f, fixed = TRUE)) {
    return(tryCatch(getExportedValue(sub("::.*$", "", f), sub("^.*::", "", f)),
                    error = function(e) NULL))
  }
  if (!is.null(env) && exists(f, envir = env, mode = "function")) return(get(f, envir = env, mode = "function"))
  for (pkg in c("ggplot2", "ggsurvfit", "patchwork", "scales")) {
    # (an export of the package or one it passes on: ggplot2's unit(), grid's)
    if (requireNamespace(pkg, quietly = TRUE) &&
        (exists(f, envir = asNamespace(pkg), inherits = FALSE) || f %in% getNamespaceExports(pkg))) {
      return(getExportedValue(pkg, f))
    }
  }
  NULL
}

# The function a value's call is of: as .fig_find_fun(), and base R's that
# a value is a call of (seq(0, 52, by = 4), as.Date(), grDevices' palettes
# hcl.colors(3, "Dark 3")) -- seq()'s arguments its default method's
.fig_find_value_fun <- function(f) {
  fun <- .fig_find_fun(f)
  if (!is.null(fun)) return(fun)
  if (identical(f, "seq")) return(seq.default)
  for (pkg in c("base", "grDevices")) {
    if (exists(f, envir = asNamespace(pkg), inherits = FALSE)) {
      fun <- get(f, envir = asNamespace(pkg))
      if (is.function(fun)) return(fun)
    }
  }
  NULL
}

# ---- the code ---------------------------------------------------------------

# The figure's code from its content: the data part (its steps, as the
# designer writes them), then each plot (p_<name>, or `name` for one plot)
# its items `+`-ed in order, then the arrangement into `name`.  The study's
# figure setup is around it (theme_tfl(), scale_colour_tfl(),
# tfl_colours()).  Returns the lines, with the attributes of
# tfl_fig_design_code()'s (reads, libs, needs, ard).
.fig_content_code <- function(output, name = "content") {
  parts <- .fig_split_content(output$content)
  lv <- .codelist_levels(.codelists_rows(output$codelists, output$id), output$id)
  d <- .fig_steps_code(output$data$steps %||% list(), lv)
  head <- if (length(lv)) .codelists_head(lv)
  plots <- parts$plots
  one <- length(plots) == 1L
  pre <- character()
  body <- character()
  libs <- c("dplyr", "ggplot2", d$libs)
  needs <- character()
  ard <- any(vapply(output$data$steps %||% list(), function(s) identical(s$step, "ard_stats"), NA))
  k_ard <- 0L
  for (p in plots) {
    target <- if (one) name else paste0("p_", p$name)
    first <- p$items[[1L]]
    f1 <- .schema_fn_name(first$fn)
    lines <- character()
    if (f1 %in% c("tfl_risk_panel", "tfl_text_column", "tfl_n_panel")) {
      own <- .fig_own_panel(first, target)
      pre <- c(pre, own$data)
      lines <- own$code
      libs <- c(libs, own$libs)
    } else {
      terms <- character()
      for (it in p$items) {
        fn <- .schema_fn_name(it$fn)
        if (identical(fn, ".r")) {
          terms <- c(terms, as.character(it$args$code))
          next
        }
        if (identical(fn, "tfl_ard_number")) {
          k_ard <- k_ard + 1L
          own <- .fig_own_ard_number(it, k_ard)
          pre <- c(pre, own$pre)
          terms <- c(terms, own$term)
          ard <- TRUE
          next
        }
        terms <- c(terms, .fig_term_text(.fig_item_lang(it)))
        pk <- if (grepl("::", it$fn, fixed = TRUE)) sub("::.*$", "", it$fn) else .fig_fn_package(fn)
        if (!is.null(pk) && !pk %in% .fig_attached) needs <- c(needs, pk)
        if (fn %in% c("ggsurvfit", "ggcuminc") || startsWith(fn, "add_") || identical(pk, "ggsurvfit")) {
          libs <- c(libs, "ggsurvfit")
        }
        # patchwork's inside one plot (p + inset_element(...))
        if (identical(pk, "patchwork")) libs <- c(libs, "patchwork")
      }
      lines <- .fig_plus_chain(target, terms)
    }
    body <- c(body, lines, "")
  }
  if (!one) {
    libs <- c(libs, "patchwork")
    body <- c(body, .fig_compose_lines(name, plots, parts$compose))
  }
  code <- c(
    section("data"),
    head, d$code,
    if (length(pre)) c(pre, ""),
    section("plot"),
    .fig_factor_dodge(body),
    .fig_image_save(output, parts$image, name))
  code <- .code_lines(code)
  design_like <- list(data = output$data$steps %||% list())
  code <- .fig_rename_df(code, tfl_fig_data_name(design_like))
  code <- .fig_tidy_names(code)
  needs <- setdiff(unique(needs), c("ggplot2", "ggsurvfit", "patchwork"))
  .fig_code_out(c(.fig_units_used(code), code), d$reads, unique(libs), needs, ard = ard)
}

# `target <- a +` then each term on its line
.fig_plus_chain <- function(target, terms) {
  if (!length(terms)) return(paste(target, "<- ggplot()"))
  ind <- function(t) gsub("\n", "\n  ", t, fixed = TRUE)
  t <- vapply(terms, ind, "")
  n <- length(t)
  c(paste0(target, " <- ", t[1L], if (n > 1L) " +"),
    if (n > 1L) paste0("  ", t[-1L], c(rep(" +", n - 2L), "")))
}

# the plots into one: wrap_plots() when the content says it, else one
# under the other; then the arrangement's other items, `+`-ed
.fig_compose_lines <- function(name, plots, compose) {
  syms <- paste0("p_", vapply(plots, `[[`, "", "name"))
  fns <- vapply(compose, function(it) .schema_fn_name(it$fn), "")
  w <- compose[fns == "wrap_plots"]
  first <- if (length(w)) {
    a <- w[[1L]]$args %||% list()
    pl <- if (!is.null(a$plots)) paste0("p_", unlist(a$plots)) else syms
    a$plots <- NULL
    rest <- if (length(a)) .fig_deparse(.fig_item_lang(tfl_item("f", a)))
    rest <- if (!is.null(rest)) sub("^f\\(", "", sub("\\)$", "", rest))
    sprintf("wrap_plots(%s)", paste(c(pl, rest[nzchar(rest %||% "")]), collapse = ", "))
  } else paste(syms, collapse = " / ")
  others <- compose[fns != "wrap_plots"]
  if (!length(others)) return(.fig_plus_chain(name, first))
  # each item `+`-ed, or `&`-ed when it says so (op: "&": a theme for every plot)
  ops <- vapply(others, function(it) if (identical(it$args$op, "&")) "&" else "+", "")
  terms <- vapply(others, function(it) {
    it$args$op <- NULL
    .fig_deparse(.fig_item_lang(it))
  }, "")
  c(paste0(name, " <- ", first, " ", ops[1L]),
    paste0("  ", terms, c(paste0(" ", ops[-1L]), "")))
}

# a position_dodge() said twice or more: once, as `pd`
.fig_factor_dodge <- function(lines) {
  txt <- paste(lines, collapse = "\n")
  m <- regmatches(txt, gregexpr("position_dodge\\(width = [0-9.]+\\)", txt))[[1L]]
  if (length(m) < 2L || length(unique(m)) != 1L) return(lines)
  txt <- gsub(m[1L], "pd", txt, fixed = TRUE)
  c(paste("pd <-", m[1L]), "", strsplit(txt, "\n", fixed = TRUE)[[1L]])
}

# tflspec's own items, through the designer's code for the same piece: a
# number of the ARD (its text in the data part, ard_label_<k>) ...
.fig_own_ard_number <- function(it, k) {
  l <- c(list(layer = "ard_number"), .fig_args_plain(it$args))
  x <- .fig_layer_code(l, k, list())
  pre <- gsub(sprintf(".ARDLABEL%d.", k), paste0("ard_label_", k), x$pre, fixed = TRUE)
  code <- gsub(sprintf(".ARDLABEL%d.", k), paste0("ard_label_", k), x$code, fixed = TRUE)
  term <- .fig_terms(code)
  list(pre = c(if (k == 1L) "# the numbers it prints, from the ARD", pre), term = unlist(term))
}

# ... and a panel: a plot of its own (its data, then its chain)
.fig_own_panel <- function(it, target) {
  fn <- .schema_fn_name(it$fn)
  a <- .fig_args_plain(it$args)
  piece <- c(tfl_risk_panel = "risk_table", tfl_text_column = "text_column",
             tfl_n_panel = "n_table")[[fn]]
  plot <- list(colour_by = a$by, base_size = a$base_size %||% pp_default_options()$base_size)
  a$by <- NULL
  a$base_size <- NULL
  pal <- a$palette
  a$palette <- NULL
  l <- c(list(layer = piece), a, if (identical(piece, "risk_table")) list(method = "panel"))
  x <- .fig_layer_code(l, 1L, plot)
  chain <- x$chain
  old <- x$panel$name
  chain <- gsub(paste0("\\b", old, "\\b"), target, chain)
  pal_line <- if (!is.null(pal)) {
    sprintf("pal <- tfl_colours(%s%s)", q(pal),
            if (!is.null(plot$colour_by)) sprintf(", levels(factor(df$%s))", plot$colour_by) else "")
  }
  list(data = x$data, code = c(pal_line, chain), libs = x$libs)
}

# an item's arguments as the designer's fields (plain values)
.fig_args_plain <- function(a) {
  lapply(a %||% list(), function(v) {
    if (inherits(v, "tfl_r")) return(as.character(unclass(v)))
    if (is.list(v) && is.null(names(v))) return(unlist(v))
    v
  })
}

# ---- the designer's design -> the content (once) -----------------------------

#' A figure design as a figure report's content
#'
#' Converts a figure design of the designer (its pieces and `plot:`
#' settings; [tfl_fig_design()]) once into a figure report's `content:` of
#' function calls, one item a call: the plot's first item (`ggplot`,
#' `ggsurvfit`), its layers by their functions (`geom_line`,
#' `add_censor_mark`, `geom_hline` ...), its settings (`scale_*`, `coord_*`,
#' `labs`, `theme_tfl`, `theme`), tflspec's own items for what tflspec
#' draws (`tfl_ard_number`, and the panels -- `tfl_risk_panel`,
#' `tfl_text_column`, `tfl_n_panel` -- as named plots with the
#' `wrap_plots` item that arranges them), and `tfl_image` last.  The
#' figure is the one the design draws with the study's figure setup
#' ([tfl_fig_setup_code()]): the palette is `scale_colour_tfl()` and the
#' theme `theme_tfl()`, as the setup has them.
#'
#' @param design A [tfl_fig_design()] (or its list).
#' @return A list: `data` (the report's `data:`, its steps the design's,
#'   with the x axis's breaks as a `code` step when the figure uses them),
#'   `content` (the items: [tfl_item()] and [tfl_plot()]), `template`.
#' @examples
#' d <- tfl_fig_template("km_simple", data = "ADTTE", param = "TTDE",
#'                       pop = "SAFFL", group = "TRT01A")
#' x <- tfl_fig_content_from_design(d)
#' vapply(x$content, function(it) it$fn, "")
#' @export
tfl_fig_content_from_design <- function(design) {
  design <- if (inherits(design, "tfl_fig_design")) design else .fig_design_from_list(design)
  if (length(design$plots)) {
    return(tryCatch(.fig_compose_to_items(design), error = function(e) {
      .ard_stop(paste("tfl_fig_content_from_design():", conditionMessage(e)))
    }))
  }
  steps <- design$data %||% list()
  whole <- Filter(function(l) identical(l$layer, "figure"), design$layers)
  if (length(whole)) {
    # a figure type of its own (ae_dot, sankey ...): its function's code,
    # the report's own (content: !r)
    code <- unlist(strsplit(paste(as.character(tfl_fig_design_code(
      design, setup = TRUE, save = FALSE, name = "content")), collapse = "\n"), "\n", fixed = TRUE))
    # (the type's script saves its PNG and prints the figure: the report's
    # content does neither -- its program writes the figure into its RTF)
    cut <- grep("^# ---- saving the figure", code)
    if (length(cut)) code <- code[seq_len(cut[1L] - 1L)]
    code <- code[code != "fig"]
    while (length(code) && !nzchar(trimws(code[length(code)]))) code <- code[-length(code)]
    return(list(data = list(source = "code", steps = steps),
                content = tfl_r(paste(c(code, "content <- fig"), collapse = "\n")),
                template = design$template))
  }
  gg <- .fig_target_version(NULL, design)
  plot <- design$plot
  b <- .fig_design_body(design, gg, setup = TRUE, name = "content")
  # the x axis's breaks: a code step, last of the data's
  xb <- grep("^x_breaks <- ", b$step1, value = TRUE)
  if (length(xb)) steps <- c(steps, list(list(step = "code", label = "the x axis's breaks", code = xb)))
  env <- new.env(parent = globalenv())
  eval(parse(text = tfl_fig_setup_code()), env)
  ex <- parse(text = b$step2, keep.source = FALSE)
  subs <- list()
  main <- NULL
  assemble <- NULL
  panels <- list()
  for (e in as.list(ex)) {
    if (!is.call(e) || !identical(e[[1L]], as.name("<-"))) next
    lhs <- as.character(e[[2L]])
    rhs <- e[[3L]]
    if (lhs %in% c("pal", "lty", "shp", "lv", "pd")) subs[[lhs]] <- rhs
    else if (identical(lhs, "content") && is.null(main)) main <- rhs
    else if (identical(lhs, "content")) assemble <- rhs
    else panels[[lhs]] <- rhs
  }
  layers <- design$layers
  kinds <- vapply(layers, function(l) l$layer %||% "", "")
  ard_layers <- layers[kinds == "ard_number"]
  pname <- .pv(plot, "palette")
  # (the figure's own colours: its `pal`, not the setup's palette scale)
  scale_pal <- if (is.null(.fig_own_colours(plot))) pname
  items <- list()
  for (t in .fig_split_plus(main)) {
    txt <- .fig_deparse(t)
    m <- regmatches(txt, regexpr("ard_label_[0-9]+", txt))
    if (length(m)) {
      k <- as.integer(sub("ard_label_", "", m))
      l <- ard_layers[[k]]
      l$layer <- NULL
      items <- c(items, list(tfl_item("tfl_ard_number", .fig_plain_args(l))))
      next
    }
    pal_scale <- if (!is.null(scale_pal)) .fig_pal_scale(t, scale_pal)
    if (!is.null(pal_scale)) {
      items <- c(items, list(pal_scale))
      next
    }
    t <- eval(call("substitute", t, subs))
    items <- c(items, list(.fig_lang_item(t, env)))
  }
  img <- tfl_item("tfl_image", list(dpi = .pv(plot, "dpi")))
  if (!length(panels)) {
    return(list(data = list(source = "code", steps = steps), content = c(items, list(img)),
                template = design$template))
  }
  # the panels: plots of their own, named after what they are
  out <- list(tfl_plot("plot", items))
  names_of <- c(plot = "content")
  for (pn in names(panels)) {
    i <- if (identical(pn, "p_risk")) which(kinds == "risk_table")[1L]
         else as.integer(sub("^p_[a-z]+", "", pn))
    own <- .fig_panel_item(layers[[i]], plot)
    out <- c(out, list(tfl_plot(own$name, list(own$item))))
    names_of[[own$name]] <- pn
  }
  out <- c(out, list(.fig_wrap_item(assemble, names_of)), list(img))
  list(data = list(source = "code", steps = steps), content = out, template = design$template)
}

# a ggplot chain's terms, in order (`a + b + c`)
.fig_split_plus <- function(e) {
  if (is.call(e) && identical(e[[1L]], as.name("+")) && length(e) == 3L) {
    return(c(.fig_split_plus(e[[2L]]), list(e[[3L]])))
  }
  list(e)
}

# the design's palette scale (scale_colour_manual(values = pal, breaks =
# names(pal))) as the setup's own scale: scale_colour_tfl()
.fig_pal_scale <- function(t, pname) {
  if (!is.call(t)) return(NULL)
  f <- .fig_deparse(t[[1L]])
  if (!f %in% c("scale_colour_manual", "scale_fill_manual")) return(NULL)
  a <- as.list(t)[-1L]
  if (!identical(a$values, as.name("pal"))) return(NULL)
  rest <- a[setdiff(names(a), c("values", "breaks"))]
  # (the colours in the order of the scale's values: the groups' order,
  # as the design's names gave them)
  args <- c(list(palette = pname), lapply(rest, .fig_lang_value))
  tfl_item(sub("_manual$", "_tfl", f), args)
}

# a piece's fields as an item's arguments (a sequence as a vector)
.fig_plain_args <- function(l) {
  lapply(l, function(v) if (is.list(v) && is.null(names(v))) unlist(v) else v)
}

# a panel piece as its plot: tflspec's item, with what the panel drew from
# the design's settings (the colour variable, the palette, the base size)
.fig_panel_item <- function(l, plot) {
  k <- l$layer
  for (f in c("layer", "method", "height", "width", "side")) l[[f]] <- NULL
  common <- list(base_size = .pv(plot, "base_size"))
  colours <- Filter(Negate(is.null), list(by = plot$colour_by, palette = .pv(plot, "palette")))
  switch(k,
    risk_table = list(name = "risk", item = tfl_item("tfl_risk_panel",
                                                     c(.fig_plain_args(l), colours, common))),
    text_column = list(name = "text", item = tfl_item("tfl_text_column",
                                                      c(.fig_plain_args(l), common))),
    n_table = list(name = "n", item = tfl_item("tfl_n_panel",
                                               c(.fig_plain_args(l), colours, common))),
    .ard_stop(sprintf("tfl_fig_content_from_design(): a panel `%s` is not converted.", k)))
}

# the design's arrangement (content / p_risk + plot_layout(heights = h), or
# content + p_txt + plot_layout(widths = w)) as wrap_plots()
.fig_wrap_item <- function(assemble, names_of) {
  terms <- .fig_split_plus(assemble)
  syms <- character()
  below <- FALSE
  lay <- list()
  for (t in terms) {
    if (is.name(t)) {
      syms <- c(syms, as.character(t))
    } else if (is.call(t) && identical(t[[1L]], as.name("/"))) {
      below <- TRUE
      syms <- c(syms, as.character(t[[2L]]), as.character(t[[3L]]))
    } else if (is.call(t) && identical(.fig_deparse(t[[1L]]), "plot_layout")) {
      lay <- lapply(as.list(t)[-1L], function(v) eval(v, baseenv()))
    }
  }
  nm <- vapply(syms, function(s) names(names_of)[match(s, names_of)], "")
  args <- c(list(plots = unname(nm)), if (below) list(ncol = 1L) else list(nrow = 1L), lay)
  tfl_item("wrap_plots", args)
}

# ---- an item's arguments as R text, and back ---------------------------------

#' An item's arguments as R, and R as an item's arguments
#'
#' A card of the builder can be edited as the R a function takes:
#' `tfl_args_to_r()` writes an item's arguments (its `args`, as the spec
#' holds them) as the text of a call's arguments -- `cols = "TRTA", rows =
#' c(group = "variable")`; `tfl_args_from_r()` reads that text back into
#' the item's arguments, each value in the form the spec file holds it
#' (the schema's types: a map for a named vector, `!r` for an expression,
#' `{fn:, args:}` for a call of constants), and checks them
#' ([tfl_schema_check()]).  A figure's functions (`ggplot`, `geom_line`,
#' `labs` ...) are read as the figure's content reads them: `aes()` as a
#' map of columns (`aes(AVAL, CHG)` is x and y), the plot's data by its
#' name, a facet's variables as its columns (`facet_grid(SEX ~ TRTA)` is
#' `rows: SEX, cols: TRTA`), a value's constructor (a position, a guide,
#' an element, `theme()`, `unit()`, `rel()`, `sec_axis()` ...) as
#' `{fn:, args:}` with its arguments named and any that is no constant as
#' `!r` in it, `1:5` as the numbers.
#'
#' @param fn The function's name (`plan_cells`, `geom_line`).
#' @param args The item's arguments, a named list.
#' @param text The arguments as R: what goes between the call's
#'   parentheses.
#' @return `tfl_args_to_r()`: one string.  `tfl_args_from_r()`: the
#'   arguments, a named list, with the problems of the check as the
#'   attribute `problems` (none: no rows).
#' @examples
#' tfl_args_to_r("plan_cells", list(categorical = "{n} ({p:.1f%})"))
#' a <- tfl_args_from_r("geom_line", "aes(x = AVISIT, y = mean), linewidth = 0.5")
#' str(a)
#' tfl_args_to_r("geom_line", a)
#' @export
tfl_args_to_r <- function(fn, args) {
  if (!is.character(fn) || length(fn) != 1L) .ard_stop("tfl_args_to_r(): `fn` is a function's name.")
  args <- args %||% list()
  if (.fig_is_figure_fn(fn)) {
    e <- .fig_item_lang(tfl_item(fn, args))
    return(sub("\\)$", "", sub("^[^(]*\\(", "", .fig_deparse(e))))
  }
  vals <- .schema_to_r(fn, args)
  nm <- names(vals) %||% rep("", length(vals))
  paste(vapply(seq_along(vals), function(i) {
    v <- .spec_code_line(vals[[i]])
    if (nzchar(nm[i])) paste(.spec_name(nm[i]), "=", v) else v
  }, ""), collapse = ", ")
}

#' @rdname tfl_args_to_r
#' @export
tfl_args_from_r <- function(fn, text) {
  if (!is.character(fn) || length(fn) != 1L) .ard_stop("tfl_args_from_r(): `fn` is a function's name.")
  e <- tryCatch(str2lang(sprintf("%s(%s)", .schema_fn_name(fn), paste(text, collapse = "\n"))),
                error = function(err) .ard_stop(sprintf("tfl_args_from_r(): not R: %s", conditionMessage(err))))
  if (.fig_is_figure_fn(fn)) {
    e[[1L]] <- .fig_fn_sym(fn)
    it <- .fig_lang_item(e)
    if (identical(it$fn, ".r")) {
      .ard_stop(sprintf("tfl_args_from_r(): an argument of %s() with no name its formals can give it.", fn))
    }
    args <- it$args
  } else {
    a <- as.list(e)[-1L]
    vals <- lapply(a, .args_value_of)
    args <- .schema_from_r(fn, vals)
  }
  # a whole number where the schema takes an integer, as one (26, not 26.0):
  # the text and the item agree both ways
  ent <- tfl_schema(fn)
  for (k in names(args)) {
    ty <- .schema_arg_type(ent, k)
    v <- args[[k]]
    if (!is.null(ty) && .schema_int_type(ty) && is.numeric(v) && !is.integer(v) &&
        all(!is.na(v) & v == round(v))) {
      args[[k]] <- as.integer(v)
    }
  }
  attr(args, "problems") <- tfl_schema_check(tfl_item(fn, args))
  args
}

# a call's argument as R holds it: what evaluates on its own (a constant,
# c(group = "variable"), cell_rows() of templates and guards) as its
# value, anything else (a column, an expression) as an expression
.args_value_of <- function(x) {
  if (is.name(x)) return(.spec_expr(as.character(x)))
  # (NULL is the R text NULL: before is.atomic(), TRUE for NULL before R 4.4)
  if (is.null(x)) return(.spec_expr("NULL"))
  if (is.atomic(x)) return(x)
  env <- new.env(parent = asNamespace("rtfreporter"))
  v <- tryCatch(eval(x, env), error = function(e) NULL)
  if (!is.null(v) && !is.function(v)) return(v)
  .spec_expr(.fig_deparse(x))
}

# a function of a figure's content (the catalog's, or a ggplot2 layer or
# setting the catalog has not)
.fig_is_figure_fn <- function(fn) {
  nm <- .schema_fn_name(fn)
  ent <- .schema_all(figure = FALSE)[[nm]]
  if (!is.null(ent$figure)) return(TRUE)
  is.null(ent) && grepl("^(geom|stat|scale|coord|facet|theme|guide|position|element)_", nm)
}

# A figure report of the designer's pieces as one of function calls: its
# design (the data steps, the layers, `plot:`) converted, the rest of the
# report as it is (its page, code lists, ARD)
.fig_output_to_items <- function(output) {
  if (.fig_new_content(output$content) || inherits(output$content, "tfl_r")) return(output)
  x <- tfl_fig_content_from_design(.fig_design_of_output(output))
  output$data <- c(x$data, output$data[setdiff(names(output$data), c("source", "steps", "stats"))])
  output$content <- x$content
  output$plot <- NULL
  output$plots <- NULL
  output$compose <- NULL
  if (!is.null(x$template)) output$template <- x$template
  output
}

# The image saved, when its tfl_image names a file: at its size on the page
# and its resolution, in its format (`{output_id}` filled)
.fig_image_save <- function(output, img, name = "content") {
  if (is.null(img$file) || !nzchar(img$file)) return(character())
  pg <- output$page %||% list()
  lines <- length(pg$titles %||% list()) + length(pg$footnotes %||% list())
  sz <- .fig_image_size(img, pg, lines)
  file <- gsub("{output_id}", output$id %||% "figure", img$file, fixed = TRUE)
  fmt <- img$format %||% tools::file_ext(file)
  if (!nzchar(fmt)) fmt <- "png"
  c("", "# the image's file",
    sprintf("ggsave(%s, %s, width = %s, height = %s, dpi = %s, device = %s)",
            q(file), name, format(sz$width_in), format(sz$height_in),
            format(as.numeric(img$dpi %||% 300)), q(fmt)))
}

# ---- a composed design (plots: + compose:) as named plots --------------------

# A design of several designs, each its own data (`plots:`), arranged by
# `compose:` -- as named plots sharing one data part: each plot's objects
# renamed after it (df -> df_<name>, sm -> sm_<name>), its items with them;
# `compose.layout` a wrap_plots() (one row `a | b`, one column `a / b`),
# `compose.add` items (an `&` kept as the item's `op`), `plot.dpi` tfl_image.
.fig_compose_to_items <- function(design) {
  nms <- names(design$plots)
  steps <- list()
  out <- list()
  for (nm in nms) {
    sub <- design$plots[[nm]]
    sub <- if (inherits(sub, "tfl_fig_design")) sub else .fig_design_from_list(sub)
    if (length(sub$plots)) .ard_stop("a composed figure inside a composed figure is not converted.")
    x <- tfl_fig_content_from_design(sub)
    if (inherits(x$content, "tfl_r") || any(vapply(x$content, inherits, NA, "tfl_plot"))) {
      .ard_stop(sprintf("the composed figure's plot `%s` is a figure of its own or has panels: not converted.", nm))
    }
    st <- x$data$steps %||% list()
    io <- .fig_step_io(st)
    objs <- unique(stats::na.omit(unlist(lapply(io, function(z) c(z[["in"]], z[["out"]])))))
    objs <- setdiff(objs, c("ard", tolower(vapply(st, function(s) s$dataset %||% "", ""))))
    # the x axis's breaks, a code step's object, are the plot's too
    if (any(vapply(st, function(s) identical(s$step, "code") && grepl("^x_breaks <-", s$code %||% ""), NA))) {
      objs <- c(objs, "x_breaks")
    }
    ren <- function(v) if (!is.null(v) && length(v) == 1L && v %in% objs) paste0(v, "_", nm) else v
    txt <- function(v) {
      for (o in objs) v <- gsub(paste0("\\b", o, "\\b"), paste0(o, "_", nm), v, perl = TRUE)
      v
    }
    for (i in seq_along(st)) {
      s <- st[[i]]
      k <- s$step
      if (identical(k, "read")) s$out <- ren(io[[i]]$out)
      else if (k %in% c(.fig_row_steps, "code")) {
        s$`in` <- ren(io[[i]][["in"]])
        if (!is.na(io[[i]]$out)) s$out <- ren(io[[i]]$out)
        if (identical(k, "code")) s$code <- txt(s$code)
      } else if (k %in% c("survfit", "summary", "summary_by", "rate", "count", "function")) {
        s$`in` <- ren(io[[i]][["in"]])
        s$name <- ren(io[[i]]$out)
      } else if (identical(k, "subset")) {
        s$name <- ren(io[[i]]$out)
        if (!is.null(s$dataset)) s$dataset <- ren(s$dataset)
      }
      # an expression of the step that names an object (a derive's)
      for (f in c("expr", "where")) if (is.character(s[[f]])) s[[f]] <- txt(s[[f]])
      st[[i]] <- s
    }
    steps <- c(steps, st)
    items <- Filter(function(it) !identical(it$fn, "tfl_image"), x$content)
    items <- lapply(items, function(it) {
      it$args <- .fig_rename_args(it$args, ren, txt)
      it
    })
    out <- c(out, list(tfl_plot(nm, items)))
  }
  layout <- design$compose$layout %||% .fig_layout_default(design)
  tok <- .fig_layout_tokens(layout)
  ops <- unique(tok[!grepl("^[A-Za-z]", tok)])
  if (is.null(tok) || length(ops) > 1L || any(ops %in% c("(", ")", "-"))) {
    .ard_stop(sprintf("compose.layout `%s`: only one row (a | b) or one column (a / b) is converted.", layout))
  }
  wrap <- c(list(plots = tok[grepl("^[A-Za-z]", tok)]),
            if (identical(ops, "/")) list(ncol = 1L) else list(nrow = 1L))
  out <- c(out, list(tfl_item("wrap_plots", wrap)))
  for (a in design$compose$add %||% list()) {
    args <- a$args %||% list()
    if (identical(a$op, "&")) args$op <- "&"
    out <- c(out, list(tfl_item(a$fn, args)))
  }
  out <- c(out, list(tfl_item("tfl_image", list(dpi = .pv(design$plot %||% list(), "dpi")))))
  list(data = list(source = "code", steps = steps), content = out, template = design$template)
}

# an item's arguments with a plot's objects renamed: an object argument by
# its name, R text by its words, a call's arguments in turn
.fig_rename_args <- function(a, ren, txt) {
  lapply(stats::setNames(names(a), names(a)), function(k) {
    v <- a[[k]]
    if (inherits(v, "tfl_r")) return(tfl_r(txt(as.character(unclass(v)))))
    if (inherits(v, "tfl_call")) { v$args <- .fig_rename_args(v$args, ren, txt); return(v) }
    if (k %in% .fig_object_args || k == "x") return(ren(v))
    if (identical(k, "mapping") && is.list(v)) {
      return(lapply(v, function(e) if (inherits(e, "tfl_r")) tfl_r(txt(as.character(unclass(e)))) else e))
    }
    v
  })
}

# Is a type an integer, or a sequence of them (list<integer>), first of its
# alternatives?
.schema_int_type <- function(ty) {
  if (identical(ty$kind, "integer")) return(TRUE)
  if (identical(ty$kind, "list")) return(.schema_int_type(ty$args[[1L]]))
  if (identical(ty$kind, "either")) return(.schema_int_type(ty$args[[1L]]))
  FALSE
}
