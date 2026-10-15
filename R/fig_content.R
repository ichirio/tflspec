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

# A value of an item as the R it stands for (a language object)
.fig_value_lang <- function(v, arg = "", objects = .fig_object_args) {
  if (inherits(v, "tfl_unset")) {
    warning(sprintf(paste0("A figure's `%s` is !unset, which removes a key in the layering; ",
                           "NULL is !r NULL: written as NULL."), arg), call. = FALSE)
    return(NULL)
  }
  if (inherits(v, "tfl_r")) return(str2lang(as.character(unclass(v))))
  if (inherits(v, "tfl_call")) {
    a <- lapply(stats::setNames(names(v$args %||% list()), names(v$args %||% list())),
                function(k) .fig_value_lang(v$args[[k]], k))
    return(as.call(c(list(.fig_fn_sym(v$fn)), a)))
  }
  if (identical(arg, "mapping") && is.list(v)) {
    a <- lapply(v, function(e) if (inherits(e, "tfl_r")) str2lang(as.character(unclass(e)))
                               else as.name(e))
    return(as.call(c(list(as.name("aes")), a)))
  }
  if (arg %in% objects && is.character(v) && length(v) == 1L) return(as.name(v))
  if (is.list(v)) {
    el <- lapply(v, .fig_value_lang)
    return(as.call(c(list(as.name("c")), el)))
  }
  # (a whole number read from YAML is an integer: written as R writes 16)
  if (is.integer(v)) v <- as.numeric(v)
  if (length(v) == 1L && is.null(names(v))) return(v)
  as.call(c(list(as.name("c")), as.list(v)))
}

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

# An item as its call
.fig_item_lang <- function(it) {
  a <- it$args %||% list()
  obj <- .fig_objects_of(it$fn)
  args <- lapply(stats::setNames(names(a), names(a)), function(k) .fig_value_lang(a[[k]], k, obj))
  as.call(c(list(.fig_fn_sym(it$fn)), args))
}

.fig_deparse <- function(e) paste(deparse(e, width.cutoff = 500L), collapse = " ")

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
# else as R (!r)
.fig_lang_value <- function(x, arg = "", objects = .fig_object_args) {
  if (is.null(x)) return(tfl_r("NULL"))
  if (is.atomic(x) && length(x) == 1L) return(x)
  if (is.name(x)) {
    if (arg %in% objects) return(as.character(x))
    return(tfl_r(as.character(x)))
  }
  if (is.call(x)) {
    f <- if (is.name(x[[1L]])) as.character(x[[1L]]) else .fig_deparse(x[[1L]])
    a <- as.list(x)[-1L]
    if (identical(f, "aes") && identical(arg, "mapping")) {
      out <- lapply(a, function(e) if (is.name(e)) as.character(e) else tfl_r(.fig_deparse(e)))
      return(out)
    }
    # a negative number, a vector of constants
    if (identical(f, "-") && length(a) == 1L && is.numeric(a[[1L]])) return(-a[[1L]])
    if (identical(f, "c") && length(a) && is.null(names(a)) &&
        all(vapply(a, function(e) is.atomic(e) && length(e) == 1L, NA))) {
      return(unlist(a))
    }
    if (f %in% .fig_constructors && all(nzchar(names(a) %||% rep("", length(a))))) {
      vals <- lapply(stats::setNames(names(a), names(a)), function(k) .fig_lang_value(a[[k]], k))
      if (!any(vapply(vals, inherits, NA, "tfl_r"))) return(tfl_call(f, vals))
    }
  }
  tfl_r(.fig_deparse(x))
}

# the constructors an argument is written as {fn:, args:} when its own
# arguments are constants
.fig_constructors <- c("position_dodge", "position_jitter", "position_jitterdodge",
                       "position_nudge", "position_stack", "position_fill",
                       "element_text", "element_line", "element_rect", "element_blank",
                       "margin", "unit", "expansion", "guide_legend", "guide_none",
                       "arrow", "rel", "element_markdown", "element_textbox_simple")

# A term of a ggplot chain (a call) as an item: its function, its
# arguments by name (positional ones named by the function's formals)
.fig_lang_item <- function(e, env = NULL) {
  f <- .fig_deparse(e[[1L]])
  fun <- .fig_find_fun(f, env)
  given <- names(as.list(e)[-1L]) %||% rep("", length(e) - 1L)
  if (!is.null(fun) && is.function(fun) && !is.primitive(fun)) {
    m <- tryCatch(match.call(fun, e, expand.dots = TRUE), error = function(err) NULL)
    if (!is.null(m)) {
      # the arguments in the order they were written: match.call() puts
      # them in the formals' order and names the positional ones -- those
      # (not named as given) take the positional places, in their order
      mn <- names(as.list(m)[-1L])
      pos <- setdiff(mn, given[nzchar(given)])
      ord <- given
      ord[!nzchar(ord)] <- pos[seq_len(sum(!nzchar(ord)))]
      if (!anyNA(ord) && setequal(ord, mn)) {
        e <- as.call(c(list(m[[1L]]), as.list(m)[-1L][ord]))
      } else {
        e <- m
      }
    }
  }
  a <- as.list(e)[-1L]
  nm <- names(a) %||% rep("", length(a))
  # an aes() given first, unnamed (geom_line(aes(...)))
  for (i in which(!nzchar(nm))) {
    if (is.call(a[[i]]) && identical(.fig_deparse(a[[i]][[1L]]), "aes")) nm[i] <- "mapping"
  }
  if (any(!nzchar(nm))) {
    # an argument the function's formals cannot name: the whole call as R
    return(tfl_item(".r", list(code = tfl_r(.fig_deparse(e)))))
  }
  names(a) <- nm
  obj <- .fig_objects_of(f)
  tfl_item(f, lapply(stats::setNames(nm, nm), function(k) .fig_lang_value(a[[k]], k, obj)))
}

.fig_find_fun <- function(f, env = NULL) {
  if (grepl("::", f, fixed = TRUE)) {
    return(tryCatch(getExportedValue(sub("::.*$", "", f), sub("^.*::", "", f)),
                    error = function(e) NULL))
  }
  if (!is.null(env) && exists(f, envir = env, mode = "function")) return(get(f, envir = env, mode = "function"))
  for (pkg in c("ggplot2", "ggsurvfit", "patchwork", "scales")) {
    if (requireNamespace(pkg, quietly = TRUE) &&
        exists(f, envir = asNamespace(pkg), inherits = FALSE)) {
      return(getExportedValue(pkg, f))
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
        if (fn %in% c("ggsurvfit", "ggcuminc") || startsWith(fn, "add_")) libs <- c(libs, "ggsurvfit")
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
    pal_scale <- .fig_pal_scale(t, pname)
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
#' map of columns, the plot's data by its name.
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
  if (is.atomic(x)) return(x)
  env <- new.env(parent = if (requireNamespace("rtfreporter", quietly = TRUE))
    asNamespace("rtfreporter") else baseenv())
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
