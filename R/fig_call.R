# The generic `call` piece: `p <- p + fn(data = ..., aes(...), pos..., args...)`
# for any ggplot2 (or extension package) function -- the escape hatch for
# what the curated geom catalog (fig_geoms.R) does not cover, without going
# all the way to free-form R code (layer_code / a data step's code).
#
# Used in two places, both written the same way (see tfl_fig_parts()):
#   layers:  {layer: call, fn, package, data, aes, pos, args, base}
#   plot.add: the same, minus `layer` and `base`
#
# .fig_build_call() writes one call's R code (recursively, for nested calls
# such as element_text(), unit(), arrow() ...); .fig_arg_code() writes one
# argument's value per the YAML -> R table in the design doc.  Every call
# this writes is checked with parse() before being handed back.

#' Raw R code in a figure design
#'
#' A figure design's YAML is mostly declarative (numbers, strings, small
#' maps) written out as R by [tfl_fig_design_code()]; `tfl_fig_r()` marks a
#' string that is already R code and must be emitted verbatim (e.g.
#' `vars(PARAM)`, `scales::label_number()`, a bare `NA`) instead of being
#' quoted as a string. In YAML this is the `!r` tag: `theme: !r
#' theme_risktable_default(axis.text.y.size = 9)`.
#'
#' @param code One string of R code.
#' @return A length-1 character vector of class `tfl_fig_r`.
#' @export
tfl_fig_r <- function(code) {
  x <- structure(as.character(code)[1L], class = "tfl_fig_r")
  attr(x, "tag") <- "!r"
  x
}

.is_fig_r <- function(x) inherits(x, "tfl_fig_r")

# packages a generated call is always free to call bare (already library()'d
# by every generated script, or by the ones the call mechanism itself adds).
.fig_bare_pkgs <- c("ggplot2", "ggsurvfit", "patchwork", "dplyr")

# `fn` split on "::", else (fn, package) as given.
.fig_resolve_fn <- function(fn, package = NULL) {
  if (grepl("::", fn, fixed = TRUE)) {
    parts <- strsplit(fn, "::", fixed = TRUE)[[1L]]
    return(list(pkg = parts[1L], fn = parts[2L]))
  }
  list(pkg = package, fn = fn)
}

# ggplot2 -> ggsurvfit -> patchwork, the first that exports `fn`; else the
# call catalog's package (installed or not).
.fig_find_pkg <- function(fn) {
  for (pkg in c("ggplot2", "ggsurvfit", "patchwork")) {
    if (requireNamespace(pkg, quietly = TRUE) && fn %in% getNamespaceExports(pkg)) return(pkg)
  }
  cc <- .fig_calls()
  if (fn %in% cc$fn) return(cc$package[match(fn, cc$fn)])
  NULL
}

.fig_calls <- function() {
  if (is.null(.fig_registry$calls)) {
    .fig_registry$calls <- utils::read.csv(
      system.file("fig", "calls.csv", package = "tflspec", mustWork = TRUE),
      stringsAsFactors = FALSE, na.strings = "", encoding = "UTF-8")
  }
  .fig_registry$calls
}

#' Extension functions a figure design knows
#'
#' Functions of ggplot2 extension packages worth offering by name for a
#' `call` piece (see [tfl_fig_design()]): nested facets (ggh4x), markdown
#' text (ggtext), a zoomed panel (ggforce), a second colour or fill scale
#' (ggnewscale). A `call` of one of them needs no `package:`; the script
#' calls it as `pkg::fn` and notes the package under "# also needs". Any
#' other function (cowplot, ggpubr, ggbreak ...) is reached the same way
#' with `package:` given. Geoms of extension packages (ggrepel's
#' `geom_text_repel`, ggforce's `geom_sina`) are layers of the catalog
#' instead ([tfl_fig_add_layer()]).
#'
#' @return A data frame: `package`, `fn`, `where` (`layers`, `plot.add`,
#'   or `nested`: a value inside another call, e.g. in `theme()`),
#'   `label`, `help`.
#' @export
tfl_fig_calls <- function() .fig_calls()

# `fn` or `pkg::fn`, recording what a bare call needs added to `library()`
# lines (ggsurvfit/patchwork) or to the "# also needs" line (other packages).
.fig_call_text <- function(fn, pkg, needs_env) {
  pkg <- pkg %||% "ggplot2"
  if (pkg %in% .fig_bare_pkgs) {
    if (pkg %in% c("ggsurvfit", "patchwork") && !is.null(needs_env)) {
      needs_env$libs <- union(needs_env$libs, pkg)
    }
    return(fn)
  }
  if (!is.null(needs_env)) needs_env$pkgs <- union(needs_env$pkgs, pkg)
  paste0(pkg, "::", fn)
}

.fig_num_code <- function(x) {
  if (is.na(x)) return("NA")
  if (is.infinite(x)) return(if (x > 0) "Inf" else "-Inf")
  format(x, trim = TRUE, scientific = FALSE)
}

# A name as a function argument (backtick if not syntactic) or as a c()/
# list() name (quoted if not syntactic).
.fig_arg_name <- function(nm, vector = FALSE) {
  if (nzchar(nm) && make.names(nm) == nm) return(nm)
  if (vector) q(nm) else paste0("`", nm, "`")
}

# One design value's R code (recursive): x is a YAML-parsed scalar, sequence
# (list) or map; expr = TRUE inside aes()/vars() writes a plain string as an
# expression (unquoted) instead of a quoted string.
.fig_arg_code <- function(x, expr = FALSE, needs_env = NULL) {
  if (is.null(x)) return("NULL")
  if (.is_fig_r(x)) return(as.character(x)[1L])
  if (is.list(x)) {
    nms <- names(x)
    if (!is.null(nms) && "fn" %in% nms) return(.fig_build_call(x, needs_env))
    if (!is.null(nms) && any(nzchar(nms))) {
      items <- vapply(seq_along(x), function(i) {
        nm <- nms[i]
        code <- .fig_arg_code(x[[i]], expr = FALSE, needs_env = needs_env)
        if (nzchar(nm)) paste0(.fig_arg_name(nm, vector = TRUE), " = ", code) else code
      }, character(1))
      return(paste0("c(", paste(items, collapse = ", "), ")"))
    }
    items <- vapply(x, .fig_arg_code, character(1), expr = expr, needs_env = needs_env)
    if (!length(items)) return("c()")
    return(paste0("c(", paste(items, collapse = ", "), ")"))
  }
  if (length(x) > 1L) {
    items <- vapply(x, function(v) .fig_arg_code(v, expr = expr, needs_env = needs_env), character(1))
    return(paste0("c(", paste(items, collapse = ", "), ")"))
  }
  if (is.logical(x)) return(if (is.na(x)) "NA" else if (isTRUE(x)) "TRUE" else "FALSE")
  if (is.numeric(x)) return(.fig_num_code(x))
  if (expr) return(as.character(x))
  q(as.character(x))
}

# One call's code: fn(data = ..., aes(...), pos..., args...). `spec` is a
# list with fn (required), package, data, aes, pos, args -- from a `call`
# layer, a `plot.add` element, or a nested {fn: ..., args: ...} value.
.fig_build_call <- function(spec, needs_env = NULL) {
  fn <- spec$fn
  if (is.null(fn) || !nzchar(fn)) stop("call: `fn` is required", call. = FALSE)
  rf <- .fig_resolve_fn(fn, spec$package)
  fn_name <- rf$fn
  pkg <- rf$pkg %||% .fig_find_pkg(fn_name) %||% "ggplot2"
  call_text <- .fig_call_text(fn_name, pkg, needs_env)
  is_expr_fn <- fn_name %in% c("aes", "vars")
  parts <- character()
  if (!is.null(spec$data)) parts <- c(parts, paste0("data = ", spec$data))
  if (!is.null(spec$aes) && length(spec$aes)) {
    a_items <- vapply(seq_along(spec$aes), function(i) {
      paste0(names(spec$aes)[i], " = ", .fig_arg_code(spec$aes[[i]], expr = TRUE, needs_env = needs_env))
    }, character(1))
    parts <- c(parts, paste0("aes(", paste(a_items, collapse = ", "), ")"))
  }
  if (!is.null(spec$pos) && length(spec$pos)) {
    parts <- c(parts, vapply(spec$pos, .fig_arg_code, character(1), expr = is_expr_fn, needs_env = needs_env))
  }
  if (!is.null(spec$args) && length(spec$args)) {
    nms <- names(spec$args)
    a_items <- vapply(seq_along(spec$args), function(i) {
      paste0(.fig_arg_name(nms[i]), " = ", .fig_arg_code(spec$args[[i]], expr = is_expr_fn, needs_env = needs_env))
    }, character(1))
    parts <- c(parts, a_items)
  }
  paste0(call_text, "(", paste(parts, collapse = ", "), ")")
}

# A `call` layer or `plot.add` element's code: the assignment line(s), plus
# what it needs (libs to add to library(), packages to note as "# also
# needs"). Errors (not just warns) when the generated call does not parse.
# `ggplot2_version`: the call is written for that version (fig_compat.R) --
# renamed functions/arguments under its names, with a comment saying so.
.fig_call_code <- function(spec, target = "p", plus = TRUE, ggplot2_version = NULL) {
  needs_env <- new.env(parent = emptyenv())
  needs_env$libs <- character()
  needs_env$pkgs <- character()
  cw <- NULL
  if (!is.null(ggplot2_version)) {
    cw <- .fig_compat_walk(spec, ggplot2_version, "")
    spec <- cw$spec
  }
  call_text <- .fig_build_call(spec, needs_env)
  line <- if (plus) sprintf("%s <- %s + %s", target, target, call_text) else sprintf("%s <- %s", target, call_text)
  if (length(cw$renames)) {
    line <- sprintf("%s   # ggplot2 %s: %s", line, ggplot2_version,
                    paste(unique(cw$renames), collapse = ", "))
  }
  parsed <- tryCatch(parse(text = line), error = function(e) e)
  if (inherits(parsed, "error")) {
    stop("Could not generate valid R code for `", spec$fn %||% "?", "`: ", conditionMessage(parsed), call. = FALSE)
  }
  list(line = line, call = call_text, libs = needs_env$libs, pkgs = needs_env$pkgs,
       guard = cw$guard, features = cw$features)
}

# plot.add: the lines after the figure-wide finish block, before panels.
.fig_plot_add_code <- function(add, ggplot2_version = NULL) {
  if (!length(add)) return(list(lines = character(), libs = character(), pkgs = character()))
  libs <- character(); pkgs <- character(); guard <- character(); features <- character()
  lines <- unlist(lapply(add, function(a) {
    res <- .fig_call_code(a, ggplot2_version = ggplot2_version)
    libs <<- union(libs, res$libs)
    pkgs <<- union(pkgs, res$pkgs)
    guard <<- c(guard, res$guard)
    features <<- c(features, res$features)
    res$line
  }))
  list(lines = c("# ---- plot.add ----", lines), libs = libs, pkgs = pkgs,
       guard = guard, features = features)
}

# ---- validation (tfl_check_fig_design(), design doc S2) --------------------

# a plot-wide function used in `layers` should be in `plot.add` instead
.fig_plotwide_re <- "^(theme|theme_[A-Za-z0-9_.]+|scale_[A-Za-z0-9_.]+|coord_[A-Za-z0-9_.]+|facet_[A-Za-z0-9_.]+|labs|guides|xlab|ylab|ggtitle)$"

.fig_bare_fn_name <- function(fn) sub("^.*::", "", fn %||% "")

# agrep suggestion for an unknown argument name.
.fig_suggest <- function(nm, known) {
  hit <- tryCatch(agrep(nm, known, max.distance = 0.3, value = TRUE), error = function(e) character())
  if (length(hit)) paste0(" (did you mean ", paste(utils::head(hit, 3L), collapse = ", "), "?)") else ""
}

# ggplot2's element tree names (theme()'s valid argument names), cached.
.fig_theme_elements <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) cache <<- names(ggplot2::get_element_tree())
    cache
  }
})

# Validates one call spec (a `call` layer, a `plot.add` element, or a nested
# {fn: ...}); returns a data frame (part, field, problem), possibly 0-row.
# `part` is the caller's part label; nested calls get " > fn" appended.
.fig_check_call <- function(spec, part) {
  out <- data.frame(part = character(), field = character(), problem = character(),
                    stringsAsFactors = FALSE)
  add <- function(p, f, x) out[nrow(out) + 1L, ] <<- list(p, f, x)
  fn <- spec$fn
  if (is.null(fn) || !nzchar(fn)) { add(part, "fn", "is required"); return(out) }
  rf <- .fig_resolve_fn(fn, spec$package)
  fn_name <- rf$fn
  pkg <- rf$pkg
  if (is.null(pkg)) {
    pkg <- .fig_find_pkg(fn_name)
    # a ggplot2 function of the other version: the compat table's to judge
    if (is.null(pkg) && .fig_compat_names(fn_name)$fn) return(out)
    if (is.null(pkg)) {
      add(part, "fn", paste0("function '", fn_name, "' not found in ggplot2/ggsurvfit/patchwork; specify package:"))
      return(out)
    }
  }
  if (!requireNamespace(pkg, quietly = TRUE)) {
    add(part, "package", paste0("'", pkg, "' is not installed; its arguments are not checked"))
    .fig_check_nested(spec, part, out_env = environment())
    return(out)
  }
  # what differs between ggplot2 3.5 and 4.0 is the compat table's to judge
  # (fig_compat.R), not the installed version's
  cn <- if (identical(pkg, "ggplot2")) .fig_compat_names(fn_name) else list(args = character(), arg_globs = character(), fn = FALSE)
  in_table <- function(a) a %in% cn$args | vapply(a, function(x) any(.fig_glob(cn$arg_globs, x)), logical(1))
  if (!fn_name %in% getNamespaceExports(pkg)) {
    if (!cn$fn) add(part, "fn", paste0("'", fn_name, "' is not exported by ", pkg))
    return(out)
  }
  fn_obj <- getExportedValue(pkg, fn_name)
  fm <- formals(fn_obj)
  fm_names <- names(fm)
  has_dots <- "..." %in% fm_names
  given <- names(spec$args)
  if (!is.null(spec$data)) given <- union(given, "data")
  if (!is.null(spec$aes)) given <- union(given, "mapping")
  if (!has_dots) {
    unknown <- setdiff(given, fm_names)
    unknown <- unknown[!in_table(unknown)]
    for (u in unknown) add(part, paste0("args$", u), paste0(
      "'", u, "' is not an argument of ", fn_name, "()", .fig_suggest(u, fm_names)))
    no_default <- fm_names[vapply(fm, function(d) identical(d, quote(expr = )), logical(1))]
    no_default <- setdiff(no_default, "...")
    n_pos <- length(spec$pos %||% list())
    filled_by_pos <- if (n_pos) fm_names[seq_len(min(n_pos, length(fm_names)))] else character()
    missing <- setdiff(no_default, union(given, filled_by_pos))
    for (m in missing) add(part, "args", paste0("required argument '", m, "' is missing"))
  } else if (identical(fn_name, "theme")) {
    known <- .fig_theme_elements()
    unknown <- setdiff(names(spec$args), known)
    unknown <- unknown[!in_table(unknown)]
    for (u in unknown) add(part, paste0("args$", u), paste0(
      "'", u, "' is not a theme() element", .fig_suggest(u, known)))
  } else if (grepl("^(geom|stat)_", fn_name)) {
    layer_obj <- tryCatch(do.call(fn_obj, list()), error = function(e) NULL)
    if (inherits(layer_obj, "Layer")) {
      known <- unique(c(layer_obj$geom$aesthetics(), layer_obj$geom$parameters(TRUE),
                        layer_obj$stat$parameters(TRUE),
                        c("na.rm", "show.legend", "inherit.aes", "key_glyph", "orientation",
                          "mapping", "data", "position", "stat", "geom")))
      unknown <- setdiff(names(spec$args), known)
      unknown <- unknown[!in_table(unknown)]
      for (u in unknown) add(part, paste0("args$", u), paste0(
        "'", u, "' is not a parameter of ", fn_name, "()", .fig_suggest(u, known)))
      unknown_aes <- setdiff(names(spec$aes), layer_obj$geom$aesthetics())
      for (u in unknown_aes) add(part, paste0("aes$", u), paste0(
        "'", u, "' is not an aesthetic of ", fn_name, "()"))
    }
  }
  # nested {fn:} values and `!r` parse checks
  .fig_check_nested(spec, part, out_env = environment())
  out
}

# checks nested {fn:} calls (recursively) and `!r` parse-ability, wherever
# they appear in aes/pos/args; appends findings to `out` in the caller's env.
.fig_check_nested <- function(spec, part, out_env) {
  walk <- function(x, where) {
    if (is.null(x)) return()
    if (.is_fig_r(x)) {
      p <- tryCatch(parse(text = as.character(x)), error = function(e) e)
      if (inherits(p, "error")) out_env$add(part, where, paste0("!r does not parse: ", conditionMessage(p)))
      return()
    }
    if (is.list(x)) {
      nms <- names(x)
      if (!is.null(nms) && "fn" %in% nms) {
        sub <- .fig_check_call(x, paste0(part, " > ", x$fn %||% "?"))
        if (nrow(sub)) for (r in seq_len(nrow(sub))) out_env$add(sub$part[r], sub$field[r], sub$problem[r])
        return()
      }
      for (i in seq_along(x)) walk(x[[i]], if (!is.null(nms) && nzchar(nms[i])) paste0(where, "$", nms[i]) else where)
    }
  }
  walk(spec$aes, "aes")
  walk(spec$pos, "pos")
  walk(spec$args, "args")
}
