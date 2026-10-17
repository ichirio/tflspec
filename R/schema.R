# ============================================================================
#  The spec schema: what each argument of a spec item takes
# ----------------------------------------------------------------------------
#  A spec's `content:` is a list of items (tfl_item()), one a function call
#  by its arguments' names.  inst/spec/schema.yml says, for the functions a
#  spec may name, the TYPE of each value where the spec writes it otherwise
#  than R does (a YAML map for a named vector, rows for a data frame, `!r`
#  for an expression or a formula) and the grammar of `...` where `...` is
#  the function's body.  From it:
#    tfl_schema()        the schema, or one function's entry;
#    tfl_schema_check()  one item checked: its function, its arguments, their
#                        values;
#    .schema_to_r() / .schema_from_r()  an item's arguments as the spec holds
#                        them <-> the R values its call takes, the one place
#                        the two meet (tfl_content_code() writes the second).
# ============================================================================

.schema_cache <- new.env(parent = emptyenv())

.schema_all <- function(figure = TRUE) {
  if (is.null(.schema_cache$all)) {
    f <- system.file("spec", "schema.yml", package = "tflspec")
    all <- yaml::read_yaml(f)
    # the sheets' columns are no function: kept apart (.schema_sheets())
    .schema_cache$sheets <- all$.sheets
    all$.sheets <- NULL
    .schema_cache$all <- all
  }
  all <- .schema_cache$all
  if (!figure) return(all)
  # a figure's layers: from the figure designer's catalog as it is now (a
  # layer added with tfl_fig_add_layer() too; R/spec_figure.R)
  fig <- .schema_fig_entries()
  c(all, fig[setdiff(names(fig), names(all))])
}

#' The spec schema: the arguments a spec item may give, and their types
#'
#' A spec's `content:` lists items, one a function call ([tfl_item()]): the
#' function, its arguments by name.  The schema says what each takes --
#' the type of a value where the spec writes it differently from R (a YAML
#' map for a named vector, a list of rows for a data frame, `!r` for an
#' expression), and the grammar of `...` for the functions whose body is
#' `...` (`plan_cells()`, `plan_digits()`, `plan_sort()` and ten more).
#' The same file is what the GUI's help and the AI kit's schema read.
#'
#' The types, in a short notation: `string`, `integer`, `number`,
#' `logical`, `enum[a|b]`, `column`, `stat`, `template`, `r` (`!r`),
#' `formula` (`!r`, written `~ <text>`), `guard` (`!r`, a two-sided
#' `cond ~ "text"`), `call` and `call<fn>`
#' (`{fn:, args:}`), `list<T>` (a sequence), `vec<K, V>` (a map that is a
#' named vector in R), `map<K, V>` (a map that is a named list),
#' `rows<f1, f2, ...>` (a sequence of maps that is a data frame) and `T|U`
#' (either).
#'
#' @param fn A function's name (`"plan_cells"`), or `NULL` for all.
#' @return The function's entry (`package`, `description`, `dots`, `args`;
#'   `rank`, its place in the recommended order, and `stage`, `"mapping"`,
#'   `"finishing"` or `"output"`; `formals`, the function's own arguments
#'   but the data's (`x`, `plan`, `.plan`, `data`, `mapping`), and
#'   `required`, those of them with no default value),
#'   or the whole schema, a list by function; `NULL` for a function the
#'   schema does not have.
#' @examples
#' names(tfl_schema())
#' tfl_schema("plan_sort")$dots
#' @seealso [tfl_schema_check()]
#' @export
tfl_schema <- function(fn = NULL) {
  if (is.null(fn)) return(.schema_all())
  nm <- .schema_fn_name(fn)
  # the file's entries first; a figure's layer, made from the designer's
  # catalog, only for a name the file does not have
  ent <- .schema_all(figure = FALSE)[[nm]]
  # a catalog layer of ggplot2 (geom_line): its own entry over what the
  # installed layer takes -- its geom's and stat's parameters, its
  # aesthetics (they come through `...`)
  if (!is.null(ent) && identical(ent$figure, "layer") && grepl("^(geom|stat)_", nm)) {
    gg <- .schema_ggplot_entry(if (identical(ent$package, "ggplot2")) nm else paste0(ent$package, "::", nm))
    if (!is.null(gg)) {
      for (k in setdiff(names(gg$args), names(ent$args))) ent$args[[k]] <- gg$args[[k]]
      # the catalog's aesthetics (those it needs marked) and the layer's own
      cat_aes <- ent$aes %||% list()
      ent$aes_required <- names(Filter(function(a) isTRUE(a$required), cat_aes))
      ent$aes <- unique(c(names(cat_aes), gg$aes))
    }
  }
  ent <- ent %||% .schema_fig_entries()[[nm]] %||%
    # a ggplot2 layer function (geom_area, ggrepel::geom_text_repel): from
    # the installed package itself
    (if (grepl("^(geom|stat)_", nm)) .schema_ggplot_entry(fn))
  if (is.null(ent)) return(NULL)
  .schema_add_formals(ent, nm)
}

# The function's own formals, the data's arguments left out (`x`, `plan`,
# `.plan`, `data`, `mapping`), and `required`: those with no default value
# (`...` is never required).  From the installed package; none when it is
# not installed or has no such function.
.schema_data_args <- c("x", "plan", ".plan", "data", "mapping")
.schema_add_formals <- function(ent, nm) {
  pkg <- ent$package %||% "rtfreporter"
  f <- if (requireNamespace(pkg, quietly = TRUE)) {
    tryCatch(formals(getExportedValue(pkg, nm)), error = function(e) NULL)
  }
  if (is.null(f)) return(ent)
  f <- f[setdiff(names(f), .schema_data_args)]
  ent$formals <- names(f)
  ent$required <- names(f)[vapply(f, function(v) is.symbol(v) && !nzchar(as.character(v)), NA) &
                             names(f) != "..."]
  ent
}

# `rtfreporter::plan_cells` and `plan_cells` are one entry
.schema_fn_name <- function(fn) sub("^.*::", "", fn)

# ---- the type notation -------------------------------------------------------

# "list<vec<string, number>>|string" -> a parsed type: list(kind, args), the
# alternatives of a `|` as kind "either"
.schema_type <- function(txt) {
  txt <- trimws(txt)
  alts <- .schema_split(txt, "|")
  if (length(alts) > 1L) {
    return(list(kind = "either", args = lapply(alts, .schema_type)))
  }
  if (grepl("^enum\\[", txt)) {
    v <- sub("^enum\\[(.*)\\]$", "\\1", txt)
    return(list(kind = "enum", args = strsplit(v, "|", fixed = TRUE)[[1L]]))
  }
  m <- regmatches(txt, regexec("^([a-z_]+)<(.*)>$", txt))[[1L]]
  if (length(m)) {
    inner <- .schema_split(m[3L], ",")
    if (m[2L] %in% c("rows", "call")) {
      return(list(kind = m[2L], args = trimws(inner)))
    }
    return(list(kind = m[2L], args = lapply(inner, .schema_type)))
  }
  list(kind = txt, args = list())
}

# split at `sep` outside < > and [ ]
.schema_split <- function(x, sep) {
  ch <- strsplit(x, "")[[1L]]
  depth <- 0L
  out <- character()
  cur <- ""
  for (c in ch) {
    if (c %in% c("<", "[")) depth <- depth + 1L
    if (c %in% c(">", "]")) depth <- depth - 1L
    if (c == sep && depth == 0L) {
      out <- c(out, cur)
      cur <- ""
    } else {
      cur <- paste0(cur, c)
    }
  }
  trimws(c(out, cur))
}

.schema_scalars <- c("string", "integer", "number", "logical", "column", "stat",
                     "template", "enum")

# ---- checking an item ----------------------------------------------------------

#' Check one spec item against the schema
#'
#' One item of a spec's `content:` ([tfl_item()]): its function is one the
#' schema or its package knows; each argument is the function's (its
#' formals, or the grammar of its `...`); each value has the argument's
#' type.  What the reader of a spec file calls on every item, and what the
#' AI review reports.
#'
#' An argument the function does not have is a **warning**, not an error:
#' the last word is the installed package's formals when the code runs (a
#' newer package may have it).  An argument the schema marks `renamed_to`
#' says what it is now and since which version, in the words the GUI's
#' suggestion and the AI review also use.
#'
#' @param item A [tfl_item()] (or a list with `fn` and `args`).
#' @return The problems: a data frame (`fn`, `arg`, `level` -- `"error"` or
#'   `"warning"` -- and `message`), no rows when the item is fine.
#' @examples
#' tfl_schema_check(tfl_item("plan_sort", list(keys = c("-n", "SOC"))))
#' tfl_schema_check(tfl_item("plan_sort", list(keys = c("-n"), sort = 1)))
#' @seealso [tfl_schema()]
#' @export
tfl_schema_check <- function(item) {
  fn <- item$fn
  out <- data.frame(fn = character(), arg = character(), level = character(),
                    message = character(), stringsAsFactors = FALSE)
  # a figure's named plot: its items, each; a figure's R item: R
  if (inherits(item, "tfl_plot")) {
    return(do.call(rbind, c(list(out), lapply(item$items, tfl_schema_check))))
  }
  if (identical(fn, ".r")) return(out)
  bad <- function(arg, msg, level = "error") {
    out[nrow(out) + 1L, ] <<- list(fn, arg, level, msg)
  }
  if (!is.character(fn) || length(fn) != 1L || !nzchar(fn)) {
    bad("", "the item names no function")
    return(out)
  }
  ent <- tfl_schema(fn)
  # a figure's own ggplot2 layer: the layer function's aesthetics and
  # parameters (R/spec_figure.R)
  if (identical(fn, "geom") && identical(ent$figure, "layer")) .schema_check_geom(item, bad)
  f <- .schema_formals(fn, ent)
  if (is.null(ent) && is.null(f)) {
    bad("", sprintf("%s is a function neither the schema nor its package has", fn))
    return(out)
  }
  args <- item$args %||% list()
  dots <- ent$dots
  known <- setdiff(names(f) %||% character(), c("...", "plan", ".plan", "data", "x"))
  known <- unique(c(known, names(ent$args)))
  if (!is.null(dots) && identical(dots$style, "positional")) known <- c(known, dots$key)
  for (a in names(args)) {
    v <- args[[a]]
    if (inherits(v, "tfl_unset")) next
    # a figure's arrangement item says how it joins the plots (op: "&")
    if (identical(a, "op") && !is.null(ent$figure)) {
      if (!v %in% c("+", "&")) bad(a, "op is + or &")
      next
    }
    if (!is.null(ent$args[[a]]$renamed_to)) {
      bad(a, .schema_renamed_text(fn, a, ent$args[[a]]), "warning")
      next
    }
    if (a %in% names(ent$args) && !is.null(ent$args[[a]]$type)) {
      msg <- .schema_fits(v, .schema_type(ent$args[[a]]$type))
      if (!is.null(msg)) bad(a, msg)
    } else if (!is.null(dots) && identical(dots$style, "positional") && identical(a, dots$key)) {
      msg <- .schema_fits(v, .schema_type(dots$type))
      if (!is.null(msg)) bad(a, msg)
    } else if (!a %in% known) {
      if (!is.null(dots) && identical(dots$style, "named")) {
        msg <- .schema_fits(v, .schema_type(dots$type))
        if (!is.null(msg)) bad(a, msg)
      } else if (!is.null(f) && "..." %in% names(f) && is.null(ent)) {
        next                                 # a function of `...` the schema does not cover
      } else {
        bad(a, sprintf("%s has no argument `%s`%s", fn, a,
                       if (length(known)) paste0(" (it has: ", paste(known, collapse = ", "), ")") else ""),
            "warning")
      }
    }
  }
  out
}

# The one wording of a renamed argument: what the check, the GUI's
# suggestion and the AI review all say.
.schema_renamed_text <- function(fn, arg, a) {
  since <- a$until %||% a$since
  sprintf("%s(%s = ) is now `%s`%s.", fn, arg, a$renamed_to,
          if (!is.null(since)) paste0(" (since ", since, ")") else "")
}

# An argument the schema says arrived in a later version than the one
# installed (`since: "rtfreporter 0.8.2.9047"`): said as a warning before
# the code is written, never a stop -- the last word is the installed
# package's own formals when the code runs.
.schema_need_version <- function(item) {
  ent <- tfl_schema(item$fn)
  if (is.null(ent)) return(invisible(NULL))
  for (a in intersect(names(item$args), names(ent$args))) {
    since <- ent$args[[a]]$since
    if (is.null(since)) next
    pv <- strsplit(trimws(since), "[[:space:]]+")[[1L]]
    if (length(pv) != 2L || !requireNamespace(pv[1L], quietly = TRUE)) next
    if (utils::packageVersion(pv[1L]) < pv[2L]) {
      warning(sprintf(paste0(
        "%s(%s = ) is in %s >= %s; this one is %s, so the code may not run ",
        "until it is updated."), item$fn, a, pv[1L], pv[2L],
        as.character(utils::packageVersion(pv[1L]))), call. = FALSE)
    }
  }
  invisible(NULL)
}

# the function's formals, from its package (NULL when it is not installed)
.schema_formals <- function(fn, ent) {
  pkg <- if (grepl("::", fn, fixed = TRUE)) sub("::.*$", "", fn) else ent$package %||% "rtfreporter"
  nm <- .schema_fn_name(fn)
  if (!requireNamespace(pkg, quietly = TRUE)) return(NULL)
  ns <- asNamespace(pkg)
  if (!exists(nm, envir = ns, inherits = FALSE)) return(NULL)
  formals(get(nm, envir = ns))
}

# NULL when `v` (as the spec holds it) has the type, else what is wrong
.schema_fits <- function(v, ty) {
  ok <- .schema_ok(v, ty)
  if (ok) return(NULL)
  sprintf("is %s; it takes %s", .schema_what(v), .schema_show(ty))
}

.schema_ok <- function(v, ty) {
  k <- ty$kind
  if (k == "either") return(any(vapply(ty$args, function(t) .schema_ok(v, t), NA)))
  if (k %in% c("r", "formula", "guard")) return(inherits(v, c("tfl_r", "tfl_spec_expr", "tfl_spec_formula", "formula")))
  if (k == "call") {
    if (inherits(v, "cell_rows")) return("cell_rows" %in% unlist(ty$args))
    if (!inherits(v, c("tfl_call", "tfl_spec_call"))) return(FALSE)
    f <- if (inherits(v, "tfl_call")) v$fn else v$fun
    return(!length(ty$args) || .schema_fn_name(f) %in% ty$args)
  }
  if (k == "list") {
    if (inherits(v, c("tfl_r", "tfl_call"))) return(FALSE)
    el <- if (is.list(v)) v else as.list(v)
    return(all(vapply(el, function(e) .schema_ok(e, ty$args[[1L]]), NA)))
  }
  if (k %in% c("vec", "map")) {
    if (inherits(v, c("tfl_r", "tfl_call")) || is.data.frame(v)) return(FALSE)
    if (is.null(names(v)) || !length(v)) return(FALSE)
    el <- if (is.list(v)) v else as.list(v)
    return(all(vapply(el, function(e) .schema_ok(e, ty$args[[2L]]), NA)))
  }
  if (k == "rows") {
    if (is.data.frame(v)) return(all(names(v) %in% ty$args))
    return(is.list(v) && all(vapply(v, function(r) is.list(r) && all(names(r) %in% ty$args), NA)))
  }
  # a scalar
  if (is.list(v) || inherits(v, c("tfl_r", "tfl_call")) || length(v) != 1L) return(FALSE)
  switch(k,
    logical = is.logical(v),
    integer = is.numeric(v) && !is.na(v) && v == round(v),
    number = is.numeric(v),
    enum = as.character(v) %in% ty$args,
    is.character(v) || is.numeric(v) || is.logical(v))
}

.schema_show <- function(ty) {
  k <- ty$kind
  if (k == "either") return(paste(vapply(ty$args, .schema_show, ""), collapse = " or "))
  if (k == "enum") return(paste0("one of ", paste(ty$args, collapse = ", ")))
  if (k %in% c("rows", "call")) return(paste0(k, "<", paste(ty$args, collapse = ", "), ">"))
  if (length(ty$args)) return(paste0(k, "<", paste(vapply(ty$args, .schema_show, ""), collapse = ", "), ">"))
  k
}

.schema_what <- function(v) {
  if (inherits(v, "tfl_r")) return("an !r expression")
  if (inherits(v, "tfl_call")) return(paste0("a call of ", v$fn))
  if (is.data.frame(v)) return("a table")
  if (is.list(v)) return(if (is.null(names(v))) "a sequence" else "a map")
  if (length(v) != 1L) return(paste0("a sequence of ", length(v)))
  paste0(class(v)[1L], " ", format(v))
}

# ---- the spec's arguments <-> the call's ---------------------------------------

# An item's arguments as R takes them: `...` laid out (positional entries
# unnamed), each value converted by its type.
.schema_to_r <- function(fn, args) {
  ent <- tfl_schema(fn)
  dots <- ent$dots
  out <- list()
  for (a in names(args)) {
    v <- args[[a]]
    if (inherits(v, "tfl_unset")) {
      .ard_stop(sprintf(paste0(
        "%s: `%s` is !unset, which the spec's defaults should have resolved ",
        "before the code is written."), fn, a))
    }
    ty <- .schema_arg_type(ent, a)
    if (!is.null(dots) && identical(dots$style, "positional") && identical(a, dots$key)) {
      v <- .schema_value_to_r(v, ty)
      el <- if (is.list(v) && !is.object(v) && is.null(names(v))) v else list(v)
      out <- c(out, el)
      names(out)[length(out) - length(el) + seq_along(el)] <- ""
      next
    }
    out[[a]] <- .schema_value_to_r(v, ty)
  }
  out
}

# The other way: a call's arguments as the spec holds them.
.schema_from_r <- function(fn, args) {
  ent <- tfl_schema(fn)
  dots <- ent$dots
  nm <- names(args) %||% rep("", length(args))
  pos <- args[!nzchar(nm)]
  out <- list()
  # a verb that takes its entries as one named vector (`map: true`:
  # plan_labels(c(AGE = "Age")), as rtfreporter reads it): the entries
  if (length(pos) == 1L && !is.null(dots) && isTRUE(dots$map) &&
      length(pos[[1L]]) && !is.null(names(pos[[1L]])) && all(nzchar(names(pos[[1L]])))) {
    m <- as.list(pos[[1L]])
    args <- c(m, args[nzchar(nm)])
    nm <- names(args)
    pos <- list()
  }
  if (length(pos)) {
    if (is.null(dots) || !identical(dots$style, "positional")) {
      .ard_stop(sprintf("%s: an argument with no name, which the schema cannot place.", fn))
    }
    v <- if (length(pos) == 1L) pos[[1L]] else pos
    out[[dots$key]] <- .schema_value_from_r(v, .schema_type(dots$type))
  }
  for (a in nm[nzchar(nm)]) {
    out[[a]] <- .schema_value_from_r(args[[a]], .schema_arg_type(ent, a))
  }
  out
}

.schema_arg_type <- function(ent, a) {
  t <- ent$args[[a]]$type
  if (!is.null(t)) return(.schema_type(t))
  d <- ent$dots
  if (!is.null(d) && (identical(d$style, "named") || identical(a, d$key))) {
    return(.schema_type(d$type))
  }
  NULL
}

# a value as R takes it, from the spec's form
.schema_value_to_r <- function(v, ty) {
  if (inherits(v, "tfl_r")) {
    txt <- as.character(unclass(v))
    # a guard (`n == 0 ~ "0"`) is the formula itself; a `where` is
    # written `~ <text>`; anything else is an expression
    if (.schema_has(ty, "guard") && grepl("~", txt, fixed = TRUE)) {
      return(stats::as.formula(txt, env = globalenv()))
    }
    if (.schema_has(ty, "formula")) return(.spec_formula(txt))
    return(.spec_expr(txt))
  }
  if (inherits(v, "tfl_call")) {
    a <- .schema_to_r(v$fn, v$args)
    if (identical(.schema_fn_name(v$fn), "cell_rows")) {
      .spec_need_rtfreporter()
      return(do.call(rtfreporter::cell_rows, lapply(a, .schema_eval_guards)))
    }
    return(do.call(.spec_call, c(list(.schema_call_fun(v)), a)))
  }
  if (is.null(ty)) return(v)
  k <- ty$kind
  if (k == "either") {
    for (t in ty$args) if (.schema_ok(v, t)) return(.schema_value_to_r(v, t))
    return(v)
  }
  if (k == "list") {
    el <- ty$args[[1L]]
    if (is.list(v) && !is.object(v)) {
      w <- lapply(v, .schema_value_to_r, ty = el)
      if (.schema_scalar(el) && all(vapply(w, function(e) is.atomic(e) && length(e) == 1L, NA))) {
        return(.schema_coerce(unlist(w, use.names = FALSE), el))
      }
      return(w)
    }
    return(if (.schema_scalar(el)) .schema_coerce(v, el) else v)
  }
  if (k == "vec") {
    vt <- ty$args[[2L]]
    w <- if (is.list(v)) unlist(lapply(v, .schema_value_to_r, ty = vt)) else v
    return(.schema_coerce(w, vt))
  }
  if (k == "map") {
    vt <- ty$args[[2L]]
    return(lapply(v, .schema_value_to_r, ty = vt))
  }
  if (k == "rows") {
    if (is.data.frame(v)) return(v)
    cols <- ty$args
    have <- cols[vapply(cols, function(cn) any(vapply(v, function(r) !is.null(r[[cn]]), NA)), NA)]
    df <- as.data.frame(lapply(stats::setNames(have, have), function(cn)
      vapply(v, function(r) if (is.null(r[[cn]])) NA_character_ else as.character(r[[cn]]), "")),
      stringsAsFactors = FALSE)
    return(df)
  }
  .schema_coerce(v, ty)
}

# a value as the spec holds it, from R's
.schema_value_from_r <- function(v, ty) {
  if (inherits(v, "tfl_spec_expr")) return(tfl_r(v$text))
  if (inherits(v, "tfl_spec_formula")) return(tfl_r(v$text))
  if (inherits(v, "formula")) return(tfl_r(paste(deparse(v, width.cutoff = 500L), collapse = " ")))
  if (inherits(v, "tfl_spec_call")) {
    return(tfl_call(v$fun, .schema_from_r(v$fun, v$args)))
  }
  if (inherits(v, "cell_rows")) {
    a <- unclass(v)
    a <- lapply(a, function(e) if (is.list(e)) lapply(e, .schema_value_from_r, ty = NULL) else e)
    return(tfl_call("cell_rows", a))
  }
  if (is.data.frame(v)) {
    return(lapply(seq_len(nrow(v)), function(i) {
      r <- as.list(v[i, , drop = FALSE])
      r[!vapply(r, function(e) length(e) == 1L && is.na(e), NA)]
    }))
  }
  if (is.null(ty)) return(v)
  k <- ty$kind
  if (k == "either") {
    for (t in ty$args) if (.schema_ok(v, t)) return(.schema_value_from_r(v, t))
    return(v)
  }
  if (k == "vec" && is.atomic(v) && !is.null(names(v))) return(as.list(v))
  if (k == "list" && is.list(v) && !is.object(v)) return(lapply(v, .schema_value_from_r, ty = ty$args[[1L]]))
  if (k == "map" && is.list(v)) return(lapply(v, .schema_value_from_r, ty = ty$args[[2L]]))
  v
}

# A call's function as the code writes it: bare for rtfreporter's and
# tflspec's (the program attaches rtfreporter), `pkg::fn` for another
# package's (cards::continuous_summary_fns), as the call or its schema
# entry names the package.
.schema_call_fun <- function(v) {
  fn <- .schema_fn_name(v$fn)
  if (grepl("::", v$fn, fixed = TRUE)) {
    pkg <- sub("::.*$", "", v$fn)
  } else {
    pkg <- v$package %||% tfl_schema(fn)$package %||% "rtfreporter"
  }
  if (pkg %in% c("rtfreporter", "tflspec")) fn else paste0(pkg, "::", fn)
}

# guards inside cell_rows(): a `!r` there is a formula object, as R writes them
.schema_eval_guards <- function(e) {
  if (is.list(e) && !is.object(e)) {
    return(lapply(e, function(x) if (inherits(x, "tfl_spec_formula") || inherits(x, "tfl_spec_expr"))
      stats::as.formula(x$text, env = globalenv()) else x))
  }
  e
}

.schema_has <- function(ty, kind) {
  if (is.null(ty)) return(FALSE)
  if (identical(ty$kind, kind)) return(TRUE)
  any(vapply(ty$args, function(t) is.list(t) && .schema_has(t, kind), NA))
}

.schema_scalar <- function(ty) ty$kind %in% .schema_scalars

.schema_coerce <- function(v, ty) {
  if (is.null(ty) || is.list(v)) return(v)
  k <- ty$kind
  if (k == "either") {
    for (t in ty$args) if (.schema_ok(v, t) || (is.atomic(v) && all(vapply(as.list(v), function(e) .schema_ok(e, t), NA)))) return(.schema_coerce(v, t))
    return(v)
  }
  nm <- names(v)
  out <- switch(k,
    integer = if (is.numeric(v) && all(is.na(v) | v == round(v))) as.integer(v) else v,
    number = if (is.numeric(v)) as.numeric(v) else v,
    logical = v,
    v)
  names(out) <- nm
  out
}

# The spec sheets' columns, as the schema file says them: one entry a sheet
# (its description, its columns in order, each a description and an
# example, and `at:` the argument it is)
.schema_sheets <- function() {
  if (is.null(.schema_cache$all)) .schema_all(figure = FALSE)
  .schema_cache$sheets
}

#' The help of a spec item's argument
#'
#' What the GUI's `?` and the AI kit say about one argument of a spec item:
#' the schema's description of it, or, for an argument a spec sheet's
#' column also holds (`rtf_page(paper_size = )` is the `page` sheet's
#' `paper_size`), that column's help -- one text, wherever it is read.
#'
#' @param fn A function of the spec (`"rtf_page"`, `"plan_cells"`).
#' @param arg One of its arguments.
#' @return The text, or `NA` when the schema says nothing of it.
#' @examples
#' tfl_schema_help("rtf_page", "orientation")
#' @seealso [tfl_schema()], [tfl_spec_columns()]
#' @export
tfl_schema_help <- function(fn, arg) {
  ent <- tfl_schema(fn)
  d <- ent$args[[arg]]$description
  if (!is.null(d)) return(d)
  key <- paste0(.schema_fn_name(fn), ".", arg)
  for (sh in .schema_sheets()) {
    for (cl in sh$columns) if (identical(cl$at, key)) return(cl$description)
  }
  NA_character_
}
