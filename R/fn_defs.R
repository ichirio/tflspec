# ============================================================================
#  Function and object definitions: what a process card calls, what an
#  object card holds
# ----------------------------------------------------------------------------
#  canon 1748456, 0a237f3, f3c4527 (object cards and process cards): a step
#  (data, analysis, content) is a list of object cards (To, From), each a
#  pipe of process cards.  The join of a process card (`|>` or ggplot2's
#  `+`) and the argument the pipe fills come from the function's definition
#  (tfl_fn_defs()); the default names of the objects a step holds from the
#  object definitions (tfl_object_defs()).  The page step is out of both
#  (7e56ab2): no rtf_* rows, no page rows.
# ============================================================================

.fn_def_steps <- c("data", "analysis", "content")
.fn_def_joins <- c("|>", "+")
.fn_def_returns <- c("data", "ard", "model", "value", "object", "plot", "plan", "content",
                     "doc", "unknown")
.fn_def_cols <- c("fn", "step", "label_en", "label_ja", "join", "first_arg_name", "returns")

# the arrangement of the plots (B-11): a plot, the first call of its object
.fn_def_plot_fns <- c("tfl_arrange")

.fn_def_cache <- new.env(parent = emptyenv())

# the step names, for a step argument; NULL is every step
.fn_def_check_step <- function(step, steps, what) {
  if (is.null(step)) return(invisible(NULL))
  if (!is.character(step) || length(step) != 1L || is.na(step) || !step %in% steps) {
    stop(sprintf("%s: step %s is not one of %s.", what,
                 encodeString(paste(as.character(step), collapse = ", "), quote = "\""),
                 paste(steps, collapse = ", ")), call. = FALSE)
  }
  invisible(step)
}

# The first formal of pkg::fn, from the installed package; else the first
# argument the schema entry writes (but `...`); NA when neither has one.
.fn_def_first_formal <- function(fn, pkg, args = NULL) {
  f <- NULL
  if (is.character(pkg) && length(pkg) == 1L && nzchar(pkg) &&
      requireNamespace(pkg, quietly = TRUE)) {
    ns <- asNamespace(pkg)
    if (exists(fn, envir = ns, inherits = FALSE)) {
      obj <- get(fn, envir = ns, inherits = FALSE)
      if (is.function(obj)) f <- names(formals(obj))
    }
  }
  if (is.null(f)) f <- setdiff(names(args %||% list()), "...")
  if (length(f)) f[1L] else NA_character_
}

# One exception key of a schema entry (`join:`, `first_arg_name:`,
# `returns:`), checked: NULL when it is not written.
.fn_def_key <- function(ent, key, allowed, fn) {
  v <- ent[[key]]
  if (is.null(v)) return(NULL)
  if (!is.character(v) || length(v) != 1L || is.na(v) || !nzchar(v) ||
      (!is.null(allowed) && !v %in% allowed)) {
    stop(sprintf("schema %s: %s: %s is not one of %s.", fn, key,
                 encodeString(paste(as.character(unlist(v)), collapse = ", "), quote = "\""),
                 if (is.null(allowed)) "an argument's name" else paste(allowed, collapse = ", ")),
         call. = FALSE)
  }
  v
}

# The table built from a schema (a list by function, as schema.yml) and an
# analysis method catalog (tfl_ard_methods()'s columns `method`, `label`,
# `call`).  The rules:
#   step     a cards / cardx function is analysis, the rest content
#   join     a `figure:` kind other than plot is `+`, the rest `|>`
#   first_arg_name  the R function's first formal for `|>`, NA for `+`
#   returns  plan_* and table_plan plan; a plot (figure: plot), tfl_arrange
#            and a `+` layer plot; an analysis method ard; else unknown
# An entry's own `join:`, `first_arg_name:` or `returns:` wins.
.fn_defs_build <- function(schema, methods) {
  rows <- list()
  add <- function(fn, step, label, join, first, returns) {
    rows[[length(rows) + 1L]] <<- data.frame(
      fn = fn, step = step, label_en = label[["en"]], label_ja = label[["ja"]], join = join,
      first_arg_name = if (identical(join, "+")) NA_character_ else as.character(first),
      returns = returns, stringsAsFactors = FALSE)
  }
  # the data row: one function of its keys (rule 1)
  add("data_row", "data", c(en = "Data row", ja = NA_character_), "|>", "from", "data")
  # the analysis methods: the cards / cardx functions behind `method:`
  calls <- as.character(methods$call)
  is_fn <- grepl("::", calls, fixed = TRUE)
  m_fn <- .schema_fn_name(calls[is_fn])
  m_label <- as.character(methods$label)[is_fn]
  keep <- !duplicated(m_fn)
  m_fn <- m_fn[keep]
  m_label <- m_label[keep]
  fns <- names(schema)
  fns <- fns[!startsWith(fns, ".") & !startsWith(fns, "rtf_")]
  for (st in .fn_def_steps[-1L]) {
    for (fn in fns) {
      ent <- schema[[fn]]
      if (!is.list(ent)) next
      pkg <- as.character(ent$package %||% "rtfreporter")
      fstep <- if (pkg %in% c("cards", "cardx")) "analysis" else "content"
      if (!identical(fstep, st)) next
      label <- .arg_en_ja(ent[["label"]], sprintf("schema %s", fn), "label")
      kind <- ent[["figure"]]
      join <- .fn_def_key(ent, "join", .fn_def_joins, fn) %||%
        (if (!is.null(kind) && !identical(kind, "plot")) "+" else "|>")
      method <- fn %in% m_fn
      if (method && is.na(label[["ja"]]) && is.na(label[["en"]])) {
        label[["en"]] <- m_label[match(fn, m_fn)]
      }
      returns <- .fn_def_key(ent, "returns", .fn_def_returns, fn) %||%
        (if (startsWith(fn, "plan_") || identical(fn, "table_plan")) "plan"
         else if (identical(kind, "plot") || fn %in% .fn_def_plot_fns || identical(join, "+")) "plot"
         else if (method) "ard"
         else "unknown")
      first <- .fn_def_key(ent, "first_arg_name", NULL, fn) %||%
        (if (identical(join, "+")) NA_character_
         else if (method) "data"
         else .fn_def_first_formal(fn, pkg, ent$args))
      add(fn, fstep, label, join, first, returns)
    }
    if (identical(st, "analysis")) {
      for (i in which(!m_fn %in% fns)) {
        add(m_fn[i], "analysis", c(en = m_label[i], ja = NA_character_), "|>", "data", "ard")
      }
    }
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out[.fn_def_cols]
}

#' The definitions of the functions a card calls
#'
#' One row a function a process card can call: the join that puts it after
#' the card above it (`|>` or ggplot2's `+`), the argument the pipe fills,
#' and the kind of object it gives.  The screen (tflplanner) reads it to
#' show or lock a call's first argument and to draw the join; the program
#' writer, to write the call.  The page step's functions (`rtf_*`) are not
#' in it: the page keeps its own screen.
#'
#' The rows: every function of the schema (`inst/spec/schema.yml`) but
#' `rtf_*` -- the `plan_*()` functions, `table_plan()`, the figure's plots,
#' layers, scales and the rest --, the data row (`fn = "data_row"`), and
#' each analysis method of the built-in catalog ([tfl_ard_methods()]) by
#' the function its code calls (`cards::ard_summary` is `"ard_summary"`).
#'
#' The values come from rules, so few are written by hand: a function of
#' `cards` or `cardx` is the analysis step, the rest content; a figure's
#' function of a kind other than `plot` (a layer, a scale, a theme ...) is
#' joined with `+`, every other with `|>`; `first_arg_name` is the R
#' function's first formal; `plan_*()` and `table_plan()` give a `plan`, a
#' plot, `tfl_arrange()` and a `+` layer a `plot`, an analysis method an
#' `ard`.  A schema entry's own `join:`, `first_arg_name:` or `returns:`
#' wins; an unknown `join` or `returns` word stops, naming the words.
#'
#' @param step `NULL` for every row, or one step: `"data"`, `"analysis"` or
#'   `"content"`.  Another stops, naming the steps.
#' @return A data frame, one row a function, in the order data, analysis,
#'   content (the schema's order within a step).  The columns:
#'   \describe{
#'     \item{fn}{the function's name, without its package.}
#'     \item{step}{`data`, `analysis` or `content`.}
#'     \item{label_en, label_ja}{its label, from the schema entry's
#'       `label: {en: , ja: }` (an analysis method's catalog label); `NA`
#'       when there is none.}
#'     \item{join}{`|>` or `+`: how the call follows the one above it.}
#'     \item{first_arg_name}{the argument the pipe fills (the field a call
#'       with From does not show); `NA` for `+`.}
#'     \item{returns}{what it gives, one of `data`, `ard`, `model`, `value`,
#'       `object`, `plot`, `plan`, `content`, `doc`, `unknown`.}
#'   }
#' @seealso [tfl_object_defs()] (the objects a step holds), [tfl_arg_defs()]
#'   (a card's arguments; its attribute `"fn"` is the function's row here).
#' @examples
#' d <- tfl_fn_defs()
#' d[d$fn %in% c("data_row", "ard_summary", "table_plan", "plan_cells", "ggplot",
#'               "geom_line", "labs"), ]
#' head(tfl_fn_defs("analysis"))
#' @export
tfl_fn_defs <- function(step = NULL) {
  .fn_def_check_step(step, .fn_def_steps, "tfl_fn_defs()")
  if (is.null(.fn_def_cache$fn)) {
    .fn_def_cache$fn <- .fn_defs_build(.schema_all(figure = FALSE), .ard_methods_builtin())
  }
  out <- .fn_def_cache$fn
  if (!is.null(step)) {
    out <- out[out$step == step, , drop = FALSE]
    rownames(out) <- NULL
  }
  out
}

# the one row of tfl_fn_defs() of a card's function (tfl_arg_defs()'s
# attribute "fn"); a data row card is the data_row function.  NULL when
# there is none.
.fn_def_row <- function(fn) {
  nm <- if (identical(fn, "data")) "data_row" else .schema_fn_name(fn)
  d <- tfl_fn_defs()
  row <- d[d$fn == nm, , drop = FALSE]
  if (!nrow(row)) return(NULL)
  rownames(row) <- NULL
  row
}

# ---- object definitions -------------------------------------------------------

# An object's name: R's, not a reserved word (`make.names(x) == x`)
.object_name_re <- "^[.A-Za-z][A-Za-z0-9._]*$"
.object_name_ok <- function(x) {
  x <- as.character(x)
  !is.na(x) & grepl(.object_name_re, x) & make.names(x) == x
}

#' The definitions of the objects a step holds
#'
#' One row a kind of object an object card of a step holds: its default
#' name, whether the name can be changed, the pattern a name must match, and
#' whether it is the step's result (the object the next step reads).  The
#' page step has no object cards (it keeps its own screen).
#'
#' The rows:
#' \itemize{
#'   \item data, a data row: `{dataset}_sub` (the dataset's name in lower
#'     case; `_sub2`, `_sub3` ... when the name is taken).
#'   \item analysis, an analysis item: `.ard_{id}`.
#'   \item analysis, the result: `ard`.
#'   \item content, a table's or a listing's result: `plan`.
#'   \item content, a figure's result: `content`.
#'   \item each step, a new object: `{kind}_{n}`, `{kind}` the kind of
#'     object its last call gives (`plot_1`), `obj` when it is not known
#'     (`obj_1`).
#' }
#' Every name can be changed, the results' too: a default is a default only.
#' A name matches `pattern` and is not one of R's reserved words
#' (`make.names(name) == name`).
#'
#' @param step `NULL` for every row, or one step: `"data"`, `"analysis"` or
#'   `"content"`.  Another stops, naming the steps.
#' @return A data frame, one row a kind of object.  The columns:
#'   \describe{
#'     \item{step}{`data`, `analysis` or `content`.}
#'     \item{default_name}{the default name, a pattern whose `{...}` are
#'       filled in (`{dataset}`, `{id}`, `{kind}`, `{n}`).}
#'     \item{fixed}{`TRUE` when the name cannot be changed (none can).}
#'     \item{pattern}{the regular expression a name matches.}
#'     \item{final}{`TRUE` for the step's result.}
#'   }
#' @seealso [tfl_fn_defs()] (the functions a process card calls).
#' @examples
#' tfl_object_defs()
#' tfl_object_defs("analysis")
#' @export
tfl_object_defs <- function(step = NULL) {
  .fn_def_check_step(step, .fn_def_steps, "tfl_object_defs()")
  out <- data.frame(
    step = c("data", "analysis", "analysis", "content", "content", .fn_def_steps),
    default_name = c("{dataset}_sub", ".ard_{id}", "ard", "plan", "content",
                     rep("{kind}_{n}", 3L)),
    fixed = FALSE,
    pattern = .object_name_re,
    final = c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE)
  out <- out[order(match(out$step, .fn_def_steps)), , drop = FALSE]
  if (!is.null(step)) out <- out[out$step == step, , drop = FALSE]
  rownames(out) <- NULL
  out
}
