# The YAML spec (design: _archive/spec-format-redesign-2026-10-14.md).
#
# A study is study.yml and spec/: data.yml, defaults.yml, codelists.yml,
# patterns/<name>.yml, toc.yml and outputs/<id>.yml -- one file a report.
# A report file is `data:` (what it reads), `content:` (an ordered list of
# layers, one item one call of the target package: a plan verb, a listing
# column, a ggplot2 piece) and `page:` (the rtfreporter document).  Every
# item's keys are the function's arguments, so a reader can check them.
#
# Stage 0 (shared with S1): the R objects and a reader / writer of one
# report file.  `extends` is read, not resolved yet (stage 1).

#' R objects of the YAML spec
#'
#' The values a spec may hold that are not plain data, and the pieces a
#' report is made of:
#'
#' * `tfl_r()`: a string of R code, written as the `!r` tag (`where: !r
#'   "p > 0.5"`); kept as it is and put into the program unquoted.
#' * `tfl_unset()`: the `!unset` tag -- a value a report sets to nothing,
#'   overriding its defaults (writing nothing leaves the default).  It is
#'   resolved when the defaults are layered; the generators never see one.
#' * `tfl_call()`: a nested call (`{fn: rtf_border, args: {top: single}}`),
#'   for an argument that is itself a constructor's result.
#' * `tfl_item()`: one layer of `content:` -- a function (`fn`), its
#'   arguments by name (`args`), and where it came from (`from`: `"own"`,
#'   `"defaults"` or `"pattern:<name>"`).
#' * `tfl_output()`: one report: `id`, `kind`, `label`, `population`,
#'   `extends`, `data`, `codelists`, `content` (a list of `tfl_item`) and
#'   `page`.
#'
#' @param code One string of R code.
#' @param fn A function's name (`plan_cells`, `cards::ard_summary`,
#'   `rtf_border`).
#' @param args The arguments, a named list.
#' @param package The package `fn` is in, when its name does not say.
#' @param from Where an item came from: `"own"`, `"defaults"`,
#'   `"pattern:<name>"`, `"template:<name>"`.
#' @param id The report's id (letters, digits, `.`, `_`, `-`); the file is
#'   `spec/outputs/<id>.yml`.
#' @param kind `"table"`, `"figure"` or `"listing"`.
#' @param label,population The report's label and analysis set (`NA`: none).
#' @param extends The files layered under this one, nearest first
#'   (`"defaults"`, `"patterns/<name>"`); `"none"` for none.
#' @param data What the report reads, its step 1 (a list: `source` --
#'   `"ard"`, `"code"` or `"dataset"` -- and `object`, `steps`, `code`,
#'   `dataset` ...).
#' @param ard The report's part of the study ARD, made in the ARD stage
#'   before any report (a list: `analysis_data`, `analyses`); `NULL` none.
#' @param codelists The report's code lists, by variable.
#' @param content The layers, a list of `tfl_item`.
#' @param page The document: `titles`, `footnotes`, `header`, `footer`
#'   (maps by line), `tokens`, and the `rtf_*` calls' arguments.
#' @param template A template SPEC, `list(name = , args = )`: the report
#'   is the template's items ([tfl_templates()]) with `ard`, `data` and
#'   `content` adding to them or changing them ([tfl_resolve_output()],
#'   [tfl_template_to_spec()]); `NULL` none.
#' @return `tfl_r()`: a string of class `tfl_r`; `tfl_unset()`: an object
#'   of class `tfl_unset`; the rest: lists of class `tfl_call`, `tfl_item`,
#'   `tfl_output`.
#' @examples
#' it <- tfl_item("plan_cells", list(continuous = list(n = "{N}")))
#' out <- tfl_output("T-1", "table", content = list(it))
#' out$content[[1]]$fn
#' @export
tfl_r <- function(code) {
  if (!is.character(code) || !length(code) || anyNA(code)) {
    stop("tfl_r(): `code` is a string of R.", call. = FALSE)
  }
  structure(paste(code, collapse = "
"), class = "tfl_r", tag = "!r")
}

#' @rdname tfl_r
#' @export
tfl_unset <- function() structure("", class = "tfl_unset", tag = "!unset")

#' @rdname tfl_r
#' @export
tfl_call <- function(fn, args = list(), package = NULL) {
  .check_fn(fn, "tfl_call()")
  .check_args(args, "tfl_call()")
  structure(list(fn = fn, package = package, args = args), class = "tfl_call")
}

#' @rdname tfl_r
#' @export
tfl_item <- function(fn, args = list(), from = "own") {
  .check_fn(fn, "tfl_item()")
  .check_args(args, "tfl_item()")
  if (!is.character(from) || length(from) != 1L ||
      !(from %in% c("own", "defaults") || startsWith(from, "pattern:") ||
        startsWith(from, "template:"))) {
    stop("tfl_item(): `from` is \"own\", \"defaults\", \"pattern:<name>\" or \"template:<name>\".",
         call. = FALSE)
  }
  structure(list(fn = fn, args = args, from = from), class = "tfl_item")
}

#' @rdname tfl_r
#' @export
tfl_output <- function(id, kind = c("table", "figure", "listing"),
                       label = NA_character_, population = NA_character_,
                       extends = "defaults", data = NULL, codelists = list(),
                       content = list(), page = list(), ard = NULL, template = NULL) {
  if (!is.character(id) || length(id) != 1L || is.na(id) ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", id)) {
    stop("tfl_output(): `id` is letters, digits, '.', '_' or '-' (T-14-1-1).",
         call. = FALSE)
  }
  kind <- match.arg(kind)
  if (!inherits(content, "tfl_r") &&
      (!is.list(content) || !all(vapply(content, inherits, NA, "tfl_item")))) {
    stop("tfl_output(): `content` is a list of tfl_item(), or one tfl_r() (the content as code).",
         call. = FALSE)
  }
  if (!is.character(extends) || !length(extends)) {
    stop("tfl_output(): `extends` is a character vector (\"defaults\", \"patterns/<name>\", \"none\").",
         call. = FALSE)
  }
  out <- structure(list(id = id, kind = kind,
                        label = .na_scalar(label), population = .na_scalar(population),
                        extends = extends, ard = ard, data = data, codelists = codelists,
                        content = content, page = page),
                   class = "tfl_output")
  if (!is.null(template)) out$template <- template
  out
}

.check_fn <- function(fn, who) {
  if (!is.character(fn) || length(fn) != 1L || is.na(fn) || !nzchar(fn)) {
    stop(who, ": `fn` is a function's name.", call. = FALSE)
  }
}
.check_args <- function(args, who) {
  if (!is.list(args) || (length(args) && (is.null(names(args)) || any(!nzchar(names(args)))))) {
    stop(who, ": `args` is a named list.", call. = FALSE)
  }
}
.na_scalar <- function(x) {
  if (is.null(x) || !length(x) || is.na(x[1L]) || !nzchar(x[1L])) NA_character_ else as.character(x[1L])
}

#' @export
print.tfl_output <- function(x, ...) {
  cat("<tfl_output> ", x$id, " (", x$kind, ")",
      if (!is.na(x$label)) paste0(": ", x$label), "\n", sep = "")
  cat("  extends: ", paste(x$extends, collapse = ", "), "\n", sep = "")
  if (length(x$ard$analyses)) {
    cat("  ard: ", paste(vapply(x$ard$analyses, function(a) a$id %||% "?", ""), collapse = ", "),
        "\n", sep = "")
  }
  if (!is.null(.tpl_spec_of(x))) cat("  template: ", x$template$name, "\n", sep = "")
  if (inherits(x$content, "tfl_r")) {
    cat("  content: (R code)\n")
  } else if (length(x$content)) {
    cat("  content:\n")
    for (it in x$content) {
      cat("    - ", it$fn, "(", paste(names(it$args), collapse = ", "), ")",
          if (it$from != "own") paste0("  # ", it$from), "\n", sep = "")
    }
  }
  invisible(x)
}

#' @export
print.tfl_item <- function(x, ...) {
  cat("<tfl_item> ", x$fn, "(", paste(names(x$args), collapse = ", "), ")",
      if (x$from != "own") paste0("  # ", x$from), "\n", sep = "")
  invisible(x)
}

# -- reading ----------------------------------------------------------------

#' Read and write a report's YAML file
#'
#' `tfl_read_output()` reads `spec/outputs/<id>.yml`: the `!r` and `!unset`
#' tags become [tfl_r()] and [tfl_unset()], a `{fn:, args:}` map a
#' [tfl_call()], and each item of `content:` (`- plan_cells: {...}`) a
#' [tfl_item()] with `from = "own"`.  A file whose top is `outputs:` holds
#' several reports (a family written in one go): a list of them comes
#' back, each with the file's `base:` merged under it as a plain list (the
#' layering with `defaults` is not done here).  `tfl_write_output()`
#' writes one report back in the same form.
#'
#' @param path The file.
#' @param x A `tfl_output`.
#' @return `tfl_read_output()`: a `tfl_output`, or a list of them.
#'   `tfl_write_output()`: `path`, invisibly.
#' @examples
#' f <- system.file("extdata", "spec-yaml", "outputs", "T-14-1-1.yml", package = "tflspec")
#' out <- tfl_read_output(f)
#' out$content[[1]]
#' @export
tfl_read_output <- function(path) {
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) {
    stop("tfl_read_output(): no file ", path, call. = FALSE)
  }
  y <- yaml::read_yaml(path, handlers = .yaml_handlers())
  if (!is.list(y)) stop("tfl_read_output(): ", path, " is not a map.", call. = FALSE)
  if (!is.null(y$outputs)) {
    base <- y$base %||% list()
    return(lapply(y$outputs, function(o) .output_from_list(.merge_plain(base, o), path)))
  }
  .output_from_list(y, path)
}

# The tags: !r, !unset; and YAML 1.1's implicit booleans (n, N, y, Y, yes,
# no, on, off) kept as the text they are -- a statistic is called n or N --
# so that only true / false become logical, as keys and as values
.yaml_handlers <- function() {
  keep <- function(x) {
    x <- as.character(x)
    if (tolower(x) %in% c("true", "false")) return(identical(tolower(x), "true"))
    x
  }
  list(r = function(x) tfl_r(as.character(x)),
       unset = function(x) tfl_unset(),
       `bool#yes` = keep, `bool#no` = keep,
       seq = .yaml_seq)
}

# A sequence: the reader folds one of scalars into a vector (as the yaml
# package does), but one holding an !r, a call or a map stays a list, so
# that a guard's R keeps its class
.yaml_seq <- function(x) {
  if (!length(x)) return(list())
  keep <- any(vapply(x, function(e) inherits(e, c("tfl_r", "tfl_call", "tfl_unset")) || is.list(e), NA))
  if (keep) return(x)
  if (all(vapply(x, function(e) is.atomic(e) && length(e) == 1L, NA)) &&
      length(unique(vapply(x, function(e) class(e)[1L], ""))) == 1L) return(unlist(x))
  x
}

# the plain list of a report file as a tfl_output
.output_from_list <- function(y, path = "") {
  need <- c("id", "kind")
  miss <- need[!need %in% names(y)]
  if (length(miss)) {
    stop("tfl_read_output(): ", path, " lacks ", paste(miss, collapse = ", "), ".",
         call. = FALSE)
  }
  if (nzchar(path) && is.null(attr(y, "multi")) &&
      !identical(tools::file_path_sans_ext(basename(path)), y$id) &&
      !grepl("^outputs?$", basename(path))) {
    # (a multi-report file is named as its author likes)
  }
  # the ARD's analyses are the report's ard: section (2026-10-15), not data:'s
  old <- intersect(c("analysis_data", "analyses"), names(y$data))
  if (length(old)) {
    stop("tfl_read_output(): ", y$id, "'s data: has ", paste(old, collapse = " and "),
         "; a report's ARD analyses are its ard: section (ard: {analysis_data, analyses}).",
         call. = FALSE)
  }
  content <- if (inherits(y$content, "tfl_r")) y$content else
    lapply(y$content %||% list(), .item_from_list)
  extends <- y$extends %||% "defaults"
  out <- tfl_output(
    id = y$id, kind = y$kind, label = y$label, population = y$population,
    extends = as.character(unlist(extends)),
    ard = if (!is.null(y$ard)) .calls_in(y$ard),
    data = .data_steps_in(.calls_in(y$data)), codelists = y$codelists %||% list(),
    content = content, page = .calls_in(y$page %||% list()))
  for (k in setdiff(names(y), c(.output_fields, "outputs", "base"))) out[[k]] <- .calls_in(y[[k]])
  out
}

# `- plan_cells: {...}` (a map of one key) as a tfl_item
.item_from_list <- function(el) {
  # a figure's item that is R (`- !r p + annotate(...)`)
  if (inherits(el, "tfl_r")) return(tfl_item(".r", list(code = el), "own"))
  if (is.list(el) && length(el) == 1L && !is.null(names(el)) && nzchar(names(el))) {
    args <- el[[1L]]
    if (is.null(args)) args <- list()
    # a figure's named plot: `- km: [items]`, a sequence of items
    if (is.list(args) && length(args) && is.null(names(args)) &&
        all(vapply(args, function(a) is.list(a) || inherits(a, "tfl_r"), NA))) {
      return(tfl_plot(names(el), lapply(args, .item_from_list)))
    }
    if (!is.list(args)) {
      stop("A content item is `- fn: {args}`; `", names(el), "` has a value that is not a map.",
           call. = FALSE)
    }
    return(tfl_item(names(el), .calls_in(args), "own"))
  }
  if (is.list(el) && !is.null(el$fn)) {
    return(tfl_item(el$fn, .calls_in(el$args %||% list()), el$from %||% "own"))
  }
  stop("A content item is `- fn: {args}`.", call. = FALSE)
}

# every `{fn:, args:}` map inside a value as a tfl_call (recursively)
.calls_in <- function(x) {
  if (!is.list(x) || inherits(x, c("tfl_call", "tfl_item"))) return(x)
  if (!is.null(names(x)) && "fn" %in% names(x) && all(names(x) %in% c("fn", "args", "package"))) {
    return(tfl_call(x$fn, .calls_in(x$args %||% list()), x$package))
  }
  x[] <- lapply(x, .calls_in)
  x
}

# a report's own keys over a family's base (plain lists, no layering rules
# yet: a key the report has replaces the base's)
.merge_plain <- function(base, own) {
  for (k in names(own)) base[[k]] <- own[[k]]
  base
}

# -- writing ----------------------------------------------------------------

#' @rdname tfl_read_output
#' @export
tfl_write_output <- function(x, path) {
  if (!inherits(x, "tfl_output")) stop("tfl_write_output(): `x` is a tfl_output.", call. = FALSE)
  y <- .output_to_list(x)
  txt <- yaml::as.yaml(y, indent.mapping.sequence = TRUE, handlers = .yaml_out_handlers())
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(txt, path, useBytes = TRUE)
  invisible(path)
}

# written: !r and !unset by their tags; a logical as true / false (not
# yes / no, which the reader keeps as text)
.yaml_out_handlers <- function() {
  list(tfl_r = function(x) structure(as.character(x), tag = "!r"),
       tfl_unset = function(x) structure("", tag = "!unset"),
       logical = .yaml_logical)
}

# the plain list a tfl_output is written as (NA scalars left out; a
# tfl_call as {fn, args}; an item as `- fn: {args}`)
.output_to_list <- function(x) {
  y <- list(id = x$id, kind = x$kind)
  if (!is.na(x$label)) y$label <- x$label
  if (!is.na(x$population)) y$population <- x$population
  y$extends <- as.list(x$extends)
  if (length(x$ard)) y$ard <- .ard_seqs_out(.calls_out(x$ard))
  if (!is.null(x$data)) y$data <- .calls_out(x$data)
  if (length(x$codelists)) y$codelists <- x$codelists
  y$content <- if (inherits(x$content, "tfl_r")) x$content else .items_to_list(x$content)
  if (length(x$page)) y$page <- .calls_out(x$page)
  # a kind's own parts beside the common ones (a figure's template, plot,
  # plots, compose)
  for (k in setdiff(names(x), .output_fields)) if (!is.null(x[[k]])) y[[k]] <- .calls_out(x[[k]])
  # a template SPEC's arguments: a named vector as a map (`labels: {AGE: ...}`)
  if (!is.null(.tpl_spec_of(x))) y$template <- .named_as_map(y$template)
  y
}

# a value's named vectors as maps, through lists
.named_as_map <- function(x) {
  if (inherits(x, c("tfl_r", "tfl_unset"))) return(x)
  if (is.atomic(x) && length(names(x))) return(as.list(x))
  if (is.list(x)) x[] <- lapply(x, .named_as_map)
  x
}

.output_fields <- c("id", "kind", "label", "population", "extends", "ard", "data", "codelists",
                    "content", "page")

.calls_out <- function(x) {
  # an R object (a formula, a call) that found its way in: written as !r
  if (is.language(x) || inherits(x, "formula")) return(tfl_r(paste(deparse(x), collapse = "
")))
  if (inherits(x, "tfl_call")) {
    out <- list(fn = x$fn)
    if (!is.null(x$package)) out$package <- x$package
    out$args <- .calls_out(x$args)
    if (x$fn %in% c("listing_col", "rtfreporter::listing_col")) out$args <- .seqs_out(out$args, .seq_keys$listing_col)
    return(out)
  }
  if (!is.list(x) || inherits(x, c("tfl_r", "tfl_unset"))) return(x)
  x[] <- lapply(x, .calls_out)
  x
}

# The keys whose value is a list of names (what the sheets bridge writes as
# a sequence): written as a sequence even when it holds one name, as the
# reader read it (`by: [TRT01A]` read is "TRT01A", written `- TRT01A`)
.seq_keys <- list(
  analysis_data = c("add", "keep", "distinct"),
  analyses = c("by", "strata", "variables", "statistics", "formats"),
  listing_col = "vars")

.seqs_out <- function(x, keys) {
  if (!is.list(x)) return(x)
  for (k in intersect(keys, names(x))) {
    v <- x[[k]]
    if (is.atomic(v) && is.null(names(v)) && length(v) == 1L && !inherits(v, c("tfl_r", "tfl_unset"))) {
      x[[k]] <- as.list(v)
    }
  }
  x
}

.ard_seqs_out <- function(ard) {
  if (!is.list(ard)) return(ard)
  for (part in c("analysis_data", "analyses")) {
    if (is.list(ard[[part]]) && is.null(names(ard[[part]]))) {
      ard[[part]] <- lapply(ard[[part]], .seqs_out, .seq_keys[[part]])
    }
  }
  ard
}

# content items as they are written: `- fn: {args}`
.items_to_list <- function(items) {
  lapply(items, function(it) {
    if (inherits(it, "tfl_plot")) return(stats::setNames(list(.items_to_list(it$items)), it$fn))
    if (identical(it$fn, ".r")) return(it$args$code)
    a <- .calls_out(it$args)
    # no arguments: an empty map ({}), not an empty sequence
    if (!length(a)) a <- stats::setNames(list(), character())
    stats::setNames(list(a), it$fn)
  })
}

# the data stage's steps as a list: an `!r` step (or a sequence of them,
# which the YAML reader folds into a character vector) as tfl_r, a
# figure's step maps as they are
.data_steps_in <- function(data) {
  if (is.null(data) || is.null(data$steps)) return(data)
  st <- data$steps
  if (inherits(st, "tfl_r")) st <- list(st)
  else if (is.character(st)) st <- lapply(st, tfl_r)
  else if (is.list(st)) st <- lapply(st, function(s) if (is.character(s) && !inherits(s, "tfl_r") && length(s) == 1L) tfl_r(s) else s)
  data$steps <- st
  data
}

# A logical as YAML: true / false, and NA as `.na` -- what the reader reads
# as NA again (`NA` would come back as the text "NA": a table_plan()'s
# `label = NA`, a label built to tell rows apart and dropped, is NA)
.yaml_logical <- function(x) {
  v <- ifelse(is.na(x), ".na", ifelse(x, "true", "false"))
  class(v) <- "verbatim"
  v
}
