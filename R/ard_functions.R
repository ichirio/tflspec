# ============================================================================
#  The ARD functions an analysis may call, and their arguments
# ----------------------------------------------------------------------------
#  Two layers, as for the figure calls (inst/fig/calls.csv):
#
#    - generic: any ard_*() of cards / cardx that is installed can be named
#      (`method = cards::ard_summary`), and its arguments are read from its
#      own formals() -- so a function or argument a new version adds is
#      usable before anyone describes it;
#    - catalog: inst/ard/functions.csv says what a person reads to choose
#      one (a category, a heading, a sentence, its shape) and
#      inst/ard/args.csv how each argument is filled in (its kind, a hint,
#      choices).  A company adds rows of its own -- its own functions, its
#      own hints -- with options(tflspec.ard_functions =,
#      tflspec.ard_args =); a row for the same call (and argument) wins.
#
#  The catalog describes; it does not decide what may be called.  The
#  tests compare it with the installed cards / cardx, so a version that
#  adds or renames a function is noticed.
# ============================================================================

.ard_catalog_csv <- function(file) {
  f <- system.file("ard", file, package = "tflspec")
  if (!nzchar(f)) return(NULL)
  d <- utils::read.csv(f, colClasses = "character", na.strings = "",
                       encoding = "UTF-8")
  d[] <- lapply(d, function(v) ifelse(is.na(v), "", v))
  d
}

# the built-in rows, with a company's on top (same key: theirs)
.ard_catalog_rows <- function(file, option, key) {
  d <- .ard_catalog_csv(file)
  own <- getOption(option)
  if (is.null(own)) return(d)
  own <- as.data.frame(own, stringsAsFactors = FALSE)
  for (cn in setdiff(names(d), names(own))) own[[cn]] <- ""
  own[] <- lapply(own, function(v) ifelse(is.na(v), "", as.character(v)))
  k_own <- do.call(paste, c(own[key], sep = "\r"))
  k_d <- do.call(paste, c(d[key], sep = "\r"))
  rbind(d[!k_d %in% k_own, , drop = FALSE], own[names(d)])
}

# `pkg::fun` -> the function, or NULL when it cannot be found
.ard_fun <- function(call) {
  if (!grepl("::", call, fixed = TRUE)) {
    return(tryCatch(get(call, mode = "function"), error = function(e) NULL))
  }
  pkg <- sub("::.*$", "", call)
  fn <- sub("^.*::", "", call)
  if (!requireNamespace(pkg, quietly = TRUE)) return(NULL)
  ns <- asNamespace(pkg)
  if (!fn %in% getNamespaceExports(pkg)) return(NULL)
  get(fn, envir = ns)
}

# The arguments a call takes on a data frame: the data.frame method of a
# generic (ard_summary(data, ...) dispatches), else the function itself.
# `data_first` is whether the first argument is the data: a data.frame
# method's first argument is (ard_survival_survfit(x = <data>)), and so is
# one named data / .data.
.ard_formals <- function(call) {
  f <- .ard_fun(call)
  if (is.null(f)) return(NULL)
  ns <- environment(f)
  fn <- sub("^.*::", "", call)
  m <- tryCatch(utils::getS3method(fn, "data.frame", envir = ns),
                error = function(e) NULL)
  fm <- if (!is.null(m)) formals(m) else formals(f)
  first <- names(fm)[1L]
  structure(fm, data_first = !is.null(m) ||
              isTRUE(first %in% c("data", ".data")))
}

# cardx re-exports cards' summaries (for its survey.design methods): one
# name, the cards one
.ard_installed <- function() {
  out <- character()
  for (pkg in c("cards", "cardx")) {
    if (!requireNamespace(pkg, quietly = TRUE)) next
    e <- getNamespaceExports(pkg)
    e <- sort(e[startsWith(e, "ard_")])
    if (pkg == "cardx" && "cards" %in% sub("::.*$", "", out)) {
      e <- setdiff(e, sub("^cards::", "", out))
    }
    out <- c(out, paste0(pkg, "::", e))
  }
  out
}

#' The ARD functions an analysis may call
#'
#' Every `ard_*()` of cards and cardx that is installed, with what a person
#' reads to choose one: its `category` (the group it is listed under), a
#' `label` (the heading) and a one-line `description`, and its `shape` --
#' how it takes its input:
#'
#' * `variables`: columns (`by`, `variables`, `strata`) of the data;
#' * `formula`: a model formula (`AVAL ~ TRT01P`) and the data;
#' * `model`: a fitted model, written in `args` (`x = lm(..., data = data)`);
#' * `columns`: named columns of its own (`time`, `count`; `postbaseline`);
#' * `wrapper`: runs other analyses (`ard_stack()`, `ard_strata()`,
#'   `ard_pairwise()`);
#' * `data`: the data alone.
#'
#' An analysis row names one of them as its `method` (`cards::ard_summary`),
#' as it names any function; this list is the catalog that describes them.
#' A function the catalog does not describe (one a newer cards adds) is
#' listed too, with `in_catalog = FALSE` and its help page's title as its
#' label.  `replaced_by` names the new name of an old one.
#'
#' A company adds its own rows -- its own ARD functions, other headings --
#' with `options(tflspec.ard_functions = <data frame>)` (the same columns;
#' a row with the same `call` replaces the built-in one).
#'
#' @param installed `TRUE` (default) only the functions that can be called
#'   here (their package installed); `FALSE` every row of the catalog.
#' @return A data frame: `call`, `category`, `label`, `description`,
#'   `shape`, `replaced_by`, `installed`, `in_catalog`.
#' @seealso [tfl_ard_args()] for one function's arguments;
#'   [tfl_ard_methods()] for the keywords an analysis may name.
#' @examples
#' f <- tfl_ard_functions()
#' f[f$category == "Summaries", c("call", "label")]
#' @export
tfl_ard_functions <- function(installed = TRUE) {
  d <- .ard_catalog_rows("functions.csv", "tflspec.ard_functions", "call")
  d$in_catalog <- TRUE
  extra <- setdiff(.ard_installed(), d$call)
  if (length(extra)) {
    d <- rbind(d, data.frame(
      call = extra, category = "Other", label = vapply(extra, .ard_help_title, ""),
      description = "", shape = "", replaced_by = "", in_catalog = FALSE,
      stringsAsFactors = FALSE))
  }
  d$installed <- vapply(d$call, function(cl) !is.null(.ard_fun(cl)), NA)
  if (isTRUE(installed)) d <- d[d$installed, , drop = FALSE]
  rownames(d) <- NULL
  d[c("call", "category", "label", "description", "shape", "replaced_by",
      "installed", "in_catalog")]
}

# the title of a function's help page (a function the catalog does not
# describe), or its name
.ard_help_title <- function(call) {
  pkg <- sub("::.*$", "", call)
  fn <- sub("^.*::", "", call)
  t <- tryCatch({
    db <- tools::Rd_db(pkg)
    hit <- Filter(function(rd) {
      tags <- vapply(rd, function(x) attr(x, "Rd_tag"), "")
      fn %in% unlist(lapply(rd[tags == "\\alias"], as.character))
    }, db)
    if (!length(hit)) NA_character_ else {
      rd <- hit[[1L]]
      tags <- vapply(rd, function(x) attr(x, "Rd_tag"), "")
      trimws(paste(unlist(rd[tags == "\\title"]), collapse = " "))
    }
  }, error = function(e) NA_character_)
  if (is.na(t) || !nzchar(t)) fn else t
}

#' The arguments of one ARD function, and how each is filled in
#'
#' Read from the function's own `formals()` (its data frame method), so it
#' is always the installed version's: every argument, its default (as R
#' text; `NA` when it has none and must be given) and, from the catalog
#' (`inst/ard/args.csv`, a company's rows on top), its `kind` and a `hint`.
#'
#' `kind` says what fills it: `data` (the analysis data), `columns` /
#' `column` (columns of the data), `levels` (levels of a column),
#' `denominator`, `statistics`, `number`, `logical`, `choice`, `text`,
#' `formula`, or `code` (R written as is).  An argument the catalog does not
#' describe gets one from its default: `TRUE` / `FALSE` -> `logical`, a
#' number -> `number`, a character vector -> `choice`, else `code`.
#' `choices` are the catalog's, or the values of a character vector default
#' (`method = c("waldcc", "wald", ...)`).
#'
#' `column` says where an analysis row writes it: the `by`, `variables`,
#' `strata`, `denominator` or `statistics` column, or `args`.  The data is
#' the row's own (`data`).
#'
#' A company adds its own rows with `options(tflspec.ard_args = <data
#' frame>)`: `call` (or `*` for every function), `arg`, `kind`, `hint`,
#' `choices`; a row for the same call and argument wins, and a row for the
#' function wins over a `*` row.
#'
#' @param call The function, as an analysis names it: `"cards::ard_summary"`.
#' @return A data frame, one row per argument (`...` left out): `arg`,
#'   `default`, `required`, `kind`, `hint`, `choices`, `column`.
#' @examples
#' tfl_ard_args("cardx::ard_categorical_ci")
#' @export
tfl_ard_args <- function(call) {
  if (!is.character(call) || length(call) != 1L || is.na(call)) {
    .ard_stop("tfl_ard_args(): `call` is one function, e.g. \"cards::ard_summary\".")
  }
  fm <- .ard_formals(call)
  if (is.null(fm)) {
    .ard_stop(sprintf(paste0(
      "tfl_ard_args(): %s cannot be found -- is its package installed?\n",
      "  The ARD functions here: tfl_ard_functions()."), call))
  }
  data_first <- isTRUE(attr(fm, "data_first"))
  first <- names(fm)[1L]
  fm <- fm[setdiff(names(fm), c("...", "fmt_fn"))]
  cat_rows <- .ard_catalog_rows("args.csv", "tflspec.ard_args", c("call", "arg"))
  pick <- function(a, field) {
    own <- cat_rows[cat_rows$call == call & cat_rows$arg == a, field]
    any <- cat_rows[cat_rows$call == "*" & cat_rows$arg == a, field]
    v <- c(own, any)
    v <- v[nzchar(v)]
    if (length(v)) v[1L] else ""
  }
  # an argument with no default is the empty symbol, which may not be
  # touched as a value
  no_default <- vapply(fm, is.name, NA) & as.character(fm) == ""
  rows <- lapply(names(fm), function(a) {
    required <- no_default[[a]]
    d <- if (required) NULL else fm[[a]]
    dtext <- if (required) NA_character_ else
      paste(deparse(d, width.cutoff = 500L), collapse = " ")
    kind <- if (data_first && identical(a, first)) "data" else pick(a, "kind")
    if (!nzchar(kind)) kind <- .ard_arg_kind(d, required)
    ch <- pick(a, "choices")
    if (!nzchar(ch) && is.call(d) && identical(d[[1L]], as.name("c")) &&
        all(vapply(as.list(d)[-1L], is.character, NA))) {
      ch <- paste(unlist(as.list(d)[-1L]), collapse = " | ")
    }
    data.frame(arg = a, default = dtext, required = required, kind = kind,
               hint = pick(a, "hint"), choices = ch,
               column = .ard_arg_column(a, kind), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# what fills an argument the catalog does not describe, from its default
.ard_arg_kind <- function(d, required) {
  if (required) return("code")
  if (is.logical(d) && length(d) == 1L && !is.na(d)) return("logical")
  if (is.numeric(d)) return("number")
  if (is.character(d) && length(d) == 1L) return("text")
  if (is.call(d) && identical(d[[1L]], as.name("c")) &&
      all(vapply(as.list(d)[-1L], is.character, NA))) return("choice")
  "code"
}

# the analysis row's column an argument is written in
.ard_arg_column <- function(arg, kind) {
  m <- c(by = "by", variables = "variables", strata = "strata",
         denominator = "denominator", statistic = "statistics")
  if (arg %in% names(m)) return(m[[arg]])
  if (identical(kind, "data")) "data" else "args"
}
