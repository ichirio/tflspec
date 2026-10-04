# ============================================================================
#  An ARD function of one's own: does it behave as an analysis's method?
# ----------------------------------------------------------------------------
#  A company or a study may write its own ard_*() (the study key `source`
#  loads the file; an analysis names it as its method).  Before it is
#  used, it is tried: on some data, does it give a cards ARD -- the shape
#  cards' own tools take -- with the statistics it says it gives?
# ============================================================================

#' Try an ARD function of one's own
#'
#' Calls `fun(data, ...)` and checks what it gives: a cards ARD (class
#' `card`, [cards::check_ard_structure()] as notes), the columns every ARD
#' has, and the statistics it says it gives among its rows.  An error or a
#' warning in the call is reported, not raised.
#'
#' A function says which statistics it gives the way cards' own do:
#' `cards::as_cards_fn(fun, stat_names = c("estimate", "p.value"))` (the
#' templates of [tfl_ard_function_template()] are written so).  One that
#' says nothing is tried all the same, with a note.
#'
#' An analysis row calls the function as `fun(<the analysis data>, by = ,
#' variables = , <args>)` (see [tfl_ard_methods()]), so try it the same way:
#' `tfl_check_ard_function(ard_riskdiff_mn, adsl, by = TRT01A, variables =
#' AEFL)`.
#'
#' @param fun The function, or its name.
#' @param data The data to try it on.
#' @param ... Its other arguments, as an analysis row would give them
#'   (unquoted column names are passed as they are).
#' @param stat_names The statistics it should give; `NULL` (default):
#'   the ones the function declares (`attr(fun, "stat_names")`, as
#'   [cards::as_cards_fn()] sets it), if any.
#' @return A data frame of problems as [tfl_check_ard()] (`level`, `check`,
#'   `message`); no rows: it behaves.  The ARD it gave is the attribute
#'   `ard`.
#' @export
tfl_check_ard_function <- function(fun, data, ..., stat_names = NULL) {
  out <- data.frame(level = character(), check = character(),
                    message = character(), stringsAsFactors = FALSE)
  add <- function(level, check, message) {
    out[nrow(out) + 1L, ] <<- list(level, check, message)
  }
  f <- if (is.function(fun)) fun else
    tryCatch(get(fun, mode = "function", envir = parent.frame()),
             error = function(e) NULL)
  if (!is.function(f)) {
    add("error", "function", sprintf("no function %s", deparse(substitute(fun))))
    return(out)
  }
  if (is.null(stat_names)) stat_names <- attr(f, "stat_names")
  if (is.null(stat_names)) {
    add("note", "statistics", paste0(
      "it does not say which statistics it gives: ",
      "cards::as_cards_fn(<the function>, stat_names = c(...)) lets a check see them"))
  }
  call <- as.call(c(list(f, data), as.list(substitute(list(...)))[-1L]))
  warn <- character()
  res <- tryCatch(
    withCallingHandlers(eval(call, parent.frame()), warning = function(w) {
      warn <<- c(warn, conditionMessage(w))
      invokeRestart("muffleWarning")
    }),
    error = function(e) {
      add("error", "call", conditionMessage(e))
      NULL
    })
  for (w in unique(warn)) add("warning", "call", w)
  if (is.null(res)) return(out)
  if (!inherits(res, "card")) {
    add("error", "result", sprintf(
      "it gives %s, not a cards ARD (class card): end it with cards::as_card() or build it with cards' helpers",
      .what(res)))
  }
  if (is.data.frame(res)) {
    p <- tfl_check_ard(res)
    out <- rbind(out, p[p$check != "shape" | p$level != "note", , drop = FALSE])
    if (!is.null(stat_names) && "stat_name" %in% names(res)) {
      miss <- setdiff(stat_names, unique(as.character(unlist(res$stat_name))))
      if (length(miss)) add("error", "statistics", sprintf(
        "it does not give %s", paste(miss, collapse = ", ")))
    }
    if ("error" %in% names(res)) {
      e <- unique(unlist(res$error))
      for (m in e[nzchar(e)]) add("warning", "result", paste("a captured error:", m))
    }
  }
  structure(out, ard = res)
}

#' Start an ARD function of one's own from a template
#'
#' Writes the skeleton of an `ard_*()` function an analysis row can name as
#' its method, ready to edit, in one of three shapes -- the three ways cards
#' and cardx write their own:
#'
#' * `"summary"`: statistics of one's own on numeric variables, by group
#'   (`cards::ard_summary(statistic = )`; a coefficient of variation and a
#'   geometric mean as the example);
#' * `"test"`: a test across groups made an ARD with
#'   [cards::tidy_as_ard()] (a Wilcoxon rank-sum test as the example); its
#'   errors and warnings go into the ARD, as cardx's tests do;
#' * `"free"`: any calculation, group by group, with [cards::ard_strata()]
#'   and [cards::ard_identity()].
#'
#' Each declares the statistics it gives (`cards::as_cards_fn(stat_names =
#' )`), so [tfl_check_ard_function()] checks that it gives them.  With
#' `test = TRUE` a testthat file is written next to it, which runs that
#' check on cards' example data.  The function is called as an analysis row
#' calls it: `fun(data, by = , variables = , <args>)`.
#'
#' @param name The function's name (`ard_riskdiff`).
#' @param type `"summary"`, `"test"` or `"free"`.
#' @param file Where to write it (`programs/ard/functions/ard_riskdiff.R`);
#'   `NULL`: not written, the code is returned.
#' @param test With `file`, also write `test-<name>.R` next to it.
#' @param overwrite Replace a file that is there?
#' @return The function's code, one element per line (invisibly when written).
#' @seealso [tfl_check_ard_function()]
#' @examples
#' cat(tfl_ard_function_template("ard_cv", "summary"), sep = "\n")
#'
#' f <- file.path(tempdir(), "ard_cv.R")
#' tfl_ard_function_template("ard_cv", "summary", file = f, test = TRUE)
#' source(f)
#' if (requireNamespace("cards", quietly = TRUE)) {
#'   tfl_check_ard_function(ard_cv, cards::ADSL, by = ARM, variables = AGE)
#' }
#' @export
tfl_ard_function_template <- function(name, type = c("summary", "test", "free"),
                                      file = NULL, test = FALSE,
                                      overwrite = FALSE) {
  type <- match.arg(type)
  if (!is.character(name) || length(name) != 1L ||
      !grepl("^[A-Za-z.][A-Za-z0-9._]*$", name)) {
    .ard_stop("tfl_ard_function_template(): `name` is the function's name, e.g. ard_riskdiff.")
  }
  fill <- function(tpl, file = "") {
    lines <- readLines(system.file("ard", "templates", tpl, package = "tflspec"),
                       warn = FALSE, encoding = "UTF-8")
    lines <- gsub("{name}", name, lines, fixed = TRUE)
    gsub("{file}", file, lines, fixed = TRUE)
  }
  code <- fill(paste0(type, ".R"))
  if (is.null(file)) return(code)
  put <- function(lines, path) {
    if (file.exists(path) && !isTRUE(overwrite)) {
      .ard_stop(sprintf("%s is there already: overwrite = TRUE to replace it.", path))
    }
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines(enc2utf8(lines), path, useBytes = TRUE)
  }
  put(code, file)
  if (isTRUE(test)) {
    put(fill("testthat.R", basename(file)),
        file.path(dirname(file), paste0("test-", name, ".R")))
  }
  invisible(code)
}
