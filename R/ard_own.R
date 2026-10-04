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
#' has, and -- with `stat_names` -- those statistics among its rows.  An
#' error or a warning in the call is reported, not raised.
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
#' @param stat_names The statistics it should give (`NULL`: not checked).
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
