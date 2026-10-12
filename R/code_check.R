#' Check a report's own code against the rules
#'
#' A report's data step or content step may be the user's own R code (the
#' report sheet's `data_code` / `content_code`: a step whose code is not
#' blank is its code).  The code follows rules the program around it
#' relies on: the datasets it reads are the report's (registered in its
#' data step: the program reads them first, by their names in lower case),
#' this report's ARD rows are `ard` when the report reads its ARD, and the
#' step leaves the object the next one reads -- the data step `data` (a
#' table or a listing) or `df` (a figure), the content step `content`.
#' The check reads the code; it does not run it.
#'
#' The rules ([tfl_review_rules()], area `program`):
#'
#' * U01 (error): the code does not read as R.
#' * U02 (error): it names a dataset of the catalog the report does not
#'   register.
#' * U03 (error): it leaves nothing for the next step (no assignment to
#'   `data` / `df` / `content`).
#' * U04 (check): it reads files itself, attaches packages or changes the
#'   session (`readRDS()`, `haven::read_xpt()`, `library()`, `setwd()`,
#'   `options()` ...).
#' * U05 (error): it uses `ard`, but the report does not read its ARD.
#'
#' @param code The code: a string (lines joined by newlines) or a
#'   character vector of lines.
#' @param step `"data"` or `"content"`.
#' @param type The report's type: `"table"`, `"listing"` or `"figure"`.
#' @param datasets The datasets the report registers (any case).
#' @param catalog The datasets of the study's catalog (any case): a name
#'   of these the code uses and does not make itself must be registered.
#' @param ard Whether the report reads its ARD (`ard` is in reach).
#' @param output_id The report, for the messages.
#' @return A `tfl_review` ([tfl_review_spec()]'s rows: sheet `report`,
#'   field `data_code` / `content_code`), with the attribute `datasets`:
#'   the catalog's datasets the code uses (upper case), registered or not.
#' @examples
#' code <- "df <- advs |> dplyr::filter(PARAMCD == 'SYSBP')"
#' r <- tfl_check_code(code, "data", "figure", datasets = "ADSL",
#'                     catalog = c("ADSL", "ADVS"), output_id = "F-1")
#' r[, c("rule", "level", "message")]
#' attr(r, "datasets")
#' @export
tfl_check_code <- function(code, step = c("data", "content"),
                           type = c("table", "listing", "figure"),
                           datasets = character(), catalog = character(),
                           ard = FALSE, output_id = NA_character_) {
  step <- match.arg(step)
  type <- match.arg(type)
  field <- paste0(step, "_code")
  code <- paste(as.character(code %||% ""), collapse = "\n")
  rows <- list()
  add <- function(rule, args) {
    rows[[length(rows) + 1L]] <<- .rv(rule, output_id, "report", output_id %|NA|% "",
                                       field, args = c(step, output_id %|NA|% "", args))
  }
  done <- function(found = character()) {
    out <- .review_finish(do.call(rbind, c(list(.rv("U01")[0L, ]), rows)))
    attr(out, "datasets") <- found
    out
  }
  if (!nzchar(trimws(code))) return(done())
  ex <- tryCatch(parse(text = code, keep.source = TRUE), error = function(e) e)
  if (inherits(ex, "error")) {
    msg <- conditionMessage(ex)
    line <- suppressWarnings(as.integer(sub("^<text>:([0-9]+):.*$", "\\1",
                                            strsplit(msg, "\n")[[1L]][1L])))
    add("U01", c(if (is.na(line)) "?" else line,
                 trimws(sub("^<text>:[0-9]+:[0-9]+: ", "", strsplit(msg, "\n")[[1L]][1L]))))
    return(done())
  }
  made <- .code_assigned(ex)
  pd <- utils::getParseData(ex)
  pd <- pd[pd$terminal, , drop = FALSE]
  pd <- pd[order(pd$line1, pd$col1), , drop = FALSE]
  prev <- c("", pd$token[-nrow(pd)])
  # a name used as a value (not a function called, not after $ @ ::, not
  # an argument's name)
  used <- unique(pd$text[pd$token == "SYMBOL" &
                           !prev %in% c("'$'", "'@'", "NS_GET", "NS_GET_INT")])
  # U02: a dataset of the catalog the report does not register
  cat_lc <- unique(tolower(catalog))
  found <- toupper(intersect(intersect(used, cat_lc), setdiff(used, made)))
  for (d in setdiff(found, toupper(datasets))) add("U02", tolower(d))
  # U03: nothing left for the next step
  leaves <- if (step == "content") "content" else if (type == "figure") "df" else "data"
  if (!leaves %in% made) add("U03", leaves)
  # U04: files read, packages attached, the session changed
  calls <- unique(pd$text[pd$token == "SYMBOL_FUNCTION_CALL"])
  bad <- intersect(calls, .code_banned)
  if (length(bad)) add("U04", paste0(bad, "()", collapse = ", "))
  # U05: `ard` used where the report does not read its ARD
  if (!isTRUE(ard) && "ard" %in% used && !"ard" %in% made) add("U05", character())
  done(found)
}

# what a report's code may not call: the program reads the data and the
# ARD, and owns the session
.code_banned <- c("readRDS", "read_xpt", "read_sas", "read_sav", "read_dta",
                  "read.csv", "read_csv", "read_excel", "read.xlsx", "load",
                  "library", "require", "setwd", "options", "source",
                  "sys.source", "attach")

# The names a code assigns (`x <- `, `x = `, `x <<- `, `-> x`, `assign("x")`,
# `x$a <- `: x), anywhere in it
.code_assigned <- function(ex) {
  out <- character()
  root <- function(e) {
    while (is.call(e) && length(e) > 1L) e <- e[[2L]]
    if (is.symbol(e)) as.character(e) else if (is.character(e)) e else NULL
  }
  walk <- function(e) {
    if (!is.call(e)) return()
    f <- e[[1L]]
    if (is.symbol(f) && as.character(f) %in% c("<-", "=", "<<-") && length(e) >= 3L) {
      out <<- c(out, root(e[[2L]]))
    }
    if (is.symbol(f) && identical(as.character(f), "assign") && length(e) >= 2L &&
        is.character(e[[2L]])) {
      out <<- c(out, e[[2L]])
    }
    # (an empty argument, `x[, 1]`, is skipped: it cannot be held)
    args <- as.list(e)[-1L]
    for (i in seq_along(args)) {
      if (!identical(args[[i]], quote(expr = ))) walk(args[[i]])
    }
  }
  for (e in as.list(ex)) walk(e)
  unique(out)
}
