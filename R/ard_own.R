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

#' What the ARD functions of one's own in some R files are
#'
#' Reads the files -- it does not run them -- and returns one row per
#' function an analysis row could name as its method: a top-level
#' `name <- function(...)`, or `name <- cards::as_cards_fn(function(...),
#' stat_names = c(...))` as the templates of [tfl_ard_function_template()]
#' write it.  What a person reads to choose one comes from the roxygen
#' block above it: the title (its first line), the description (the
#' paragraph after), and each argument's `@param`.  The statistics are the
#' ones `as_cards_fn()` declares.  Test files (`test-*.R`) are skipped.
#'
#' @param files R files: a study's own functions (its key `source`), a
#'   company's (a standards folder), or both.
#' @return A data frame: `name`, `file`, `title`, `description`,
#'   `stat_names` (` | ` between them; empty when the function declares
#'   none), and `args`, a list column of data frames (`arg`, `default`,
#'   `hint`).  A file that does not parse is a row with `name` `NA` and the
#'   parse error as its `description`.
#' @seealso [tfl_ard_function_template()], [tfl_check_ard_function()]
#' @examples
#' f <- file.path(tempdir(), "ard_mine.R")
#' tfl_ard_function_template("ard_mine", "test", file = f, overwrite = TRUE)
#' info <- tfl_ard_function_info(f)
#' info[, c("name", "title", "stat_names")]
#' info$args[[1]]
#' @export
tfl_ard_function_info <- function(files) {
  files <- files[!grepl("^test-", basename(files))]
  rows <- lapply(files, .ard_fn_info_file)
  empty <- data.frame(name = character(), file = character(),
                      title = character(), description = character(),
                      stat_names = character(), stringsAsFactors = FALSE)
  empty$args <- list()
  out <- do.call(rbind, c(list(empty), rows))
  rownames(out) <- NULL
  out
}

.ard_fn_info_file <- function(file) {
  row <- function(name, title, description, stat_names, args) {
    d <- data.frame(name = name, file = file, title = title,
                    description = description, stat_names = stat_names,
                    stringsAsFactors = FALSE)
    d$args <- list(args)
    d
  }
  if (!file.exists(file)) return(NULL)
  lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
  ex <- tryCatch(parse(text = lines, keep.source = TRUE),
                 error = function(e) e)
  if (inherits(ex, "error")) {
    return(row(NA_character_, NA_character_, conditionMessage(ex), "",
               .ard_fn_args(NULL, list())))
  }
  refs <- attr(ex, "srcref")
  out <- list()
  for (i in seq_along(ex)) {
    e <- ex[[i]]
    if (!is.call(e) || !as.character(e[[1L]]) %in% c("<-", "=") ||
        !is.name(e[[2L]])) next
    rhs <- e[[3L]]
    fn <- if (is.call(rhs)) paste(deparse(rhs[[1L]]), collapse = "") else ""
    fun <- if (identical(fn, "function")) rhs else
      if (fn %in% c("cards::as_cards_fn", "as_cards_fn") &&
          is.call(rhs[[2L]]) && identical(rhs[[2L]][[1L]], as.name("function")))
        rhs[[2L]] else NULL
    if (is.null(fun)) next
    sn <- if (!identical(fn, "function") && !is.null(rhs$stat_names)) {
      v <- tryCatch(eval(rhs$stat_names, baseenv()), error = function(e) NULL)
      if (is.character(v)) v else character()
    } else character()
    rox <- .ard_fn_roxygen(lines, refs[[i]][1L])
    out[[length(out) + 1L]] <- row(
      as.character(e[[2L]]), rox$title, rox$description,
      paste(sn, collapse = " | "), .ard_fn_args(fun, rox$params))
  }
  do.call(rbind, out)
}

# the roxygen block right above line `at`: its title, description, @params
.ard_fn_roxygen <- function(lines, at) {
  # blank lines between the block and the function are allowed, as roxygen
  # allows them
  while (at > 1L && !nzchar(trimws(lines[at - 1L]))) at <- at - 1L
  i <- at - 1L
  while (i >= 1L && grepl("^\\s*#'", lines[i])) i <- i - 1L
  block <- if (i + 1L <= at - 1L) lines[(i + 1L):(at - 1L)] else character()
  block <- sub("^\\s*#' ?", "", block)
  tag <- grepl("^@", block)
  first_tag <- if (any(tag)) which(tag)[1L] else length(block) + 1L
  text <- block[seq_len(first_tag - 1L)]
  paras <- split(text, cumsum(!nzchar(trimws(text))))
  paras <- vapply(paras, function(p) trimws(paste(trimws(p[nzchar(trimws(p))]),
                                                  collapse = " ")), "")
  paras <- paras[nzchar(paras)]
  params <- list()
  cur <- NULL
  for (ln in block[seq_along(block) >= first_tag]) {
    m <- regmatches(ln, regexec("^@param\\s+([^[:space:]]+)\\s*(.*)$", ln))[[1L]]
    if (length(m)) {
      for (a in strsplit(m[2L], ",", fixed = TRUE)[[1L]]) params[[trimws(a)]] <- m[3L]
      cur <- strsplit(m[2L], ",", fixed = TRUE)[[1L]]
    } else if (grepl("^@", ln)) {
      cur <- NULL
    } else if (!is.null(cur) && nzchar(trimws(ln))) {
      for (a in cur) params[[trimws(a)]] <- paste(params[[trimws(a)]], trimws(ln))
    }
  }
  list(title = if (length(paras)) paras[[1L]] else NA_character_,
       description = if (length(paras) > 1L) paste(paras[-1L], collapse = "\n\n")
                     else NA_character_,
       params = params)
}

# a function's arguments, from its definition (not run), with their hints
.ard_fn_args <- function(fun, params) {
  fm <- if (is.null(fun)) NULL else fun[[2L]]
  if (is.null(fm)) {
    return(data.frame(arg = character(), default = character(),
                      hint = character(), stringsAsFactors = FALSE))
  }
  dflt <- vapply(as.list(fm), function(d) {
    if (is.name(d) && identical(as.character(d), "")) NA_character_
    else paste(deparse(d, width.cutoff = 500L), collapse = " ")
  }, "")
  data.frame(arg = names(fm), default = unname(dflt),
             hint = vapply(names(fm), function(a) params[[a]] %||% NA_character_, "",
                           USE.NAMES = FALSE),
             stringsAsFactors = FALSE)
}
