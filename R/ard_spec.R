# ============================================================================
#  The study's ARD from its definition (ARD spec)
# ----------------------------------------------------------------------------
#  Moved from tflplanner (R/ard_spec.R) so the definition -> code -> ARD
#  engine works without the GUI.  tflplanner keeps what belongs to a study
#  folder: editing the rows, saving the programs, running them, the status.
#
# A study's analyses are rows: which data, which population, which subset,
# grouped by what, which variables, which method and statistics.  Each row
# becomes one cards / cardx call; every result is tagged with the ids that
# trace it -- output_id, analysis_id, population_id -- and all of them are
# bound into ONE ARD for the study (output/ard/ard.rds).
#
# The workbook is turned into R code (tfl_ard_code()) and the ARD is made by
# running that code (tfl_build_ard()), so the code a programmer can read, keep
# and rerun is exactly what made the ARD.
#
#   study        key / value: id (the subject key, USUBJID), output (where
#                the ARD goes)
#   datasets     dataset, path, derive           the analysis data
#   populations  population_id, dataset, where,  analysis sets (and the
#                derive                          denominators)
#   analyses     output_id, analysis_id, parent, method, dataset,
#                population_id, where, by, variables, statistics, formats,
#                args, code
#
# The concepts are those of CDISC ARS (analysis set, data subset, grouping,
# method), so an tfl_ard_spec can later be written as ARS metadata.

.ard_spec_sheets <- list(
  study = c("key", "value"),
  datasets = c("dataset", "level", "path", "derive"),
  populations = c("population_id", "dataset", "where", "derive"),
  analysis_data = c("output_id", "data_id", "label", "from", "population_id", "subjects",
                    "where", "add", "derive", "keep", "distinct", "code"),
  analyses = c("output_id", "analysis_id", "parent", "label", "method",
               "data", "dataset",
               "population_id", "where", "by", "strata", "variables",
               "statistics", "denominator", "formats", "args", "post",
               "code", "purpose", "reason"))

.split_bar <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x)) return(character())
  # the same cells come back many times in a review of many reports
  hit <- .split_bar_memo[[x]]
  if (!is.null(hit)) return(hit)
  out <- if (!nzchar(trimws(x))) character() else
    trimws(strsplit(x, "|", fixed = TRUE)[[1L]])
  if (length(.split_bar_memo) > 20000L) rm(list = ls(.split_bar_memo), envir = .split_bar_memo)
  .split_bar_memo[[x]] <- out
  out
}
.split_bar_memo <- new.env(hash = TRUE, parent = emptyenv())

# The names of the arguments an analysis's `args` gives: `args` read as the
# arguments of a call, so neither spacing nor an argument of a nested call
# (`.f = function(df) ard(df, by = X)`) misleads it.
.args_given <- function(args) {
  nm <- names(.args_list(args))
  if (is.null(nm)) character() else nm[nzchar(nm)]
}

# `args` as a list of its arguments (unevaluated), named as given
.args_list <- function(args) {
  if (is.null(args) || is.na(args) || !nzchar(trimws(args))) return(list())
  as.list(str2lang(paste0("f(", args, "\n)")))[-1L]
}

# Why `args` does not read as the arguments of a call, or NULL
.args_problem <- function(args) {
  if (is.na(args) || !nzchar(trimws(args))) return(NULL)
  tryCatch({
    str2lang(paste0("f(", args, "\n)"))
    NULL
  }, error = function(e) conditionMessage(e))
}

# The catalog row of a method: its keyword (`categorical`), or the
# function a keyword calls (`cards::ard_tabulate`) -- the same analysis
# either way, with the keyword's statistics, defaults and formats.  NA for a
# function the catalog does not know (a study's own, another cards one).
.method_key <- function(m, keys) {
  k <- match(m, keys$method)
  if (is.na(k)) k <- match(m, keys$call)
  k
}

# How the analysis data goes into a method's call: `data` first, as cards
# and cardx functions take it; nothing when the function's first argument
# is given in `args` and it has no `data` argument (a fitted model:
# cardx::ard_car_anova(x = lm(..., data = data))).  A function that cannot
# be looked up (its package not installed) takes `data` first.
.data_arg <- function(fn, has) {
  f <- tryCatch(eval(str2lang(fn)), error = function(e) NULL)
  if (!is.function(f)) return("data")
  fm <- names(formals(f))
  if ("data" %in% fm || !length(fm)) return("data")
  if (fm[1L] != "..." && has(fm[1L])) return(NULL)
  "data"
}

.normalize_ard_sheet <- function(d, sheet) {
  cols <- .ard_spec_sheets[[sheet]]
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  out <- lapply(cols, function(c) {
    v <- if (c %in% names(d)) as.character(d[[c]]) else
      rep(NA_character_, nrow(d))
    v <- trimws(v)
    v[!is.na(v) & !nzchar(v)] <- NA
    v
  })
  out <- as.data.frame(stats::setNames(out, cols), stringsAsFactors = FALSE)
  out <- out[rowSums(!is.na(out)) > 0, , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Write an ARD definition to a workbook
#'
#' Writes the five sheets (`study`, `datasets`, `populations`,
#' `analysis_data`, `analyses`)
#' and no more; what each column means is a comment on its header cell
#' ([tfl_spec_columns()]).  [tfl_read_ard_spec()] takes one back.  With
#' other specs in one workbook: [tfl_write_specs()].
#'
#' @param spec An ARD definition: an [tfl_ard_spec()], or a list of the sheets.
#' @param path The workbook to write.
#' @param statistics,methods The catalogs ([tfl_ard_statistics()],
#'   [tfl_ard_methods()]) to use instead of the current ones.
#' @param catalogs `TRUE` also writes the catalogs the spec is checked
#'   against, as the sheets `_methods` and `_statistics` (for reference: the
#'   reader passes over sheets whose name starts with `_`).
#' @return `path`, invisibly.
#' @export
tfl_write_ard_spec <- function(spec, path, statistics = NULL, methods = NULL,
                               catalogs = FALSE) {
  old <- .set_catalogs(statistics, methods)
  on.exit(options(old), add = TRUE)
  a <- .ard_spec_normalized(spec)
  .write_spec_book(c(a, if (isTRUE(catalogs))
    list(`_methods` = tfl_ard_methods(),
         `_statistics` = tfl_ard_statistics())), path)
}

# the sheets of an ARD definition, in shape
.ard_spec_normalized <- function(spec) {
  stats::setNames(lapply(names(.ard_spec_sheets), function(s)
    .normalize_ard_sheet(spec[[s]], s)), names(.ard_spec_sheets))
}

#' Read and check an ARD definition workbook
#'
#' @param path An `ard_spec.xlsx`.
#' @param check `FALSE` reads a definition still being written without
#'   refusing it.
#' @inheritParams tfl_write_ard_spec
#' @return An `tfl_ard_spec`: a list of the five sheets (a workbook
#'   without `analysis_data`, written before it was added, reads it empty).
#' @export
tfl_read_ard_spec <- function(path, check = TRUE, statistics = NULL,
                          methods = NULL) {
  if (!is.character(path) || length(path) != 1L || is.na(path)) {
    .ard_stop(sprintf("tfl_read_ard_spec(): `path` must be the path of an ARD spec workbook; got %s.",
                      .what(path)))
  }
  if (!file.exists(path)) .stop_no_file(path, "tfl_read_ard_spec")
  sheets <- readxl::excel_sheets(path)
  out <- lapply(names(.ard_spec_sheets), function(s) {
    cols <- .ard_spec_sheets[[s]]
    d <- if (s %in% sheets) .xlsx_text(path, s) else
      data.frame(matrix(character(), 0, length(cols),
                        dimnames = list(NULL, cols)))
    bad <- setdiff(names(d), c(cols, "note"))
    if (length(bad)) {
      stop("Sheet `", s, "` has columns it does not read: ",
           paste(bad, collapse = ", "), call. = FALSE)
    }
    for (c in cols) if (!c %in% names(d)) d[[c]] <- NA_character_
    d <- d[cols]
    d[] <- lapply(d, function(v) {
      v <- trimws(as.character(v))
      v[!is.na(v) & !nzchar(v)] <- NA
      v
    })
    d[rowSums(!is.na(d)) > 0, , drop = FALSE]
  })
  names(out) <- names(.ard_spec_sheets)
  if (check) tfl_ard_spec(out, statistics = statistics, methods = methods) else
    structure(out, class = "tfl_ard_spec")
}

# a sheet as text, as it reads in Excel
.xlsx_text <- function(path, sheet) {
  d <- readxl::read_excel(path, sheet, col_types = "text",
                          .name_repair = "minimal")
  .xlsx_lf(as.data.frame(d, stringsAsFactors = FALSE, check.names = FALSE))
}

# A line break in a cell as "\n".  openxlsx on Windows writes "\n" as
# "\r\n" (its XML goes out in text mode), so each write would add a "\r".
.xlsx_lf <- function(d) {
  for (j in seq_along(d)) {
    if (is.character(d[[j]])) d[[j]] <- gsub("\r\n", "\n", d[[j]],
                                             fixed = TRUE)
  }
  d
}

#' @rdname tfl_read_ard_spec
#' @param x A list of the sheets (data frames).
#' @section Problems: `tfl_ard_spec()` stops with every problem it finds,
#'   one line each; the condition (class `tflspec_spec_error`) carries them
#'   as rows too, `cnd$problems`: `output_id`, `sheet`, `row` (the row's key:
#'   the `analysis_id`, the `data_id`, the `population_id`), `field` and
#'   `message`.  [tfl_review_spec()] lists them without stopping.
#' @export
tfl_ard_spec <- function(x, statistics = NULL, methods = NULL) {
  old <- .set_catalogs(statistics, methods)
  on.exit(options(old), add = TRUE)
  x <- .ard_spec_shaped(x)
  m <- stats::na.omit(x$analyses$method)
  own <- !m %in% tfl_ard_methods()$method &
    !grepl("^[A-Za-z.][A-Za-z0-9.]*::[A-Za-z._][A-Za-z0-9._]*$", m) &
    grepl("^[A-Za-z.][A-Za-z0-9._]*$", m)
  if (any(own) && !length(.split_bar(.study_value(x, "source", NA)))) {
    warning(sprintf(paste(
      "method(s) %s: not a keyword of tfl_ard_methods(), so read as your",
      "own function(s) -- which the study key `source` should load"),
      paste(unique(m[own]), collapse = ", ")), call. = FALSE)
  }
  p <- .ard_spec_problems(x)
  if (nrow(p)) {
    .spec_stop(paste(c("The ARD definition is not valid:", p$message),
                     collapse = "\n  "), p, "tflspec_ard_spec_error")
  }
  x
}

# The sheets as tfl_ard_spec() reads them: the analysis data in shape (none,
# written before it was added: empty), a column a definition does not have
# yet (written before it was added) blank
.ard_spec_shaped <- function(x) {
  x$analysis_data <- .adata_sheet(x)
  for (s in intersect(names(.ard_spec_sheets), names(x))) {
    for (c in setdiff(.ard_spec_sheets[[s]], names(x[[s]]))) {
      x[[s]][[c]] <- rep(NA_character_, nrow(x[[s]]))
    }
  }
  structure(x, class = "tfl_ard_spec")
}

# One problem of an analysis (row `i` of `a`), keyed by its analysis_id
.ard_problem <- function(a, i, field, message) {
  if (!length(message)) return(.spec_problems_empty())
  .spec_problem(message, "analyses", a$output_id[i],
                if (is.na(a$analysis_id[i])) "" else a$analysis_id[i], field)
}

# Every problem of an ARD definition (.spec_problems_keyed()'s rows), in
# the order of the lines tfl_ard_spec() stops with
.ard_spec_problems <- function(x) {
  a <- x$analyses
  pr <- list(.spec_problems_empty())
  add <- function(i, field, message) {
    pr[[length(pr) + 1L]] <<- .ard_problem(a, i, field, message)
  }
  for (k in c("output_id", "analysis_id", "method")) {
    i <- which(is.na(a[[k]]))
    if (length(i)) add(i[1L], k, sprintf("`analyses$%s` is blank in row(s) %s",
                                         k, paste(i, collapse = ", ")))
  }
  dup <- duplicated(paste(a$output_id, a$analysis_id))
  if (any(dup)) add(which(dup)[1L], "analysis_id", sprintf(
    "output_id / analysis_id repeated: %s",
    paste(unique(paste(a$output_id, a$analysis_id)[dup]), collapse = ", ")))
  pr <- c(pr, list(.ard_parent_problems(a), .adata_problems(x, a)))
  for (i in which(!is.na(a$post %||% rep(NA, nrow(a))))) {
    tag <- paste(a$output_id[i], a$analysis_id[i], sep = " / ")
    if (!is.na(a$parent[i] %||% NA)) {
      add(i, "post", sprintf(paste(
        "%s: `post` works on an analysis's ARD; inside %s it goes on the",
        "parent's row"), tag, a$parent[i]))
    }
    for (st in .split_post(a$post[i])) {
      p <- .post_problem(st)
      if (!is.null(p)) add(i, "post", sprintf("%s: `post` %s", tag, p))
    }
  }
  m <- a$method
  known <- m %in% tfl_ard_methods()$method
  pkgfun <- grepl("^[A-Za-z.][A-Za-z0-9.]*::[A-Za-z._][A-Za-z0-9._]*$", m)
  own <- !known & !pkgfun & grepl("^[A-Za-z.][A-Za-z0-9._]*$", m)
  bad <- which(!is.na(m) & !known & !pkgfun & !own)
  if (length(bad)) add(bad[1L], "method", sprintf(
    "unknown method(s): %s (a keyword of tfl_ard_methods(), pkg::function, or a function the study key `source` loads)",
    paste(unique(m[bad]), collapse = ", ")))
  for (i in which(!is.na(a$args))) {
    p <- .args_problem(a$args[i])
    if (!is.null(p)) add(i, "args", sprintf(
      "%s / %s: `args` does not read as R arguments (%s)", a$output_id[i],
      a$analysis_id[i], gsub("\\s+", " ", p)))
  }
  # one argument, one place: a column that gives an argument of the call
  # and `args` may not both give it (else one of them is dropped unseen)
  km <- tfl_ard_methods()
  for (i in which(!is.na(a$args))) {
    if (!is.null(.args_problem(a$args[i]))) next
    k <- .method_key(a$method[i], km)
    passes_stats <- !is.na(k) && (km$kind[k] %in%
      c("continuous", "categorical", "missing") ||
        identical(km$call[k], "(subjects)"))
    col_arg <- c(by = "by", variables = "variables", strata = "strata",
                 denominator = "denominator",
                 statistics = if (passes_stats) "statistic")
    twice <- names(col_arg)[!is.na(unlist(a[i, names(col_arg)])) &
                              col_arg %in% .args_given(a$args[i])]
    for (cn in twice) add(i, cn, sprintf(
      "%s / %s: `%s` is given twice, by the `%s` column and in `args`; write it in one place",
      a$output_id[i], a$analysis_id[i], col_arg[[cn]], cn))
  }
  for (col in c("strata", "denominator")) {
    for (i in which(!is.na(a[[col]] %||% rep(NA, nrow(a))))) {
      tag <- paste(a$output_id[i], a$analysis_id[i], sep = " / ")
      if (identical(a$method[i], "custom")) {
        add(i, col, sprintf("%s: a `custom` analysis takes no `%s` (its code says it)",
                            tag, col))
        next
      }
      f <- .method_fun(a$method[i])
      if (!is.null(f) && !any(c(col, "...") %in% names(formals(f)))) {
        add(i, col, sprintf("%s: %s takes no `%s`", tag, a$method[i], col))
      }
    }
  }
  den <- a$denominator %||% rep(NA, nrow(a))
  bad <- which(!is.na(den) & !den %in% c(.den_words,
                                         x$populations$population_id,
                                         x$datasets$dataset,
                                         x$analysis_data$data_id))
  if (length(bad)) add(bad[1L], "denominator", sprintf(
    "denominator(s) %s: population, row, column, cell, a population, a dataset or an analysis data",
    paste(unique(den[bad]), collapse = ", ")))
  ds_a <- which(!is.na(a$dataset) & !a$dataset %in% x$datasets$dataset)
  pops <- x$populations
  ds_p <- which(!is.na(pops$dataset) & !pops$dataset %in% x$datasets$dataset)
  miss <- unique(c(a$dataset[ds_a], pops$dataset[ds_p]))
  if (length(miss)) {
    msg <- sprintf("dataset(s) not in `datasets`: %s", paste(miss, collapse = ", "))
    if (length(ds_a)) add(ds_a[1L], "dataset", msg) else
      pr[[length(pr) + 1L]] <- .spec_problem(msg, "populations", NA_character_,
                                             .na_or(pops$population_id[ds_p[1L]], ""),
                                             "dataset")
  }
  bad <- which(!is.na(a$population_id) &
                 !a$population_id %in% x$populations$population_id)
  if (length(bad)) add(bad[1L], "population_id", sprintf(
    "population(s) not in `populations`: %s",
    paste(unique(a$population_id[bad]), collapse = ", ")))
  keys <- tfl_ard_methods()
  st <- tfl_ard_statistics()
  for (i in seq_len(nrow(a))) {
    k <- .method_key(a$method[i], keys)
    s <- .split_bar(a$statistics[i])
    if (!is.na(k) && keys$kind[k] == "continuous") {
      bad <- setdiff(s, st$statistic[st$kind == "continuous"])
      if (length(bad)) add(i, "statistics", sprintf(
        "%s / %s: no continuous statistic %s (see tfl_ard_statistics())",
        a$output_id[i], a$analysis_id[i], paste(bad, collapse = ", ")))
    }
    f <- .parse_formats(a$formats[i])
    bad <- names(f)[is.na(f) | !.fmt_ok(f)]
    if (length(bad)) add(i, "formats", sprintf(
      "%s / %s: formats are statistic=format, the format xx.x, xx.x%%, a number of decimals or pvalue (%s)",
      a$output_id[i], a$analysis_id[i], paste(bad, collapse = ", ")))
  }
  cust <- which(a$method %in% "custom" & is.na(a$code))
  if (length(cust)) add(cust[1L], "code", "a `custom` analysis needs its `code`")
  p <- do.call(rbind, pr)
  rownames(p) <- NULL
  p
}

# `a`, or `b` when it is NA or empty
.na_or <- function(a, b) if (length(a) && !is.na(a[1L])) a[1L] else b

# An analysis run inside another (`parent`): the parent is an analysis of
# the same report that runs others (cards::ard_stack(), ard_strata(),
# ard_pairwise()), and the rows inside take its data -- they say what is
# computed, not on what.  The problems as rows (.ard_problem()).
.ard_parent_problems <- function(a) {
  par <- a$parent %||% rep(NA_character_, nrow(a))
  pr <- list(.spec_problems_empty())
  add <- function(i, field, message) {
    pr[[length(pr) + 1L]] <<- .ard_problem(a, i, field, message)
  }
  tag <- function(i) paste(a$output_id[i], a$analysis_id[i], sep = " / ")
  for (i in which(!is.na(par))) {
    p <- which(a$output_id == a$output_id[i] & a$analysis_id == par[i])
    if (!length(p)) {
      add(i, "parent", sprintf("%s: parent %s is not an analysis of %s", tag(i),
                               par[i], a$output_id[i]))
      next
    }
    p <- p[1L]
    if (!a$method[p] %in% .ard_wrappers) {
      add(i, "parent", sprintf(paste(
        "%s: its parent %s runs no other analyses -- a parent's method is",
        "one of %s"), tag(i), par[i], paste(.ard_wrappers, collapse = ", ")))
      next
    }
    if (!is.na(par[p])) {
      add(i, "parent", sprintf("%s: its parent %s is inside another itself",
                               tag(i), par[i]))
    }
    own <- c(intersect("data", names(a)), "dataset", "population_id", "where",
             if (identical(a$method[p], "cards::ard_stack")) c("by", "strata"))
    set <- own[!is.na(unlist(a[i, own]))]
    if (length(set)) {
      add(i, set[1L], sprintf(
        "%s: %s %s the parent's (%s); leave blank", tag(i),
        paste0("`", set, "`", collapse = ", "),
        if (length(set) > 1L) "are" else "is", par[i]))
    }
    if (a$method[i] %in% c("subjects", "custom", .ard_wrappers)) {
      add(i, "method", sprintf("%s: a `%s` analysis cannot run inside %s",
                               tag(i), a$method[i], par[i]))
    }
    if (.names_data(a[i, ])) {
      add(i, "args", sprintf(paste(
        "%s: inside %s the data is the parent's, so its args may not name",
        "`data` or `population`"), tag(i), par[i]))
    }
  }
  for (p in which(a$method %in% .ard_wrappers)) {
    kids <- which(!is.na(par) & par == a$analysis_id[p] &
                    a$output_id == a$output_id[p])
    if (!length(kids)) next
    if (!identical(a$method[p], "cards::ard_stack") && length(kids) > 1L) {
      add(p, "method", sprintf("%s: %s runs one analysis; %d name it as parent",
                               tag(p), a$method[p], length(kids)))
    }
    if (identical(a$method[p], "cards::ard_pairwise") &&
        length(.split_bar(a$variables[p])) != 1L) {
      add(p, "variables", sprintf(
        "%s: ard_pairwise() compares the pairs of ONE column's levels: `variables`",
        tag(p)))
    }
    if (identical(a$method[p], "cards::ard_stack")) {
      v <- unlist(lapply(kids, function(k) .split_bar(a$variables[k])))
      if (anyDuplicated(v)) {
        add(p, "variables", sprintf(paste(
          "%s: %s is analysed by two of the analyses inside it, whose rows",
          "could not be told apart; make one of them an analysis of its own"),
          tag(p), paste(unique(v[duplicated(v)]), collapse = ", ")))
      }
      keys <- tfl_ard_methods()
      for (k in kids) {
        kk <- .method_key(a$method[k], keys)
        kind <- if (is.na(kk)) "" else keys$kind[kk]
        if (!is.na(a$statistics[k]) &&
            !kind %in% c("continuous", "categorical", "missing")) {
          add(k, "statistics", sprintf(paste(
            "%s: inside a stack, `statistics` cannot keep some of what %s",
            "gives; make it an analysis of its own"), tag(k), a$method[k]))
        }
      }
    }
  }
  do.call(rbind, pr)
}

# The analyses as ARS sees them: an analysis inside another is one of its
# own, on the parent's data, analysis set and condition, grouped by the
# parent's groups (a stack's by; the subgroups of ard_strata()); the parent
# that only runs them is not an analysis.
.ard_spec_flat <- function(a) {
  par <- a$parent %||% rep(NA_character_, nrow(a))
  if (all(is.na(par))) return(a)
  drop <- logical(nrow(a))
  for (i in which(!is.na(par))) {
    p <- which(a$output_id == a$output_id[i] & a$analysis_id == par[i])[1L]
    if (is.na(p)) next
    for (cn in c("dataset", "population_id", "where")) a[[cn]][i] <- a[[cn]][p]
    grp <- c(.split_bar(a$by[p]),
             if (!identical(a$method[p], "cards::ard_stack"))
               c(.split_bar(a$strata[p]), .split_bar(a$by[i])))
    a$by[i] <- if (length(grp)) paste(unique(grp), collapse = " | ") else NA
    drop[p] <- TRUE
  }
  a <- a[!drop, , drop = FALSE]
  a$parent <- NA_character_
  rownames(a) <- NULL
  a
}

# The `post` column: steps on the ARD after the call, each a call whose
# first argument -- the ARD -- is left out
# (`cards::add_calculated_row(expr = sd / sqrt(N), stat_name = "se")`),
# `|` between them.  A `|` inside a call (an R "or") is the call's.
.split_post <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(trimws(x))) return(character())
  ch <- strsplit(x, "", fixed = TRUE)[[1L]]
  depth <- cumsum(ch %in% c("(", "[", "{")) - cumsum(ch %in% c(")", "]", "}"))
  quote <- character()
  cut <- integer()
  for (i in seq_along(ch)) {
    if (length(quote)) {
      if (ch[i] == quote && (i == 1L || ch[i - 1L] != "\\")) quote <- character()
      next
    }
    if (ch[i] %in% c("\"", "'")) quote <- ch[i]
    else if (ch[i] == "|" && depth[i] == 0L) cut <- c(cut, i)
  }
  parts <- substring(x, c(1L, cut + 1L), c(cut - 1L, nchar(x)))
  parts <- trimws(parts)
  parts[nzchar(parts)]
}

# what is wrong with one step of `post`, or NULL
.post_problem <- function(step) {
  e <- tryCatch(str2lang(step), error = function(e) NULL)
  fn <- if (is.call(e)) paste(deparse(e[[1L]]), collapse = "") else ""
  named <- grepl("^([A-Za-z.][A-Za-z0-9._]*:::?)?[A-Za-z.][A-Za-z0-9._]*$", fn)
  if (!named) {
    return(sprintf("`%s` is not a call (a function with its arguments, the ARD left out)",
                   step))
  }
  f <- tryCatch(eval(e[[1L]]), error = function(e) NULL)
  if (is.function(f) && !length(formals(f))) {
    return(sprintf("%s takes no ARD", fn))
  }
  NULL
}

# A report's code lists, each variable's values in their order and what
# each is in the ARD: the `codelists` sheet of a table definition, or a
# data frame with `output_id`, `variable`, `value`, `label` and `order`.  A
# variable's list is the labels named by their values (c(F = "Female"))
# when any value has a label (one without stays itself), else the values
# alone.  A code list is a report's: a row without `output_id` stops.
# `vars`: only these variables (the ones the report's program reads); NULL,
# all the report's.
.codelist_levels <- function(codelists, output_id = NULL, vars = NULL) {
  if (is.null(codelists)) return(NULL)
  if (inherits(codelists, "tfl_table_spec") || (is.list(codelists) &&
                                                !is.data.frame(codelists))) {
    codelists <- codelists$codelists
  }
  if (!is.data.frame(codelists) || !nrow(codelists)) return(NULL)
  d <- as.data.frame(codelists, stringsAsFactors = FALSE)
  .codelists_check(d)
  if (length(output_id) != 1L || is.na(output_id)) return(NULL)
  d <- d[d$output_id == output_id & !is.na(d$variable) & !is.na(d$value), ,
         drop = FALSE]
  if (!is.null(vars)) d <- d[d$variable %in% vars, , drop = FALSE]
  if (!nrow(d)) return(NULL)
  ord <- suppressWarnings(as.numeric(d$order %||% NA))
  d <- d[order(match(d$variable, unique(d$variable)), is.na(ord), ord), , drop = FALSE]
  d <- d[!duplicated(d[c("variable", "value")]), , drop = FALSE]
  lab <- if (is.null(d$label)) rep(NA_character_, nrow(d)) else as.character(d$label)
  lab[!is.na(lab) & !nzchar(trimws(lab))] <- NA
  lapply(split(seq_len(nrow(d)), factor(d$variable, levels = unique(d$variable))),
         function(i) {
           v <- as.character(d$value[i])
           l <- lab[i]
           if (all(is.na(l))) return(v)
           stats::setNames(ifelse(is.na(l), v, l), v)
         })
}

# Every code list row names its report (no study-wide rows)
.codelists_check <- function(d) {
  if (!is.null(d) && nrow(d) && !"output_id" %in% names(d)) {
    .ard_stop("The code lists have no `output_id` column.\n",
              "  A code list is a report's: give each row its report.")
  }
  .pb_stop_first(.codelists_problems(d))
}

.codelists_problems <- function(d) {
  if (is.null(d) || !nrow(d) || !"output_id" %in% names(d)) return(.pb_none())
  used <- !is.na(d$variable) | !is.na(d$value)
  blank <- which(used & (is.na(d$output_id) | !nzchar(trimws(d$output_id))))
  .pb(sprintf(paste0(
    "Row %d of the code lists (%s) has no `output_id`.\n",
    "  A code list is a report's: give the report, and copy the rows into ",
    "each report that uses them."),
    blank, paste(d$variable[blank], d$value[blank], sep = " / ")),
    blank, rep("output_id", length(blank)))
}

# The columns a report's analyses read as variables: by, strata, variables,
# and the names their own R (args, code, post) uses
.ard_read_vars <- function(a) {
  cols <- unlist(lapply(c("by", "strata", "variables"), function(cn)
    unlist(lapply(a[[cn]], .split_bar))))
  r <- unlist(lapply(c("args", "code", "post"), function(cn) a[[cn]]))
  r <- r[!is.na(r) & nzchar(r)]
  syms <- unlist(lapply(r, function(txt) tryCatch(all.names(parse(text = txt)),
                                                  error = function(e) NULL)))
  unique(c(cols, syms))
}

# Each report's code lists for its ARD (NULL when none has any)
.codelist_levels_by <- function(codelists, a) {
  outs <- unique(a$output_id)
  lv <- lapply(stats::setNames(outs, outs), function(o)
    .codelist_levels(codelists, o, .ard_read_vars(a[a$output_id == o, , drop = FALSE])))
  if (all(lengths(lv) == 0L)) NULL else lv
}

.study_value <- function(x, key, default) {
  v <- x$study$value[match(key, x$study$key)]
  if (length(v) && !is.na(v)) v else default
}

.r_name <- function(x) make.names(tolower(x))

# A condition and another: the second in brackets only when it needs them
# (an "or", or R that does not read)
.cond_and <- function(a, b) {
  e <- tryCatch(str2lang(b), error = function(err) NULL)
  low <- is.call(e) && as.character(e[[1L]]) %in% c("|", "||")
  sprintf(if (is.null(e) || low) "%s & (%s)" else "%s & %s", a, b)
}

# `NAME = expr | NAME = expr` as a transform() call on `obj`
.derive_code <- function(obj, derive) {
  d <- .split_bar(derive)
  if (!length(d)) return(NULL)
  sprintf("%s <- transform(%s, %s)", obj, obj, paste(d, collapse = ", "))
}

.reader <- function(path) {
  ext <- tolower(tools::file_ext(path))
  # an .rda / .RData file holds one dataset: the object it has
  if (ext %in% c("rda", "rdata")) {
    return(sprintf("local({ e <- new.env(); load(%s, envir = e); e[[ls(e)[1L]]] })",
                   .path_code(path)))
  }
  f <- switch(ext, rds = "readRDS", xpt = "haven::read_xpt",
              sas7bdat = "haven::read_sas", csv = "utils::read.csv",
              parquet = "arrow::read_parquet",
              stop("No reader for .", ext, call. = FALSE))
  sprintf("%s(%s)", f, .path_code(path))
}

# Does the R a row writes itself (args, code) name `data` or `population`?
.names_data <- function(r) {
  txt <- c(r$args, r$code)
  txt <- txt[!is.na(txt) & nzchar(txt)]
  if (!length(txt)) return(FALSE)
  syms <- tryCatch(unlist(lapply(txt, function(t)
    all.names(parse(text = paste0("f(", t, "\n)"), keep.source = FALSE)))),
    error = function(e) {
      # code that is not a call's arguments (custom): parse it whole
      tryCatch(all.names(parse(text = txt, keep.source = FALSE)),
               error = function(e) c("data", "population"))
    })
  any(c("data", "population") %in% syms)
}

# The function a method calls, when it can be found (else NULL)
.method_fun <- function(m) {
  keys <- tfl_ard_methods()
  k <- .method_key(m, keys)
  fn <- if (is.na(k)) m else keys$call[k]
  if (startsWith(fn, "(")) return(NULL)
  f <- tryCatch(eval(str2lang(fn)), error = function(e) NULL)
  if (is.function(f)) f else NULL
}

# the continuous statistics the ARD program computes (not cards)
.computed_stats <- function() {
  d <- tfl_ard_statistics("continuous")
  d$statistic[!is.na(d$fun)]
}

# the kinds of statistic a method's analysis may ask for
.stat_kinds <- function(kind) {
  switch(kind %||% "",
         continuous = "continuous", categorical = "categorical",
         missing = "missing", "result")
}

.fmt_ok <- function(f) grepl("^(x+(\\.x+)?%?|[0-9]+|pvalue)$", f)

# `mean=xx.x | AGE:sd=xx.xx` as a named vector
.parse_formats <- function(x) {
  p <- .split_bar(x)
  if (!length(p)) return(character())
  k <- trimws(sub("=.*$", "", p))
  v <- trimws(sub("^[^=]*=", "", p))
  v[!grepl("=", p, fixed = TRUE)] <- NA
  stats::setNames(v, k)
}

# The cards / cardx functions whose call takes fmt_fun (that of their
# data.frame method; cards 0.9.0, cardx 0.3.4), for when the package is not
# there to ask.  The rest -- cardx's tests, CIs and models, ard_stack(),
# ard_stack_hierarchical(), ard_total_n() -- get their formats after the
# call (fmt_ard()).
.fmt_fun_known <- c("cards::ard_summary", "cards::ard_tabulate",
                    "cards::ard_tabulate_value", "cards::ard_missing",
                    "cards::ard_hierarchical", "cards::ard_hierarchical_count",
                    "cards::ard_mvsummary", "cards::ard_tabulate_rows",
                    "cardx::ard_tabulate_max")

# Does a method's function take fmt_fun?  A subject flag is
# cards::ard_tabulate_value(); code, a study's own function, no.
.takes_fmt_fun <- function(fn) {
  if (identical(fn, "(subjects)")) fn <- "cards::ard_tabulate_value"
  if (startsWith(fn, "(") || !grepl("::", fn, fixed = TRUE)) return(FALSE)
  f <- tryCatch(eval(str2lang(fn)), error = function(e) NULL)
  if (!is.function(f)) return(fn %in% .fmt_fun_known)
  if ("fmt_fun" %in% names(formals(f))) return(TRUE)
  m <- tryCatch(utils::getS3method(sub("^.*::", "", fn), "data.frame",
                                   optional = TRUE, envir = environment(f)),
                error = function(e) NULL)
  is.function(m) && "fmt_fun" %in% names(formals(m))
}

#' The R code that makes the study's ARD
#'
#' One cards / cardx call per analysis row, each result under a name of its
#' own (`ard_<analysis_id>`) and tagged with its `output_id`, `analysis_id`
#' and `population_id` (`tag_ard()`), bound into one ARD and saved to the
#' study key `output` (default `output/ard/ard.rds`).  The code runs from
#' the study folder, and reads as a person writes it: the tidyverse layout
#' (a call on one line when it fits in 80 characters, else an argument a
#' line), dplyr verbs for the data (`filter()`, `mutate()`, `select()`), and
#' a `custom` analysis's code with the program's names in place of `data`
#' and `population` (in `local()` only when it needs a scope of its own: it
#' defines a function, or assigns a name the program has).
#'
#' The formats (the method's and the row's `formats`, over each statistic's
#' default in [tfl_ard_statistics()]) are in the cards call itself, its
#' `fmt_fun` argument, where a reader sees them next to the statistics:
#' `fmt_fun = everything() ~ modifyList(fmt_default, list(mean = 2L))`, a
#' variable's own (`BMIBL:sd=3`) with its own list.  An integer is
#' that many decimals, `xx.x%` is `label_round(1, scale = 100)` and
#' `pvalue` the program's `fmt_pvalue()` (`<0.001` or 3 decimals);
#' `cards::apply_fmt_fun()` then fills `stat_fmt`.  What takes no `fmt_fun`
#' gets the same formats after the call, from `fmt_ard()`: cardx's tests, CIs
#' and models, `cards::ard_stack_hierarchical()`, a study's own function,
#' `custom` code, `ard_pairwise()`, `ard_stack()`'s own rows (the by counts,
#' the total N), an analysis whose `args` gives `fmt_fun` or whose `post`
#' changes the ARD, a variable's own format for `ard_hierarchical()` or
#' `ard_tabulate_rows()`.  `stat_fmt` is the same either way.
#'
#' The functions the program calls -- `set_levels()`, `tag_ard()`,
#' `fmt_ard()`, `keep_stats()`, `fmt_pvalue()`, `save_ard()` -- are
#' [tfl_helpers_code()]'s: the whole program (`part = "all"`) carries them,
#' and a study keeps them in a file of its own, so its programs run without
#' tflspec.
#'
#' `part` gives a piece of it instead, for a program layout of one's own
#' (tflplanner writes one program per output that sources a shared setup):
#' `"setup"` is what every piece starts with -- `library(cards)`,
#' `library(dplyr)`, every computed statistic of the catalog (`tfl_stats`)
#' and each statistic's format (`fmt_default`); `"body"` is the analyses of
#' `output_id`, starting with `report_id <- "..."` and its code lists
#' (`cl_<variable>`), and ending in `save_ard()` (one report, `save =
#' TRUE`) or `ard`.
#'
#' @param spec An [tfl_ard_spec()] (or the path of one).
#' @param output_id Only these outputs' analyses; `NULL` for all.
#' @param save `FALSE` leaves out the final `saveRDS()`.  For one report's
#'   `"body"`, `TRUE` ends it in `save_ard()` (its rows into the
#'   study ARD, with the definition's fingerprint), `FALSE` in `ard`.
#' @param part `"all"` (the whole program), `"setup"` or `"body"`.
#' @param dir The study folder: the fingerprints saved with the ARD
#'   ([tfl_ard_spec_hash()]) read the study's own function files from it.
#' @param codelists The reports' code lists: a table definition (its
#'   `codelists` sheet) or a data frame with `output_id`, `variable`,
#'   `value`, `label` and `order`.  A code list is a report's: every row
#'   names its report (a blank `output_id` stops).  A report's rows of the
#'   variables its analyses read (`by`, `strata`, `variables`, and the
#'   names in `args`, `code` and `post`) count: the program writes them at
#'   its head (`cl_sex <- c(F = "Female", M = "Male")`) and puts them on
#'   the data the analyses read (`set_levels(SEX = cl_sex)`): each column a
#'   factor in the list's order, its values the labels (a value without one
#'   stays itself) -- the ARD holds `"Female"`, and counts a level no record
#'   has (`n = 0`).  A value the list does not have (not NA) stops the
#'   program.  An analysis's `where` on such a column is written in its
#'   labels: one that names a value whose label differs stops here.  In the
#'   study's program, each report's part reads its data again with its own.
#'   `NULL` (default): the data as read.
#' @inheritParams tfl_write_ard_spec
#' @return The code, one element per line.
#' @export
tfl_ard_code <- function(spec, output_id = NULL, save = TRUE,
                          part = c("all", "setup", "body"),
                          statistics = NULL, methods = NULL, dir = ".",
                          codelists = NULL) {
  part <- match.arg(part)
  old <- .set_catalogs(statistics, methods)
  on.exit(options(old), add = TRUE)
  x <- .as_spec(spec, "ard", "tfl_ard_code")
  a <- x$analyses
  if (!is.null(output_id)) a <- a[a$output_id %in% output_id, , drop = FALSE]
  # written for its setup, which attaches cards and dplyr (not when
  # tfl_build_ard() runs it: every `pkg::` kept)
  if (!isTRUE(getOption("tflspec.plain_ns"))) {
    user <- .attached()
    old_att <- options(tflspec.user_attached = user,
                       tflspec.attached = union(user, c(.ard_pkgs, .ard_base_pkgs)))
    on.exit(options(old_att), add = TRUE)
  }
  if (part == "setup") {
    st <- tfl_ard_statistics("continuous")
    return(.drop_attached_ns(.ard_common_lines(st$statistic[!is.na(st$fun)], x)))
  }
  # each report's code lists, of the variables its analyses read
  lv <- .codelist_levels_by(codelists, a)
  .check_where_labels(a, lv)
  if (part == "body") {
    # one report's: its ARD into the study's, with its definition's
    # fingerprint
    sv <- if (save && length(unique(a$output_id)) == 1L) {
      out <- .study_value(x, "output", "output/ard/ard.rds")
      list(comment = c("# the report's rows of the study ARD, with the fingerprint of its",
                       "# definition (tfl_ard_spec_hash())"),
           args = c(list(definition = .q(.ard_output_hash(x, a$output_id[1L], dir,
                                                           codelists))),
                    if (out != "output/ard/ard.rds") list(path = .path_code(out))))
    }
    return(.drop_attached_ns(.ard_body_lines(x, a, lv, save = sv)))
  }
  out <- .study_value(x, "output", "output/ard/ard.rds")
  code <- c(
    "# The study's ARD, made from its ARD definition.  Run from the study folder.",
    paste0("# Generated by tflspec ", utils::packageVersion("tflspec"),
           ", ", format(Sys.Date())),
    "",
    # (with the functions the program calls: a study keeps them in a file
    # of its own, tfl_helpers_code())
    .ard_common_lines(unlist(lapply(a$statistics, .split_bar)), x,
                      helpers = tfl_helpers_code()),
    .ard_body_lines(x, a, lv))
  if (save) {
    ids <- unique(a$output_id)
    hashes <- vapply(ids, function(id) .ard_output_hash(x, id, dir, codelists), "")
    q <- function(v) paste(encodeString(v, quote = "\""), collapse = ", ")
    code <- c(code,
              sprintf("dir.create(dirname(%s), recursive = TRUE, showWarnings = FALSE)",
                      .path_code(out)),
              sprintf("saveRDS(ard, %s)", .path_code(out)),
              "# what was built, and from which definition (tfl_ard_spec_hash())",
              "status <- data.frame(",
              sprintf("  output_id = c(%s),", q(ids)),
              sprintf("  definition = c(%s),", q(hashes)),
              "  built = format(Sys.time(), \"%Y-%m-%d %H:%M:%S\"),",
              "  stringsAsFactors = FALSE)",
              "status$rows <- as.integer(table(factor(ard$output_id, levels = status$output_id)))",
              sprintf("utils::write.csv(status, file.path(dirname(%s), \"ard_status.csv\"), row.names = FALSE)",
                      .path_code(out)))
  }
  .drop_attached_ns(c(code, ""))
}

# A fingerprint of what makes an output's ARD: its analysis rows and the
# data, populations and study keys they use.  A built ARD whose fingerprint
# differs from the definition's now is outdated.
#' A fingerprint of one output's ARD definition
#'
#' The md5 of what makes an output's ARD: its analysis rows and the data,
#' populations and study keys they use, and the content of the study's own
#' function files (the study key `source`; a file not there counts as
#' `"missing"`).  An ARD built from a definition whose fingerprint differs
#' from the one now is outdated.  A column blank in every analysis row of
#' the output does not count, so a column added to the definition later
#' leaves the fingerprints as they were.
#'
#' @param spec An [tfl_ard_spec()].
#' @param output_id The output.
#' @param dir The study folder, which `source` files are relative to.
#' @param codelists As [tfl_ard_code()]: they change the ARD, so they are
#'   part of the fingerprint (only when there are some).
#' @return A single string.
#' @export
tfl_ard_spec_hash <- function(spec, output_id, dir = ".", codelists = NULL) {
  .ard_output_hash(spec, output_id, dir, codelists)
}

.ard_output_hash <- function(spec, output_id, dir = ".", codelists = NULL) {
  a <- spec$analyses[spec$analyses$output_id %in% output_id, , drop = FALSE]
  a <- a[vapply(a, function(v) !all(is.na(v)), NA)]
  src <- .split_bar(.study_value(spec, "source", NA))
  src_md5 <- vapply(src, function(f) {
    p <- file.path(dir, f)
    if (file.exists(p)) unname(tools::md5sum(p)) else "missing"
  }, "")
  ad <- .adata_of(.adata_sheet(spec), output_id)
  ad <- ad[ad$data_id %in% .adata_used(ad, a), , drop = FALSE]
  # a column added since (subjects, keep) blank in every row does not count
  later <- c("subjects", "keep", "code")
  ad <- ad[setdiff(names(ad), later[vapply(later, function(cn) all(is.na(ad[[cn]])), NA)])]
  pops <- spec$populations[spec$populations$population_id %in%
                             c(a$population_id, ad$population_id), , drop = FALSE]
  dss <- spec$datasets[spec$datasets$dataset %in%
                         c(a$dataset, pops$dataset, ad$from), , drop = FALSE]
  txt <- paste(c(utils::capture.output(print(as.list(a[order(a$analysis_id), ]))),
                 if (nrow(ad)) utils::capture.output(print(as.list(ad))),
                 utils::capture.output(print(as.list(pops))),
                 utils::capture.output(print(as.list(dss[setdiff(names(dss), "level")]))),
                 utils::capture.output(print(as.list(spec$study))),
                 if (length(src)) paste(src, src_md5),
                 # the code lists, only when there are some: a study without
                 # keeps the fingerprints it had
                 if (length(lv <- .codelist_levels(codelists, output_id,
                                                   .ard_read_vars(a))))
                   utils::capture.output(print(lv))),
               collapse = "\n")
  f <- tempfile()
  on.exit(unlink(f))
  writeLines(enc2utf8(txt), f, useBytes = TRUE)
  unname(tools::md5sum(f))
}

#' Make the study's ARD from its definition
#'
#' Runs [tfl_ard_code()] from the study folder: the ARD is exactly what the
#' code gives.
#'
#' @inheritParams tfl_ard_code
#' @param dir The study folder (the code's working directory).
#' @return The ARD (with `output_id`, `analysis_id`, `population_id`),
#'   invisibly; with `save`, also written where the study key `output`
#'   says.
#' @export
tfl_build_ard <- function(spec, dir = ".", output_id = NULL, save = TRUE,
                      statistics = NULL, methods = NULL, codelists = NULL) {
  # run where nothing is attached and no folder variable is defined
  old <- options(tflspec.paths = NULL, tflspec.attached = NULL,
                 tflspec.plain_ns = TRUE)
  on.exit(options(old), add = TRUE)
  code <- tfl_ard_code(spec, output_id = output_id, save = save,
                        statistics = statistics, methods = methods, dir = dir,
                        codelists = codelists)
  owd <- setwd(dir)
  on.exit(setwd(owd), add = TRUE)
  e <- new.env(parent = globalenv())
  eval(parse(text = code, encoding = "UTF-8"), envir = e)
  invisible(e$ard)
}

#' One output's part of the study ARD
#'
#' @param ard The study ARD ([tfl_build_ard()]).
#' @param output_id The output.
#' @return Its rows, without the id columns: what [rtfreporter::normalize_ard()]
#'   takes.
#' @export
tfl_ard_for <- function(ard, output_id) {
  if (!is.data.frame(ard) || !"output_id" %in% names(ard)) {
    .ard_stop(sprintf(
      "tfl_ard_for(): `ard` must be a study ARD (tfl_build_ard()), with an output_id column; got %s.",
      .what(ard)))
  }
  have <- unique(stats::na.omit(ard$output_id))
  miss <- setdiff(output_id, have)
  if (length(miss)) {
    .ard_stop(sprintf(
      "tfl_ard_for(): the ARD has no output %s.\n  It has: %s.",
      paste(encodeString(miss, quote = "'"), collapse = ", "),
      if (length(have)) paste(have, collapse = ", ") else "(none)"))
  }
  d <- ard[ard$output_id %in% output_id, , drop = FALSE]
  d[setdiff(names(d), c("output_id", "analysis_id", "population_id"))]
}

#' An analysis as R: the code of a custom analysis
#'
#' The call an analysis row of the definition stands for, written as the
#' `code` of a `custom` analysis (where `data` and `population` are the
#' analysis's data and analysis set), with the formats its method gives by
#' default written out: an analysis the columns cannot say starts from what
#' they say now, and gives the same ARD.  The definition keeps the R (the
#' program written from it is never edited).
#'
#' @param spec A `tfl_ard_spec` (or the list of its sheets).
#' @param output_id,analysis_id The analysis.
#' @return A list: `code` (the R; `NA` for an analysis that runs others
#'   inside it, or one run inside another), `formats` (the row's `formats`
#'   with its method's defaults, as the column writes them; `NA` for none),
#'   `method` (`"custom"`).
#' @export
tfl_ard_as_custom <- function(spec, output_id, analysis_id) {
  x <- spec
  a <- x$analyses
  i <- which(a$output_id %in% output_id & a$analysis_id %in% analysis_id)
  if (length(i) != 1L) {
    stop("tfl_ard_as_custom(): no analysis ", output_id, " / ", analysis_id, ".",
         call. = FALSE)
  }
  r <- a[i, , drop = FALSE]
  par <- a$parent %||% rep(NA_character_, nrow(a))
  none <- list(code = NA_character_, formats = NA_character_, method = "custom")
  if (!is.na(par[i]) || any(!is.na(par) & par == analysis_id & a$output_id == output_id)) {
    return(none)
  }
  if (identical(r$method, "custom")) {
    return(list(code = r$code, formats = r$formats, method = "custom"))
  }
  subj <- .study_value(x, "id", "USUBJID")
  x$analysis_data <- .adata_of(.adata_sheet(x), output_id)
  d <- .data_col(r)
  if (!is.na(d)) r$population_id <- .adata_pop(x$analysis_data, d)
  keys <- tfl_ard_methods()
  pid <- r$population_id
  pop <- if (!is.na(pid)) paste0("pop_", .r_name(pid))
  given <- c(.args_given(r$args), if (!is.na(r$strata)) "strata",
             if (!is.na(r$denominator)) "denominator")
  has <- function(arg) arg %in% given
  den <- .den_code(r$denominator, x, pop, subj, population = "population")
  body <- .analysis_body(r, keys, subj, has, den)
  k <- .method_key(r$method, keys)
  fmt <- c(if (!is.na(k)) .parse_formats(keys$formats[k]), .parse_formats(r$formats))
  fmt <- fmt[!duplicated(names(fmt), fromLast = TRUE)]
  fmt <- if (!length(fmt)) NA_character_ else
    paste(ifelse(is.na(fmt), names(fmt), paste0(names(fmt), "=", fmt)), collapse = " | ")
  list(code = body, formats = fmt, method = "custom")
}

# An analysis's condition runs on its data after the code lists are put on
# it (set_levels()): a listed variable's values are their labels there.  A
# condition that names a value whose label differs (SEX == "F" where F is
# Female) would keep no row: it stops, and says the label to write.
.check_where_labels <- function(a, levels) {
  if (!length(levels)) return(invisible(NULL))
  for (i in which(!is.na(a$where))) {
    lv <- levels[[a$output_id[i]]]
    if (!length(lv)) next
    bad <- .where_values_not_labels(a$where[i], lv)
    if (length(bad)) {
      .ard_stop(sprintf(paste0(
        "%s / %s: `where` is `%s`, but the analysis's data have the code ",
        "lists' labels by then -- write %s."),
        a$output_id[i], a$analysis_id[i], a$where[i],
        paste(sprintf("%s for %s", encodeString(unname(bad), quote = "\""),
                      encodeString(names(bad), quote = "\"")), collapse = ", ")))
    }
  }
  invisible(NULL)
}

# The values a condition compares a listed variable with whose label
# differs: named vector, label named by the value
.where_values_not_labels <- function(where, lv) {
  e <- tryCatch(str2lang(where), error = function(err) NULL)
  out <- character()
  walk <- function(x) {
    if (!is.call(x)) return(invisible())
    op <- as.character(x[[1L]])[1L]
    if (op %in% c("==", "!=", "%in%") && length(x) == 3L) {
      for (k in 2:3) {
        v <- x[[k]]
        other <- x[[5L - k]]
        if (!is.name(v)) next
        cl <- lv[[as.character(v)]]
        if (is.null(names(cl))) next
        vals <- tryCatch(eval(other, baseenv()), error = function(err) NULL)
        if (!is.character(vals)) next
        hit <- vals[vals %in% names(cl) & !vals %in% cl]
        out <<- c(out, stats::setNames(unname(cl[hit]), hit))
      }
    }
    for (y in as.list(x)[-1L]) walk(y)
  }
  walk(e)
  out[!duplicated(names(out))]
}

# A condition written in the code lists' labels as the data's own values
# (SEX == "Female" -> SEX == "F"), for what reads the data as they are
# (ARS); a condition with no listed variable, as it is
.where_labels_to_values <- function(where, lv) {
  if (is.na(where) || !length(lv)) return(where)
  e <- tryCatch(str2lang(where), error = function(err) NULL)
  if (is.null(e)) return(where)
  changed <- FALSE
  back <- function(cl, vals) {
    to <- stats::setNames(names(cl), unname(cl))
    ifelse(vals %in% names(to), unname(to[vals]), vals)
  }
  walk <- function(x) {
    if (!is.call(x)) return(x)
    op <- as.character(x[[1L]])[1L]
    if (op %in% c("==", "!=", "%in%") && length(x) == 3L) {
      for (k in 2:3) {
        v <- x[[k]]
        if (!is.name(v)) next
        cl <- lv[[as.character(v)]]
        if (is.null(names(cl))) next
        other <- x[[5L - k]]
        vals <- tryCatch(eval(other, baseenv()), error = function(err) NULL)
        if (!is.character(vals)) next
        new <- back(cl, vals)
        if (identical(new, vals)) next
        changed <<- TRUE
        x[[5L - k]] <- if (length(new) == 1L) new else as.call(c(as.name("c"), as.list(new)))
      }
      return(x)
    }
    for (j in seq_along(x)[-1L]) x[[j]] <- walk(x[[j]])
    x
  }
  e <- walk(e)
  if (!changed) return(where)
  paste(deparse(e, width.cutoff = 500L), collapse = " ")
}
