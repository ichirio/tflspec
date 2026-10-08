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
  if (is.null(x) || is.na(x) || !nzchar(trimws(x))) return(character())
  trimws(strsplit(x, "|", fixed = TRUE)[[1L]])
}

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

# The call an analysis row stands for, with `data` and `population` bound.
.analysis_body <- function(r, keys, subj, has, den = NULL, data = "data",
                           population = "population", fmt_fun = NULL) {
  m <- r$method
  k <- .method_key(m, keys)
  fn <- if (is.na(k)) m else keys$call[k]
  kind <- if (is.na(k)) "" else keys$kind[k]
  stats <- if (!is.na(r$statistics)) r$statistics else if (!is.na(k) &&
    nzchar(keys$statistics[k])) keys$statistics[k] else NA
  if (identical(fn, "(code)")) return(r$code)
  by <- .vars(r$by)
  vars <- .vars(r$variables)
  strata <- .vars(r$strata)
  own <- c(if (!is.null(strata)) paste("strata =", strata),
           if (!is.null(den)) paste("denominator =", den))
  if (identical(fn, "(subjects)")) {
    # a subject-level flag: has the subject any record of the data?
    flag <- if (length(.split_bar(r$variables))) .split_bar(r$variables)[1L] else
      make.names(r$analysis_id)
    st <- if (!has("statistic")) .stat_arg("categorical", stats)
    return(paste0(
      sprintf("population$%s <- population$%s %%in%% data$%s\n", flag, subj,
              subj),
      sprintf("cards::ard_tabulate_value(population%s, variables = %s, value = list(%s = TRUE)%s%s%s%s)",
              if (!is.null(by)) paste0(", by = ", by) else "", flag, flag,
              if (!is.null(st)) paste0(", ", st) else "",
              if (length(own)) paste0(", ", paste(own, collapse = ", ")) else "",
              if (!is.na(r$args)) paste0(", ", r$args) else "",
              if (!is.null(fmt_fun)) paste0(",\n    ", fmt_fun) else "")))
  }
  # the keyword's own arguments, each unless the row's args gives it
  dflt <- if (!is.na(k) && nzchar(keys$defaults[k])) {
    d <- trimws(strsplit(gsub("<id>", subj, keys$defaults[k], fixed = TRUE),
                         ",")[[1L]])
    d <- d[!vapply(sub("\\s*=.*$", "", d), has, NA)]
    gsub("\\bpopulation\\b", population, d, perl = TRUE)
  }
  args <- c(
    if (!is.null(by)) paste("by =", by),
    if (!identical(fn, "cards::ard_total_n") && !is.null(vars))
      paste("variables =", vars),
    own,
    if (!has("statistic")) .stat_arg(kind, stats),
    dflt,
    if (!is.na(r$args)) r$args,
    fmt_fun)
  first <- .data_arg(fn, has)
  if (!is.null(first)) first <- data
  sprintf("%s(%s)", fn, paste(c(first, args), collapse = ",\n    "))
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
#' @export
tfl_ard_spec <- function(x, statistics = NULL, methods = NULL) {
  old <- .set_catalogs(statistics, methods)
  on.exit(options(old), add = TRUE)
  # a column a definition does not have yet (written before it was added)
  # is blank
  # the analysis data in shape (none, written before it was added: empty)
  x$analysis_data <- .adata_sheet(x)
  for (s in intersect(names(.ard_spec_sheets), names(x))) {
    for (c in setdiff(.ard_spec_sheets[[s]], names(x[[s]]))) {
      x[[s]][[c]] <- rep(NA_character_, nrow(x[[s]]))
    }
  }
  x <- structure(x, class = "tfl_ard_spec")
  a <- x$analyses
  err <- character()
  need <- c("output_id", "analysis_id", "method")
  for (k in need) {
    if (any(is.na(a[[k]]))) err <- c(err, sprintf("`analyses$%s` is blank in row(s) %s", k,
                                                   paste(which(is.na(a[[k]])), collapse = ", ")))
  }
  dup <- duplicated(paste(a$output_id, a$analysis_id))
  if (any(dup)) err <- c(err, sprintf("output_id / analysis_id repeated: %s",
                                      paste(unique(paste(a$output_id, a$analysis_id)[dup]),
                                            collapse = ", ")))
  err <- c(err, .ard_parent_problems(a), .adata_problems(x, a))
  for (i in which(!is.na(a$post %||% rep(NA, nrow(a))))) {
    tag <- paste(a$output_id[i], a$analysis_id[i], sep = " / ")
    if (!is.na(a$parent[i] %||% NA)) {
      err <- c(err, sprintf(paste(
        "%s: `post` works on an analysis's ARD; inside %s it goes on the",
        "parent's row"), tag, a$parent[i]))
    }
    for (st in .split_post(a$post[i])) {
      p <- .post_problem(st)
      if (!is.null(p)) err <- c(err, sprintf("%s: `post` %s", tag, p))
    }
  }
  m <- stats::na.omit(a$method)
  known <- m %in% tfl_ard_methods()$method
  pkgfun <- grepl("^[A-Za-z.][A-Za-z0-9.]*::[A-Za-z._][A-Za-z0-9._]*$", m)
  own <- !known & !pkgfun & grepl("^[A-Za-z.][A-Za-z0-9._]*$", m)
  bad <- m[!known & !pkgfun & !own]
  if (length(bad)) err <- c(err, sprintf(
    "unknown method(s): %s (a keyword of tfl_ard_methods(), pkg::function, or a function the study key `source` loads)",
    paste(unique(bad), collapse = ", ")))
  if (any(own) && !length(.split_bar(.study_value(x, "source", NA)))) {
    warning(sprintf(paste(
      "method(s) %s: not a keyword of tfl_ard_methods(), so read as your",
      "own function(s) -- which the study key `source` should load"),
      paste(unique(m[own]), collapse = ", ")), call. = FALSE)
  }
  for (i in which(!is.na(a$args))) {
    p <- .args_problem(a$args[i])
    if (!is.null(p)) err <- c(err, sprintf(
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
    for (cn in twice) err <- c(err, sprintf(
      "%s / %s: `%s` is given twice, by the `%s` column and in `args`; write it in one place",
      a$output_id[i], a$analysis_id[i], col_arg[[cn]], cn))
  }
  for (col in c("strata", "denominator")) {
    for (i in which(!is.na(a[[col]] %||% rep(NA, nrow(a))))) {
      tag <- paste(a$output_id[i], a$analysis_id[i], sep = " / ")
      if (identical(a$method[i], "custom")) {
        err <- c(err, sprintf("%s: a `custom` analysis takes no `%s` (its code says it)",
                              tag, col))
        next
      }
      f <- .method_fun(a$method[i])
      if (!is.null(f) && !any(c(col, "...") %in% names(formals(f)))) {
        err <- c(err, sprintf("%s: %s takes no `%s`", tag, a$method[i], col))
      }
    }
  }
  den <- a$denominator %||% rep(NA, nrow(a))
  bad <- unique(stats::na.omit(den[!den %in% c(.den_words,
                                               x$populations$population_id,
                                               x$datasets$dataset,
                                               x$analysis_data$data_id)]))
  if (length(bad)) err <- c(err, sprintf(
    "denominator(s) %s: population, row, column, cell, a population, a dataset or an analysis data",
    paste(bad, collapse = ", ")))
  miss <- setdiff(stats::na.omit(c(a$dataset, x$populations$dataset)),
                  x$datasets$dataset)
  if (length(miss)) err <- c(err, sprintf("dataset(s) not in `datasets`: %s",
                                          paste(miss, collapse = ", ")))
  miss <- setdiff(stats::na.omit(a$population_id),
                  x$populations$population_id)
  if (length(miss)) err <- c(err, sprintf("population(s) not in `populations`: %s",
                                          paste(miss, collapse = ", ")))
  keys <- tfl_ard_methods()
  st <- tfl_ard_statistics()
  for (i in seq_len(nrow(a))) {
    k <- .method_key(a$method[i], keys)
    s <- .split_bar(a$statistics[i])
    if (!is.na(k) && keys$kind[k] == "continuous") {
      bad <- setdiff(s, st$statistic[st$kind == "continuous"])
      if (length(bad)) err <- c(err, sprintf(
        "%s / %s: no continuous statistic %s (see tfl_ard_statistics())",
        a$output_id[i], a$analysis_id[i], paste(bad, collapse = ", ")))
    }
    f <- .parse_formats(a$formats[i])
    bad <- names(f)[is.na(f) | !.fmt_ok(f)]
    if (length(bad)) err <- c(err, sprintf(
      "%s / %s: formats are statistic=format, the format xx.x, xx.x%%, a number of decimals or pvalue (%s)",
      a$output_id[i], a$analysis_id[i], paste(bad, collapse = ", ")))
  }
  cust <- a$method %in% "custom" & is.na(a$code)
  if (any(cust)) err <- c(err, "a `custom` analysis needs its `code`")
  if (length(err)) stop(paste(c("The ARD definition is not valid:", err),
                              collapse = "\n  "), call. = FALSE)
  x
}

# An analysis run inside another (`parent`): the parent is an analysis of
# the same report that runs others (cards::ard_stack(), ard_strata(),
# ard_pairwise()), and the rows inside take its data -- they say what is
# computed, not on what.
.ard_parent_problems <- function(a) {
  par <- a$parent %||% rep(NA_character_, nrow(a))
  err <- character()
  tag <- function(i) paste(a$output_id[i], a$analysis_id[i], sep = " / ")
  for (i in which(!is.na(par))) {
    p <- which(a$output_id == a$output_id[i] & a$analysis_id == par[i])
    if (!length(p)) {
      err <- c(err, sprintf("%s: parent %s is not an analysis of %s", tag(i),
                            par[i], a$output_id[i]))
      next
    }
    p <- p[1L]
    if (!a$method[p] %in% .ard_wrappers) {
      err <- c(err, sprintf(paste(
        "%s: its parent %s runs no other analyses -- a parent's method is",
        "one of %s"), tag(i), par[i], paste(.ard_wrappers, collapse = ", ")))
      next
    }
    if (!is.na(par[p])) {
      err <- c(err, sprintf("%s: its parent %s is inside another itself",
                            tag(i), par[i]))
    }
    own <- c(intersect("data", names(a)), "dataset", "population_id", "where",
             if (identical(a$method[p], "cards::ard_stack")) c("by", "strata"))
    set <- own[!is.na(unlist(a[i, own]))]
    if (length(set)) {
      err <- c(err, sprintf(
        "%s: %s %s the parent's (%s); leave blank", tag(i),
        paste0("`", set, "`", collapse = ", "),
        if (length(set) > 1L) "are" else "is", par[i]))
    }
    if (a$method[i] %in% c("subjects", "custom", .ard_wrappers)) {
      err <- c(err, sprintf("%s: a `%s` analysis cannot run inside %s",
                            tag(i), a$method[i], par[i]))
    }
    if (.names_data(a[i, ])) {
      err <- c(err, sprintf(paste(
        "%s: inside %s the data is the parent's, so its args may not name",
        "`data` or `population`"), tag(i), par[i]))
    }
  }
  for (p in which(a$method %in% .ard_wrappers)) {
    kids <- which(!is.na(par) & par == a$analysis_id[p] &
                    a$output_id == a$output_id[p])
    if (!length(kids)) next
    if (!identical(a$method[p], "cards::ard_stack") && length(kids) > 1L) {
      err <- c(err, sprintf("%s: %s runs one analysis; %d name it as parent",
                            tag(p), a$method[p], length(kids)))
    }
    if (identical(a$method[p], "cards::ard_pairwise") &&
        length(.split_bar(a$variables[p])) != 1L) {
      err <- c(err, sprintf(
        "%s: ard_pairwise() compares the pairs of ONE column's levels: `variables`",
        tag(p)))
    }
    if (identical(a$method[p], "cards::ard_stack")) {
      v <- unlist(lapply(kids, function(k) .split_bar(a$variables[k])))
      if (anyDuplicated(v)) {
        err <- c(err, sprintf(paste(
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
          err <- c(err, sprintf(paste(
            "%s: inside a stack, `statistics` cannot keep some of what %s",
            "gives; make it an analysis of its own"), tag(k), a$method[k]))
        }
      }
    }
  }
  err
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

# the program's lines for `post`: the ARD piped through each step
.post_lines <- function(post) {
  st <- .split_post(post)
  if (!length(st)) return(NULL)
  c("ard <- ard |>", paste0("  ", st, c(rep(" |>", length(st) - 1L), "")))
}

# A report's code lists as each variable's values in their order: the
# `codelists` sheet of a table definition, or a data frame with
# `output_id`, `variable`, `value` and `order`.  A code list is a report's:
# a row without `output_id` stops.  `vars`: only these variables (the ones
# the report's ARD reads); NULL, all the report's.
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
  lapply(split(as.character(d$value), factor(d$variable, levels = unique(d$variable))),
         unique)
}

# Every code list row names its report (no study-wide rows)
.codelists_check <- function(d) {
  if (is.null(d) || !nrow(d)) return(invisible(NULL))
  used <- !is.na(d$variable) | !is.na(d$value)
  if (!"output_id" %in% names(d)) {
    .ard_stop("The code lists have no `output_id` column.\n",
              "  A code list is a report's: give each row its report.")
  }
  blank <- which(used & (is.na(d$output_id) | !nzchar(trimws(d$output_id))))
  if (length(blank)) {
    .ard_stop(sprintf(paste0(
      "Row %d of the code lists (%s) has no `output_id`.\n",
      "  A code list is a report's: give the report, and copy the rows into ",
      "each report that uses them."),
      blank[1L], paste(d$variable[blank[1L]], d$value[blank[1L]], sep = " / ")))
  }
  invisible(NULL)
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

# `obj` made from `src` by `steps` (calls without their data: "subset(X)",
# "transform(A = B)", ".levels()") in one statement, the way a person
# writes it: no step, `obj <- src`; one step on a name, `obj <- subset(src,
# X)`; else a pipe, a step a line
.make_code <- function(obj, src, steps) {
  steps <- steps[!is.na(steps) & nzchar(steps)]
  if (!length(steps)) return(sprintf("%s <- %s", obj, src))
  if (length(steps) == 1L && grepl("^[A-Za-z.][A-Za-z0-9._]*$", src)) {
    s <- steps[[1L]]
    inner <- sub("^[^(]*\\((.*)\\)$", "\\1", s)
    fn <- sub("\\(.*$", "", s)
    return(sprintf("%s <- %s(%s%s)", obj, fn, src, if (nzchar(inner)) paste0(", ", inner) else ""))
  }
  if (length(steps) == 1L) return(sprintf("%s <- %s |> %s", obj, src, steps[[1L]]))
  paste0(obj, " <- ", src, " |>\n  ", paste(steps, collapse = " |>\n  "))
}

# the steps of a derive and of the code lists: transform(...), .levels().
# With code lists, a data made from another is made factors again: the
# columns it derives, and the values its rows no longer have that the
# lists do not list (a population's: not the screen failures' arm)
.derive_steps <- function(derive, levels = NULL) {
  d <- .split_bar(derive)
  c(if (length(d)) sprintf("transform(%s)", paste(d, collapse = ", ")),
    if (length(levels)) ".levels()")
}

# `nm <- expr`: a data made for an analysis (with code lists made factors
# again, as .derive_steps())
.made_line <- function(nm, expr, levels = NULL) {
  sprintf(if (length(levels)) "%s <- %s |> .levels()" else "%s <- %s", nm, expr)
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
                   encodeString(path, quote = "\"")))
  }
  f <- switch(ext, rds = "readRDS", xpt = "haven::read_xpt",
              sas7bdat = "haven::read_sas", csv = "utils::read.csv",
              parquet = "arrow::read_parquet",
              stop("No reader for .", ext, call. = FALSE))
  sprintf("%s(%s)", f, encodeString(path, quote = "\""))
}

.vars <- function(x) {
  v <- .split_bar(x)
  if (!length(v)) return(NULL)
  if (length(v) == 1L) v else sprintf("c(%s)", paste(v, collapse = ", "))
}

# The denominator column's words: the analysis set, or cards' own
.den_words <- c("population", "row", "column", "cell")

# The denominator column as R: `population` (the analysis set), "row" /
# "column" / "cell" (cards' percentages within a row, a column, of the
# whole), a population (its analysis set), or a dataset (its records of the
# analysis set's subjects)
.den_code <- function(den, x, pop, subj, population = "population") {
  if (is.null(den) || is.na(den)) return(NULL)
  if (den == "population") return(population)
  if (den %in% .den_words) return(encodeString(den, quote = "\""))
  if (den %in% x$populations$population_id) {
    return(paste0("pop_", .r_name(den)))
  }
  if (den %in% x$analysis_data$data_id) return(den)
  obj <- .r_name(den)
  if (is.null(pop)) obj else
    sprintf("subset(%s, %s %%in%% %s$%s)", obj, subj, population, subj)
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

.stat_arg <- function(method, stats) {
  s <- .split_bar(stats)
  if (!length(s)) return(NULL)
  q <- function(v) paste(encodeString(v, quote = "\""), collapse = ", ")
  switch(method,
    continuous = {
      # cards' own statistics and those computed by .tfl_stats, in the
      # order asked
      own <- !s %in% .computed_stats()
      run <- cumsum(c(TRUE, own[-1L] != own[-length(own)]))
      parts <- vapply(split(seq_along(s), run), function(i) {
        if (own[i[1L]]) sprintf("cards::continuous_summary_fns(c(%s))", q(s[i]))
        else sprintf(".tfl_stats[c(%s)]", q(s[i]))
      }, "")
      sprintf("statistic = ~ %s", if (length(parts) == 1L) parts else
        sprintf("c(%s)", paste(parts, collapse = ", ")))
    },
    categorical = ,
    missing = sprintf("statistic = ~ c(%s)", q(s)),
    NULL)
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

# A format of the spec as R that cards' fmt_fun takes: an integer is that
# many decimals (cards::label_round(d)), xx.x% a proportion as a percent,
# pvalue the program's .pvalue()
.fmt_r <- function(f) {
  if (identical(f, "pvalue")) return(".pvalue")
  d <- if (grepl("^[0-9]+$", f)) as.integer(f) else
    nchar(sub("^[^.]*[.]?", "", sub("%$", "", f)))
  if (endsWith(f, "%")) sprintf("cards::label_round(%d, scale = 100)", d) else
    sprintf("%dL", d)
}

# a name as R writes it: `AGE:sd` and the like in backquotes
.r_arg_name <- function(x) {
  ifelse(make.names(x) == x, x, encodeString(x, quote = "`"))
}

# `mean = 2L, sd = 3L`
.fmt_args <- function(f) {
  paste(sprintf("%s = %s", .r_arg_name(names(f)), vapply(f, .fmt_r, "")),
        collapse = ", ")
}

# The formats an analysis's call gives cards itself: `fmt_fun = ...`, each
# variable's statistics with the defaults (.fmt_default) and the analysis's
# own formats over them, a variable's own (`BMIBL:sd`) over those for every
# variable -- what .fmt() gives the same ARD after the call.  cards takes a
# variable's list whole (a later formula does not add to an earlier one),
# so a variable with formats of its own gets every one again.  NULL when
# the call cannot say them: a format of a variable the call's `variables`
# do not name (or name as R, not by name).
.fmt_fun_arg <- function(fmt, vars) {
  if (anyNA(fmt) || !all(.fmt_ok(fmt))) return(NULL)
  own <- grepl(":", names(fmt), fixed = TRUE)
  plain <- fmt[!own]
  spec <- fmt[own]
  if (!all(sub(":.*$", "", names(spec)) %in% vars)) return(NULL)
  # a format every variable has, the same (an analysis's own in a stack,
  # given to each of its variables): one for every variable
  for (st in unique(sub("^.*:", "", names(spec)))) {
    k <- paste0(vars, ":", st)
    if (all(k %in% names(spec)) && length(unique(spec[k])) == 1L) {
      plain[st] <- spec[[k[1L]]]
      spec <- spec[!names(spec) %in% k]
    }
  }
  sv <- sub(":.*$", "", names(spec))
  one <- function(m) if (length(m)) sprintf(".fmts(%s)", .fmt_args(m)) else
    ".fmt_default"
  # each variable's whole list (variables with the same: c(AGE, BMIBL))
  lists <- vapply(intersect(vars, sv), function(v) {
    m <- plain
    o <- spec[sv == v]
    m[sub("^.*:", "", names(o))] <- o
    one(m)
  }, "")
  parts <- c(paste("everything() ~", one(plain)),
             vapply(unique(lists), function(l) {
               v <- .r_arg_name(names(lists)[lists == l])
               sprintf("%s ~ %s", if (length(v) > 1L)
                 sprintf("c(%s)", paste(v, collapse = ", ")) else v, l)
             }, ""))
  sprintf("fmt_fun = %s", if (length(parts) == 1L) parts else
    sprintf("list(\n      %s)", paste(parts, collapse = ",\n      ")))
}

# The cards / cardx functions whose call takes fmt_fun (that of their
# data.frame method; cards 0.9.0, cardx 0.3.4), for when the package is not
# there to ask.  The rest -- cardx's tests, CIs and models, ard_stack(),
# ard_stack_hierarchical(), ard_total_n() -- get their formats after the
# call (.fmt()).
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

# the helpers every ARD program starts with: the computed statistics it
# uses, and stat_fmt
.ard_helpers <- function(used) {
  st <- tfl_ard_statistics()
  cst <- st[st$kind == "continuous" & !is.na(st$fun) & st$statistic %in% used, ]
  dflt <- st[!duplicated(st$statistic) & !is.na(st$fmt), ]
  dflt <- stats::setNames(dflt$fmt, dflt$statistic)
  fl <- sprintf("%s = %s", .r_arg_name(names(dflt)), vapply(dflt, .fmt_r, ""))
  fl <- vapply(split(fl, ceiling(seq_along(fl) / 5)), paste, "",
               collapse = ", ")
  c(if (nrow(cst)) c(
      "# statistics cards does not compute itself (company standards)",
      ".tfl_stats <- list(",
      paste0("  ", encodeString(cst$statistic, quote = "`"), " = ", cst$fun,
             c(rep(",", nrow(cst) - 1L), "")),
      ")", ""),
    "# stat_fmt: each statistic's format, as cards' fmt_fun takes it -- an",
    "# integer is that many decimals, label_round(1, scale = 100) a proportion",
    "# as a percent, .pvalue <0.001 or 3 decimals",
    ".pvalue <- function(x) ifelse(x < 0.001, \"<0.001\", sprintf(\"%.3f\", x))",
    ".fmt_default <- list(",
    paste0("  ", fl, c(rep(",", length(fl) - 1L), "")),
    ")",
    "# the defaults with an analysis's own formats: fmt_fun = everything() ~ .fmts(mean = 2L)",
    ".fmts <- function(...) utils::modifyList(.fmt_default, list(...))",
    "# the formats after the call, for what takes no fmt_fun (cardx, a study's",
    "# own functions, code, ard_stack()'s own rows): `fmt` by statistic or",
    "# variable:statistic over the defaults; the rows of the variables `skip`",
    "# have theirs already (fmt_fun inside ard_stack())",
    ".fmt <- function(ard, fmt = list(), skip = character()) {",
    "  # a method that gives several ARDs (cards::ard_pairwise(): one per",
    "  # pair of groups): one, each row keeping its ARD's name as `pairwise`",
    "  if (is.list(ard) && !is.data.frame(ard)) {",
    "    ard <- dplyr::bind_rows(ard, .id = \"pairwise\")",
    "  }",
    "  if (!inherits(ard, \"card\")) return(ard)",
    "  f <- utils::modifyList(.fmt_default, fmt)",
    "  f <- f[order(grepl(\":\", names(f), fixed = TRUE))]",
    "  for (k in names(f)) {",
    "    s <- sub(\"^.*:\", \"\", k)",
    "    v <- if (grepl(\":\", k, fixed = TRUE)) sub(\":.*$\", \"\", k)",
    "    rows <- ard$stat_name == s & (is.null(v) | ard$variable %in% v) &",
    "      !ard$variable %in% skip",
    "    if (!any(rows)) next",
    "    ard <- cards::update_ard_fmt_fun(",
    "      ard, variables = dplyr::all_of(unique(ard$variable[rows])),",
    "      stat_names = s, fmt_fun = f[[k]])",
    "  }",
    "  cards::apply_fmt_fun(ard)",
    "}",
    "# only the statistics asked for, of a method that gives more",
    ".keep <- function(ard, keep) {",
    "  # several ARDs (cards::ard_pairwise(): one per pair): each of them",
    "  if (is.list(ard) && !is.data.frame(ard)) return(lapply(ard, .keep, keep))",
    "  ard[ard$stat_name %in% keep, , drop = FALSE]",
    "}",
    "")
}

#' The R code that makes the study's ARD
#'
#' One cards / cardx call per analysis row, each result tagged with its
#' `output_id`, `analysis_id` and `population_id`, bound into one ARD and
#' saved to the study key `output` (default `output/ard/ard.rds`).  The code
#' runs from the study folder.
#'
#' The formats (the method's and the row's `formats`, over each statistic's
#' default in [tfl_ard_statistics()]) are in the cards call itself, its
#' `fmt_fun` argument, where a reader sees them next to the statistics:
#' `fmt_fun = everything() ~ .fmts(mean = 2L)`, a variable's own
#' (`BMIBL:sd=3`) as `BMIBL ~ .fmts(mean = 2L, sd = 3L)`.  An integer is
#' that many decimals, `xx.x%` is `label_round(1, scale = 100)` and
#' `pvalue` the program's `.pvalue()` (`<0.001` or 3 decimals);
#' `cards::apply_fmt_fun()` then fills `stat_fmt`.  What takes no `fmt_fun`
#' gets the same formats after the call, from `.fmt()`: cardx's tests, CIs
#' and models, `cards::ard_stack_hierarchical()`, a study's own function,
#' `custom` code, `ard_pairwise()`, `ard_stack()`'s own rows (the by counts,
#' the total N), an analysis whose `args` gives `fmt_fun` or whose `post`
#' changes the ARD, a variable's own format for `ard_hierarchical()` or
#' `ard_tabulate_rows()`.  `stat_fmt` is the same either way.
#'
#' `part` gives a piece of it instead, for a program layout of one's own
#' (tflplanner writes one program per output that sources a shared setup):
#' `"setup"` is what every piece starts with -- `library(cards)`, the
#' tagging helper, every computed statistic of the catalog and the
#' `stat_fmt` helpers (`.fmt_default`, `.fmts()`, `.pvalue()`, `.fmt()`);
#' `"body"` is the analyses of `output_id`, ending in
#' `ard` -- without a header or `saveRDS()`.
#'
#' @param spec An [tfl_ard_spec()] (or the path of one).
#' @param output_id Only these outputs' analyses; `NULL` for all.
#' @param save `FALSE` leaves out the final `saveRDS()`.
#' @param part `"all"` (the whole program), `"setup"` or `"body"`.
#' @param dir The study folder: the fingerprints saved with the ARD
#'   ([tfl_ard_spec_hash()]) read the study's own function files from it.
#' @param codelists The reports' code lists: a table definition (its
#'   `codelists` sheet) or a data frame with `output_id`, `variable`,
#'   `value` and `order`.  A code list is a report's: every row names its
#'   report (a blank `output_id` stops).  A report's rows of the variables
#'   its analyses read (`by`, `strata`, `variables`, and the names in
#'   `args`, `code` and `post`) count: each such column of the data the
#'   report reads, and of the populations and analysis data that derive it,
#'   becomes a factor in that order before any analysis (a value the list
#'   does not have comes after, alphabetically), so the ARD keeps the order
#'   and counts a level no record has (`n = 0`).  In the study's program,
#'   when a report has code lists, each report's part reads its data again
#'   with its own.  `NULL` (default): the data as read.
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
  if (part == "setup") {
    st <- tfl_ard_statistics("continuous")
    return(.ard_common_lines(st$statistic[!is.na(st$fun)], x))
  }
  # each report's code lists, of the variables its analyses read
  lv <- .codelist_levels_by(codelists, a)
  if (part == "body") return(.ard_body_lines(x, a, lv))
  out <- .study_value(x, "output", "output/ard/ard.rds")
  code <- c(
    "# The study's ARD, made from its ARD definition.  Run from the study folder.",
    paste0("# Generated by tflspec ", utils::packageVersion("tflspec"),
           ", ", format(Sys.Date())),
    "",
    .ard_common_lines(unlist(lapply(a$statistics, .split_bar)), x),
    .ard_body_lines(x, a, lv))
  if (save) {
    ids <- unique(a$output_id)
    hashes <- vapply(ids, function(id) .ard_output_hash(x, id, dir, codelists), "")
    q <- function(v) paste(encodeString(v, quote = "\""), collapse = ", ")
    code <- c(code,
              sprintf("dir.create(dirname(%s), recursive = TRUE, showWarnings = FALSE)",
                      encodeString(out, quote = "\"")),
              sprintf("saveRDS(ard, %s)", encodeString(out, quote = "\"")),
              "# what was built, and from which definition (tfl_ard_spec_hash())",
              "status <- data.frame(",
              sprintf("  output_id = c(%s),", q(ids)),
              sprintf("  definition = c(%s),", q(hashes)),
              "  built = format(Sys.time(), \"%Y-%m-%d %H:%M:%S\"),",
              "  stringsAsFactors = FALSE)",
              "status$rows <- as.integer(table(factor(ard$output_id, levels = status$output_id)))",
              sprintf("utils::write.csv(status, file.path(dirname(%s), \"ard_status.csv\"), row.names = FALSE)",
                      encodeString(out, quote = "\"")))
  }
  c(code, "")
}


# library(cards), the study's own functions (its key `source`), .tag() and
# the helpers (the computed statistics `used`, stat_fmt)
.ard_common_lines <- function(used, x = NULL) {
  src <- .split_bar(.study_value(x, "source", NA))
  c("library(cards)",
    if (length(src)) c(
      "# the study's own analysis functions (study key `source`)",
      sprintf("source(%s)", encodeString(src, quote = "\""))),
    "",
    "# the ids in front; a cards ARD stays one (class card), so the study ARD",
    "# is one too and cards' own tools (as_nested_list(), compare_ard()) take it",
    ".tag <- function(ard, output_id, analysis_id, population_id) {",
    "  # several analyses run together (cards::ard_stack()): each variable's",
    "  # rows are its own analysis's, the rest (the by counts, the total N)",
    "  # the stack's",
    "  if (length(analysis_id) > 1L) {",
    "    id <- unname(analysis_id[as.character(ard$variable)])",
    "    id[is.na(id)] <- analysis_id[[\".other\"]]",
    "    analysis_id <- id",
    "  }",
    "  if (inherits(ard, \"card\")) {",
    "    return(dplyr::mutate(ard, output_id = output_id,",
    "                         analysis_id = analysis_id,",
    "                         population_id = population_id, .before = 1L))",
    "  }",
    "  ard <- as.data.frame(ard)",
    "  cbind(output_id = output_id, analysis_id = analysis_id,",
    "        population_id = population_id, ard, stringsAsFactors = FALSE)",
    "}",
    "",
    .ard_helpers(used))
}

# `.codelists <- list(...)`: the code lists' values of each variable
.codelists_line <- function(levels) {
  sprintf(".codelists <- list(\n  %s)", paste(vapply(names(levels), function(v)
    sprintf("%s = c(%s)", encodeString(v, quote = "`"),
            paste(encodeString(levels[[v]], quote = "\""), collapse = ", ")),
    ""), collapse = ",\n  "))
}

.levels_fn_lines <- function() {
  c(".levels <- function(d) {",
    "  for (v in intersect(names(.codelists), names(d))) {",
    "    x <- d[[v]]",
    "    if (!is.character(x) && !is.factor(x)) next",
    "    seen <- sort(unique(as.character(x[!is.na(x)])))",
    "    d[[v]] <- factor(as.character(x), levels = unique(c(.codelists[[v]], seen)))",
    "  }",
    "  d",
    "}")
}

# The datasets and populations analyses `a` (and analysis data `nad`)
# read: their lines, with `.levels()` when there are code lists
.ard_data_lines <- function(x, a, nad, levels = NULL) {
  pops <- unique(stats::na.omit(c(a$population_id,
                                  a$denominator[a$denominator %in%
                                                  x$populations$population_id],
                                  nad$population_id)))
  den_ds <- a$denominator[a$denominator %in% x$datasets$dataset &
                            !a$denominator %in% .den_words]
  used_ds <- unique(stats::na.omit(c(a$dataset, den_ds,
                                     nad$from[nad$from %in% x$datasets$dataset],
                                     x$populations$dataset[
    x$populations$population_id %in% pops])))
  code <- character()
  for (ds in used_ds) {
    r <- x$datasets[x$datasets$dataset == ds, ]
    obj <- .r_name(ds)
    d <- .split_bar(r$derive[1L])
    code <- c(code, .make_code(obj, .reader(r$path[1L]), c(
      if (length(d)) sprintf("transform(%s)", paste(d, collapse = ", ")),
      if (length(levels)) ".levels()")))
  }
  pop_code <- character()
  for (pid in pops) {
    r <- x$populations[x$populations$population_id == pid, ]
    obj <- paste0("pop_", .r_name(pid))
    src <- .r_name(r$dataset[1L])
    pop_code <- c(pop_code, .make_code(obj, src, c(
      if (!is.na(r$where[1L])) sprintf("subset(%s)", r$where[1L]),
      .derive_steps(r$derive[1L], levels))))
  }
  list(data = code, pops = pop_code, used_ds = used_ds, pops_id = pops)
}

# the analyses `a`: their data, their analysis sets, one call each, bound
# into `ard`.  `levels`: each report's code lists (.codelist_levels_by()).
.ard_body_lines <- function(x, a, levels = NULL) {
  subj <- .study_value(x, "id", "USUBJID")
  # an analysis on an analysis data is of its population: the data is its
  # own report's (analysis_data is by report); the named data each report
  # reads (and its denominator), with the rows they are made from
  ad <- .adata_sheet(x)
  x$analysis_data <- ad
  dcol <- .data_col(a)
  outs <- unique(a$output_id)
  for (i in which(!is.na(dcol))) {
    a$population_id[i] <- .adata_pop(.adata_of(ad, a$output_id[i]), dcol[i])
  }
  named <- lapply(stats::setNames(outs, outs), function(o) {
    ad_o <- .adata_of(ad, o)
    .adata_used(ad_o, a[a$output_id == o, , drop = FALSE])
  })
  nad_of <- function(o) {
    ad_o <- .adata_of(ad, o)
    ad_o[ad_o$data_id %in% named[[o]], , drop = FALSE]
  }
  nad <- do.call(rbind, c(list(ad[0L, , drop = FALSE]), lapply(outs, nad_of)))
  several <- length(outs) > 1L
  # the study's program with code lists: each report's part reads its data
  # with its own (a report's factors are not the next report's)
  own_data <- several && length(levels) > 0L
  lv_of <- function(o) if (is.null(levels)) NULL else levels[[o]]
  code <- "# ---- data"
  lv1 <- if (!several) lv_of(outs[1L])
  if (length(lv1)) {
    code <- c(code,
      "# the report's code lists: each listed column a factor in their order,",
      "# so the ARD keeps the order and counts a level no record has (0)",
      .codelists_line(lv1), .levels_fn_lines())
  } else if (own_data) {
    code <- c(code,
      "# a report's code lists (.codelists, in its part): each listed column a",
      "# factor in their order, so the ARD keeps the order and counts a level",
      "# no record has (0)",
      .levels_fn_lines())
  }
  if (own_data) {
    used_ds <- character()
    pops <- character()
  } else {
    dl <- .ard_data_lines(x, a, nad, lv1)
    used_ds <- dl$used_ds
    pops <- dl$pops_id
    code <- c(code, dl$data, "", "# ---- populations", dl$pops)
  }
  keys <- tfl_ard_methods()
  par <- a$parent %||% rep(NA_character_, nrow(a))
  # One report's analysis data and analyses.  Several reports: one part a
  # report, each making its own analysis data before its analyses (a name
  # the next report uses too is simply made again), after "ards <- list()"
  # once; the ards index is the row's in the study's table.
  if (several) code <- c(code, "", "ards <- list()")
  for (o in outs) {
    xo <- x
    ad_o <- .adata_of(ad, o)
    xo$analysis_data <- ad_o
    rows <- which(a$output_id == o)
    lv <- if (several) lv_of(o) else lv1
    part <- NULL
    if (own_data) {
      dl <- .ard_data_lines(x, a[rows, , drop = FALSE], nad_of(o), lv)
      used_ds <- dl$used_ds
      pops <- dl$pops_id
      part <- c(if (length(lv)) .codelists_line(lv), dl$data, dl$pops)
    }
    # each analysis's data: the dataset, restricted to the population's
    # subjects (or the population itself when it is that dataset), and to
    # the analysis's own subset -- made once, under a name, for every
    # analysis that reads it
    taken <- c(.r_name(used_ds), paste0("pop_", .r_name(pops)), ad_o$data_id)
    data_of <- character()
    made <- .adata_lines(xo, named[[o]], subj, lv)
    data_name <- vapply(rows, function(i) {
      r <- a[i, ]
      # on an analysis data: it, or its records the analysis's condition keeps
      if (!is.na(dcol[i])) {
        if (is.na(r$where)) return(dcol[i])
        expr <- sprintf("subset(%s, %s)", dcol[i], r$where)
        if (!is.na(data_of[expr])) return(data_of[[expr]])
        nm <- make.unique(c(taken, dcol[i]), sep = "_")[length(taken) + 1L]
        taken <<- c(taken, nm)
        data_of[[expr]] <<- nm
        made <<- c(made, .made_line(nm, expr, lv))
        return(nm)
      }
      pid <- r$population_id
      pop <- if (!is.na(pid)) paste0("pop_", .r_name(pid))
      ds <- if (!is.na(r$dataset)) .r_name(r$dataset) else pop
      pop_ds <- if (!is.na(pid)) x$populations$dataset[
        x$populations$population_id == pid][1L]
      # one subset() for the population's subjects and the analysis's own
      # condition (subset() drops the rows a condition leaves NA either way)
      whr <- if (!is.na(r$where)) r$where
      expr <- if (is.null(pop) || identical(r$dataset, pop_ds) ||
                  is.na(r$dataset)) {
        src <- if (is.null(pop)) ds else pop
        if (is.null(whr)) src else sprintf("subset(%s, %s)", src, whr)
      } else {
        cond <- sprintf("%s %%in%% %s$%s", subj, pop, subj)
        if (!is.null(whr)) cond <- .cond_and(cond, whr)
        sprintf("subset(%s, %s)", ds, cond)
      }
      # no dataset and no population: no data (as before, `data` is NULL)
      if (is.null(expr)) return("NULL")
      if (expr %in% taken) return(expr)            # a dataset or a population
      if (!is.na(data_of[expr])) return(data_of[[expr]])
      base <- paste(c(if (!is.na(r$dataset)) .r_name(r$dataset) else
        .r_name(pop_ds), if (!is.na(pid)) .r_name(pid)), collapse = "_")
      nm <- make.unique(c(taken, base), sep = "_")[length(taken) + 1L]
      taken <<- c(taken, nm)
      data_of[[expr]] <<- nm
      made <<- c(made, .made_line(nm, expr, lv))
      nm
    }, "")
    names(data_name) <- as.character(rows)
    if (several) {
      code <- c(code, "", sprintf("# ---- %s", o), part, made)
    } else {
      if (length(made)) code <- c(code, "", "# ---- the analysis data", made)
      code <- c(code, "", "# ---- analyses", "ards <- list()")
    }
    for (i in rows) {
      r <- a[i, ]
      # an analysis run inside another is written with it
      if (!is.na(par[i])) next
      kids <- which(!is.na(par) & par == r$analysis_id &
                      a$output_id == r$output_id)
      if (length(kids)) {
        code <- c(code, .ard_wrapper_lines(xo, r, a[kids, , drop = FALSE],
                                           keys, subj, data_name[[as.character(i)]], i))
        next
      }
      pid <- r$population_id
      pop <- if (!is.na(pid)) paste0("pop_", .r_name(pid))
      pop_name <- if (is.null(pop)) "NULL" else pop
      given <- c(.args_given(r$args), if (!is.na(r$strata)) "strata",
                 if (!is.na(r$denominator)) "denominator")
      has <- function(arg) arg %in% given
      k <- .method_key(r$method, keys)
      kind <- if (is.na(k)) "" else keys$kind[k]
      # R the row writes itself (args, code) may name `data` and
      # `population`, and a subject flag changes its population: those bind
      # the two names in a local scope; any other analysis is one call on
      # the data by its name
      bind <- identical(keys$call[k], "(subjects)") ||
        identical(keys$call[k], "(code)") || .names_data(r)
      den <- .den_code(r$denominator, xo, pop, subj,
                       population = if (bind) "population" else pop_name)
      fmt <- c(if (!is.na(k)) .parse_formats(keys$formats[k]),
               .parse_formats(r$formats))
      fmt <- fmt[!duplicated(names(fmt), fromLast = TRUE)]
      # the formats in the call (fmt_fun), when the function takes them and
      # nothing changes the ARD after it; else after it (.fmt())
      fmt_fun <- .call_fmt_fun(r, keys, given, fmt)
      body <- if (bind) .analysis_body(r, keys, subj, has, den,
                                       fmt_fun = fmt_fun) else
        .analysis_body(r, keys, subj, has, den, data = data_name[[as.character(i)]],
                       population = pop_name, fmt_fun = fmt_fun)
      core <- strsplit(body, "\n", fixed = TRUE)[[1L]]
      if (bind) {
        core <- c("local({",
                  paste0("  data <- ", data_name[[as.character(i)]]),
                  paste0("  population <- ", pop_name),
                  paste0("  ", core),
                  "})")
      }
      keep <- if (!kind %in% c("continuous", "categorical", "missing") &&
                  !identical(keys$call[k], "(subjects)"))
        .split_bar(r$statistics)
      core[1L] <- paste("ard <-", core[1L])
      code <- c(code,
                sprintf("# %s / %s%s", r$output_id, r$analysis_id,
                        if (!is.na(r$label)) paste(":", r$label) else ""),
                core, .post_lines(r$post),
                if (length(keep)) sprintf("ard <- .keep(ard, c(%s))",
                                          paste(encodeString(keep, quote = "\""),
                                                collapse = ", ")),
                sprintf("ards[[%d]] <- .tag(%s, %s, %s, %s)", i,
                        .fmt_call(fmt, !is.null(fmt_fun)),
                        encodeString(r$output_id, quote = "\""),
                        encodeString(r$analysis_id, quote = "\""),
                        if (is.na(pid)) "NA_character_" else
                          encodeString(pid, quote = "\"")))
    }
  }
  # dplyr::bind_rows(), not cards::bind_ard(): bind_ard() does not count
  # output_id / analysis_id as part of a row's key, so the same statistic in
  # two outputs (AGE's mean in two analysis sets) would stop it, or, with the
  # same value, lose one output's rows.  The class card is kept by .tag().
  code <- c(code, "", "ard <- do.call(dplyr::bind_rows, ards)")
  # (one report's program: an analysis set that is only its adsl_<set> is
  # made once, under that name; the study's keeps pop_* for every report)
  if (several) code else .pop_as_adata(code, ad$data_id)
}

# An analysis data that is an analysis set and nothing else (`adsl_saf <-
# pop_saf`), when the set is used for nothing else in the program: made
# once, under the data's name, where the analysis data are
# (`adsl_saf <- adsl |> subset(SAFFL == "Y") |> ...`), and no pop_saf.
.pop_as_adata <- function(code, data_ids) {
  rx <- "^([A-Za-z.][A-Za-z0-9._]*) <- (pop_[A-Za-z0-9._]+)$"
  for (k in rev(grep(rx, code))) {
    d <- sub(rx, "\\1", code[k])
    p <- sub(rx, "\\2", code[k])
    if (!d %in% data_ids) next
    def <- which(startsWith(code, paste0(p, " <- ")))
    if (length(def) != 1L) next
    word <- paste0("(^|[^A-Za-z0-9._])", gsub(".", "\\.", p, fixed = TRUE), "($|[^A-Za-z0-9._])")
    if (any(grepl(word, code[-c(k, def)]))) next
    code[k] <- paste0(d, substring(code[def], nchar(p) + 1L))
    code <- code[-def]
  }
  # a populations section with nothing left in it goes
  h <- which(code == "# ---- populations")
  if (length(h) && (h == length(code) || !nzchar(code[h + 1L]))) {
    code <- code[-c(if (h > 1L && !nzchar(code[h - 1L])) h - 1L, h)]
  }
  code
}

# An analysis's `fmt_fun = ...`, or NULL when its formats go after the call:
# its function takes no fmt_fun (cardx's tests and models, a study's own,
# code), its `args` gives one itself, or `post` changes the ARD after it.
.call_fmt_fun <- function(r, keys, given, fmt) {
  k <- .method_key(r$method, keys)
  fn <- if (is.na(k)) r$method else keys$call[k]
  if (!.takes_fmt_fun(fn) || any(c("fmt_fun", "fmt_fn") %in% given) ||
      !is.na(r$post)) return(NULL)
  vars <- if (identical(fn, "(subjects)")) {
    if (length(.split_bar(r$variables))) .split_bar(r$variables)[1L] else
      make.names(r$analysis_id)
  } else if (fn %in% .fmt_fun_every) character() else .split_bar(r$variables)
  .fmt_fun_arg(fmt, vars)
}

# Functions whose fmt_fun is for every variable only: ard_hierarchical()
# formats a column of its own (a variable's formula finds no AEDECOD), and
# ard_tabulate_rows() has no variables.  A variable's own format: after.
.fmt_fun_every <- c("cards::ard_hierarchical", "cards::ard_hierarchical_count",
                    "cards::ard_tabulate_rows")

# What the tagged ARD is: formatted in the call, its stat_fmt filled
# (cards::apply_fmt_fun()); or formatted after it (.fmt(), the defaults and
# `fmt`)
.fmt_call <- function(fmt, in_call, skip = NULL) {
  if (in_call && is.null(skip)) return("cards::apply_fmt_fun(ard)")
  sprintf(".fmt(%s)", paste(c("ard",
    if (length(fmt)) sprintf("list(%s)", .fmt_args(fmt)),
    if (length(skip)) sprintf("skip = c(%s)", paste(encodeString(skip, quote = "\""),
                                                     collapse = ", "))),
    collapse = ", "))
}

# The functions that run other analyses: an analysis row whose `parent`
# names a row with one of these as its method is run inside it.
.ard_wrappers <- c("cards::ard_stack", "cards::ard_strata",
                   "cards::ard_pairwise")

# One analysis that runs others (its `parent` rows), as the call a person
# writes: cards::ard_stack(data, .by = ARM, ard_summary(...), ...).
# The rows inside take the parent's data, analysis set and condition; in a
# stack, its by too.  Each variable's rows are tagged with the analysis that
# computed them, the stack's own (the by counts, the total N) with the
# parent's id; inside ard_strata() / ard_pairwise() the one analysis's id.
.ard_wrapper_lines <- function(x, r, kids, keys, subj, data, i) {
  pid <- r$population_id
  pop <- if (!is.na(pid)) paste0("pop_", .r_name(pid))
  pop_name <- if (is.null(pop)) "NULL" else pop
  stack <- identical(r$method, "cards::ard_stack")
  pairwise <- identical(r$method, "cards::ard_pairwise")
  # formats: a row's own, for its own variables
  fmt <- character()
  for (j in seq_len(nrow(kids))) {
    kr <- kids[j, ]
    k <- .method_key(kr$method, keys)
    f <- c(if (!is.na(k)) .parse_formats(keys$formats[k]),
           .parse_formats(kr$formats))
    if (stack && any(!grepl(":", names(f), fixed = TRUE))) {
      plain <- !grepl(":", names(f), fixed = TRUE)
      vv <- .split_bar(kr$variables)
      f <- c(f[!plain], unlist(lapply(vv, function(v)
        stats::setNames(f[plain], paste0(v, ":", names(f)[plain])))))
    }
    fmt <- c(fmt, f)
  }
  fmt <- c(.parse_formats(r$formats), fmt)
  fmt <- fmt[!duplicated(names(fmt), fromLast = TRUE)]
  given_of <- function(kr) c(.args_given(kr$args), if (!is.na(kr$strata)) "strata",
                             if (!is.na(kr$denominator)) "denominator")
  # each analysis's formats in its call (fmt_fun), when every one takes
  # them; not inside ard_pairwise() (a list of ARDs), nor when `post`
  # changes the ARD after
  fmt_funs <- if (!pairwise && is.na(r$post)) lapply(seq_len(nrow(kids)), function(j) {
    kr <- kids[j, ]
    kr$post <- NA
    f <- fmt
    # in a stack, a variable's own formats go to its analysis's call (the
    # rest, the stack's own rows, after it)
    if (stack) f <- f[!grepl(":", names(f), fixed = TRUE) |
                        sub(":.*$", "", names(f)) %in% .split_bar(kr$variables)]
    .call_fmt_fun(kr, keys, given_of(kr), f)
  })
  in_call <- length(fmt_funs) > 0L && !any(vapply(fmt_funs, is.null, NA))
  child <- function(kr, fmt_fun) {
    given <- given_of(kr)
    has <- function(arg) arg %in% given
    den <- .den_code(kr$denominator, x, pop, subj, population = pop_name)
    if (stack) kr$by <- NA
    body <- .analysis_body(kr, keys, subj, has, den,
                           data = if (stack) NULL else ".x",
                           population = pop_name,
                           fmt_fun = if (in_call) fmt_fun)
    gsub("\n", "\n  ", body, fixed = TRUE)
  }
  bodies <- vapply(seq_len(nrow(kids)), function(j)
    child(kids[j, ], fmt_funs[[j]]), "")
  args <- c(data,
            if (stack && !is.na(r$by)) paste(".by =", .vars(r$by)),
            if (!stack && identical(r$method, "cards::ard_strata")) c(
              if (!is.na(r$by)) paste(".by =", .vars(r$by)),
              if (!is.na(r$strata)) paste(".strata =", .vars(r$strata))),
            if (identical(r$method, "cards::ard_pairwise"))
              paste("variable =", .vars(r$variables)),
            if (stack) bodies else paste(".f = ~", bodies),
            if (!is.na(r$args)) r$args)
  call <- sprintf("%s(%s)", r$method, paste(args, collapse = ",\n    "))
  core <- strsplit(call, "\n", fixed = TRUE)[[1L]]
  core[1L] <- paste("ard <-", core[1L])
  # ard_stack()'s own rows (the by counts, the total N) take no fmt_fun:
  # theirs after it, the analyses' variables skipped (formatted in their
  # calls).  With .missing, the stack's missing rows are of those variables
  # too: all after it, as in the calls.
  skip <- if (in_call && stack) {
    if (".missing" %in% .args_given(r$args)) character() else
      unique(unlist(lapply(kids$variables, .split_bar)))
  }
  post_fmt <- if (length(skip)) fmt[!grepl(":", names(fmt), fixed = TRUE) |
                                      !sub(":.*$", "", names(fmt)) %in% skip] else fmt
  keep <- if (!stack) {
    k <- .method_key(kids$method[1L], keys)
    kind <- if (is.na(k)) "" else keys$kind[k]
    if (!kind %in% c("continuous", "categorical", "missing"))
      .split_bar(kids$statistics[1L])
  }
  ids <- if (stack) {
    v <- unlist(lapply(seq_len(nrow(kids)), function(j)
      stats::setNames(rep(kids$analysis_id[j],
                          length(.split_bar(kids$variables[j]))),
                      .split_bar(kids$variables[j]))))
    v <- c(v, .other = r$analysis_id)
    sprintf("c(%s)", paste(sprintf("%s = %s", encodeString(names(v), quote = "`"),
                                   encodeString(v, quote = "\"")),
                           collapse = ", "))
  } else encodeString(kids$analysis_id[1L], quote = "\"")
  c(sprintf("# %s / %s%s: %s", r$output_id, r$analysis_id,
            if (!is.na(r$label)) paste(":", r$label) else "",
            paste(kids$analysis_id, collapse = ", ")),
    core, .post_lines(r$post),
    if (length(keep)) sprintf("ard <- .keep(ard, c(%s))",
                              paste(encodeString(keep, quote = "\""),
                                    collapse = ", ")),
    sprintf("ards[[%d]] <- .tag(%s, %s, %s, %s)", i,
            .fmt_call(post_fmt, in_call && !stack,
                      skip = if (stack && in_call) skip),
            encodeString(r$output_id, quote = "\""), ids,
            if (is.na(pid)) "NA_character_" else
              encodeString(pid, quote = "\"")))
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
