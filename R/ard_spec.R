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
#   analyses     output_id, analysis_id, method, dataset, population_id,
#                where, by, variables, statistics, formats, args, code
#
# The concepts are those of CDISC ARS (analysis set, data subset, grouping,
# method), so an tfl_ard_spec can later be written as ARS metadata.

.ard_spec_sheets <- list(
  study = c("key", "value"),
  datasets = c("dataset", "level", "path", "derive"),
  populations = c("population_id", "dataset", "where", "derive"),
  analyses = c("output_id", "analysis_id", "label", "method", "dataset",
               "population_id", "where", "by", "strata", "variables",
               "statistics", "denominator", "formats", "args", "code",
               "purpose", "reason"))

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

# The call an analysis row stands for, with `data` and `population` bound.
.analysis_body <- function(r, keys, subj, has, den = NULL, data = "data",
                           population = "population") {
  m <- r$method
  k <- match(m, keys$method)
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
      sprintf("cards::ard_dichotomous(population%s, variables = %s, value = list(%s = TRUE)%s%s%s)",
              if (!is.null(by)) paste0(", by = ", by) else "", flag, flag,
              if (!is.null(st)) paste0(", ", st) else "",
              if (length(own)) paste0(", ", paste(own, collapse = ", ")) else "",
              if (!is.na(r$args)) paste0(", ", r$args) else "")))
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
    if (!is.na(r$args)) r$args)
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
#' Writes the four sheets (`study`, `datasets`, `populations`, `analyses`)
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

# the four sheets of an ARD definition, in shape
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
#' @return An `tfl_ard_spec`: a list of the four sheets.
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
    k <- match(a$method[i], km$method)
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
                                               x$datasets$dataset)]))
  if (length(bad)) err <- c(err, sprintf(
    "denominator(s) %s: population, row, column, cell, a population or a dataset",
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
    k <- match(a$method[i], keys$method)
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

.study_value <- function(x, key, default) {
  v <- x$study$value[match(key, x$study$key)]
  if (length(v) && !is.na(v)) v else default
}

.r_name <- function(x) make.names(tolower(x))

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
  k <- match(m, keys$method)
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

.fmt_vector <- function(f) {
  if (!length(f)) return("character()")
  sprintf("c(%s)", paste(sprintf("%s = %s", encodeString(names(f), quote = "`"),
                                 encodeString(f, quote = "\"")),
                         collapse = ", "))
}

# the helpers every ARD program starts with: the computed statistics it
# uses, and stat_fmt
.ard_helpers <- function(used) {
  st <- tfl_ard_statistics()
  cst <- st[st$kind == "continuous" & !is.na(st$fun) & st$statistic %in% used, ]
  dflt <- st[!duplicated(st$statistic) & !is.na(st$fmt), ]
  dflt <- stats::setNames(dflt$fmt, dflt$statistic)
  fl <- sprintf("%s = %s", encodeString(names(dflt), quote = "`"),
                encodeString(dflt, quote = "\""))
  fl <- vapply(split(fl, ceiling(seq_along(fl) / 5)), paste, "",
               collapse = ", ")
  c(if (nrow(cst)) c(
      "# statistics cards does not compute itself (company standards)",
      ".tfl_stats <- list(",
      paste0("  ", encodeString(cst$statistic, quote = "`"), " = ", cst$fun,
             c(rep(",", nrow(cst) - 1L), "")),
      ")", ""),
    "# stat_fmt: each statistic formatted -- xx.x = 1 decimal, xx.x% = a",
    "# proportion as a percent, pvalue = <0.001 or 3 decimals",
    ".fmt_default <- c(",
    paste0("  ", fl, c(rep(",", length(fl) - 1L), "")),
    ")",
    ".fmt <- function(ard, fmt = character()) {",
    "  # a method that gives several ARDs (cards::ard_pairwise(): one per",
    "  # pair of groups): one, each row keeping its ARD's name as `pairwise`",
    "  if (is.list(ard) && !is.data.frame(ard)) {",
    "    ard <- dplyr::bind_rows(ard, .id = \"pairwise\")",
    "  }",
    "  if (!inherits(ard, \"card\")) return(ard)",
    "  f <- .fmt_default",
    "  f[names(fmt)] <- fmt",
    "  f <- f[order(grepl(\":\", names(f), fixed = TRUE))]",
    "  for (k in names(f)) {",
    "    s <- sub(\"^.*:\", \"\", k)",
    "    v <- if (grepl(\":\", k, fixed = TRUE)) sub(\":.*$\", \"\", k)",
    "    rows <- ard$stat_name == s & (is.null(v) | ard$variable %in% v)",
    "    if (!any(rows)) next",
    "    fun <- if (f[[k]] == \"pvalue\") {",
    "      function(x) ifelse(x < 0.001, \"<0.001\", sprintf(\"%.3f\", x))",
    "    } else {",
    "      # the decimals (the x after the point, or the number); % scales by 100",
    "      d <- if (grepl(\"^[0-9]+$\", f[[k]])) as.integer(f[[k]]) else",
    "        nchar(sub(\"^[^.]*[.]?\", \"\", sub(\"%$\", \"\", f[[k]])))",
    "      sc <- if (endsWith(f[[k]], \"%\")) 100 else 1",
    "      local({ d <- d; sc <- sc; cards::label_round(d, scale = sc) })",
    "    }",
    "    ard <- cards::update_ard_fmt_fun(",
    "      ard, variables = dplyr::all_of(unique(ard$variable[rows])),",
    "      stat_names = s, fmt_fun = fun)",
    "  }",
    "  cards::apply_fmt_fun(ard)",
    "}",
    "# only the statistics asked for, of a method that gives more",
    ".keep <- function(ard, keep) ard[ard$stat_name %in% keep, , drop = FALSE]",
    "")
}

#' The R code that makes the study's ARD
#'
#' One cards / cardx call per analysis row, each result tagged with its
#' `output_id`, `analysis_id` and `population_id`, bound into one ARD and
#' saved to the study key `output` (default `output/ard/ard.rds`).  The code
#' runs from the study folder.
#'
#' `part` gives a piece of it instead, for a program layout of one's own
#' (tflplanner writes one program per output that sources a shared setup):
#' `"setup"` is what every piece starts with -- `library(cards)`, the
#' tagging helper, every computed statistic of the catalog and the
#' `stat_fmt` helpers; `"body"` is the analyses of `output_id`, ending in
#' `ard` -- without a header or `saveRDS()`.
#'
#' @param spec An [tfl_ard_spec()] (or the path of one).
#' @param output_id Only these outputs' analyses; `NULL` for all.
#' @param save `FALSE` leaves out the final `saveRDS()`.
#' @param part `"all"` (the whole program), `"setup"` or `"body"`.
#' @param dir The study folder: the fingerprints saved with the ARD
#'   ([tfl_ard_spec_hash()]) read the study's own function files from it.
#' @inheritParams tfl_write_ard_spec
#' @return The code, one element per line.
#' @export
tfl_ard_code <- function(spec, output_id = NULL, save = TRUE,
                          part = c("all", "setup", "body"),
                          statistics = NULL, methods = NULL, dir = ".") {
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
  if (part == "body") return(.ard_body_lines(x, a))
  out <- .study_value(x, "output", "output/ard/ard.rds")
  code <- c(
    "# The study's ARD, made from its ARD definition.  Run from the study folder.",
    paste0("# Generated by tflspec ", utils::packageVersion("tflspec"),
           ", ", format(Sys.Date())),
    "",
    .ard_common_lines(unlist(lapply(a$statistics, .split_bar)), x),
    .ard_body_lines(x, a))
  if (save) {
    ids <- unique(a$output_id)
    hashes <- vapply(ids, function(id) .ard_output_hash(x, id, dir), "")
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

# the analyses `a`: their data, their analysis sets, one call each, bound
# into `ard`
.ard_body_lines <- function(x, a) {
  subj <- .study_value(x, "id", "USUBJID")
  code <- "# ---- data"
  pops <- unique(stats::na.omit(c(a$population_id,
                                  a$denominator[a$denominator %in%
                                                  x$populations$population_id])))
  den_ds <- a$denominator[a$denominator %in% x$datasets$dataset &
                            !a$denominator %in% .den_words]
  used_ds <- unique(stats::na.omit(c(a$dataset, den_ds, x$populations$dataset[
    x$populations$population_id %in% pops])))
  for (ds in used_ds) {
    r <- x$datasets[x$datasets$dataset == ds, ]
    obj <- .r_name(ds)
    code <- c(code, sprintf("%s <- %s", obj, .reader(r$path[1L])),
              .derive_code(obj, r$derive[1L]))
  }
  code <- c(code, "", "# ---- populations")
  for (pid in pops) {
    r <- x$populations[x$populations$population_id == pid, ]
    obj <- paste0("pop_", .r_name(pid))
    src <- .r_name(r$dataset[1L])
    code <- c(code, if (is.na(r$where[1L])) sprintf("%s <- %s", obj, src) else
      sprintf("%s <- subset(%s, %s)", obj, src, r$where[1L]),
      .derive_code(obj, r$derive[1L]))
  }
  keys <- tfl_ard_methods()
  # each analysis's data: the dataset, restricted to the population's
  # subjects (or the population itself when it is that dataset), and to
  # the analysis's own subset -- made once, under a name, for every
  # analysis that reads it
  taken <- c(.r_name(used_ds), paste0("pop_", .r_name(pops)))
  data_of <- character()
  made <- character()
  data_name <- vapply(seq_len(nrow(a)), function(i) {
    r <- a[i, ]
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
      if (!is.null(whr)) cond <- sprintf("%s & (%s)", cond, whr)
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
    made <<- c(made, sprintf("%s <- %s", nm, expr))
    nm
  }, "")
  if (length(made)) code <- c(code, "", "# ---- the analysis data", made)
  code <- c(code, "", "# ---- analyses", "ards <- list()")
  for (i in seq_len(nrow(a))) {
    r <- a[i, ]
    pid <- r$population_id
    pop <- if (!is.na(pid)) paste0("pop_", .r_name(pid))
    pop_name <- if (is.null(pop)) "NULL" else pop
    given <- c(.args_given(r$args), if (!is.na(r$strata)) "strata",
               if (!is.na(r$denominator)) "denominator")
    has <- function(arg) arg %in% given
    k <- match(r$method, keys$method)
    kind <- if (is.na(k)) "" else keys$kind[k]
    # R the row writes itself (args, code) may name `data` and
    # `population`, and a subject flag changes its population: those bind
    # the two names in a local scope; any other analysis is one call on
    # the data by its name
    bind <- identical(keys$call[k], "(subjects)") ||
      identical(keys$call[k], "(code)") || .names_data(r)
    den <- .den_code(r$denominator, x, pop, subj,
                     population = if (bind) "population" else pop_name)
    body <- if (bind) .analysis_body(r, keys, subj, has, den) else
      .analysis_body(r, keys, subj, has, den, data = data_name[[i]],
                     population = pop_name)
    core <- strsplit(body, "\n", fixed = TRUE)[[1L]]
    if (bind) {
      core <- c("local({",
                paste0("  data <- ", data_name[[i]]),
                paste0("  population <- ", pop_name),
                paste0("  ", core),
                "})")
    }
    keep <- if (!kind %in% c("continuous", "categorical", "missing") &&
                !identical(keys$call[k], "(subjects)"))
      .split_bar(r$statistics)
    fmt <- c(if (!is.na(k)) .parse_formats(keys$formats[k]),
             .parse_formats(r$formats))
    fmt <- fmt[!duplicated(names(fmt), fromLast = TRUE)]
    core[1L] <- paste("ard <-", core[1L])
    code <- c(code,
              sprintf("# %s / %s%s", r$output_id, r$analysis_id,
                      if (!is.na(r$label)) paste(":", r$label) else ""),
              core,
              if (length(keep)) sprintf("ard <- .keep(ard, c(%s))",
                                        paste(encodeString(keep, quote = "\""),
                                              collapse = ", ")),
              sprintf("ards[[%d]] <- .tag(.fmt(ard%s), %s, %s, %s)", i,
                      if (length(fmt)) paste0(", ", .fmt_vector(fmt)) else "",
                      encodeString(r$output_id, quote = "\""),
                      encodeString(r$analysis_id, quote = "\""),
                      if (is.na(pid)) "NA_character_" else
                        encodeString(pid, quote = "\"")))
  }
  c(code, "", "ard <- do.call(dplyr::bind_rows, ards)")
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
#' @return A single string.
#' @export
tfl_ard_spec_hash <- function(spec, output_id, dir = ".") {
  .ard_output_hash(spec, output_id, dir)
}

.ard_output_hash <- function(spec, output_id, dir = ".") {
  a <- spec$analyses[spec$analyses$output_id %in% output_id, , drop = FALSE]
  a <- a[vapply(a, function(v) !all(is.na(v)), NA)]
  src <- .split_bar(.study_value(spec, "source", NA))
  src_md5 <- vapply(src, function(f) {
    p <- file.path(dir, f)
    if (file.exists(p)) unname(tools::md5sum(p)) else "missing"
  }, "")
  pops <- spec$populations[spec$populations$population_id %in%
                             a$population_id, , drop = FALSE]
  dss <- spec$datasets[spec$datasets$dataset %in%
                         c(a$dataset, pops$dataset), , drop = FALSE]
  txt <- paste(c(utils::capture.output(print(as.list(a[order(a$analysis_id), ]))),
                 utils::capture.output(print(as.list(pops))),
                 utils::capture.output(print(as.list(dss[setdiff(names(dss), "level")]))),
                 utils::capture.output(print(as.list(spec$study))),
                 if (length(src)) paste(src, src_md5)),
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
                      statistics = NULL, methods = NULL) {
  code <- tfl_ard_code(spec, output_id = output_id, save = save,
                        statistics = statistics, methods = methods, dir = dir)
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
