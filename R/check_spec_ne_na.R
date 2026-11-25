# tfl_check_spec(adam = ): a condition `x != "A"` where x has NA in the data.
# In a filter a row whose x is NA gives NA, and the row is dropped; the
# author who wrote `x != "A"` meant "not A", NA rows included.  The check is
# of the SPEC against the data, at check time only: the generated code is
# what the SPEC says.

# The warnings for the conditions of the ARD definition (populations,
# records, analysis data, analyses: their `where`) that compare a variable
# with `!=` where the data have NA in the variable.  `adam`: the ADaM data as
# tfl_check_fig_design(adam = ) takes it; NULL, a dataset that is not found
# and a variable not in it give no row.
.check_ne_na_rows <- function(study, adam, ids, study_rows = TRUE) {
  if (is.null(adam) || is.null(study$dir)) return(.check_rows())
  adam <- tryCatch(pp_prep_adam(adam), error = function(e) NULL)
  x <- tryCatch(.ard_spec_of_study(study)$spec, error = function(e) NULL)
  if (is.null(adam) || is.null(x)) return(.check_rows())
  ad <- .adata_sheet(x)
  a <- .normalize_ard_sheet(x$analyses %||% data.frame(), "analyses")
  # (output_id, what it is, the condition, the dataset it is applied to)
  cand <- list()
  add <- function(output_id, tag, where, dataset) {
    if (is.null(where) || !length(where)) return(invisible())
    for (i in seq_along(where)) {
      if (is.na(where[i]) || is.na(dataset[i])) next
      cand[[length(cand) + 1L]] <<- list(output_id = output_id[i], tag = tag[i],
                                         where = where[i], dataset = dataset[i])
    }
  }
  if (study_rows) {
    p <- x$populations
    if (is.data.frame(p) && nrow(p)) add(rep(NA_character_, nrow(p)), paste("population", p$population_id), p$where, p$dataset)
    r <- .records_sheet(x)
    if (nrow(r)) add(rep(NA_character_, nrow(r)), paste("record", r$record_id), r$where, r$dataset)
  }
  if (nrow(ad)) {
    keep <- ad$output_id %in% ids
    add(ad$output_id[keep], paste("analysis data", ad$data_id[keep]), ad$where[keep],
        vapply(ad$data_id[keep], function(d) .adata_dataset(ad, d), ""))
  }
  if (nrow(a)) {
    keep <- a$output_id %in% ids
    ds <- a$dataset
    dat <- .data_col(a)
    for (i in which(!is.na(dat))) ds[i] <- .adata_dataset(ad, dat[i])
    add(a$output_id[keep], paste("analysis", a$analysis_id[keep]), a$where[keep], ds[keep])
  }
  rows <- list(.check_rows())
  for (cd in cand) {
    df <- adam[[toupper(cd$dataset)]]
    if (is.null(df)) next
    for (m in .ne_na_messages(cd$where, df, toupper(cd$dataset))) {
      rows[[length(rows) + 1L]] <- .check_rows(
        output_id = cd$output_id, part = "ard", item = paste(cd$tag, "/ where"),
        level = "warning",
        message = sprintf("%s: `where` is `%s`: %s", cd$tag, cd$where, m))
    }
  }
  out <- do.call(rbind, rows)
  out[!duplicated(out), , drop = FALSE]
}

# The messages of the `var != value` parts of a condition (R text) that drop
# rows of `df` for an NA of the variable; none when the text does not parse.
.ne_na_messages <- function(where, df, ds_name) {
  e <- tryCatch(parse(text = where, keep.source = FALSE), error = function(err) NULL)
  if (is.null(e)) return(character())
  hits <- list()
  walk <- function(x) {
    if (!is.call(x)) return(invisible())
    h <- .ne_na_part(x)
    if (!is.null(h)) hits[[length(hits) + 1L]] <<- h
    for (y in as.list(x)) if (is.call(y)) walk(y)
  }
  for (ex in as.list(e)) walk(ex)
  out <- character()
  seen <- character()
  for (h in hits) {
    if (!h$var %in% names(df)) next
    key <- paste(h$var, deparse1(h$value))
    if (key %in% seen) next
    seen <- c(seen, key)
    n <- .ne_na_dropped(e[[1L]], h, df)
    if (!n) next
    fix <- sprintf("!%s %%in%% %s", h$var, deparse1(h$value))
    out <- c(out, sprintf(
      paste0("`%s` drops %d row%s of %s where %s is NA (a filter keeps only TRUE); ",
             "to keep them write `%s`"),
      deparse1(h$call), n, if (n == 1L) "" else "s", ds_name, h$var, fix))
  }
  out
}

# `var != value` or `value != var` (a bare name and a constant), or NULL
.ne_na_part <- function(x) {
  if (!identical(x[[1L]], as.name("!=")) || length(x) != 3L) return(NULL)
  const <- function(v) {
    if (is.call(v) && identical(v[[1L]], as.name("-")) && length(v) == 2L) v <- v[[2L]]
    (is.character(v) || is.numeric(v) || is.logical(v)) && length(v) == 1L && !is.na(v)
  }
  for (k in 2:3) {
    if (is.name(x[[k]]) && const(x[[5L - k]])) {
      return(list(call = x, var = as.character(x[[k]]), value = x[[5L - k]]))
    }
  }
  NULL
}

# The rows the condition drops for an NA of the variable: those the condition
# with its `!=` written as `!var %in% value` keeps and it does not (the whole
# condition is run on the data); the data's NA count of the variable when the
# condition cannot be run here (a derived column, a function of a package).
.ne_na_dropped <- function(expr, h, df) {
  swap <- function(x) {
    if (!is.call(x)) return(x)
    if (identical(x, h$call)) {
      return(call("!", call("%in%", as.name(h$var), h$value)))
    }
    as.call(lapply(as.list(x), swap))
  }
  kept <- function(ex) {
    r <- tryCatch(eval(ex, df, baseenv()), error = function(err) NULL)
    if (is.logical(r) && length(r) == nrow(df)) !is.na(r) & r else NULL
  }
  k0 <- kept(expr)
  k1 <- kept(swap(expr))
  if (is.null(k0) || is.null(k1)) return(sum(is.na(df[[h$var]])))
  sum(k1 & !k0)
}
