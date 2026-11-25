# tfl_check_spec(adam = ): a table's header N and its counts, read from
# different data, where a column (or page) variable has another value for
# the same subject (A2, 2026-11-25).  The N is the report's own: whatever
# data or population its BIGN analysis names (ADSL's subjects, or the
# subjects with records in ADPC); the counts are its analyses that group by
# BIGN's variable.  Where the two data differ, a subject may be counted under
# one group in the N and under another in the rows.  A data check, with the
# study's data as the `!=` check reads them (.ne_na_reader()): a warning,
# nothing rewritten.

# The dataset an analysis's rows come from (a row of the analyses sheet; `ad`
# its report's analysis data): its analysis data's first dataset, else its
# dataset, else its population's; NA when none
.n_rows_from <- function(x, ad, r) {
  d <- r$data %||% NA
  if (!is.na(d)) return(.adata_dataset(ad, d))
  if (!is.na(r$dataset %||% NA)) return(r$dataset)
  p <- r$population_id %||% NA
  if (is.na(p)) NA_character_ else x$populations$dataset[match(p, x$populations$population_id)]
}

# The conditions that keep an analysis's rows of its dataset `ds`: its
# population's (when the population is of that dataset), its analysis data's
# and those it is made from, its own -- R text, in order
.n_rows_where <- function(x, ad, r, ds) {
  d <- r$data %||% NA
  ch <- if (!is.na(d)) .adata_chain(ad, d)
  p <- if (!is.na(d)) .adata_pop(ad, d) else r$population_id %||% NA
  k <- match(p, x$populations$population_id)
  pw <- if (!is.na(k) && identical(toupper(x$populations$dataset[k]), toupper(ds))) {
    x$populations$where[k]
  }
  w <- c(pw, ad$where[match(ch, ad$data_id)], r$where %||% NA)
  w[!is.na(w)]
}

# The rows of `df` the conditions keep (one that cannot be run here -- a
# derived column, a package's function -- keeps all)
.n_keep <- function(df, where) {
  for (w in where) {
    e <- tryCatch(parse(text = w, keep.source = FALSE)[[1L]], error = function(err) NULL)
    k <- if (!is.null(e)) tryCatch(eval(e, df, baseenv()), error = function(err) NULL)
    if (is.logical(k) && length(k) == nrow(df)) df <- df[!is.na(k) & k, , drop = FALSE]
  }
  df
}

# What to look at, from the ARD definition alone (no data): one item a
# report, BIGN analysis and analysis grouping by its variable from another
# dataset -- not when the analysis's data takes the variable from the
# subjects (`add` of its data or of one it is made from; a group a
# denominator of the same data counts by, .den_needs(): the ARD program
# replaces the records' value with the subjects')
.n_count_pairs <- function(x, ids) {
  a <- .normalize_ard_sheet(x$analyses %||% data.frame(), "analyses")
  if (!nrow(a)) return(list())
  keys <- tfl_ard_methods()
  out <- list()
  for (o in intersect(unique(stats::na.omit(a$output_id)), ids)) {
    ao <- a[a$output_id %in% o, , drop = FALSE]
    bn <- which(ao$analysis_id %in% .bign_id)
    if (!length(bn) || nrow(ao) < 2L) next
    ad <- .adata_of(.adata_sheet(x), o)
    d <- .data_col(ao)
    src <- paste(ao$dataset, ao$population_id, ao$where, sep = "\r")
    grp <- function(i) unlist(lapply(c(ao$by[i], ao$strata[i] %||% NA), .split_bar))
    taken <- function(i) {
      same <- if (!is.na(d[i])) which(d %in% d[i]) else which(is.na(d) & src == src[i])
      add <- if (!is.na(d[i])) unlist(lapply(.adata_chain(ad, d[i]), function(k)
        .split_bar(ad$add[match(k, ad$data_id)])))
      c(add, unlist(lapply(same, function(j) .den_needs(ao[j, ], keys))))
    }
    for (b in bn) {
      vb <- unique(c(.split_bar(ao$variables[b]), grp(b)))
      db <- .n_rows_from(x, ad, ao[b, ])
      if (!length(vb) || is.na(db)) next
      for (i in setdiff(seq_len(nrow(ao)), bn)) {
        v <- setdiff(intersect(vb, grp(i)), taken(i))
        di <- .n_rows_from(x, ad, ao[i, ])
        if (!length(v) || is.na(di) || identical(toupper(di), toupper(db))) next
        out[[length(out) + 1L]] <- list(
          output_id = o, bign = ao$analysis_id[b], analysis = ao$analysis_id[i], vars = v,
          n_dataset = db, n_where = .n_rows_where(x, ad, ao[b, ], db),
          dataset = di, where = .n_rows_where(x, ad, ao[i, ], di))
      }
    }
  }
  out
}

# The warnings: the subjects of the N's rows whose records (the counts' rows)
# carry another value of the variable.  `adam` as .check_ne_na_rows() takes
# it; the data are read only for the pairs above, each dataset once
.check_n_rows <- function(study, adam, ids) {
  if (is.null(adam) || isFALSE(adam) || is.null(study$dir)) return(.check_rows())
  x <- tryCatch(suppressMessages(suppressWarnings(.ard_spec_of_study(study)$spec)),
                error = function(e) NULL)
  if (is.null(x)) return(.check_rows())
  pairs <- .n_count_pairs(x, ids)
  if (!length(pairs)) return(.check_rows())
  get_df <- .ne_na_reader(study, adam)
  if (is.null(get_df)) return(.check_rows())
  subj <- .study_value(x, "id", "USUBJID")
  rows <- list(.check_rows())
  for (pr in pairs) {
    dn <- get_df(pr$n_dataset)
    dr <- get_df(pr$dataset)
    if (!is.null(dn) && !is.null(dr)) rows[[length(rows) + 1L]] <- .n_pair_rows(pr, dn, dr, subj)
  }
  out <- do.call(rbind, rows)
  out[!duplicated(out), , drop = FALSE]
}

# One pair's warnings (.n_count_pairs()'s item `pr`), on the N's data `dn`
# and the counts' data `dr` (each kept by its conditions here): a variable
# both carry, with the subject key, whose value of a subject's records is
# none of its value in the N's rows
.n_pair_rows <- function(pr, dn, dr, subj) {
  rows <- list(.check_rows())
  dn <- .n_keep(dn, pr$n_where)
  dr <- .n_keep(dr, pr$where)
  for (v in pr$vars) {
    if (!all(c(subj, v) %in% names(dn)) || !all(c(subj, v) %in% names(dr))) next
    key <- function(df) paste(as.character(df[[subj]]), as.character(df[[v]]), sep = "\r")
    off <- dr[[subj]] %in% dn[[subj]] & !key(dr) %in% key(dn)
    who <- unique(as.character(dr[[subj]][off]))
    if (!length(who)) next
    vals <- function(df) paste(unique(as.character(df[[v]][as.character(df[[subj]]) == who[1L]])),
                               collapse = ", ")
    rows[[length(rows) + 1L]] <- .check_rows(
      output_id = pr$output_id, part = "ard", item = paste("analysis", pr$analysis, "/ by"),
      level = "warning", message = sprintf(paste0(
        "the header N (%s) counts %s's %s and analysis %s counts %s's: they differ for %d ",
        "subject%s of the data (%s: %s in %s, %s in %s), counted under another group than in ",
        "the N. If the two must agree, take the N's value (`add: %s` on the analysis data)"),
        pr$bign, toupper(pr$n_dataset), v, pr$analysis, toupper(pr$dataset), length(who),
        if (length(who) == 1L) "" else "s", who[1L], vals(dn), toupper(pr$n_dataset), vals(dr),
        toupper(pr$dataset), v))
  }
  do.call(rbind, rows)
}
