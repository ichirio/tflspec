# ============================================================================
#  The analysis data: named data an analysis reads (sheet `analysis_data`)
# ============================================================================
#
#  A row makes one data, under its `data_id` -- the object's name in the ARD
#  program too: from a dataset or an analysis data above it, the subjects of
#  a population, the records a condition keeps, columns of the population's
#  data added by the subject key, columns derived, and one row for each set
#  of values of some columns (a denominator per subject and phase).  In that
#  order.  An analysis names one in `data` (instead of `dataset` /
#  `population_id`) and may name one as its `denominator`.  The analysis
#  data is the study's: one name, one meaning, for every report.
# ============================================================================

# the sheet, in shape (none: no rows)
.adata_sheet <- function(x) {
  d <- x$analysis_data
  if (is.null(d)) d <- data.frame()
  .normalize_ard_sheet(d, "analysis_data")
}

# an analyses sheet's `data` column (blank when it has none)
.data_col <- function(a) a$data %||% rep(NA_character_, nrow(a))

# A name the ARD program gives no other object: R's, starting with a
# lower-case letter
.adata_name_ok <- function(id) {
  grepl("^[a-z][a-z0-9_.]*$", id) & make.names(id) == id
}

# The rows a data is made from, from the first to itself (its `from`
# followed while it names an analysis data), or NULL when one is missing or
# they go round
.adata_chain <- function(ad, id) {
  out <- character()
  while (!is.na(id) && id %in% ad$data_id) {
    if (id %in% out) return(NULL)
    out <- c(id, out)
    id <- ad$from[match(id, ad$data_id)]
  }
  if (!length(out)) NULL else out
}

# The analysis set of a data: its own population, else the nearest above
# it (NA when none)
.adata_pop <- function(ad, id) {
  for (d in rev(.adata_chain(ad, id))) {
    p <- ad$population_id[match(d, ad$data_id)]
    if (!is.na(p)) return(p)
  }
  NA_character_
}

# The dataset a data is first made from
.adata_dataset <- function(ad, id) {
  ch <- .adata_chain(ad, id)
  if (is.null(ch)) return(NA_character_)
  ad$from[match(ch[1L], ad$data_id)]
}

# The analysis data the analyses `a` read (`data`, a `denominator`), with
# the rows they are made from, in the sheet's order
.adata_used <- function(ad, a) {
  if (!nrow(ad)) return(character())
  ids <- intersect(c(.data_col(a), a$denominator %||% character()), ad$data_id)
  all <- unique(unlist(lapply(ids, function(id) .adata_chain(ad, id))))
  ad$data_id[ad$data_id %in% all]
}

# The problems of the sheet and of the analyses' `data` (character())
.adata_problems <- function(x, a) {
  ad <- .adata_sheet(x)
  err <- character()
  tag <- function(i) sprintf("analysis_data %s", if (is.na(ad$data_id[i]))
    sprintf("row %d", i) else ad$data_id[i])
  pops <- x$populations$population_id
  dss <- x$datasets$dataset
  # the names the program gives other objects
  taken <- c(.r_name(dss), paste0("pop_", .r_name(pops)),
             "data", "population", "ard", "ards", "status")
  for (i in seq_len(nrow(ad))) {
    id <- ad$data_id[i]
    if (is.na(id)) {
      err <- c(err, sprintf("%s: `data_id` is blank", tag(i)))
      next
    }
    if (!.adata_name_ok(id)) {
      err <- c(err, sprintf(paste(
        "%s: `data_id` is the data's name in the ARD program -- a lower-case",
        "letter first, then letters, digits, _ or ."), tag(i)))
    } else if (id %in% taken) {
      err <- c(err, sprintf(
        "%s: `data_id` is the name of another object of the ARD program (a dataset, pop_<population>, data, population, ard ...)",
        tag(i)))
    }
    if (id %in% ad$data_id[seq_len(i - 1L)]) {
      err <- c(err, sprintf("%s: `data_id` repeated", tag(i)))
    }
    from <- ad$from[i]
    if (is.na(from)) {
      err <- c(err, sprintf("%s: `from` is blank (a dataset, or an analysis data above)", tag(i)))
    } else if (!from %in% c(dss, ad$data_id[seq_len(i - 1L)])) {
      err <- c(err, sprintf(
        "%s: `from` %s is neither a dataset nor an analysis data above it",
        tag(i), from))
    }
    p <- ad$population_id[i]
    if (!is.na(p) && !p %in% pops) {
      err <- c(err, sprintf("%s: population %s is not in `populations`", tag(i), p))
    }
    if (length(.split_bar(ad$add[i])) && is.na(.adata_pop(ad, id))) {
      err <- c(err, sprintf(paste(
        "%s: `add` takes columns from the population's data, and it has no",
        "population (here or above)"), tag(i)))
    }
    for (cn in c("where")) {
      v <- ad[[cn]][i]
      if (!is.na(v) && is.null(tryCatch(str2lang(v), error = function(e) NULL))) {
        err <- c(err, sprintf("%s: `%s` does not read as R", tag(i), cn))
      }
    }
  }
  dcol <- .data_col(a)
  for (i in which(!is.na(dcol))) {
    t <- paste(a$output_id[i], a$analysis_id[i], sep = " / ")
    if (!dcol[i] %in% ad$data_id) {
      err <- c(err, sprintf("%s: data %s is not in `analysis_data`", t, dcol[i]))
    }
    both <- c("dataset", "population_id")[!is.na(c(a$dataset[i], a$population_id[i]))]
    if (length(both)) {
      err <- c(err, sprintf(paste(
        "%s: an analysis reads its `data` or a %s; not both (the analysis",
        "data has them)"), t, paste0("`", both, "`", collapse = " and ")))
    }
  }
  err
}

# The lines that make the analysis data `ids` (in their order), and the
# population each is of
.adata_lines <- function(x, ids, subj) {
  ad <- .adata_sheet(x)
  out <- character()
  for (id in ids) {
    r <- ad[match(id, ad$data_id), ]
    from_data <- r$from %in% ad$data_id
    src <- if (from_data) r$from else .r_name(r$from)
    pid <- r$population_id
    pop <- if (!is.na(pid)) paste0("pop_", .r_name(pid))
    pop_ds <- if (!is.na(pid)) x$populations$dataset[
      x$populations$population_id == pid][1L]
    whr <- if (!is.na(r$where)) r$where
    # as each analysis's data: the population itself when the data is its
    # dataset, else the dataset's records of its subjects
    expr <- if (is.null(pop)) {
      if (is.null(whr)) src else sprintf("subset(%s, %s)", src, whr)
    } else if (!from_data && identical(r$from, pop_ds)) {
      if (is.null(whr)) pop else sprintf("subset(%s, %s)", pop, whr)
    } else {
      cond <- sprintf("%s %%in%% %s$%s", subj, pop, subj)
      if (!is.null(whr)) cond <- sprintf("%s & (%s)", cond, whr)
      sprintf("subset(%s, %s)", src, cond)
    }
    out <- c(out, sprintf("%s <- %s", id, expr))
    add <- .split_bar(r$add)
    if (length(add)) {
      p <- paste0("pop_", .r_name(.adata_pop(ad, id)))
      q <- function(v) paste(encodeString(v, quote = "\""), collapse = ", ")
      # the population's values replace a column of the same name
      out <- c(out, sprintf(
        "%s <- dplyr::left_join(%s[setdiff(names(%s), c(%s))], %s[c(%s)], by = %s)",
        id, id, id, q(add), p, q(c(subj, add)), encodeString(subj, quote = "\"")))
    }
    out <- c(out, .derive_code(id, r$derive))
    dis <- .split_bar(r$distinct)
    if (length(dis)) {
      out <- c(out, sprintf("%s <- dplyr::distinct(%s, %s, .keep_all = TRUE)",
                            id, id, paste(dis, collapse = ", ")))
    }
  }
  out
}
