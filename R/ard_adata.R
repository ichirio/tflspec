# ============================================================================
#  The analysis data: named data an analysis reads (sheet `analysis_data`)
# ============================================================================
#
#  A row makes one data, under its `data_id` -- the object's name in the ARD
#  program too: from a dataset or an analysis data above it, the subjects of
#  a population (or of an analysis data above: `subjects`, the subjects a
#  report's own analysis set kept), the records a condition keeps, columns
#  of the population's (or that data's) added by the subject key, columns
#  derived (the code lists made factors of them too), the columns kept, and
#  one row for each set of values of some columns (a denominator per
#  subject and phase).  In that order.  Or, when the columns cannot say it,
#  `code`: R that makes the data itself (the program's objects in reach:
#  the datasets, pop_<population>, the analysis data above), the other
#  columns but `from` left blank.  An analysis names one in `data` (instead of `dataset` /
#  `population_id`) and may name one as its `denominator`.  The analysis
#  data is a report's (`output_id`): its name means one thing in that
#  report, and another report may give the same name another meaning (an
#  adsl_saf of a Phase I table and of a Phase II table).  A report's rows
#  name only its own rows and the study's datasets.
# ============================================================================

# the sheet, in shape (none: no rows)
.adata_sheet <- function(x) {
  d <- x$analysis_data
  if (is.null(d)) d <- data.frame()
  .normalize_ard_sheet(d, "analysis_data")
}

# a report's rows of the sheet
.adata_of <- function(ad, output_id) {
  ad[!is.na(ad$output_id) & ad$output_id %in% output_id, , drop = FALSE]
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

# The analysis set of a data: its own population, else that of the data
# whose subjects it keeps, else the nearest above it (NA when none)
.adata_pop <- function(ad, id, seen = character()) {
  for (d in rev(.adata_chain(ad, id))) {
    i <- match(d, ad$data_id)
    p <- ad$population_id[i]
    if (!is.na(p)) return(p)
    s <- ad$subjects[i] %||% NA
    if (!is.na(s) && !s %in% c(seen, d)) return(.adata_pop(ad, s, c(seen, d)))
  }
  NA_character_
}

# The data a row's `add` takes its columns from: the data whose subjects it
# keeps, else its analysis set's (`pop_<id>`); NA when none
.adata_add_from <- function(ad, id) {
  for (d in rev(.adata_chain(ad, id))) {
    i <- match(d, ad$data_id)
    if (!is.na(ad$subjects[i] %||% NA)) return(ad$subjects[i])
    if (!is.na(ad$population_id[i])) return(paste0("pop_", .r_name(ad$population_id[i])))
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
  # with the data whose subjects they keep (and theirs)
  all <- character()
  while (length(new <- setdiff(unique(unlist(lapply(ids, function(id)
    .adata_chain(ad, id)))), all))) {
    all <- c(all, new)
    ids <- intersect(stats::na.omit(ad$subjects[match(new, ad$data_id)]), ad$data_id)
  }
  ad$data_id[ad$data_id %in% all]
}

# The problems of the sheet and of the analyses' `data` (character())
.adata_problems <- function(x, a) {
  ad <- .adata_sheet(x)
  err <- character()
  tag <- function(i) sprintf("analysis_data %s%s",
    if (is.na(ad$output_id[i])) "" else paste0(ad$output_id[i], " / "),
    if (is.na(ad$data_id[i])) sprintf("row %d", i) else ad$data_id[i])
  pops <- x$populations$population_id
  dss <- x$datasets$dataset
  # the names the program gives other objects
  taken <- c(.r_name(dss), paste0("pop_", .r_name(pops)),
             "data", "population", "ard", "ards", "status")
  # an analysis data is a report's: each row names its report, and the
  # rows above it are those of the same report
  for (i in which(is.na(ad$output_id))) {
    err <- c(err, sprintf(paste(
      "%s: `output_id` is blank -- an analysis data is a report's (one made",
      "before tflspec 0.0.24.9055 is the study's: give its rows their report)"),
      tag(i)))
  }
  for (o in unique(stats::na.omit(ad$output_id))) {
  rows <- which(ad$output_id == o)
  ad_o <- ad[rows, , drop = FALSE]
  for (k in seq_along(rows)) {
    i <- rows[k]
    above <- ad$data_id[rows[seq_len(k - 1L)]]
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
    if (id %in% above) {
      err <- c(err, sprintf("%s: `data_id` repeated in the report", tag(i)))
    }
    from <- ad$from[i]
    if (is.na(from)) {
      err <- c(err, sprintf("%s: `from` is blank (a dataset, or an analysis data above)", tag(i)))
    } else if (!from %in% c(dss, above)) {
      err <- c(err, sprintf(
        "%s: `from` %s is neither a dataset nor an analysis data above it in the report",
        tag(i), from))
    }
    p <- ad$population_id[i]
    if (!is.na(p) && !p %in% pops) {
      err <- c(err, sprintf("%s: population %s is not in `populations`", tag(i), p))
    }
    s <- ad$subjects[i]
    if (!is.na(s)) {
      if (!s %in% above) {
        err <- c(err, sprintf(
          "%s: `subjects` %s is not an analysis data above it in the report", tag(i), s))
      }
      if (!is.na(p)) {
        err <- c(err, sprintf(paste(
          "%s: the subjects are a population's (`population_id`) or an",
          "analysis data's (`subjects`); not both"), tag(i)))
      }
    }
    code <- ad$code[i] %||% NA
    if (!is.na(code)) {
      if (is.null(tryCatch(parse(text = code), error = function(e) NULL))) {
        err <- c(err, sprintf("%s: `code` does not read as R", tag(i)))
      }
      also <- c("population_id", "subjects", "where", "add", "derive", "keep", "distinct")
      also <- also[vapply(also, function(cn) !is.na(ad[[cn]][i] %||% NA), NA)]
      if (length(also)) {
        err <- c(err, sprintf(paste(
          "%s: `code` makes the data itself; %s are for a data the columns make",
          "(leave them blank, or the code blank)"), tag(i),
          paste0("`", also, "`", collapse = ", ")))
      }
      next
    }
    if (length(.split_bar(ad$add[i])) && is.na(.adata_add_from(ad_o, id))) {
      err <- c(err, sprintf(paste(
        "%s: `add` takes columns from the population's data or the data of",
        "`subjects`, and it has neither (here or above)"), tag(i)))
    }
    for (cn in c("where")) {
      v <- ad[[cn]][i]
      if (!is.na(v) && is.null(tryCatch(str2lang(v), error = function(e) NULL))) {
        err <- c(err, sprintf("%s: `%s` does not read as R", tag(i), cn))
      }
    }
  }
  }
  # an analysis reads (and divides by) its own report's analysis data
  dcol <- .data_col(a)
  den <- a$denominator %||% rep(NA_character_, nrow(a))
  for (i in which(!is.na(dcol) | den %in% ad$data_id)) {
    t <- paste(a$output_id[i], a$analysis_id[i], sep = " / ")
    own <- .adata_of(ad, a$output_id[i])$data_id
    if (!is.na(dcol[i]) && !dcol[i] %in% own) {
      err <- c(err, sprintf(paste(
        "%s: data %s is not an analysis data of %s%s"), t, dcol[i], a$output_id[i],
        if (dcol[i] %in% ad$data_id) " (another report's: copy it into this one)" else ""))
    }
    if (!is.na(den[i]) && den[i] %in% ad$data_id && !den[i] %in% own) {
      err <- c(err, sprintf(
        "%s: denominator %s is not an analysis data of %s (another report's)",
        t, den[i], a$output_id[i]))
    }
    if (is.na(dcol[i])) next
    both <- c("dataset", "population_id")[!is.na(c(a$dataset[i], a$population_id[i]))]
    if (length(both)) {
      err <- c(err, sprintf(paste(
        "%s: an analysis reads its `data` or a %s; not both (the analysis",
        "data has them)"), t, paste0("`", both, "`", collapse = " and ")))
    }
  }
  err
}

# The lines that make the analysis data `ids` (in their order); `levels`:
# the code lists, made factors of the columns a row derives
.adata_lines <- function(x, ids, subj, levels = NULL) {
  ad <- .adata_sheet(x)
  out <- character()
  for (id in ids) {
    r <- ad[match(id, ad$data_id), ]
    # written as R: its value is the data (what it makes on the way stays
    # inside)
    if (!is.na(r$code %||% NA)) {
      out <- c(out, sprintf("%s <- local({\n%s\n})", id,
                            paste0("  ", strsplit(trimws(r$code), "\n", fixed = TRUE)[[1L]],
                                   collapse = "\n")))
      next
    }
    from_data <- r$from %in% ad$data_id
    src <- if (from_data) r$from else .r_name(r$from)
    pid <- r$population_id
    pop <- if (!is.na(pid)) paste0("pop_", .r_name(pid))
    pop_ds <- if (!is.na(pid)) x$populations$dataset[
      x$populations$population_id == pid][1L]
    # the subjects an analysis data above kept (a report's own analysis set)
    if (!is.na(r$subjects)) pop <- r$subjects
    whr <- if (!is.na(r$where)) r$where
    # as each analysis's data: the population itself when the data is its
    # dataset, else the dataset's records of its subjects
    base <- if (is.null(pop)) {
      list(src, if (!is.null(whr)) sprintf("subset(%s)", whr))
    } else if (!from_data && is.na(r$subjects) && identical(r$from, pop_ds)) {
      list(pop, if (!is.null(whr)) sprintf("subset(%s)", whr))
    } else {
      cond <- sprintf("%s %%in%% %s$%s", subj, pop, subj)
      if (!is.null(whr)) cond <- .cond_and(cond, whr)
      list(src, sprintf("subset(%s)", cond))
    }
    add <- .split_bar(r$add)
    keep <- .split_bar(r$keep)
    dis <- .split_bar(r$distinct)
    # what follows the rows: derived, its code lists, the columns kept, one
    # row per ... -- in the statement that makes it, unless columns are
    # added from the subjects' data first (a join, a statement of its own)
    rest <- c(.derive_steps(r$derive, levels),
              if (length(keep)) sprintf("subset(select = c(%s))",
                                        paste(unique(c(subj, keep)), collapse = ", ")),
              if (length(dis)) sprintf("dplyr::distinct(%s, .keep_all = TRUE)",
                                       paste(dis, collapse = ", ")))
    if (!length(add)) {
      out <- c(out, .make_code(id, base[[1L]], c(base[[2L]], rest)))
      next
    }
    out <- c(out, .make_code(id, base[[1L]], base[[2L]]))
    p <- .adata_add_from(ad, id)
    q <- function(v) paste(encodeString(v, quote = "\""), collapse = ", ")
    # the population's values replace a column of the same name
    out <- c(out, sprintf(
      "%s <- dplyr::left_join(%s[setdiff(names(%s), c(%s))], %s[c(%s)], by = %s)",
      id, id, id, q(add), p, q(c(subj, add)), encodeString(subj, quote = "\"")))
    if (length(rest)) out <- c(out, .make_code(id, id, rest))
  }
  out
}
