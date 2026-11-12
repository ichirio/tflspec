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
  # (normalized once while the sheet is the same: every report's
  # fingerprint and check asks for it as a study opens)
  hit <- .adata_memo$sheet
  if (!is.null(hit) && identical(hit$raw, d)) return(hit$out)
  out <- .normalize_ard_sheet(d, "analysis_data")
  .adata_memo$sheet <- list(raw = d, out = out)
  out
}
.adata_memo <- new.env(parent = emptyenv())

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

# The analysis data whose subjects an analysis data keeps (`subjects`; the
# nearest one, from the data itself up the rows it is made from), or NULL
# when none: the report's analysis set as it counts them, a subset applied
.adata_subjects_of <- function(ad, id) {
  if (is.null(ad) || length(id) != 1L || is.na(id)) return(NULL)
  for (d in rev(.adata_chain(ad, id))) {
    s <- ad$subjects[match(d, ad$data_id)] %||% NA
    if (!is.na(s)) return(s)
  }
  NULL
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
    ids <- intersect(c(stats::na.omit(ad$subjects[match(new, ad$data_id)]),
                       .adata_code_reads(ad, new)), ad$data_id)
  }
  ad$data_id[ad$data_id %in% all]
}

# The analysis data the code of the rows `ids` reads by name (a data made
# by `code` may read another of its report's: a template's subset set)
.adata_code_reads <- function(ad, ids) {
  code <- ad$code[match(ids, ad$data_id)] %||% NA
  code <- code[!is.na(code)]
  if (!length(code)) return(character())
  unique(unlist(lapply(code, function(x) {
    intersect(tryCatch(all.names(parse(text = x, keep.source = FALSE)),
                       error = function(e) character()), ad$data_id)
  })))
}

# The problems of the sheet and of the analyses' `data`, as rows
# (.spec_problems_keyed()'s)
.adata_problems <- function(x, a) {
  ad <- .adata_sheet(x)
  pr <- list(.spec_problems_empty())
  # a row of the sheet's, and an analysis's
  add_d <- function(i, field, message) {
    pr[[length(pr) + 1L]] <<- .spec_problem(message, "analysis_data",
      ad$output_id[i], if (is.na(ad$data_id[i])) "" else ad$data_id[i], field)
  }
  add_a <- function(i, field, message) {
    pr[[length(pr) + 1L]] <<- .ard_problem(a, i, field, message)
  }
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
    add_d(i, "output_id", sprintf(paste(
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
      add_d(i, "data_id", sprintf("%s: `data_id` is blank", tag(i)))
      next
    }
    if (!.adata_name_ok(id)) {
      add_d(i, "data_id", sprintf(paste(
        "%s: `data_id` is the data's name in the ARD program -- a lower-case",
        "letter first, then letters, digits, _ or ."), tag(i)))
    } else if (id %in% taken) {
      add_d(i, "data_id", sprintf(
        "%s: `data_id` is the name of another object of the ARD program (a dataset, pop_<population>, data, population, ard ...)",
        tag(i)))
    }
    if (id %in% above) {
      add_d(i, "data_id", sprintf("%s: `data_id` repeated in the report", tag(i)))
    }
    from <- ad$from[i]
    if (is.na(from)) {
      add_d(i, "from", sprintf("%s: `from` is blank (a dataset, or an analysis data above)", tag(i)))
    } else if (!from %in% c(dss, above)) {
      add_d(i, "from", sprintf(
        "%s: `from` %s is neither a dataset nor an analysis data above it in the report",
        tag(i), from))
    }
    p <- ad$population_id[i]
    if (!is.na(p) && !p %in% pops) {
      add_d(i, "population_id", sprintf("%s: population %s is not in `populations`", tag(i), p))
    }
    # each derive piece `name = expression`, as R reads it (a `|` of R's own
    # inside parentheses is not a separator)
    for (piece in .split_bar(ad$derive[i] %||% NA)) {
      ok <- !is.null(tryCatch(parse(text = sprintf("list(%s)", piece), keep.source = FALSE),
                              error = function(e) NULL)) &&
        grepl("^[A-Za-z.][A-Za-z0-9._]* *=[^=]", piece)
      if (!ok) {
        add_d(i, "derive", sprintf(paste(
          "%s: `derive` piece %s does not read as `name = expression`",
          "(pieces are separated by `|` outside parentheses; R's own `|` goes inside them)"),
          tag(i), sQuote(piece, FALSE)))
      }
    }
    s <- ad$subjects[i]
    if (!is.na(s)) {
      if (!s %in% above) {
        add_d(i, "subjects", sprintf(
          "%s: `subjects` %s is not an analysis data above it in the report", tag(i), s))
      }
      if (!is.na(p)) {
        add_d(i, "subjects", sprintf(paste(
          "%s: the subjects are a population's (`population_id`) or an",
          "analysis data's (`subjects`); not both"), tag(i)))
      }
    }
    code <- ad$code[i] %||% NA
    if (!is.na(code)) {
      if (is.null(tryCatch(parse(text = code), error = function(e) NULL))) {
        add_d(i, "code", sprintf("%s: `code` does not read as R", tag(i)))
      }
      # (`population_id` with `code`: the set's subjects are made first,
      # in reach as <dataset>_sub; the other columns are for a data the
      # columns make)
      also <- c("subjects", "where", "add", "derive", "keep", "distinct")
      also <- also[vapply(also, function(cn) !is.na(ad[[cn]][i] %||% NA), NA)]
      if (length(also)) {
        add_d(i, "code", sprintf(paste(
          "%s: `code` makes the data itself; %s are for a data the columns make",
          "(leave them blank, or the code blank)"), tag(i),
          paste0("`", also, "`", collapse = ", ")))
      }
      next
    }
    if (length(.split_bar(ad$add[i])) && is.na(.adata_add_from(ad_o, id))) {
      add_d(i, "add", sprintf(paste(
        "%s: `add` takes columns from the population's data or the data of",
        "`subjects`, and it has neither (here or above)"), tag(i)))
    }
    for (cn in c("where")) {
      v <- ad[[cn]][i]
      if (!is.na(v) && is.null(tryCatch(str2lang(v), error = function(e) NULL))) {
        add_d(i, "where", sprintf("%s: `%s` does not read as R", tag(i), cn))
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
      add_a(i, "data", sprintf(paste(
        "%s: data %s is not an analysis data of %s%s"), t, dcol[i], a$output_id[i],
        if (dcol[i] %in% ad$data_id) " (another report's: copy it into this one)" else ""))
    }
    if (!is.na(den[i]) && den[i] %in% ad$data_id && !den[i] %in% own) {
      add_a(i, "denominator", sprintf(
        "%s: denominator %s is not an analysis data of %s (another report's)",
        t, den[i], a$output_id[i]))
    }
    if (is.na(dcol[i])) next
    both <- c("dataset", "population_id")[!is.na(c(a$dataset[i], a$population_id[i]))]
    if (length(both)) {
      add_a(i, "data", sprintf(paste(
        "%s: an analysis reads its `data` or a %s; not both (the analysis",
        "data has them)"), t, paste0("`", both, "`", collapse = " and ")))
    }
  }
  do.call(rbind, pr)
}

# The comment above a built dataset (DATA-PART 6, N11): what it is -- the
# subjects, the records, the denominator -- and where each step takes its
# input from, then the definition's own label.  Lines of at most 80 columns.
.adata_comment <- function(id, what = NULL, label = NA) {
  txt <- paste(c(paste0(id, ":"), what,
                 if (!is.na(label) && nzchar(trimws(label))) trimws(label)),
               collapse = " ")
  paste0("# ", strwrap(txt, width = 78L, simplify = TRUE))
}

# The lines that make the analysis data `ids` (in their order); `levels`:
# the code lists, each data made factors again (.derive_steps()); `avoid`:
# the program's names, which a data's own code does not assign; `reads`:
# each data's columns the analyses read (kept whatever `keep` says); `dens`:
# the data an analysis divides by (the denominators)
.adata_lines <- function(x, ids, subj, levels = NULL, avoid = character(),
                         reads = list(), needs = list(), dens = character()) {
  ad <- .adata_sheet(x)
  out <- character()
  lv <- if (length(levels)) .levels_step
  for (id in ids) {
    r <- ad[match(id, ad$data_id), ]
    lbl <- r$label %||% NA
    # written as R: its value is the data.  What comes before its last
    # expression stays as it is, the last is the data (.code_as_program()); R
    # that assigns a name the program has runs in local()
    if (!is.na(r$code %||% NA)) {
      # what it is, as the definition says, above it (a reviewer maps the SAP
      # to the code by it)
      if (!is.na(lbl) && nzchar(trimws(lbl))) out <- c(out, .adata_comment(id, NULL, lbl))
      code <- trimws(r$code)
      # (made from another analysis data, which has the code lists on
      # already: none after it -- a level its code adds, demog's Missing,
      # stays)
      lv_c <- if (!(r$from %||% NA) %in% ad$data_id) lv
      made <- .code_as_program(code, id, list(lv_c), c(avoid, ids))
      if (is.null(made)) {
        made <- c(sprintf("%s <- local({\n%s\n})", id,
                          paste0("  ", strsplit(code, "\n", fixed = TRUE)[[1L]],
                                 collapse = "\n")),
                  if (length(lv_c)) .make_code(id, id, lv_c))
      }
      out <- c(out, made)
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
    flt <- if (!is.null(whr)) sprintf("dplyr::filter(%s)", whr)
    add <- .split_bar(r$add)
    keep <- .split_bar(r$keep)
    dis <- .split_bar(r$distinct)
    # the subject-level columns it takes: those asked for (`add`), and the
    # groups the analyses' denominators count by (their by, strata)
    # (not the columns the population's derive makes: a record's TRTA,
    # APERIOD ... are its own)
    pops <- x$populations
    take <- c(add, setdiff(needs[[id]], .derive_names(
      pops$derive[pops$population_id %in% .adata_pop(ad, id)])))
    # three stages: the denominator (the population's subjects: its own
    # data), the target subjects (the records joined to them, the subjects'
    # columns replacing theirs), the target records (the report's condition,
    # after the join).  A data of the subjects already: `add` takes its
    # columns by the same join
    add_from <- .adata_add_from(ad, id)
    if (is.null(pop) && length(add) && !is.na(add_from)) {
      base <- src
      steps0 <- c(.subjects_join(add_from, subj, add), flt)
      kind <- "made"
    } else if (is.null(pop)) {
      base <- src
      steps0 <- flt
      kind <- "made"
    } else if (!from_data && is.na(r$subjects) && identical(r$from, pop_ds)) {
      # the population itself (its dataset): its subjects, the condition
      base <- pop
      steps0 <- flt
      kind <- "subjects"
    } else {
      base <- src
      steps0 <- c(.subjects_join(pop, subj, take), flt)
      kind <- "records"
    }
    # what it is, above it
    lab_pop <- if (!is.na(pid)) {
      k <- match(pid, x$populations$population_id)
      pl <- x$populations$label[k] %||% NA
      sprintf("population %s%s", pid, if (!is.na(pl)) sprintf(" (%s)", pl) else "")
    }
    what <- switch(kind,
      subjects = paste0("the subjects. ", lab_pop, if (!is.null(whr)) "; subset"),
      records = sprintf("the records. %s of the subjects of %s%s%s",
                        toupper(if (from_data) .adata_dataset(ad, id) %||% src else r$from),
                        pop, if (length(take)) sprintf(", %s from ADSL", paste(take, collapse = ", ")) else "",
                        if (!is.null(whr)) "; where" else ""),
      made = if (id %in% dens) "the denominator." else sprintf("made from %s.", src))
    out <- c(out, .adata_comment(id, what, lbl))
    # what follows the rows: the columns added, derived, the columns kept,
    # one row per ..., its code lists
    steps <- c(steps0, .derive_steps(r$derive),
               # the columns kept: the subject, those the analyses read on
               # it (their by, strata, variables), and the ones asked for
               if (length(keep)) sprintf("dplyr::select(%s)",
                                         paste(unique(c(subj, keep, reads[[id]])),
                                               collapse = ", ")),
               if (length(dis)) sprintf("dplyr::distinct(%s, .keep_all = TRUE)",
                                        paste(dis, collapse = ", ")),
               # the same rows as the data it is made from: the same levels --
               # but a dataset read as it is has none yet
               if (length(c(steps0, .split_bar(r$derive), dis, add)) ||
                   (is.null(pop) && !from_data)) lv)
    out <- c(out, .make_code(id, base, steps))
  }
  out
}
