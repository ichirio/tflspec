# ============================================================================
#  What the data say, kept small: the facts the review checks against
# ----------------------------------------------------------------------------
#  Reading a study's datasets is what costs (seconds for a big file); the
#  review needs only what they hold -- each column's name, class and
#  values, how many subjects each analysis set keeps, how many rows each
#  condition keeps.  tfl_data_facts() reads the frames once and keeps those
#  facts, so a review can be run again (after every edit) without the data;
#  a program keeps them as long as the files do not change.
# ============================================================================

#' What the data hold, for the review
#'
#' Reads the datasets once and keeps the facts [tfl_review_spec()] checks
#' a definition against: every column's class, label, number of distinct
#' values and, for a character or factor column with at most `max_levels`
#' of them, the values; each dataset's rows and subjects; each analysis
#' set's subjects and the values of its flag; and each condition's rows and
#' subjects (or why it cannot be evaluated).  The facts are small (a few KB
#' a dataset) and hold no records, so they can be kept between sessions.
#'
#' @param data The datasets: a named list of data frames (names as the
#'   ARD definition's `datasets` sheet gives them: `ADSL`, `ADAE` ...).
#' @param populations The analysis sets: the `populations` sheet
#'   (`population_id`, `dataset`, `where`), or a whole ARD definition
#'   ([tfl_ard_spec()] or its sheets), whose analysis data and analyses
#'   then give the conditions too.
#' @param conditions More conditions to count: a data frame with `dataset`,
#'   `where` and optionally `population_id` (the analysis set the rows are
#'   first narrowed to).  The ARD definition's own are added when
#'   `populations` is one.
#' @param max_levels A column with more distinct values than this keeps only
#'   their number (200, as many as a condition builder offers).
#' @param listings A listing definition ([tfl_listing_spec()]): its `where`
#'   are counted too.
#' @param id The subject key.
#' @return A `tfl_data_facts`: a list of `datasets` (each `n`,
#'   `n_subjects` and `columns`, a data frame with `name`, `class`,
#'   `label`, `n_distinct` and the list column `values`), `populations`
#'   (each `dataset`, `n_subjects`, `flag`, `flag_values` and the values of
#'   the columns the definition reads among its subjects, `values`),
#'   `conditions` (a data frame: `dataset`, `population_id`, `where`,
#'   `n_rows`, `n_subjects`, `error`, and the list column `values`), `ard`
#'   (empty: what an ARD holds, [tfl_ard_facts()], is added by name) and
#'   `made` (the time).
#' @seealso [tfl_review_spec()], [tfl_ard_facts()]
#' @examples
#' adsl <- data.frame(USUBJID = c("1", "2", "3"), SAFFL = c("Y", "Y", "N"),
#'                    SEX = c("F", "M", "F"), AGE = c(50, 61, 47))
#' f <- tfl_data_facts(list(ADSL = adsl),
#'   populations = data.frame(population_id = "SAF", dataset = "ADSL",
#'                            where = 'SAFFL == "Y"'))
#' f$populations$SAF$n_subjects
#' f$datasets$ADSL$columns[, c("name", "class", "n_distinct")]
#' @export
tfl_data_facts <- function(data, populations = NULL, conditions = NULL,
                           max_levels = 200L, listings = NULL, id = "USUBJID") {
  if (!is.list(data) || is.data.frame(data) || is.null(names(data)) ||
      !all(nzchar(names(data))) ||
      !all(vapply(data, is.data.frame, NA))) {
    .ard_stop("tfl_data_facts(): `data` must be a named list of data frames.")
  }
  names(data) <- toupper(names(data))
  ard <- NULL
  if (inherits(populations, "tfl_ard_spec") ||
      (is.list(populations) && !is.data.frame(populations) &&
       !is.null(populations$populations))) {
    ard <- .ard_spec_shaped(populations)
    populations <- ard$populations
  }
  pops <- if (is.null(populations)) data.frame() else
    as.data.frame(populations, stringsAsFactors = FALSE)
  # the columns whose values are kept among each set's subjects and in each
  # condition's rows: those the definition reads
  read <- if (!is.null(ard)) .ard_read_vars(ard$analyses) else character()
  ds_facts <- lapply(data, .dataset_facts, max_levels = max_levels, id = id)
  pop_facts <- list()
  pop_subj <- list()
  for (i in seq_len(nrow(pops))) {
    pid <- pops$population_id[i]
    if (is.na(pid)) next
    ds <- toupper(.na_or(pops$dataset[i], ""))
    d <- data[[ds]]
    f <- list(dataset = ds, n_subjects = NA_integer_, flag = NA_character_,
              flag_values = NULL, error = NA_character_, values = list())
    flag <- .where_flag(pops$where[i])
    if (!is.null(d)) {
      if (!is.na(flag) && flag %in% names(d)) {
        f$flag <- flag
        f$flag_values <- sort(unique(as.character(d[[flag]])), na.last = TRUE)
      }
      r <- .facts_filter(d, pops$where[i])
      if (!is.null(r$keep)) {
        kept <- d[r$keep, , drop = FALSE]
        f$n_subjects <- .n_subjects(kept, id)
        pop_subj[[pid]] <- if (id %in% names(kept)) unique(kept[[id]])
        f$values <- .facts_values(kept, read, max_levels)
      } else if (!is.null(r$error)) {
        f$error <- r$error
      }
    }
    pop_facts[[pid]] <- f
  }
  cond <- rbind(.facts_conditions_of(ard, listings),
                .facts_conditions_df(conditions))
  cond <- cond[!duplicated(cond[c("dataset", "population_id", "where")]), ,
               drop = FALSE]
  cond_out <- data.frame(dataset = cond$dataset,
                         population_id = cond$population_id,
                         where = cond$where,
                         n_rows = rep(NA_integer_, nrow(cond)),
                         n_subjects = rep(NA_integer_, nrow(cond)),
                         error = rep(NA_character_, nrow(cond)),
                         stringsAsFactors = FALSE)
  cond_out$values <- rep(list(list()), nrow(cond))
  for (i in seq_len(nrow(cond))) {
    d <- data[[toupper(cond$dataset[i])]]
    if (is.null(d)) next
    p <- cond$population_id[i]
    if (!is.na(p)) {
      s <- pop_subj[[p]]
      if (is.null(s) || !id %in% names(d)) next
      d <- d[d[[id]] %in% s, , drop = FALSE]
    }
    r <- .facts_filter(d, cond$where[i])
    if (!is.null(r$error)) cond_out$error[i] <- r$error
    if (is.null(r$keep)) next
    kept <- d[r$keep, , drop = FALSE]
    cond_out$n_rows[i] <- nrow(kept)
    cond_out$n_subjects[i] <- .n_subjects(kept, id)
    cond_out$values[[i]] <- .facts_values(kept, read, max_levels)
  }
  structure(list(datasets = ds_facts, populations = pop_facts,
                 conditions = cond_out, ard = list(), id = id,
                 max_levels = as.integer(max_levels), made = Sys.time()),
            class = "tfl_data_facts")
}

#' @export
print.tfl_data_facts <- function(x, ...) {
  cat("<tfl_data_facts> made ", format(x$made, "%Y-%m-%d %H:%M"), "\n", sep = "")
  for (ds in names(x$datasets)) {
    f <- x$datasets[[ds]]
    cat(sprintf("  %-8s %6d rows %5s subjects %4d columns\n", ds, f$n,
                if (is.na(f$n_subjects)) "-" else format(f$n_subjects),
                nrow(f$columns)))
  }
  for (p in names(x$populations)) {
    f <- x$populations[[p]]
    cat(sprintf("  set %-8s %5s subjects\n", p,
                if (is.na(f$n_subjects)) "-" else format(f$n_subjects)))
  }
  if (nrow(x$conditions)) cat(sprintf("  %d conditions\n", nrow(x$conditions)))
  if (length(x$ard)) cat(sprintf("  ARD of %s\n", paste(names(x$ard), collapse = ", ")))
  invisible(x)
}

# One dataset's facts
.dataset_facts <- function(d, max_levels, id) {
  cls <- vapply(d, .facts_class, "")
  lab <- vapply(d, function(v) {
    l <- attr(v, "label")
    if (is.character(l) && length(l) == 1L) l else NA_character_
  }, "")
  nd <- vapply(d, function(v) length(unique(v[!is.na(v)])), 1L)
  vals <- lapply(seq_along(d), function(j) {
    if (!cls[j] %in% c("character", "factor") || nd[j] > max_levels) return(NULL)
    .facts_distinct(d[[j]])
  })
  cols <- data.frame(name = names(d), class = unname(cls), label = unname(lab),
                     n_distinct = unname(nd), stringsAsFactors = FALSE)
  cols$values <- vals
  list(n = nrow(d), n_subjects = .n_subjects(d, id), columns = cols)
}

.facts_class <- function(v) {
  if (is.factor(v)) return("factor")
  if (inherits(v, c("Date", "POSIXt"))) return("Date")
  if (is.logical(v)) return("logical")
  if (is.numeric(v)) return("numeric")
  if (is.character(v)) return("character")
  class(v)[1L]
}

.facts_distinct <- function(v) {
  u <- if (is.factor(v)) union(levels(v), unique(as.character(v))) else
    unique(as.character(v))
  u <- u[!is.na(u)]
  sort(u)
}

.n_subjects <- function(d, id) {
  if (!id %in% names(d)) return(NA_integer_)
  length(unique(d[[id]]))
}

# the distinct values of the columns `cols` among the rows `d` (NULL for a
# column with more than `max_levels`, or not character / factor)
.facts_values <- function(d, cols, max_levels) {
  cols <- intersect(cols, names(d))
  out <- lapply(cols, function(cn) {
    v <- d[[cn]]
    if (!is.character(v) && !is.factor(v)) return(NULL)
    u <- .facts_distinct(v)
    if (length(u) > max_levels) NULL else u
  })
  stats::setNames(out, cols)
}

# a condition's rows of `d`: list(keep = logical) or list(error = text)
.facts_filter <- function(d, where) {
  if (is.null(where) || is.na(where) || !nzchar(trimws(where))) {
    return(list(keep = rep(TRUE, nrow(d))))
  }
  e <- tryCatch(str2lang(where), error = function(e) e)
  if (inherits(e, "error")) return(list(error = "it does not read as R"))
  miss <- setdiff(all.vars(e), names(d))
  if (length(miss)) {
    return(list(error = sprintf("no column %s", paste(miss, collapse = ", "))))
  }
  v <- tryCatch(eval(e, d, baseenv()), error = function(e) e)
  # a function of a package the condition does not name: not counted here
  # (the program attaches it), not wrong
  if (inherits(v, "error") && grepl("could not find function", conditionMessage(v))) {
    return(list(unknown = TRUE))
  }
  if (inherits(v, "error")) return(list(error = conditionMessage(v)))
  if (!is.logical(v) || !length(v) %in% c(1L, nrow(d))) {
    return(list(error = "it does not give TRUE / FALSE for each row"))
  }
  list(keep = rep_len(!is.na(v) & v, nrow(d)))
}

# The flag a population's condition is: `SAFFL == "Y"` -> SAFFL
.where_flag <- function(where) {
  if (is.null(where) || is.na(where)) return(NA_character_)
  e <- tryCatch(str2lang(where), error = function(e) NULL)
  if (is.call(e) && identical(e[[1L]], as.name("==")) && is.name(e[[2L]]) &&
      is.character(e[[3L]]) && identical(toupper(e[[3L]]), "Y")) {
    return(as.character(e[[2L]]))
  }
  NA_character_
}

.facts_conditions_df <- function(x) {
  empty <- data.frame(dataset = character(), population_id = character(),
                      where = character(), stringsAsFactors = FALSE)
  if (is.null(x) || !nrow(x)) return(empty)
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  data.frame(dataset = toupper(as.character(x$dataset)),
             population_id = as.character(x$population_id %||%
                                          rep(NA_character_, nrow(x))),
             where = as.character(x$where), stringsAsFactors = FALSE)
}

# The conditions a definition asks for: each analysis's data (its dataset,
# analysis set and every `where` on the way) and each listing's `where`
.facts_conditions_of <- function(ard, listings) {
  out <- .facts_conditions_df(NULL)
  if (!is.null(ard)) {
    a <- .review_flat(ard$analyses)
    for (i in seq_len(nrow(a))) {
      s <- .review_source(ard, a, i)
      if (is.null(s) || is.na(s$dataset)) next
      out[nrow(out) + 1L, ] <- list(s$dataset, s$population_id, s$where)
    }
  }
  if (!is.null(listings)) {
    l <- listings$listings
    for (i in seq_len(NROW(l))) {
      if (is.na(l$dataset[i])) next
      out[nrow(out) + 1L, ] <- list(toupper(l$dataset[i]), NA_character_,
                                    l$where[i])
    }
  }
  out
}

#' What an ARD holds, for the review
#'
#' The groups (and their levels), the variables (their levels, statistics
#' and contexts) and the statistics of an ARD, and what went wrong while it
#' was made ([tfl_ard_conditions()]): what [tfl_review_spec()] checks a
#' report's table definition against when the report's ARD exists.  Put a
#' report's facts in `facts$ard[[output_id]]`.
#'
#' @param ard An ARD (a cards ARD or a data frame with its columns).
#' @return A list: `groups` (a named list: each group variable's levels),
#'   `variables` (a named list: each variable's `levels`, `stats` and
#'   `contexts`), `stats` (every statistic) and `conditions`.
#' @seealso [tfl_data_facts()], [tfl_check_ard()]
#' @examples
#' if (requireNamespace("cards", quietly = TRUE)) {
#'   ard <- cards::ard_summary(cards::ADSL, by = ARM, variables = AGE)
#'   str(tfl_ard_facts(ard)$variables$AGE$stats)
#' }
#' @export
tfl_ard_facts <- function(ard) {
  ard <- as.data.frame(ard, stringsAsFactors = FALSE)
  chr <- function(col) {
    if (!col %in% names(ard)) return(rep(NA_character_, nrow(ard)))
    vapply(ard[[col]], function(x) {
      x <- unlist(x)
      if (length(x) && !is.na(x[1L])) as.character(x[1L]) else NA_character_
    }, "", USE.NAMES = FALSE)
  }
  g <- grep("^group[0-9]+$", names(ard), value = TRUE)
  groups <- list()
  for (k in g) {
    var <- chr(k)
    lev <- chr(paste0(k, "_level"))
    for (v in unique(stats::na.omit(var))) {
      groups[[v]] <- unique(c(groups[[v]], stats::na.omit(lev[var %in% v])))
    }
  }
  var <- chr("variable")
  lev <- chr("variable_level")
  st <- chr("stat_name")
  ctx <- chr("context")
  vars <- list()
  for (v in unique(stats::na.omit(var))) {
    i <- var %in% v
    vars[[v]] <- list(levels = unique(stats::na.omit(lev[i])),
                      stats = unique(stats::na.omit(st[i])),
                      contexts = unique(stats::na.omit(ctx[i])))
  }
  list(groups = groups, variables = vars, stats = unique(stats::na.omit(st)),
       conditions = tryCatch(tfl_ard_conditions(ard), error = function(e) NULL))
}
