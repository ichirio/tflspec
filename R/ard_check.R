# ============================================================================
#  Is this an ARD a report can be made from?
# ----------------------------------------------------------------------------
#  An ARD made outside the study's own ARD definition -- by another program,
#  a CRO, another tool -- is checked when it is taken in: its shape as a
#  cards ARD, and, given the report's table definition, whether it has what
#  the table reads (the column and row keys, the variables, the statistics
#  the templates name).  The answer is a list of problems, so a GUI can show
#  them and a program can stop on them.
# ============================================================================

#' Check an ARD before a report uses it
#'
#' For an ARD made elsewhere and taken in (a report's `ard_source` is
#' `import:<file>`).  Checks, each a row of the result:
#'
#' * **shape**: the columns every ARD has (`variable`, `stat_name`, `stat`);
#'   with cards installed, its own [cards::check_ard_structure()] (as notes,
#'   less its wish for `method` rows, which no report reads); an old column
#'   name (`fmt_fn`, now `fmt_fun`).  A `groupN` without `groupN_level` is
#'   a result across the groups (a test), as cardx gives it.
#' * with `spec`, the report's table definition: the columns its `tables`
#'   roles name (`cols`, `rows`) are in the ARD (as a group or a variable),
#'   the variables its `cells` name are analysed, and the statistics its
#'   templates read (`{mean}`, `{n:d}`) are there.
#'
#' @param ard The ARD (a data frame or a cards ARD), e.g. [tfl_read_ard()].
#' @param spec Optional: the report's table definition ([tfl_read_table_spec()]),
#'   narrowed to the report or with `output_id`.
#' @param output_id The report, when `spec` defines several.
#' @return A data frame, one row per problem: `level` (`"error"`: the report
#'   cannot be made; `"warning"`: something it reads is missing; `"note"`),
#'   `check` and `message`.  No rows: nothing found.
#' @seealso [tfl_read_ard()], [tfl_write_ard()]
#' @export
tfl_check_ard <- function(ard, spec = NULL, output_id = NULL) {
  out <- data.frame(level = character(), check = character(),
                    message = character(), stringsAsFactors = FALSE)
  add <- function(level, check, message) {
    out[nrow(out) + 1L, ] <<- list(level, check, message)
  }
  if (!is.data.frame(ard)) {
    add("error", "shape", sprintf("not a data frame but %s", .what(ard)))
    return(out)
  }
  need <- setdiff(c("variable", "stat_name", "stat"), names(ard))
  if (length(need)) {
    add("error", "shape", sprintf("no column %s: not an ARD",
                                  paste(need, collapse = ", ")))
    return(out)
  }
  # a groupN with no groupN_level is a result across the groups (a test:
  # cardx's own ard_stats_*() give that shape), not a broken ARD
  g <- grep("^group[0-9]+$", names(ard), value = TRUE)
  if ("fmt_fn" %in% names(ard) && !"fmt_fun" %in% names(ard)) {
    add("note", "shape", "the column fmt_fn has cards' old name (fmt_fun since cards 0.6.1)")
  }
  if (!nrow(ard)) add("warning", "shape", "the ARD has no rows")
  if (requireNamespace("cards", quietly = TRUE)) {
    x <- tryCatch(cards::as_card(as.data.frame(ard), check = FALSE),
                  error = function(e) NULL)
    if (!is.null(x)) {
      msg <- character()
      withCallingHandlers(
        tryCatch(cards::check_ard_structure(x), error = function(e)
          msg <<- c(msg, conditionMessage(e))),
        message = function(m) {
          msg <<- c(msg, trimws(conditionMessage(m)))
          invokeRestart("muffleMessage")
        })
      # cards asks for the `method` rows its own ard_*() add: a report
      # never reads them, and an ARD written and read back has none
      msg <- msg[nzchar(msg) & !grepl("tidy_ard_column_order", msg, fixed = TRUE) &
                   !grepl("stat_name = 'method'", msg, fixed = TRUE)]
      for (m in unique(msg)) add("note", "cards", m)
    }
  }
  if (is.null(spec)) return(out)

  sp <- .ard_spec_scope(spec, output_id)
  have_vars <- unique(as.character(unlist(ard$variable)))
  have_groups <- unique(unlist(lapply(g, function(k) as.character(unlist(ard[[k]])))))
  have_stats <- unique(as.character(unlist(ard$stat_name)))
  # the roles: `KEY` or `role = KEY`, | between them
  role_cols <- function(x) {
    p <- .split_bar(x)
    trimws(sub("^.*=", "", p))
  }
  t <- sp$tables
  if (nrow(t)) {
    for (role in c("cols", "rows")) {
      if (!role %in% names(t)) next
      for (k in role_cols(t[[role]][1L])) {
        if (k %in% c("variable", "label", "variable_level", "context",
                     "stat_name")) next
        if (!k %in% c(have_groups, have_vars)) {
          add("warning", role, sprintf(
            "the table's %s name %s, which is neither a group nor a variable of the ARD",
            role, k))
        }
      }
    }
  }
  ce <- sp$cells
  if (nrow(ce)) {
    vv <- setdiff(stats::na.omit(unique(ce$variable)),
                  c("continuous", "categorical"))
    for (v in setdiff(vv, have_vars)) {
      add("warning", "cells", sprintf(
        "the cells are written for %s, which the ARD does not analyse", v))
    }
    tok <- unlist(regmatches(ce$template, gregexpr("\\{[^{}]+\\}", ce$template)))
    st <- unique(sub("[:}].*$", "", sub("^\\{", "", tok)))
    for (s in setdiff(st, have_stats)) {
      add("warning", "statistics", sprintf(
        "a template reads {%s}, a statistic the ARD does not have", s))
    }
  }
  out
}


#' What went wrong while an ARD was made
#'
#' cards does not stop when one analysis fails: a test that cannot be run
#' (two groups needed, three given), a statistic whose function stops or
#' warns, leaves its message in the ARD's `error` / `warning` column and the
#' other analyses carry on.  `tfl_ard_conditions()` lists those messages,
#' one row per analysis, variable, groups and message, so a screen can show
#' them and a program can stop on them.  It is what
#' [cards::print_ard_conditions()] prints, as a table, with the study ARD's
#' `output_id` and `analysis_id`.
#'
#' @param ard An ARD: the study's ([tfl_build_ard()]), one report's
#'   ([tfl_ard_for()]), or any cards ARD.
#' @return A data frame: `output_id` and `analysis_id` (when the ARD has
#'   them), `variable`, `groups` (the group columns, `|` between them; empty
#'   for none), `level` (`"error"` or `"warning"`), `message`, and
#'   `statistics` (the statistics it is about, `, ` between them).  Errors
#'   first.  No rows: nothing went wrong.
#' @seealso [tfl_check_ard()] for whether a report can be made from an ARD.
#' @examples
#' if (requireNamespace("cards", quietly = TRUE)) {
#'   ard <- cards::ard_summary(cards::ADSL, variables = AGE,
#'     statistic = ~ list(mean = mean, bad = function(x) stop("no data")))
#'   tfl_ard_conditions(ard)
#' }
#' @export
tfl_ard_conditions <- function(ard) {
  ids <- intersect(c("output_id", "analysis_id"), names(ard))
  out <- data.frame(stringsAsFactors = FALSE,
                    matrix(character(), 0L, length(ids) + 5L,
                           dimnames = list(NULL, c(ids, "variable", "groups",
                                                   "level", "message",
                                                   "statistics"))))
  if (!is.data.frame(ard) || !nrow(ard)) return(out)
  g <- grep("^group[0-9]+$", names(ard), value = TRUE)
  groups <- if (length(g)) {
    vapply(seq_len(nrow(ard)), function(i) {
      v <- vapply(g, function(k) {
        x <- unlist(ard[[k]][i])
        if (length(x) && !is.na(x[1L])) as.character(x[1L]) else NA_character_
      }, "")
      paste(stats::na.omit(v), collapse = " | ")
    }, "")
  } else rep("", nrow(ard))
  key <- function(col, i) {
    if (!col %in% names(ard)) return(NA_character_)
    x <- unlist(ard[[col]][i])
    if (length(x)) as.character(x[1L]) else NA_character_
  }
  rows <- list()
  for (level in c("error", "warning")) {
    if (!level %in% names(ard)) next
    msgs <- ard[[level]]
    for (i in seq_len(nrow(ard))) {
      m <- unlist(msgs[i])
      m <- m[!is.na(m) & nzchar(m)]
      for (one in unique(m)) {
        rows[[length(rows) + 1L]] <- c(
          stats::setNames(vapply(ids, key, "", i = i), ids),
          variable = key("variable", i), groups = groups[i], level = level,
          message = one, stat = key("stat_name", i))
      }
    }
  }
  if (!length(rows)) return(out)
  d <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  by <- c(ids, "variable", "groups", "level", "message")
  d$.k <- do.call(paste, c(lapply(by, function(k) d[[k]]), sep = "\r"))
  first <- !duplicated(d$.k)
  res <- d[first, by, drop = FALSE]
  res$statistics <- vapply(d$.k[first], function(k)
    paste(unique(stats::na.omit(d$stat[d$.k == k])), collapse = ", "), "",
    USE.NAMES = FALSE)
  rownames(res) <- NULL
  res
}
