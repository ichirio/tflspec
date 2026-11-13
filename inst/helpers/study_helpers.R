# The functions a study's generated programs call: the code lists on a
# data, the ids in front of an analysis's ARD, the formats after a call,
# the statistics asked for, one report's rows into the study ARD, and a
# figure's reading of an ARD (one statistic, statistics as a data frame,
# the fingerprint of what was built).
# Base R, cards and dplyr only: the programs run without tflspec.

# The code lists on a data: each listed column a factor whose levels are
# the CODES, in its list's order (a list with names: the names, the codes
# the data holds; a list without: the values themselves).  No labels are
# applied: the ARD holds what the ADaM data holds, and a code list's text
# reaches a table through the table plan (plan_labels()).  A level with no
# record stays, counted 0.  A list may be narrowed to the levels the data's
# filter leaves (cl_paramcd[c("ALT", "AST")]): set_levels() is the step
# after that filter.  A value its list does not have stops the program and
# says which (a missing value does not: it stays missing).  A blank text
# value (an XPT holds a missing text as a blank, whitespace only too) is a
# missing value: it becomes NA, is not reported, and is not a level, as in
# order_levels().
#   set_levels(adsl, SEX = c(F = "Female", M = "Male"), TRT01A = cl_trt01a)
set_levels <- function(data, ...) {
  lists <- list(...)
  for (v in intersect(names(lists), names(data))) {
    cl <- lists[[v]]
    values <- if (is.null(names(cl))) unname(cl) else names(cl)
    x <- as.character(data[[v]])
    x[!is.na(x) & !nzchar(trimws(x))] <- NA
    unknown <- setdiff(unique(x[!is.na(x)]), values)
    if (length(unknown)) {
      stop(sprintf("%s has values its code list does not: %s.  Add them to the list.",
                   v, paste0("\"", unknown, "\"", collapse = ", ")), call. = FALSE)
    }
    data[[v]] <- factor(x, levels = values)
  }
  data
}

# A column in another's order, when the data has it: reorder(factor(x), by)
# for a program's data step (PARAM by PARAMN, AVISIT by AVISITN).  An order
# is not a condition that defines the report, so a data without the order
# variable does not stop the program: the values stay in their own order
# (factor(x)), and a message says so.
#   mutate(adlb, PARAM = reorder_by(PARAM, PARAMN))
reorder_by <- function(x, by) {
  what <- deparse(substitute(by))
  by <- tryCatch(by, error = function(e) NULL)
  if (is.null(by)) {
    message(sprintf("reorder_by(): the data has no %s: the values in their own order.", what))
    return(factor(x))
  }
  stats::reorder(factor(x), by)
}

# The order of the arms and of the categories, as factor levels set in the
# data step, before the ARD: the display order, and a comparison's
# reference, come from them.  The variables are the arm variables (TRT...,
# ARM..., ACTARM...) and those `vars` names (the categories: "SEX",
# "RACE"); each is made a factor, its levels in the order of
#   1. its code list, given as `VAR = list` in `...` (set_levels() does it,
#      with its stop on a value the list does not have);
#   2. else its numeric pair, the name and N (TRT01A by TRT01AN, SEX by
#      SEXN, RACE by RACEN, AGEGR1 by AGEGR1N): the value of the pair's
#      lowest number first, equal numbers in the order the values come;
#   3. else the fallback, which is what the ARD does without this step: the
#      column is left as it is, and cards orders a text column's values by
#      order(x, method = "radix") -- the C locale (byte order: digits, <, >,
#      upper case, _, lower case), whatever the session's locale is.
# (A factor made of a column with no pair would count a level the analysis
# set has no record of, as 0: the fallback makes none.)
# A factor already in the data keeps its levels; a column that is not text
# (a number, a date) is left as it is; a variable the data does not have is
# skipped.  A blank (a subject of no arm, a category with no value, as an
# XPT holds a missing text) is missing, not a level.  A code list given later (set_levels())
# wins.
#   adsl |> order_levels()
#   adsl |> order_levels(c("AGEGR1", "SEX", "RACE"))
#   adsl |> order_levels(c("SEX", "RACE"), TRT01A = cl_trt01a)
# `from` (a data frame, ADSL): the arm of a data with no pair of its own (a
# records data without TRT01AN) is ordered by the matching arm of `from`,
# its values kept; a value `from` does not have goes last.
#   adae |> order_levels(from = adsl)
order_levels <- function(data, vars = NULL, from = NULL, ...) {
  lists <- list(...)
  if (length(lists)) data <- set_levels(data, ...)
  arms <- grep("^(TRT|ARM|ACTARM)", names(data), value = TRUE)
  arms <- arms[!grepl("N$", arms)]
  for (v in unique(c(arms, vars))) {
    if (!v %in% names(data) || v %in% names(lists) || is.factor(data[[v]])) next
    n <- paste0(v, "N")
    src <- data
    pair <- n
    if (!n %in% names(data) || !is.numeric(data[[n]])) {
      # the records' arm with no pair of its own: the order of the matching
      # arm of `from` (ADSL), by the values both have (TRTA by TRT01AN
      # through ADSL's TRT01A and TRT01AN); the records' values are kept
      if (!v %in% arms || !is.data.frame(from)) next
      x0 <- unique(as.character(data[[v]]))
      cand <- c(v, grep("^(TRT|ARM|ACTARM)", names(from), value = TRUE))
      cand <- cand[cand %in% names(from) & !grepl("N$", cand) &
                     paste0(cand, "N") %in% names(from)]
      cand <- Filter(function(cn) any(x0 %in% as.character(from[[cn]])), unique(cand))
      if (!length(cand)) next
      src <- from
      pair <- paste0(cand[1L], "N")
      key <- as.character(from[[cand[1L]]])
      # the map of the value to its number is one to one: a value with two
      # numbers has no order
      nn <- tapply(src[[pair]], key, function(z) length(unique(z[!is.na(z)])))
      if (any(nn[intersect(names(nn), x0)] > 1L, na.rm = TRUE)) {
        stop(sprintf("order_levels(): %s has values with more than one %s in `from`: their order is not defined.",
                     v, pair), call. = FALSE)
      }
    }
    if (!v %in% arms && !is.character(data[[v]])) next
    x <- as.character(data[[v]])
    x[!is.na(x) & !nzchar(trimws(x))] <- NA
    lv <- unique(x[!is.na(x)])
    if (identical(src, data)) key <- x
    first <- vapply(lv, function(l) suppressWarnings(min(src[[pair]][key %in% l], na.rm = TRUE)), 0)
    data[[v]] <- factor(x, levels = lv[order(first, match(lv, lv))])
  }
  data
}

# The arms in their numeric order when the data has it: each arm variable
# (TRT..., ARM..., ACTARM...) with the numeric pair of its name and N
# (TRT01A by TRT01AN, ARM by ARMN) is made a factor in that order, as a
# program's data step; one without its pair is left as it is.  A blank
# (a subject of no arm, as an XPT holds a missing text) is missing, not a
# level.  A code list set after wins.  order_levels() without categories.
#   adsl |> order_arms()
order_arms <- function(data) order_levels(data)

# The ids in front: the report, the analysis and its analysis set.  A cards
# ARD stays one (class card), so the study ARD is one too and cards' own
# tools (as_nested_list(), compare_ard()) take it.  Analyses run together
# (cards::ard_stack()): the rows of each one's variables (`analyses`) are
# its own, the rest (the by counts, the total N) `analysis_id`'s.
tag_ard <- function(ard, report_id, analysis_id, population = NA_character_,
                    analyses = NULL) {
  need <- c("variable", "stat_name", "stat")
  if (!is.data.frame(ard) || !all(need %in% names(ard))) {
    stop(sprintf(paste0("analysis %s: its code must return a cards ARD (columns variable, ",
                        "stat_name, stat ..., as ard_summary() or ard_tabulate() make); it returned %s."),
                 analysis_id, if (is.data.frame(ard)) {
                   sprintf("a data frame with columns %s", paste(names(ard), collapse = ", "))
                 } else paste("an object of class", class(ard)[1L])), call. = FALSE)
  }
  if (length(analyses)) {
    own <- rep(names(analyses), lengths(analyses))
    id <- own[match(as.character(ard$variable), unlist(analyses))]
    analysis_id <- ifelse(is.na(id), analysis_id, id)
  }
  if (inherits(ard, "card")) {
    return(dplyr::mutate(ard, output_id = report_id, analysis_id = analysis_id,
                         population_id = population, .before = 1L))
  }
  ard <- as.data.frame(ard)
  cbind(output_id = report_id, analysis_id = analysis_id,
        population_id = population, ard, stringsAsFactors = FALSE)
}

# A p-value as text: <0.001, else 3 decimals
fmt_pvalue <- function(x) ifelse(x < 0.001, "<0.001", sprintf("%.3f", x))

# The formats after the call, for what takes no fmt_fun (cardx's tests and
# models, a study's own functions, code, ard_stack()'s own rows): by
# statistic (mean) or variable and statistic ("AGE:sd"); the rows of the
# variables `skip` have theirs already.  A list of ARDs
# (cards::ard_pairwise(), one per pair of groups) becomes one, each row
# keeping its ARD's name as `pairwise`.
fmt_ard <- function(ard, formats = list(), skip = character()) {
  if (is.list(ard) && !is.data.frame(ard)) {
    ard <- dplyr::bind_rows(ard, .id = "pairwise")
  }
  if (!inherits(ard, "card")) return(ard)
  f <- formats[order(grepl(":", names(formats), fixed = TRUE))]
  for (k in names(f)) {
    s <- sub("^.*:", "", k)
    v <- if (grepl(":", k, fixed = TRUE)) sub(":.*$", "", k)
    rows <- ard$stat_name == s & (is.null(v) | ard$variable %in% v) &
      !ard$variable %in% skip
    if (!any(rows)) next
    ard <- cards::update_ard_fmt_fun(
      ard, variables = dplyr::all_of(unique(ard$variable[rows])),
      stat_names = s, fmt_fun = f[[k]])
  }
  # a statistic that is text (a test's method, a reference level) stays
  # text: cardx gives some of them a number of decimals
  # (ard_survival_survfit_diff()'s method and its factor reference_level)
  txt <- vapply(ard$stat, function(x) is.character(x) || is.factor(x), NA)
  if (any(txt)) ard$fmt_fun[txt] <- list(as.character)
  cards::apply_fmt_fun(ard)
}

# Only the statistics asked for, of a method that gives more (a list of
# ARDs: each of them)
keep_stats <- function(ard, stats) {
  if (is.list(ard) && !is.data.frame(ard)) return(lapply(ard, keep_stats, stats))
  ard[ard$stat_name %in% stats, , drop = FALSE]
}

# The rows of a group no record fell in dropped: a group a code list
# names that the data does not have (the baseline visit in the change's
# records, whose level the code list put back) has N = 0 and nothing else;
# its cells stay empty instead of printing 0.  The groups are the ARD's
# group columns together.
#   ard_summary(...) |> drop_empty_groups()
drop_empty_groups <- function(ard) {
  if (is.list(ard) && !is.data.frame(ard)) return(lapply(ard, drop_empty_groups))
  gl <- grep("^group[0-9]+_level$", names(ard), value = TRUE)
  if (!length(gl) || !"stat_name" %in% names(ard)) return(ard)
  key <- do.call(paste, c(lapply(ard[gl], function(v) vapply(v, function(x)
    if (is.null(x) || length(x) == 0L) "" else as.character(x)[1L], "")), sep = "\r"))
  n0 <- ard$stat_name == "N" & vapply(ard$stat, function(x) {
    if (!length(x)) return(FALSE)
    v <- suppressWarnings(as.numeric(x[[1L]]))
    length(v) == 1L && !is.na(v) && v == 0
  }, NA)
  empty <- unique(key[n0])
  ard[!key %in% empty, , drop = FALSE]
}

# The terms of a nested count (cards::ard_stack_hierarchical()) that `pct`
# % of subjects or more have, the other terms' rows dropped: in `met_in`
# "any" column (each arm, and the Total column when the ARD has one),
# "all" of them, or the "total" column alone (an ARD with no Total column
# stops).  The last level's rows (the PT) stand on their own percentage;
# a level above it (the SOC) stays when a term below it does.  The overall
# row and the by variable's counts stay.  (cards::filter_ard_hierarchical()
# drops the Total column's rows.)
#   ard |> keep_min_pct(5)
#   ard |> keep_min_pct(10, "all")
keep_min_pct <- function(ard, pct, met_in = c("any", "all", "total")) {
  met_in <- match.arg(met_in)
  one <- function(x) vapply(x, function(v)
    if (length(v) && !is.na(v[[1L]])) as.character(v[[1L]]) else NA_character_, "")
  term <- ard$context %in% "hierarchical" & !startsWith(as.character(ard$variable), "..")
  vars <- unique(as.character(ard$variable[term]))
  if (!any(term)) return(ard)
  gk <- grep("^group[0-9]+$", names(ard), value = TRUE)
  g <- lapply(gk, function(k) as.character(ard[[k]]))
  gl <- lapply(gk, function(k) one(ard[[paste0(k, "_level")]]))
  # a row's path (the levels it sits under, then its own) and its column (the
  # by group's level; none: the Total column)
  path <- rep("", nrow(ard))
  column <- rep("", nrow(ard))
  depth <- integer(nrow(ard))
  for (k in seq_along(gk)) {
    above <- g[[k]] %in% vars
    path[above] <- paste0(path[above], gl[[k]][above], "\r")
    depth <- depth + above
    by <- !is.na(g[[k]]) & !above
    column[by] <- gl[[k]][by]
  }
  path <- paste0(path, one(ard$variable_level))
  depth <- depth + 1L
  n <- length(vars)
  p <- vapply(ard$stat, function(s) if (length(s)) suppressWarnings(as.numeric(s[[1L]])) else NA_real_, 0)
  at <- term & ard$stat_name %in% "p" & !is.na(p)
  if (met_in == "total" && any(at) && !any(at & column == "")) {
    stop("keep_min_pct(): met_in = \"total\" needs the Total column, and the ARD has none: ",
         "add the Total column (ard_stack_hierarchical(overall = TRUE)), or use met_in = \"any\".",
         call. = FALSE)
  }
  # (the last level: its own percentage)
  own <- at & depth == n
  pass <- vapply(split(seq_len(nrow(ard))[own], path[own]), function(i) {
    v <- if (met_in == "total") p[i][column[i] == ""] else p[i]
    if (!length(v)) return(FALSE)
    if (met_in == "all") all(v >= pct / 100) else any(v >= pct / 100)
  }, NA)
  kept <- names(pass)[pass]
  # (a level above: when a term below it stays)
  up <- unique(path[term & depth < n])
  kept <- c(kept, up[vapply(up, function(u) any(startsWith(kept, paste0(u, "\r"))), NA)])
  ard[!term | path %in% kept, , drop = FALSE]
}

# An ARD's columns in their code lists' order: the rows of a listed
# variable (as `variable` or a group) get its list's levels (as the ARD has
# them, the labels).  For what does not keep its data's factor order --
# cardx's survfit makes the arms' factor again, in alphabetical order.
# Values and rows are as they were; a value the list does not have stays.
#   relevel_ard(ard, TRT01A = cl_trt01a)
relevel_ard <- function(ard, ...) {
  if (is.list(ard) && !is.data.frame(ard)) return(lapply(ard, relevel_ard, ...))
  lists <- list(...)
  cols <- c("variable", grep("^group[0-9]+$", names(ard), value = TRUE))
  for (k in cols) {
    kl <- if (k == "variable") "variable_level" else paste0(k, "_level")
    if (!kl %in% names(ard)) next
    for (v in intersect(names(lists), unique(ard[[k]]))) {
      levs <- unname(lists[[v]])
      on <- which(ard[[k]] %in% v)
      ard[[kl]][on] <- lapply(ard[[kl]][on], function(x) {
        if (length(x) != 1L || is.na(x) || !as.character(x) %in% levs) return(x)
        factor(as.character(x), levels = levs)
      })
    }
  }
  ard
}

# cards::ard_stack_hierarchical(), and the ARD of no records: cards stops
# on data with none ("all missing"), and a table of zero events is common.
# With none: the overall row (when over_variables = TRUE) n = 0 and p = 0
# with each group's N, and the by variable's counts (a header's N) -- no
# SOC / PT rows.  The same arguments as cards::ard_stack_hierarchical().
ard_hierarchical_or_none <- function(data, ...) {
  if (nrow(data)) {
    # a group (an arm) the records have none of: cards counts the groups of
    # the data, so the arm would have no column.  The denominator's groups
    # are added to the data's as levels no record has: the group's N, and
    # 0 events.
    cl <- match.call()
    den <- tryCatch(eval(cl$denominator, parent.frame()), error = function(e) NULL)
    if (is.data.frame(den)) {
      for (v in intersect(all.vars(cl$by), intersect(names(data), names(den)))) {
        lack <- setdiff(as.character(unique(den[[v]][!is.na(den[[v]])])),
                        c(as.character(data[[v]]), if (is.factor(data[[v]])) levels(data[[v]])))
        if (length(lack)) {
          lv <- if (is.factor(data[[v]])) levels(data[[v]]) else sort(unique(data[[v]]))
          # (in the denominator's order: the groups' order)
          all <- if (is.factor(den[[v]])) levels(den[[v]]) else sort(unique(den[[v]]))
          data[[v]] <- factor(data[[v]], levels = c(all[all %in% c(lv, lack)], setdiff(lv, all)))
        }
      }
    }
    return(cards::ard_stack_hierarchical(data, ...))
  }
  cl <- match.call()
  env <- parent.frame()
  den <- eval(cl$denominator, env)
  if (!is.data.frame(den) || !nrow(den)) {
    stop("ard_hierarchical_or_none(): no records, and no `denominator` data to count ",
         "the groups' N from.", call. = FALSE)
  }
  over <- isTRUE(eval(cl$over_variables, env))
  by <- all.vars(cl$by)
  # a stand-in record of the denominator for each group gives the ARD its
  # shape (every group's N)
  stand_in <- den[!duplicated(den[intersect(by, names(den))]), , drop = FALSE]
  for (v in all.vars(cl$variables)) stand_in[[v]] <- "(none)"
  e <- new.env(parent = env)
  e$.stand_in <- stand_in
  cl[[1L]] <- quote(cards::ard_stack_hierarchical)
  cl$data <- quote(.stand_in)
  cl$over_variables <- TRUE
  ard <- eval(cl, e)
  keep <- (over & ard$variable %in% "..ard_hierarchical_overall..") | ard$variable %in% by
  ard <- ard[keep, , drop = FALSE]
  ov <- ard$variable %in% "..ard_hierarchical_overall.."
  ard$stat[ov & ard$stat_name == "n"] <- list(0L)
  ard$stat[ov & ard$stat_name == "p"] <- list(0)
  ard
}

# A hierarchical ARD (cards::ard_stack_hierarchical(), SOC / PT) with its
# code lists' values no subject has, counted 0 -- cards counts only the
# values some group has.  The outermost variable: each level of its factor
# (its code list) without a row gets one in every group, n = 0 and p = 0,
# N the group's.  An inner variable's value: only where `under` names the
# value one level out it sits under (as they are in the ARD); one without
# is not added, and a message says which.  The rows keep the factors'
# levels: the table's order is the code lists'.
# Under an overall row of 0 (no subject has any event) nothing is added:
# such a table shows its overall row alone; `when_none = TRUE` adds the
# code lists' values all the same.
#   complete_hierarchy(ard, c("AEBODSYS", "AEDECOD"),
#                      under = list(AEDECOD = c("Blurred vision" = "EYE DISORDERS")))
complete_hierarchy <- function(ard, variables, under = list(), when_none = FALSE) {
  ov <- ard$variable %in% "..ard_hierarchical_overall.." & ard$stat_name %in% "n"
  none <- !any(ard$variable %in% variables & ard$context %in% "hierarchical") ||
    (any(ov) && all(suppressWarnings(as.numeric(unlist(ard$stat[ov]))) %in% 0))
  if (none && !isTRUE(when_none)) return(ard)
  hier <- ard[ard$variable %in% variables & ard$context %in% "hierarchical", , drop = FALSE]
  if (!nrow(hier)) return(ard)
  gcols <- grep("^group[0-9]+$", names(ard), value = TRUE)
  one <- function(x) if (is.null(x) || !length(x) || is.na(x[[1L]])) NA_character_ else as.character(x[[1L]])
  # a row's groups that are not the hierarchy's (the by columns), and its statistic
  key_of <- function(d) vapply(seq_len(nrow(d)), function(i) {
    g <- vapply(gcols, function(k) one(d[[k]][i]), "")
    gl <- vapply(gcols, function(k) one(d[[paste0(k, "_level")]][[i]]), "")
    on <- !is.na(g) & !g %in% variables
    paste(c(g[on], gl[on], d$stat_name[i]), collapse = "\r")
  }, "")
  as_level <- function(v, levels) if (is.null(levels)) v else factor(v, levels = levels)
  added <- list()
  for (d in seq_along(variables)) {
    v <- variables[d]
    rows <- hier[hier$variable == v, , drop = FALSE]
    if (!nrow(rows)) next
    lv <- levels(rows$variable_level[[1L]])
    seen <- unique(vapply(rows$variable_level, one, ""))
    parent <- if (d > 1L) under[[v]]
    if (d == 1L) {
      miss <- setdiff(lv, seen)
    } else {
      cand <- if (is.null(lv)) names(parent) else lv
      miss <- setdiff(cand, seen)
      lost <- setdiff(miss, names(parent))
      if (length(lost)) {
        message(sprintf("complete_hierarchy(): %s: %s not added (`under` does not say its %s).",
                        v, paste0("\"", lost, "\"", collapse = ", "), variables[d - 1L]))
      }
      miss <- intersect(miss, names(parent))
    }
    if (!length(miss)) next
    # a row of each group and statistic, as the rows of the new values
    tmpl <- rows[!duplicated(key_of(rows)), , drop = FALSE]
    for (m in miss) {
      r <- tmpl
      r$variable_level <- rep(list(as_level(m, lv)), nrow(r))
      if (d > 1L) {
        # its parent's row: the parent, and the parent's own levels out
        p <- unname(parent[[m]])
        prow <- hier[hier$variable == variables[d - 1L] &
                       vapply(hier$variable_level, one, "") %in% p, , drop = FALSE]
        if (!nrow(prow)) {
          message(sprintf("complete_hierarchy(): %s: \"%s\" not added (no %s \"%s\").",
                          v, m, variables[d - 1L], p))
          next
        }
        up <- list()
        for (k in gcols) {
          g <- one(prow[[k]][1L])
          if (!is.na(g) && g %in% variables) up[[g]] <- prow[[paste0(k, "_level")]][[1L]]
        }
        up[[variables[d - 1L]]] <- prow$variable_level[[1L]]
        for (k in gcols) {
          g <- vapply(r[[k]], one, "")
          on <- !is.na(g) & g %in% names(up)
          if (any(on)) r[[paste0(k, "_level")]][on] <- up[g[on]]
        }
      }
      r$stat[r$stat_name == "n"] <- list(0L)
      r$stat[r$stat_name == "p"] <- list(0)
      if ("stat_fmt" %in% names(r)) r$stat_fmt <- rep(list(NULL), nrow(r))
      added[[length(added) + 1L]] <- r
      hier <- dplyr::bind_rows(hier, r)
    }
  }
  if (!length(added)) return(ard)
  out <- dplyr::bind_rows(ard, added)
  class(out) <- class(ard)
  out
}

# One statistic of an ARD, as text: the row of the analysis, the variable
# (and its level), the statistic and the groups named (`TRT01A =
# "Placebo"`); exactly one row, or the program stops and says how many.
# The text is the ARD's own (stat_fmt), or the value with `digits`
# decimals; a missing value is `na`.
#   ard_value(ard, "KM", "prob", "estimate", TRT01A = "Placebo", level = 0.5)
ard_value <- function(ard, analysis_id, variable, stat, ..., level = NULL,
                      digits = NULL, na = "NE") {
  chr <- function(x) {
    if (!is.list(x)) return(as.character(x))
    vapply(x, function(v) if (length(v)) as.character(v[[1L]]) else NA_character_, "")
  }
  k <- ard$analysis_id %in% analysis_id & ard$variable %in% variable &
    ard$stat_name %in% stat
  if (!is.null(level)) k <- k & chr(ard$variable_level) %in% as.character(level)
  groups <- list(...)
  for (g in names(groups)) {
    hit <- rep(FALSE, nrow(ard))
    for (col in grep("^group[0-9]+$", names(ard), value = TRUE)) {
      hit <- hit | (chr(ard[[col]]) %in% g &
                      chr(ard[[paste0(col, "_level")]]) %in% as.character(groups[[g]]))
    }
    k <- k & hit
  }
  if (sum(k) != 1L) {
    what <- paste(c(analysis_id, variable, if (!is.null(level)) paste("level", level), stat,
                    if (length(groups)) paste0(names(groups), " = ", unlist(groups))),
                  collapse = ", ")
    stop(sprintf("ard_value(): %d rows of the ARD are %s, not one.", sum(k), what),
         call. = FALSE)
  }
  i <- which(k)
  v <- if (is.list(ard$stat)) ard$stat[[i]] else ard$stat[i]
  if (!length(v) || is.na(v[[1L]])) return(na)
  if (!is.null(digits)) return(formatC(as.numeric(v[[1L]]), format = "f", digits = digits))
  f <- if ("stat_fmt" %in% names(ard)) {
    if (is.list(ard$stat_fmt)) ard$stat_fmt[[i]] else ard$stat_fmt[i]
  }
  if (!length(f) || is.na(f[[1L]])) as.character(v[[1L]]) else as.character(f[[1L]])
}

# Statistics of an ARD as a data frame: a row per group (and level of the
# variable), a column per statistic (its value).  `by` names the group
# columns kept (by their variables' names: TRT01A); the variable's levels
# are a column of the variable's name; several analyses or variables add
# their columns `analysis_id`, `variable`.
#   ard_stats(ard, "KM", "time", "n.risk", by = "TRT01A")  # TRT01A, time, n.risk
ard_stats <- function(ard, analysis_id, variable = NULL, stats, by = NULL) {
  chr <- function(x) {
    if (!is.list(x)) return(as.character(x))
    vapply(x, function(v) if (length(v)) as.character(v[[1L]]) else NA_character_, "")
  }
  num <- function(x) suppressWarnings(as.numeric(chr(x)))
  k <- ard$analysis_id %in% analysis_id & ard$stat_name %in% stats
  if (!is.null(variable)) k <- k & ard$variable %in% variable
  a <- ard[k, , drop = FALSE]
  if (!nrow(a)) {
    stop(sprintf("ard_stats(): the ARD has no rows of %s, %s.",
                 paste(analysis_id, collapse = ", "), paste(stats, collapse = ", ")),
         call. = FALSE)
  }
  gcols <- grep("^group[0-9]+$", names(a), value = TRUE)
  if (is.null(by)) by <- unique(stats::na.omit(unlist(lapply(gcols, function(g) chr(a[[g]])))))
  keys <- data.frame(row.names = seq_len(nrow(a)))
  if (length(unique(a$analysis_id)) > 1L) keys$analysis_id <- a$analysis_id
  for (g in by) {
    v <- rep(NA_character_, nrow(a))
    for (col in gcols) {
      at <- chr(a[[col]]) %in% g
      v[at] <- chr(a[[paste0(col, "_level")]])[at]
    }
    keys[[g]] <- v
  }
  vars <- unique(a$variable)
  if (length(vars) > 1L) keys$variable <- a$variable
  if ("variable_level" %in% names(a)) {
    lv <- chr(a$variable_level)
    if (!all(is.na(lv))) {
      nm <- if (length(vars) == 1L) vars else "level"
      keys[[nm]] <- if (all(is.na(lv) | !is.na(num(a$variable_level)))) num(a$variable_level) else lv
    }
  }
  id <- do.call(paste, c(lapply(keys, as.character), list(sep = "\r")))
  if (!ncol(keys)) id <- rep("", nrow(a))
  first <- !duplicated(id)
  out <- keys[first, , drop = FALSE]
  for (s in stats) {
    at <- a$stat_name == s
    out[[s]] <- num(a$stat)[at][match(id[first], id[at])]
  }
  rownames(out) <- NULL
  out
}

# The fingerprint of a report's ARD definition as ard_status.csv recorded it
# when its rows were built ("" when none were)
ard_fingerprint <- function(report_id, path = "output/ard/ard_status.csv") {
  if (!file.exists(path)) return("")
  st <- utils::read.csv(path, colClasses = "character")
  d <- st$definition[st$output_id == report_id]
  if (!length(d) || is.na(d[1L])) "" else d[1L]
}

# One report's rows into the study ARD (the other reports' left as they
# are), and what was built in ard_status.csv next to it: the report, its
# definition's fingerprint, when, how many rows, and the md5 of the files
# `sources` names (the study's setup: option tflspec.ard_sources)
save_ard <- function(ard, report_id, definition = NA_character_,
                     path = "output/ard/ard.rds",
                     sources = getOption("tflspec.ard_sources")) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  old <- if (file.exists(path)) readRDS(path)
  new <- if (is.null(old)) ard else
    dplyr::bind_rows(old[old$output_id != report_id, , drop = FALSE], ard)
  tmp <- paste0(path, ".tmp")
  saveRDS(new, tmp)
  file.rename(tmp, path)
  sf <- file.path(dirname(path), "ard_status.csv")
  row <- data.frame(output_id = report_id, definition = definition,
                    built = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                    rows = as.character(nrow(ard)), error = "",
                    stringsAsFactors = FALSE)
  for (k in names(sources)) {
    row[[k]] <- if (file.exists(sources[[k]])) unname(tools::md5sum(sources[[k]])) else ""
  }
  st <- if (file.exists(sf)) utils::read.csv(sf, colClasses = "character")
  if (!is.null(st) && nrow(st)) {
    for (k in setdiff(names(row), names(st))) st[[k]] <- ""
    row <- rbind(st[st$output_id != report_id, names(row), drop = FALSE], row)
  }
  utils::write.csv(row, sf, row.names = FALSE)
  cat(sprintf("%s: %d rows into %s\n", report_id, nrow(ard), path))
  invisible(ard)
}
