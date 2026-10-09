# The functions a study's generated programs call: the code lists on a
# data, the ids in front of an analysis's ARD, the formats after a call,
# the statistics asked for, one report's rows into the study ARD, and a
# figure's reading of an ARD (one statistic, statistics as a data frame,
# the fingerprint of what was built).
# Base R, cards and dplyr only: the programs run without tflspec.

# The code lists on a data: each listed column a factor, in its list's
# order, each value as the list has it in the ARD (a list without names:
# the values themselves).  A value its list does not have stops the
# program and says which (a missing value does not: it stays missing).
#   set_levels(adsl, SEX = c(F = "Female", M = "Male"), TRT01A = cl_trt01a)
set_levels <- function(data, ...) {
  lists <- list(...)
  for (v in intersect(names(lists), names(data))) {
    cl <- lists[[v]]
    values <- if (is.null(names(cl))) unname(cl) else names(cl)
    x <- as.character(data[[v]])
    unknown <- setdiff(unique(x[!is.na(x)]), values)
    if (length(unknown)) {
      stop(sprintf("%s has values its code list does not: %s.  Add them to the list.",
                   v, paste0("\"", unknown, "\"", collapse = ", ")), call. = FALSE)
    }
    data[[v]] <- factor(x, levels = values, labels = unname(cl))
  }
  data
}

# The ids in front: the report, the analysis and its analysis set.  A cards
# ARD stays one (class card), so the study ARD is one too and cards' own
# tools (as_nested_list(), compare_ard()) take it.  Analyses run together
# (cards::ard_stack()): the rows of each one's variables (`analyses`) are
# its own, the rest (the by counts, the total N) `analysis_id`'s.
tag_ard <- function(ard, report_id, analysis_id, population = NA_character_,
                    analyses = NULL) {
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
  cards::apply_fmt_fun(ard)
}

# Only the statistics asked for, of a method that gives more (a list of
# ARDs: each of them)
keep_stats <- function(ard, stats) {
  if (is.list(ard) && !is.data.frame(ard)) return(lapply(ard, keep_stats, stats))
  ard[ard$stat_name %in% stats, , drop = FALSE]
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
