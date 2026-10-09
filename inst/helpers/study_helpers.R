# The functions a study's generated programs call: the code lists on a
# data, the ids in front of an analysis's ARD, the formats after a call,
# the statistics asked for, and one report's rows into the study ARD.
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
