# The functions an ARD program calls (tfl_ard_code() writes the program
# with them): its data's code lists, the ids in front, the formats, the
# statistics asked for, and the report's rows into the study ARD

#' Columns as factors in the order of a report's code lists
#'
#' Each column named in the code lists becomes a factor, its levels in the
#' list's order; a value the list does not have comes after them
#' (alphabetically), so no record is lost.  The ARD then keeps the order and
#' counts a level no record has (`n = 0`).  A column that is neither
#' character nor factor, and a list of a column the data does not have, are
#' left alone.
#'
#' @param data A data frame.
#' @param ... The code lists: `VARIABLE = c("level", ...)`, or a named
#'   list of them (`codelists`).
#' @return `data`, the listed columns factors.
#' @examples
#' d <- data.frame(SEX = c("M", "F", "U"))
#' levels(set_levels(d, SEX = c("F", "M"))$SEX)
#' @export
set_levels <- function(data, ...) {
  cl <- list(...)
  nm <- names(cl) %||% rep("", length(cl))
  cl <- do.call(c, lapply(seq_along(cl), function(i)
    if (nzchar(nm[i])) cl[i] else as.list(cl[[i]])))
  for (v in intersect(names(cl), names(data))) {
    x <- data[[v]]
    if (!is.character(x) && !is.factor(x)) next
    seen <- sort(unique(as.character(x[!is.na(x)])))
    data[[v]] <- factor(as.character(x), levels = unique(c(cl[[v]], seen)))
  }
  data
}

#' The ids in front of an analysis's ARD
#'
#' Puts `output_id`, `analysis_id` and `population_id` in front of the
#' ARD's columns.  A cards ARD stays one (class `card`), so the study ARD is
#' one too and cards' own tools (`as_nested_list()`, `compare_ard()`) take
#' it.
#'
#' @param ard An ARD (a cards one, or any data frame).
#' @param output_id The report.
#' @param analysis_id The analysis.  For analyses run together
#'   (`cards::ard_stack()`), the rows that are none of `analyses`' (the by
#'   counts, the total N).
#' @param population The analysis set's id.
#' @param analyses For analyses run together: each analysis's variables,
#'   `list(CONT = "AGE", CAT = c("SEX", "RACE"))`; their rows are theirs.
#' @return The ARD with the three ids first.
#' @export
tag_ard <- function(ard, output_id, analysis_id, population = NA_character_,
                    analyses = NULL) {
  if (length(analyses)) {
    own <- rep(names(analyses), lengths(analyses))
    id <- own[match(as.character(ard$variable), unlist(analyses))]
    analysis_id <- ifelse(is.na(id), analysis_id, id)
  }
  if (inherits(ard, "card")) {
    rlang::check_installed("dplyr")
    return(dplyr::mutate(ard, output_id = output_id,
                         analysis_id = analysis_id,
                         population_id = population, .before = 1L))
  }
  ard <- as.data.frame(ard)
  cbind(output_id = output_id, analysis_id = analysis_id,
        population_id = population, ard, stringsAsFactors = FALSE)
}

#' A p-value as text
#'
#' The `pvalue` format of an ARD definition: `<0.001`, else 3 decimals.
#'
#' @param x Numbers.
#' @return Character.
#' @examples
#' fmt_pvalue(c(0.0004, 0.0123))
#' @export
fmt_pvalue <- function(x) ifelse(x < 0.001, "<0.001", sprintf("%.3f", x))

#' Formats after the call
#'
#' For what takes no `fmt_fun` (cardx's tests and models, a study's own
#' functions, code, `ard_stack()`'s own rows): each statistic's format, as
#' cards' `fmt_fun` takes it, by statistic (`mean`) or variable and
#' statistic (`"AGE:sd"`), then `cards::apply_fmt_fun()`.  A list of ARDs
#' (`cards::ard_pairwise()`, one per pair of groups) becomes one, each row
#' keeping its ARD's name as `pairwise`.
#'
#' @param ard An ARD, or a list of them.
#' @param formats The formats: a named list, an integer that many decimals
#'   or a function.
#' @param skip Variables whose rows have theirs already (formatted in their
#'   call).
#' @return The ARD, `stat_fmt` filled.  Not a cards ARD: as it is.
#' @export
fmt_ard <- function(ard, formats = list(), skip = character()) {
  if (is.list(ard) && !is.data.frame(ard)) {
    rlang::check_installed("dplyr")
    ard <- dplyr::bind_rows(ard, .id = "pairwise")
  }
  if (!inherits(ard, "card")) return(ard)
  rlang::check_installed(c("cards", "dplyr"))
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

#' Only the statistics asked for
#'
#' Of a method that gives more than the analysis asks for.
#'
#' @param ard An ARD, or a list of them (`cards::ard_pairwise()`).
#' @param stats The statistics (`stat_name`) to keep.
#' @return The ARD with only those rows.
#' @export
keep_stats <- function(ard, stats) {
  if (is.list(ard) && !is.data.frame(ard)) return(lapply(ard, keep_stats, stats))
  ard[ard$stat_name %in% stats, , drop = FALSE]
}

#' One report's rows into the study ARD
#'
#' Replaces the report's rows of the study ARD (the other reports' are left
#' as they are) and records what was built in `ard_status.csv` next to it:
#' the report, the definition's fingerprint ([tfl_ard_spec_hash()]), when,
#' and how many rows.
#'
#' @param ard The report's ARD.
#' @param output_id The report.
#' @param definition The fingerprint of the report's definition.
#' @param path The study ARD.
#' @param sources Files the ARD was built with, named: each one's md5 is a
#'   column of the status (`""` when the file is not there).  By default the
#'   option `tflspec.ard_sources`, which a study's setup sets once
#'   (`options(tflspec.ard_sources = c(setup = "programs/study_setup.R"))`).
#' @return `ard`, invisibly.
#' @export
save_ard <- function(ard, output_id, definition = NA_character_,
                     path = "output/ard/ard.rds",
                     sources = getOption("tflspec.ard_sources")) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  old <- if (file.exists(path)) readRDS(path)
  new <- if (is.null(old)) ard else {
    rlang::check_installed("dplyr")
    dplyr::bind_rows(old[old$output_id != output_id, , drop = FALSE], ard)
  }
  tmp <- paste0(path, ".tmp")
  saveRDS(new, tmp)
  file.rename(tmp, path)
  sf <- file.path(dirname(path), "ard_status.csv")
  row <- data.frame(output_id = output_id, definition = definition,
                    built = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                    rows = as.character(nrow(ard)), error = "",
                    stringsAsFactors = FALSE)
  for (k in names(sources)) {
    row[[k]] <- if (file.exists(sources[[k]])) unname(tools::md5sum(sources[[k]])) else ""
  }
  st <- if (file.exists(sf)) utils::read.csv(sf, colClasses = "character")
  if (!is.null(st) && nrow(st)) {
    for (k in setdiff(names(row), names(st))) st[[k]] <- ""
    row <- rbind(st[st$output_id != output_id, names(row), drop = FALSE], row)
  }
  utils::write.csv(row, sf, row.names = FALSE)
  cat(sprintf("%s: %d rows into %s\n", output_id, nrow(ard), path))
  invisible(ard)
}
