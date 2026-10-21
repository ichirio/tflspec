# ============================================================================
#  The ADaM datasets as a summary for an AI chat, and a study folder to start
# ----------------------------------------------------------------------------
#  The data never go to the AI: only what a programmer would read in a
#  define -- each dataset's variables, their labels and types, and the
#  values of a short code list (SEX, AGEGR1, PARAMCD ...).  The summary is
#  text to paste and, as a frame, the `datasets` a drafting context checks
#  the answer against.
#
#    tfl_ai_adam_summary()   the datasets' variables, as text and a frame
#    tfl_ai_study_init()     study.yml, spec/data.yml, spec/defaults.yml
# ============================================================================

# Variables whose values are listed even when there are many of them: the
# names a table's spec picks rows by.
.ai_adam_key_vars <- "^(PARAMCD|PARCAT[0-9]*|AVISIT|ATPT|DTYPE|ANRIND|BNRIND|ATOXGR|BTOXGR|AVALC|CRIT[0-9]*|AEBODSYS|AESOC)$"

# Reads a folder of ADaM files (or one file) into a named list of data
# frames, the dataset names in upper case.
.ai_adam_read <- function(path, datasets, fn) {
  if (.ai_is_data_list(path)) {
    x <- path
    names(x) <- toupper(names(x))
  } else {
    if (!is.character(path) || length(path) != 1L || !file.exists(path)) {
      .ard_stop(sprintf(paste0("%s: no folder or file %s in %s; `path` is the folder of the ",
                               "ADaM files (.xpt), one file, or a named list of data frames."),
                        fn, sQuote(paste(format(path), collapse = " "), FALSE),
                        sQuote(getwd(), FALSE)))
    }
    files <- if (dir.exists(path)) {
      list.files(path, pattern = "[.](xpt|sas7bdat|rds)$", ignore.case = TRUE, full.names = TRUE)
    } else {
      path
    }
    if (!length(files)) {
      .ard_stop(sprintf("%s: no .xpt, .sas7bdat or .rds file in %s; copy the ADaM files there.",
                        fn, sQuote(path, FALSE)))
    }
    nm <- toupper(tools::file_path_sans_ext(basename(files)))
    if (!is.null(datasets)) {
      keep <- nm %in% toupper(datasets)
      files <- files[keep]
      nm <- nm[keep]
    }
    x <- stats::setNames(lapply(files, .ai_adam_read_one, fn = fn), nm)
  }
  if (!is.null(datasets)) {
    miss <- setdiff(toupper(datasets), names(x))
    if (length(miss)) {
      .ard_stop(sprintf("%s: no dataset %s; there are %s.", fn,
                        paste(miss, collapse = ", "), paste(names(x), collapse = ", ")))
    }
    x <- x[toupper(datasets)]
  }
  x
}

.ai_adam_read_one <- function(f, fn) {
  ext <- tolower(tools::file_ext(f))
  if (ext == "rds") return(as.data.frame(readRDS(f)))
  if (!requireNamespace("haven", quietly = TRUE)) {
    .ard_stop(sprintf("%s: reading .%s files needs the haven package: install.packages(\"haven\").",
                      fn, ext))
  }
  d <- if (ext == "xpt") haven::read_xpt(f) else haven::read_sas(f)
  lab <- attr(d, "label", exact = TRUE)
  d <- as.data.frame(d)
  attr(d, "label") <- lab
  d
}

#' The ADaM datasets as a summary for an AI chat
#'
#' What an AI chat needs to know of the study's data to draft its specs,
#' and nothing more: per dataset its label and size, and per variable its
#' name, label and type, with the values of a character or factor variable
#' that has few of them (SEX, AGEGR1, the arms, PARAMCD ...).  No record is
#' written: identifiers, dates and times never have their values listed.
#' Paste the text into the chat; never attach the datasets themselves.
#'
#' The summary's `variables` frame is what [tfl_ai_context()] takes as
#' `datasets`: [tfl_ai_check()] then checks every dataset and variable an
#' answer names against it.
#'
#' @param path A folder of ADaM files (`.xpt`, also `.sas7bdat`, `.rds`), one
#'   file, or a named list of data frames.  `.xpt` and `.sas7bdat` need the
#'   haven package.
#' @param datasets The datasets to keep (`"ADSL"`, `"ADAE"` ...); `NULL` for
#'   all.
#' @param max_levels A variable's values are listed when it has this many at
#'   most (PARAMCD, PARCAT, AVISIT, the grades: four times as many).
#' @param file A file to write the text to (UTF-8), or `NULL`.
#' @return A `tfl_ai_adam`: a list with `text` (one string, Markdown),
#'   `variables` (one row a variable: `dataset`, `dataset_label`,
#'   `variable`, `label`, `kind`, `levels` -- ` | ` between the values --
#'   as [tfl_adam_metadata()] gives it) and `datasets` (`dataset`, `label`,
#'   `records`, `variables`).  `format()` gives the text.
#' @seealso [tfl_ai_read_shell()], [tfl_ai_context()].
#' @examples
#' s <- tfl_ai_adam_summary(list(adsl = tflspec::adsl, adae = tflspec::adae))
#' cat(substr(format(s), 1, 700))
#' head(s$variables)
#' \donttest{
#' if (requireNamespace("haven", quietly = TRUE)) {
#'   d <- file.path(tempdir(), "adam")
#'   dir.create(d, showWarnings = FALSE)
#'   haven::write_xpt(tflspec::adsl, file.path(d, "adsl.xpt"))
#'   tfl_ai_adam_summary(d)
#' }
#' }
#' @export
tfl_ai_adam_summary <- function(path, datasets = NULL, max_levels = 15L, file = NULL) {
  fn <- "tfl_ai_adam_summary()"
  x <- .ai_adam_read(path, datasets, fn)
  meta <- tfl_adam_metadata(datasets = x, max_levels = max_levels)
  # the names a spec picks rows by: their values even when many
  key <- grepl(.ai_adam_key_vars, meta$variable)
  for (i in which(key & is.na(meta$levels))) {
    v <- x[[meta$dataset[i]]][[meta$variable[i]]]
    meta$levels[i] <- .ai_md_levels(v, 4L * max_levels)
  }
  vars <- meta[c("dataset", "dataset_label", "variable", "label", "kind", "levels")]
  ds <- data.frame(
    dataset = names(x),
    label = vapply(names(x), function(n) {
      l <- attr(x[[n]], "label", exact = TRUE)
      if (is.character(l) && length(l) == 1L) l else NA_character_
    }, ""),
    records = vapply(x, nrow, 1L), variables = vapply(x, ncol, 1L),
    stringsAsFactors = FALSE)
  rownames(ds) <- NULL
  text <- .ai_adam_text(ds, vars)
  out <- structure(list(text = text, variables = vars, datasets = ds), class = "tfl_ai_adam")
  if (!is.null(file)) .ai_write_text(text, file)
  out
}

#' @export
format.tfl_ai_adam <- function(x, ...) x$text

#' @export
print.tfl_ai_adam <- function(x, ...) {
  cat(sprintf("<tfl_ai_adam: %d dataset(s), %d variables, %d characters>\n",
              nrow(x$datasets), nrow(x$variables), nchar(x$text)))
  cat(sprintf("  %s (%s records, %d variables)\n", x$datasets$dataset,
              format(x$datasets$records, big.mark = ","), x$datasets$variables), sep = "")
  cat("format(x) gives the text to paste.\n")
  invisible(x)
}

.ai_adam_kind <- c(character = "char", numeric = "num", factor = "char", date = "date",
                   logical = "lgl")

.ai_adam_text <- function(ds, vars) {
  head <- c(
    "# ADaM datasets: variables only",
    "",
    paste("Each line: `NAME (type) Label: values`. The values are listed for a",
          "character variable with few of them; no record is shown."),
    "")
  body <- unlist(lapply(seq_len(nrow(ds)), function(i) {
    v <- vars[vars$dataset == ds$dataset[i], , drop = FALSE]
    kind <- unname(.ai_adam_kind[v$kind])
    kind[is.na(kind)] <- v$kind[is.na(kind)]
    lines <- paste0(v$variable, " (", kind, ")",
                    ifelse(is.na(v$label), "", paste0(" ", v$label)),
                    ifelse(is.na(v$levels), "", paste0(": ", v$levels)))
    c(sprintf("## %s%s (%s records, %d variables)", ds$dataset[i],
              if (is.na(ds$label[i])) "" else paste0(": ", ds$label[i]),
              format(ds$records[i], big.mark = ","), ds$variables[i]),
      lines, "")
  }), use.names = FALSE)
  paste(c(head, body), collapse = "\n")
}

# ---- the study folder ---------------------------------------------------------

#' Start a study folder for drafted report files
#'
#' Writes what every drafted report file stands on, so that the AI writes
#' only the reports: `study.yml` (the study's id and settings: the RTF
#' files to `output/tfl`, the programs to `programs/tfl`, as tflplanner
#' keeps them, so the folder opens there as a study),
#' `spec/data.yml` (each ADaM file as a dataset, and an analysis set for
#' each population flag of ADSL -- `SAFFL`, `FASFL`, `ITTFL`, `PPROTFL` ...
#' -- as `<flag> == "Y"`) and `spec/defaults.yml` (the page every report
#' starts from: the running header and footer, and the cell of a count).
#' Each report file then says `extends: [defaults]` and only what differs.
#' Check the analysis sets' ids and labels: the table prompt gives them to
#' the AI.
#'
#' @param dir The study folder (made if need be).
#' @param adam The ADaM folder, as the study's programs reach it from `dir`
#'   (relative, such as `"adam"`, or absolute); its `.xpt` (or
#'   `.sas7bdat`, `.rds`) files become the datasets.
#' @param study_id,title The study's id and title.
#' @param overwrite Write over files already there?
#' @return The paths written, invisibly.
#' @examples
#' d <- file.path(tempdir(), "study-init")
#' dir.create(file.path(d, "adam"), recursive = TRUE, showWarnings = FALSE)
#' saveRDS(tflspec::adsl, file.path(d, "adam", "adsl.rds"))
#' tfl_ai_study_init(d, adam = "adam", study_id = "ONCO-01", overwrite = TRUE)
#' cat(readLines(file.path(d, "spec", "data.yml")), sep = "\n")
#' @export
tfl_ai_study_init <- function(dir, adam = "adam", study_id = "STUDY-01", title = NULL,
                              overwrite = FALSE) {
  fn <- "tfl_ai_study_init()"
  adam_dir <- if (grepl("^(/|[A-Za-z]:)", adam)) adam else file.path(dir, adam)
  files <- list.files(adam_dir, pattern = "[.](xpt|sas7bdat|rds)$", ignore.case = TRUE)
  if (!length(files)) {
    .ard_stop(sprintf(paste0("%s: no .xpt, .sas7bdat or .rds file in %s; copy the ADaM files ",
                             "there, or give `adam` as their folder from %s."), fn,
                      sQuote(adam_dir, FALSE), sQuote(dir, FALSE)))
  }
  paths <- file.path(dir, c("study.yml", "spec/data.yml", "spec/defaults.yml"))
  there <- paths[file.exists(paths)]
  if (length(there) && !isTRUE(overwrite)) {
    .ard_stop(sprintf("%s: %s already there; pass `overwrite = TRUE`.", fn,
                      paste(sQuote(basename(there), FALSE), collapse = ", ")))
  }
  dir.create(file.path(dir, "spec", "outputs"), recursive = TRUE, showWarnings = FALSE)
  nm <- toupper(tools::file_path_sans_ext(files))
  # (an ADaM folder in the study, given whole, from the study folder: the
  # study then reads it wherever it is copied; one outside it, whole)
  if (grepl("^(/|[A-Za-z]:)", adam)) {
    root <- paste0(normalizePath(dir, winslash = "/", mustWork = FALSE), "/")
    q <- normalizePath(adam, winslash = "/", mustWork = FALSE)
    same <- if (.Platform$OS.type == "windows") startsWith(tolower(q), tolower(root)) else
      startsWith(q, root)
    if (same) adam <- substring(q, nchar(root) + 1L)
  }
  rel <- gsub("\\\\", "/", file.path(adam, files))
  pops <- .ai_adam_populations(file.path(adam_dir, files[nm == "ADSL"]), fn)
  study <- list(study_id = study_id, title = title %||% study_id,
                settings = list(rounding = "sas", output_path = "output/tfl",
                                program_dir = "programs/tfl", file = "{output_id}.rtf"),
                keys = list(id = "USUBJID"))
  data <- list(
    datasets = lapply(seq_along(nm), function(i) list(dataset = nm[i], level = "ADaM",
                                                      path = rel[i])),
    populations = lapply(seq_len(nrow(pops)), function(i) list(
      id = pops$id[i], dataset = "ADSL", where = tfl_r(sprintf("%s == \"Y\"", pops$flag[i])),
      label = pops$label[i])))
  .ai_write_text(.ai_yaml_text(study), paths[1L])
  .ai_write_text(.ai_yaml_text(data), paths[2L])
  .ai_write_text(.ai_defaults_yml(study_id), paths[3L])
  invisible(normalizePath(paths, winslash = "/"))
}

.ai_yaml_text <- function(x) {
  sub("\n$", "", yaml::as.yaml(x, indent.mapping.sequence = TRUE,
                               handlers = .yaml_out_handlers()))
}

# The analysis sets: ADSL's population flags (a name ending in FL whose
# label says population, set or analysis, or one of the usual names).
.ai_adam_populations <- function(adsl_file, fn) {
  none <- data.frame(id = character(), flag = character(), label = character(),
                     stringsAsFactors = FALSE)
  if (!length(adsl_file)) return(none)
  d <- .ai_adam_read_one(adsl_file[1L], fn)
  fl <- grep("FL$", names(d), value = TRUE)
  lab <- vapply(fl, function(v) {
    l <- attr(d[[v]], "label", exact = TRUE)
    if (is.character(l) && length(l) == 1L) l else NA_character_
  }, "")
  usual <- c(SAFFL = "SAF", FASFL = "FAS", ITTFL = "ITT", PPROTFL = "PPS", EFFFL = "EFF",
             RANDFL = "RAND", ENRLFL = "ENRL", COMPLFL = "COMPL", PKFL = "PK")
  pop <- fl %in% names(usual) |
    grepl("(?i)population|analysis set|\\bset\\b", ifelse(is.na(lab), "", lab), perl = TRUE)
  fl <- fl[pop]
  if (!length(fl)) return(none)
  id <- ifelse(fl %in% names(usual), usual[fl], sub("FL$", "", fl))
  label <- ifelse(is.na(lab[fl]) | !nzchar(lab[fl]), paste(id, "Population"),
                  sub("(?i)\\s+flag$", "", lab[fl], perl = TRUE))
  data.frame(id = unname(id), flag = fl, label = unname(label), stringsAsFactors = FALSE)
}

.ai_defaults_yml <- function(study_id) {
  c("# What every report starts from; a report file says only what differs",
    "# (extends: [defaults]).",
    "content:",
    "  - normalize_ard: {}",
    "  - plan_cells: {categorical: \"{n} ({p:.1f%})\"}",
    "  - plan_stub: {name: row_label, before: true}",
    "  - plan_blanks: {where: between_groups}",
    "page:",
    "  header:",
    sprintf("    '1': {left: \"%s\", right: \"Page {PAGE} of {TOTAL_PAGES}\"}", study_id),
    "  footer:",
    "    '1': {left: \"{PROGRAM}\", right: \"{DATETIME}\"}")
}

# A data frame's factors as character (as an XPT file holds them).
.ai_chr <- function(d) {
  lab <- lapply(d, attr, "label", exact = TRUE)
  d[] <- lapply(d, function(v) if (is.factor(v)) as.character(v) else v)
  for (n in names(d)) attr(d[[n]], "label") <- lab[[n]]
  d
}
