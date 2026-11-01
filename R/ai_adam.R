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
    files <- if (dir.exists(path)) .ai_adam_files(path, full = TRUE) else path
    if (!length(files)) {
      .ard_stop(sprintf("%s: no .xpt, .sas7bdat, .rds or .parquet file in %s; copy the ADaM files there.",
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

# The ADaM files of a folder, one a dataset: of a dataset in several forms
# (adsl.sas7bdat and adsl.xpt, adsl.xpt and adsl.rds), the one read is the
# first of .sas7bdat, .xpt, .rds, .parquet (#523)
.ai_adam_exts <- c("sas7bdat", "xpt", "rds", "parquet")
.ai_adam_files <- function(dir, full = FALSE) {
  files <- list.files(dir, pattern = "[.](xpt|sas7bdat|rds|parquet)$", ignore.case = TRUE,
                      full.names = full)
  if (!length(files)) return(files)
  nm <- toupper(tools::file_path_sans_ext(basename(files)))
  rank <- match(tolower(tools::file_ext(files)), .ai_adam_exts)
  o <- order(nm, rank)
  files <- files[o][!duplicated(nm[o])]
  files[order(toupper(tools::file_path_sans_ext(basename(files))))]
}

.ai_adam_read_one <- function(f, fn) {
  ext <- tolower(tools::file_ext(f))
  if (ext == "rds") return(as.data.frame(readRDS(f)))
  if (ext == "parquet") {
    .ard_need("arrow", paste(fn, "reading a .parquet file"))
    return(as.data.frame(arrow::read_parquet(f)))
  }
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
#' @param define,adam_spec The study's `define.xml` (2.0 or 2.1), or its
#'   ADaM spec workbook in the Pinnacle 21 layout ([tfl_read_define_spec()]),
#'   or what either reader returns -- one of them.  What the define says is
#'   added to what the data shows: each code list's decodes (`[codes: M =
#'   Male | F = Female]`; the values listed stay the data's, in the code
#'   list's order where it has them), a variable's label the data lacks, the digits and
#'   display format of a numeric variable, and the value-level metadata (a
#'   variable's digits and format by where clause, `AVAL where PARAMCD ==
#'   "BILI": digits 1, format 8.1`) as a section of its own.  `NULL`
#'   (default): the data alone.
#' @return A `tfl_ai_adam`: a list with `text` (one string, Markdown),
#'   `variables` (one row a variable: `dataset`, `dataset_label`,
#'   `variable`, `label`, `kind`, `levels` -- ` | ` between the values --
#'   as [tfl_adam_metadata()] gives it; `missing`, how many records have no
#'   value (`NA`, and `""` for a character variable); and `codes`: what each code of a
#'   code variable stands for, `"1 = <65 | 2 = >=65"`, from its value labels
#'   or the variable it pairs with -- `AGEGR1N` and `AGEGR1`, `ARMCD` and
#'   `ARM`, `PARAMCD` and `PARAM` -- when each code has one text) and `datasets` (`dataset`, `label`,
#'   `records`, `subjects` -- the distinct `USUBJID`s, `NA` without one --
#'   `variables`), and `value_level` (the define's, one row an
#'   entry; none without a define).  `format()` gives the text.
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
tfl_ai_adam_summary <- function(path, datasets = NULL, max_levels = 15L, file = NULL,
                                define = NULL, adam_spec = NULL) {
  fn <- "tfl_ai_adam_summary()"
  def <- if (!is.null(define)) .ai_md_read(define, "define", fn) else
    if (!is.null(adam_spec)) .ai_md_read(adam_spec, "adam_spec", fn)
  x <- .ai_adam_read(path, datasets, fn)
  meta <- tfl_adam_metadata(datasets = x, max_levels = max_levels)
  # the names a spec picks rows by: their values even when many
  key <- grepl(.ai_adam_key_vars, meta$variable)
  for (i in which(key & is.na(meta$levels))) {
    v <- x[[meta$dataset[i]]][[meta$variable[i]]]
    meta$levels[i] <- .ai_md_levels(v, 4L * max_levels)
  }
  vars <- meta[c("dataset", "dataset_label", "variable", "label", "kind", "levels")]
  vars$missing <- vapply(seq_len(nrow(vars)), function(i) {
    .ai_adam_missing(x[[vars$dataset[i]]][[vars$variable[i]]])
  }, 1L)
  # what a code stands for: its value labels (a SAS format read with the
  # data), or the variable it pairs with (AGEGR1N / AGEGR1, ARMCD / ARM,
  # PARAMCD / PARAM) when each code has one text
  vars$codes <- unlist(lapply(seq_len(nrow(vars)), function(i) {
    .ai_adam_codes(x[[vars$dataset[i]]], vars$variable[i], 4L * max_levels)
  }), use.names = FALSE)
  ds <- data.frame(
    dataset = names(x),
    label = vapply(names(x), function(n) {
      l <- attr(x[[n]], "label", exact = TRUE)
      if (is.character(l) && length(l) == 1L) l else NA_character_
    }, ""),
    records = vapply(x, nrow, 1L),
    subjects = vapply(x, function(d) {
      u <- d[["USUBJID"]]
      if (is.null(u)) NA_integer_ else length(unique(u[!is.na(u)]))
    }, 1L),
    variables = vapply(x, ncol, 1L),
    stringsAsFactors = FALSE)
  rownames(ds) <- NULL
  vl <- .define_vl_empty()
  src <- NULL
  if (!is.null(def)) {
    src <- attr(def, "source")$kind %||% "define"
    vars <- .ai_adam_define_merge(vars, def, 4L * max_levels)
    vl <- .ai_adam_value_level(def, names(x))
  }
  text <- .ai_adam_text(ds, vars, vl, src)
  out <- structure(list(text = text, variables = vars, datasets = ds, value_level = vl),
                   class = "tfl_ai_adam")
  if (!is.null(file)) .ai_write_text(text, file)
  out
}

#' @export
format.tfl_ai_adam <- function(x, ...) x$text

#' @export
print.tfl_ai_adam <- function(x, ...) {
  cat(sprintf("<tfl_ai_adam: %d dataset(s), %d variables, %d characters>\n",
              nrow(x$datasets), nrow(x$variables), nchar(x$text)))
  cat(sprintf("  %s (%s records%s, %d variables)\n", x$datasets$dataset,
              format(x$datasets$records, big.mark = ","),
              .ai_adam_subjects(x$datasets$subjects), x$datasets$variables), sep = "")
  cat("format(x) gives the text to paste.\n")
  invisible(x)
}

.ai_adam_kind <- c(character = "char", numeric = "num", factor = "char", date = "date",
                   logical = "lgl")

# How many of a variable's values are missing: NA, and "" of a character
# (or factor) variable
.ai_adam_missing <- function(v) {
  if (is.factor(v)) v <- as.character(v)
  miss <- is.na(v)
  if (is.character(v)) miss <- miss | !nzchar(trimws(v))
  sum(miss)
}

# ", 120 subjects" after the records; nothing without a USUBJID
.ai_adam_subjects <- function(n) {
  ifelse(is.na(n), "", paste0(", ", format(n, big.mark = ","), " subjects"))
}

# A code variable's codes and what each stands for, "1 = <65 | 2 = >=65":
# from its value labels (haven's `labels`), else from the variable it pairs
# with -- `<X>N` and `<X>`, `<X>CD` and `<X>` -- when each code has one text
# and there are `max` codes at most; NA else
.ai_adam_codes <- function(d, v, max) {
  x <- d[[v]]
  show <- function(code, text) {
    o <- order(code)
    code <- code[o]; text <- text[o]
    if (is.numeric(code)) code <- format(code, trim = TRUE, drop0trailing = TRUE)
    paste(sprintf("%s = %s", code, text), collapse = " | ")
  }
  lab <- attr(x, "labels", exact = TRUE)
  if (length(lab) && length(lab) <= max) return(show(unname(lab), names(lab)))
  pair <- if (grepl("N$", v)) sub("N$", "", v) else if (grepl("CD$", v)) sub("CD$", "", v)
  if (is.null(pair) || !nzchar(pair) || !pair %in% names(d) || identical(pair, v)) {
    return(NA_character_)
  }
  y <- d[[pair]]
  if (is.factor(y)) y <- as.character(y)
  if (!is.character(y)) return(NA_character_)
  ok <- !is.na(x) & !is.na(y) & nzchar(y)
  m <- unique(data.frame(code = as.vector(x[ok]), text = y[ok], stringsAsFactors = FALSE))
  if (!nrow(m) || nrow(m) > max || anyDuplicated(m$code) || anyDuplicated(m$text)) {
    return(NA_character_)
  }
  show(m$code, m$text)
}

# What the define says, put on the data's variables: a code list's decodes
# as the codes (the values listed stay the data's -- a define's `Not
# Reported` is not the data's `NOT REPORTED` -- put in the code list's
# order where it has them), a label the data lacks, a numeric variable's
# digits and display format (`define`)
.ai_adam_define_merge <- function(vars, def, max) {
  dv <- def$variables
  cl <- def$codelists
  vars$define <- NA_character_
  key <- paste(toupper(vars$dataset), toupper(vars$variable))
  dkey <- paste(toupper(dv$dataset), toupper(dv$variable))
  for (i in seq_len(nrow(dv))) {
    k <- match(dkey[i], key)
    if (is.na(k)) next
    if (is.na(vars$label[k]) && !is.na(dv$label[i])) vars$label[k] <- dv$label[i]
    id <- dv$codelist[i]
    if (!is.na(id) && nzchar(id) && !is.null(cl)) {
      x <- cl[cl$codelist %in% id, , drop = FALSE]
      if (nrow(x) && nrow(x) <= max) {
        if ("order" %in% names(x) && !all(is.na(x$order))) {
          x <- x[order(suppressWarnings(as.numeric(x$order))), , drop = FALSE]
        }
        if (!is.na(vars$levels[k])) {
          lv <- strsplit(vars$levels[k], " | ", fixed = TRUE)[[1L]]
          pos <- match(toupper(lv), toupper(as.character(x$value)))
          vars$levels[k] <- paste(c(lv[!is.na(pos)][order(pos[!is.na(pos)])], lv[is.na(pos)]),
                                  collapse = " | ")
        }
        dec <- as.character(x$decode)
        if (any(!is.na(dec) & nzchar(dec) & dec != as.character(x$value))) {
          dec[is.na(dec) | !nzchar(dec)] <- as.character(x$value)[is.na(dec) | !nzchar(dec)]
          vars$codes[k] <- paste(sprintf("%s = %s", x$value, dec), collapse = " | ")
        }
      }
    }
    note <- c(if (!is.na(dv$digits[i])) sprintf("digits %d", dv$digits[i]),
              if (!is.na(dv$format[i]) && nzchar(dv$format[i])) sprintf("format %s", dv$format[i]))
    if (length(note)) vars$define[k] <- paste(note, collapse = ", ")
  }
  vars
}

# The define's value-level rows of the datasets at hand (all of them when
# the define names datasets the data does not have yet)
.ai_adam_value_level <- function(def, datasets) {
  vl <- def$value_level %||% .define_vl_empty()
  if (!nrow(vl)) return(vl)
  keep <- toupper(vl$dataset) %in% toupper(datasets)
  if (any(keep)) vl <- vl[keep, , drop = FALSE]
  # (an entry with neither digits nor a format says nothing of the look)
  vl <- vl[!is.na(vl$digits) | (!is.na(vl$format) & nzchar(vl$format)), , drop = FALSE]
  rownames(vl) <- NULL
  vl
}

.ai_adam_text <- function(ds, vars, vl = NULL, define = NULL) {
  head <- c(
    "# ADaM datasets: variables only",
    "",
    paste0("Each line: `NAME (type) Label: values [missing: ...]`. The values are listed for a ",
           "character variable with few of them, as the data holds them; `[missing: 3 of 120]` ",
           "is how many records have no value (NA, or blank); no record is shown.",
           if (any(!is.na(vars$codes))) paste0(
             " A code variable shows what each code stands for: ",
             "`[codes: 1 = <65 | 2 = >=65]`."),
           if (!is.null(define)) paste0(
             " The ", define, " adds its code lists' decodes (the values listed stay the data's) ",
             "and a numeric variable's digits and display format (`[", define, ": digits 1, format 8.1]`): ",
             "the study's digits, to use when the shell does not show them.")),
    "")
  vl_text <- if (!is.null(vl) && nrow(vl)) {
    c(sprintf("## Value level (%s): digits and format by where clause", define %||% "define"),
      "",
      "A variable's digits in the records a condition picks (a lab parameter's AVAL): the study's digits for that parameter, to use when the shell does not show them.",
      "",
      vapply(seq_len(nrow(vl)), function(i) {
        has_where <- !is.na(vl$where[i]) && nzchar(vl$where[i])
        lab <- if (!is.na(vl$label[i]) && nzchar(vl$label[i])) vl$label[i]
        note <- c(if (!is.na(vl$digits[i])) sprintf("digits %d", vl$digits[i]),
                  if (!is.na(vl$format[i]) && nzchar(vl$format[i])) sprintf("format %s", vl$format[i]),
                  if (has_where) lab)
        # (a Define-XML 1.0 has no where clause: the entry's label says
        # which records -- a parameter's name)
        sprintf("- %s %s %s: %s", vl$dataset[i], vl$variable[i],
                if (has_where) paste0("where `", vl$where[i], "`") else
                  if (!is.null(lab)) paste0("(", lab, ")") else "(a value level entry)",
                if (length(note)) paste(note, collapse = ", ") else "(no digits)")
      }, ""),
      "")
  }
  body <- unlist(lapply(seq_len(nrow(ds)), function(i) {
    v <- vars[vars$dataset == ds$dataset[i], , drop = FALSE]
    kind <- unname(.ai_adam_kind[v$kind])
    kind[is.na(kind)] <- v$kind[is.na(kind)]
    lines <- paste0(v$variable, " (", kind, ")",
                    ifelse(is.na(v$label), "", paste0(" ", v$label)),
                    ifelse(is.na(v$levels), "", paste0(": ", v$levels)),
                    ifelse(is.na(v$codes %||% NA), "", paste0(" [codes: ", v$codes, "]")),
                    ifelse(is.na(v$define %||% NA), "", paste0(" [", define, ": ", v$define, "]")),
                    " [missing: ", ifelse(v$missing == 0L, "none",
                                          paste(v$missing, "of", ds$records[i])), "]")
    c(sprintf("## %s%s (%s records%s, %d variables)", ds$dataset[i],
              if (is.na(ds$label[i])) "" else paste0(": ", ds$label[i]),
              format(ds$records[i], big.mark = ","), .ai_adam_subjects(ds$subjects[i]),
              ds$variables[i]),
      lines, "")
  }), use.names = FALSE)
  paste(c(head, body, vl_text), collapse = "\n")
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
  files <- .ai_adam_files(adam_dir)
  if (!length(files)) {
    .ard_stop(sprintf(paste0("%s: no .xpt, .sas7bdat, .rds or .parquet file in %s; copy the ADaM files ",
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
    # (the flags' sets, and every subject of ADSL as a set of its own: a
    # report of all subjects names ALL)
    populations = c(lapply(seq_len(nrow(pops)), function(i) list(
      id = pops$id[i], dataset = "ADSL", where = tfl_r(sprintf("%s == \"Y\"", pops$flag[i])),
      label = pops$label[i])),
      list(list(id = "ALL", dataset = "ADSL", label = "All subjects (every subject of ADSL)"))))
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
