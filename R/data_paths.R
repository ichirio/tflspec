# Where the study's datasets are: spec/data.yml's paths against the files
# on disk (ichirio/tflplanner#515)

# The datasets spec/data.yml lists, each with its path, whether the file is
# there (paths from the study folder, or absolute) and, when it is not, the
# same name in an extension that is (.xpt for .rds) -- as a data frame:
# dataset, path, exists, other
.data_paths_check <- function(datasets, dir = ".") {
  n <- length(datasets)
  out <- data.frame(dataset = character(n), path = character(n),
                    exists = logical(n), other = NA_character_,
                    stringsAsFactors = FALSE)
  for (i in seq_len(n)) {
    z <- datasets[[i]]
    out$dataset[i] <- as.character(z$dataset %||% NA_character_)
    p <- as.character(z$path %||% NA_character_)
    out$path[i] <- p
    if (is.na(p) || !nzchar(p)) next
    full <- if (grepl("^([A-Za-z]:)?[/\\\\]", p)) p else file.path(dir, p)
    out$exists[i] <- file.exists(full)
    if (!out$exists[i]) {
      stem <- tools::file_path_sans_ext(basename(full))
      fold <- dirname(full)
      has <- if (dir.exists(fold)) list.files(fold, ignore.case = TRUE) else character()
      has <- has[tolower(tools::file_path_sans_ext(has)) == tolower(stem) &
                   tolower(tools::file_ext(has)) %in% c("sas7bdat", "xpt", "rds", "parquet", "csv")]
      if (length(has)) {
        has <- has[order(match(tolower(tools::file_ext(has)),
                               c("sas7bdat", "xpt", "rds", "parquet", "csv")))]
        out$other[i] <- sub(basename(p), has[1L], p, fixed = TRUE)
      }
    }
  }
  out
}

# "ADSL: no file at data/adam/adsl.rds (spec/data.yml). Point it at the
# study's file, e.g. data/adam/adsl.xpt."
.data_missing_text <- function(dataset, path, other = NA_character_) {
  sprintf("%s: no file at %s (spec/data.yml). Point it at the study's file%s.",
          dataset, path, if (is.na(other)) "" else paste0(", e.g. ", other))
}

# The message for the first of `datasets` whose file is missing and that
# `code` (or `only`: the datasets named) reads; NULL when none is missing
.data_missing_message <- function(datasets, dir = ".", only = NULL, code = NULL) {
  if (!length(datasets)) return(NULL)
  chk <- .data_paths_check(datasets, dir)
  bad <- which(!chk$exists)
  if (!is.null(only)) bad <- bad[toupper(chk$dataset[bad]) %in% toupper(only)]
  if (!length(bad)) return(NULL)
  if (!is.null(code)) {
    named <- bad[vapply(bad, function(i) grepl(basename(chk$path[i]), code, fixed = TRUE), NA)]
    if (length(named)) bad <- named
  }
  i <- bad[1L]
  .data_missing_text(chk$dataset[i], chk$path[i], chk$other[i])
}

# eval() of a program's code in the study folder `dir`: a file of data.yml's
# that is missing is named (the dataset, the path), not "cannot open the
# connection"
.eval_study_code <- function(code, env, study = NULL, dir = ".") {
  txt <- if (is.character(code)) paste(code, collapse = "\n") else ""
  exprs <- if (is.character(code)) parse(text = code, encoding = "UTF-8") else code
  tryCatch(eval(exprs, envir = env), error = function(e) {
    ds <- if (inherits(study, "tfl_study_spec")) study$data$datasets else NULL
    msg <- tryCatch(.data_missing_message(ds, dir = dir, code = txt), error = function(e2) NULL)
    if (is.null(msg) || !grepl("cannot open|No such file|does not exist|Failed to open",
                               conditionMessage(e), ignore.case = TRUE)) stop(e)
    .ard_stop(msg)
  })
}

# A data.yml's dataset read from its path: the file must be there
.data_file_must_exist <- function(dataset, path, dir = ".") {
  full <- if (grepl("^([A-Za-z]:)?[/\\\\]", path)) path else file.path(dir, path)
  if (file.exists(full)) return(invisible(path))
  msg <- .data_missing_message(list(list(dataset = dataset, path = path)), dir = dir)
  .ard_stop(msg)
}

# tfl_ai_prepare(): spec/data.yml's paths against the ADaM files given
# (the folder `adam_dir`): per dataset of data.yml its `in_data_yml` path,
# the `given` file (the one tfl_ai_adam_summary() reads: .sas7bdat, .xpt,
# .rds, .parquet in that order; `NA` none), whether the data.yml's file
# `exists` and `differs` -- the given file is another file or type than
# data.yml's.  NULL when there is no data.yml.
.ai_data_paths <- function(dir, adam_dir) {
  f <- file.path(dir, "spec", "data.yml")
  if (!file.exists(f)) return(NULL)
  y <- tryCatch(yaml::read_yaml(f), error = function(e) NULL)
  ds <- if (is.list(y)) y$datasets else NULL
  if (!length(ds)) return(NULL)
  chk <- .data_paths_check(ds, dir)
  given <- if (dir.exists(adam_dir)) .ai_adam_files(adam_dir, full = TRUE) else character()
  gname <- toupper(tools::file_path_sans_ext(basename(given)))
  g <- given[match(toupper(chk$dataset), gname)]
  norm <- function(p) {
    ifelse(is.na(p), NA_character_, normalizePath(p, winslash = "/", mustWork = FALSE))
  }
  inyml <- ifelse(grepl("^([A-Za-z]:)?[/\\\\]", chk$path), chk$path, file.path(dir, chk$path))
  differs <- !is.na(g) & (is.na(chk$path) | norm(g) != norm(inyml))
  data.frame(dataset = chk$dataset, in_data_yml = chk$path, given = g,
             exists = chk$exists, differs = differs, stringsAsFactors = FALSE)
}

# the warning text for the rows that are missing or differ
.ai_data_paths_text <- function(x) {
  if (is.null(x)) return(character())
  bad <- x[!x$exists | x$differs, , drop = FALSE]
  if (!nrow(bad)) return(character())
  sprintf("%s: spec/data.yml has %s%s; the ADaM given is %s",
          bad$dataset, bad$in_data_yml, ifelse(bad$exists, "", " (no such file)"),
          ifelse(is.na(bad$given), "not there", bad$given))
}
