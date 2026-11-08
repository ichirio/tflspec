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
# (`named_only`: NULL, too, when `code` names none of the missing files)
.data_missing_message <- function(datasets, dir = ".", only = NULL, code = NULL,
                                  named_only = FALSE) {
  if (!length(datasets)) return(NULL)
  chk <- .data_paths_check(datasets, dir)
  # (a dataset data.yml gives no path is not a missing file)
  bad <- which(!chk$exists & !is.na(chk$path) & nzchar(chk$path))
  if (!is.null(only)) bad <- bad[toupper(chk$dataset[bad]) %in% toupper(only)]
  if (!length(bad)) return(NULL)
  if (!is.null(code)) {
    named <- bad[vapply(bad, function(i) grepl(basename(chk$path[i]), code, fixed = TRUE), NA)]
    if (length(named)) bad <- named
    else if (isTRUE(named_only)) return(NULL)
  }
  i <- bad[1L]
  .data_missing_text(chk$dataset[i], chk$path[i], chk$other[i])
}

# eval() of a program's code in the study folder `dir`: a file of data.yml's
# that is missing is named (the dataset, the path), not "cannot open the
# connection"
.eval_study_code <- function(code, env, study = NULL, dir = ".", output_id = NULL) {
  txt <- if (is.character(code)) paste(code, collapse = "\n") else ""
  exprs <- if (is.character(code)) parse(text = code, encoding = "UTF-8") else code
  tryCatch(eval(exprs, envir = env), error = function(e) {
    ds <- if (inherits(study, "tfl_study_spec")) study$data$datasets else NULL
    # (a missing file the code reads is its error, whatever language R says
    # it in; else only an error that reads as a missing file)
    msg <- tryCatch(.data_missing_message(ds, dir = dir, code = txt, named_only = TRUE),
                    error = function(e2) NULL)
    if (is.null(msg) && grepl("cannot open|No such file|does not exist|Failed to open",
                              conditionMessage(e), ignore.case = TRUE)) {
      msg <- tryCatch(.data_missing_message(ds, dir = dir, code = txt), error = function(e2) NULL)
    }
    if (is.null(msg)) {
      # (the failure path only: a program that ran is not looked at.  The
      # data it had made before it stopped is in `env`, nothing is run again)
      empty <- tryCatch(.empty_data_message(env, study, output_id), error = function(e2) NULL)
      if (is.null(empty)) stop(e)
      stop(structure(class = c("tflspec_empty_data", "error", "condition"),
                     list(message = paste0(empty, "\nThe build stopped with: ",
                                           conditionMessage(e)), call = NULL)))
    }
    .ard_stop(msg)
  })
}

# What a program that stopped had made with no records, in words: the report,
# the dataset, the analysis set and the `where`; NULL when it had made no
# data without records (or the study is not known).  `env` is the program's
# environment after the stop.  Looks at the data the program made, and at the
# study's definition for what it was made from -- it runs nothing.
.empty_data_message <- function(env, study, output_id = NULL) {
  if (is.null(study)) return(NULL)
  nm <- ls(env)
  nm <- nm[!grepl("^ard(_|$)", nm)]
  empty <- nm[vapply(nm, function(n) {
    x <- get0(n, envir = env, inherits = FALSE)
    is.data.frame(x) && nrow(x) == 0L
  }, NA)]
  if (!length(empty)) return(NULL)
  is_study <- inherits(study, "tfl_study_spec")
  spec <- if (is_study) .ard_spec_of_study(study, output_id = output_id)$spec else .as_spec(study, "ard", "tfl_build_ard")
  dn <- tfl_ard_data_names(study, output_id = output_id)
  outs <- if (is_study) tfl_read_outputs(study$dir, skip_bad = TRUE) else list()
  first <- function(x) {
    x <- x[!is.na(x)]
    if (length(x)) x[[1L]] else NA_character_
  }
  pops <- spec$populations
  ds <- spec$datasets
  # a dataset read, then an analysis set, then the data made from them: the
  # first kind there is, the others come from it
  is_ds <- empty %in% .r_name(ds$dataset)
  is_pop <- empty %in% paste0("pop_", .r_name(pops$population_id))
  in_dn <- empty %in% dn$data
  out <- NULL
  if (any(is_ds)) {
    d <- ds$dataset[match(empty[is_ds][1L], .r_name(ds$dataset))]
    out <- sprintf("The dataset %s has no records.", d)
  } else if (any(is_pop)) {
    k <- match(empty[is_pop][1L], paste0("pop_", .r_name(pops$population_id)))
    out <- sprintf("The analysis set %s (%s) selects no subjects of %s.", pops$population_id[k],
                   if (is.na(pops$where[k])) "no condition" else pops$where[k], pops$dataset[k])
  } else if (any(in_dn)) {
    n <- dn$data[dn$data %in% empty][1L]
    rows <- dn[dn$data == n, , drop = FALSE]
    oid <- rows$output_id[1L]
    others <- setdiff(unique(rows$output_id), oid)
    ad <- spec$analysis_data
    ad <- if (!is.null(ad) && nrow(ad)) ad[ad$data_id %in% n & ad$output_id %in% oid, , drop = FALSE]
    a <- spec$analyses[spec$analyses$analysis_id %in% rows$analysis_id &
                         spec$analyses$output_id %in% oid, , drop = FALSE]
    # (an analysis data made from another: its source's condition and set too)
    ds_name <- first(c(ad$from, a$dataset))
    where <- c(ad$where, a$where)
    pop <- first(c(ad$population_id, a$population_id))
    all_ad <- spec$analysis_data
    hops <- 0L
    while (!is.na(ds_name) && !is.null(all_ad) && ds_name %in% all_ad$data_id && hops < 10L) {
      up <- all_ad[all_ad$data_id %in% ds_name & all_ad$output_id %in% oid, , drop = FALSE]
      if (!nrow(up)) break
      where <- c(where, up$where)
      if (is.na(pop)) pop <- first(up$population_id)
      ds_name <- first(up$from)
      hops <- hops + 1L
    }
    # (a template puts its `where` in the program it writes: the report has it)
    o <- Filter(function(x) identical(x$id, oid), outs)
    targs <- if (length(o)) o[[1L]]$template$args else NULL
    if (!length(stats::na.omit(where)) && !is.null(targs$where)) {
      where <- paste(as.character(targs$where), collapse = " ")
    }
    if (is.na(ds_name) && !is.null(targs$data)) ds_name <- as.character(targs$data)[1L]
    if (is.na(pop) && !is.null(targs$population)) pop <- as.character(targs$population)[1L]
    where <- unique(stats::na.omit(where))
    pop_where <- if (!is.na(pop)) first(pops$where[pops$population_id %in% pop])
    out <- paste0(
      sprintf("The report %s%s reads no records: its data %s is empty", oid,
              if (length(others)) paste0(" (and ", paste(others, collapse = ", "), ")") else "", n),
      sprintf(" (dataset %s", if (is.na(ds_name)) "not known" else ds_name),
      if (!is.na(pop)) sprintf(", analysis set %s%s", pop,
                               if (is.na(pop_where)) "" else sprintf(" [%s]", pop_where)),
      if (length(where)) sprintf(", where %s", paste(where, collapse = " & ")),
      "). The analysis set and the `where` leave no record; compare them with the data.",
      if (any(grepl("is.na(", where, fixed = TRUE)))
        paste0(" A dataset read from a transport file holds \"\" for no value, so is.na()",
               " finds none of them: use == \"\" or != \"Y\"."))
  }
  out
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
