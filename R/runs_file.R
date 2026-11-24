# ---- run definitions: spec/runs.yml (G4 slice B1) ---------------------------
# One saved thing: a name, the reports it selects, its options.  tflspec owns
# the file: it reads, checks and writes it (tflplanner puts the text into its
# spec model).  The base-R runner checks only the manifest's copy.

#' The study's run definitions: spec/runs.yml
#'
#' @description
#' A run definition is one saved thing: a name, the reports it selects and
#' its options.  The file `spec/runs.yml` holds them, a list (the name is a
#' value, not a key):
#'
#' ```
#' runs:
#'   - name: "Topline"
#'     reports: ["T-14-1-1", "T-14-3-1", "F-14-2-1"]
#'     mode: all          # all (the ARD, then the reports) | ard
#'     copy_data: false
#'   - name: "All"
#'     reports: all
#' ```
#'
#' `reports` is a list of output ids, or the word `all` (every report of the
#' study; the runner's manifest holds it expanded).  The two options of a
#' definition are `mode` (`all`, the default, or `ard`) and `copy_data`
#' (`false` by default).  A key that is not one of these is kept when the
#' file is written, and the definition's run is refused (update tflspec).
#'
#' * `tfl_runs_file()` is the path of the file: the only place that spells it.
#' * `tfl_read_runs()` reads the definitions, ids and names as text (`No`,
#'   `Y`, `1.0` and `001` stay text), and the word `all` as a lower-case
#'   scalar only.  It never stops for what the file holds: a file that does
#'   not parse is a problem row of [tfl_check_runs()].  When the file is
#'   missing it returns what [tfl_runs_migrate()] finds (it reads, and
#'   never writes), with the attribute `"migrated"` (how many).
#' * `tfl_runs_migrate()` makes definitions from the older carriers of a
#'   report's membership: toc.yml's `sets:` and the planner sheet's `batches`
#'   column of a workbook study; or `outputs`, a data frame with the columns
#'   `output_id` and `batches` (its values joined with `" | "`), an older
#'   state of tflplanner.  One definition for each name, in the order the
#'   names first appear in the report list, each holding its reports in
#'   the report list's order; a name is kept as it was (the check flags one
#'   that breaks the rule).  `attr(, "migrated")` is how many.
#' * `tfl_check_runs()` gives one row for each problem of each definition
#'   (`definition`, `field`, `problem`, `level`): the name rule (ASCII
#'   letters, digits, `_` and `-`; it starts with a letter; at most 40
#'   characters); two names the same ignoring case; an id the study does not
#'   have, an id twice, an empty list, `all` written as a list or as `All`;
#'   an option this tflspec does not know, or a value it does not take; and
#'   "toc.yml sets: is ignored (runs.yml exists)".  [tfl_check_spec()] adds
#'   these rows.
#' * `tfl_runs_text()` is the text of the file: the same definitions give the
#'   same bytes, names and ids quoted, the ids in the order of the report list
#'   (`order`, by default the one `tfl_read_runs()` found), the keys that are
#'   not known kept as read.
#' * `tfl_write_runs()` writes the text.  It stops for a name that is new or
#'   changed and breaks the rule or repeats another.  No definition: the file
#'   is removed.  Until tflspec writes the programs itself (G5), the study's
#'   programs are regenerated through tflplanner: the manifest of
#'   `programs/batch.R` holds the resolved definitions, so a changed file
#'   makes the programs stale (the runner refuses them).
#'
#' @param study_dir The study folder (the one with study.yml).
#' @param outputs `NULL`, or a data frame with the columns `output_id` (or
#'   `id`) and `batches`: the membership of an older state, read instead of
#'   the files.
#' @param runs The definitions, as `tfl_read_runs()` returns them: a list,
#'   each element a list with `name`, `reports` and, when written, `mode`
#'   and `copy_data`.
#' @param spec The study read with [tfl_read_study_spec()], or its folder:
#'   what ids the study has.
#' @param order The ids of the report list, in order: the ids of a
#'   definition are written in this order.  `NULL`: as they are.
#' @return `tfl_runs_file()`: the path.  `tfl_read_runs()` and
#'   `tfl_runs_migrate()`: the definitions (class `tfl_runs`, a list);
#'   attributes `"file"`, `"order"` (the report list's ids), `"migrated"`
#'   (a count, when they were migrated) and `"read_problems"` (a data frame).
#'   `tfl_check_runs()`: a data frame, no row when all is well.
#'   `tfl_runs_text()`: the lines of the file.  `tfl_write_runs()`: the path
#'   written, invisibly (`NULL` when the file was removed).
#' @examples
#' d <- tempfile()
#' dir.create(file.path(d, "spec"), recursive = TRUE)
#' runs <- list(list(name = "Topline", reports = c("T-1", "T-2"), mode = "all"))
#' cat(tfl_runs_text(runs), sep = "\n")
#' @name tfl_read_runs
NULL

# the options of a definition, in the form of tfl_arg_defs()'s table (the
# exported tfl_run_options() comes with the generic fields, slice C)
.runs_options <- function() {
  df <- data.frame(
    name = c("mode", "copy_data"),
    label_en = c("What the run makes", "Keep the data in the batch folder"),
    label_ja = NA_character_,
    help_en = c("all: the ARD, then the reports; ard: the ARD only.",
                "Copy the data folders into the batch folder."),
    help_ja = NA_character_,
    step = "run", group = NA_character_,
    input = "pick", type = c("enum[all|ard]", "logical"),
    required = FALSE, default_kind = "value",
    tier = "key", description = c("What the run makes.", "Copy the data."),
    stringsAsFactors = FALSE)
  df$default <- list("all", FALSE)
  df$values <- list(c("all", "ard"), c(TRUE, FALSE))
  df
}

.runs_modes <- c("all", "ard")

# the keys of a definition this tflspec knows
.runs_keys <- c("name", "reports", "mode", "copy_data")

# the name rule: ASCII letters, digits, _ and -; starts with a letter; at
# most 40 characters (the runner holds the same pattern: a test compares)
.runs_name_rx <- "^[A-Za-z][A-Za-z0-9_-]{0,39}\\z"

.runs_name_rule <- paste0("a name has ASCII letters, digits, _ and - only, ",
                          "starts with a letter, and has at most 40 characters")

.runs_name_problem <- function(x) {
  if (is.null(x) || length(x) != 1L || !is.character(x) || is.na(x)) {
    return("the name is missing")
  }
  if (!nzchar(x)) return("the name is empty")
  if (!grepl(.runs_name_rx, x, perl = TRUE)) {
    return(paste0("the name '", x, "' breaks the rule: ", .runs_name_rule))
  }
  ""
}

#' @rdname tfl_read_runs
#' @export
tfl_runs_file <- function(study_dir) {
  file.path(study_dir, "spec", "runs.yml")
}

# the scalars of the file stay text (a name `No`, an id `14.10` or `001`):
# only a sequence keeps its marker, so that `reports: all` and
# `reports: [all]` and an empty list are told apart
.runs_handlers <- function() {
  same <- function(x) as.character(x)
  tags <- c("bool#yes", "bool#no", "int", "int#hex", "int#oct", "float#fix",
            "float#exp", "float#base60", "float#inf", "float#neginf",
            "float#nan", "timestamp#ymd", "timestamp#iso8601",
            "timestamp#spaced")
  h <- stats::setNames(rep(list(same), length(tags)), tags)
  h$seq <- function(x) structure(x, class = "runs_seq")
  h
}

# a parsed value without the sequence marker
.runs_plain <- function(x) {
  if (is.list(x)) {
    nm <- names(x)
    x <- lapply(unclass(x), .runs_plain)
    if (!is.null(nm)) names(x) <- nm
  }
  x
}

.runs_one_line <- function(x) trimws(gsub("[[:space:]]+", " ", x))

.runs_prob <- function(definition = character(), field = character(),
                       problem = character(), level = character()) {
  data.frame(definition = as.character(definition), field = as.character(field),
             problem = as.character(problem),
             level = if (length(level)) as.character(level) else
               rep("error", length(definition)),
             stringsAsFactors = FALSE)
}

# the definitions with their attributes
.runs_new <- function(defs, file = NA_character_, order = NULL, migrated = NULL,
                      read_problems = .runs_prob()) {
  structure(defs, class = "tfl_runs", file = file, order = order,
            migrated = migrated, read_problems = read_problems)
}

# the ids of the report list (toc.yml's `outputs:`), in order
.runs_toc_order <- function(study_dir) {
  toc <- .runs_read_yaml(file.path(study_dir, "spec", "toc.yml"))
  if (inherits(toc, "error") || !is.list(toc)) return(character())
  ids <- vapply(toc$outputs %||% list(), function(e) {
    id <- if (is.list(e)) e$id else NULL
    if (length(id) == 1L && !is.na(id)) as.character(id) else NA_character_
  }, "")
  ids[!is.na(ids) & nzchar(ids)]
}

# a YAML file as text-valued lists; the error, when it cannot be read
.runs_read_yaml <- function(f) {
  if (!file.exists(f)) return(NULL)
  tryCatch(.runs_plain(yaml::read_yaml(f, handlers = .runs_handlers())),
           error = function(e) e)
}

#' @rdname tfl_read_runs
#' @export
tfl_read_runs <- function(study_dir) {
  f <- tfl_runs_file(study_dir)
  if (!file.exists(f)) return(tfl_runs_migrate(study_dir))
  order <- .runs_toc_order(study_dir)
  y <- tryCatch(yaml::read_yaml(f, handlers = .runs_handlers()),
                error = function(e) e)
  if (inherits(y, "error")) {
    return(.runs_new(list(), f, order, read_problems = .runs_prob(
      "", "file", paste("runs.yml cannot be read as YAML:",
                        .runs_one_line(conditionMessage(y))))))
  }
  probs <- .runs_prob()
  add <- function(def, field, problem) {
    probs <<- rbind(probs, .runs_prob(def, field, problem))
  }
  entries <- list()
  if (is.null(y)) {
    # (an empty file, or only comments: no definition)
  } else if (!is.list(y) || inherits(y, "runs_seq") || is.null(names(y))) {
    add("", "file", "runs.yml must be a map with a `runs:` list")
  } else {
    r <- y$runs
    if (is.null(r) && !"runs" %in% names(y)) {
      add("", "file", "runs.yml has no `runs:` list")
    } else if (is.null(r)) {
      # (`runs:` with nothing under it: no definition)
    } else if (!inherits(r, "runs_seq")) {
      add("", "runs", "`runs:` must be a list of definitions")
    } else {
      entries <- unclass(r)
    }
  }
  defs <- list()
  for (i in seq_along(entries)) {
    e <- entries[[i]]
    label <- paste0("(entry ", i, ")")
    if (!is.list(e) || inherits(e, "runs_seq") || is.null(names(e))) {
      add(label, "", "a definition is a map with `name:` and `reports:`")
      next
    }
    nm <- e$name
    name <- if (is.character(nm) && length(nm) == 1L) nm else NA_character_
    label <- if (is.na(name)) label else name
    d <- list(name = name)
    # the ids: a list of texts, or one scalar (the word `all` or one id)
    r <- e$reports
    if (inherits(r, "runs_seq")) {
      ok <- vapply(unclass(r), function(v) is.character(v) && length(v) == 1L, NA)
      if (!all(ok)) add(label, "reports", "an id is a text")
      ids <- as.character(unlist(unclass(r)[ok]))
      if (length(ids) == 1L && identical(ids, "all")) {
        add(label, "reports", "`[all]` is read as the word all: write `reports: all`")
      }
      d$reports <- ids
    } else if (!is.null(r)) {
      if (is.character(r) && length(r) == 1L) {
        d$reports <- r
      } else {
        add(label, "reports", "`reports:` is a list of ids, or the word all")
        d$reports <- character()
      }
    }
    if (!is.null(e$mode)) d$mode <- e$mode
    if (!is.null(e$copy_data)) {
      cd <- e$copy_data
      d$copy_data <- if (is.character(cd) && length(cd) == 1L &&
                         tolower(cd) %in% c("true", "false")) {
        identical(tolower(cd), "true")
      } else cd
    }
    for (k in setdiff(names(e), .runs_keys)) d[[k]] <- .runs_plain(e[[k]])
    defs[[length(defs) + 1L]] <- d
  }
  .runs_new(defs, f, order, read_problems = probs)
}

# the membership an older carrier holds, per report
.runs_old_membership <- function(study_dir, outputs) {
  split <- function(v) {
    if (is.null(v) || (length(v) == 1L && is.na(v))) return(character())
    x <- trimws(strsplit(as.character(v), "|", fixed = TRUE)[[1L]])
    x[!is.na(x) & nzchar(x)]
  }
  if (!is.null(outputs)) {
    if (!is.data.frame(outputs) || !"batches" %in% names(outputs)) {
      stop("tfl_runs_migrate(): `outputs` is a data frame with the column `batches`.",
           call. = FALSE)
    }
    idc <- intersect(c("output_id", "id"), names(outputs))
    if (!length(idc)) {
      stop("tfl_runs_migrate(): `outputs` needs the column `output_id`.", call. = FALSE)
    }
    id <- as.character(outputs[[idc[[1L]]]])
    return(list(ids = id, sets = lapply(as.character(outputs$batches), split)))
  }
  toc <- .runs_read_yaml(file.path(study_dir, "spec", "toc.yml"))
  ids <- character()
  sets <- list()
  if (is.list(toc) && !inherits(toc, "error")) {
    for (e in toc$outputs %||% list()) {
      if (!is.list(e) || length(e$id) != 1L) next
      s <- unlist(e$sets)
      ids <- c(ids, as.character(e$id))
      sets[[length(sets) + 1L]] <- as.character(s[!is.na(s) & nzchar(s)])
    }
  }
  if (length(unlist(sets))) return(list(ids = ids, sets = sets))
  # a workbook study: the planner sheet's `batches` column
  books <- list.files(file.path(study_dir, "spec"), "[.]xlsx$", full.names = TRUE)
  books <- books[!startsWith(basename(books), "~$")]
  d <- if (length(books)) tryCatch(.conv_read_planner_sheet(books), error = function(e) NULL)
  if (is.data.frame(d) && "batches" %in% names(d) && nrow(d)) {
    return(list(ids = as.character(d$output_id), sets = lapply(d$batches, split)))
  }
  list(ids = ids, sets = sets)
}

#' @rdname tfl_read_runs
#' @export
tfl_runs_migrate <- function(study_dir, outputs = NULL) {
  m <- .runs_old_membership(study_dir, outputs)
  names_in_order <- unique(unlist(m$sets))
  defs <- lapply(names_in_order, function(nm) {
    has <- vapply(m$sets, function(s) nm %in% s, NA)
    list(name = nm, reports = unique(m$ids[has]))
  })
  order <- if (!is.null(outputs)) m$ids[!is.na(m$ids)] else .runs_toc_order(study_dir)
  .runs_new(defs, tfl_runs_file(study_dir), order, migrated = length(defs))
}

# the ids of a study, in the report list's order (toc.yml), then the reports
# that only have a file; `spec`: a study read, or its folder
.runs_report_ids <- function(spec) {
  dir <- if (inherits(spec, "tfl_study_spec")) spec$dir else spec
  toc_ids <- character()
  if (inherits(spec, "tfl_study_spec")) {
    toc_ids <- vapply(spec$toc$outputs %||% list(), function(e) {
      id <- if (is.list(e)) e$id else NULL
      if (length(id) == 1L && !is.na(id)) as.character(id) else NA_character_
    }, "")
    toc_ids <- toc_ids[!is.na(toc_ids)]
  } else if (!is.null(dir)) {
    toc_ids <- .runs_toc_order(dir)
  }
  file_ids <- character()
  if (!is.null(dir) && dir.exists(.outputs_dir(dir))) {
    outs <- tryCatch(suppressWarnings(tfl_read_outputs(dir, skip_bad = TRUE)),
                     error = function(e) list())
    files <- list.files(.outputs_dir(dir), "[.]ya?ml$", recursive = TRUE)
    file_ids <- unique(c(names(outs), sub("[.]ya?ml$", "", basename(files))))
  }
  unique(c(toc_ids, file_ids))
}

# does toc.yml still hold `sets:`?
.runs_toc_has_sets <- function(dir) {
  toc <- .runs_read_yaml(file.path(dir, "spec", "toc.yml"))
  is.list(toc) && !inherits(toc, "error") &&
    any(vapply(toc$outputs %||% list(), function(e) is.list(e) && !is.null(e$sets), NA))
}

#' @rdname tfl_read_runs
#' @export
tfl_check_runs <- function(runs, spec = NULL) {
  defs <- unclass(runs)
  attributes(defs) <- NULL
  out <- .runs_prob()
  add <- function(def, field, problem, level = "error") {
    out <<- rbind(out, .runs_prob(def, field, problem, level))
  }
  rp <- attr(runs, "read_problems")
  if (is.data.frame(rp) && nrow(rp)) out <- rbind(out, rp)
  known <- if (!is.null(spec)) .runs_report_ids(spec)
  label <- vapply(seq_along(defs), function(i) {
    nm <- defs[[i]]$name
    if (length(nm) == 1L && is.character(nm) && !is.na(nm) && nzchar(nm)) nm else
      paste0("(entry ", i, ")")
  }, "")
  nm_of <- vapply(defs, function(d) {
    nm <- d$name
    if (length(nm) == 1L && is.character(nm) && !is.na(nm)) nm else NA_character_
  }, "")
  for (i in seq_along(defs)) {
    d <- defs[[i]]
    lb <- label[[i]]
    p <- .runs_name_problem(d$name)
    if (nzchar(p)) add(lb, "name", p)
    # two names the same ignoring case
    if (!is.na(nm_of[[i]]) && nzchar(nm_of[[i]])) {
      same <- which(tolower(nm_of) == tolower(nm_of[[i]]))
      if (length(same) > 1L) {
        add(lb, "name", paste0("the name is used twice (names are compared ignoring case): ",
                               paste(nm_of[setdiff(same, i)], collapse = ", ")))
      }
    }
    r <- d$reports
    if (is.null(r) || !length(r)) {
      add(lb, "reports", "the report list is empty")
    } else if (!is.character(r) || anyNA(r) || any(!nzchar(r))) {
      add(lb, "reports", "an id is a text")
    } else if (identical(r, "all")) {
      # (the word: every report of the study)
    } else {
      w <- which(tolower(r) == "all")
      if (length(w) && length(r) == 1L) {
        add(lb, "reports", paste0("write the word in lower case, `reports: all` (not `",
                                  r, "`)"))
      } else if (length(w)) {
        add(lb, "reports", paste("`all` stands for every report: write `reports: all`,",
                                 "with no other id"))
      }
      ids <- if (length(w)) r[-w] else r
      for (x in unique(ids[duplicated(ids)])) {
        add(lb, "reports", paste0("the id ", x, " is listed twice"))
      }
      if (!is.null(known)) {
        for (x in unique(setdiff(ids, known))) {
          add(lb, "reports", paste0("the study has no report ", x))
        }
      }
    }
    if (!is.null(d$mode) && !(is.character(d$mode) && length(d$mode) == 1L &&
                              d$mode %in% .runs_modes)) {
      add(lb, "mode", paste0("mode is one of: ", paste(.runs_modes, collapse = ", ")))
    }
    if (!is.null(d$copy_data) && !(is.logical(d$copy_data) && length(d$copy_data) == 1L &&
                                   !is.na(d$copy_data))) {
      add(lb, "copy_data", "copy_data is true or false")
    }
    for (k in setdiff(names(d), .runs_keys)) {
      add(lb, k, paste0("`", k, "` is not an option this tflspec knows: update tflspec"))
    }
  }
  # toc.yml's `sets:` next to a runs file
  dir <- if (inherits(spec, "tfl_study_spec")) spec$dir else if (is.character(spec)) spec
  if (!is.null(dir) && file.exists(tfl_runs_file(dir)) && .runs_toc_has_sets(dir)) {
    add("", "sets", "toc.yml sets: is ignored (runs.yml exists)", "warning")
  }
  rownames(out) <- NULL
  out
}

# a text as a YAML double-quoted string (R's escapes are YAML's)
.runs_q <- function(x) encodeString(as.character(x), quote = "\"")

#' @rdname tfl_read_runs
#' @export
tfl_runs_text <- function(runs, order = attr(runs, "order")) {
  defs <- unclass(runs)
  attributes(defs) <- NULL
  lines <- "# spec/runs.yml: the study's run definitions"
  if (!length(defs)) return(c(lines, "runs: []"))
  lines <- c(lines, "runs:")
  for (d in defs) {
    nm <- d$name
    lines <- c(lines, paste0("  - name: ", .runs_q(if (length(nm) == 1L && !is.na(nm)) nm else "")))
    r <- d$reports
    if (identical(r, "all")) {
      lines <- c(lines, "    reports: all")
    } else if (!is.null(r)) {
      r <- as.character(r)
      if (length(order) && length(r)) {
        pos <- match(r, order)
        r <- r[order(is.na(pos), pos, seq_along(r))]
      }
      lines <- c(lines, paste0("    reports: [", paste(.runs_q(r), collapse = ", "), "]"))
    }
    if (!is.null(d$mode)) {
      m <- d$mode
      lines <- c(lines, paste0("    mode: ", if (is.character(m) && length(m) == 1L &&
                                                 m %in% .runs_modes) m else
                                  .runs_q(paste(as.character(unlist(m)), collapse = ", "))))
    }
    if (!is.null(d$copy_data)) {
      cd <- d$copy_data
      lines <- c(lines, paste0("    copy_data: ", if (is.logical(cd) && length(cd) == 1L &&
                                                       !is.na(cd)) tolower(as.character(cd)) else
                                 .runs_q(paste(as.character(unlist(cd)), collapse = ", "))))
    }
    for (k in setdiff(names(d), .runs_keys)) {
      y <- yaml::as.yaml(stats::setNames(list(.runs_plain(d[[k]])), k))
      y <- strsplit(sub("\n$", "", y), "\n", fixed = TRUE)[[1L]]
      lines <- c(lines, paste0("    ", y))
    }
  }
  lines
}

#' @rdname tfl_read_runs
#' @export
tfl_write_runs <- function(runs, study_dir) {
  f <- tfl_runs_file(study_dir)
  defs <- unclass(runs)
  attributes(defs) <- NULL
  if (!length(defs)) {
    unlink(f)
    return(invisible(NULL))
  }
  # a name that is new or changed must keep the rule and be its own
  on_disk <- if (file.exists(f)) {
    vapply(unclass(tfl_read_runs(study_dir)), function(d) {
      if (is.character(d$name) && length(d$name) == 1L) d$name else NA_character_
    }, "")
  } else character()
  p <- tfl_check_runs(runs)
  p <- p[p$field == "name" & !p$definition %in% on_disk, , drop = FALSE]
  if (nrow(p)) {
    stop("tfl_write_runs(): ", paste0(p$definition, ": ", p$problem, collapse = "; "),
         call. = FALSE)
  }
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  txt <- paste0(paste(tfl_runs_text(runs), collapse = "\n"), "\n")
  # (bytes: no CRLF on Windows)
  writeBin(charToRaw(enc2utf8(txt)), f)
  invisible(f)
}
