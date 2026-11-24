# ---- the runner ------------------------------------------------------------
# (tflplanner writes the lists above for the study; this part is the same
# for every study)

.rscript <- file.path(R.home("bin"), "Rscript")

# logrx's "Errors:" / "Warnings:" section: the tab-indented lines under it
.batch_section <- function(lines, head) {
  i <- match(head, lines)
  if (is.na(i)) return(character())
  out <- character()
  for (l in lines[-seq_len(i)]) {
    if (!startsWith(l, "\t")) break
    if (nzchar(trimws(l))) out <- c(out, trimws(l))
  }
  out
}

# the end of a run started from Rscript with a non-zero status (one place,
# so the tests that source this file can catch it instead of ending R)
.batch_quit <- function(status) quit(save = "no", status = status)

# R's message prefixes, in English and Japanese (Error: U+30A8 U+30E9 U+30FC;
# Warning: U+8B66 U+544A; "Warning messages:" is U+8B66 U+544A U+30E1 U+30C3
# U+30BB U+30FC U+30B8 and a colon)
.batch_re_error <- "^(Error|エラー)"
.batch_re_warning <- "^(Warning|警告)"
.batch_re_warnings <- "^(Warning messages:|警告メッセージ:)"

# the issue lines of an R CMD BATCH log: ^Error lines, ^Warning lines (the
# numbered lines under "Warning messages:" one each), and "There were N
# warnings" as N rows (English and Japanese).  A data frame: level, text.
.batch_log_issues <- function(lines) {
  lv <- character()
  tx <- character()
  add <- function(level, text) {
    lv <<- c(lv, level)
    tx <<- c(tx, text)
  }
  n <- length(lines)
  for (i in seq_len(n)) {
    l <- lines[i]
    if (grepl(.batch_re_error, l)) {
      add("error", l)
    } else if (grepl(.batch_re_warnings, l)) {
      k <- i + 1L
      got <- 0L
      while (k <= n && grepl("^[0-9]+: ", lines[k])) {
        add("warning", lines[k])
        got <- got + 1L
        k <- k + 1L
      }
      if (!got) add("warning", l)
    } else if (grepl("^There w(ere|as) [0-9]+", l)) {
      N <- as.integer(sub("^There w(?:ere|as) ([0-9]+).*$", "\\1", l, perl = TRUE))
      for (j in seq_len(N)) add("warning", l)
    } else if (grepl(.batch_re_warning, l)) {
      # (the Japanese count line starts with the word for warning and holds
      # the number and warnings(): N rows, as the English one)
      N <- 1L
      if (!grepl("^Warning", l) && grepl("[0-9]+.*warnings[(][)]", l)) {
        N <- as.integer(sub("^[^0-9]*([0-9]+).*$", "\\1", l))
      }
      for (j in seq_len(max(N, 1L, na.rm = TRUE))) add("warning", l)
    }
  }
  data.frame(level = lv, text = tx, stringsAsFactors = FALSE)
}

# one program in its own R process, from the working directory, with its log
.batch_run <- function(part, prog, log_dir, engine) {
  log <- file.path(log_dir, sub("[.][Rr]$", ".log", basename(prog)))
  t0 <- Sys.time()
  # no program to run: the study was not saved since, or its ARD
  # definition does not hold (saving then writes no ARD program)
  if (!file.exists(prog)) {
    note <- paste0("no program ", prog, ": save the study (an ARD definition ",
                   "that does not hold writes none; see the Review tab)")
    cat(sprintf("%-5s %-4s %-32s %s\n", "ERROR", part, basename(prog), note))
    r <- data.frame(part = part, program = prog, status = "ERROR",
                    errors = 1L, warnings = 0L, note = note,
                    started = format(t0, "%Y-%m-%d %H:%M:%S"), seconds = 0,
                    md5 = NA_character_, log = NA_character_,
                    stringsAsFactors = FALSE)
    return(.batch_with_issues(r, "error", note))
  }
  if (engine == "logrx") {
    expr <- sprintf("logrx::axecute('%s', log_name = '%s', log_path = '%s')",
                    prog, basename(log), log_dir)
    # what R says on stderr is kept beside the log: an error that stops the
    # program before logrx can log it (a package too old) is only there
    err <- sub("[.]log$", ".err", log)
    rc <- system2(.rscript, c("-e", shQuote(expr)), stdout = FALSE,
                  stderr = err)
    lines <- if (file.exists(log)) readLines(log, warn = FALSE) else character()
    errs <- .batch_section(lines, "Errors:")
    warns <- .batch_section(lines, "Warnings:")
    if (!identical(rc, 0L) && !length(errs) && file.exists(err)) {
      e <- grep(.batch_re_error, readLines(err, warn = FALSE), value = TRUE)
      if (length(e)) errs <- e[length(e)]
    }
    if (file.exists(err) && !file.size(err)) unlink(err)
    iss <- data.frame(level = c(rep("error", length(errs)), rep("warning", length(warns))),
                      text = c(errs, warns), stringsAsFactors = FALSE)
  } else {
    rc <- system2(file.path(R.home("bin"), "R"),
                  c("CMD", "BATCH", "--no-save", "--no-restore", "--quiet",
                    shQuote(prog), shQuote(log)))
    lines <- if (file.exists(log)) readLines(log, warn = FALSE) else character()
    iss <- .batch_log_issues(lines)
    errs <- iss$text[iss$level == "error"]
    warns <- iss$text[iss$level == "warning"]
  }
  status <- if (identical(rc, 0L)) "OK" else "ERROR"
  secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  cat(sprintf("%-5s %-4s %-32s %6.1fs\n", status, part, basename(prog), secs))
  r <- data.frame(part = part, program = prog, status = status,
                  errors = length(errs), warnings = length(warns),
                  note = if (length(errs)) errs[1L] else "",
                  started = format(t0, "%Y-%m-%d %H:%M:%S"), seconds = secs,
                  md5 = unname(tools::md5sum(prog)), log = log,
                  stringsAsFactors = FALSE)
  attr(r, "issues") <- iss
  attr(r, "rc") <- rc
  r
}

# a run row that carries its issues (level, text) as an attribute
.batch_with_issues <- function(r, level, text) {
  attr(r, "issues") <- data.frame(level = level, text = text,
                                  stringsAsFactors = FALSE)
  r
}

# an ARD program that failed: the study ARD keeps that output's rows, and
# its error is recorded
.batch_ard_error <- function(output_id, note) {
  sf <- file.path(dirname(.batch_ard), "ard_status.csv")
  row <- data.frame(output_id = output_id, definition = "",
                    built = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                    rows = "", error = if (nzchar(note)) note else "failed",
                    stringsAsFactors = FALSE)
  st <- if (file.exists(sf)) utils::read.csv(sf, colClasses = "character")
  if (!is.null(st) && nrow(st)) {
    for (k in setdiff(names(row), names(st))) st[[k]] <- ""
    row <- rbind(st[st$output_id != output_id, names(row), drop = FALSE], row)
  }
  dir.create(dirname(sf), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(row, sf, row.names = FALSE)
}

.batch_env <- function() environment(.batch_env)

# a path with / for \ and no leading ./
.batch_norm <- function(p) sub("^(\\./+)+", "", gsub("\\\\", "/", p))

# why a path is not inside the study ("" when it is): judged by its
# segments -- absolute (/, \, C:, C:foo, ~) or a .. segment
.batch_bad_path <- function(p) {
  q <- gsub("\\\\", "/", p)
  if (is.na(q) || !nzchar(q)) return("is empty")
  if (startsWith(q, "/") || grepl("^[A-Za-z]:", q) || startsWith(q, "~")) {
    return("is an absolute path")
  }
  if (".." %in% strsplit(q, "/", fixed = TRUE)[[1L]]) return("has a .. segment")
  ""
}

# the same test for a string literal in the study's code, where a string is
# often not a path: only what reads as one (/dir, \\server, C:/, ~/, a ..
# segment between slashes, or a start in a data folder)
.batch_bad_literal <- function(s) {
  if (is.na(s) || nchar(s) < 2L || grepl("\n", s, fixed = TRUE)) return("")
  q <- gsub("\\\\", "/", s)
  # an absolute path needs a path shape: /dir/, //server/, C:/, ~/ (so "/$",
  # "/N", "/100 PY", "N:" and two backslashes are not paths)
  if (grepl("^/[^/[:space:]]+/", q) || grepl("^//[^/]+/", q) ||
      grepl("^[A-Za-z]:/", q) || grepl("^~/", q)) {
    return("an absolute path")
  }
  # a .. segment counts only in a string that holds a separator (the string
  # ".." alone is a prefix, as in study_helpers.R)
  if (grepl("/", q, fixed = TRUE) && ".." %in% strsplit(q, "/", fixed = TRUE)[[1L]]) {
    return("a path with a .. segment")
  }
  n <- tolower(gsub("/+", "/", .batch_norm(s)))
  for (d in tolower(unname(.batch_data_dirs))) {
    if (identical(n, d) || startsWith(n, paste0(d, "/"))) {
      return("a path in a data folder (it bypasses the data root)")
    }
  }
  ""
}

# What stops a run before any folder is made, one line each (empty: all well)
.batch_check_paths <- function(selected) {
  out <- character()
  allowed <- if (identical(.batch_kind, "preview")) {
    as.character(unlist(.batch_paths$data, use.names = FALSE))
  } else character()
  if (!identical(.batch_ard, "output/ard/ard.rds")) {
    out <- c(out, sprintf(paste0("The study ARD is '%s': it must be output/ard/ard.rds, ",
                                 "which the reports read (ichirio/tflspec#914)."), .batch_ard))
  }
  for (p in unique(unlist(selected, use.names = FALSE))) {
    for (f in .batch_data[[p]]) {
      if (f %in% allowed) next
      why <- .batch_bad_path(f)
      if (nzchar(why)) {
        out <- c(out, sprintf(paste0("Data path '%s' (%s) %s, outside the study folder: ",
                                     "move the file under data/ or give it through the data root."),
                              f, p, why))
        next
      }
      if (startsWith(.batch_norm(f), "input/ard/")) next
      v <- unname(.batch_data_vars[c(f, .batch_norm(f))])
      if (!any(!is.na(v) & nzchar(v))) {
        keys <- names(.batch_data_dirs)
        keys <- if (length(keys) > 1L) {
          paste0(paste(keys[-length(keys)], collapse = ", "), " or ", keys[length(keys)])
        } else keys
        out <- c(out, sprintf(paste0("Data path '%s' (%s) is not mapped to a data-folder variable: ",
                                     "add its folder as a data key of the layout (%s in study.yml), ",
                                     "or move the file under one of: %s."),
                              f, p, keys, paste(unname(.batch_data_dirs), collapse = ", ")))
      }
    }
  }
  for (s in .batch_sources) {
    why <- .batch_bad_path(s)
    if (!nzchar(why) &&
        !strsplit(.batch_norm(s), "/", fixed = TRUE)[[1L]][1L] %in% c("programs", "spec", "input")) {
      why <- "is not inside programs/, spec/ or input/"
    }
    if (nzchar(why)) {
      out <- c(out, sprintf(paste0("Study source file '%s' %s: it must lie inside programs/, ",
                                   "spec/ or input/ (they are copied into the batch folder)."), s, why))
    }
  }
  for (f in .batch_check_code[file.exists(.batch_check_code)]) {
    pd <- tryCatch(utils::getParseData(parse(
      text = readLines(f, warn = FALSE, encoding = "UTF-8"), keep.source = TRUE)),
      error = function(e) NULL)
    if (is.null(pd)) {
      out <- c(out, sprintf("%s: cannot be read as R code, so it cannot be checked for paths.", f))
      next
    }
    # a line that ends in the comment `# tfl: not a path` is not scanned
    marked <- pd$line1[pd$token == "COMMENT" &
                         grepl("^#\\s*tfl:\\s*not a path\\s*$", pd$text)]
    pd <- pd[pd$token == "STR_CONST" & !pd$line1 %in% marked, , drop = FALSE]
    for (i in seq_len(nrow(pd))) {
      v <- tryCatch(parse(text = pd$text[i], keep.source = FALSE)[[1L]],
                    error = function(e) NA_character_)
      if (!is.character(v) || length(v) != 1L) next
      why <- .batch_bad_literal(v)
      if (nzchar(why)) {
        out <- c(out, sprintf(paste0("%s:%d: the string %s is %s; a run touches only its batch ",
                                     "folder and the data root. If it is not a path, end the ",
                                     "line with the comment  # tfl: not a path"),
                              f, pd$line1[i], substr(pd$text[i], 1L, 60L), why))
      }
    }
  }
  out
}

# What stops a run, said once: to the screen and to stderr, then the run ends.
# Every entry (a hand Rscript, tfl_run_study(), the Runs tab) prints this text.
.batch_refuse <- function(...) {
  msg <- c(...)
  cat(msg, sep = "\n")
  message(paste(msg, collapse = "\n"))
  .batch_quit(1L)
  stop(paste(msg, collapse = "\n"), call. = FALSE)
}

# The name rule of a run definition: ASCII letters, digits, _ and -; starts
# with a letter; at most 40 characters (tflspec's tfl_check_runs() holds the
# same pattern, and a test compares them).  The folder's name is made of it.
.batch_name_rx <- "^[A-Za-z][A-Za-z0-9_-]{0,39}\\z"

# The run definition a first run starts from: `run` (--run=<name>, exact), or
# the only one the study has.  Refused, before anything is made: none, several
# and no --run, an unknown name, a definition with a problem, a name that
# breaks the rule, an option that is not one, a listed report with no program.
.batch_pick_run <- function(run) {
  runs <- .batch_runs
  nms <- vapply(runs, function(r) r$name, "")
  if (is.null(run)) {
    if (!length(runs)) {
      .batch_refuse("The run was not started: the study has no run definition.",
                    paste0("  Save one (tflplanner's Runs tab), or write it in spec/runs.yml and ",
                           "regenerate the programs; then start again with --run=<name>."))
    }
    if (length(runs) > 1L) {
      .batch_refuse("The run was not started: the study has several run definitions; say which with --run=<name>:",
                    paste0("  ", nms))
    }
    i <- 1L
  } else {
    i <- which(nms == run)
    if (!length(i)) {
      near <- nms[tolower(nms) == tolower(run)]
      .batch_refuse(sprintf("The run was not started: no run definition named '%s'.", run),
                    if (length(near)) sprintf("  Names are exact: did you mean '%s'?", near[[1L]]),
                    if (length(nms)) paste0("  The study's: ", paste(nms, collapse = ", ")))
    }
    if (length(i) > 1L) {
      .batch_refuse(sprintf("The run was not started: two run definitions are named '%s'.", run))
    }
  }
  d <- runs[[i]]
  bad <- character()
  if (nzchar(d$problem)) bad <- c(bad, d$problem)
  if (!grepl(.batch_name_rx, d$name, perl = TRUE)) {
    bad <- c(bad, sprintf(paste0("the name '%s' breaks the rule: ASCII letters, digits, _ and -; ",
                                 "it starts with a letter; at most 40 characters"), d$name))
  }
  if (!d$mode %in% c("all", "ard")) {
    bad <- c(bad, sprintf("mode '%s' is not one of: all, ard", d$mode))
  }
  gone <- setdiff(d$reports, c(.batch_ard_outputs, .batch_report_ids))
  if (length(gone)) {
    bad <- c(bad, paste0("no program for ", paste(gone, collapse = ", "),
                         " (save the study to write the programs)"))
  }
  if (length(bad)) {
    .batch_refuse(sprintf("The run was not started: the run definition '%s' has a problem.", d$name),
                  paste0("  ", bad))
  }
  d
}

# A run definition written beside a batch folder's results (run_definition.yml,
# one the stage wrote): name, mode, copy_data, stamp, reports.  NULL when
# there is no file.
.batch_read_definition <- function(f) {
  if (!file.exists(f)) return(NULL)
  x <- readLines(f, warn = FALSE, encoding = "UTF-8")
  val <- function(s) {
    s <- trimws(s)
    if (nchar(s) >= 2L && startsWith(s, "\"") && endsWith(s, "\"")) {
      s <- gsub("\\\\(.)", "\\1", substr(s, 2L, nchar(s) - 1L))
    }
    s
  }
  one <- function(key) {
    v <- grep(paste0("^", key, ":"), x, value = TRUE)
    if (length(v)) val(sub(paste0("^", key, ":"), "", v[[1L]])) else NA_character_
  }
  i <- match("reports:", x)
  ids <- if (is.na(i)) character() else {
    r <- x[-seq_len(i)]
    r <- r[startsWith(r, "  - ")]
    vapply(r, function(l) val(substring(l, 5L)), "", USE.NAMES = FALSE)
  }
  list(name = one("name"), mode = one("mode"),
       copy_data = identical(one("copy_data"), "true"), stamp = one("stamp"),
       reports = ids)
}

# the lines of run_definition.yml: the resolved definition and the stamp
.batch_definition_lines <- function(d) {
  q <- function(x) encodeString(x, quote = "\"")
  c(paste0("name: ", q(d$name)),
    paste0("mode: ", d$mode),
    paste0("copy_data: ", if (isTRUE(d$copy_data)) "true" else "false"),
    paste0("stamp: ", q(.batch_stamp)),
    "reports:",
    paste0("  - ", q(d$reports)))
}

# The definition a run again of a batch folder uses: the folder's own
# run_definition.yml (the study's list is not looked at), else the manifest's
.batch_rerun_definition <- function(run) {
  d <- .batch_read_definition("run_definition.yml")
  if (!is.null(d) && identical(d$name, run)) return(d)
  hit <- Filter(function(r) identical(r$name, run), .batch_runs)
  if (length(hit)) return(hit[[1L]])
  .batch_refuse(sprintf("No run definition named '%s' in this batch folder.", run))
}

# The run's selection: the parts, the run definition (--run=<name>) or the
# names / --exclude of a run again, checked, and the programs selected.  A
# selected report that reads the ARD of one the run does not make stops it,
# before anything is made (have_ard: the ARD of the batch folder is there, as
# in a reports-only run again).  `first`: the run from the study (the stage);
# otherwise a run again in a batch folder, which is never official.
.batch_plan <- function(parts, args = character(), have_ard = FALSE, first = TRUE) {
  data <- "--data" %in% args
  logrx <- "--logrx" %in% args
  ex_args <- grep("^--exclude=", args, value = TRUE)
  exclude <- sub("^--exclude=", "", ex_args)
  args <- args[!grepl("^--data-root=", args)]
  if (any(args == "--batch" | grepl("^--batch=", args))) {
    .batch_refuse("--batch is gone: use --run=<name> (a run definition).")
  }
  # the run definition: --run=<name>, or --run <name>
  run <- NULL
  r <- grep("^--run=", args)
  if (length(r) > 1L || (length(r) && "--run" %in% args) ||
      sum(args == "--run") > 1L) {
    .batch_refuse("--run is given twice: a run starts from one definition.")
  }
  if (length(r)) {
    run <- sub("^--run=", "", args[r])
    args <- args[-r]
  }
  b <- match("--run", args)
  if (!is.na(b)) {
    if (b == length(args)) .batch_refuse("--run needs the definition's name: --run=<name>.")
    run <- args[b + 1L]
    args <- args[-c(b, b + 1L)]
  }
  if (!is.null(run) && !nzchar(run)) {
    .batch_refuse("--run needs the definition's name: --run=<name>.")
  }
  only <- setdiff(args, c("--no-code", "--data", "--logrx", ex_args))
  # the parts: an official run starts from autoexec_all.R and the definition's
  # `mode` decides them; the study's autoexec_ard.R and autoexec_report.R are
  # not official runs (they take no --run)
  def <- NULL
  if (first && identical(.batch_kind, "run")) {
    official <- identical(parts, c("ard", "tfl"))
    if (!is.null(run) && !official) {
      .batch_refuse(paste0("The run was not started: --run goes with programs/autoexec_all.R; ",
                           "the study's autoexec_ard.R and autoexec_report.R are not official runs."))
    }
    if (official) {
      if (length(only) || length(exclude)) {
        .batch_refuse(paste0("The run was not started: an official run takes no names and no ",
                             "--exclude (a run definition says what it runs)."),
                      "  Save a definition with these reports, and start it with --run=<name>.")
      }
      def <- .batch_pick_run(run)
      parts <- if (identical(def$mode, "ard")) "ard" else c("ard", "tfl")
    }
  } else if (first) {
    if (!is.null(run)) .batch_refuse("A preview takes no --run.")
  } else if (!is.null(run)) {
    def <- .batch_rerun_definition(run)
  }
  # the ARD is made before the reports, whatever order `parts` came in
  if (!length(parts) || !all(parts %in% c("ard", "tfl"))) {
    stop("parts must be \"ard\", \"tfl\", or both (not ",
         if (length(parts)) paste(setdiff(parts, c("ard", "tfl")), collapse = ", ") else "empty", ").")
  }
  parts <- intersect(c("ard", "tfl"), parts)
  # the programs of a part, and the output id each one makes
  id_of <- function(part, progs) {
    unname(if (part == "ard") .batch_ard_outputs[basename(progs)] else
      .batch_report_ids[progs])
  }
  all_ids <- unique(stats::na.omit(c(.batch_ard_outputs, .batch_report_ids)))
  # a name -> the output ids it stands for: an id, or the file name (with or
  # without .R) of that id's ARD program or report program.  A name that
  # stands for two ids stops the run; one that stands for none runs nothing.
  resolve <- function(tokens) {
    ids <- character()
    for (tk in tokens) {
      hit <- character()
      for (part in c("ard", "tfl")) {
        progs <- if (part == "ard") names(.batch_ard_outputs) else names(.batch_report_ids)
        # (the ARD outputs are named by file name, the reports by path)
        key <- basename(progs)
        of <- unname(if (part == "ard") .batch_ard_outputs else .batch_report_ids)
        m <- key == tk | sub("[.][Rr]$", "", key) == tk
        hit <- c(hit, of[m])
      }
      hit <- unique(c(hit, all_ids[all_ids == tk]))
      # a name that stands for nothing (a typing slip) would silently run
      # nothing, or leave nothing out: said
      if (!length(hit)) {
        stop("The name '", tk, "' stands for no output.", call. = FALSE)
      }
      if (length(hit) > 1L) {
        stop("The name '", tk, "' stands for more than one output: ",
             paste(hit, collapse = ", "), ".  Name the output id.", call. = FALSE)
      }
      ids <- c(ids, hit)
    }
    ids
  }
  only_ids <- resolve(only)
  exclude_ids <- resolve(exclude)
  excluded <- character()
  # the programs of a part the run selects (the outputs named, the
  # definition's reports, less those excluded); `track`: the ones left out
  # are kept for the record
  select <- function(part, track = FALSE) {
    progs <- .batch_programs[[part]]
    of <- id_of(part, progs)
    if (length(only)) {
      progs <- progs[!is.na(of) & of %in% only_ids]
      of <- id_of(part, progs)
    }
    if (!is.null(def)) {
      keep <- !is.na(of) & of %in% def$reports
      if (track) excluded <<- c(excluded, progs[!keep])
      progs <- progs[keep]
      of <- id_of(part, progs)
    }
    gone <- !is.na(of) & of %in% exclude_ids
    if (track) excluded <<- c(excluded, progs[gone])
    progs[!gone]
  }
  # the ARD is made for the selected reports only (#790): a selected report
  # that reads the ARD of one not selected stops the run, before anything is
  # made (the app asks to tick them; here the run is started again)
  selected <- lapply(stats::setNames(parts, parts), select, track = TRUE)
  # (only an ARD program this run runs counts: a report whose ARD program is
  # left out would read older rows -- except in a batch folder that has its
  # own ARD, where a reports-only run again reads that one)
  chosen <- if ("ard" %in% parts) {
    unname(.batch_ard_outputs[basename(select("ard"))])
  } else character()
  met <- have_ard && !"ard" %in% parts
  lacking <- NULL
  for (p in selected[["tfl"]]) {
    if (met) break
    for (need in setdiff(unlist(.batch_needs[p], use.names = FALSE), chosen)) {
      lacking <- c(lacking, sprintf("%s reads the ARD of %s",
                                    .batch_report_ids[[p]], need))
    }
  }
  # a selected report whose ARD read cannot be worked out stops it too
  if (exists(".batch_unresolved", inherits = TRUE)) {
    unres <- get(".batch_unresolved", inherits = TRUE)
    for (p in intersect(selected[["tfl"]], names(unres))) {
      lacking <- c(lacking, sprintf("%s: %s", .batch_report_ids[[p]], unres[[p]]))
    }
  }
  if (length(lacking)) {
    msg <- c("The run was not started: a selected report reads the ARD of a report the run does not select, or what it reads cannot be worked out.",
             paste0("  ", lacking),
             "Select the reports it reads too (or leave this report out), fix the others, then start again.")
    cat(msg, sep = "\n")
    message(paste(msg, collapse = "\n"))
    .batch_quit(1L)
  }
  what <- if (!is.null(def)) def$mode else if (length(parts) > 1L) "all" else
    c(ard = "ard", tfl = "report")[[parts]]
  list(parts = parts, only = only, def = def, selected = selected,
       excluded = excluded, data = data || isTRUE(def$copy_data), what = what,
       logrx = logrx)
}

# run_paths.R: where the data and the study are, seen from the batch folder
# (.tfl_data_root, .tfl_study_root); "." and "../.." as defaults
.batch_read_paths <- function() {
  out <- list(data = ".", study = "../..")
  if (!file.exists("run_paths.R")) return(out)
  e <- new.env()
  tryCatch(sys.source("run_paths.R", envir = e), error = function(err) NULL)
  one <- function(nm) {
    r <- get0(nm, envir = e, inherits = FALSE)
    if (is.character(r) && length(r) == 1L && !is.na(r)) r else NULL
  }
  if (!is.null(one(".tfl_data_root"))) out$data <- one(".tfl_data_root")
  if (!is.null(one(".tfl_study_root"))) out$study <- one(".tfl_study_root")
  out
}

.batch_paths_lines <- function(data_root, study_root) {
  c(sprintf(".tfl_data_root <- %s", encodeString(data_root, quote = "\"")),
    sprintf(".tfl_study_root <- %s", encodeString(study_root, quote = "\"")))
}

.batch_is_abs <- function(p) grepl("^([A-Za-z]:|/|~)", gsub("\\\\", "/", p))

# the path from a batch folder back to the study: one .. per segment of the
# folder's path below the study, or the study's absolute path when the runs'
# root is absolute (a preview) or has a .. in it
.batch_study_root <- function(batch_dir, study_dir) {
  q <- .batch_norm(batch_dir)
  seg <- strsplit(q, "/", fixed = TRUE)[[1L]]
  seg <- seg[nzchar(seg) & seg != "."]
  if (.batch_is_abs(q) || ".." %in% seg) {
    return(normalizePath(study_dir, winslash = "/", mustWork = FALSE))
  }
  paste(rep("..", length(seg)), collapse = "/")
}

# where the runner itself reads a data file of the catalog: through the data
# root when a data-folder variable maps it; an input/ard/ file as the copy in
# the folder; a preview's own data file as it is
.batch_data_at <- function(f, root) {
  allowed <- as.character(unlist(.batch_paths$data, use.names = FALSE))
  one <- function(f) {
    n <- .batch_norm(f)
    v <- unname(.batch_data_vars[c(f, n)])
    if (f %in% allowed) f
    else if (startsWith(n, "input/ard/")) n
    else if (any(!is.na(v) & nzchar(v))) file.path(root, n)
    else f
  }
  vapply(f, one, "", USE.NAMES = FALSE)
}

# the files under a folder, as paths from the working directory; no dot
# names (.git, .backup, .Rhistory), no editor files (~, .tmp)
.batch_walk <- function(dirs) {
  f <- unlist(lapply(dirs[dir.exists(dirs)], function(d) {
    file.path(d, list.files(d, recursive = TRUE, all.files = TRUE))
  }))
  f <- as.character(f)
  f[!grepl("(^|/)[.]", f) & !grepl("(~|[.]tmp)$", f) &
      !grepl("(^|/)~[$]", f) &
      !grepl("(^|/)(Thumbs[.]db|desktop[.]ini)$", f, ignore.case = TRUE)]
}

# The data files the run's programs read (.batch_data, by the data
# catalog; under the data root), listed with their size, md5 and time in
# data_manifest.csv.  Returns the manifest.
.batch_keep_data <- function(progs, root) {
  files <- unique(unlist(.batch_data[intersect(names(.batch_data), progs)],
                         use.names = FALSE))
  files <- files[file.exists(.batch_data_at(files, root))]
  at <- .batch_data_at(files, root)
  man <- data.frame(
    path = files, kept = files,
    size = unname(file.size(at)),
    md5 = unname(tools::md5sum(at)),
    modified = format(file.mtime(at), "%Y-%m-%d %H:%M:%S"),
    stringsAsFactors = FALSE)
  utils::write.csv(man, "data_manifest.csv", row.names = FALSE)
  man
}

# The data files the run's programs read (.batch_data), read through the
# data root, with their size and md5 when the run starts, in data_read.csv:
# written by every run, kept data or not, so a later change of the data is seen.
.batch_data_read <- function(progs, root) {
  rows <- lapply(intersect(names(.batch_data), progs), function(p) {
    f <- .batch_data[[p]]
    if (!length(f)) return(NULL)
    at <- .batch_data_at(f, root)
    there <- file.exists(at)
    data.frame(program = p, file = f,
               size = ifelse(there, file.size(at), NA_real_),
               md5 = ifelse(there, unname(tools::md5sum(at)), NA_character_),
               stringsAsFactors = FALSE)
  })
  read <- do.call(rbind, c(list(data.frame(
    program = character(), file = character(), size = numeric(),
    md5 = character(), stringsAsFactors = FALSE)), rows))
  utils::write.csv(read, "data_read.csv", row.names = FALSE)
}

# The imported ARDs (input/ard/) the run's programs read, in inputs.csv:
# kept apart from the ARD the run made.
.batch_inputs <- function(progs) {
  f <- unique(unlist(.batch_data[intersect(names(.batch_data), progs)],
                     use.names = FALSE))
  f <- .batch_norm(f)
  f <- f[startsWith(f, "input/ard/")]
  md5 <- if (length(f)) unname(tools::md5sum(f)) else character()
  utils::write.csv(data.frame(file = f, kind = rep("imported ARD", length(f)),
                              md5 = md5, stringsAsFactors = FALSE),
                   "inputs.csv", row.names = FALSE)
}

# One md5 of what the programs were made from: study.yml and every file of
# the spec folder as the copy takes it (.batch_walk(): the run definitions'
# file spec/runs.yml included; no dot names, no editor files), each file's
# path and md5 in path order.  The stamp: batch.R holds the one of the spec
# it was made from (.batch_stamp), the stage refuses a run when the spec's is
# another, and batch.txt records it as "Spec".  Bytes are hashed (writeBin:
# writeLines would give CRLF on Windows), so Windows and Linux agree.
# tflspec's tfl_spec_stamp() evaluates this function from this file.
# study.yml's bytes, its `updated:` line left out (lines split at LF, each
# kept as it is, CR included, so the same file gives the same bytes anywhere)
.batch_study_yml_bytes <- function() {
  x <- readBin("study.yml", "raw", file.size("study.yml"))
  if (!length(x)) return(x)
  lf <- which(x == as.raw(10L))
  ends <- c(lf, if (!length(lf) || lf[length(lf)] < length(x)) length(x))
  starts <- c(1L, utils::head(ends, -1L) + 1L)
  pre <- charToRaw("updated:")
  keep <- vapply(seq_along(starts), function(i) {
    ends[i] - starts[i] + 1L < length(pre) ||
      !identical(x[starts[i]:(starts[i] + length(pre) - 1L)], pre)
  }, logical(1L))
  unlist(lapply(which(keep), function(i) x[starts[i]:ends[i]]))
}

# the md5 of some bytes
.batch_md5_bytes <- function(bytes) {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeBin(as.raw(bytes), tmp)
  unname(tools::md5sum(tmp))
}

.batch_spec_md5 <- function() {
  f <- c(if (file.exists("study.yml")) "study.yml",
         if (dir.exists(.batch_spec)) .batch_walk(.batch_spec))
  if (!length(f)) return("none")
  # (radix: the same order in every locale, so the same md5 on every machine)
  f <- sort(unique(f), method = "radix")
  md5 <- unname(tools::md5sum(f))
  # study.yml without its `updated:` line: the time of a save is not what the
  # programs are made from, so a save that changes nothing else keeps the stamp
  i <- which(f == "study.yml")
  if (length(i)) md5[i] <- .batch_md5_bytes(.batch_study_yml_bytes())
  txt <- paste0(f, " ", md5, "\n", collapse = "")
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeBin(charToRaw(enc2utf8(txt)), tmp)
  unname(tools::md5sum(tmp))
}

# The packages the run's programs name (library(), require(), pkg::), with
# the versions installed here, and sessionInfo() once they are loaded:
# session_info.txt.  The study's renv.lock is in the batch folder, as the
# record.  Never the end of a run: what cannot be read is said in the file.
.batch_session_info <- function(progs) {
  ver <- tryCatch({
    # the R files only (the support files are workbooks too)
    progs <- progs[grepl("[.][Rr]$", progs) & file.exists(progs)]
    code <- unlist(lapply(progs, readLines, warn = FALSE, encoding = "UTF-8"))
    code <- sub("#.*$", "", code, useBytes = TRUE)
    lib <- unlist(regmatches(code, gregexpr(
      "(library|require|requireNamespace)\\(\\s*['\"]?[A-Za-z][A-Za-z0-9.]*", code,
      perl = TRUE, useBytes = TRUE)))
    lib <- sub("^.*\\(\\s*['\"]?", "", lib, useBytes = TRUE)
    ns <- unlist(regmatches(code, gregexpr("[A-Za-z][A-Za-z0-9.]*(?=:::?)", code,
                                           perl = TRUE, useBytes = TRUE)))
    pkgs <- sort(unique(c(lib, ns)))
    pkgs <- pkgs[vapply(pkgs, function(p) nzchar(system.file(package = p)), NA)]
    for (p in pkgs) try(suppressPackageStartupMessages(loadNamespace(p)), silent = TRUE)
    vapply(pkgs, function(p) as.character(utils::packageVersion(p)), "")
  }, error = function(e) structure(character(), note = conditionMessage(e)))
  writeLines(c(
    "The R and packages of the run: the packages its programs name, with",
    "the versions installed where it ran, then sessionInfo() once they are loaded.",
    "",
    sprintf("%-24s %s", c("package", names(ver)), c("version", ver)),
    if (!is.null(attr(ver, "note"))) paste("(the programs could not be read:",
                                           attr(ver, "note"), ")"),
    "",
    utils::capture.output(print(utils::sessionInfo()))),
    "session_info.txt")
  invisible(ver)
}

# The conditions kept in an ARD's error / warning columns (cards does not
# stop when one analysis fails): output_id, level, text; no rows when there
# is none, or no ARD.  (The logic of tflspec::tfl_ard_conditions(), in base R.)
.batch_ard_conditions <- function(f) {
  none <- data.frame(output_id = character(), level = character(),
                     text = character(), stringsAsFactors = FALSE)
  if (!file.exists(f)) return(none)
  a <- tryCatch(readRDS(f), error = function(e) NULL)
  if (!is.data.frame(a) || !nrow(a)) return(none)
  first <- function(x) {
    x <- unlist(x)
    if (length(x)) as.character(x[1L]) else NA_character_
  }
  oid <- if ("output_id" %in% names(a)) a[["output_id"]] else rep(NA_character_, nrow(a))
  oid_one <- function(i) first(if (is.list(oid)) oid[[i]] else oid[i])
  out <- list()
  for (level in c("error", "warning")) {
    if (!level %in% names(a)) next
    for (i in seq_len(nrow(a))) {
      m <- unlist(a[[level]][i])
      m <- m[!is.na(m) & nzchar(m)]
      for (one in unique(m)) {
        out[[length(out) + 1L]] <- data.frame(output_id = oid_one(i), level = level,
                                              text = one, stringsAsFactors = FALSE)
      }
    }
  }
  if (!length(out)) return(none)
  d <- do.call(rbind, out)
  d[!duplicated(d), , drop = FALSE]
}

# what a re-run clears: the outputs and logs (a reports-only run: the
# reports' only)
.batch_clear <- function(parts, report_ids = character()) {
  if (identical(parts, "tfl")) {
    unlink(c("output/tfl", "logs/tfl"), recursive = TRUE)
    # (and the conditions the selected reports wrote)
    unlink(file.path("logs", "conditions", paste0(report_ids, ".csv")))
  } else {
    unlink(c("output", "logs"), recursive = TRUE)
  }
}

# The first lines of batch.txt, written when the run starts (before any
# program) and again at its end: the definition it runs, what it makes, the
# kind, the start, the log engine (R CMD BATCH unless --logrx), the folder
.batch_head <- function(plan, batch) {
  c(paste("Run      :", if (!is.null(plan$def)) plan$def$name else "none"),
    paste("Mode     :", plan$what),
    paste("Kind     :", .batch_kind),
    paste("Started  :", format(plan$started, "%Y-%m-%d %H:%M:%S")),
    paste("Logs     :", if (identical(plan$engine, "logrx")) "logrx" else "R CMD BATCH"),
    paste("Batch    :", batch),
    paste("Study    :", .batch_study))
}

# the log engine: R CMD BATCH, always, unless --logrx asks for logrx (the
# same definition then gives the same logs on every machine)
.batch_engine <- function(logrx) {
  if (!isTRUE(logrx)) return("batch")
  if (!requireNamespace("logrx", quietly = TRUE)) {
    .batch_refuse("--logrx needs the logrx package: it is not installed.")
  }
  "logrx"
}

# The stage: in the study folder, check the paths, make the batch folder,
# copy what the programs need, check the copy, write the entry files, and
# run in the folder.  Nothing is made when a check stops it; a failure after
# the folder exists removes it.
.batch_stage <- function(parts, args = character()) {
  if (!file.exists("study.yml")) {
    stop("Run it from the study folder (the one with study.yml).")
  }
  args <- args[!grepl("^--data-root=", args)]
  # the programs must be made from the spec as it is now (a preview is not an
  # official run: it is not tied to the saved spec)
  if (identical(.batch_kind, "run") && !identical(.batch_stamp, .batch_spec_md5())) {
    .batch_refuse(paste0("programs/batch.R is older than the spec: save the study (tflplanner) ",
                         "or regenerate the programs."))
  }
  plan <- .batch_plan(parts, args, have_ard = FALSE)
  if (!length(unlist(plan$selected))) stop("Nothing to run.")
  plan$engine <- .batch_engine(plan$logrx)
  problems <- .batch_check_paths(plan$selected)
  if (length(problems)) {
    msg <- c("The run was not started: it would read or write outside its batch folder.",
             paste0("  ", problems))
    cat(msg, sep = "\n")
    message(paste(msg, collapse = "\n"))
    .batch_quit(1L)
    stop("The run was not started.")
  }
  started <- Sys.time()
  plan$started <- started
  stamp <- format(started, "%Y%m%d-%H%M%S")
  # batch_<date>-<time>_<definition>; a study's autoexec_ard.R / autoexec_report.R
  # (no definition) name the part; a preview is preview_<date>-<time>
  folder <- if (identical(.batch_kind, "preview")) paste0("preview_", stamp) else
    paste0("batch_", stamp, "_", if (!is.null(plan$def)) plan$def$name else plan$what)
  base <- file.path(.batch_root, folder)
  dir.create(.batch_root, recursive = TRUE, showWarnings = FALSE)
  # the folder is taken by dir.create() itself, which fails when it exists: a
  # run started in the same second as another gets _2, _3
  dir <- base
  n <- 1L
  while (!dir.create(dir, showWarnings = FALSE)) {
    n <- n + 1L
    if (n > 500L) stop("Cannot make a batch folder in ", .batch_root, ".")
    dir <- paste0(base, "_", n)
  }
  cat("Batch folder:", dir, "\n\n")
  # the path from the batch folder back to the study
  study_root <- .batch_study_root(dir, getwd())
  fail <- function(msg) {
    unlink(dir, recursive = TRUE, force = TRUE)
    cat(msg, "\n")
    message(msg)
    .batch_quit(1L)
    stop(msg)
  }
  res <- tryCatch({
    files <- c(.batch_walk(c("programs", "spec", "input")),
               c("study.yml", "renv.lock")[file.exists(c("study.yml", "renv.lock"))])
    if (plan$data) files <- c(files, .batch_walk(.batch_data_dirs))
    files <- unique(files)
    before <- unname(tools::md5sum(files))
    size <- unname(file.size(files))
    for (f in files) {
      dir.create(file.path(dir, dirname(f)), recursive = TRUE, showWarnings = FALSE)
      if (!file.copy(f, file.path(dir, f), copy.date = TRUE)) {
        stop("could not copy ", f)
      }
    }
    utils::write.csv(data.frame(path = files, size = size, md5 = before,
                                stringsAsFactors = FALSE),
                     file.path(dir, "snapshot.csv"), row.names = FALSE)
    copy <- unname(tools::md5sum(file.path(dir, files)))
    after <- unname(tools::md5sum(files))
    bad <- files[is.na(copy) | copy != before | is.na(after) | after != before]
    if (length(bad)) {
      paste0("The copy does not match the study (something was saved while it was made): ",
             paste(utils::head(bad, 5L), collapse = ", "))
    } else {
      writeLines(.batch_paths_lines(if (plan$data) "." else study_root, study_root),
                 file.path(dir, "run_paths.R"))
      # the start record, before any program: what runs, from which definition
      writeLines(if (!is.null(plan$def)) paste0("--run=", plan$def$name) else args,
                 file.path(dir, "run_args.txt"))
      if (!is.null(plan$def)) {
        writeLines(.batch_definition_lines(plan$def), file.path(dir, "run_definition.yml"))
      }
      writeLines(.batch_head(plan, basename(dir)), file.path(dir, "batch.txt"))
      for (e in c(all = "all", ard = "ard", report = "report")) {
        p <- switch(e, all = "c(\"ard\", \"tfl\")", ard = "\"ard\"", report = "\"tfl\"")
        writeLines(c(paste0("# Entry file of this batch folder: run it from here (",
                            "Rscript autoexec_", e, ".R)."),
                     "source(\"programs/batch.R\")",
                     paste0("run_batch(", p, ", commandArgs(TRUE))")),
                   file.path(dir, paste0("autoexec_", e, ".R")))
      }
      NULL
    }
  }, error = function(e) paste("The batch folder could not be made:", conditionMessage(e)))
  if (is.character(res)) fail(res)
  # hand over: the same code the entry files of the folder call, in this
  # process, with the folder as the working directory
  assign(".batch_staged", TRUE, envir = .batch_env())
  old <- setwd(dir)
  on.exit({
    setwd(old)
    assign(".batch_staged", FALSE, envir = .batch_env())
  })
  .batch_execute(parts, args, plan = plan)
}

# An official run, in its batch folder (the working directory): the programs
# of `parts` ("ard", "tfl"), each logged, and what the run made:
#   logs/      one log per program (logrx; R CMD BATCH without logrx)
#   output/    the ARD the run made, the reports it made
#   programs/, spec/, input/, study.yml, renv.lock  the study as copied
#   data/      with --data: the data folders as copied
#   run_paths.R, run_args.txt, autoexec_*.R  how the folder runs again
#   snapshot.csv   the copied files: path, size, md5
#   run.csv    one row per program: its output id, status, errors, warnings,
#              time, md5, empty (a report: TRUE, FALSE or unknown)
#   issues.csv   program, output_id, level, source, text: the log lines, the
#              conditions kept in the ARD, a declared output not made, an
#              empty report (a note)
#   data_read.csv  the data files each program read: size and md5 as the
#              run started (every run, kept data or not)
#   inputs.csv     the imported ARDs the programs read
#   session_info.txt  the R and package versions
#   batch.txt  the study, the md5 of its spec, who, where, when, which R,
#              what was kept; "Verified : yes" last
# A first run comes from the stage (.batch_staged is set).  An entry file run
# by hand runs again: it stops inside a study's runs/, elsewhere it clears
# the outputs, keeps the first run.csv as run_1.csv, uses run_args.txt when
# no names are given, and notes "Re-run".  `args`: --run=<name> (the run
# definition; a re-run reads the folder's run_definition.yml); --data (a hand
# choice: copy the data); --logrx; --data-root=<dir> (a re-run: where the data
# are); program or output names to run only those and --exclude=<name> to
# leave a program out (a re-run only: an official run takes neither);
# --no-code is accepted and does nothing.
.batch_execute <- function(parts, args = character(), plan = NULL) {
  staged <- isTRUE(get0(".batch_staged", envir = .batch_env(), inherits = FALSE))
  rerun <- !staged
  paths <- .batch_read_paths()
  if (rerun && identical(.batch_kind, "run")) {
    sr <- paths$study
    in_study <- file.exists(file.path(sr, "study.yml"))
    if (in_study && .batch_is_abs(sr)) {
      # (an absolute study path: in place only when this folder lies in it)
      # (Windows paths ignore case: "C:/Study" is "c:/study")
      np <- function(p) {
        p <- normalizePath(p, winslash = "/", mustWork = FALSE)
        if (.Platform$OS.type == "windows") tolower(p) else p
      }
      in_study <- startsWith(np(getwd()), paste0(np(sr), "/"))
    }
    if (in_study) {
      stop("Official results cannot be overwritten: copy this folder elsewhere and run it there.",
           call. = FALSE)
    }
  }
  root_arg <- sub("^--data-root=", "", grep("^--data-root=", args, value = TRUE))
  args <- args[!grepl("^--data-root=", args)]
  if (rerun && !length(args) && file.exists("run_args.txt")) {
    args <- readLines("run_args.txt", warn = FALSE)
  }
  root <- if (length(root_arg)) root_arg[[length(root_arg)]] else paths$data
  # a reports-only run again reads this folder's own ARD
  if (rerun && identical(intersect(c("ard", "tfl"), parts), "tfl") &&
      !file.exists(.batch_ard)) {
    stop("No ARD in this batch folder: run autoexec_ard.R or autoexec_all.R first.",
         call. = FALSE)
  }
  if (is.null(plan)) {
    plan <- .batch_plan(parts, args, have_ard = file.exists(.batch_ard), first = FALSE)
    plan$engine <- .batch_engine(plan$logrx)
    plan$started <- Sys.time()
  }
  parts <- plan$parts
  # (checked before anything is cleared)
  if (!length(unlist(plan$selected[parts]))) stop("Nothing to run.")
  # no data: none of the selected programs' mapped data files is under the root
  if (rerun) {
    mapped <- names(.batch_data_vars)[nzchar(.batch_data_vars)]
    need <- unique(unlist(.batch_data[intersect(names(.batch_data),
                                                unlist(plan$selected[parts], use.names = FALSE))],
                          use.names = FALSE))
    need <- need[need %in% c(mapped, .batch_norm(mapped))]
    if (length(need) && !any(file.exists(.batch_data_at(need, root)))) {
      stop("This batch folder has no data: run it with --data-root=<the study folder> or make it with the data kept.",
           call. = FALSE)
    }
  }
  old_rerun <- character()
  if (rerun) {
    if (file.exists("batch.txt")) {
      old_rerun <- grep("^Re-run", readLines("batch.txt", warn = FALSE), value = TRUE)
    }
    if (file.exists("run.csv") && !file.exists("run_1.csv")) file.copy("run.csv", "run_1.csv")
    if (file.exists("issues.csv") && !file.exists("issues_1.csv")) file.copy("issues.csv", "issues_1.csv")
    rep_ids <- unname(.batch_report_ids[plan$selected[["tfl"]]])
    .batch_clear(parts, rep_ids[!is.na(rep_ids)])
    if (length(root_arg)) writeLines(.batch_paths_lines(root, paths$study), "run_paths.R")
  }
  # the programs' own R processes find the packages this one does
  libs <- Sys.getenv("R_LIBS", unset = NA)
  Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
  on.exit(if (is.na(libs)) Sys.unsetenv("R_LIBS") else Sys.setenv(R_LIBS = libs),
          add = TRUE)
  dir.create(file.path("output", "ard"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path("output", "tfl"), recursive = TRUE, showWarnings = FALSE)
  selected <- plan$selected
  excluded <- plan$excluded
  only <- plan$only
  engine <- plan$engine
  started <- plan$started
  # the data and the spec as the run starts
  .batch_data_read(unlist(selected[parts], use.names = FALSE), root)
  spec_md5 <- .batch_spec_md5()
  rows <- list()
  issues <- data.frame(program = character(), output_id = character(),
                       level = character(), source = character(),
                       text = character(), stringsAsFactors = FALSE)
  add_issues <- function(program, output_id, level, source, text) {
    if (!length(text)) return(invisible())
    issues <<- rbind(issues, data.frame(
      program = program, output_id = output_id, level = level, source = source,
      text = text, stringsAsFactors = FALSE))
  }
  # the reports whose ARD program failed in this run
  failed_ard <- character()
  for (part in parts) {
    progs <- selected[[part]]
    if (!length(progs)) next
    log_dir <- file.path("logs", part)
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
    for (p in progs) {
      # a report whose ARD program failed in this run is not run on the
      # rows an earlier run left: it is recorded as failed
      bad <- intersect(unlist(.batch_needs[p], use.names = FALSE), failed_ard)
      if (part == "tfl" && length(bad)) {
        note <- paste0("not run: the ARD of ", paste(bad, collapse = ", "), " failed in this run")
        cat(sprintf("%-5s %-4s %-32s %s\n", "ERROR", part, basename(p), note))
        r <- data.frame(
          part = part, program = p, status = "ERROR", errors = 1L, warnings = 0L,
          note = note, started = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), seconds = 0,
          md5 = NA_character_, log = NA_character_, stringsAsFactors = FALSE)
        rows[[length(rows) + 1L]] <- .batch_with_issues(r, "error", note)
        next
      }
      r <- .batch_run(part, p, log_dir, engine)
      if (part == "ard" && r$status != "OK") {
        failed_ard <- c(failed_ard, unname(.batch_ard_outputs[basename(p)]))
        .batch_ard_error(.batch_ard_outputs[[basename(p)]], r$note)
      }
      rows[[length(rows) + 1L]] <- r
    }
  }
  result <- do.call(rbind, rows)
  if (is.null(result)) stop("Nothing to run.")
  if (length(excluded)) {
    cat("\nLeft out:", paste(basename(excluded), collapse = ", "), "\n")
  }
  # the output each program made or was for (the screens find a report's
  # latest run by it)
  oid <- ifelse(result$part == "ard",
                unname(.batch_ard_outputs[basename(result$program)]),
                unname(.batch_report_ids[result$program]))
  oid[is.na(oid)] <- ""
  # the program that made an output id (a report's, else the ARD's)
  prog_of <- function(id) {
    k <- which(oid == id & result$part == "tfl")
    if (!length(k)) k <- which(oid == id & result$part == "ard")
    if (length(k)) result$program[k[[1L]]] else ""
  }
  # the log lines
  for (k in seq_along(rows)) {
    a <- attr(rows[[k]], "issues")
    if (!is.null(a) && nrow(a)) {
      add_issues(result$program[k], oid[k], a$level, "log", a$text)
    }
  }
  # a declared output not made: the program did not do its job
  for (k in seq_len(nrow(result))) {
    p <- result$program[k]
    if (result$status[k] == "OK" && p %in% names(.batch_outputs)) {
      outs <- as.character(.batch_outputs[[p]])
      gone <- outs[!file.exists(outs)]
      if (length(gone)) {
        result$status[k] <- "ERROR"
        result$note[k] <- paste("missing output:", paste(gone, collapse = ", "))
        add_issues(p, oid[k], "error", "output", paste("declared output not made:", gone))
      }
    }
  }
  # the conditions kept in the ARD
  ac <- .batch_ard_conditions(.batch_ard)
  if (nrow(ac)) {
    ap <- result$program[result$part == "ard"]
    ai <- oid[result$part == "ard"]
    pr <- ap[match(ac$output_id, ai)]
    pr[is.na(pr)] <- ""
    add_issues(pr, ac$output_id, ac$level, "ard", ac$text)
  }
  # the conditions a generated program wrote (logs/conditions/<output_id>.csv)
  for (cf in list.files(file.path("logs", "conditions"), pattern = "[.]csv$",
                        full.names = TRUE)) {
    cd <- tryCatch(utils::read.csv(cf, colClasses = "character"), error = function(e) NULL)
    if (is.null(cd) || !nrow(cd) || !all(c("output_id", "class", "message") %in% names(cd))) next
    id <- ifelse(is.na(cd$output_id) | !nzchar(cd$output_id),
                 sub("[.]csv$", "", basename(cf)), cd$output_id)
    cl <- tolower(cd$class)
    add_issues(vapply(id, prog_of, "", USE.NAMES = FALSE), id,
               ifelse(grepl("error", cl), "error", ifelse(grepl("warning", cl), "warning", "note")),
               "condition", cd$message)
  }
  # an empty report (report_status.csv's `empty`): a note, not a failure
  empty <- ifelse(result$part == "tfl", "unknown", "")
  rs <- file.path("output", "tfl", "report_status.csv")
  st <- if (file.exists(rs)) tryCatch(utils::read.csv(rs, colClasses = "character"),
                                      error = function(e) NULL)
  if (!is.null(st) && all(c("output_id", "empty") %in% names(st))) {
    tf <- which(result$part == "tfl")
    v <- toupper(trimws(st$empty[match(oid[tf], st$output_id)]))
    empty[tf] <- ifelse(!is.na(v) & v %in% c("TRUE", "FALSE"), v, "unknown")
    for (k in tf[empty[tf] == "TRUE"]) {
      add_issues(result$program[k], oid[k], "note", "output", "no rows")
    }
  }
  # one rule for errors: a program is ERROR when its exit status is not 0, or
  # it has an error-level issue from any source; an ERROR program with no
  # error issue gets one, from the log
  for (k in seq_len(nrow(result))) {
    errs_k <- issues$text[issues$program == result$program[k] & issues$level == "error"]
    if (length(errs_k)) {
      if (result$status[k] == "OK") {
        result$status[k] <- "ERROR"
        result$note[k] <- errs_k[[1L]]
      }
    } else if (result$status[k] == "ERROR") {
      rc <- attr(rows[[k]], "rc")
      lg <- result$log[k]
      txt <- sprintf("the program ended with status %s; see %s",
                     if (is.null(rc)) "?" else rc, if (is.na(lg)) "its log" else lg)
      add_issues(result$program[k], oid[k], "error", "log", txt)
      if (!nzchar(result$note[k])) result$note[k] <- txt
    }
  }
  # errors and warnings: as issues.csv has them
  result$errors <- vapply(result$program, function(p)
    sum(issues$program == p & issues$level == "error"), 1L, USE.NAMES = FALSE)
  result$warnings <- vapply(result$program, function(p)
    sum(issues$program == p & issues$level == "warning"), 1L, USE.NAMES = FALSE)
  n_err <- sum(issues$level == "error")
  n_warn <- sum(issues$level == "warning")
  man <- NULL
  if (identical(root, ".")) man <- .batch_keep_data(result$program, root)
  progs_all <- unique(c(result$program, unlist(.batch_support[c(parts, "all")])))
  .batch_session_info(progs_all)
  .batch_inputs(result$program)
  result <- cbind(result[1:2], output_id = oid, result[-(1:2)],
                  stringsAsFactors = FALSE)
  result$empty <- empty
  utils::write.csv(result, "run.csv", row.names = FALSE)
  utils::write.csv(issues, "issues.csv", row.names = FALSE)
  # the end record, written last
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  writeLines(c(
    .batch_head(plan, basename(getwd())),
    paste("Spec     :", spec_md5),
    paste("Runs     :", paste(parts, collapse = ", "),
          if (length(only)) paste0("(", paste(only, collapse = ", "), ")")),
    paste("Finished :", now),
    paste("User     :", Sys.info()[["user"]]),
    paste("Machine  :", Sys.info()[["nodename"]]),
    paste("R        :", R.version.string),
    paste("Data     :", if (!is.null(man)) sprintf(
      "kept (data/, %d file(s), %.1f MB; data_manifest.csv)", nrow(man),
      sum(man$size) / 1024^2) else "not kept"),
    paste("Session  :", if (file.exists("renv.lock"))
      "session_info.txt and renv.lock" else "session_info.txt"),
    paste("Excluded :", if (length(excluded))
      paste(excluded, collapse = ", ") else "none"),
    paste("Result   :", sum(result$status == "OK"), "of", nrow(result),
          "program(s) without error"),
    paste("Errors   :", n_err),
    paste("Warnings :", n_warn),
    paste("Data root :", root),
    if (rerun) c(old_rerun, paste("Re-run   :", now)),
    if (!rerun && identical(.batch_kind, "run")) "Verified : yes"),
    "batch.txt")
  cat(sprintf("\n%d of %d program(s) ran without error.  %s\n",
              sum(result$status == "OK"), nrow(result), getwd()))
  if (any(result$status != "OK")) {
    print(result[result$status != "OK", c("program", "note")],
          right = FALSE, row.names = FALSE)
    if (!interactive()) .batch_quit(1L)
  }
  invisible(result)
}

# The call an entry file makes: the first run (from the study folder) is made
# by the stage; a batch folder's own entry file runs again.
run_batch <- function(parts, args = character()) {
  staged <- isTRUE(get0(".batch_staged", envir = .batch_env(), inherits = FALSE))
  # (snapshot.csv alone is the sign of a batch folder)
  in_batch <- file.exists("snapshot.csv")
  if (!staged && !in_batch) return(invisible(.batch_stage(parts, args)))
  invisible(.batch_execute(parts, args))
}
