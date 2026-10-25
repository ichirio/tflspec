# ============================================================================
#  The AI's reports, listed for the person to check, and the list the
#  person expects (the review loop, 10/24)
# ----------------------------------------------------------------------------
#  tfl_ai_outputs() is one row a report the AI's answers make: its shell
#  sheet, its TOC item, its title, analysis set and how its TOC row is split
#  for it, its status, its RTF, the open questions on it, whether it is
#  adopted.  A screen groups it by sheet and by TOC item.  The person
#  corrects a copy of it -- ai/expected_outputs.csv, theirs, never the AI's:
#  a row added (`add`), dropped (`drop`, with its reason) or changed
#  (`change`, the old values in `was`), and a note on what the report
#  should show.  run.R compares the reports made with it and says what
#  differs in ai/status.md -- it tells, it does not refuse.
# ============================================================================

.ai_expected_cols <- c("sheet", "toc_id", "output_id", "title", "population", "split",
                       "action", "was", "reason", "note")
.ai_expected_actions <- c("add", "drop", "change")

.ai_expected_file <- function(study_dir) file.path(study_dir, "ai", "expected_outputs.csv")

# a CSV of text, every column character, blanks NA
.ai_csv_read <- function(f) {
  if (!file.exists(f)) return(NULL)
  x <- tryCatch(utils::read.csv(f, stringsAsFactors = FALSE, colClasses = "character",
                                encoding = "UTF-8", na.strings = character(),
                                check.names = FALSE),
                error = function(e) NULL)
  if (is.null(x)) return(NULL)
  x[] <- lapply(x, function(v) {
    v <- trimws(v)
    v[!nzchar(v)] <- NA_character_
    v
  })
  x
}

# The reports' rows of the study's draft report files (ai/spec/outputs/,
# or spec/outputs/ in a study of the form before): id, title, analysis set
.ai_draft_rows <- function(study_dir) {
  area <- .ai_area(study_dir) %||% file.path("spec", "outputs")
  fs <- list.files(file.path(study_dir, area), pattern = "\\.ya?ml$", full.names = TRUE)
  fs <- fs[!startsWith(basename(fs), "_")]
  rows <- lapply(fs, function(f) {
    o <- tryCatch(tfl_read_output(f), error = function(e) NULL)
    if (is.null(o) || is.null(o$id)) return(NULL)
    data.frame(output_id = o$id, draft_title = .na_scalar(o$label),
               population = .na_scalar(o$population),
               file = gsub("\\\\", "/", file.path(area, basename(f))),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, c(list(data.frame(output_id = character(), draft_title = character(),
                                   population = character(), file = character(),
                                   stringsAsFactors = FALSE)), rows))
}

#' The AI's reports, one row a report
#'
#' The list a person checks the AI's work by: every report the answers make
#' ([tfl_ai_collect_answers()]'s manifest, `ai/manifest.csv`) and every
#' draft report file there, with where it is from and how far it got.  A
#' screen shows it in three levels -- the shell sheets, the TOC items, the
#' reports -- by its `sheet` and `toc_id`.
#'
#' @param study_dir The study folder.
#' @return A data frame, one row a report: `sheet` (its request's shell
#'   sheets, ` | ` between them), `shell_code` (the id written at the top
#'   of its shell, the TOC entry's), `toc_id` (its TOC row's number, as
#'   the answer lists it; else its TOC entry's id -- a part's, its
#'   number's),
#'   `output_id`, `title` (as the answer lists it, else its report file's
#'   label), `population` (its analysis set), `split` (how its TOC row is
#'   split for it), `request`, `status` (`ok`, `warning`, `error`, `no
#'   answer`, `not answered`; `draft` for a report file no answer lists),
#'   `rtf` (its RTF, `NA` when none), `problem` (why it has none),
#'   `questions` (the open questions in `ai/questions.md` naming it),
#'   `adopted` (`no`; `yes`; `yes, draft newer` -- the draft differs from
#'   the study's file adopted from it) and `file` (its draft report file).
#' @seealso [tfl_ai_expected_write()] for the list the person expects.
#' @export
tfl_ai_outputs <- function(study_dir = ".") {
  ai <- file.path(study_dir, "ai")
  man <- .ai_csv_read(file.path(ai, "manifest.csv"))
  if (is.null(man)) {
    man <- data.frame(request = character(), toc = character(), output_id = character(),
                      title = character(), split = character(), status = character(),
                      stringsAsFactors = FALSE)
  }
  man <- man[!is.na(man$output_id), , drop = FALSE]
  man <- man[!duplicated(man$output_id), , drop = FALSE]
  dr <- .ai_draft_rows(study_dir)
  ids <- unique(c(man$output_id, dr$output_id))
  m <- match(ids, man$output_id)
  d <- match(ids, dr$output_id)
  pick <- function(x, i) if (is.null(x)) rep(NA_character_, length(i)) else x[i]
  req <- pick(man$request, m)
  # the request's shell sheets
  rq <- tfl_ai_requests(study_dir)
  sheet <- rq$sheets[match(req, rq$request)]
  # the shell's code: the TOC entry's (toc.yml), a part's its number's
  tf <- file.path(study_dir, "spec", "toc.yml")
  toc <- if (file.exists(tf)) tryCatch(yaml::read_yaml(tf)$outputs, error = function(e) NULL)
  toc <- toc %||% list()
  tk <- vapply(toc, function(e) {
    tryCatch(.ai_id_key(e$id %||% ""), error = function(err) NA_character_)
  }, "")
  sc <- vapply(toc, function(e) as.character(e$shell_code %||% NA_character_), "")
  te <- vapply(toc, function(e) as.character(e$id %||% NA_character_), "")
  entry_of <- function(id) {
    k <- tryCatch(.ai_id_key(id), error = function(e) NA_character_)
    if (is.na(k)) return(NA_integer_)
    j <- match(k, tk)
    if (is.na(j) && grepl("[0-9][A-Z]$", k)) j <- match(sub("[A-Z]$", "", k), tk)
    j
  }
  ei <- vapply(ids, entry_of, 0L, USE.NAMES = FALSE)
  # its RTF, or why none (the last run's, ai/built.csv)
  b <- .ai_built_read(study_dir)
  bi <- match(ids, b$output_id)
  # the open questions naming it
  qf <- file.path(ai, "questions.md")
  q <- if (file.exists(qf)) .ai_questions_read(readLines(qf, warn = FALSE, encoding = "UTF-8")) else
    .ai_questions_read(character())
  q <- q[q$status %in% "open", , drop = FALSE]
  nq <- vapply(ids, function(id) {
    sum(vapply(strsplit(as.character(ifelse(is.na(q$reports), "", q$reports)), ",\\s*"),
               function(r) id %in% trimws(r), NA))
  }, 0L, USE.NAMES = FALSE)
  out <- data.frame(
    sheet = sheet,
    shell_code = sc[ei],
    # (the answer's TOC row, else the TOC entry it is, or its number's)
    toc_id = ifelse(is.na(pick(man$toc, m)), te[ei], pick(man$toc, m)),
    output_id = ids,
    title = ifelse(is.na(pick(man$title, m)), pick(dr$draft_title, d), pick(man$title, m)),
    population = pick(dr$population, d),
    split = pick(man$split, m),
    request = req,
    status = ifelse(is.na(m), "draft", pick(man$status, m)),
    rtf = pick(b$rtf, bi),
    problem = pick(b$problem, bi),
    questions = nq,
    adopted = .ai_adopted_state(study_dir, ids, pick(dr$file, d)),
    file = pick(dr$file, d),
    stringsAsFactors = FALSE)
  rownames(out) <- NULL
  out
}

# Each report's adoption: `no`; `yes` (its study file adopted from the
# draft as it is); `yes, draft newer` (the AI redrafted it since)
.ai_adopted_state <- function(study_dir, ids, drafts) {
  ad <- .ai_adopted_read(study_dir)
  vapply(seq_along(ids), function(i) {
    j <- match(ids[i], ad$output_id)
    if (is.na(j)) return("no")
    f <- drafts[i]
    if (is.na(f) || !file.exists(file.path(study_dir, f))) return("yes")
    now <- unname(tools::md5sum(file.path(study_dir, f)))
    if (!is.na(ad$md5[j]) && !identical(now, ad$md5[j])) "yes, draft newer" else "yes"
  }, "")
}

#' The list of reports the person expects
#'
#' The person's own list of the study's reports, `ai/expected_outputs.csv`:
#' first a copy of [tfl_ai_outputs()], then corrected by the person (on a
#' screen, or in a spreadsheet) -- a row's `action` says what to do: blank,
#' keep it; `add`, a report to add (its sheet and TOC item, and what is
#' known of its id, title, analysis set and part); `drop`, a report not
#' wanted (the row stays, with its `reason`); `change`, a row whose title or
#' analysis set the person changed (`was`: the values before,
#' `title: Demographics | population: SAF`).  `note` is what the report
#' should show, free text.  The AI never writes it; `run.R` compares the
#' reports with it ([tfl_ai_run()]).
#'
#' `tfl_ai_expected_write()` writes it: `x` (the person's rows), or, when
#' `NULL`, the first copy of the AI's list -- only when there is none yet
#' (`overwrite = TRUE` to start again).  `tfl_ai_expected_read()` reads it.
#'
#' @param study_dir The study folder.
#' @param x The rows to write: a data frame with the list's columns
#'   (`sheet`, `toc_id`, `output_id`, `title`, `population`, `split`,
#'   `action`, `was`, `reason`, `note`; a column it lacks is blank).
#'   `NULL`: the AI's list now.
#' @param overwrite `TRUE` to write over the list there.
#' @return `tfl_ai_expected_write()`: the file's path, invisibly.
#'   `tfl_ai_expected_read()`: the list (a data frame of those columns), or
#'   `NULL` when the study has none.
#' @seealso [tfl_ai_outputs()]
#' @export
tfl_ai_expected_write <- function(study_dir = ".", x = NULL, overwrite = FALSE) {
  f <- .ai_expected_file(study_dir)
  if (file.exists(f) && !overwrite) {
    .ard_stop(sprintf(paste0("tfl_ai_expected_write(): %s is there (the person's list); ",
                             "overwrite = TRUE to write over it."), f))
  }
  if (is.null(x)) x <- tfl_ai_outputs(study_dir)
  x <- .ai_expected_shape(as.data.frame(x, stringsAsFactors = FALSE))
  bad <- setdiff(stats::na.omit(x$action), .ai_expected_actions)
  if (length(bad)) {
    .ard_stop(sprintf("tfl_ai_expected_write(): action is blank, %s; not %s.",
                      paste(.ai_expected_actions, collapse = ", "),
                      paste(unique(bad), collapse = ", ")))
  }
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  out <- x
  out[] <- lapply(out, function(v) ifelse(is.na(v), "", v))
  con <- file(f, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  utils::write.csv(out, con, row.names = FALSE)
  invisible(f)
}

#' @rdname tfl_ai_expected_write
#' @export
tfl_ai_expected_read <- function(study_dir = ".") {
  f <- .ai_expected_file(study_dir)
  if (!file.exists(f)) return(NULL)
  x <- .ai_csv_read(f)
  if (is.null(x)) .ard_stop(sprintf("tfl_ai_expected_read(): %s cannot be read as a CSV.", f))
  x <- .ai_expected_shape(x)
  x$action <- tolower(x$action)
  x
}

# the list's columns, in order, as text (a column missing: blank)
.ai_expected_shape <- function(x) {
  n <- nrow(x)
  out <- lapply(.ai_expected_cols, function(k) {
    v <- if (k %in% names(x)) as.character(x[[k]]) else rep(NA_character_, n)
    v <- trimws(v)
    v[!is.na(v) & !nzchar(v)] <- NA_character_
    v
  })
  out <- as.data.frame(stats::setNames(out, .ai_expected_cols), stringsAsFactors = FALSE)
  rownames(out) <- NULL
  out
}

# The reports made against the person's list: one row a difference
# (`output_id`, `result` -- missing, differs, still there, not expected --,
# `detail`), and the counts.  Compared by id; a title with its spaces and
# case set aside, an analysis set by its id; a part not compared (free
# text).  NULL when the study has no list.
.ai_expected_compare <- function(expected, made) {
  if (is.null(expected)) return(NULL)
  made <- made[!is.na(made$file), , drop = FALSE]
  e <- expected[!is.na(expected$output_id) | expected$action %in% "add", , drop = FALSE]
  norm <- function(x) tolower(gsub("[[:space:]]+", " ", trimws(ifelse(is.na(x), "", x))))
  rows <- list()
  add <- function(id, result, detail = NA_character_) {
    rows[[length(rows) + 1L]] <<- data.frame(output_id = id, result = result, detail = detail,
                                            stringsAsFactors = FALSE)
  }
  n_ok <- 0L
  for (i in seq_len(nrow(e))) {
    id <- e$output_id[i]
    act <- e$action[i]
    there <- !is.na(id) && id %in% made$output_id
    if (act %in% "drop") {
      if (there) add(id, "still there", e$reason[i])
      next
    }
    if (!there) {
      add(id %|% sprintf("(sheet %s, TOC %s)", e$sheet[i] %|% "?", e$toc_id[i] %|% "?"),
          "missing", e$title[i])
      next
    }
    mi <- match(id, made$output_id)
    diff <- c(
      if (!is.na(e$title[i]) && norm(e$title[i]) != norm(made$title[mi]))
        sprintf("title \"%s\" (expected \"%s\")", made$title[mi] %|% "", e$title[i]),
      if (!is.na(e$population[i]) && !identical(toupper(e$population[i]),
                                               toupper(made$population[mi] %|% "")))
        sprintf("population %s (expected %s)", made$population[mi] %|% "none", e$population[i]))
    if (length(diff)) add(id, "differs", paste(diff, collapse = "; ")) else n_ok <- n_ok + 1L
  }
  for (id in setdiff(made$output_id, e$output_id)) add(id, "not expected")
  d <- do.call(rbind, c(list(data.frame(output_id = character(), result = character(),
                                        detail = character(), stringsAsFactors = FALSE)), rows))
  count <- function(k) sum(d$result == k)
  wanted <- sum(!e$action %in% "drop")
  structure(d, text = sprintf(
    "Expected %d, made %d: missing %d, still there %d, differs %d, not expected %d",
    wanted, nrow(made), count("missing"), count("still there"), count("differs"),
    count("not expected")), ok = n_ok)
}

# x, or y when x is NA / NULL
`%|%` <- function(x, y) if (is.null(x) || length(x) == 0L || is.na(x[1L])) y else x

# status.md's section of the comparison (none without a list)
.ai_expected_status <- function(cmp) {
  if (is.null(cmp)) return(NULL)
  cell <- function(x) ifelse(is.na(x), "", gsub("|", "/", x, fixed = TRUE))
  c("",
    "## Expected outputs (ai/expected_outputs.csv)",
    "",
    paste0(attr(cmp, "text"), "."),
    if (nrow(cmp)) c(
      "",
      "| report | result | detail |",
      "|---|---|---|",
      sprintf("| %s | %s | %s |", cell(cmp$output_id), cmp$result, cell(cmp$detail))))
}
