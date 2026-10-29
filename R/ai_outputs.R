# ============================================================================
#  The AI's reports, listed for the person to check
# ----------------------------------------------------------------------------
#  tfl_ai_outputs() is one row a report the AI's drafts make: its shell
#  sheet, its TOC item, its title, analysis set and how its TOC row is split
#  for it, its status, its RTF, whether it is adopted.  A screen groups it
#  by sheet and by TOC item.
# ============================================================================

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
#'   `adopted` (`no`; `yes`; `yes, draft newer` -- the draft differs from
#'   the study's file adopted from it) and `file` (its draft report file).
#' @seealso [tfl_ai_adopt()].
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
