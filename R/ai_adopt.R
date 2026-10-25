# ============================================================================
#  The AI's drafts apart, adopted into the study by a person (10/25)
# ----------------------------------------------------------------------------
#  The flow: the agent drafts the report files, the person answers its
#  questions and asks for corrections, the agent rewrites the drafts -- two
#  or three rounds -- and a finished draft is ADOPTED into the study's own
#  report files.  So the drafts live apart: `ai/spec/outputs/` (written by
#  every collection), built into `ai/output/` by run.R; the study's
#  `spec/outputs/` holds what was adopted, which the AI never writes over.
#  A study is in this form once `ai/spec/outputs/` is there
#  (tfl_ai_migrate()); one without it works as before.
# ============================================================================

# The AI's drafts' folder, relative to the study folder; NULL: the study
# keeps its drafts in spec/outputs (the form before)
.ai_area <- function(study_dir) {
  a <- file.path("ai", "spec", "outputs")
  if (dir.exists(file.path(study_dir, a))) a else NULL
}

# The options under which the AI's drafts are the study's report files
# (read, checked, built): NULL when the study has no drafts' folder
.ai_area_options <- function(study_dir) {
  a <- .ai_area(study_dir)
  if (is.null(a)) return(NULL)
  options(tflspec.outputs_dir = a)
}

.ai_adopted_file <- function(study_dir) file.path(study_dir, "ai", "adopted.csv")

.ai_adopted_read <- function(study_dir) {
  f <- .ai_adopted_file(study_dir)
  empty <- data.frame(output_id = character(), adopted = character(), md5 = character(),
                      stringsAsFactors = FALSE)
  if (!file.exists(f)) return(empty)
  x <- tryCatch(utils::read.csv(f, stringsAsFactors = FALSE, colClasses = "character",
                                encoding = "UTF-8"),
                error = function(e) empty)
  x[intersect(names(empty), names(x))]
}

#' Adopt the AI's drafts into the study's report files
#'
#' The AI drafts a study's report files apart from the study's own (in a
#' study with `ai/spec/outputs/`, [tfl_ai_migrate()]): every collection
#' ([tfl_ai_collect_answers()], `run.R`) writes the drafts there and builds
#' them into `ai/output/`.  A draft the person finds finished is adopted:
#' copied to `spec/outputs/`, where the study's lists, programs and runs
#' read it, and the AI never writes over it.  Adopting again after the AI
#' redrafted a report replaces the adopted file only with `overwrite =
#' TRUE` -- a person may have changed it since.
#'
#' @param study_dir The study folder.
#' @param outputs The reports to adopt (their ids); `NULL`, every draft.
#' @param overwrite `TRUE` to write over a report already adopted (or
#'   written by hand) that differs from its draft.
#' @return A data frame, one row a report: `output_id`, `status`
#'   (`adopted`, `same` -- the study's file is the draft already --, `kept`
#'   -- the study's file differs and was left, pass `overwrite = TRUE` --,
#'   `no draft`), `file` (the study's file).  The adoptions are recorded in
#'   `ai/adopted.csv` (the report, when, the draft's md5).
#' @seealso [tfl_ai_migrate()], [tfl_ai_collect_answers()]
#' @export
tfl_ai_adopt <- function(study_dir, outputs = NULL, overwrite = FALSE) {
  a <- .ai_area(study_dir)
  if (is.null(a)) {
    .ard_stop(sprintf(paste0("tfl_ai_adopt(): %s keeps the AI's drafts in spec/outputs (no ",
                             "ai/spec/outputs): they are the study's already. tfl_ai_migrate() ",
                             "puts them apart."), study_dir))
  }
  drafts <- list.files(file.path(study_dir, a), "[.]ya?ml$", full.names = TRUE)
  ids <- tools::file_path_sans_ext(basename(drafts))
  want <- outputs %||% ids
  dest_dir <- file.path(.spec_dir(study_dir), "outputs")
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  log <- .ai_adopted_read(study_dir)
  rows <- lapply(want, function(id) {
    src <- drafts[match(id, ids)]
    dest <- file.path(dest_dir, paste0(id, ".yml"))
    rel <- file.path("spec", "outputs", paste0(id, ".yml"))
    if (is.na(src)) return(data.frame(output_id = id, status = "no draft", file = NA_character_))
    md5 <- unname(tools::md5sum(src))
    if (file.exists(dest)) {
      if (identical(unname(tools::md5sum(dest)), md5)) {
        return(data.frame(output_id = id, status = "same", file = rel))
      }
      if (!isTRUE(overwrite)) return(data.frame(output_id = id, status = "kept", file = rel))
    }
    file.copy(src, dest, overwrite = TRUE)
    log <<- rbind(log[log$output_id != id, , drop = FALSE],
                  data.frame(output_id = id, adopted = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                             md5 = md5, stringsAsFactors = FALSE))
    data.frame(output_id = id, status = "adopted", file = rel)
  })
  out <- do.call(rbind, c(list(data.frame(output_id = character(), status = character(),
                                          file = character())), rows))
  utils::write.csv(log, .ai_adopted_file(study_dir), row.names = FALSE, fileEncoding = "UTF-8")
  rownames(out) <- NULL
  kept <- out$output_id[out$status == "kept"]
  if (length(kept)) {
    message(sprintf(paste0("tfl_ai_adopt(): %s left as the study has them (they differ from the ",
                           "drafts); pass overwrite = TRUE to adopt the drafts."),
                    paste(kept, collapse = ", ")))
  }
  out
}

#' Put a study's AI drafts apart from its own report files
#'
#' Makes `ai/spec/outputs/`, the AI's drafts' folder (see [tfl_ai_adopt()]),
#' and copies there the report files the AI wrote -- those its answers name
#' (`ai/manifest.csv`, `spec/_assumptions.yml`) -- so that later rounds
#' redraft them there.  The study's own `spec/outputs/` keep them, recorded
#' as adopted (they were the study's), so nothing a person changed is lost.
#' From then on the collection and `run.R` write the drafts apart, and
#' `run.R` builds them into `ai/output/`.  A study already apart is left as
#' it is.
#'
#' @param study_dir The study folder.
#' @return The ids of the drafts copied apart, invisibly.
#' @seealso [tfl_ai_adopt()]
#' @export
tfl_ai_migrate <- function(study_dir) {
  if (!is.null(.ai_area(study_dir))) return(invisible(character()))
  area <- file.path(study_dir, "ai", "spec", "outputs")
  dir.create(area, recursive = TRUE, showWarnings = FALSE)
  # the AI's reports: those its answers named
  mf <- file.path(study_dir, "ai", "manifest.csv")
  ids <- character()
  if (file.exists(mf)) {
    m <- tryCatch(utils::read.csv(mf, stringsAsFactors = FALSE, encoding = "UTF-8"),
                  error = function(e) NULL)
    if (!is.null(m$output_id)) ids <- m$output_id[!is.na(m$output_id)]
  }
  asm <- .ai_folder_assumptions(study_dir)
  ids <- unique(c(ids, asm$output_id[!is.na(asm$output_id)]))
  src <- file.path(.spec_dir(study_dir), "outputs", paste0(ids, ".yml"))
  ids <- ids[file.exists(src)]
  src <- src[file.exists(src)]
  file.copy(src, area, overwrite = TRUE)
  if (length(ids)) {
    log <- .ai_adopted_read(study_dir)
    log <- rbind(log[!log$output_id %in% ids, , drop = FALSE],
                 data.frame(output_id = ids, adopted = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                            md5 = unname(tools::md5sum(src)), stringsAsFactors = FALSE))
    utils::write.csv(log, .ai_adopted_file(study_dir), row.names = FALSE, fileEncoding = "UTF-8")
  }
  invisible(ids)
}
