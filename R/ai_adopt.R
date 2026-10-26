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
#' A report adopted that the study's `spec/toc.yml` does not list (a part
#' the AI wrote, a report it named otherwise) gets an entry there -- after
#' its number's, else at the end -- so the study's report list shows it.
#'
#' @param dry_run `TRUE` to copy nothing and say what adopting would do:
#'   `status` `new` (no study file yet), `replace` (the study's file differs
#'   from the draft: adopting writes over it, with `overwrite = TRUE`),
#'   `same`, `no draft` -- for a screen's question before it adopts.
#' @return A data frame, one row a report: `output_id`, `status`
#'   (`adopted`, `same` -- the study's file is the draft already --, `kept`
#'   -- the study's file differs and was left, pass `overwrite = TRUE` --,
#'   `no draft`), `file` (the study's file), `toc` (`added`: its entry was
#'   added to `spec/toc.yml`; with `dry_run`, `add`).  The adoptions are recorded in
#'   `ai/adopted.csv` (the report, when, the draft's md5).
#' @seealso [tfl_ai_migrate()], [tfl_ai_collect_answers()]
#' @export
tfl_ai_adopt <- function(study_dir, outputs = NULL, overwrite = FALSE, dry_run = FALSE) {
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
    if (isTRUE(dry_run)) {
      st <- if (!file.exists(dest)) "new" else
        if (identical(unname(tools::md5sum(dest)), md5)) "same" else "replace"
      return(data.frame(output_id = id, status = st, file = rel))
    }
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
  rownames(out) <- NULL
  # (the report list shows what spec/toc.yml lists: a report adopted that
  # it does not -- a part, a report the AI named -- gets its entry)
  in_toc <- .ai_toc_ids(study_dir)
  if (isTRUE(dry_run)) {
    out$toc <- ifelse(out$status %in% c("new", "replace", "same") & !out$output_id %in% in_toc,
                      "add", NA_character_)
    return(out)
  }
  added <- .ai_toc_add(study_dir, out$output_id[out$status == "adopted" &
                                                  !out$output_id %in% in_toc])
  out$toc <- ifelse(out$output_id %in% added, "added", NA_character_)
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

#' What the AI changed in a report since it was adopted
#'
#' A report adopted from the AI's draft ([tfl_ai_adopt()]) and redrafted by
#' the AI since ([tfl_ai_outputs()]'s `adopted` `yes, draft newer`): its
#' study file and its draft compared field by field, for the person to see
#' before adopting again.  The two YAML files are read as they are written
#' (an `!r` value as its text), each value under its path: `label`,
#' `page.titles.2.center`, `ard.analyses[AE].where` (an item of a list
#' named by its `id`, else by its place, `[2]`).
#'
#' @param study_dir The study folder.
#' @param output_id The report; `NULL`, every report whose draft is newer
#'   than its adoption.
#' @return A data frame, one row a value that differs: `output_id`, `where`
#'   (the path), `change` (`changed`, `added` -- in the draft only --,
#'   `removed` -- in the study's file only), `adopted` and `draft` (the
#'   value, one line: a short YAML; `NA` where there is none); no row when
#'   they are the same.  Its attribute `text` says how many, for a screen.
#' @seealso [tfl_ai_adopt()], [tfl_ai_outputs()]
#' @export
tfl_ai_adopted_diff <- function(study_dir = ".", output_id = NULL) {
  a <- .ai_area(study_dir)
  empty <- data.frame(output_id = character(), where = character(), change = character(),
                      adopted = character(), draft = character(), stringsAsFactors = FALSE)
  if (is.null(a)) return(structure(empty, text = "no drafts apart (ai/spec/outputs)"))
  if (is.null(output_id)) {
    o <- tfl_ai_outputs(study_dir)
    output_id <- o$output_id[o$adopted %in% "yes, draft newer"]
  }
  rows <- lapply(output_id, function(id) {
    f_new <- file.path(study_dir, a, paste0(id, ".yml"))
    f_old <- file.path(.spec_dir(study_dir), "outputs", paste0(id, ".yml"))
    old <- if (file.exists(f_old)) .ai_yaml_paths(f_old) else character()
    new <- if (file.exists(f_new)) .ai_yaml_paths(f_new) else character()
    keys <- union(names(old), names(new))
    if (!length(keys)) return(NULL)
    o <- unname(old[keys])
    n <- unname(new[keys])
    ch <- ifelse(is.na(o), "added", ifelse(is.na(n), "removed", "changed"))
    keep <- is.na(o) | is.na(n) | o != n
    if (!any(keep)) return(NULL)
    data.frame(output_id = id, where = keys[keep], change = ch[keep], adopted = o[keep],
               draft = n[keep], stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, c(list(empty), rows))
  rownames(out) <- NULL
  n_out <- length(unique(out$output_id))
  structure(out, text = if (!nrow(out)) "no differences" else
    sprintf("%d difference%s in %d report%s", nrow(out), if (nrow(out) == 1L) "" else "s",
            n_out, if (n_out == 1L) "" else "s"))
}

# A report file's values, each as one line of YAML under its path (an `!r`
# value as its text)
.ai_yaml_paths <- function(f) {
  h <- .yaml_handlers()
  h$r <- function(x) paste("!r", x)
  h$unset <- function(x) "!unset"
  y <- yaml::read_yaml(f, handlers = h)
  out <- character()
  one <- function(v) {
    if (is.null(v)) return("~")
    if (is.atomic(v) && length(v) == 1L) return(as.character(v))
    gsub("\n", " ", sub("\n$", "", yaml::as.yaml(v, line.sep = "\n", column.major = FALSE)),
         fixed = TRUE)
  }
  walk <- function(x, path) {
    if (is.list(x) && length(x)) {
      nm <- names(x)
      for (i in seq_along(x)) {
        el <- x[[i]]
        key <- if (!is.null(nm) && nzchar(nm[i])) {
          if (nzchar(path)) paste(path, nm[i], sep = ".") else nm[i]
        } else {
          id <- if (is.list(el) && length(el[["id"]]) == 1L) as.character(el[["id"]])
          sprintf("%s[%s]", path, id %||% i)
        }
        walk(el, key)
      }
    } else {
      out[[path]] <<- one(x)
    }
  }
  walk(y, "")
  out
}

# The ids spec/toc.yml lists
.ai_toc_ids <- function(study_dir) {
  f <- file.path(.spec_dir(study_dir), "toc.yml")
  if (!file.exists(f)) return(character())
  e <- tryCatch(yaml::read_yaml(f)$outputs, error = function(e) NULL) %||% list()
  vapply(e, function(x) as.character(x$id %||% NA_character_), "")
}

# Reports adopted into the study's spec/toc.yml: each an entry from its
# report file (id, kind, label, population), after the entry of its number
# (a part t.15.1.5.1.a after t.15.1.5.1 and its parts there), else at the
# end.  The ids added.
.ai_toc_add <- function(study_dir, ids) {
  if (!length(ids)) return(character())
  f <- file.path(.spec_dir(study_dir), "toc.yml")
  y <- if (file.exists(f)) tryCatch(yaml::read_yaml(f), error = function(e) NULL) else NULL
  toc <- y$outputs %||% list()
  base_key <- function(id) {
    k <- tryCatch(.ai_id_key(id), error = function(e) NA_character_)
    if (is.na(k)) k else sub("([0-9])[A-Z]$", "\\1", k)
  }
  added <- character()
  for (id in ids) {
    o <- tryCatch(tfl_read_output(file.path(.spec_dir(study_dir), "outputs", paste0(id, ".yml"))),
                  error = function(e) NULL)
    e <- list(id = id, kind = o$kind %||% "table")
    if (!is.null(o) && !is.na(o$label %||% NA)) e$label <- o$label
    if (!is.null(o) && !is.na(o$population %||% NA)) e$population <- o$population
    b <- base_key(id)
    near <- which(vapply(toc, function(x) identical(base_key(x$id %||% ""), b), NA))
    if (!is.na(b) && length(near)) {
      toc <- append(toc, list(e), after = max(near))
    } else {
      toc[[length(toc) + 1L]] <- e
    }
    added <- c(added, id)
  }
  y$outputs <- toc
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  .ai_write_text(yaml::as.yaml(y, indent.mapping.sequence = TRUE), f)
  added
}
