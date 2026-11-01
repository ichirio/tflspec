# The agent's drafts, from a stored answer (tests/testthat/fixtures/
# answers-toc, the answers' form of before): each report of the answer
# written as ai/spec/outputs/<id>.yml, its assumptions under the draft's
# `ai:` key -- as the agent writes them now.  A figure's answer (its
# design) is written as the figure's report file, as the check writes it.
# Returns the ids written.
ai_drafts_from_answer <- function(study_dir, text, task = "table") {
  if (length(text) == 1L && file.exists(text)) {
    text <- readLines(text, encoding = "UTF-8", warn = FALSE)
  }
  a <- tfl_ai_parse(text, task)
  area <- file.path(study_dir, "ai", "spec", "outputs")
  dir.create(area, recursive = TRUE, showWarnings = FALSE)
  if (identical(task, "figure")) {
    id <- a$output_id
    ctxs <- readRDS(file.path(study_dir, "ai", "requests", "_contexts.rds"))
    ctx <- NULL
    for (x in ctxs) {
      if (identical(x$context$task, "figure") && id %in% tflspec:::.ai_file_id(x$ids)) ctx <- x$context
    }
    stopifnot(!is.null(ctx))
    a <- tflspec:::.ai_fig_as_files(a, id, ctx)
    stopifnot(is.null(a$error))
  }
  rows <- a$assumption_rows
  for (id in names(a$outputs)) {
    o <- a$outputs[[id]]
    r <- if (!is.null(rows)) rows[is.na(rows$output_id) | rows$output_id == id, , drop = FALSE]
    if (!is.null(r) && nrow(r)) {
      o$ai <- list(assumptions = lapply(seq_len(nrow(r)), function(i) {
        e <- list(where = r$where[i], item = r$item[i], assumption = r$assumption[i],
                  value = r$value[i])
        if ("question" %in% names(r) && !is.na(r$question[i])) e$question <- r$question[i]
        e[!vapply(e, function(v) is.null(v) || (length(v) == 1L && is.na(v)), NA)]
      }))
    }
    tfl_write_output(o, file.path(area, paste0(id, ".yml")))
  }
  invisible(names(a$outputs))
}

# Every stored answer of a fixtures folder as drafts (figure-*.md as
# figures); `only`: the requests' names to take (NULL: all)
ai_drafts_from_fixtures <- function(study_dir, dir = test_path("fixtures", "answers-toc"),
                                    only = NULL) {
  fs <- list.files(dir, "[.]md$", full.names = TRUE)
  if (!is.null(only)) fs <- fs[sub("[.]md$", "", basename(fs)) %in% only]
  ids <- character()
  for (f in fs) {
    task <- if (startsWith(basename(f), "figure")) "figure" else "table"
    ids <- c(ids, ai_drafts_from_answer(study_dir, f, task))
  }
  invisible(ids)
}
