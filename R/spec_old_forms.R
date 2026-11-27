# ============================================================================
#  The old forms of the SPEC, and what to write instead (canon 11-10
#  :377-:379, 11-12 :380, :433).  A reader reads the current form only: an
#  old form stops with the one line that says what to write instead -- a
#  classed condition, `tflspec_old_form` (`cnd$form` the form's id,
#  `cnd$where` the file or report), that a GUI reads as data.  The
#  spec_version stop lists every line (.old_form_lines()).  One table, one
#  place: a reader that finds an old form calls .old_form_stop().
# ============================================================================

# The old forms: `id`; the form as it was written (`was`); what to write
# instead (`instead`); the canon's line (PRINCIPLES.md) that ended it.
# (A function: the population line is the report reader's, R/spec_yaml.R.)
.old_forms <- function() {
  rows <- list(
    c("spec_version", "a study.yml with no `spec_version`, or one below 2",
      paste0("the SPEC's form changed (spec_version 2): change the old forms below that the study ",
             "has, then write `spec_version: 2` in study.yml"),
      "377"),
    c("patterns_dir", "spec/patterns/<name>.yml (a layer a report extends)",
      paste0("a pattern's page is a page configuration (spec/defaults.yml `styles: <name>:`, which a ",
             "report picks with `style: <name>`), its content the study's defaults (spec/defaults.yml ",
             "`content:`) or the reports' own; move it there and remove the folder"),
      "380, 433"),
    c("extends_pattern", "`extends: patterns/<name>`",
      paste0("`extends` names `defaults` or `none`, or a map by step ({content: none}); a report picks ",
             "its page configuration with `style: <name>`"),
      "342, 380"),
    c("item_fn_map", "a content item written as a map `{fn: <fn>, args: {...}}`",
      "write it `- <fn>: {<args>}`", "378"),
    c("titles_at", "study.yml `settings: titles_at`",
      paste0("a title prints where it is written: a line of a report's `page: titles` above the ",
             "table, a line of its `page: header` in the running header; remove the setting"),
      "433"),
    c("footnotes_at", "study.yml `settings: footnotes_at`",
      paste0("a footnote prints where it is written: a line of a report's `page: footnotes` below ",
             "the table, a line of its `page: footer` in the running footer; remove the setting"),
      "433"),
    c("population", "a report's top-level `population:`",
      sub("^`population:` -> ", "", .common_old_line), "380"),
    c("defaults_page", "spec/defaults.yml `page:`",
      paste0("it moved to a page configuration (styles:): write the page's settings and lines ",
             "under `styles: <name>: page:`, and the tokens under study.yml `tokens:`"),
      "380"),
    c("series_settings", "study.yml `series_settings:`",
      paste0("a study changes a series's values in its copy of the series: move them into ",
             "spec/series/<series>/series.yml `values:` (tfl_series_values_set()), after ",
             "tfl_copy_templates()"),
      "778"),
    c("report_settings", "a report's `settings:` (its `file:`)",
      paste0("a report's RTF file and program are its TOC entry's (spec/toc.yml `file:`, ",
             "`program:`), else the study's (study.yml `settings: file`, `program`)"),
      "380"))
  m <- do.call(rbind, rows)
  data.frame(id = m[, 1L], was = m[, 2L], instead = m[, 3L], canon = m[, 4L],
             stringsAsFactors = FALSE)
}

# Stop on the old form `id` found at `where` (a file, a report: "tfl_read_
# study_spec(): spec/patterns/"); `was` and `instead` replace the table's
# text where the place knows better (a series by name)
.old_form_stop <- function(id, where, was = NULL, instead = NULL) {
  f <- .old_forms()
  r <- f[f$id == id, , drop = FALSE]
  if (!nrow(r)) stop(".old_form_stop(): no old form ", id, call. = FALSE)
  msg <- sprintf("%s: %s is no longer read: %s.", where, was %||% r$was, instead %||% r$instead)
  stop(structure(class = c("tflspec_old_form", "error", "condition"),
                 list(message = sub("[.]+$", ".", msg), call = NULL, form = id, where = where)))
}

# Every old form and what to write instead, one line each ("- was -> instead"):
# the spec_version stop's list, and the help's
.old_form_lines <- function() {
  f <- .old_forms()
  f <- f[f$id != "spec_version", , drop = FALSE]
  sprintf("- %s -> %s", f$was, f$instead)
}

# The known name closest to `k` (2 edits or fewer, or one the start of the
# other), NA when none is close
.near_name <- function(k, known) {
  if (!length(known)) return(NA_character_)
  d <- drop(utils::adist(k, known))
  near <- which(d <= 2L | startsWith(known, k) | startsWith(k, known))
  if (length(near)) known[near[which.min(d[near])]] else NA_character_
}

# Stop on a key `k` that is not one of `known` (a closed list: canon 11-10
# :379, an unknown name stops and says how to fix it in one line), naming
# the closest known one when one is close, else the known ones
.spec_key_stop <- function(k, known, where, what = "a key") {
  near <- .near_name(k, known)
  hint <- if (!is.na(near)) sprintf("did you mean `%s:`?", near) else
    sprintf("the keys are %s.", paste0("`", known, "`", collapse = ", "))
  stop(sprintf("%s: `%s:` is not %s; %s", where, k, what, hint), call. = FALSE)
}

# study.yml read as a list (`study`; `dir` the study folder, NULL when not
# known): its old forms stop.  `file`, the study.yml it was read from, also
# checks its spec_version (a list given in code has none to check).  Both
# readers of study.yml call it: tfl_read_study_spec() and .tr_study().
.study_yml_form_check <- function(study, dir = NULL, file = NULL) {
  if (!is.list(study)) return(invisible())
  where <- file %||% "study.yml"
  .series_settings_gone(study, dir)
  # (where a title or footnote prints is where it is written: canon 11-12 :433)
  set <- study[["settings"]]
  for (k in c("titles_at", "footnotes_at")) {
    if (is.list(set) && !is.null(set[[k]])) .old_form_stop(k, where, was = sprintf("`settings: %s`", k))
  }
  invisible()
}
