# ============================================================================
#  The old forms of the SPEC, and what to write instead (canon 11-10
#  :377-:379, 11-12 :380, :433).  A reader reads the current form only: an
#  old form stops with the one line that says what to write instead -- a
#  classed condition, `tflspec_old_form` (`cnd$form` the form's id,
#  `cnd$where` the file or report; of the family `tflspec_spec_form`), that
#  a GUI reads as data.  The spec_version stop lists every line
#  (.old_form_lines()).  One table, one place: a reader that finds an old
#  form calls .old_form_stop(); an old key of a file is a row of that
#  file's one stop of its keys (R/spec_keys.R).
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
    c("sheet_pattern", "a sheets' `pattern` cell that names no page configuration (a pattern file)",
      paste0("the `pattern` cell names a page configuration, the `@<name>` of rows of the page ",
             "sheets: add its rows, or blank the cell; content goes on the default rows or the ",
             "report's own"),
      "380"),
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
    c("ard_key", "a report's `ard:`",
      paste0("write `analysis:` (the same map): ", .ard_why), "587, 312"),
    c("template_ard", "`template: {ard:}`",
      paste0("write `template: {analysis:}` (the same map): ", .ard_why), "587, 312"),
    c("extends_ard", "`extends: {ard:}`",
      paste0("write `extends: {analysis:}` (the same map): ", .ard_why), "587, 312"),
    c("report_settings", "a report's `settings:` (its `file:`)",
      paste0("a report's RTF file and program are its TOC entry's (spec/toc.yml `file:`, ",
             "`program:`), else the study's (study.yml `settings: file`, `program`)"),
      "380"),
    c("study_series", "study.yml `series:`",
      paste0("a study's series are copied, whole, into spec/series/<series>/ ",
             "(tfl_copy_templates()), which the study reads; remove the key"),
      "462, 782, 465"),
    c("settings_shell_code", "study.yml `settings: shell_code`",
      "the AI kit's settings are study.yml `ai:`: write it `ai: shell_code:` (the same value)",
      "465"),
    c("ai_ask_when", "study.yml `ai: ask_when`",
      "`ai: run` (one or batch) replaces it; remove the key", "465"),
    c("ai_footnotes_at", "study.yml `ai: footnotes_at`",
      "write `ai: shell_footnotes:` (footer or table, the same value)", "465"),
    c("ai_titles_in_header", "study.yml `ai: titles_in_header`",
      paste0("it is taken from the study's page configuration (the tokens its header ",
             "prints); remove the key"), "465"),
    c("report_layers", "a report's top-level `layers:`",
      paste0("a figure's layers are its content's items: write each under `content:` ",
             "(`- <layer>: {...}`), after the template's"), "465, 308, 311"),
    c("report_outputs", "a report file's `outputs:` and `base:` (several reports in one file)",
      "one report a file: write each in its own spec/outputs/<id>.yml", "465"),
    c("ard_study_rounding", "the ARD sheets' `study` key `rounding` (study.yml `keys: rounding`)",
      .force_instead("sas", "and remove the row"), "368, 465"),
    c("header_n_column", "the tables sheet's column `header_n`",
      paste0("it is named `col_header_values` (the same value, plan_col_header(values = )): ",
             "rename the column"), "472"),
    # (the study's rounding has one place, defaults.yml `force:`: R/spec_force.R)
    c("settings_rounding", "study.yml `settings: rounding`",
      .force_instead("sas", "and remove the setting"), "368, 369, 378"),
    c("content_rounding", paste0("a `plan_digits: {rounding: }` card in the study's content ",
                                 "(spec/defaults.yml `content:`, a style set's `content:`)"),
      .force_instead("sas", "and take `rounding` out of the card"), "369, 378"))
  m <- do.call(rbind, rows)
  data.frame(id = m[, 1L], was = m[, 2L], instead = m[, 3L], canon = m[, 4L],
             stringsAsFactors = FALSE)
}

# Why the old key `ard:` went (the report's map, a template SPEC's part, a
# step of `extends`)
.ard_why <- "a report's second step is its analysis step, which makes its ARD (canon 11-15, `ard:` -> `analysis:`)"

# What to write instead of the old form `id`, one sentence
.old_form_instead <- function(id) {
  f <- .old_forms()
  sub("[.]+$", ".", paste0(f$instead[f$id == id], "."))
}

# The old form `id` as one line, "<was> is no longer read: <instead>." (a
# check's problem text, where nothing stops)
.old_form_text <- function(id) {
  f <- .old_forms()
  r <- f[f$id == id, , drop = FALSE]
  if (!nrow(r)) stop(".old_form_text(): no old form ", id, call. = FALSE)
  sprintf("%s is no longer read: %s.", r$was, r$instead)
}

# Stop on the old form `id` found at `where` (a file, a report: "tfl_read_
# study_spec(): spec/patterns/"); `was` and `instead` replace the table's
# text where the place knows better (a series by name); `more`, lines
# printed under the one line (the YAML to write, indented)
.old_form_stop <- function(id, where, was = NULL, instead = NULL, more = NULL) {
  f <- .old_forms()
  r <- f[f$id == id, , drop = FALSE]
  if (!nrow(r)) stop(".old_form_stop(): no old form ", id, call. = FALSE)
  msg <- sub("[.]+$", ".", sprintf("%s: %s is no longer read: %s.", where, was %||% r$was,
                                   instead %||% r$instead))
  if (length(more)) msg <- paste(c(msg, paste0("  ", more)), collapse = "
")
  stop(structure(class = c("tflspec_old_form", "tflspec_spec_form", "error", "condition"),
                 list(message = msg, call = NULL, form = id, where = where)))
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
  # (the edits a typo may have scale with the key: a short key one letter
  # added or dropped -- a letter changed makes another word, `date` / `data`
  # --, a longer one two edits)
  a <- utils::adist(k, known, counts = TRUE)
  d <- drop(a)
  sub <- drop(attr(a, "counts")[, , "sub"])
  ok <- if (nchar(k) < 5L) d <= 1L & sub == 0L else d <= 2L
  near <- which(ok | startsWith(known, k) | startsWith(k, known))
  if (length(near)) known[near[which.min(d[near])]] else NA_character_
}

# study.yml read as a list (`study`; `dir` the study folder, NULL when not
# known): its keys are the list's (R/spec_keys.R), every one it does not
# take -- an old form among them -- in one stop.  `file`, the study.yml it
# was read from, also checks its spec_version (a list given in code has
# none to check).  Both readers of study.yml call it: tfl_read_study_spec()
# and .tr_study(); so does the converter, on the sheets' study.  Its
# value: `study` without the keys it does not take (what a reader that
# gathers the problems, .spec_keys_collect(), goes on with).
.study_yml_form_check <- function(study, dir = NULL, file = NULL) {
  if (!is.list(study)) return(invisible(study))
  if (!is.null(file)) .check_spec_version(study[["spec_version"]], file)
  p <- .study_yml_key_problems(study, dir, file %||% "study.yml")
  .spec_keys_raise(p)
  invisible(.study_yml_clean(study, p))
}

# ---- a report's keys: a closed list (R/spec_keys.R) -------------------------

# The keys of a report file (.spec_keys' part "report"): `key`, `kinds`,
# `field`.  The reader, the writer and .output_fields read it.
.output_keys <- .spec_keys[.spec_keys$part == "report", c("key", "kinds", "field")]

# A report's keys (`keys`; `kind` its kind) checked against the list: the
# writer's, of the parts a report holds beside its fields (`who` the
# report, "tfl_write_output(): T-1"); every key it does not take in one stop
.output_keys_check <- function(keys, kind, who) {
  x <- stats::setNames(vector("list", length(keys)), keys)
  .spec_keys_raise(.spec_keys_find(x, "report", who, kind = as.character(kind %||% "")[1L]))
  invisible()
}
