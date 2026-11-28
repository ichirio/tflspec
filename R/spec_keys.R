# ============================================================================
#  The keys each SPEC file takes: one closed list (canon 11-28 :465,
#  spec-keys-v2.md 2-4).  Every reader, writer and check reads it: study.yml
#  (its top level, `settings:`, `ai:`, `keys:`) and a report file (its top
#  level, its `ai:`; its `common:`, the maps of its analysis step and its
#  data lines by .common_keys and .yaml_known_keys).  A key a file does not
#  take stops at read time: every such key of one file in one classed
#  error, `tflspec_spec_keys` (of the family `tflspec_spec_form`, as
#  `tflspec_old_form` is), its fields `file`, `keys` (the key paths) and
#  `problems` (a row a key: what it is, what to do).  Each key's line ends
#  in what to do -- a typo (the nearest name), an old form (where it moved),
#  your own memo (`note:` or a `#` comment), a new need (an Issue) -- and
#  the keys the file takes follow.  tfl_check_spec() gives the same rows.
#  No key is dropped in silence and none is only warned of.
# ============================================================================

# The keys: `part`, the map they are keys of ("study", study.yml's top
# level, and its maps "settings", "ai", "keys"; "report", a report file's
# top level, and its "report_ai"); `key`; for a report, `kinds`, the kinds
# that have it ("" every kind), and `field`, a part tfl_output() itself
# holds (the rest ride on the report as they are).  A report's keys are in
# the order the writer writes them (canon 11-28 CANON-3): who it is (id,
# kind, label), the layers it picks (template, style, extends), the named
# things several steps use (codelists, common), the steps (data, analysis,
# content, page), the rest, the run control (last_step).  `common` until
# it goes (A2-T T4); `last_step` takes only `page` until the steps are
# built (canon 11-10 :338).
.spec_keys <- local({
  part <- function(p, keys, kinds = "", field = FALSE) {
    data.frame(part = p, key = keys, kinds = kinds, field = field, stringsAsFactors = FALSE)
  }
  rbind(
    part("study", c("spec_version", "study_id", "title", "short_title", "compound", "phase",
                    "description", "settings", "setup", "tokens", "ai", "packages", "keys",
                    "created", "updated", "tflplanner")),
    part("settings", c("output_path", "program_dir", "program", "file", "style", "adam_levels",
                       "adam_levels_file", "adam_levels_applied", "treatment", "sd_missing",
                       "formats", "params")),
    part("ai", c("run", "review", "language", "requests_per_chat", "carry", "max_rounds",
                 "shell_footnotes", "shell_code")),
    part("keys", c("id", "output", "source", "study_id")),
    part("report", c("id", "kind", "label"), field = TRUE),
    part("report", c("template", "style")),
    part("report", c("extends", "codelists", "common", "data", "analysis", "content"), field = TRUE),
    part("report", "contents"),
    part("report", "page", field = TRUE),
    part("report", c("plot", "figure", "plots", "compose", "ggplot2_version"), kinds = "figure"),
    part("report", c("note", "ai", "last_step")),
    part("report_ai", c("assumptions", "confirm")))
})

# The old keys, by the map they were keys of, and their old form
# (.old_forms(): where each moved)
.spec_keys_old <- data.frame(
  part = c("study", "study", "settings", "settings", "settings", "settings", "ai", "ai", "ai",
           "keys", "report", "report", "report", "report", "report", "report"),
  key = c("series_settings", "series", "titles_at", "footnotes_at", "rounding", "shell_code",
          "ask_when", "footnotes_at", "titles_in_header", "rounding",
          "population", "settings", "ard", "layers", "outputs", "base"),
  form = c("series_settings", "study_series", "titles_at", "footnotes_at", "settings_rounding",
           "settings_shell_code", "ai_ask_when", "ai_footnotes_at", "ai_titles_in_header",
           "ard_study_rounding", "population", "report_settings", "ard_key", "report_layers",
           "report_outputs", "report_outputs"),
  stringsAsFactors = FALSE)

# The keys of `part` (a report's: those of its `kind`); `more`, the keys a
# study's templates add (`settings`: .settings_declared())
.spec_keys_of <- function(part, kind = NULL, more = character()) {
  k <- .spec_keys[.spec_keys$part == part, , drop = FALSE]
  if (identical(part, "report") && !is.null(kind)) k <- k[!nzchar(k$kinds) | k$kinds == kind, , drop = FALSE]
  unique(c(k$key, more))
}

# The map a part is, as its keys' line names it
.spec_keys_label <- function(part, kind = NULL) {
  switch(part,
         study = "study.yml", settings = "study.yml `settings:`", ai = "study.yml `ai:`",
         keys = "study.yml `keys:`",
         report = if (length(kind) && nzchar(kind)) sprintf("a %s report", kind) else "a report",
         report_ai = "a report's `ai:`", part)
}

# The problems' frame: `file`; `key`, the key's path ("settings: file");
# `kind` -- "typo" (`near` the name it is close to), "old_form" (`form` its
# old form), "unknown" (a memo, or a new need), "kind" (a key of another
# kind of report), "shape" (a key the value of another key has), "later"
# (not built yet), "value" (a value the key does not take); `text`, the
# key's line after the path; `more`, lines to write under it ("" none);
# `takes`, the keys its map takes, as a line
.spec_keys_rows <- function(file = character(), key = character(), kind = character(),
                            text = character(), near = NA_character_, form = NA_character_,
                            more = "", takes = NA_character_) {
  n <- length(key)
  if (!n) {
    return(data.frame(file = character(), key = character(), kind = character(),
                      near = character(), form = character(), text = character(),
                      more = character(), takes = character(), stringsAsFactors = FALSE))
  }
  data.frame(file = rep_len(file, n), key = key, kind = rep_len(kind, n),
             near = rep_len(near, n), form = rep_len(form, n), text = rep_len(text, n),
             more = rep_len(more, n), takes = rep_len(takes, n), stringsAsFactors = FALSE)
}

# The keys of the map `x` (`part`, at `path` in `file`) that it does not
# take: a frame of .spec_keys_rows().  `kind`, a report's kind; `ok`, the
# keys when the map is not one of .spec_keys (an analysis item ...), with
# `label` its name; `more`, keys a study's templates add; `dir`, the study
# folder (an old form's line names it)
.spec_keys_find <- function(x, part, file, path = character(), kind = NULL, ok = NULL,
                            label = NULL, more = character(), dir = NULL) {
  if (!is.list(x) || is.null(names(x))) return(.spec_keys_rows())
  keys <- names(x)[nzchar(names(x))]
  ok <- unique(ok %||% .spec_keys_of(part, kind, more))
  bad <- setdiff(keys, ok)
  if (!length(bad)) return(.spec_keys_rows())
  pre <- if (length(path)) paste0(paste(path, collapse = ": "), ": ") else ""
  takes <- sprintf("%s takes %s.", label %||% .spec_keys_label(part, kind),
                   paste0("`", ok, "`", collapse = ", "))
  # (a report's maps: its `note:` holds a memo; study.yml, a `#` comment)
  memo <- if (part %in% c("report", "report_ai", "common", "analysis", "data")) {
    "under `note:` or in a `#` comment"
  } else {
    "in a `#` comment"
  }
  kind_shown <- if (length(kind) && !is.na(kind[1L]) && nzchar(kind[1L])) kind[1L] else "?"
  rows <- lapply(bad, function(k) {
    old <- .spec_keys_old$form[.spec_keys_old$part == part & .spec_keys_old$key == k]
    if (length(old)) {
      o <- .spec_keys_old_text(old[1L], x[[k]], dir)
      return(.spec_keys_rows(file, paste0(pre, k), "old_form",
                             paste("an old form, no longer read:", o$text), form = old[1L],
                             more = o$more, takes = takes))
    }
    all_kinds <- .spec_keys[.spec_keys$part == part & .spec_keys$key == k, , drop = FALSE]
    if (nrow(all_kinds)) {
      return(.spec_keys_rows(file, paste0(pre, k), "kind",
                             sprintf("a key of a %s report, not of a %s one; remove it.",
                                     all_kinds$kinds[1L], kind_shown), takes = takes))
    }
    near <- .near_name(k, ok)
    if (!is.na(near)) {
      return(.spec_keys_rows(file, paste0(pre, k), "typo",
                             sprintf("not a key: a typo? did you mean `%s%s:`?", pre, near),
                             near = near, takes = takes))
    }
    .spec_keys_rows(file, paste0(pre, k), "unknown", paste0(
      "not a key. Your own memo: write it ", memo, ". A new need: it is not a key ",
      "(spec-keys-v2), ask for it in an Issue."), takes = takes)
  })
  do.call(rbind, rows)
}

# An old key's line: `text`, where it moved (the old form's), and `more`,
# the YAML to write ("" none); `value`, the key's value; `dir`, the study
# folder
.spec_keys_old_text <- function(form, value, dir = NULL) {
  r <- function(v) if (is.character(v) && length(v) == 1L && v %in% c("sas", "r")) v else "sas"
  switch(form,
    settings_rounding = list(text = .force_instead(r(value), "and remove the setting"),
                             more = paste(.force_yaml(r(value)), collapse = "\n")),
    ard_study_rounding = list(text = .force_instead(r(value), "and remove the row"),
                              more = paste(.force_yaml(r(value)), collapse = "\n")),
    series_settings = list(text = .series_settings_instead(value, dir), more = ""),
    list(text = sub("[.]+$", ".", paste0(.old_forms()$instead[.old_forms()$id == form], ".")),
         more = ""))
}

# Raise the problems `p` (.spec_keys_rows()): none, nothing; else, while a
# reader gathers them (.spec_keys_collect()), kept; else they stop
.spec_keys_state <- new.env(parent = emptyenv())
.spec_keys_raise <- function(p) {
  if (!NROW(p)) return(invisible())
  if (!is.null(.spec_keys_state$rows)) {
    .spec_keys_state$rows <- rbind(.spec_keys_state$rows, p)
    return(invisible())
  }
  .spec_keys_stop(p)
}

# `expr` evaluated with its problems gathered, not stopped: list(value,
# problems)
.spec_keys_collect <- function(expr) {
  old <- .spec_keys_state$rows
  .spec_keys_state$rows <- .spec_keys_rows()
  on.exit(.spec_keys_state$rows <- old, add = TRUE)
  value <- expr
  list(value = value, problems = .spec_keys_state$rows)
}

# The one stop of the problems `p`, a block a file: the file and each key's
# path and line, the YAML to write under it, then the keys its maps take.
# `spec`, what the read gives with the keys left out (a study that one key
# of study.yml stops still opens in a GUI: cnd$spec)
.spec_keys_stop <- function(p, spec = NULL) {
  files <- unique(p$file)
  msg <- unlist(lapply(files, function(f) {
    q <- p[p$file == f, , drop = FALSE]
    takes <- unique(q$takes[!is.na(q$takes)])
    more <- vapply(q$more, function(m) if (nzchar(m)) {
      paste0("\n", paste0("      ", strsplit(m, "\n", fixed = TRUE)[[1L]], collapse = "\n"))
    } else "", "")
    c(sprintf("%s: %s it does not take:", f,
              if (nrow(q) == 1L) "a key" else sprintf("%d keys", nrow(q))),
      sprintf("  - `%s`: %s%s", q$key, q$text, more),
      if (length(takes)) paste0("  ", takes))
  }))
  stop(structure(class = c("tflspec_spec_keys", "tflspec_spec_form", "error", "condition"),
                 list(message = paste(msg, collapse = "\n"), call = NULL, file = files,
                      keys = p$key, problems = p, spec = spec)))
}

# The problems as tfl_check_spec() rows: part "file", the item the file and
# the key's path, level error
.spec_keys_check_rows <- function(p, output_id = NA_character_) {
  if (!NROW(p)) return(.check_rows())
  # (the YAML to write, an old form's, below the line)
  more <- ifelse(nzchar(p$more), paste0("\n", p$more), "")
  .check_rows(output_id = output_id, part = "file", item = paste0(basename(p$file), ": ", p$key),
              level = "error", message = paste0("`", p$key, "`: ", p$text, more))
}

# A study's spec where a reader goes on without one: the study read, or,
# when only its keys stop (tflspec_spec_keys), the study read without
# them (the error's `spec`); NULL when it cannot be read
.study_spec_or_null <- function(dir) {
  tryCatch(tfl_read_study_spec(dir),
           tflspec_spec_keys = function(e) e$spec,
           error = function(e) NULL)
}

# ---- study.yml ---------------------------------------------------------------

# The study's `settings:` keys its templates declare they read, beyond the
# list: a registered template's parameter (`tfl_param(study =
# "settings.<k>")`) and a series template copied into the study (`!study
# settings.<k>`, `!study {key: settings.<k>, ...}` with its keys in any
# order) -- the study's copies (`dir` the study folder) and the series
# folders of the option tflspec.series_dir
.settings_declared <- function(dir = NULL) {
  out <- character()
  for (nm in ls(.tpl_registry)) {
    t <- get0(nm, envir = .tpl_registry, inherits = FALSE)
    for (p in t$params %||% list()) out <- c(out, as.character(unlist(p$study)))
  }
  sd <- c(if (!is.null(dir)) file.path(.spec_dir(dir), "series"),
          as.character(unlist(getOption("tflspec.series_dir"))))
  for (s in sd[nzchar(sd) & dir.exists(sd)]) {
    for (f in list.files(s, "\\.ya?ml$", recursive = TRUE, full.names = TRUE)) {
      txt <- readLines(f, warn = FALSE, encoding = "UTF-8")
      hit <- regmatches(txt, gregexpr(
        "!study\\s*(\\{[^}]*?\\bkey:\\s*)?['\"]?settings[.][A-Za-z][A-Za-z0-9_]*", txt, perl = TRUE))
      out <- c(out, unlist(hit))
    }
  }
  k <- regmatches(out, regexpr("settings[.][A-Za-z][A-Za-z0-9_]*", out))
  unique(sub("^settings[.]", "", k))
}

# study.yml read as a list (`study`; `dir` the study folder, NULL when not
# known; `file` its name in the lines): the keys it, its `settings:`, `ai:`
# and `keys:` do not take.  With no folder the settings its series declare
# are not known: only the old `settings:` keys stop there.
.study_yml_key_problems <- function(study, dir = NULL, file = "study.yml") {
  if (!is.list(study)) return(.spec_keys_rows())
  p <- .spec_keys_find(study, "study", file, dir = dir)
  st <- study[["settings"]]
  if (.is_map(st)) {
    more <- if (!is.null(dir)) .settings_declared(dir) else
      setdiff(names(st), .spec_keys_old$key[.spec_keys_old$part == "settings"])
    p <- rbind(p, .spec_keys_find(st, "settings", file, "settings", more = more, dir = dir))
  }
  for (k in c("ai", "keys")) {
    if (.is_map(study[[k]])) p <- rbind(p, .spec_keys_find(study[[k]], k, file, k, dir = dir))
  }
  p
}

# `study` (study.yml read as a list) without the keys of the problems `p`
# (.study_yml_key_problems(): a top-level key, or one of `settings:`,
# `ai:`, `keys:`), its attributes kept
.study_yml_clean <- function(study, p) {
  if (!is.list(study) || !NROW(p)) return(study)
  for (k in p$key) {
    path <- strsplit(k, ": ", fixed = TRUE)[[1L]]
    if (length(path) == 1L) {
      study[[path]] <- NULL
    } else if (length(path) == 2L && .is_map(study[[path[1L]]])) {
      study[[path[1L]]][[path[2L]]] <- NULL
    }
  }
  study
}

# A study's `ai:` (`ai`, from `file`: study.yml or spec/defaults.yml, the
# study standard's): the keys it does not take
.ai_key_problems <- function(ai, file) {
  if (identical(basename(file), "defaults.yml")) {
    return(.spec_keys_find(ai, "ai", "spec/defaults.yml", "ai", label = "spec/defaults.yml `ai:`"))
  }
  .spec_keys_find(ai, "ai", file, "ai")
}
.ai_keys_check <- function(ai, file) {
  .spec_keys_raise(.ai_key_problems(ai, file))
  invisible()
}

# ---- a report ----------------------------------------------------------------

# The names a figure's `plot:` holds: the designer's figure-wide settings
# (.fig_plot_fields()) and the ones a template reads from it
.plot_setting_names <- function() {
  unique(c(.fig_plot_fields()$field, "risk_title", "facet", "scales"))
}

# The problems of a report's keys (`y`, its file read as a list; `file`):
# the top-level keys (.spec_keys, by its kind), `last_step` other than
# `page`, a registered template's old `ard:` part, the keys of its `ai:`,
# `common:`, its analysis step's maps and its data lines, and a key whose
# value has the shape of its twin's (`plot` / `plots`, `content` /
# `contents`)
.output_key_problems <- function(y, file) {
  kind <- as.character(y[["kind"]] %||% "")[1L]
  p <- .spec_keys_find(y, "report", file, kind = kind)
  row <- function(key, kind, text, ...) .spec_keys_rows(file, key, kind, text, ...)
  ls <- y[["last_step"]]
  if ("last_step" %in% names(y) && !identical(ls, "page")) {
    p <- rbind(p, row("last_step", "later", paste0(
      "not built yet: a report is made to its last step, `page`, the one value read; ",
      "write `page` or remove the key.")))
  }
  tpl <- y[["template"]]
  if (.is_tpl_spec(tpl) && !.tpl_is_file_name(tpl$name) && "ard" %in% names(tpl)) {
    p <- rbind(p, row("template: ard", "old_form", paste("an old form, no longer read:",
                                                        .old_form_instead("template_ard")),
                      form = "template_ard"))
  }
  if (.is_map(y[["ai"]])) p <- rbind(p, .spec_keys_find(y[["ai"]], "report_ai", file, "ai"))
  if (.is_map(y[["common"]])) {
    p <- rbind(p, .spec_keys_find(y[["common"]], "common", file, "common", ok = .common_keys,
                                  label = "`common:`"))
  }
  named <- function(x) is.list(x) && !is.null(names(x))
  an <- y[["analysis"]]
  if (named(an)) {
    p <- rbind(p, .spec_keys_find(an, "analysis", file, "analysis",
                                  ok = .yaml_known_keys$analysis_part, label = "`analysis:`"))
    for (a in if (is.list(an$analyses) && is.null(names(an$analyses))) an$analyses) {
      if (named(a)) {
        p <- rbind(p, .spec_keys_find(a, "analysis", file, c("analysis", "analyses", a$id %||% "?"),
                                      ok = .yaml_known_keys$analysis, label = "an analysis"))
      }
    }
  }
  d <- y[["data"]]
  if (is.list(d) && is.null(names(d))) {
    for (r in d) {
      if (named(r)) {
        p <- rbind(p, .spec_keys_find(r, "data", file, c("data", r$id %||% "?"),
                                      ok = .yaml_known_keys$data, label = "a data line"))
      }
    }
  }
  # (two keys a letter apart, both right: the value's shape says which.  A
  # `plot:` of maps is one figure's when a name of it is a setting --
  # `colours: {CR: darkgreen}` -- and none of its maps holds a plot's parts)
  maps_of_maps <- function(x) .is_map(x) && length(x) && all(vapply(x, .is_map, NA))
  plot_parts <- c("data", "content", "template")
  plots_like <- function(x) {
    maps_of_maps(x) && (!any(names(x) %in% .plot_setting_names()) ||
                          any(vapply(x, function(m) any(names(m) %in% plot_parts), NA)))
  }
  if (!is.null(y[["plot"]]) && plots_like(y[["plot"]])) {
    p <- rbind(p, row("plot", "shape", paste0(
      "holds plots by name, which is `plots:` (a composed figure); `plot:` is one figure's ",
      "settings (`theme`, `base_size` ...).")))
  }
  if (length(y[["plots"]]) && !maps_of_maps(y[["plots"]])) {
    p <- rbind(p, row("plots", "shape", paste0(
      "is the plots of a composed figure, by name (`plots: {km: {...}}`); one figure's ",
      "settings are `plot:`.")))
  }
  if (.is_map(y[["content"]]) && !inherits(y[["content"]], "tfl_r")) {
    p <- rbind(p, row("content", "shape", paste0(
      "is a list of items (`- <fn>: {args}`); objects by name on the report's pages are ",
      "`contents:`.")))
  }
  cs <- y[["contents"]]
  if (!is.null(cs) && is.list(cs) && length(cs) && is.null(names(cs))) {
    p <- rbind(p, row("contents", "shape", paste0(
      "is the report's objects by name (`contents: {demog: [...]}`); one list of items is ",
      "`content:`.")))
  }
  p
}
