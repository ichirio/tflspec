# A report written as a template SPEC: its YAML file's
#
#   template:
#     name: demog
#     args: {continuous: AGE, ...}
#
# (or `ard: {...}` -- what is computed -- and `content: {...}` -- how it is
# shown -- in place of the flat `args:`, or beside it)
#
# with `ard:` / `data:` / `content:` holding only what the report adds or
# changes.  tfl_resolve_output() puts the template's items between the
# study's layers (defaults, patterns) and the report's own; the program is
# the template's call (A, tfl_content_code()) with the study's and the
# report's lines after it; tfl_template_to_spec() writes the items into
# the file instead (B).  (A figure designer's `template: km_simple`, a
# name, is something else: a template SPEC is a map with `name`.)

# The report's template SPEC, or NULL
.tpl_spec_of <- function(output) {
  t <- output$template
  if (!is.list(t) || is.null(t$name)) return(NULL)
  # (a figure design's preset, not a template SPEC: the design's own)
  if (.fig_preset_named(t)) return(NULL)
  if (!is.character(t$name) || length(t$name) != 1L || !nzchar(t$name)) {
    .ard_stop(sprintf("%s: `template: name:` is one template's name.", output$id))
  }
  list(name = t$name, args = .tpl_spec_args(t))
}

# A template SPEC's arguments: the flat `args:`, and the two parts -- `ard:`
# (what is computed) and `content:` (how it is shown) -- as one list.  A name
# given twice (in `args:` and a part, or in both parts) stops.  A template
# that declares where its parameters act (`parts` of the registry) has a name
# in the other part stopped, naming the right one; a template with no parts
# takes the names as written.
.tpl_spec_args <- function(t) {
  # (the report's fields, not the template's arguments: canon 1.32)
  for (k in intersect(c("extends", "style"), names(t))) {
    .ard_stop(sprintf(paste0("template %s: `%s` is the report's field, not `template:`'s: write it ",
                             "as the report's field (`%s:` beside `template:`)."), t$name, k, k))
  }
  args <- as.list(t$args %||% list())
  tpl <- if (is.null(t$ard) && is.null(t$content)) NULL else
    tryCatch(.tpl_get(t$name, "template:"), error = function(e) NULL)
  for (part in c("ard", "content")) {
    p <- t[[part]]
    if (is.null(p)) next
    if (!is.list(p) || (length(p) && is.null(names(p)))) {
      .ard_stop(sprintf("template %s: `%s:` is a map of parameters (`%s: {...}`).", t$name, part, part))
    }
    other <- setdiff(c("ard", "content"), part)
    wrong <- if (!is.null(tpl$parts)) setdiff(names(p), tpl$parts[[part]]) else character()
    wrong <- intersect(wrong, tpl$parts[[other]])
    if (length(wrong)) {
      .ard_stop(sprintf("template %s: %s acts in `%s:`, not `%s:`.", t$name,
                        paste(sQuote(wrong, FALSE), collapse = ", "), other, part))
    }
    dup <- intersect(names(p), names(args))
    if (length(dup)) {
      .ard_stop(sprintf("template %s: %s is given twice (in `args:` and `%s:`, or in both parts).",
                        t$name, paste(sQuote(dup, FALSE), collapse = ", "), part))
    }
    args <- c(args, p)
  }
  args
}

# The part (`"ard"` or `"content"`) a template's parameter acts in, from its
# registry entry; NA when the template declares no parts or does not list it
# (a flat `args:` is read by name, so this is for writers)
.tpl_part_of <- function(name, param) {
  parts <- tryCatch(.tpl_get(name, "template:")$parts, error = function(e) NULL)
  for (part in c("ard", "content")) if (param %in% parts[[part]]) return(part)
  NA_character_
}

# Is a `template: {name: }` a figure design's preset (tfl_fig_templates():
# box_by_visit, swimmer_full ...) rather than a template SPEC?  An agent
# writes the preset instructions-figure.md lists in the template SPEC's
# form; a name the registry has is the template SPEC's -- unless both have
# it (forest_hr, mean_se, mean_ci) and it is written without the `args`
# the template SPEC needs: that can only be the preset
.fig_preset_named <- function(t) {
  if (!is.list(t) || !is.character(t$name) || length(t$name) != 1L) return(FALSE)
  if (!t$name %in% tfl_fig_templates()$template) return(FALSE)
  if (!exists(t$name, envir = .tpl_registry, inherits = FALSE)) return(TRUE)
  need <- Filter(function(p) isTRUE(p$required), get0(t$name, envir = .tpl_registry, inherits = FALSE)$params)
  length(need) > 0L && !any(names(need) %in% names(t$args))
}

# The study standard's value of a parameter that says where it is
# (tfl_param(study = <key>)): the one reader, for any key, with no table of
# keys.  `settings.<name>` is study.yml's `settings:`; `figure_style.<key>`
# (or `figure_style.<type>.<key>`) is defaults.yml's `figure_style:` over
# tflspec's built-in one; `output.<field>` is the report's own field;
# `km.<key>` is the company's figure style (until the Kaplan-Meier batch);
# any other first part is a verb: `<verb>.<argument>[.<name>]` is that
# argument of the verb's item in the reference's range (`range`: the report's
# layers' content merged, the style set's and the content step's files';
# none for a report whose content step lays nothing), else -- a template
# not yet migrated -- of the last item of the verb in spec/defaults.yml's
# `content:` (a part further down is a name in it).  The company's files are
# not read.  NULL when the study has none.
.tpl_study_read <- function(key, output = NULL, study = NULL, range = NULL) {
  .tpl_study_find(key, output, study, range)$value
}

# A reference may name several keys: they are read in order, the first the
# study has wins.  The key found (NA when none) and its value (NULL when none)
.tpl_study_find <- function(keys, output = NULL, study = NULL, range = NULL) {
  for (k in keys) {
    v <- .tpl_study_read1(k, output, study, range)
    if (!is.null(v)) return(list(key = k, value = v))
  }
  list(key = NA_character_, value = NULL)
}

.tpl_study_read1 <- function(key, output = NULL, study = NULL, range = NULL) {
  root <- sub("[.].*$", "", key)
  path <- strsplit(sub("^[^.]*[.]", "", key), ".", fixed = TRUE)[[1L]]
  down <- function(x, path) {
    for (k in path) {
      if (!is.list(x) || !k %in% names(x)) return(NULL)
      x <- x[[k]]
    }
    x
  }
  switch(root,
    output = {
      v <- down(output, path)
      # (none when the report names none: NA is tfl_output()'s "not given")
      if (length(v) == 1L && !is.na(v)) v
    },
    settings = down(study$study$settings, path),
    figure_style = .tpl_fig_style_read(path, study),
    km = .tpl_km_style_value(path[1L]),
    {
      # (the range is merged already: an `!unset` in it took the value away)
      items <- range %||% study$defaults$content
      if (inherits(items, "tfl_r") || !length(items)) return(NULL)
      hit <- Filter(function(it) identical(it$fn, root) && !is.null(down(it$args, path)), items)
      if (length(hit)) down(hit[[length(hit)]]$args, path) else NULL
    })
}

# A template whose data stage is the report's (its data-stage parameters
# hidden: km_survival, km_cumulative): the report says it in its own
# `data:` steps -- they read the data, and a dataset of parameters (all but
# ADSL) is kept to one (a param step, or a filter on PARAMCD).  Without
# them the figure would be of every record: no report, so an error.
.tpl_data_said <- function(t, output, id) {
  hid <- Filter(function(p) isTRUE(p$hidden) && identical(p$step, "data"), t$params)
  if (!length(hid) || is.null(output)) return(invisible(TRUE))
  steps <- output$data$steps %||% list()
  kind <- vapply(steps, function(st) if (is.list(st)) as.character(st$step %||% "") else "", "")
  how <- "data: {steps: [{step: read, dataset: ADTTE}, {step: param, value: <PARAMCD>}]}"
  if (!"read" %in% kind) {
    .ard_stop(sprintf(paste0("%s: template %s: the report says its data in its own `data:` steps ",
                             "-- read, the analysis set, the parameter (%s)."), id, t$name, how))
  }
  # (a read of the report's analysis data: its dataset the one it is made
  # from, its parameter kept when a condition on its way says PARAMCD)
  ad <- output$ard$analysis_data %||% list()
  ad_said <- character()
  ad_root <- function(id) {
    seen <- character()
    repeat {
      e <- Filter(function(a) identical(a$id, id), ad)
      if (!length(e) || id %in% seen) return("")
      seen <- c(seen, id)
      if (grepl("PARAMCD", paste(as.character(e[[1L]]$where %||% ""), collapse = " "), fixed = TRUE)) {
        ad_said <<- c(ad_said, id)
      }
      from <- as.character(e[[1L]]$from %||% "")
      if (!from %in% vapply(ad, function(a) as.character(a$id %||% ""), "")) return(from)
      id <- from
    }
  }
  reads <- toupper(vapply(steps[kind == "read"], function(st) {
    dd <- .fig_read_data(st)
    if (!is.null(dd)) ad_root(dd) else as.character(st$dataset %||% "")
  }, ""))
  on_param <- "param" %in% kind || length(ad_said) > 0L ||
    any(vapply(steps[kind == "filter"], function(st)
      grepl("PARAMCD", paste(as.character(st$expr %||% ""), collapse = " "), fixed = TRUE), NA))
  if (any(reads != "ADSL") && !on_param) {
    .ard_stop(sprintf(paste0("%s: template %s: its `data:` steps read %s but keep no parameter: ",
                             "add {step: param, value: <PARAMCD>} (the parameter is what the ",
                             "report is of)."), id, t$name, paste(unique(reads[reads != "ADSL"]),
                                                                   collapse = ", ")))
  }
  invisible(TRUE)
}

# The template SPEC's arguments with the study's defaults: a parameter the
# SPEC does not give takes the study's value (tfl_param(study = )); one it
# gives over a different study value says so (a message).  Returns the
# arguments, the names the study gave, their keys and each parameter's
# route (.tpl_param_route()).  `range`: the reference's range
# (.tpl_study_read()).
.tpl_study_args <- function(t, args, id, output = NULL, study = NULL, range = NULL) {
  # (a parameter the template hides is not the report's: its look)
  hidden <- intersect(names(args), names(Filter(function(p) isTRUE(p$hidden), t$params)))
  stage <- hidden[vapply(hidden, function(k) identical(t$params[[k]]$step, "data"), NA)]
  if (length(stage)) {
    .ard_stop(sprintf(paste0("%s: template %s's %s: the data stage, the report's own `data:` ",
                             "steps (read, the analysis set's join, param, filter), not ",
                             "`template: args:`."),
                      id, t$name, paste(sQuote(stage, FALSE), collapse = ", ")))
  }
  if ("population" %in% hidden) {
    .ard_stop(sprintf(paste0("%s: template %s takes the analysis set from the report's `population:` ",
                             "field, not from `template:`."), id, t$name))
  }
  if (length(hidden)) {
    .ard_stop(sprintf(paste0("%s: template %s takes %s from the study standard, not from ",
                             "`template: args:`; a figure setting (`plot:`: the title, axes, legend, theme, palette, ",
                             "base_size, and the Kaplan-Meier types' risk_title) is set there; ",
                             "the rest is the study's figure style."),
                      id, t$name, paste(sQuote(hidden, FALSE), collapse = ", ")))
  }
  .tpl_data_said(t, output, id)
  # a template with a `style` parameter: the report's `style:` field is its value
  # when the SPEC does not give one
  if ("style" %in% names(t$params) && !"style" %in% names(args) && length(output$style) == 1L &&
      !is.na(output$style)) {
    args$style <- output$style
  }
  # (what the report gives: its arguments, its `style:` field)
  given <- args
  from_study <- character()
  found <- list()
  for (p in t$params) {
    if (is.null(p$study)) next
    hit <- .tpl_study_find(p$study, output, study, range)
    v <- hit$value
    if (is.null(v)) next
    if (!p$name %in% names(args)) {
      args[[p$name]] <- v
      from_study <- c(from_study, p$name)
      found[[p$name]] <- hit$key
    } else if (!identical(args[[p$name]], v)) {
      message(sprintf("%s: template %s's %s overrides the study standard (%s).",
                      id, t$name, p$name, hit$key))
    }
  }
  keys <- stats::setNames(vapply(from_study, function(k) found[[k]], ""), from_study)
  route <- vapply(t$params, function(p) {
    .tpl_param_route(p, if (p$name %in% names(given)) given[p$name], keys[p$name])
  }, "")
  list(args = args, study = from_study, keys = keys, route = stats::setNames(route, names(t$params)))
}

# A template whose set holds its `extends` (tfl_template(extends = )): one
# layer order, the study's layers under the template's items (canon 1.27,
# .tpl_layers_below()).  A template not yet migrated (`extends` NULL) keeps
# the old order until its batch.  An alias is its template; a variant is
# not migrated.
.tpl_layered <- function(name) {
  t <- tryCatch(.tpl_get(name, ".tpl_layered()"), error = function(e) NULL)
  !is.null(t$extends)
}

# The set a `style` parameter names, for the parameter that takes it
# (a template's `style`, `tfl_param("style", study = "settings.style",
# default = "tfl_standard")`).  A name tflspec ships ("tfl_standard",
# "tfl_small", "stf_t02") is tflspec's set: a study's `styles:` may not take
# it (tfl_read_study_spec() stops).  Any other name is the study's own set
# (spec/defaults.yml `styles:`).  A set is a page configuration: its `page`
# (complete for the page step) and its `content`.  A name neither has stops,
# naming the sets there are.
.tpl_style_page_yaml <- function(margin = NULL) c(
  "page:",
  if (!is.null(margin)) sprintf("  rtf_page: {margin_left_in: %s, margin_right_in: %s}", margin, margin),
  "  rtf_tables: {auto_section: true}",
  "  header:",
  "    first:",
  "      - {left: \"{COMPANY}\", right: \"{STATUS}\"}",
  "      - {left: \"Protocol: {STUDY_ID}\", right: \"Page {PAGE} of {TOTAL_PAGES}\"}",
  "      - {center: \"{OUTPUT_LABEL}\"}",
  "      - {center: \"{OUTPUT_TITLE}\"}",
  "      - {center: \"<{OUTPUT_POPULATION}>\"}",
  "  footer:",
  "    last:",
  "      - {left: \"{PROGRAM}\", right: \"{DATETIME}\"}")
.tpl_style_builtin_yaml <- c(
  tfl_standard = paste(.tpl_style_page_yaml(), collapse = "
"),
  tfl_small = paste(c(
    .tpl_style_page_yaml(0.5),
    "  rtf_style: {font_size_half_points: 14}",
    "content:",
    "  - plan_style: {font_size_half_points: 14}",
    "  - plan_paginate_rows: {max_rows: 42}"), collapse = "
"),
  # FDA STF IG v2.0, section 2.1.2, Table 2's look (not an FDA format)
  stf_t02 = paste(
    "content:",
    "  - plan_style:",
    "      font: Arial",
    "      font_size_half_points: 16",
    "      header_bold: true",
    "      header_align: right",
    "      align: right",
    "      border_header: {fn: rtf_border, args: {top: true, bottom: true}}",
    "      border_last_row: {fn: rtf_border, args: {bottom: true}}",
    "      border_group: {fn: rtf_border, args: {bottom: true}}",
    "  - plan_cell_style: {cols: row_label, align: left}",
    "page:",
    "  rtf_page: {orientation: landscape}",
    sep = "\n"))
.tpl_style_builtin_names <- names(.tpl_style_builtin_yaml)
.tpl_style_builtin_set <- function(name) {
  y <- yaml::yaml.load(.tpl_style_builtin_yaml[[name]], handlers = .yaml_handlers())
  .style_set_from_list(if (length(y)) y else list(), name)
}
.tpl_style_set <- function(name, study = NULL) {
  if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name)) {
    .ard_stop("`style` is one style's name (a set of defaults.yml's styles:, or a built-in one).")
  }
  set <- study$styles[[name]]
  if (is.null(set) && name %in% .tpl_style_builtin_names) return(.tpl_style_builtin_set(name))
  if (is.null(set)) {
    .ard_stop(sprintf("no style %s in defaults.yml's styles: or among the built-in sets (%s).",
                      dQuote(name, FALSE),
                      paste(.style_names(study), collapse = ", ")))
  }
  set
}

# The template expanded for a report: its arguments with the study's
# defaults and the report's code lists (`codelists`: the layers' and its
# own).  `lb`: the layers below the report (.tpl_layers_below()).  A
# template with an `extends` in its set reads its references in the
# content step's range (none under `{content: none}`) and folds the study's
# stub over that range; one not yet migrated over the layers' content.
.tpl_expand_for <- function(output, study, lb, codelists, warn = TRUE) {
  ts <- .tpl_spec_of(output)
  t <- .tpl_get(ts$name, sprintf("%s: template:", output$id))
  if (warn) .tpl_plot_unused_warn(t, output)
  layered <- !is.null(t$extends)
  base <- if (layered) lb$range %||% list() else lb$acc$content
  sa <- .tpl_study_args(t, ts$args, output$id, output, study, range = if (layered) lb$range)
  sa$args <- .tpl_plot_axis_args(t, sa$args, output$plot)
  x <- tfl_template_expand(ts$name, sa$args, codelists = codelists,
                           population = study$data$populations)
  .tpl_den_known(t, sa$args, x, output)
  if (!identical(t$kind, output$kind)) {
    .ard_stop(sprintf("%s: template %s makes a %s; the report is a %s.",
                      output$id, ts$name, t$kind, output$kind))
  }
  # a table's ARD normalized first, as tpl_*() does: the template's own
  # step when neither it nor the study's layers have one
  nz <- function(items) any(vapply(if (inherits(items, "tfl_r")) list() else items,
                                   function(it) identical(it$fn, "normalize_ard"), NA))
  if (identical(t$kind, "table") && !nz(x$content) && !nz(base)) {
    x$content <- c(list(tfl_item("normalize_ard", list(), from = paste0("template:", ts$name))),
                   x$content)
  }
  x$content <- .tpl_stub_first(x$content, base)
  list(name = ts$name, args = sa$args, study = sa$study, study_keys = sa$keys, x = x,
       layered = layered, route = sa$route, t = t,
       strip = if (layered) .tpl_step_off(t, sa$args) else character())
}

# A `denominator` that is an analysis data's id: the template makes it, or
# the report's own `ard:` does (step 1's analysis data)
.tpl_den_known <- function(t, args, x, output) {
  v <- args$denominator
  # (an `!unset` or a NULL: the analysis set's, nothing to check)
  if (!"denominator" %in% names(t$params) || inherits(v, "tfl_unset") || !is.character(v) || length(v) != 1L ||
      is.na(v)) return(invisible())
  v <- trimws(v)
  if (identical(v, "data") || !is.null(.den_numbers(v))) return(invisible())
  ids <- function(ad) vapply(ad %||% list(), function(e) as.character(e$id %||% ""), "")
  have <- unique(c(ids(x$ard$analysis_data), ids(output$ard$analysis_data)))
  if (!v %in% have) {
    .ard_stop(sprintf(paste0(
      "%s: template %s: denominator \"%s\" is not an analysis data of the report (%s).",
      " The template makes %s and the other analysis sets' adsl_<id>; the report's own come from its `ard:`."),
      output$id, t$name, v, paste(have, collapse = ", "), paste0("adsl_", tolower(args$population))))
  }
  invisible()
}

# A stub the study folds first (its plan_stub(before = TRUE)) takes the
# table's row keys away before anything reads them: a row group's
# `collapse` and a header cell's `cols` that name a key name the stub's
# column from then on (`row_label`).  The template, which does not know the
# study's stub, names the keys.
.tpl_stub_first <- function(items, base) {
  if (inherits(items, "tfl_r") || inherits(base, "tfl_r") || !length(items)) return(items)
  is_fn <- function(fn) function(it) identical(as.character(it$fn %||% ""), fn)
  study <- Filter(is_fn("plan_stub"), base)
  if (!length(study) || !isTRUE(study[[length(study)]]$args$before)) return(items)
  own <- Filter(is_fn("plan_stub"), items)
  own_args <- if (length(own)) own[[length(own)]]$args else list()
  study_args <- study[[length(study)]]$args
  stub <- c(own_args, study_args[setdiff(names(study_args), names(own_args))])
  name <- stub$name
  tp <- Filter(is_fn("table_plan"), items)
  if (!length(tp) || !is.character(name) || length(name) != 1L) return(items)
  rows <- tp[[1L]]$args$rows
  # (a key is named by its name, else by its column)
  keys <- as.character(unlist(rows))
  if (length(names(rows))) keys <- ifelse(nzchar(names(rows)), names(rows), keys)
  folded <- as.character(unlist(stub$vars))
  if (!length(folded)) folded <- keys
  swap <- function(x) {
    v <- as.character(unlist(x))
    if (!any(v %in% folded)) return(x)
    v <- unique(replace(v, v %in% folded, name))
    if (is.list(x)) as.list(v) else v
  }
  lapply(items, function(it) {
    fn <- as.character(it$fn %||% "")
    if (identical(fn, "plan_row_group") && length(it$args$collapse)) {
      it$args$collapse <- swap(it$args$collapse)
    } else if (identical(fn, "plan_col_header") && is.list(it$args$header)) {
      it$args$header <- lapply(it$args$header, function(cell) {
        if (is.list(cell) && length(cell$cols)) cell$cols <- swap(cell$cols)
        cell
      })
    }
    it
  })
}

# The keys of the report's `plot:` that do nothing in a Kaplan-Meier type:
# its look (hidden, the study's figure style) that the figure's settings
# and the type's own reading of `plot:` do not take (.tpl_km_look_what)
.tpl_plot_unused <- function(t, plot) {
  if (!identical(t$layout, "km") || !length(plot)) return(character())
  look <- intersect(names(Filter(function(p) isTRUE(p$hidden), t$params)), .tpl_km_look)
  intersect(names(plot), setdiff(look, c(.fig_plot_fields()$field, .tpl_km_plot_routed)))
}

# What it says: the key, the template, and where the setting comes from
# instead when the study's figure style has it
.tpl_plot_unused_text <- function(t, key) {
  style <- sub("^km[.]", "", t$params[[key]]$study[1L] %||% "")
  what <- .tpl_km_look_what[key]
  if (is.na(what)) what <- sprintf("its `%s`", key)
  sprintf("plot: %s has no effect in template %s: %s %s.", key, t$name, what,
          if (nzchar(style)) sprintf("comes from the study's figure style (%s)", style)
          else "is the template's own")
}

# Said once a key, as a warning of class `tfl_plot_key_unused` (the draft
# check and tfl_check_spec() take it as a problem of the report).  The
# build goes on.
.tpl_plot_unused_warn <- function(t, output) {
  for (k in .tpl_plot_unused(t, output$plot)) {
    text <- .tpl_plot_unused_text(t, k)
    warning(structure(class = c("tfl_plot_key_unused", "warning", "condition"),
                      list(message = paste0(output$id, ": ", text), call = NULL, id = output$id,
                           key = k, template = t$name, text = text)))
  }
  invisible()
}

# A figure template that makes its x axis itself (its breaks and the times
# at risk: km_survival's x_step, x_max) takes the report's `plot.x_by` /
# `plot.x_max` for them when its arguments do not say them: the template's
# axis would otherwise win over the report's settings
.tpl_plot_axis_args <- function(t, args, plot) {
  map <- c(x_step = "x_by", x_max = "x_max")
  for (a in intersect(names(map), names(t$params))) {
    v <- plot[[map[[a]]]]
    if (is.null(args[[a]]) && !is.null(v) && length(v) == 1L && !is.na(v) && !identical(v, "")) {
      args[[a]] <- as.numeric(v)
    }
  }
  # the risk table's heading (a report type hides risk_title as the study's
  # figure style): the report's `plot.risk_title` goes to the template as its
  # own `plot` setting, which the template reads
  v <- plot$risk_title
  if ("risk_title" %in% names(t$params) && is.null(args$plot$risk_title) &&
        is.character(v) && length(v) == 1L && !is.na(v) && nzchar(v)) {
    args$plot <- c(args$plot, list(risk_title = v))
  }
  args
}

# A template's code lists (`codelists` of its result) added to the report's:
# the report's list keeps its values, the template's gives the ones it lacks
.tpl_add_codelists <- function(cl, add) {
  for (v in names(add)) {
    have <- cl[[v]]
    if (is.null(have)) {
      cl[[v]] <- add[[v]]
      next
    }
    rows <- .tpl_cl_rows(have)
    vals <- vapply(rows, function(r) as.character(r$value), "")
    new <- Filter(function(r) !as.character(r$value) %in% vals, add[[v]])
    if (length(new)) cl[[v]] <- c(rows, new)
  }
  cl
}

# A report's ard: over the template's: an entry of the report's replaces the
# template's of the same id, the others go after
.merge_ard <- function(tpl, own) {
  if (!length(tpl)) return(own)
  if (!length(own)) return(tpl)
  out <- tpl
  for (part in union(names(tpl), names(own))) {
    a <- tpl[[part]] %||% list()
    b <- own[[part]] %||% list()
    ids <- vapply(a, function(e) e$id %||% NA_character_, "")
    for (e in b) {
      k <- match(e$id %||% NA_character_, ids)
      if (!is.na(k)) a[[k]] <- e else a <- c(a, list(e))
    }
    out[[part]] <- a
  }
  out
}

#' A template report written out as its items
#'
#' The B use of a report's template SPEC (`template: {name, args}` in its
#' YAML file): the template's items -- with the study's defaults for the
#' parameters that take them, as [tfl_resolve_output()] gives them --
#' merged with the report's own `ard:`, `data:` and `content:`, and the
#' `template:` taken out.  Each item the template made keeps the mark
#' `from = "template:<name>"`.  The report is then a code SPEC: later
#' changes of the template do not reach it.
#'
#' @param output A `tfl_output` as read from its file ([tfl_read_output()]).
#' @param study Its study ([tfl_read_study_spec()]).
#' @return The `tfl_output`, unresolved (its `extends` kept): write it
#'   with [tfl_write_output()].  A report with no template SPEC comes back
#'   as it is.
#' @examples
#' d <- system.file("extdata", "spec-yaml", package = "tflspec")
#' s <- tfl_read_study_spec(d)
#' o <- tfl_output("T-9", "table", population = "SAF",
#'                 template = list(name = "demog", args = list(by = "TRT01A", continuous = "AGE")))
#' b <- tfl_template_to_spec(o, s)
#' vapply(b$content, function(it) it$fn, "")
#' @export
tfl_template_to_spec <- function(output, study) {
  if (!inherits(output, "tfl_output")) .ard_stop("tfl_template_to_spec(): `output` is a tfl_output.")
  if (!inherits(study, "tfl_study_spec")) .ard_stop("tfl_template_to_spec(): `study` is a tfl_study_spec.")
  if (is.null(.tpl_spec_of(output))) return(output)
  lb <- .tpl_layers_below(output, study)
  # (the resolve below says what `plot:` does not reach, once)
  e <- .tpl_expand_for(output, study, lb, .merge_map(lb$acc$codelists, output$codelists), warn = FALSE)
  # the content: what the resolved report has from the template and its own
  # file, argument by argument -- the study's layers stay the layers', so
  # the report resolves the same
  ro <- tfl_resolve_output(output, study, provenance = TRUE)
  tp <- attr(ro, "template")
  # the code SPEC says what the template report extends and its style, always
  # (canon: B = A, explicit `extends` and `style:`)
  output$extends <- .extends_compact(lb$extends)
  output$ard <- .merge_ard(e$x$ard, output$ard)
  own <- if (.tpl_composes(output$data, e$x)) .tpl_compose_data(output$data, e$x) else output$data
  output$data <- .merge_map(e$x$data %||% list(), own %||% list())
  output$content <- if (inherits(ro$content, "tfl_r")) ro$content else .tpl_own_part(ro$content)
  if (isTRUE(e$layered) && !inherits(output$content, "tfl_r")) {
    # what a parameter's stop value took out of the study's layers (its
    # `maps_to`: blanks = FALSE, no `where`, bug 9) is `!unset` in the code
    # SPEC, whose layers would give it back
    output$content <- .tpl_unset_removed(output$content, tp$removed)
    # (`widths = "auto"` with no code list for the arms: the template report
    # takes them from its ARD as it runs; the code SPEC gets the widths
    # written, from the report's ARD built here -- none when it cannot be)
    # (the build reads the study's reports, this one through tfl_template_to_spec()
    # again: that inner call writes no widths)
    w <- e$args$widths %||% e$t$params$widths$default
    ard <- NULL
    if (identical(output$kind, "table") && identical(w, "auto") && !isTRUE(.tpl_to_spec_busy$on)) {
      .tpl_to_spec_busy$on <- TRUE
      ard <- tryCatch(suppressMessages(
        tfl_build_ard(study, dir = study$dir %||% ".", output_id = output$id, save = FALSE)),
        error = function(err) NULL, finally = .tpl_to_spec_busy$on <- FALSE)
    }
    if (is.data.frame(ard)) {
      ro$content <- output$content
      output$content <- .tpl_auto_widths(ro, ard)
    }
    if (!is.null(lb$style_name)) output$style <- lb$style_name
  }
  # a figure's design: its settings, the template's under the report's
  if (!is.null(ro$plot)) output$plot <- ro$plot
  if (!is.null(e$x$codelists)) output$codelists <- .tpl_add_codelists(output$codelists, e$x$codelists)
  output$template <- NULL
  output
}

# (tfl_template_to_spec() building a report's ARD for its widths)
.tpl_to_spec_busy <- new.env(parent = emptyenv())

# A figure template (its call makes the data's last `call_steps` steps: the
# fit ...) and a report's own data steps (its data stage: the analysis set,
# the parameter, the records kept): the report's steps, then the
# template's call steps -- tfl_resolve_output() and tfl_template_to_spec()
# alike.  Without own steps, or for a table's template, the template's.
.tpl_composes <- function(data, x) {
  as.integer(x$call_steps %||% 0L) > 0L && length(data$steps) > 0L
}
.tpl_compose_data <- function(data, x, id = "", name = "") {
  n <- as.integer(x$call_steps)
  call <- utils::tail(x$data$steps %||% list(), n)
  # a step of the call's kind written by the report (a survfit before the
  # template's own): the template makes it, and would make it again on
  # the report's -- a clear stop here, not a failure of the build
  # (a step of the data stage -- a filter, a derive, R code -- is the
  # report's to write even when the call has one of the kind too)
  kind <- function(s) vapply(s, function(e) as.character(e$step %||% ""), "")
  stage <- c("", "read", "join", "param", "filter", "derive", "code", "flag", "select",
             "mutate", "arrange")
  dup <- setdiff(intersect(kind(data$steps), kind(call)), stage)
  if (length(dup)) {
    .ard_stop(sprintf(paste0(
      "%s uses template %s, which makes its own %s step%s (%s); `data.steps` writes %s too. ",
      "Keep the data stage (read, the analysis set, param, filter, derive) and remove %s; ",
      "keep plot and layers."),
      id, name, paste(dup, collapse = " and "), if (length(dup) > 1L) "s" else "",
      paste(unique(kind(call)), collapse = ", "), paste(dup, collapse = ", "),
      paste(dup, collapse = ", ")))
  }
  data$steps <- c(data$steps, call)
  data
}

# The resolved items' part from the template and the report's own file:
# a named-dots verb's keys from them, any other verb's item when it is
# theirs (its provenance, `arg_from`, dropped)
.tpl_own_part <- function(items) {
  mine <- function(f) startsWith(f, "template:") || identical(f, "own")
  out <- list()
  for (it in items) {
    keys <- names(it$args) %||% character()
    af <- it$arg_from %||% stats::setNames(rep(it$from %||% "own", length(keys)), keys)
    k <- if (.arg_keyed(it$fn, items) && length(keys)) keys[vapply(af[keys], mine, NA)] else NULL
    keep <- if (is.null(k)) mine(it$from %||% "own") else length(k) > 0L
    if (!keep) next
    if (!is.null(k)) it$args <- it$args[k]
    it$arg_from <- NULL
    it$below <- NULL
    out <- c(out, list(it))
  }
  out
}

# The A use's lines after the template's call: what the resolved items have
# that the template did not make -- a verb's whole item, or for a verb whose
# `...` is named its keys, by where they come from (`arg_from`).  `ok` is
# FALSE when the call cannot say it: the table's roles (table_plan()) or
# the normalization's arguments come from elsewhere.  `layered` (a template
# whose set has its `extends`): the study's parts go in the call's `study =`,
# merged under the template's items, so they do not say `before` again.
.tpl_rest <- function(items, name, tpl_items = list(), layered = FALSE) {
  tag <- paste0("template:", name)
  # an item the template made the same (its parameters took the study's
  # values: stub_name, blanks ...) is in the call, not after it
  same_as_tpl <- function(it) any(vapply(tpl_items, function(t) {
    identical(t$fn, it$fn) && identical(t$args, it$args)
  }, NA))
  rest <- list()
  ok <- TRUE
  for (it in items) {
    af <- it$arg_from %||% character()
    keys <- names(it$args) %||% character()
    not_tpl <- if (length(keys)) keys[af[keys] != tag] else character()
    if (it$fn %in% c("normalize_ard", "table_plan")) {
      if (length(not_tpl) || (identical(it$fn, "table_plan") && !identical(it$from, tag))) ok <- FALSE
      next
    }
    if (.arg_keyed(it$fn, items) && length(keys)) {
      for (s in unique(af[not_tpl])) {
        take <- af == s
        # (rtfreporter's plan_stub() always sets `before`, FALSE by default: a
        # second call would put it back, so the call says it again)
        if (identical(it$fn, "plan_stub") && (!layered || identical(s, "own"))) {
          take <- take | keys == "before"
        }
        rest <- c(rest, list(tfl_item(it$fn, it$args[keys[take]], from = s)))
      }
    } else if (!identical(it$from, tag) && !same_as_tpl(it)) {
      rest <- c(rest, list(it))
    }
  }
  list(items = rest, ok = ok)
}

# A package and its version, as a program records where a template is from
.tpl_pkg_version <- function(pkg) {
  v <- tryCatch(as.character(utils::packageVersion(pkg)), error = function(e) NULL)
  paste(c(pkg, v), collapse = " ")
}

# The template's call as a program writes it: `tflspec::tfl_tpl` (a table's
# or a listing's), `tflspec::tpl_<name>` (a figure's), a package's own
# template with its `pkg::`; one made in the session bare.  A study's
# program does not library(tflspec), so the package is named;
# .drop_attached_ns() takes the `pkg::` off for a package the study's setup
# attaches (option tflspec.attached).
.tpl_call_name <- function(t, generic, called = NULL) {
  fn <- if (generic) "tfl_tpl" else t$call %||% paste0("tpl_", called %||% t$name)
  if (grepl("::", fn, fixed = TRUE)) return(fn)
  pkg <- if (generic) "tflspec" else t$package %||% "tflspec"
  if (!nzchar(pkg) || identical(pkg, "(session)")) return(fn)
  # a package's name only when it exports the function: a template a study
  # or a test registers with its own `call` is not tflspec's (the program
  # finds it where the study's setup put it)
  has <- tryCatch(fn %in% getNamespaceExports(pkg), error = function(e) FALSE)
  if (has) paste0(pkg, "::", fn) else fn
}

# An argument's !r values, in a list too, written as the R they are
# (`n == 0 ~ "0"`), not as the text they are held in
.tpl_r_code <- function(x) {
  if (inherits(x, "tfl_r")) return(.spec_expr(as.character(unclass(x))))
  # an `!unset` (over a default of the template) as the call that says it
  if (inherits(x, "tfl_unset")) return(.spec_call("tflspec::tfl_unset"))
  # a call (a rule: rtf_border(top = TRUE)) as the call it is
  if (inherits(x, "tfl_call")) return(.schema_value_to_r(x, NULL, FALSE))
  if (is.list(x) && !is.object(x)) x[] <- lapply(x, .tpl_r_code)
  x
}

# The template's call as the program writes it, with the study's items
# (`study`, the resolved items not the template's nor the report's) as its
# last argument: `study = list(<verb> = list(<args>), ...)`, each part after
# a note of the layer it is from, its values as the SPEC holds them
.tpl_call_study_code <- function(call, study, lead) {
  if (!length(study)) return(.spec_call_code(call, 0L, lead))
  # (the call over lines, an argument a line, as .spec_code() writes a long one)
  p <- .spec_parts(call)
  el <- vapply(seq_along(p$items), function(i) {
    paste0("  ", if (nzchar(p$names[i])) paste0(.spec_name(p$names[i]), " = "),
           .spec_code(p$items[[i]], 2L))
  }, "")
  code <- paste0(p$open, "(\n", paste(el, collapse = ",\n"))
  ent <- vapply(study, function(it) {
    a <- .tpl_study_value(it$args %||% list())
    one <- .spec_code_line(a)
    v <- if (nchar(one) + nchar(it$fn) + 8L <= 80L) one else .spec_code(a, 4L)
    paste0("    ", .spec_name(it$fn), " = ", v)
  }, "")
  note <- vapply(study, function(it) paste0("    ", .tpl_from_note(it$from)), "")
  paste0(code, ",\n",
         "  # (the study's items, under the template's: the template's values win)\n",
         "  study = list(\n",
         paste(paste0(note, "\n", ent), collapse = ",\n"), "\n  )\n)")
}

# A value of the call's `study =` as the program writes it: as the SPEC
# holds it (the call merges it with the template's items), an `!r` as
# tfl_r("<code>"), an `!unset` as tfl_unset(), a call (a rule) as the call
.tpl_study_value <- function(x) {
  if (inherits(x, "tfl_r")) return(.spec_call("tflspec::tfl_r", as.character(unclass(x))))
  if (inherits(x, "tfl_unset")) return(.spec_call("tflspec::tfl_unset"))
  if (inherits(x, "tfl_call")) return(.schema_value_to_r(x, NULL, FALSE))
  if (is.list(x) && !is.object(x)) x[] <- lapply(x, .tpl_study_value)
  x
}

# where an item of a template report's program is from, as its note says
.tpl_from_note <- function(f) {
  if (identical(f, "own")) return("# (own)")
  if (identical(f, "defaults")) return("# (study default)")
  if (startsWith(f, "style:")) return(paste0("# (study style: ", sub("^style:", "", f), ")"))
  paste0("# (pattern ", sub("^pattern:", "", f), ")")
}

# A template report's program (the A use): the template's call, the
# arguments its SPEC gives and the study's defaults it took (not the ones
# left at the template's own default), the code lists it reads at the head;
# then the study's and the report's own lines, each marked
.content_tpl_code <- function(output, data = "ard", plan = "plan", pipe = NULL) {
  tp <- attr(output, "template")
  op <- .ard_pipe_op(pipe)
  t <- .tpl_get(tp$name, "tfl_content_code()")
  args <- tp$args[names(tp$args) %in% names(t$params)]
  # the template called: a table's or a listing's by tfl_tpl(<data>, "<name>",
  # ...); a variant of tflspec's by its name, a company's or a study's by
  # the template it is made from, its defaults written out (the program
  # does not library() the company's package)
  called <- tp$name
  if (!is.null(t$from) && !identical(t$package, "tflspec")) {
    args <- c(t$defaults[setdiff(names(t$defaults), names(args))], args)
    args <- args[intersect(names(t$params), names(args))]
    # (a variant of demog calls demog: its denominator and Missing row are demog's, not summary's)
    called <- if (.tpl_is_demog(t$from)) "demog" else t$root %||% t$from
  }
  generic <- identical(t$call, "tfl_tpl")
  # an !r value (a guard in `count_cell`'s list) as the R it is
  args <- .tpl_r_code(args)
  # the code lists the template reads: those its plan_levels() item names
  lv <- Filter(function(it) identical(it$fn, "plan_levels"), tp$items)
  vars <- setdiff(unlist(lapply(lv, function(it) names(it$args))), c("variable", ".drop_empty"))
  vars <- intersect(vars, names(output$codelists))
  # (each code in its order, named by its label when the list has labels:
  # the template's plan_labels() applies them)
  levels <- stats::setNames(lapply(vars, function(v)
    .tpl_cl_dict(output$codelists[[v]]) %||% .tpl_levels(v, output$codelists)[[v]]), vars)
  cl <- if (length(levels)) c("# the code lists: each code in its order (its label, when it has one)",
                              .codelists_lines(levels), "")
  call <- do.call(.spec_call, c(if (generic) list(.tpl_call_name(t, TRUE), .spec_sym(data), called)
                                else list(.tpl_call_name(t, FALSE, called), .spec_sym(data)), args,
                                if (length(levels)) list(codelists = stats::setNames(
                                  lapply(names(levels), function(v) .spec_sym(.cl_name(v))),
                                  names(levels)))))
  rest <- .levels_prune(tp$rest, output)
  # a template whose set has its `extends` (canon 1.28): the study's items
  # (its defaults, patterns and style set) in the call's `study =`, under the
  # template's; the report's own after the call
  study <- if (isTRUE(tp$layered) && generic) {
    Filter(function(it) !identical(it$from %||% "own", "own"), rest)
  }
  if (length(study)) rest <- Filter(function(it) identical(it$from %||% "own", "own"), rest)
  head <- c(sprintf("# template %s%s%s", tp$name,
                    if (!is.null(t$from)) sprintf(": a variant of %s (%s)", t$from,
                                                   .tpl_pkg_version(t$package)) else "",
                    if (length(tp$study)) paste0("; study defaults: ", paste(tp$study, collapse = ", "))
                    else ""),
            paste(plan, "<-", .tpl_call_study_code(call, study, nchar(plan) + 4L)))
  if (!length(rest)) return(.drop_attached_ns(c(cl, head)))
  steps <- .content_calls(rest)
  from <- vapply(rest, function(it) it$from %||% "own", "")
  lines <- vapply(steps, function(s) paste0("  ", .spec_call_code(.content_print(s))), "")
  note <- vapply(from, function(f) paste0("  ", .tpl_from_note(f)), "")
  body <- c(head[2L], unlist(lapply(seq_along(lines), function(i) c(note[i], lines[i]))))
  # the pipe: after every call but the last (a comment line takes none)
  is_call <- !startsWith(trimws(body), "#")
  last <- max(which(is_call))
  body[is_call & seq_along(body) < last] <- paste(body[is_call & seq_along(body) < last], op)
  .drop_attached_ns(c(cl, head[1L], unlist(strsplit(body, "\n", fixed = TRUE))))
}
