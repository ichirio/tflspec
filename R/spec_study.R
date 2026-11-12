# The study's files of the YAML spec, and the layering of a report over its
# defaults (design: _archive/spec-format-redesign-2026-10-14.md, 3.2-3.4).
#
#   study.yml                 the study: id, title ..., keys, setup
#   spec/data.yml             datasets, populations (the data catalog)
#   spec/defaults.yml         what every report starts from (a report's shape),
#                             and the study's styles: the look a report picks
#                             (`styles: {compact: {content, page}}`, `style:`)
#   spec/patterns/<name>.yml  a layer a report may extend (a report's shape)
#   spec/toc.yml              the reports in order
#   spec/outputs/**/*.yml     one report a file (or several, `outputs:`)
#
# A report file says only what differs from the layers it extends.  The
# layering is here: maps merge by key (an `!unset` removes the key), the
# content's items of one function replace the defaults' items of that
# function, and the rest of the defaults stay in their order.

#' Read a study's YAML spec
#'
#' `tfl_read_study_spec()` reads the study-level files (every one optional)
#' and `tfl_read_outputs()` the report files under `spec/outputs/`
#' (subfolders too; a file with `outputs:` holds several).  An id in two
#' files stops the read.  A `spec/codelists.yml` is not read (a code list is
#' a report's or a layer's), with a message.
#'
#' @param dir The study folder (holds `study.yml` and `spec/`).
#' @param skip_bad `TRUE` to leave out a report file that cannot be read, with a
#'   warning naming it (`tfl_read_outputs()`); `FALSE` (default) to stop there.
#' @return `tfl_read_study_spec()`: a list of class `tfl_study_spec` --
#'   `study`, `data`, `defaults`, `styles` (by name: `defaults.yml`'s
#'   `styles:`, the look a report picks with `style:`), `figure_style`
#'   (`defaults.yml`'s `figure_style:`: the study's figure style standard,
#'   tflspec's built-in one for a part it does not give),
#'   `patterns` (by name), `toc`, `dir`.  `tfl_read_outputs()`: a named list of `tfl_output`, by id.
#' @examples
#' d <- system.file("extdata", "spec-yaml", package = "tflspec")
#' s <- tfl_read_study_spec(d)
#' names(s$defaults$content)
#' @export
tfl_read_study_spec <- function(dir) {
  if (!dir.exists(dir)) stop("tfl_read_study_spec(): no folder ", dir, call. = FALSE)
  spec <- .spec_dir(dir)
  rd <- function(f) {
    if (!file.exists(f)) return(NULL)
    y <- yaml::read_yaml(f, handlers = .yaml_handlers())
    if (is.null(y)) list() else y
  }
  pat_files <- list.files(file.path(spec, "patterns"), "\\.ya?ml$", full.names = TRUE)
  patterns <- stats::setNames(
    lapply(pat_files, function(f) .layer_from_list(
      rd(f), paste0("pattern:", tools::file_path_sans_ext(basename(f))))),
    tools::file_path_sans_ext(basename(pat_files)))
  study <- rd(file.path(dir, "study.yml"))
  .check_spec_version(study$spec_version, file.path(dir, "study.yml"))
  dflt <- rd(file.path(spec, "defaults.yml"))
  styles <- dflt$styles %||% list()
  if (file.exists(file.path(spec, "codelists.yml"))) {
    message("spec/codelists.yml is not read: a code list is a report's or a layer's ",
            "(`codelists:` in spec/defaults.yml or the report's file).")
  }
  # a study defines reports, not templates (a report's own args and cards
  # say what differs); the company's templates are copied into the study's
  # standard at its start
  if (dir.exists(file.path(spec, "templates"))) {
    stop("tfl_read_study_spec(): spec/templates/ is not read: a study defines reports, not ",
         "templates. A report that differs from a template changes its arguments or adds ",
         "cards after the template's; remove the folder.", call. = FALSE)
  }
  structure(list(
    study = study,
    data = .calls_in(rd(file.path(spec, "data.yml")) %||% list()),
    defaults = .layer_from_list(dflt, "defaults"),
    styles = stats::setNames(lapply(names(styles), function(nm)
      .layer_from_list(styles[[nm]], paste0("style:", nm))), names(styles)),
    patterns = patterns,
    # the figure style standard the study keeps (defaults.yml's figure_style:)
    figure_style = dflt$figure_style %||% list(),
    toc = rd(file.path(spec, "toc.yml")) %||% list(),
    # rtfreporter's options for every program of the study (defaults.yml's
    # options:, by rtfreporter's names: rtfreporter.png_device ...)
    options = dflt$options %||% list(),
    dir = normalizePath(dir, "/", mustWork = FALSE)),
    class = "tfl_study_spec")
}

# spec/ of a study folder, or the folder itself when it is a spec folder
# (the fixtures: defaults.yml beside outputs/)
.spec_dir <- function(dir) {
  if (dir.exists(file.path(dir, "spec"))) file.path(dir, "spec") else dir
}

# a defaults or pattern file as a layer: content items marked `from`, the
# rest (data, codelists, page) as plain lists with calls resolved
.layer_from_list <- function(y, from) {
  if (is.null(y)) y <- list()
  items <- if (inherits(y$content, "tfl_r")) y$content else {
    its <- lapply(y$content %||% list(), .item_from_list)
    lapply(its, function(it) { it$from <- from; it })
  }
  list(data = .calls_in(y$data %||% list()), codelists = y$codelists %||% list(),
       content = items, page = .calls_in(y$page %||% list()))
}

#' @rdname tfl_read_study_spec
#' @export
tfl_read_outputs <- function(dir, skip_bad = FALSE) {
  out_dir <- .outputs_dir(dir)
  files <- list.files(out_dir, "\\.ya?ml$", recursive = TRUE, full.names = TRUE)
  outs <- list()
  where <- character()
  for (f in files) {
    # (`skip_bad`: a file that cannot be read is left out with a warning,
    # so that one report's broken file stops no other -- tfl_check_spec()
    # and the AI's check say which file it is)
    r <- if (isTRUE(skip_bad)) {
      tryCatch(tfl_read_output(f), error = function(e) {
        warning(sprintf("tfl_read_outputs(): %s is left out, it cannot be read: %s",
                        basename(f), conditionMessage(e)), call. = FALSE)
        NULL
      })
    } else tfl_read_output(f)
    if (is.null(r)) next
    if (inherits(r, "tfl_output")) r <- list(r)
    for (o in r) {
      if (o$id %in% names(outs)) {
        stop("tfl_read_outputs(): the report ", o$id, " is in two files: ",
             where[[o$id]], " and ", f, ".", call. = FALSE)
      }
      outs[[o$id]] <- o
      where[[o$id]] <- f
    }
  }
  outs
}

# -- layering ---------------------------------------------------------------

#' A report over the layers it extends
#'
#' `tfl_resolve_output()` puts a report's own file over the layers its
#' `extends` names (`defaults`, `patterns/<name>`), nearest last.  Maps
#' (`page`'s lines by number, `tokens`, `codelists`, a call's arguments)
#' merge by key, and an [tfl_unset()] removes the key.  In `content`, the
#' report's items of one function replace every item of that function in
#' the layers below, in the place the first of them had; a function the
#' report does not have keeps the layers' items; a function only the
#' report has goes after them.  The `content` of `defaults` is a table's
#' (normalize_ard, the digits ...): a figure or a listing takes its other
#' parts (`page`, `codelists`), not its content.  A pattern's content
#' (`patterns/<name>`, chosen report by report) layers onto any kind.
#' `extends: none` uses no layer.  The report's style (`style: compact`, a
#' set of `defaults.yml`'s `styles:`; `standard` when it names none) is the
#' nearest layer below its own: the look it picks, content and page.  A
#' template SPEC (`template: {name, args}`) puts the template's items
#' ([tfl_template_expand()], marked `template:<name>`) between the layers
#' and the report's own -- for a template whose set holds its `extends`
#' ([tfl_template()]), all of them over the style set its `style` names
#' and the files each step of `extends` names ([tfl_output_extends()]),
#' with what a parameter's stop value turns off ([tfl_param()]`(maps_to =
#' )`) taken out of those; for a template not yet migrated, its look
#' (plan_style, the widths, the rows a page ...) under the study's layers,
#' which say the look and win -- and
#' its `ard:` under the report's (an entry of the report's replaces the
#' template's of the same id); a template parameter that says where its
#' study default is takes it from the layers when the SPEC does not give
#' it, and a SPEC value over a different one says so.  Each item's
#' `from` says where it came from.  The result holds no `tfl_unset`.
#'
#' @param output A `tfl_output` as read from its file.
#' @param study A `tfl_study_spec` (its `defaults` and `patterns`).
#' @param provenance `TRUE` to say where each content item's arguments come
#'   from: each item gets `arg_from` (a named character vector, one an
#'   argument: `"own"`, `"defaults"` or `"pattern:<name>"`; a verb whose
#'   `...` is named, such as `plan_cells()`, by top-level key, any other verb
#'   one layer's whole) and `below` (the same verb's item with the report's
#'   own layer left out, `NULL` when no layer has it: what a revert goes
#'   back to).  `FALSE` (default): the items as they are.
#' @return The `tfl_output`, resolved (attribute `resolved` = `TRUE`).
#' @examples
#' d <- system.file("extdata", "spec-yaml", package = "tflspec")
#' s <- tfl_read_study_spec(d)
#' out <- tfl_resolve_output(tfl_read_outputs(d)[["T-14-1-1"]], s)
#' vapply(out$content, function(it) paste(it$fn, it$from), "")
#' @export
tfl_resolve_output <- function(output, study, provenance = FALSE) {
  if (!inherits(output, "tfl_output")) stop("tfl_resolve_output(): `output` is a tfl_output.", call. = FALSE)
  if (!inherits(study, "tfl_study_spec")) stop("tfl_resolve_output(): `study` is a tfl_study_spec.", call. = FALSE)
  # (a template with an `extends` in its set: the style its `style`
  # parameter names, resolved first)
  lb <- .tpl_layers_below(output, study)
  layers <- lb$layers
  acc <- lb$acc
  # a template SPEC: the template's items between the layers and the
  # report's own (its ard: under the report's ard:)
  tpl <- NULL
  if (!is.null(.tpl_spec_of(output))) {
    tpl <- .tpl_expand_for(output, study, lb, .merge_map(acc$codelists, output$codelists))
    acc <- list(data = list(), codelists = list(), content = list(), page = list())
    if (isTRUE(tpl$layered)) {
      # a template whose set has its `extends` (canon 1.27, the layers 1 < 2
      # < 3 < 4): the template's items (its look too) over the style set its
      # `style` names, over the files each step of `extends` names -- the
      # content step's none under `{content: none}`.  What a parameter's stop
      # value turns off (its `maps_to`) is taken out of those layers first.
      layers <- .tpl_strip_lower(layers, tpl$strip, tpl$x$content)
      tl <- list(data = tpl$x$data %||% list(), codelists = list(),
                 content = tpl$x$content, page = list())
      for (l in rev(c(list(tl), layers$layers))) acc <- .layer_over(acc, l)
      tpl$removed <- layers$removed
      layers <- c(list(tl), layers$layers)
    } else {
      # (a template not yet migrated, its set with no `extends`: the old
      # order -- its look (plan_style, the widths, the rows a page ...)
      # under the study's layers, which say the look and win, the rest over
      # them; until its batch)
      look <- vapply(tpl$x$content, .tpl_is_look, NA)
      tlook <- list(data = list(), codelists = list(), content = tpl$x$content[look], page = list())
      tl <- list(data = tpl$x$data %||% list(), codelists = list(),
                 content = tpl$x$content[!look], page = list())
      for (l in rev(c(list(tl), layers, list(tlook)))) acc <- .layer_over(acc, l)
      layers <- c(list(tl), layers, list(tlook))
    }
    output$ard <- .merge_ard(tpl$x$ard, output$ard)
    # a figure's design: its settings (`plot:`) under the report's own
    if (!is.null(tpl$x$plot)) output$plot <- .tpl_plot_over(tpl$x$plot, output$plot)
  }
  own <- list(data = output$data %||% list(), codelists = output$codelists,
              content = output$content, page = output$page)
  # a figure template and the report's own data steps: the report's steps
  # (its data stage: the analysis set, the parameter, the records kept),
  # then the template's call steps (the fit and what the call makes from it)
  composed <- !is.null(tpl) && .tpl_composes(own$data, tpl$x)
  if (composed) own$data <- .tpl_compose_data(own$data, tpl$x, output$id, tpl$name)
  below <- acc$content
  own$content <- .stub_keep_name(own$content, below)
  if (!is.null(tpl)) own$content <- .hdr_keep_stub(own$content, below)
  acc <- .layer_over(acc, own)
  if ((isTRUE(provenance) || !is.null(tpl)) && !inherits(acc$content, "tfl_r")) {
    prov <- .content_provenance(acc$content, output$content, layers, below)
    if (!is.null(tpl)) {
      # what the program writes after the template's call (the A use)
      r <- .tpl_rest(prov, tpl$name, tpl$x$content, layered = isTRUE(tpl$layered))
      # (a report's own data: the call's data step is the template's --
      # unless the report's steps come before the template's call steps)
      # (a figure's call draws the template's layers: none of them replaced)
      ok <- r$ok && (!length(own$data) || composed) &&
        (!identical(output$kind, "figure") || !length(intersect(
        vapply(r$items, `[[`, "", "fn"), vapply(tpl$x$content, `[[`, "", "fn"))))
      attr(output, "template") <- list(name = tpl$name, args = tpl$args, study = tpl$study,
                                       study_keys = tpl$study_keys,
                                       items = tpl$x$content, rest = r$items, ok = ok,
                                       layered = isTRUE(tpl$layered),
                                       plot = tpl$x$plot, call_steps = tpl$x$call_steps,
                                       extends = .extends_compact(lb$extends),
                                       style = lb$style_name, removed = tpl$removed,
                                       routes = .tpl_routes(tpl, lb, output, prov))
    }
    if (isTRUE(provenance)) acc$content <- prov
  }
  output$data <- if (length(acc$data)) acc$data else output$data
  output$codelists <- acc$codelists
  if (!is.null(tpl$x$codelists)) output$codelists <- .tpl_add_codelists(acc$codelists, tpl$x$codelists)
  output$content <- .any_label_default(acc$content, output, study)
  output$content <- .listing_rows_default(output$content, output$kind, provenance)
  # the study's rounding: the setup's option, so the program leaves it out
  attr(output, "study_rounding") <- .study_rounding(study)
  output$page <- acc$page
  output <- .page_font_to_plan(output)
  attr(output, "resolved") <- TRUE
  output
}

# A report's own plan_stub with no `name` keeps the name of the plan_stub it
# replaces from the layers below (the study's, `row_label`): a plan_stub item
# replaces that one as a whole, and the stub column would be called by its
# columns' names, so `plan_columns(widths = )` could not name it.  A name the
# report writes wins; with no plan_stub below, nothing changes.  Only `name`.
.stub_keep_name <- function(own, below) {
  if (inherits(own, "tfl_r") || inherits(below, "tfl_r") || !length(own)) return(own)
  is_stub <- function(it) identical(it$fn, "plan_stub")
  b <- Filter(is_stub, below)
  nm <- if (length(b)) b[[1L]]$args$name
  if (is.null(nm)) return(own)
  for (i in which(vapply(own, is_stub, NA))) {
    if (is.null(own[[i]]$args$name)) own[[i]]$args$name <- nm
  }
  own
}

# A report's own plan_col_header over a template's (not a study layer's:
# the item below must be the template's) with no cell for the template's
# stub column (a cell whose `cols` is not .values) keeps the template's stub
# cells, and an empty .values cell for a line only the stub then has.  Any
# stub cell of the report, even an empty one, replaces the item as before.
.hdr_keep_stub <- function(own, below) {
  if (inherits(own, "tfl_r") || inherits(below, "tfl_r") || !length(own)) return(own)
  cell_cols <- function(h) unlist(lapply(h, function(c) as.character(unlist(c$cols))))
  tpl <- Filter(function(it) identical(it$fn, "plan_col_header") &&
                  startsWith(it$from %||% "", "template:"), below)
  th <- if (length(tpl)) tpl[[1L]]$args$header
  stub <- Filter(function(c) !all(as.character(unlist(c$cols)) == ".values"), th)
  if (!length(stub)) return(own)
  for (i in which(vapply(own, function(it) identical(it$fn, "plan_col_header"), NA))) {
    h <- own[[i]]$args$header
    if (!length(h) || any(cell_cols(h) %in% cell_cols(stub))) next
    lines <- setdiff(vapply(stub, function(c) as.character(c$line), ""),
                     vapply(h, function(c) as.character(c$line), ""))
    own[[i]]$args$header <- c(stub, h, lapply(lines, function(l) list(line = l, cols = ".values")))
  }
  own
}

# Each resolved item's provenance, argument by argument (`arg_from`: "own",
# "defaults", "pattern:<name>", in the order of its args), and `below`: the
# same verb's item with the report's own layer left out (the k-th of that
# verb), NULL when no layer has it.  A named-dots verb (plan_cells ...)
# takes each top-level key from the nearest layer that has it; any other
# verb is one layer's whole.
.content_provenance <- function(items, own, layers, below) {
  own <- if (inherits(own, "tfl_r")) list() else own %||% list()
  # the layers from the nearest: the report's own, then `extends` in order
  stack <- c(list(own), lapply(layers, function(l) {
    c <- l$content
    if (inherits(c, "tfl_r")) list() else c %||% list()
  }))
  below <- if (inherits(below, "tfl_r")) list() else below
  seen <- list()
  for (i in seq_along(items)) {
    it <- items[[i]]
    k <- (seen[[it$fn]] %||% 0L) + 1L
    seen[[it$fn]] <- k
    keys <- names(it$args) %||% character()
    if (.arg_keyed(it$fn, items) && length(keys)) {
      from <- vapply(keys, function(key) {
        for (layer in stack) {
          hit <- Filter(function(x) identical(x$fn, it$fn) && key %in% names(x$args), layer)
          if (length(hit)) return(hit[[1L]]$from %||% "own")
        }
        it$from %||% "own"
      }, "")
    } else {
      from <- rep(it$from %||% "own", length(keys))
    }
    it$arg_from <- stats::setNames(from, keys)
    same <- Filter(function(x) identical(x$fn, it$fn), below)
    it$below <- if (length(same) >= k) same[[k]] else NULL
    items[[i]] <- it
  }
  items
}

# The layers a report extends (nearest first): its style (the look it
# picks, `style:`, the study's `styles:`), then the files of `extends`; and
# their content, data, code lists and page put together (the farthest first).
# Each part is laid by its step's files (tfl_output_extends()): a file only
# some steps name keeps only their parts.  `range`: the content a study
# reference reads (the style set's and the content step's files', merged);
# none when the content step lays nothing (`{content: none}`: every content
# reference takes its fallback, canon 1.32).
.layers_below <- function(output, study, style = "output") {
  ext <- .extends_of(output)
  files <- unique(c(ext$content, ext$data, ext$page))
  layers <- lapply(files, function(nm) {
    l <- .layer_of(nm, study)
    # the defaults' content is a table's: not a figure's or a listing's layers
    if (identical(nm, "defaults") && !identical(output$kind, "table")) l$content <- list()
    for (part in names(.extends_part_step)) {
      if (!nm %in% ext[[.extends_part_step[[part]]]]) l[part] <- list(list())
    }
    l
  })
  # (`style`: "output", the report's own `style:`; or the set a template's
  # `style` parameter names, NULL for none)
  st <- if (identical(style, "output")) .style_of(output, study) else style
  if (!is.null(st)) layers <- c(list(st), layers)
  acc <- list(data = list(), codelists = list(), content = list(), page = list())
  if (length(unique(ext[c("data", "content", "page")])) == 1L) {
    for (l in rev(layers)) acc <- .layer_over(acc, l)
  } else {
    # (the steps name their files in other orders: each part in its own)
    for (part in names(acc)) {
      chain <- ext[[.extends_part_step[[part]]]]
      ls <- c(if (!is.null(st)) list(st), layers[match(chain, files) + !is.null(st)])
      a <- list(data = list(), codelists = list(), content = list(), page = list())
      for (l in rev(ls)) a <- .layer_over(a, l)
      acc[[part]] <- a[[part]]
    }
  }
  list(layers = layers, acc = acc, style = st, extends = ext,
       range = if (length(ext$content)) acc$content else list())
}

# ---- extends: what a report lays under it, step by step ---------------------

# The steps a report is made in (canon 1.32), which `extends` may name one
# by one; and the step each part of a layer goes with (a layer's code lists
# with its data: Q3 of PLAN-5B; `ard` lays nothing yet, defaults.yml has
# no `ard:`)
.extends_steps <- c("data", "ard", "content", "page")
.extends_part_step <- c(data = "data", codelists = "data", content = "content", page = "page")

# `extends` as written, checked: NULL (not written); the files, nearest
# first ("defaults", "patterns/<name>"), or "none"; or a map by step, each
# step such a value.  The files as a character vector; a map's steps too.
.extends_value <- function(x, who) {
  if (is.null(x)) return(NULL)
  bad <- sprintf(paste0("%s: `extends` is the files laid under the report (\"defaults\", ",
                        "\"patterns/<name>\"), \"none\", or a map by step ({content: none}; the ",
                        "steps: %s)."), who, paste(.extends_steps, collapse = ", "))
  files <- function(v) {
    v <- if (is.list(v) && is.null(names(v))) unlist(v) else v
    is.character(v) && length(v) > 0L && !anyNA(v) && all(nzchar(v))
  }
  if (is.list(x) && length(x) && !is.null(names(x))) {
    unknown <- setdiff(names(x), .extends_steps)
    if (length(unknown) || !all(nzchar(names(x))) || anyDuplicated(names(x))) {
      stop(sprintf("%s: `extends` has no step %s; the steps: %s.", who,
                   paste(sQuote(unknown, FALSE), collapse = ", "),
                   paste(.extends_steps, collapse = ", ")), call. = FALSE)
    }
    if (!all(vapply(x, files, NA))) stop(bad, call. = FALSE)
    return(lapply(x, function(v) as.character(unlist(v))))
  }
  if (!files(x)) stop(bad, call. = FALSE)
  as.character(unlist(x))
}

# A template report's `extends: [defaults]` as every writer wrote it before
# a template's set held its `extends` (tfl_write_output() wrote the value
# tfl_read_output() filled in): read as not written, so a saved demog report
# keeps its output and a saved stf_t02_demog report its fixed content step.
# (Since demog's set is `[defaults]` (5b-2) the two read the same for every
# template but the stf_ family, where `[defaults]` stops.)  Pending
# decision of PR 5b-1: drop this, and such files take `[defaults]`.
.extends_old_constant <- function(x) {
  identical(as.character(unlist(x)), "defaults") && !(is.list(x) && !is.null(names(x)))
}

# A step's files from a written value ("none": none)
.extends_files <- function(v) if (identical(v, "none")) character() else v

# A written value as the files of each step (the four by name): a file
# list for every step, a map's steps their own, a step the map does not
# name `fallback`'s (a template report's map over its template's: Q1 of
# PLAN-5B), else `defaults`
.extends_norm <- function(x, fallback = NULL) {
  out <- stats::setNames(lapply(.extends_steps, function(s) fallback[[s]] %||% "defaults"),
                         .extends_steps)
  if (is.null(x)) return(out)
  # (an unnamed list, as a YAML sequence is read: the files)
  if (is.list(x) && is.null(names(x))) x <- as.character(unlist(x))
  if (!is.list(x)) return(stats::setNames(rep(list(.extends_files(x)), length(.extends_steps)),
                                          .extends_steps))
  for (s in names(x)) out[[s]] <- .extends_files(x[[s]])
  out
}

# The four steps' files as the shortest value that says them: one file
# list (or "none") when the steps agree, else a map of the steps that do
# not lay `defaults`
.extends_compact <- function(norm) {
  one <- function(f) if (length(f)) f else "none"
  if (length(unique(norm)) == 1L) return(one(norm[[1L]]))
  d <- Filter(function(s) !identical(norm[[s]], "defaults"), .extends_steps)
  lapply(norm[d], one)
}

# A written value as text: "defaults, patterns/x", "content: none; page: ..."
.extends_text <- function(x) {
  if (is.null(x)) return("(the template's)")
  if (!is.list(x)) return(paste(x, collapse = ", "))
  paste(sprintf("%s: %s", names(x), vapply(x, paste, "", collapse = ", ")), collapse = "; ")
}

# What a report extends, step by step: its own value, a step its map does
# not name its template's (the set's `extends`), else `defaults`.  A step
# the template fixes (`extends_fixed`) stops on another value; the file
# list form names every step, the fixed one too (Q2 of PLAN-5B).
.extends_of <- function(output) {
  ts <- .tpl_spec_of(output)
  t <- if (!is.null(ts)) tryCatch(.tpl_get(ts$name, "extends"), error = function(e) NULL)
  set <- .extends_norm(t$extends)
  out <- .extends_norm(output$extends, set)
  # (a plain [defaults], as files saved before 5b wrote it, on a template
  # that fixes a step reads as not written: its template's)
  own <- output$extends
  if (length(t$extends_fixed) && !is.null(own) && is.null(names(own)) &&
      identical(as.character(unlist(own)), "defaults")) out <- set
  for (s in t$extends_fixed) {
    if (identical(out[[s]], set[[s]])) next
    .ard_stop(sprintf(paste0("%s: template %s fixes extends' %s step to %s; write the other steps ",
                             "as a map ({page: [patterns/x, defaults]})."),
                      output$id, t$name, s, .extends_text(.extends_compact(list(set[[s]])))))
  }
  out
}

.extends_step <- function(output, step) .extends_of(output)[[step]]

#' What a report extends
#'
#' What a report extends, step by step: the files laid under it -- the
#' study's `defaults`, its patterns (`patterns/<name>`) -- in each of the
#' steps a report is made in (`data`, `ard`, `content`, `page`), nearest
#' first.  The report's `extends:` says it, as a list of files for every
#' step, `none`, or a map by step (`{content: none}`).  A step the map does
#' not name, or a template report that writes none, takes its template's
#' (a template's set holds its `extends`, [tfl_template()]), else
#' `defaults`.  A report's style set is laid over these files in any case.
#'
#' @param output A `tfl_output` ([tfl_read_output()]).
#' @param step One step's name; `NULL` for all.
#' @return The files laid under `step`, nearest first (`character(0)`:
#'   none); `NULL` `step`: a list by step (`data`, `ard`, `content`,
#'   `page`).
#' @examples
#' o <- tfl_output("T-1", "table", extends = list(content = "none"))
#' tfl_output_extends(o)
#' tfl_output_extends(o, "page")
#' @export
tfl_output_extends <- function(output, step = NULL) {
  if (!inherits(output, "tfl_output")) .ard_stop("tfl_output_extends(): `output` is a tfl_output.")
  ext <- .extends_of(output)
  if (is.null(step)) return(ext)
  if (!is.character(step) || length(step) != 1L || !step %in% .extends_steps) {
    .ard_stop(sprintf("tfl_output_extends(): `step` is one of %s.", paste(.extends_steps, collapse = ", ")))
  }
  ext[[step]]
}

# A report's style: the study's `styles:` set it names (`style: compact`),
# `standard` when it names none (none when the study has no `standard`).
# A figure's or a listing's style is its page only, as the defaults are.
.style_of <- function(output, study) {
  nm <- output$style %||% "standard"
  if (!is.character(nm) || length(nm) != 1L) {
    .ard_stop(sprintf("%s: `style` is one of the study's styles' names.", output$id))
  }
  st <- study$styles[[nm]] %||% if (nm %in% setdiff(.tpl_style_builtin_names, "standard"))
    .tpl_style_builtin_set(nm)
  if (is.null(st)) {
    if (identical(nm, "standard")) return(NULL)
    .ard_stop(sprintf("%s: no style %s in defaults.yml's styles: (%s).", output$id, sQuote(nm, FALSE),
                      paste(c(names(study$styles), "standard")[!duplicated(c(names(study$styles), "standard"))],
                            collapse = ", ")))
  }
  if (!identical(output$kind, "table")) st$content <- list()
  st
}

.layer_of <- function(nm, study) {
  if (identical(nm, "defaults")) return(study$defaults)
  if (startsWith(nm, "patterns/")) {
    p <- study$patterns[[substring(nm, 10L)]]
    if (is.null(p)) stop("No pattern ", sQuote(nm), " in spec/patterns/.", call. = FALSE)
    return(p)
  }
  stop("`extends` names \"defaults\" or \"patterns/<name>\", not ", sQuote(nm), ".",
       call. = FALSE)
}

# `over` on top of `base`, part by part
.layer_over <- function(base, over) {
  list(data = .merge_map(base$data, over$data),
       codelists = .merge_map(base$codelists, over$codelists),
       content = .merge_content(base$content, over$content),
       # (an old page.rtf_default_format is its own layer's rtf_style shared
       # level, before the layers meet: a near layer's old key beats a far
       # layer's new one)
       page = .merge_map(.page_style_norm(base$page), .page_style_norm(over$page)))
}

# a map over a map: by key; an !unset removes the key; two maps recurse; a
# sequence (unnamed list or vector) replaces whole
.merge_map <- function(base, over) {
  if (is.null(over) || !length(over)) return(.drop_unset(base))
  if (is.null(base) || !length(base) || !.is_map(base) || !.is_map(over)) return(.drop_unset(over))
  for (k in names(over)) {
    v <- over[[k]]
    if (inherits(v, "tfl_unset")) {
      base[[k]] <- NULL
    } else if (.is_map(v) && .is_map(base[[k]])) {
      base[[k]] <- .merge_map(base[[k]], v)
    } else {
      base[[k]] <- v
    }
  }
  base
}

.is_map <- function(x) {
  is.list(x) && !inherits(x, c("tfl_call", "tfl_item", "tfl_r", "tfl_unset")) &&
    length(x) > 0L && !is.null(names(x)) && all(nzchar(names(x)))
}

# an !unset with nothing under it: gone (a report may unset a key no layer set)
.drop_unset <- function(x) {
  # (an object -- an rtf_border, a tfl_call -- is a value; `[` would drop its class)
  if (!is.list(x) || is.object(x) || !length(x)) return(x)
  keep <- !vapply(x, inherits, NA, "tfl_unset")
  x <- x[keep]
  if (.is_map(x)) x[] <- lapply(x, .drop_unset)
  x
}

# The content, a layer over the layers below (S1's finding on the sample's
# 12 tables, 2026-10-14; widened to every plan verb 2026-11-09).  An
# appended card overrides only what it writes.  By the verb's merge kind
# (.merge_kind()):
# * "dots": a verb whose `...` is named (plan_cells, plan_labels,
#   plan_levels, plan_digits, plan_nest, plan_stat_groups, plan_compare: the
#   schema's dots style "named") merges by key -- the report's keys win, the
#   layers' others stay, in the layers' order first;
# * "stack": a verb that stacks several items (.stack_verbs: plan_cell_style,
#   plan_filter, plan_hide, plan_after, plan_append): the layers' items stay
#   where they are and the report's go right after the last of them, as
#   rtfreporter accumulates them;
# * "arg": any other plan verb (one item per table), when the layers below
#   have one item of it: the report's items are folded onto that one in
#   order, argument by argument (.merge_by_arg) -- the report's win, one it
#   leaves out stays (a template's page_by, decimal, where survive a
#   report's max_rows, widths, a style key; titles are replaced whole, as a
#   positional argument is one argument).  rtfreporter merges two calls of
#   these the same way, last wins per argument.  `NULL` or `!unset` takes
#   the layers' argument away.  With several items below, the report's
#   replace them;
# * "whole": anything else (a figure's ggplot calls, table_plan ...): the
#   report's items replace every item of that verb below, in the place the
#   first of them had;
# * a verb only the report has goes where the generators' order puts it
#   (the schema's `rank`), else last.
.merge_content <- function(base, over) {
  .drop_unset_items(.merge_content_layers(base, over))
}

# an !unset argument with nothing under it means "leave the argument out", on
# every item that leaves the merge (the early returns, a verb only the report
# has, a "whole" or "stack" item: not only where the "arg" merge ran)
.drop_unset_items <- function(content) {
  if (inherits(content, "tfl_r") || !is.list(content)) return(content)
  lapply(content, function(it) {
    if (is.list(it) && is.list(it$args) && length(it$args)) it$args <- .drop_unset(it$args)
    it
  })
}

.merge_content_layers <- function(base, over) {
  # the content as R code (one !r): the later one wins whole
  if (inherits(over, "tfl_r")) return(over)
  if (inherits(base, "tfl_r")) return(if (length(over)) .table_plan_first(over) else base)
  if (!length(over)) return(.table_plan_first(base))
  if (!length(base)) return(.table_plan_first(over))
  fns_over <- unique(vapply(over, `[[`, "", "fn"))
  n_base <- table(vapply(base, `[[`, "", "fn"))
  out <- list()
  placed <- character()
  seen <- list()
  used <- list()
  for (it in base) {
    if (!it$fn %in% fns_over) {
      out <- c(out, list(it))
      next
    }
    own <- Filter(function(o) identical(o$fn, it$fn), over)
    kind <- .merge_kind(it$fn)
    if (kind == "stack") {
      # the layers' items stay; the report's follow the last of them.  A
      # report's item that is the layers' one again (the same arguments; a
      # plan_append part of the same name) updates it by argument instead of
      # adding a second (a saved report that repeats its template's part)
      hit <- .stack_same(it, own, used[[it$fn]])
      if (length(hit)) {
        upd <- own[[hit]]
        upd$args <- .merge_by_arg(it$args, own[[hit]]$args)
        out <- c(out, list(upd))
        used[[it$fn]] <- c(used[[it$fn]], hit)
      } else {
        out <- c(out, list(it))
      }
      seen[[it$fn]] <- (seen[[it$fn]] %||% 0L) + 1L
      if (seen[[it$fn]] == n_base[[it$fn]]) {
        out <- c(out, own[setdiff(seq_along(own), used[[it$fn]])])
        placed <- c(placed, it$fn)
      }
    } else if (!it$fn %in% placed) {
      if (kind == "dots" && length(own) == 1L) {
        merged <- own[[1L]]
        merged$args <- .merge_dots(it$args, own[[1L]]$args)
        out <- c(out, list(merged))
      } else if (kind == "arg" && n_base[[it$fn]] == 1L) {
        merged <- own[[1L]]
        merged$args <- it$args
        for (o in own) {
          merged$args <- .merge_by_arg(.drop_alternatives(it$fn, merged$args, o$args), o$args)
        }
        out <- c(out, list(merged))
      } else {
        out <- c(out, own)
      }
      placed <- c(placed, it$fn)
    }
  }
  rest <- Filter(function(o) !o$fn %in% placed, over)
  for (o in rest) out <- .insert_by_rank(out, o)
  .table_plan_first(out)
}

# the report's item of a stacking verb that is the layers' `it` again: a
# plan_append part of the same `name`, or any other item with the same
# arguments; its index in `own` (not one already `used`), else none
.stack_same <- function(it, own, used = integer()) {
  for (i in setdiff(seq_along(own), used)) {
    o <- own[[i]]
    if (identical(it$args, o$args)) return(i)
    if (identical(it$fn, "plan_append") && !is.null(it$args$name) &&
        identical(it$args$name, o$args$name)) return(i)
  }
  integer()
}

# the plan verbs that stack several items: an appended card adds to them
.stack_verbs <- c("plan_cell_style", "plan_filter", "plan_hide", "plan_after", "plan_append")

# how a report's item of `fn` meets the layers' (see .merge_content):
# "dots" (by key), "stack" (added after), "arg" (argument by argument), or
# "whole" (replaced; not a plan verb)
.merge_kind <- function(fn) {
  if (!startsWith(fn, "plan_")) return("whole")
  if (.dots_named(fn)) return("dots")
  if (fn %in% .stack_verbs) return("stack")
  "arg"
}

# arguments that cannot be given together (rtfreporter stops on both): the
# header of plan_col_header is `header` or `lines`.  When the over writes one
# of a group, the base's others go.
.arg_alternatives <- list(plan_col_header = list(c("header", "lines")))

.drop_alternatives <- function(fn, base, over) {
  for (grp in .arg_alternatives[[fn]]) {
    wrote <- intersect(grp, names(over))
    if (length(wrote)) base[setdiff(grp, wrote)] <- NULL
  }
  base
}

# the base's arguments updated by the over's, by name: the over's value
# replaces the base's whole; an argument over leaves out stays; NULL and
# !unset take it away.  When both values are maps (a named list, all names
# set: widths = list(...), a style's keys), they merge by key, one level:
# the over's key wins, an !unset or NULL key goes
.merge_by_arg <- function(base, over) {
  base <- base %||% list()
  for (k in names(over)) {
    if (!nzchar(k)) next
    v <- over[[k]]
    if (is.null(v) || inherits(v, "tfl_unset")) {
      base[[k]] <- NULL
    } else if (.is_map(v) && .is_map(base[[k]])) {
      b <- base[[k]]
      for (kk in names(v)) {
        vv <- v[[kk]]
        b[[kk]] <- if (inherits(vv, "tfl_unset")) NULL else vv
      }
      base[[k]] <- b
    } else {
      base[[k]] <- v
    }
  }
  base
}

# does the item of `fn` in `items` keep its arguments' origin one by one?  A
# named-dots verb always; any other single-item plan verb when it is the
# only one of its kind (then it is the merged item, or the one that replaced
# the others)
.arg_keyed <- function(fn, items) {
  kind <- .merge_kind(fn)
  kind == "dots" || (kind == "arg" &&
    sum(vapply(items, function(x) identical(x$fn, fn), NA)) == 1L)
}

# a verb whose `...` is a map by name (the schema says); FALSE without a schema
.dots_named <- function(fn) {
  sc <- tryCatch(tfl_schema(fn), error = function(e) NULL)
  identical(sc$dots$style %||% "", "named")
}

# the generators' order of a verb (the schema's `rank`); NA without one
.verb_rank <- function(fn) {
  sc <- tryCatch(tfl_schema(fn), error = function(e) NULL)
  r <- sc$rank
  if (is.null(r) || !length(r)) NA_real_ else as.numeric(r[1L])
}

# an item put after the last item whose rank is not above its own; last
# when ranks are not known
.insert_by_rank <- function(items, it) {
  r <- .verb_rank(it$fn)
  if (is.na(r) || !length(items)) return(c(items, list(it)))
  ranks <- vapply(items, function(x) .verb_rank(x$fn), 1)
  # after the last of a rank at or below its own; with none, before the
  # first ranked above it -- after the items of no rank before that (a
  # figure designer's layers have none: an item goes after them)
  below <- which(!is.na(ranks) & ranks <= r)
  above <- which(!is.na(ranks) & ranks > r)
  at <- if (length(below)) max(below) else if (length(above)) min(above) - 1L else length(items)
  append(items, list(it), after = at)
}

# table_plan is the table itself, not a layer: first, wherever a file put it
.table_plan_first <- function(items) {
  is_tp <- vapply(items, function(it) identical(it$fn, "table_plan"), NA)
  c(items[is_tp], items[!is_tp])
}

# -- the spec's version --------------------------------------------------------

# The version of the YAML spec's form (study.yml's `spec_version`): this
# reader's.  A study of a newer form is refused (update tflspec); an older
# one is converted (tfl_convert_spec_version(), when there is one).
.spec_version <- 1L

# the packages a spec is written with: their versions, stamped into
# study.yml by whatever writes it (the app, the converter, the AI kit)
.spec_packages <- function() {
  pk <- c("tflspec", "rtfreporter", "cards", "cardx", "ggplot2")
  out <- list()
  for (p in pk) {
    v <- tryCatch(as.character(utils::packageVersion(p)), error = function(e) NULL)
    if (!is.null(v)) out[[p]] <- v
  }
  out
}

.check_spec_version <- function(v, file) {
  if (is.null(v)) return(invisible())
  v <- suppressWarnings(as.integer(v))
  if (is.na(v)) stop("tfl_read_study_spec(): ", file, " has a spec_version that is not a number.", call. = FALSE)
  if (v > .spec_version) {
    stop("tfl_read_study_spec(): ", file, " is of spec_version ", v, "; this tflspec reads up to ",
         .spec_version, ". Update tflspec.", call. = FALSE)
  }
  invisible()
}

# A named-dots verb's arguments over the layers' (plan_cells ...): by key,
# as .merge_map(); and a key that is one template in one layer and rows by
# name in the other (`default: "{n} ({p}%)"` in the defaults, `default:
# {n: ..., orr_ci: ...}` in the report) is both -- the template first, the
# rows after (what the sheets' default row and a report's rows make)
.merge_dots <- function(base, over) {
  tpl <- function(v) is.character(v) && length(v) == 1L && !inherits(v, c("tfl_r", "tfl_unset"))
  rows <- function(v) .is_map(v)
  for (k in intersect(names(base), names(over))) {
    b <- base[[k]]
    o <- over[[k]]
    if (tpl(b) && rows(o)) {
      over[[k]] <- c(list(b), o)
    } else if (rows(b) && tpl(o)) {
      over[[k]] <- c(list(o), b)
    }
  }
  .merge_map(base, over)
}

# page: rtf_tables's font_size_half_points (written by a converter before
# 0.0.24.9124): a table's font size is its plan's -- moved to the content's
# plan_style(font_size_half_points = ), the report's own plan_style key
# winning.  rtfreporter 0.9.0 has no rtf_tables(font_size_half_points = ),
# so it leaves the page either way: a content that is R code says its own
# (tfl_schema_check() names where)
.page_font_to_plan <- function(output) {
  fs <- output$page$rtf_tables$font_size_half_points
  if (is.null(fs)) return(output)
  output$page$rtf_tables$font_size_half_points <- NULL
  if (!length(output$page$rtf_tables)) output$page$rtf_tables <- NULL
  if (inherits(output$content, "tfl_r") || !output$kind %in% c("table", "listing")) {
    return(output)
  }
  k <- which(vapply(output$content, function(it) identical(it$fn, "plan_style"), NA))
  if (length(k)) {
    a <- output$content[[k[1L]]]$args
    if (is.null(a$font_size_half_points)) a$font_size_half_points <- fs
    output$content[[k[1L]]]$args <- a
  } else {
    output$content <- .insert_by_rank(output$content,
                                      tfl_item("plan_style", list(font_size_half_points = fs), from = "own"))
  }
  output
}

#' Write one part of a study's YAML spec
#'
#' Writes the study-level file of one part of a [tfl_read_study_spec()]
#' result -- what a builder changes for the whole study (the rounding of
#' every table, a pattern) -- in the form the reader reads.
#'
#' @param study A `tfl_study_spec`.
#' @param part `"defaults"`, `"patterns"` (one, by `name`),
#'   `"data"` or `"toc"`.
#' @param path The file; `NULL`: the study's own (`spec/defaults.yml`,
#'   `spec/patterns/<name>.yml` ...).
#' @param name For `part = "patterns"`: the pattern.
#' @return `path`, invisibly.
#' @examples
#' d <- system.file("extdata", "spec-yaml", package = "tflspec")
#' s <- tfl_read_study_spec(d)
#' f <- tempfile(fileext = ".yml")
#' tfl_write_study_part(s, "defaults", f)
#' identical(tfl_read_study_spec(d)$defaults$content[[1]]$fn,
#'           yaml::read_yaml(f)$content[[1]] |> names())
#' @export
tfl_write_study_part <- function(study, part = c("defaults", "patterns", "data", "toc"),
                                 path = NULL, name = NULL) {
  fn <- "tfl_write_study_part()"
  if (!inherits(study, "tfl_study_spec")) .ard_stop(sprintf("%s: `study` is a tfl_study_spec.", fn))
  part <- match.arg(part)
  layer_out <- function(l) {
    y <- list()
    if (length(l$content)) {
      y$content <- if (inherits(l$content, "tfl_r")) l$content else .items_to_list(l$content)
    }
    if (length(l$page)) y$page <- .calls_out(l$page)
    if (length(l$data)) y$data <- .calls_out(l$data)
    if (length(l$codelists)) y$codelists <- l$codelists
    y
  }
  y <- switch(part,
    defaults = {
      y <- layer_out(study$defaults)
      if (length(study$styles)) y$styles <- lapply(study$styles, layer_out)
      y
    },
    patterns = {
      if (is.null(name) || is.null(study$patterns[[name]])) {
        .ard_stop(sprintf("%s: `name` is one of the study's patterns: %s.", fn,
                          paste(names(study$patterns), collapse = ", ")))
      }
      layer_out(study$patterns[[name]])
    },
    data = .calls_out(study$data),
    toc = study$toc)
  if (is.null(path)) {
    spec <- .spec_dir(study$dir)
    path <- switch(part, patterns = file.path(spec, "patterns", paste0(name, ".yml")),
                   file.path(spec, paste0(part, ".yml")))
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(yaml::as.yaml(y, indent.mapping.sequence = TRUE, handlers = .yaml_out_handlers()),
             path, useBytes = TRUE)
  invisible(path)
}

# A table's normalize_ard() without `overall`, of a report whose hierarchy
# reads a set of records with its "any" row (over_variables = TRUE): the
# set's any_label (data.yml `records:`), else "Any <id>"
.any_label_default <- function(items, output, study) {
  if (inherits(items, "tfl_r") || !identical(output$kind, "table")) return(items)
  an <- Filter(function(e) !is.null(e$records) && grepl("hierarchical", e$method %||% "") &&
                 grepl("over_variables\\s*=\\s*TRUE", paste(as.character(e$args %||% ""), collapse = " ")),
               output$ard$analyses %||% list())
  if (!length(an)) return(items)
  rec <- .normalize_ard_sheet(.rows_df(lapply(study$data$records %||% list(), function(e) {
    m <- lapply(e, .sheet_cell); m$record_id <- m$id; m$id <- NULL; m
  }), .ard_spec_sheets$records), "records")
  lab <- .records_any_label(rec, an[[1L]]$records)
  if (is.null(lab)) return(items)
  lapply(items, function(it) {
    if (identical(it$fn, "normalize_ard") && is.null(it$args$overall) &&
        length(it$args$hierarchy)) it$args$overall <- lab
    it
  })
}

# A listing with no page size pages at the rows a page the `listing` template
# has (.listing_rows_a_page): a plan_listing and no plan_paginate_rows or
# plan_paginate_group among the resolved items (the report's, a template's or
# a pattern's say it; the study's defaults do not reach a listing) gets
# plan_paginate_rows(max_rows) last, from the "defaults".  Built and written
# in the program, not in the report's file; code (`tfl_r`) and a figure are
# left alone.
.listing_rows_a_page <- 22L
.listing_rows_default <- function(items, kind = NULL, provenance = FALSE) {
  if (inherits(items, "tfl_r") || identical(kind, "figure")) return(items)
  fns <- vapply(items, function(it) as.character(it$fn), "")
  if (!"plan_listing" %in% fns || any(c("plan_paginate_rows", "plan_paginate_group") %in% fns)) return(items)
  it <- tfl_item("plan_paginate_rows", list(max_rows = .listing_rows_a_page), from = "defaults")
  if (isTRUE(provenance)) {
    it$arg_from <- c(max_rows = "defaults")
    it$below <- NULL
  }
  c(items, list(it))
}

# The folder of a study's report files: spec/outputs, or -- while the AI's
# drafts are read and built (R/ai_adopt.R) -- the folder the option
# `tflspec.outputs_dir` names (relative to the study folder)
.outputs_dir <- function(dir) {
  o <- getOption("tflspec.outputs_dir")
  if (is.null(o)) return(file.path(.spec_dir(dir), "outputs"))
  if (grepl("^([A-Za-z]:)?[/\\]", o)) o else file.path(dir, o)
}

#' The page configurations of a study
#'
#' A study's page configurations: the style sets a report picks with
#' `style:` -- the built-in ones tflspec ships (`standard`, the base and
#' nothing more; `stf_t02`) and the study's own (`defaults.yml`'s
#' `styles:`).  A set holds the settings that differ from the study's base:
#' the `page` half (the `rtf_*` calls' arguments) and the `content` half
#' (the table look verbs, such as `plan_style` and `plan_paginate_rows`).
#' The study's default is the set `settings.style` of `study.yml` names
#' (`standard` when it names none).
#'
#' @param study The study: a `tfl_study_spec` ([tfl_read_study_spec()]) or
#'   the path of the study's folder.
#' @return A data frame, one row a set: `name`; `source` (`"built-in"` or
#'   `"study"`); `default` (`TRUE` for the one a report without `style:`
#'   gets); `page_keys` and `content_verbs` (what the set holds, comma
#'   joined; empty for none).  A study's set named as a built-in one does
#'   not replace it: the row is the built-in's.
#' @examples
#' s <- tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))
#' tfl_styles(s)
#' @export
tfl_styles <- function(study) {
  fn <- "tfl_styles()"
  if (is.character(study) && length(study) == 1L && !is.na(study)) {
    study <- tfl_read_study_spec(study)
  }
  if (!inherits(study, "tfl_study_spec")) {
    .ard_stop(sprintf("%s: `study` is a tfl_study_spec, or the path of a study's folder.", fn))
  }
  own <- setdiff(names(study$styles), .tpl_style_builtin_names)
  nm <- c(.tpl_style_builtin_names, own)
  sets <- lapply(nm, function(n) .tpl_style_set(n, study))
  dflt <- .tpl_study_read1("settings.style", NULL, study) %||% "standard"
  data.frame(
    name = nm,
    source = ifelse(nm %in% .tpl_style_builtin_names, "built-in", "study"),
    default = nm == dflt,
    page_keys = vapply(sets, function(x) paste(names(x$page), collapse = ", "), ""),
    content_verbs = vapply(sets, function(x)
      paste(unique(vapply(x$content, function(it) it$fn, "")), collapse = ", "), ""),
    stringsAsFactors = FALSE)
}
