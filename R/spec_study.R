# The study's files of the YAML spec, and the layering of a report over its
# defaults (design: _archive/spec-format-redesign-2026-10-14.md, 3.2-3.4).
#
#   study.yml                 the study: id, title ..., keys, setup
#   spec/data.yml             datasets, populations (the data catalog)
#   spec/defaults.yml         what every report starts from (a report's shape),
#                             and the study's styles: the look a report picks
#                             (`styles: {compact: {content, page}}`, `style:`)
#   spec/codelists.yml        code lists to copy from (templates)
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
#' files stops the read.
#'
#' @param dir The study folder (holds `study.yml` and `spec/`).
#' @return `tfl_read_study_spec()`: a list of class `tfl_study_spec` --
#'   `study`, `data`, `defaults`, `styles` (by name: `defaults.yml`'s
#'   `styles:`, the look a report picks with `style:`), `codelists`,
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
  structure(list(
    study = study,
    data = .calls_in(rd(file.path(spec, "data.yml")) %||% list()),
    defaults = .layer_from_list(dflt, "defaults"),
    styles = stats::setNames(lapply(names(styles), function(nm)
      .layer_from_list(styles[[nm]], paste0("style:", nm))), names(styles)),
    codelists = rd(file.path(spec, "codelists.yml")) %||% list(),
    patterns = patterns,
    toc = rd(file.path(spec, "toc.yml")) %||% list(),
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
tfl_read_outputs <- function(dir) {
  out_dir <- file.path(.spec_dir(dir), "outputs")
  files <- list.files(out_dir, "\\.ya?ml$", recursive = TRUE, full.names = TRUE)
  outs <- list()
  where <- character()
  for (f in files) {
    r <- tfl_read_output(f)
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
#' and the report's own -- its look (plan_style, the widths, the rows a
#' page ...) under the study's layers, which say the look and win -- and
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
  lb <- .layers_below(output, study)
  layers <- lb$layers
  acc <- lb$acc
  # a template SPEC: the template's items between the layers and the
  # report's own (its ard: under the report's ard:) -- its look (plan_style,
  # the widths, the rows a page ...) under the study's layers, which say the
  # look and win, the rest over them
  tpl <- NULL
  if (!is.null(.tpl_spec_of(output))) {
    tpl <- .tpl_expand_for(output, study, acc$content,
                           .merge_map(acc$codelists, output$codelists))
    look <- vapply(tpl$x$content, function(it) it$fn %in% names(.tpl_look), NA)
    tlook <- list(data = list(), codelists = list(), content = tpl$x$content[look], page = list())
    tl <- list(data = tpl$x$data %||% list(), codelists = list(),
               content = tpl$x$content[!look], page = list())
    acc <- list(data = list(), codelists = list(), content = list(), page = list())
    for (l in rev(c(list(tl), layers, list(tlook)))) acc <- .layer_over(acc, l)
    layers <- c(list(tl), layers, list(tlook))
    output$ard <- .merge_ard(tpl$x$ard, output$ard)
  }
  own <- list(data = output$data %||% list(), codelists = output$codelists,
              content = output$content, page = output$page)
  below <- acc$content
  acc <- .layer_over(acc, own)
  if ((isTRUE(provenance) || !is.null(tpl)) && !inherits(acc$content, "tfl_r")) {
    prov <- .content_provenance(acc$content, output$content, layers, below)
    if (!is.null(tpl)) {
      # what the program writes after the template's call (the A use)
      r <- .tpl_rest(prov, tpl$name)
      # (a report's own data: the call's data step is the template's)
      attr(output, "template") <- list(name = tpl$name, args = tpl$args, study = tpl$study,
                                       items = tpl$x$content, rest = r$items,
                                       ok = r$ok && !length(own$data))
    }
    if (isTRUE(provenance)) acc$content <- prov
  }
  output$data <- if (length(acc$data)) acc$data else output$data
  output$codelists <- acc$codelists
  output$content <- acc$content
  output$page <- acc$page
  output <- .page_font_to_plan(output)
  attr(output, "resolved") <- TRUE
  output
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
    if (.dots_named(it$fn) && length(keys)) {
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
# their content, data, code lists and page put together (the farthest first)
.layers_below <- function(output, study) {
  chain <- output$extends
  if (identical(chain, "none")) chain <- character()
  layers <- lapply(chain, function(nm) {
    l <- .layer_of(nm, study)
    # the defaults' content is a table's: not a figure's or a listing's layers
    if (identical(nm, "defaults") && !identical(output$kind, "table")) l$content <- list()
    l
  })
  st <- .style_of(output, study)
  if (!is.null(st)) layers <- c(list(st), layers)
  acc <- list(data = list(), codelists = list(), content = list(), page = list())
  for (l in rev(layers)) acc <- .layer_over(acc, l)
  list(layers = layers, acc = acc)
}

# A report's style: the study's `styles:` set it names (`style: compact`),
# `standard` when it names none (none when the study has no `standard`).
# A figure's or a listing's style is its page only, as the defaults are.
.style_of <- function(output, study) {
  nm <- output$style %||% "standard"
  if (!is.character(nm) || length(nm) != 1L) {
    .ard_stop(sprintf("%s: `style` is one of the study's styles' names.", output$id))
  }
  st <- study$styles[[nm]]
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
       page = .merge_map(base$page, over$page))
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
  if (!is.list(x) || !length(x)) return(x)
  keep <- !vapply(x, inherits, NA, "tfl_unset")
  x <- x[keep]
  if (.is_map(x)) x[] <- lapply(x, .drop_unset)
  x
}

# The content, a layer over the layers below (S1's finding on the sample's
# 12 tables, 2026-10-14):
# * a verb whose `...` is named (plan_cells, plan_labels, plan_levels,
#   plan_digits, plan_stat_groups, plan_compare: the schema's dots style
#   "named") merges by key -- the report's keys win, the layers' others
#   stay, in the layers' order first -- as the sheets layered them;
# * any other verb: the report's items replace every item of that verb
#   below, in the place the first of them had;
# * a verb only the report has goes where the generators' order puts it
#   (the schema's `rank`), else last.
.merge_content <- function(base, over) {
  # the content as R code (one !r): the later one wins whole
  if (inherits(over, "tfl_r")) return(over)
  if (inherits(base, "tfl_r")) return(if (length(over)) .table_plan_first(over) else base)
  if (!length(over)) return(.table_plan_first(base))
  if (!length(base)) return(.table_plan_first(over))
  fns_over <- unique(vapply(over, `[[`, "", "fn"))
  out <- list()
  placed <- character()
  for (it in base) {
    if (it$fn %in% fns_over) {
      if (!it$fn %in% placed) {
        own <- Filter(function(o) identical(o$fn, it$fn), over)
        if (.dots_named(it$fn) && length(own) == 1L) {
          merged <- own[[1L]]
          merged$args <- .merge_dots(it$args, own[[1L]]$args)
          out <- c(out, list(merged))
        } else {
          out <- c(out, own)
        }
        placed <- c(placed, it$fn)
      }
    } else {
      out <- c(out, list(it))
    }
  }
  rest <- Filter(function(o) !o$fn %in% placed, over)
  for (o in rest) out <- .insert_by_rank(out, o)
  .table_plan_first(out)
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
  at <- max(c(0L, which(!is.na(ranks) & ranks <= r)))
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
# winning; a content that is R code keeps it on the page
.page_font_to_plan <- function(output) {
  fs <- output$page$rtf_tables$font_size_half_points
  if (is.null(fs) || inherits(output$content, "tfl_r") ||
      !output$kind %in% c("table", "listing")) return(output)
  output$page$rtf_tables$font_size_half_points <- NULL
  if (!length(output$page$rtf_tables)) output$page$rtf_tables <- NULL
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
#' every table, a pattern, a code list) -- in the form the reader reads.
#'
#' @param study A `tfl_study_spec`.
#' @param part `"defaults"`, `"patterns"` (one, by `name`), `"codelists"`,
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
tfl_write_study_part <- function(study, part = c("defaults", "patterns", "codelists", "data", "toc"),
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
    codelists = study$codelists,
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
