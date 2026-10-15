# The study's files of the YAML spec, and the layering of a report over its
# defaults (design: _archive/spec-format-redesign-2026-10-14.md, 3.2-3.4).
#
#   study.yml                 the study: id, title ..., keys, setup
#   spec/data.yml             datasets, populations (the data catalog)
#   spec/defaults.yml         what every report starts from (a report's shape)
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
#'   `study`, `data`, `defaults`, `codelists`, `patterns` (by name), `toc`,
#'   `dir`.  `tfl_read_outputs()`: a named list of `tfl_output`, by id.
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
  structure(list(
    study = study,
    data = .calls_in(rd(file.path(spec, "data.yml")) %||% list()),
    defaults = .layer_from_list(rd(file.path(spec, "defaults.yml")), "defaults"),
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
#' parts (`page`, `codelists`), not its content.  `extends: none` uses no
#' layer.  Each item's
#' `from` says where it came from.  The result holds no `tfl_unset`.
#'
#' @param output A `tfl_output` as read from its file.
#' @param study A `tfl_study_spec` (its `defaults` and `patterns`).
#' @return The `tfl_output`, resolved (attribute `resolved` = `TRUE`).
#' @examples
#' d <- system.file("extdata", "spec-yaml", package = "tflspec")
#' s <- tfl_read_study_spec(d)
#' out <- tfl_resolve_output(tfl_read_outputs(d)[["T-14-1-1"]], s)
#' vapply(out$content, function(it) paste(it$fn, it$from), "")
#' @export
tfl_resolve_output <- function(output, study) {
  if (!inherits(output, "tfl_output")) stop("tfl_resolve_output(): `output` is a tfl_output.", call. = FALSE)
  if (!inherits(study, "tfl_study_spec")) stop("tfl_resolve_output(): `study` is a tfl_study_spec.", call. = FALSE)
  chain <- output$extends
  if (identical(chain, "none")) chain <- character()
  layers <- lapply(chain, function(nm) {
    l <- .layer_of(nm, study)
    # the defaults' content is a table's: not a figure's or a listing's layers
    if (identical(nm, "defaults") && !identical(output$kind, "table")) l$content <- list()
    l
  })
  # the farthest first, the report's own last
  acc <- list(data = list(), codelists = list(), content = list(), page = list())
  for (l in rev(layers)) acc <- .layer_over(acc, l)
  own <- list(data = output$data %||% list(), codelists = output$codelists,
              content = output$content, page = output$page)
  acc <- .layer_over(acc, own)
  output$data <- if (length(acc$data)) acc$data else output$data
  output$codelists <- acc$codelists
  output$content <- acc$content
  output$page <- acc$page
  attr(output, "resolved") <- TRUE
  output
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
          merged$args <- .merge_map(it$args, own[[1L]]$args)
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
