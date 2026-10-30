# ============================================================================
#  One output, several objects: `contents:` (decided 10/23, B)
# ----------------------------------------------------------------------------
#  An output (one RTF) can hold several objects -- tables, figures -- on
#  its pages.  `contents: {name: [items]}` names them: a short form (a
#  list of items) reads the output's `data` / `ard`; a long form
#  (`{kind, data, content, template, plot}`) has its own data.  The
#  output's `page.layout:` rows say each object's place, as rtfreporter's
#  rtf_tables() / rtf_figures() take it (page_break, space_before,
#  first_rows, cont_title, titles, footnotes, width_in, height_in).  The
#  output's titles go above the first object, its footnotes below the last.
#
#  Each object is run as an output of its own (.output_objects()): the
#  same resolve, ARD, content and figure code as a one-object report.
# ============================================================================

.contents_long_keys <- c("kind", "data", "content", "template", "plot")

# `contents:` as read: each object's content as items, its data as steps
.contents_in <- function(x, id) {
  if (is.null(x)) return(NULL)
  if (!is.list(x) || is.null(names(x)) || any(!nzchar(names(x))) || anyDuplicated(names(x))) {
    stop("tfl_read_output(): ", id, "'s contents: is a map of objects, each named once ",
         "(contents: {demog: [...], km: {...}}).", call. = FALSE)
  }
  out <- lapply(names(x), function(nm) {
    el <- x[[nm]]
    if (is.null(el)) el <- list()
    short <- is.null(names(el)) || !all(names(el) %in% .contents_long_keys)
    if (short) {
      if (!is.list(el) || (length(el) && !is.null(names(el)))) {
        stop("tfl_read_output(): ", id, "'s object ", nm, " is a list of items or a map of ",
             paste(.contents_long_keys, collapse = ", "), ".", call. = FALSE)
      }
      return(list(content = lapply(el, .item_from_list), short = TRUE))
    }
    Filter(Negate(is.null), list(
      kind = el$kind,
      data = .data_steps_in(.calls_in(el$data)),
      content = lapply(el$content %||% list(), .item_from_list),
      template = .calls_in(el$template),
      plot = .calls_in(el$plot),
      short = FALSE))
  })
  stats::setNames(out, names(x))
}

# `contents:` as written: the form it was read in
.contents_out <- function(cs) {
  lapply(cs, function(o) {
    if (isTRUE(o$short)) return(.items_to_list(o$content %||% list()))
    Filter(Negate(is.null), list(
      kind = o$kind, data = .calls_out(o$data), template = .calls_out(o$template),
      plot = .calls_out(o$plot),
      content = if (length(o$content)) .items_to_list(o$content)))
  })
}

# An object's kind: said, else what its items or its template make
.object_kind <- function(co) {
  if (!is.null(co$kind)) return(co$kind)
  fns <- vapply(co$content %||% list(), function(it) .schema_fn_name(it$fn), "")
  if ("plan_listing" %in% fns) return("listing")
  if ("table_plan" %in% fns) return("table")
  if (!is.null(co$template$name)) {
    t <- tryCatch(.tpl_get(co$template$name, "contents:"), error = function(e) NULL)
    if (!is.null(t$kind)) return(t$kind)
  }
  "figure"
}

# The output's layout rows, by object, in their order: the layout's, else
# the objects' order (each on under the one before)
.contents_layout <- function(output) {
  cs <- output$contents
  lay <- output$page$layout
  if (is.null(lay) || !length(lay)) {
    lay <- lapply(names(cs), function(n) list(object = n))
  }
  objs <- vapply(lay, function(r) as.character(r$object %||% NA_character_), "")
  bad <- c(if (anyNA(objs)) "a layout row names no object",
           if (length(setdiff(objs[!is.na(objs)], names(cs))))
             sprintf("the layout names %s, not in contents (%s)",
                     paste(setdiff(objs, names(cs)), collapse = ", "),
                     paste(names(cs), collapse = ", ")),
           if (length(setdiff(names(cs), objs)))
             sprintf("contents %s not in the layout", paste(setdiff(names(cs), objs), collapse = ", ")),
           if (anyDuplicated(objs)) "the layout names an object twice")
  if (length(bad)) .ard_stop(sprintf("%s: page.layout: %s.", output$id, paste(bad, collapse = "; ")))
  stats::setNames(lay, objs)
}

# The output's objects, each an output of its own (the layout's order): the
# output's own parts, with the object's kind, content and -- in the long
# form -- its data, template and plot; no page (the output's is the RTF's)
.output_objects <- function(output) {
  lay <- .contents_layout(output)
  lapply(stats::setNames(names(lay), names(lay)), function(nm) {
    co <- output$contents[[nm]]
    o <- output
    o$contents <- NULL
    o$kind <- .object_kind(co)
    o$content <- co$content %||% list()
    o$page <- list()
    o$template <- NULL
    o$plot <- NULL
    if (!is.null(co$data)) o$data <- co$data
    if (!is.null(co$template)) o$template <- co$template
    if (!is.null(co$plot)) o$plot <- co$plot
    if (identical(o$kind, "figure") && is.null(co$data) && identical(output$data$source, "ard")) {
      # (a figure that reads the output's ARD)
      o$data <- output$data
    }
    attr(o, "object") <- nm
    attr(o, "layout") <- lay[[nm]]
    o
  })
}

#' The objects of a report with several: each built
#'
#' A report whose `contents:` names several objects (one RTF, its tables,
#' figures and listings on its pages): each object run as a report of its
#' own -- a table's plan from the report's ARD ([tfl_build_ard()],
#' [tfl_content_run()]), a figure's plot from its data steps, a listing's
#' plan from its records (its `data: {source: dataset, ...}`).  What
#' [tfl_report()] takes as its `content` for such a report.
#'
#' @param output The report ([tfl_read_output()]), with `contents:`.
#' @param study The study ([tfl_read_study_spec()]).
#' @param dir The study folder (the ARD and the data are read from it).
#' @return A named list, one element an object (its name in `contents:`):
#'   a table's or a listing's plan, a figure's plot.
#' @seealso [tfl_run_output()]
#' @export
tfl_output_contents <- function(output, study, dir = ".") {
  if (!inherits(output, "tfl_output") || is.null(output$contents)) {
    .ard_stop("tfl_output_contents(): `output` is a report with `contents:`.")
  }
  objs <- .output_objects(output)
  ard <- NULL
  get_ard <- function() {
    if (is.null(ard)) {
      a <- suppressMessages(tfl_build_ard(study, dir = dir, output_id = output$id, save = FALSE))
      if (!is.null(a$output_id)) a <- a[a$output_id %in% output$id, , drop = FALSE]
      ard <<- a
    }
    ard
  }
  lapply(objs, function(o) {
    ro <- tfl_resolve_output(o, study)
    if (identical(o$kind, "table")) {
      return(suppressMessages(tfl_content_run(ro, get_ard())))
    }
    if (identical(o$kind, "figure")) {
      data <- .contents_fig_data(ro, study, dir)
      if (identical(ro$data$source, "ard") || length(o$ard$analyses %||% list())) data$ard <- get_ard()
      return(suppressMessages(suppressWarnings(tfl_content_run(ro, data))))
    }
    if (identical(o$kind, "listing")) {
      # a listing's records: its data step, as its program reads them; its
      # pages laid out (the first on the page before, the rest after it)
      return(suppressMessages(tfl_content_run(ro, .listing_data_of(ro, study, dir))))
    }
    .ard_stop(sprintf("%s: object %s is a %s; contents: holds tables, figures and listings.",
                      output$id, attr(o, "object"), o$kind))
  })
}

# A figure object's datasets: those its data steps read, from data.yml's paths
.contents_fig_data <- function(o, study, dir) {
  ds <- study$data$datasets %||% list()
  have <- toupper(vapply(ds, function(z) z$dataset %||% NA_character_, ""))
  steps <- o$data$steps %||% list()
  reads <- unique(toupper(unlist(lapply(steps, function(s) if (is.list(s)) s$dataset))))
  data <- list()
  for (r in reads) {
    i <- match(r, have)
    if (is.na(i)) .ard_stop(sprintf("%s reads %s, which spec/data.yml does not list.", o$id, r))
    p <- ds[[i]]$path
    if (!grepl("^([A-Za-z]:)?[/\\\\]", p)) p <- file.path(dir, p)
    data[[tolower(r)]] <- .ai_read_dataset(p)
  }
  # the report's analysis data its reads name (`data:`): its data part of
  # the ARD program run from the study folder (tfl_ard_code(part = "data"))
  ids <- unique(unlist(lapply(steps, function(s)
    if (is.list(s) && identical(s$step, "read")) .fig_read_data(s))))
  if (length(ids)) {
    old <- options(tflspec.plain_ns = TRUE)
    on.exit(options(old), add = TRUE)
    code <- tfl_ard_code(study, o$id, part = "data", data = ids)
    env <- new.env(parent = globalenv())
    eval(parse(text = tfl_helpers_code()), env)
    wd <- setwd(dir)
    on.exit(setwd(wd), add = TRUE)
    eval(parse(text = code), env)
    for (id in ids) data[[id]] <- get(id, envir = env)
  }
  data
}

# A listing's records (`data: {source: dataset, ...}`): the data part of
# its program (tfl_listing_code(part = "data"): the dataset, or the
# report's analysis data, its analysis set's subjects, its condition, its
# order) run from the study folder
.listing_data_of <- function(o, study, dir = ".") {
  d <- o$data %||% list()
  txt <- function(x) {
    v <- unlist(lapply(x, as.character))
    if (!length(v)) NA_character_ else paste(v, collapse = " | ")
  }
  # (`[[`: `d$data` would be `dataset` by partial matching)
  l <- data.frame(output_id = o$id, dataset = txt(d[["dataset"]]), data = txt(d[["data"]]),
                  population_id = txt(d[["population"]]), where = txt(d[["where"]]),
                  sort = txt(d[["sort"]]),
                  stringsAsFactors = FALSE)
  sp <- tfl_listing_spec(l, data.frame(output_id = o$id, vars = "USUBJID"), check = FALSE)
  old <- options(tflspec.plain_ns = TRUE)
  on.exit(options(old), add = TRUE)
  ard <- .ard_spec_of_study(study, output_id = o$id)$spec
  code <- tfl_listing_code(sp, o$id, ard = ard, part = "data")
  if (is.null(code)) .ard_stop(sprintf("%s: the listing names no dataset.", o$id))
  env <- new.env(parent = globalenv())
  eval(parse(text = tfl_helpers_code()), env)
  wd <- setwd(dir)
  on.exit(setwd(wd), add = TRUE)
  eval(parse(text = code), env)
  get("data", envir = env, inherits = FALSE)
}

# A report's content object -- the plan of a table or a listing, the
# figure(s) -- from the study's files, in the study folder (the working
# directory): the data read, its ARD built, its content run.  What
# tfl_run_output() writes to RTF, and what tfl_check_build() makes to know
# that the report can be made.
.output_build_content <- function(output, st) {
  # (each stage named in what stops it: "the ARD: ...", "the table: ...")
  stage <- function(what, expr) {
    tryCatch(expr, error = function(e) .ard_stop(paste0(what, ": ", conditionMessage(e))))
  }
  kind <- output$kind %||% "table"
  if (!is.null(output$contents)) {
    return(stage("the report's objects", tfl_output_contents(output, st, ".")))
  }
  ro <- stage("resolving the report", tfl_resolve_output(output, st))
  if (any(vapply(ro$content, .is_part, NA))) {
    # a table with parts: each object it reads, by name -- the ARD, a
    # listing's records (the table's own data, a part's)
    objects <- stage("the parts' data", .parts_objects(ro, st, "."))
    stage(paste("the", kind), suppressMessages(tfl_content_run(ro, objects)))
  } else if (identical(ro$data$source, "dataset")) {
    # a listing's records (its own, or a template's: decided on the report
    # as resolved): its data step, as its program reads them
    recs <- stage("the listing's records", .listing_data_of(ro, st, "."))
    stage(paste("the", kind), suppressMessages(tfl_content_run(ro, recs)))
  } else if (identical(kind, "figure")) {
    data <- stage("the figure's data", .contents_fig_data(ro, st, "."))
    if (length(ro$ard$analyses %||% list())) {
      data$ard <- stage("the ARD", tfl_build_ard(st, ".", output_id = ro$id, save = FALSE))
    }
    stage("the figure", suppressMessages(suppressWarnings(tfl_content_run(ro, data))))
  } else {
    ard <- stage("the ARD", suppressMessages(tfl_build_ard(st, dir = ".", output_id = ro$id, save = FALSE)))
    if (!is.null(ard$output_id)) ard <- ard[ard$output_id %in% ro$id, , drop = FALSE]
    stage(paste("the", kind), suppressMessages(tfl_content_run(ro, ard)))
  }
}

#' Build a report's RTF from its YAML file
#'
#' One line to make a report's RTF from the study's files: the report read
#' from `spec/outputs/`, its ARD built, its content run, its RTF written
#' where [tfl_report_path()] says.  A report with several objects
#' (`contents:`) has them on its pages as its `page.layout:` says.
#'
#' @param output A report's id, its YAML file, or a [tfl_output()].
#' @param dir The study folder.
#' @param overwrite Write over an RTF already there.
#' @return The RTF's path, invisibly.
#' @seealso [tfl_output_contents()], [tfl_report()]
#' @export
tfl_run_output <- function(output, dir = ".", overwrite = TRUE) {
  .spec_need_rtfreporter()
  if (is.character(output) && length(output) == 1L) {
    output <- if (file.exists(output)) tfl_read_output(output) else tfl_read_outputs(dir)[[output]]
  }
  if (!inherits(output, "tfl_output")) .ard_stop("tfl_run_output(): no such report.")
  old <- setwd(dir)
  on.exit(setwd(old), add = TRUE)
  st <- tfl_read_study_spec(".")
  # the study's rounding (a half away from zero, as SAS: 63.75 is 63.8),
  # which the programs' setup sets: here too
  old_r <- .study_rounding_option(st)
  on.exit(options(old_r), add = TRUE)
  content <- .output_build_content(output, st)
  f <- tfl_report_path(output, study = st)
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  suppressMessages(rtfreporter::generate_rtfreport(tfl_report(output, content = content, study = st),
                                                   f, overwrite = overwrite))
  invisible(normalizePath(f, winslash = "/", mustWork = FALSE))
}
