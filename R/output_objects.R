# ============================================================================
#  A report's content object, built from the study's files: the data read,
#  its ARD built, its content run -- what tfl_run_output() writes to RTF
#  and tfl_check_build() makes.  A report of several objects (content
#  objects placed by `page: layout:`) is R/page_objects.R's.
# ============================================================================

# A figure's datasets: those its data steps read, from data.yml's paths
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
    .data_file_must_exist(r, ds[[i]]$path, dir)
    data[[tolower(r)]] <- .ai_read_adam(p, r, study)
  }
  # the report's analysis data its reads name (`data:`): its data part of
  # the ARD program run from the study folder (tfl_ard_code(part = "data"))
  ids <- unique(unlist(lapply(steps, function(s)
    if (is.list(s) && identical(s$step, "read")) .fig_read_data(s))))
  if (length(ids)) {
    old <- options(tflspec.plain_ns = TRUE, tflspec.study_setup = FALSE)
    on.exit(options(old), add = TRUE)
    code <- tfl_ard_code(study, o$id, part = "data", data = ids)
    env <- new.env(parent = globalenv())
    eval(parse(text = tfl_helpers_code()), env)
    wd <- setwd(dir)
    on.exit(setwd(wd), add = TRUE)
    .eval_study_code(code, env, study, ".", output_id = o$id)
    for (id in ids) data[[id]] <- get(id, envir = env)
  }
  data
}

# A listing's records (`data: {source: dataset, ...}`): the data part of
# its program (tfl_listing_code(part = "data"): the dataset, or the
# report's analysis data, its analysis set's subjects, its condition, its
# code lists, its order) run from the study folder.  `ard`, `codelists`: the
# study's ARD definition and the reports' code lists (.ard_spec_of_study())
# when the caller has them already
.listing_data_of <- function(o, study, dir = ".", ard = NULL, codelists = NULL) {
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
  old <- options(tflspec.plain_ns = TRUE, tflspec.study_setup = FALSE)
  on.exit(options(old), add = TRUE)
  if (is.null(ard)) {
    s <- .ard_spec_of_study(study, output_id = o$id)
    ard <- s$spec
    codelists <- codelists %||% s$codelists
  }
  rw <- .records_rework(d)
  code <- tfl_listing_code(sp, o$id, ard = ard, part = "data", rework = rw,
                           codelists = codelists)
  if (is.null(code)) .ard_stop(sprintf("%s: the listing names no dataset.", o$id))
  env <- new.env(parent = globalenv())
  eval(parse(text = tfl_helpers_code()), env)
  wd <- setwd(dir)
  on.exit(setwd(wd), add = TRUE)
  .eval_study_code(code, env, study, ".", output_id = o$id)
  recs <- get("data", envir = env, inherits = FALSE)
  # (the data's steps are done in it: the content run does not run them again)
  if (!is.null(rw) && is.data.frame(recs)) attr(recs, "tfl_steps_done") <- TRUE
  recs
}

# The error of reading a listing's records when it is a template's `by` that
# the data does not have (dplyr: "object 'X' not found"): said as that
.listing_by_hint <- function(e, ro) {
  msg <- conditionMessage(e)
  # (the name in quotes, not the English words: R may say it in another
  # language, with plain or curly quotes)
  q <- regmatches(msg, gregexpr("['\u2018`][^'\u2019`]+['\u2019`]", msg))[[1L]]
  by <- as.character(unlist(attr(ro, "template")$args$by))
  v <- intersect(gsub("^.|.$", "", q), by)[1L]
  if (!is.na(v)) {
    .ard_stop(sprintf("`by` names %s, which the dataset %s does not have.", sQuote(v, FALSE),
                      paste(ro$data$dataset, collapse = "")))
  }
  stop(e)
}

# A report's content object -- the plan of a table or a listing, the
# figure(s) -- from the study's files, in the study folder (the working
# directory): the data read, its ARD built, its content run.  What
# tfl_run_output() writes to RTF, and what tfl_check_build() makes to know
# that the report can be made.
.output_build_content <- function(output, st) {
  # (each stage named in what stops it: "the ARD: ...", "the table: ...")
  # (the content contract's stop and the own code's data stop keep their
  # class: tfl_content_contract, tfl_content_data)
  stage <- function(what, expr) {
    tryCatch(expr, error = function(e) {
      if (inherits(e, c("tfl_content_contract", "tfl_content_data"))) .own_restop(e, what)
      .ard_stop(paste0(what, ": ", conditionMessage(e)))
    })
  }
  kind <- output$kind %||% "table"
  ro <- stage("resolving the report", tfl_resolve_output(output, st))
  own <- inherits(ro$content, "tfl_r")
  if (own && kind %in% .own_contract_kinds && !identical(ro$data$source, "dataset")) {
    # the report's own content code: the objects it reads, as its program
    # reads them (.own_content_data()), never tflspec's sample data
    data <- stage(paste0("the ", kind, "'s data"), .own_content_data(ro, st, "."))
    stage(paste("the", kind), suppressMessages(tfl_content_run(ro, data, study = st)))
  } else if (!own && any(vapply(ro$content, .is_part, NA))) {
    # a table with parts: each object it reads, by name -- the ARD, a
    # listing's records (the table's own data, a part's)
    objects <- stage("the parts' data", .parts_objects(ro, st, "."))
    stage(paste("the", kind), suppressMessages(tfl_content_run(ro, objects)))
  } else if (identical(ro$data$source, "dataset")) {
    # a listing's records (its own, or a template's: decided on the report
    # as resolved): its data step, as its program reads them
    recs <- stage("the listing's records", tryCatch(
      .listing_data_of(ro, st, "."), error = function(e) .listing_by_hint(e, ro)))
    stage(paste("the", kind), suppressMessages(tfl_content_run(ro, recs, study = st)))
  } else if (identical(kind, "figure")) {
    data <- stage("the figure's data", .contents_fig_data(ro, st, "."))
    if (length(ro$ard$analyses %||% list())) {
      data$ard <- stage("the ARD", tfl_build_ard(st, ".", output_id = ro$id, save = FALSE))
    }
    # (the warnings stay: the template's own -- subjects or records left out,
    # bars cut off -- are the user's to see, as a table's are)
    # (its subjects: the report's analysis set, made from the study, A2)
    stage("the figure", suppressMessages(tfl_content_run(ro, data, study = st)))
  } else {
    ard <- stage("the ARD", suppressMessages(tfl_build_ard(st, dir = ".", output_id = ro$id, save = FALSE)))
    if (!is.null(ard$output_id)) ard <- ard[ard$output_id %in% ro$id, , drop = FALSE]
    # (content objects: a `from:` that names a study dataset reads it, study)
    stage(paste("the", kind), suppressMessages(tfl_content_run(ro, ard, study = st)))
  }
}

# a classed stop of a report's own content, again with the stage it
# stopped in before its message (its class kept)
.own_restop <- function(e, what) {
  e$message <- paste0(what, ": ", conditionMessage(e))
  stop(e)
}

#' Build a report's RTF from its YAML file
#'
#' One line to make a report's RTF from the study's files: the report read
#' from `spec/outputs/`, its ARD built, its content run, its RTF written
#' where [tfl_report_path()] says.  A report whose content makes several
#' objects has them on its pages as its `page: layout:` places them
#' (see [tfl_read_output()], "Several objects in one report").
#'
#' @param output A report's id, its YAML file, or a [tfl_output()].
#' @param dir The study folder.
#' @param overwrite Write over an RTF already there.
#' @return The RTF's path, invisibly.
#' @seealso [tfl_report()], [tfl_content_run()]
#' @export
tfl_run_output <- function(output, dir = ".", overwrite = TRUE) {
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
