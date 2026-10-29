# ---- a table's parts: another plan's rows under its own ---------------------
# rtfreporter's plan_append() (#737): a table whose rows come from two sources
# -- a listing of the records above, their summary below -- is two ordinary
# plans, the second laid under the first.  The SPEC writes it as rtfreporter
# does: one content item whose arguments are the part's own `data` (where
# its rows come from: `source: ard`, `source: dataset`) and its own
# `content` (an ordinary table's items), with its `name` and `absent`:
#
#   content:
#     - table_plan: {cols: ATPT, rows: {subject: !r '~ "{USUBJID}/{AGE}/{SEX}"'}, value: AVAL}
#     - plan_cells: {default: "{value:.3s}", na: BLQ}
#     - plan_append:
#         name: summary
#         data: {source: ard}
#         content:
#           - normalize_ard: {}
#           - table_plan: {cols: ATPT, rows: {block: !r '~ "Summary Statistics"'}}
#           - plan_cells: {continuous: {N: "{N:.0f}", Mean: "{mean:.4s}"}}
#
# The program writes each part's plan first, under the part's name, from
# its own data object, then the table's plan with the part laid in:
#
#   summary <- ard |> normalize_ard() |> table_plan(...) |> plan_cells(...)
#   plan <- data |> table_plan(...) |> plan_cells(...) |> plan_append(summary, name = "summary")
#
# The function's and its argument's names are said here only, so the
# rtfreporter verb's name can change in one place.

.spec_part <- list(fn = "plan_append", arg = "part")

# Is an item a part?
.is_part <- function(it) {
  !is.null(it$fn) && identical(.schema_fn_name(it$fn), .spec_part$fn)
}

# A part item as read from YAML: its content as items, its data as a
# report's data
.part_in <- function(args) {
  if (!is.null(args$content)) {
    if (!is.list(args$content) || !is.null(names(args$content))) {
      .ard_stop(sprintf("%s: `content` is a sequence of items (`- fn: {args}`).", .spec_part$fn))
    }
    args$content <- lapply(args$content, .item_from_list)
  }
  if (!is.null(args$data)) args$data <- .data_steps_in(.calls_in(args$data))
  args
}

# A part's items as items, whatever form they come in: as read (tfl_item),
# or as the study's layers hand them on (`- fn: {args}` lists)
.part_items <- function(x) {
  lapply(x %||% list(), function(it) if (inherits(it, "tfl_item")) it else .item_from_list(it))
}

# ... and as written
.part_out <- function(args) {
  out <- .calls_out(args[setdiff(names(args), c("content", "data"))])
  if (!is.null(args$data)) out$data <- .calls_out(args$data)
  if (!is.null(args$content)) out$content <- .items_to_list(.part_items(args$content))
  out[c(intersect(c("name", "absent", "data"), names(out)),
        setdiff(names(out), c("name", "absent", "data", "content")),
        intersect("content", names(out)))]
}

# The parts of a content, in order: their names (the program's object for
# each), data objects and contents
.content_parts <- function(content) {
  parts <- Filter(.is_part, content)
  if (!length(parts)) return(list())
  nm <- vapply(seq_along(parts), function(i) parts[[i]]$args$name %||% paste0("part", i + 1L), "")
  if (anyDuplicated(nm)) {
    .ard_stop(sprintf("%s: two parts are called %s; give each its own `name`.",
                      .spec_part$fn, paste(sQuote(unique(nm[duplicated(nm)])), collapse = ", ")))
  }
  lapply(seq_along(parts), function(i) {
    a <- parts[[i]]$args
    if (!length(a$content)) {
      .ard_stop(sprintf("%s(\"%s\"): its `content` is the part's own table items.", .spec_part$fn, nm[i]))
    }
    list(name = nm[i], var = make.names(nm[i]), object = .part_object(a$data),
         data = a$data, content = .part_items(a$content), absent = a$absent)
  })
}

# The object a part's plan starts from: its data's `object`; the ARD
# (`ard`) for `source: ard`; the dataset's name in lower case for
# `source: dataset`
.part_object <- function(data) {
  if (!is.null(data$object)) return(as.character(data$object))
  src <- data$source %||% "ard"
  if (identical(src, "ard")) return("ard")
  if (identical(src, "dataset") && !is.null(data$dataset)) return(tolower(as.character(data$dataset)[1L]))
  .ard_stop(sprintf("%s: a part's data is `source: ard` or `source: dataset` with its `dataset`.",
                    .spec_part$fn))
}

# The call a part item stands for in the table's plan: the verb with the
# part's plan (its object) and its name / absent
.part_call <- function(it, i) {
  a <- it$args
  nm <- a$name %||% paste0("part", i + 1L)
  args <- stats::setNames(list(.spec_sym(make.names(nm))), .spec_part$arg)
  args$name <- nm
  if (!is.null(a$absent)) args$absent <- a$absent
  do.call(.spec_call, c(list(.spec_part$fn), args))
}

# The parts' programs, before the table's: one plan each, from its object
.content_parts_code <- function(content, pipe = NULL) {
  parts <- .content_parts(content)
  unlist(lapply(parts, function(p) {
    c(sprintf("# the part \"%s\": its rows go under the table's", p$name),
      .content_table_code(p$content, data = p$object, plan = p$var, pipe = pipe, kind = "table"), "")
  }))
}

# The parts' plans, run: each from its data object (`data` a named list of
# the objects the table reads: the table's own and each part's), in an
# environment the table's plan reads them from
.content_parts_env <- function(content, data) {
  env <- new.env(parent = emptyenv())
  for (p in .content_parts(content)) {
    d <- if (is.list(data) && !is.data.frame(data) && !is.null(data[[p$object]])) data[[p$object]] else NULL
    if (is.null(d)) {
      .ard_stop(sprintf("%s(\"%s\"): no `%s` to make its rows from (the part's data).",
                        .spec_part$fn, p$name, p$object))
    }
    assign(p$var, .content_table_run(p$content, d, kind = "table"), envir = env)
  }
  env
}

# The objects a table with parts reads, by name, as its program makes them:
# the study ARD's rows of the report (`ard`) when the table or a part reads
# it, and a listing's records for the table's own `source: dataset` (as
# `data`) and each part's (under its object's name)
.parts_objects <- function(o, study, dir = ".") {
  parts <- .content_parts(o$content)
  own <- o$data %||% list(source = "ard")
  src <- own$source %||% "ard"
  want_ard <- identical(src, "ard") || any(vapply(parts, function(p) identical(p$object, "ard"), NA))
  out <- list()
  if (want_ard) {
    ard <- suppressMessages(tfl_build_ard(study, dir = dir, output_id = o$id, save = FALSE))
    if (!is.null(ard$output_id)) ard <- ard[ard$output_id %in% o$id, , drop = FALSE]
    out$ard <- ard
  }
  records <- function(data) {
    x <- o
    x$data <- data
    # (a plain data frame: the plan reads columns it may not have by `$`)
    as.data.frame(.listing_data_of(x, study, dir))
  }
  if (identical(src, "dataset")) out$data <- records(own)
  for (p in parts) {
    if (identical(p$data$source %||% "ard", "dataset") && is.null(out[[p$object]])) {
      out[[p$object]] <- records(p$data)
    }
  }
  out
}

# A records table's dataset (its own data `source: dataset`), for the
# planner sheet's datasets: NULL for a table of the ARD
.records_dataset <- function(o) {
  d <- o$data
  if (!identical(o$kind, "table") || !identical(d$source, "dataset") || is.null(d$dataset)) return(NULL)
  as.character(d$dataset)[1L]
}

#' The code that makes the objects a report's content reads
#'
#' A table's content reads one object -- the study's ARD (`ard`), or the
#' records of a dataset (`data`, a table of records: `table_plan(value = )`)
#' -- and each part appended to it (`plan_append()`) its own: the ARD, or a
#' dataset's records under the part's object name (`adpc`).  This writes the
#' code that makes each of them, as the report's program has it before the
#' content's code ([tfl_content_code()]): a dataset's records as a listing's
#' data part ([tfl_listing_code()] `part = "data"`: read, the analysis
#' set's subjects, the condition), the ARD as the study ARD's rows of the
#' report.  A statement two objects share (a dataset read once) is written
#' once, with the first.
#'
#' @param output A report ([tfl_output()]) or its id.
#' @param study The study ([tfl_read_study_spec()]).
#' @param ard `FALSE` leaves the ARD's code out (a program that writes its
#'   own: the study's ARD program, its rework).
#' @return A named list, one element an object (`data`, `ard`, a part's
#'   object), each its lines of code.
#' @examples
#' o <- tfl_output("T-1", "table", content = list(
#'   tfl_item("table_plan", list(cols = "TRT01A"))))
#' st <- tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))
#' tfl_data_code(o, st)
#' @export
tfl_data_code <- function(output, study, ard = TRUE) {
  if (is.character(output) && length(output) == 1L) output <- tfl_read_outputs(study$dir)[[output]]
  if (!inherits(output, "tfl_output")) .ard_stop("tfl_data_code(): `output` is a report, tfl_output(), or its id.")
  ro <- tfl_resolve_output(output, study)
  own <- ro$data %||% list(source = "ard")
  objs <- list()
  if (identical(own$source, "dataset") && identical(ro$kind, "table")) objs$data <- own
  else objs[[as.character(own$object %||% "ard")]] <- list(source = "ard")
  for (p in .content_parts(ro$content)) {
    if (is.null(objs[[p$object]])) objs[[p$object]] <- p$data %||% list(source = "ard")
  }
  spec <- NULL
  out <- list()
  seen <- character()
  for (nm in names(objs)) {
    d <- objs[[nm]]
    lines <- if (identical(d$source %||% "ard", "ard")) {
      if (!isTRUE(ard)) next
      c("# the study ARD's rows of this report",
        sprintf("%s <- readRDS(\"output/ard/ard.rds\") |>\n  subset(output_id == \"%s\")", nm, ro$id))
    } else {
      if (is.null(spec)) spec <- .ard_spec_of_study(study)$spec
      .records_code(ro, d, nm, spec)
    }
    stmt <- !grepl("^#", lines) & nzchar(lines)
    lines <- lines[!(stmt & lines %in% seen)]
    seen <- c(seen, lines[stmt[seq_along(lines)] %in% TRUE])
    out[[nm]] <- lines
  }
  out
}

# a dataset's records as a listing's data part, made under `name`
.records_code <- function(o, data, name, ard_spec) {
  txt <- function(x) {
    v <- unlist(lapply(x, as.character))
    if (!length(v)) NA_character_ else paste(v, collapse = " | ")
  }
  l <- data.frame(output_id = o$id, dataset = txt(data[["dataset"]]), data = txt(data[["data"]]),
                  population_id = txt(data[["population"]] %||% o$population),
                  where = txt(data[["where"]]), sort = txt(data[["sort"]]), stringsAsFactors = FALSE)
  sp <- tfl_listing_spec(l, data.frame(output_id = o$id, vars = "USUBJID"), check = FALSE)
  code <- tfl_listing_code(sp, o$id, ard = ard_spec, part = "data")
  at <- grep("^data <- ", code)
  if (length(at)) code[at] <- sub("^data <- ", paste0(name, " <- "), code[at])
  code
}
